library(tidyverse)
library(fastDummies)

source("./scripts/utils.R")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")
test_dat <- readRDS("./data/processed/test.Rds")

train_dat$measurements %>%
    filter(variable %in% c("a1c_1", "a1c_2025")) %>%
    select(id, variable, value) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    ggplot() +
    geom_density(aes(x = a1c_1, fill = factor(a1c_2025)), alpha = 0.5) +
    geom_vline(xintercept = 8, color = "red", linetype = "dashed") +
    geom_vline(xintercept = 6.75, color = "black", linetype = "dashed") +
    theme_bw()


# Let's look at modelling event to event transformation
# Patient State: Low (<8), High (>= 8)
# P_{ij}(t) = lambda_{ij}(t) / \sum_{k} \lambda_{ik}(t)
#   The intensity function lambda_{ik}(t) depends on:
#       - the time gap, patient profile,
#       - if measurements happen, and their values

create_state_transitions <- function(dat) {
    measures <- dat$measurements

    target_rows <- measures$variable == "a1c_2025"
    measures$variable[target_rows] <- "a1c_6"
    measures$value[target_rows] <- measures$value[target_rows] * 10

    # extract the transitions
    transitions <- measures %>%
        filter(variable %in% paste0("a1c_", 1:6)) %>%
        drop_na() %>%
        arrange(id, variable) %>%
        group_by(id) %>%
        mutate(
            t_start = time,
            state_start = if_else(value > 8.0, "H", "L"),
            t_end = lead(time, 1),
            state_end = lead(if_else(value > 8.0, "H", "L"), 1),
        ) %>%
        ungroup() %>%
        filter(!is.na(t_end)) %>%
        mutate(delta_t = if_else(t_end > t_start + 0.01, t_end - t_start, 0.01)) %>%
        select(id, t_start, t_end, delta_t, state_start, state_end)

    events <- transitions %>%
        select(id, t_start) %>%
        cross_join(tibble(variable = c("height", "weight", "ldl", "hdl", "chol"))) %>%
        left_join(
            measures %>%
                filter(variable %in% c("height", "weight", "ldl", "hdl", "chol")) %>%
                drop_na(),
            join_by(id, variable, closest(t_start >= time))
        ) %>%
        select(id, t_start, variable, value) %>%
        pivot_wider(
            names_from = variable,
            values_from = value,
            names_prefix = "measure_"
        ) %>%
        mutate(
            # Create missingness flags for all extracted features
            across(
                .cols = starts_with("measure_"),
                .fns = ~ as.numeric(is.na(.x) | is.nan(.x)),
                .names = "{.col}_is_missing"
            ),

            # Impute the actual values to 0 to protect the neural network's matrix math
            across(
                .cols = starts_with("measure_"),
                .fns = ~ ifelse(is.na(.x) | is.nan(.x), 0, .x)
            )
        )

    # join together
    transitions <- transitions %>%
        left_join(events, by = c("id", "t_start")) %>%
        select(-c(t_start, t_end))

    # patient level infos
    patient_vars <- dat$basic %>%
        mutate(
            across(c(gender, ethnicity, race), ~ coalesce(.x, "Missing")),
            gender = factor(gender, levels = c("F", "M", "Missing")),
            ethnicity = factor(ethnicity, levels = c("HL", "Missing", "NHL")),
            race = factor(race, levels = c("Asian", "Black", "Missing", "Native", "Other", "White"))
        ) %>%
        dummy_cols(select_columns = c("gender", "ethnicity", "race")) %>%
        mutate(
            adi_state = if_else(is.na(adi_state), 5, adi_state),
            adi_nation = if_else(is.na(adi_nation), 50, adi_nation)
        ) %>%
        select(-c(dob, gender, ethnicity, race)) %>%
        left_join(
            dat$counts %>% mutate(
                across(
                    .cols = all_of(setdiff(names(dat$counts), "id")),
                    .fns = ~ log(1 + .x)
                )
            ),
            by = "id"
        )


    patient_a1c_measures <- measures %>%
        filter(variable %in% paste0("a1c_", 1:5)) %>%
        drop_na() %>%
        select(id, variable, value) %>%
        arrange(id, variable) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        mutate(
            a1c_latest = exec(coalesce, !!!syms(paste0("a1c_", 5:1))), # latest a1c value
            n_a1c = rowSums(!is.na(across(a1c_1:a1c_5))) # number of readings
        ) %>%
        select(id, a1c_1, a1c_latest, n_a1c)
    patient_vars <- patient_vars %>% left_join(patient_a1c_measures, by = "id")

    transitions <- transitions %>%
        left_join(patient_vars, by = "id")

    # do final conversion
    train_features <- transitions %>%
        mutate(
            # Convert states to strictly binary (1 = High/Uncontrolled, 0 = Low/Controlled)
            state_start_num = ifelse(state_start == "H", 1, 0),
            state_end_num = ifelse(state_end == "H", 1, 0)
        ) %>%
        select(-state_start, -state_end) %>% # Drop character columns and identifiers not meant for the network
        mutate(
            # add more features
            bmi = if_else(measure_weight > 0 & measure_height > 0, measure_weight / (((measure_height + 0.01) / 100)^2), 0),
            non_hdl = measure_chol - measure_hdl,
            ldl_hdl_ratio = ifelse(measure_hdl > 0 & measure_ldl > 0, measure_ldl / measure_hdl, 0),
            months_elapsed = (delta_t + 1) / 30.42, # some time grouping (365/12 ~ 30.42)
            ed_visit_rate = ed_visit / months_elapsed,
            admission_rate = admission / months_elapsed,
            pcp_visit_rate = pcp_visit / months_elapsed,
            log_delta_t = log(delta_t + 1),
            medication_burden = glp1 + insulin + metformin + sglt2 + sulfonylurea + dpp4,
            on_insulin = ifelse(insulin > 0, 1, 0),
            on_metformin = ifelse(metformin > 0, 1, 0),
            on_sglt2 = ifelse(sglt2 > 0, 1, 0),
            on_sulfonylurea = ifelse(sulfonylurea > 0, 1, 0),
            on_dpp4 = ifelse(dpp4 > 0, 1, 0),
            on_glp1 = ifelse(glp1 > 0, 1, 0),
            adi_discrepancy = adi_nation - adi_state
        ) %>%
        select(-months_elapsed) # remove intermediate variable

    continuous_features <- c(
        "delta_t", "log_delta_t", "a1c_latest",
        "measure_height", "measure_weight", "bmi",
        "measure_ldl", "measure_hdl", "measure_chol", "non_hdl", "ldl_hdl_ratio",
        "age", "adi_state", "adi_nation", "adi_discrepancy",
        "ed_visit", "pcp_visit", "admission",
        "ed_visit_rate", "pcp_visit_rate", "admission_rate",
        "medication_burden"
    )

    # 2. Scale only the continuous features
    train_features %>%
        mutate(
            across(
                .cols = all_of(continuous_features),
                .fns = ~ as.numeric(scale(.x)) # scale() returns a matrix, so we wrap it in as.numeric() to keep it a clean vector
            )
        )
}

train_states <- create_state_transitions(train_dat)
train_ids <- train_states$id
train_states <- train_states %>% select(-id)
val_states <- create_state_transitions(val_dat)
val_ids <- val_states$id
val_states <- val_states %>% select(-id)

train_states
table(train_states$state_start_num, train_states$state_end_num)

# data to tensor
library(torch)
set.seed(1234)

x_tensor <- torch_tensor(as.matrix(train_states %>% select(-c(state_start_num, state_end_num))), dtype = torch_float())
start_state_tensor <- torch_tensor(as.matrix(train_states$state_start_num), dtype = torch_float())
y_tensor <- torch_tensor(as.matrix(train_states$state_end_num), dtype = torch_float())

# multi-head attention NN
transition_multi_head <- nn_module(
    "MultiHeadTransition",
    initialize = function(input_dim, hidden_dim) {
        # 1. Shared Representation: Learns general feature embeddings using ALL data
        self$shared <- nn_sequential(
            nn_linear(input_dim, hidden_dim),
            nn_relu(),
            nn_linear(hidden_dim, hidden_dim),
            nn_relu()
        )

        # 2. State-Specific Heads: Independent weights for the conditional distributions
        self$head_L_to_H <- nn_linear(hidden_dim, 1) # Active only when S_0 = 0
        self$head_H_to_H <- nn_linear(hidden_dim, 1) # Active only when S_0 = 1
    },
    forward = function(x, start_state) {
        # x: Feature matrix [N, p]
        # start_state: Binary tensor [N, 1] where 0 = L, 1 = H

        # Pass features through shared layers
        shared_features <- self$shared(x)

        # Calculate BOTH possible transitions independently
        logit_L2H <- self$head_L_to_H(shared_features)
        logit_H2H <- self$head_H_to_H(shared_features)

        # 3. The Masking Logic (Enforcing the Conditional Distribution)
        # If start_state is 0, (1 - 0) * L2H + (0) * H2H = L2H
        # If start_state is 1, (1 - 1) * L2H + (1) * H2H = H2H
        final_logits <- (1 - start_state) * logit_L2H + (start_state) * logit_H2H

        return(final_logits)
    }
)

dim(x_tensor)
model <- transition_multi_head(input_dim = 55, hidden_dim = 32)

# print model size
sum(sapply(model$parameters, function(p) {
    if (p$requires_grad) p$numel() else 0
}))


# Calculate exact inverse ratios for pos_weight
weight_L2H <- 26774 / 2785 # ~9.61
weight_H2H <- 2837 / 4208 # ~0.67
loss_fn_L2H <- nn_bce_with_logits_loss(
    pos_weight = torch_tensor(weight_L2H, dtype = torch_float()),
    reduction = "none" # We need element-wise loss to apply the mask
)
loss_fn_H2H <- nn_bce_with_logits_loss(
    pos_weight = torch_tensor(weight_H2H, dtype = torch_float()),
    reduction = "none"
)

optimizer <- optim_adam(model$parameters, lr = 0.001, weight_decay = 1e-4)

epochs <- 200
loss_curve <- numeric(epochs)
for (epoch in 1:epochs) {
    shared_features <- model$shared(x_tensor) # forward pass
    predictions_L2H <- model$head_L_to_H(shared_features)
    predictions_H2H <- model$head_H_to_H(shared_features)

    # Calculate unreduced losses
    raw_loss_L <- loss_fn_L2H(predictions_L2H, y_tensor)
    raw_loss_H <- loss_fn_H2H(predictions_H2H, y_tensor)

    # The final loss only averages over the active, properly weighted rows
    loss <- (raw_loss_L * (1 - start_state_tensor) + raw_loss_H * start_state_tensor)$mean()

    # Backward pass and optimize
    optimizer$zero_grad()
    loss$backward()
    optimizer$step()
    loss_curve[epoch] <- as.numeric(loss)

    if (epoch %% 50 == 0) {
        cat(sprintf("Epoch: %3d | Loss: %.4f\n", epoch, as.numeric(loss)))
    }
}
plot(loss_curve, type = "l")

# let's try to do inference
probs <- torch_sigmoid(model(x_tensor, start_state_tensor))

# calculate state specific threshold
idx <- train_states$state_start_num == 0
thresh0 <- best_f1_threshold(as.numeric(probs), train_states$state_end_num == 1, idx)
thresh0

idx <- train_states$state_start_num == 1
thresh1 <- best_f1_threshold(as.numeric(probs), train_states$state_end_num == 1, idx)
thresh1

compute_metrics(
    as.numeric(probs) > ifelse(train_states$state_start_num == 0, thresh0["best_th"], thresh1["best_th"]),
    train_states$state_end_num == 1
)


# validation set inference
val_x <- torch_tensor(as.matrix(val_states %>% select(-c(state_start_num, state_end_num))), dtype = torch_float())
val_start_state <- torch_tensor(as.matrix(val_states$state_start_num), dtype = torch_float())
val_probs <- torch_sigmoid(model(val_x, val_start_state))
val_th <- ifelse(val_states$state_start_num == 0, thresh0["best_th"], thresh1["best_th"])
compute_metrics(
    as.numeric(val_probs) > val_th,
    val_states$state_end_num == 1
)

##########################
# DO NOT RUN THIS PART
##########################

# peek at test set performance
test_states <- create_state_transitions(test_dat)
test_ids <- test_states$id
test_states <- test_states %>% select(-id)
test_x <- torch_tensor(as.matrix(test_states %>% select(-state_start_num, -state_end_num)), dtype = torch_float())
test_start_state <- torch_tensor(as.matrix(test_states$state_start_num), dtype = torch_float())
test_probs <- torch_sigmoid(model(test_x, test_start_state))
test_th <- ifelse(test_states$state_start_num == 0, thresh0["best_th"], thresh1["best_th"])
compute_metrics(
    as.numeric(test_probs) > test_th,
    test_states$state_end_num == 1
)


# ---
# Probabilities
tibble(
    id = c(train_ids, val_ids),
    NN = c(as.numeric(probs), as.numeric(val_probs))
) %>%
    write_csv("./data/processed/nn_probs.csv")
