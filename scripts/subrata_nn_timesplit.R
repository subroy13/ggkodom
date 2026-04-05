#### TIME-BASED VALIDATION: Multi-head NN ####
# Instead of subject-based split (train patients vs val patients),
# split by TIME within each patient:
#   - Train on: ALL patients' intermediate transitions + TRAIN patients' target transitions
#   - Eval on: VAL patients' target transitions (held out — model never sees these labels)
#
# Key difference: val patients' intermediate dynamics enter training,
# so the model "knows" each val patient's trajectory before predicting their outcome.
# But their a1c_2025 labels are never exposed during training.
#
# Uses best config from subject-split experiments: Fix 1 (loss weighting) + Fix 3 (trajectory features)

library(tidyverse)
library(fastDummies)
library(torch)

source("./scripts/utils.R")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")

#### COMBINE TRAIN + VAL ####
train_patient_ids <- train_dat$basic$id
val_patient_ids <- val_dat$basic$id

combined_dat <- list(
    basic = bind_rows(train_dat$basic, val_dat$basic),
    counts = bind_rows(train_dat$counts, val_dat$counts),
    measurements = bind_rows(train_dat$measurements, val_dat$measurements)
)
cat("Train patients:", length(train_patient_ids), "\n")
cat("Val patients:", length(val_patient_ids), "\n")

#### CREATE STATE TRANSITIONS (same function as subrata_multihead_nn.R) ####
create_state_transitions <- function(dat) {
    measures <- dat$measurements

    target_rows <- measures$variable == "a1c_2025"
    measures$variable[target_rows] <- "a1c_6"
    measures$value[target_rows] <- measures$value[target_rows] * 10

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
            end_var = lead(variable, 1)
        ) %>%
        ungroup() %>%
        filter(!is.na(t_end)) %>%
        mutate(delta_t = if_else(t_end > t_start + 0.01, t_end - t_start, 0.01)) %>%
        select(id, t_start, t_end, delta_t, state_start, state_end, end_var) %>%
        group_by(id) %>%
        mutate(is_final = as.numeric(row_number() == n())) %>%
        ungroup() %>%
        mutate(is_target = as.numeric(end_var == "a1c_6")) %>%
        select(-end_var)

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
        pivot_wider(names_from = variable, values_from = value, names_prefix = "measure_") %>%
        mutate(
            across(.cols = starts_with("measure_"), .fns = ~ as.numeric(is.na(.x) | is.nan(.x)), .names = "{.col}_is_missing"),
            across(.cols = starts_with("measure_"), .fns = ~ ifelse(is.na(.x) | is.nan(.x), 0, .x))
        )

    transitions <- transitions %>%
        left_join(events, by = c("id", "t_start")) %>%
        select(-c(t_start, t_end))

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
            dat$counts %>% mutate(across(.cols = all_of(setdiff(names(dat$counts), "id")), .fns = ~ log(1 + .x))),
            by = "id"
        )

    patient_a1c_measures <- measures %>%
        filter(variable %in% paste0("a1c_", 1:5)) %>%
        drop_na() %>%
        select(id, variable, value) %>%
        arrange(id, variable) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        mutate(
            a1c_latest = exec(coalesce, !!!syms(paste0("a1c_", 5:1))),
            n_a1c = rowSums(!is.na(across(a1c_1:a1c_5))),
            a1c_ewma = {
                vals <- as.matrix(pick(a1c_1:a1c_5))
                w <- matrix(0.5^(4:0), nrow = nrow(vals), ncol = 5, byrow = TRUE)
                present <- !is.na(vals)
                vals[is.na(vals)] <- 0
                rowSums(vals * w * present) / rowSums(w * present)
            },
            a1c_sd = {
                vals <- as.matrix(pick(a1c_1:a1c_5))
                s <- apply(vals, 1, sd, na.rm = TRUE)
                replace(s, is.na(s), 0)
            },
            frac_above_8 = {
                vals <- as.matrix(pick(a1c_1:a1c_5))
                rowSums(vals >= 8, na.rm = TRUE) / n_a1c
            }
        ) %>%
        select(id, a1c_1, a1c_latest, n_a1c, a1c_ewma, a1c_sd, frac_above_8)
    patient_vars <- patient_vars %>% left_join(patient_a1c_measures, by = "id")

    transitions <- transitions %>% left_join(patient_vars, by = "id")

    train_features <- transitions %>%
        mutate(
            state_start_num = ifelse(state_start == "H", 1, 0),
            state_end_num = ifelse(state_end == "H", 1, 0)
        ) %>%
        select(-state_start, -state_end) %>%
        mutate(
            bmi = if_else(measure_weight > 0 & measure_height > 0, measure_weight / (((measure_height + 0.01) / 100)^2), 0),
            non_hdl = measure_chol - measure_hdl,
            ldl_hdl_ratio = ifelse(measure_hdl > 0 & measure_ldl > 0, measure_ldl / measure_hdl, 0),
            months_elapsed = (delta_t + 1) / 30.42,
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
        select(-months_elapsed)

    continuous_features <- c(
        "delta_t", "log_delta_t", "a1c_latest", "a1c_ewma", "a1c_sd", "frac_above_8",
        "measure_height", "measure_weight", "bmi",
        "measure_ldl", "measure_hdl", "measure_chol", "non_hdl", "ldl_hdl_ratio",
        "age", "adi_state", "adi_nation", "adi_discrepancy",
        "ed_visit", "pcp_visit", "admission",
        "ed_visit_rate", "pcp_visit_rate", "admission_rate",
        "medication_burden"
    )

    train_features %>%
        mutate(across(.cols = all_of(continuous_features), .fns = ~ as.numeric(scale(.x))))
}

#### BUILD TRANSITIONS ON COMBINED DATA ####
all_states <- create_state_transitions(combined_dat)
all_ids <- all_states$id
all_is_final <- all_states$is_final
all_is_target <- all_states$is_target
all_is_train_patient <- as.numeric(all_ids %in% train_patient_ids)
all_states <- all_states %>% select(-id)

cat("\nTransition table (all):\n")
table(all_states$state_start_num, all_states$state_end_num)

#### TIME-BASED SPLIT ####
# Training: everything EXCEPT val patients' target transitions
# Eval: val patients' target transitions only
train_mask <- !(all_is_target == 1 & all_is_train_patient == 0)
val_eval_mask <- all_is_target == 1 & all_is_train_patient == 0

# For loss weighting: which training rows are target transitions (from train patients)?
train_is_target_subset <- all_is_target[train_mask]

cat("\n=== Time-based split ===\n")
cat("Training rows:", sum(train_mask), "\n")
cat("  intermediate (all patients):", sum(train_mask & all_is_target == 0), "\n")
cat("  target (train patients only):", sum(train_mask & all_is_target == 1), "\n")
cat("Val eval rows:", sum(val_eval_mask), "\n")

#### TENSORS ####
feature_mat <- all_states %>% select(-c(state_start_num, state_end_num, is_final, is_target))

library(torch)
set.seed(1234)

x_train <- torch_tensor(as.matrix(feature_mat[train_mask, ]), dtype = torch_float())
y_train <- torch_tensor(as.matrix(all_states$state_end_num[train_mask]), dtype = torch_float())
start_train <- torch_tensor(as.matrix(all_states$state_start_num[train_mask]), dtype = torch_float())

x_val <- torch_tensor(as.matrix(feature_mat[val_eval_mask, ]), dtype = torch_float())
y_val <- torch_tensor(as.matrix(all_states$state_end_num[val_eval_mask]), dtype = torch_float())
start_val <- torch_tensor(as.matrix(all_states$state_start_num[val_eval_mask]), dtype = torch_float())

cat("Input dim:", ncol(x_train), "\n")

#### ARCHITECTURE (same multi-head) ####
transition_multi_head <- nn_module(
    "MultiHeadTransition",
    initialize = function(input_dim, hidden_dim) {
        self$shared <- nn_sequential(
            nn_linear(input_dim, hidden_dim),
            nn_relu(),
            nn_linear(hidden_dim, hidden_dim),
            nn_relu()
        )
        self$head_L_to_H <- nn_linear(hidden_dim, 1)
        self$head_H_to_H <- nn_linear(hidden_dim, 1)
    },
    forward = function(x, start_state) {
        shared_features <- self$shared(x)
        logit_L2H <- self$head_L_to_H(shared_features)
        logit_H2H <- self$head_H_to_H(shared_features)
        final_logits <- (1 - start_state) * logit_L2H + (start_state) * logit_H2H
        return(final_logits)
    }
)

model <- transition_multi_head(input_dim = ncol(x_train), hidden_dim = 32)
cat("Model params:", sum(sapply(model$parameters, function(p) if (p$requires_grad) p$numel() else 0)), "\n")

#### CLASS WEIGHTS ####
train_states_sub <- all_states[train_mask, ]
n_L2L <- sum(train_states_sub$state_start_num == 0 & train_states_sub$state_end_num == 0)
n_L2H <- sum(train_states_sub$state_start_num == 0 & train_states_sub$state_end_num == 1)
n_H2L <- sum(train_states_sub$state_start_num == 1 & train_states_sub$state_end_num == 0)
n_H2H <- sum(train_states_sub$state_start_num == 1 & train_states_sub$state_end_num == 1)
weight_L2H <- n_L2L / n_L2H
weight_H2H <- n_H2L / n_H2H
cat(sprintf("Class weights: L->H = %.2f, H->H = %.2f\n", weight_L2H, weight_H2H))

loss_fn_L2H <- nn_bce_with_logits_loss(pos_weight = torch_tensor(weight_L2H, dtype = torch_float()), reduction = "none")
loss_fn_H2H <- nn_bce_with_logits_loss(pos_weight = torch_tensor(weight_H2H, dtype = torch_float()), reduction = "none")

#### FIX 1: LOSS WEIGHTING — upweight target transitions in training ####
final_upweight <- 10.0
transition_w <- torch_tensor(ifelse(train_is_target_subset == 1, final_upweight, 1.0), dtype = torch_float())$unsqueeze(2)

#### TRAINING ####
optimizer <- optim_adam(model$parameters, lr = 0.001, weight_decay = 1e-4)

epochs <- 200
loss_curve <- numeric(epochs)
for (epoch in 1:epochs) {
    shared_features <- model$shared(x_train)
    predictions_L2H <- model$head_L_to_H(shared_features)
    predictions_H2H <- model$head_H_to_H(shared_features)

    raw_loss_L <- loss_fn_L2H(predictions_L2H, y_train)
    raw_loss_H <- loss_fn_H2H(predictions_H2H, y_train)

    per_row_loss <- raw_loss_L * (1 - start_train) + raw_loss_H * start_train
    loss <- (per_row_loss * transition_w)$sum() / transition_w$sum()

    optimizer$zero_grad()
    loss$backward()
    optimizer$step()
    loss_curve[epoch] <- as.numeric(loss)

    if (epoch %% 50 == 0) {
        cat(sprintf("Epoch: %3d | Loss: %.4f\n", epoch, as.numeric(loss)))
    }
}
plot(loss_curve, type = "l", main = "Time-split: training loss")

#### INFERENCE ####
# Train-level (all training rows)
train_probs <- torch_sigmoid(model(x_train, start_train))

# Threshold from train patients' target transitions
train_target_idx <- which(train_is_target_subset == 1)
train_target_probs <- as.numeric(train_probs)[train_target_idx]
train_target_labels <- train_states_sub$state_end_num[train_target_idx] == 1
train_target_starts <- train_states_sub$state_start_num[train_target_idx]

cat("\n----- Threshold tuning (train patients' target transitions) -----\n")
cat("Train target rows:", length(train_target_idx), "\n")

# State-specific thresholds
idx_L <- train_target_starts == 0
thresh0 <- best_f1_threshold(train_target_probs, train_target_labels, idx_L)
cat("L->H threshold:"); print(thresh0)

idx_H <- train_target_starts == 1
thresh1 <- best_f1_threshold(train_target_probs, train_target_labels, idx_H)
cat("H->H threshold:"); print(thresh1)

# Val evaluation — these are val patients' target transitions (held out from training)
val_probs <- torch_sigmoid(model(x_val, start_val))
val_labels <- all_states$state_end_num[val_eval_mask] == 1
val_starts <- all_states$state_start_num[val_eval_mask]

cat("\n=== VAL PATIENT-LEVEL Metrics (competition metric) ===\n")
cat("Val target transitions:", sum(val_eval_mask), "\n")
cat("Base rate:", mean(val_labels), "\n\n")

# State-specific thresholds
cat("--- With state-specific thresholds (from train targets) ---\n")
val_th <- ifelse(val_starts == 0, thresh0["best_th"], thresh1["best_th"])
compute_metrics(as.numeric(val_probs) > val_th, val_labels)

# Best single threshold (tuned on val — for reference only, not fair)
cat("\n--- With best single threshold (tuned on val, reference only) ---\n")
patient_thresh <- best_f1_threshold(as.numeric(val_probs), val_labels)
cat("Best threshold:", patient_thresh["best_th"], "\n")
cat("Best F1:", patient_thresh["best_f1"], "\n")
compute_metrics(as.numeric(val_probs) > patient_thresh["best_th"], val_labels)
