library(tidyverse)

source("./scripts/utils.R")

df <- read_csv("./data/processed/imputed/dt.csv")
df$target <- factor(ifelse(df$y == 1, "Uncontrolled", "Controlled"), level = c("Controlled", "Uncontrolled"))
df_train <- df %>% select(-c(ID, y))

# glm
m1 <- glm(target ~ ., family = binomial, data = df_train)
p1_train <- predict(m1, df_train, type = "response")
p1_thresh <- best_f1_threshold(p1_train, df_train$target == "Uncontrolled")
p1_thresh

# glmnet
library(glmnet)
library(caret)

set.seed(2441139)
folds_tr <- createFolds(df_train$target, k = 5, returnTrain = TRUE)

f1_summary <- function(data, lev = NULL, model = NULL) {
    pos <- lev[2] # positive is the 2nd level
    obs_pos <- data$obs == pos
    probs <- data[, pos]

    best_f1_val <- -1
    for (th in seq(0.01, 0.99, 0.01)) {
        pred_pos <- probs >= th
        out <- compute_metrics(pred_pos, obs_pos, verbose = FALSE)
        best_f1_val <- max(best_f1_val, out$metrics["F1"])
    }
    c("best_F1" = best_f1_val)
}

ctrl_cv <- trainControl(
    method = "cv", index = folds_tr, classProbs = TRUE,
    summaryFunction = f1_summary, savePredictions = "final"
)

glmnet_grid <- expand.grid(
    alpha = seq(0, 1, by = 0.1),
    lambda = 10^seq(-4, 0, length = 30)
)

m3 <- train(target ~ .,
    data = df_train, method = "glmnet", family = "binomial",
    trControl = ctrl_cv, metric = "best_F1",
    tuneGrid = glmnet_grid,
    preProcess = c("center", "scale")
) # takes ~ 2 mins

p3_train <- predict(m3, df_train, type = "prob")$Uncontrolled
p3_thresh <- best_f1_threshold(p3_train, df_train$target == "Uncontrolled")
p3_thresh


# ----------------
# Neural Network

# =======================
# Create wide format masking data for training NN
library(tidyverse)
library(fastDummies)

train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")
test_dat <- readRDS("./data/processed/test.Rds")

create_nn_feature_target <- function(train_dat) {
    var1 <- train_dat$basic %>%
        mutate(across(c(gender, ethnicity, race), ~ coalesce(.x, "Missing"))) %>%
        dummy_cols(select_columns = c("gender", "ethnicity", "race")) %>%
        mutate(
            adi_state = if_else(is.na(adi_state), 5, adi_state),
            adi_nation = if_else(is.na(adi_nation), 50, adi_nation)
        ) %>%
        select(-c(dob, gender, ethnicity, race)) %>%
        left_join(train_dat$counts, by = "id")

    measurement_col_names <- c(paste0("time_", unique(train_dat$measurements$variable)), paste0("value_", unique(train_dat$measurements$variable)))
    l <- as.list(rep(-1, length(measurement_col_names)))
    names(l) <- measurement_col_names

    var2 <- train_dat$measurements %>%
        pivot_wider(names_from = variable, values_from = c(time, value)) %>%
        mutate(
            across(
                .cols = starts_with("value_") | starts_with("time_"), # Select the columns you want to check
                .fns = is.na, # Apply the is.na function
                .names = "is_{.col}_missing" # Define the naming convention for the new columns
            )
        ) %>%
        replace_na(replace = l)

    total_df <- var1 %>%
        left_join(var2, by = "id") %>%
        arrange(id)

    # now extract features and target
    target_df <- total_df %>%
        mutate(target = if_else(value_a1c_2025 > 0, "Uncontrolled", "Controlled")) %>%
        select(id, target)

    feature_df <- total_df %>% select(-c(id, value_a1c_2025))

    return(list(
        "feature" = feature_df,
        "target" = target_df
    ))
}

train_nn <- create_nn_feature_target(train_dat)
val_nn <- create_nn_feature_target(val_dat)

library(torch)

# Convert dataframe to tensors: ensure inputs are numeric matrices, not dataframes
x_tensor <- torch_tensor(as.matrix(train_nn$feature), dtype = torch_float())
y_tensor <- torch_tensor(as.matrix(train_nn$target$target == "Uncontrolled"), dtype = torch_float()) # Reshape to [N, 1]

# 3. Architecture remains the same (Outputting raw logits)
mlp_net <- nn_module(
    "BinaryMLP",
    initialize = function(input_dim, hd1, hd2, output_dim) {
        self$fc1 <- nn_linear(input_dim, hd1)
        self$relu1 <- nn_relu()
        self$fc2 <- nn_linear(hd1, hd2)
        self$relu2 <- nn_relu()
        self$fc3 <- nn_linear(hd2, output_dim)
    },
    forward = function(x) {
        x |>
            self$fc1() |>
            self$relu1() |>
            self$fc2() |>
            self$relu2() |>
            self$fc3() # Note: No Sigmoid here! We output raw logits.
    }
)

dim(x_tensor)
model <- mlp_net(input_dim = 69, hd1 = 32, hd2 = 16, output_dim = 1)

# 4. Use BCEWithLogitsLoss (Combines Sigmoid + Binary Cross Entropy)
loss_fn <- nn_bce_with_logits_loss()
optimizer <- optim_adam(model$parameters, lr = 0.01)

# 5. Training Loop
epochs <- 1000
for (epoch in 1:epochs) {
    predictions <- model(x_tensor)
    loss <- loss_fn(predictions, y_tensor)

    optimizer$zero_grad()
    loss$backward()
    optimizer$step()

    if (epoch %% 20 == 0) {
        cat(sprintf("Epoch: %d, Loss: %.4f\n", epoch, as.numeric(loss)))
    }
}

# 6. Inference: Apply Sigmoid to convert logits to probs
raw_logits <- model(x_tensor)
probs <- torch_sigmoid(raw_logits)
thresh <- best_f1_threshold(
    as.numeric(probs),
    train_nn$target$target == "Uncontrolled",
    !train_nn$feature$is_time_a1c_2025_missing
)
thresh

# 7. Inference: Apply on val data to convert logits to probabilities
val_x <- torch_tensor(as.matrix(val_nn$feature), dtype = torch_float())
val_probs <- torch_sigmoid(model(val_x))
compute_metrics(
    as.numeric(val_probs) > thresh,
    val_nn$target$target == "Uncontrolled",
    !val_nn$feature$is_time_a1c_2025_missing
)
