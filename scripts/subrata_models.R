###############################################################################
## GLM on Subhrajyoty's train/val split — using our EDA-informed features
## Competition metric: F1-score
###############################################################################

# load packages and utility functions
library(tidyverse)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")

# load the clean dataset
train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")

df_train <- impute_and_flag(create_features(train_dat))
df_val <- impute_and_flag(create_features(val_dat))


# -------------------------
# GLM based formula
# ----------------------
f_glm <- target ~
    a1c_latest + value_a1c_1 + a1c_weighted + abs_change + a1c_sd + a1c_range + a1c_rate +
    n_a1c + n_drug_classes + total_meds +
    sulfonylurea + insulin + metformin + sglt2 + glp1 + dpp4 +
    ed_visits + pcp_visits + admissions +
    age + gender_male +
    value_hdl + value_ldl + value_chol +
    time_weight + time_ldl + time_hdl +
    high_a1c_insulin + a1c_x_meds +
    high_a1c_no_meds + a1c_x_ndrug +
    a1c_change + a1c_per_drug +
    improving_on_meds + worsening_on_meds + stable_on_meds +
    on_modern_drugs + n_a1c_x_ndrug +
    still_high_improving + a1c_change_if_high +
    abs_change_miss + a1c_sd_miss + a1c_range_miss + a1c_rate_miss +
    value_hdl_miss + value_ldl_miss + value_chol_miss +
    time_weight_miss + time_ldl_miss + time_hdl_miss


# --------------
# Fit basic glm
# F1 score: ~ 56%

m1 <- glm(f_glm, data = df_train, family = binomial)
p1_train <- predict(m1, df_train, type = "response")
p1_thresh <- best_f1_threshold(p1_train, df_train$target == "Uncontrolled", df_train$mask)
p1_thresh

p1_val <- predict(m1, df_val, type = "response")
compute_metrics(p1_val > p1_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)


# ----------
# Fit glm with class weights to handle imbalance
# F1 score: ~ 56% - 58%

wts <- ifelse(df_train$target == "Uncontrolled", 1 - mean(df_train$target == "Uncontrolled"), mean(df_train$target == "Uncontrolled"))

m2 <- glm(f_glm, data = df_train, family = binomial, weights = wts)
p2_train <- predict(m2, df_train, type = "response")
p2_thresh <- best_f1_threshold(p2_train, df_train$target == "Uncontrolled", df_train$mask)
p2_thresh

p2_val <- predict(m2, df_val, type = "response")
compute_metrics(p2_val > p2_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)


# ------------------
# Fit glmnet elastic net (tuned via 5-fold cv on train)
# F1 score: ~ 60-61%

library(caret)
library(glmnet)

# create folds
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

m3 <- train(f_glm,
    data = df_train, method = "glmnet", family = "binomial",
    trControl = ctrl_cv, metric = "best_F1",
    tuneGrid = glmnet_grid,
    preProcess = c("center", "scale")
) # takes ~ 2 mins

p3_train <- predict(m3, df_train, type = "prob")$Uncontrolled
p3_thresh <- best_f1_threshold(p3_train, df_train$target == "Uncontrolled", df_train$mask)
p3_thresh

p3_val <- predict(m3, df_val, type = "prob")$Uncontrolled
compute_metrics(p3_val > p3_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)



# --------------
# Nonlinear model training
# Example: Decision Tree, GAM, GBM (xgboost)
# -------------
library(rpart)
library(rpart.plot)
library(mgcv)
library(xgboost)


# load training and validation data, but don't impute??
train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")

df_train <- create_features(train_dat)
df_val <- create_features(val_dat)
df_train_imputed <- impute_and_flag(df_train)
df_val_imputed <- impute_and_flag(df_val)


# feature columns to use
feat_cols <- c(
    "a1c_latest", "value_a1c_1", "a1c_weighted", "abs_change", "a1c_sd",
    "a1c_range", "a1c_rate", "a1c_change", "a1c_change_if_high",
    "n_a1c", "n_drug_classes", "total_meds",
    "sulfonylurea", "insulin", "metformin", "sglt2", "glp1", "dpp4",
    "ed_visits", "pcp_visits", "admissions",
    "age", "gender_male",
    "value_hdl", "value_ldl", "value_chol",
    "time_weight", "time_ldl", "time_hdl",
    "high_a1c_insulin", "a1c_x_meds",
    "high_a1c_no_meds", "a1c_x_ndrug",
    "a1c_per_drug", "improving_on_meds", "worsening_on_meds",
    "stable_on_meds", "on_modern_drugs", "n_a1c_x_ndrug",
    "still_high_improving"
)
dt_formula <- as.formula(paste("target ~", paste(feat_cols, collapse = " + ")))


# ---------------
# Model: Decision tree
# F1 Score: ~ 60-61%

m1 <- rpart(dt_formula,
    data = df_train, method = "class",
    control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 50)
)

p1_tr <- predict(m1, df_train, type = "prob")[, "Uncontrolled"]
p1_thresh <- best_f1_threshold(p1_tr, df_train$target == "Uncontrolled", df_train$mask)
p1_thresh

p1_val <- predict(m1, df_val, type = "prob")[, "Uncontrolled"]
compute_metrics(p1_val > p1_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)


# -------------
# Model: GAM
# F1 Score: ~ 60%

m2 <- gam(
    target ~
        s(a1c_weighted, k = 8) + s(a1c_latest, k = 8) + s(value_a1c_1, k = 8) +
        s(a1c_per_drug, k = 6) + s(a1c_change_if_high, k = 6) +
        n_a1c + n_drug_classes + total_meds +
        sulfonylurea + insulin + metformin + sglt2 + glp1 +
        ed_visits + age + gender_male +
        high_a1c_insulin + a1c_x_meds +
        high_a1c_no_meds + worsening_on_meds + improving_on_meds +
        stable_on_meds + still_high_improving + on_modern_drugs,
    family = binomial,
    data = df_train_imputed
) # takes ~ 1 min
p2_tr <- predict(m2, df_train_imputed, type = "response")

p2_thresh <- best_f1_threshold(p2_tr, df_train_imputed$target == "Uncontrolled", df_train_imputed$mask)
p2_thresh
p2_val <- predict(m2, df_val_imputed, type = "response")
compute_metrics(p2_val > p2_thresh["best_th"], df_val_imputed$target == "Uncontrolled", df_val_imputed$mask)


# ---------------
# XGBoost
# F1 Score: ~ 59%-60%

## xgboost handles NAs natively
xgb_train <- xgb.DMatrix(
    data = as.matrix(df_train[, feat_cols]),
    label = df_train$target == "Uncontrolled"
)
xgb_val <- xgb.DMatrix(
    data = as.matrix(df_val[, feat_cols]),
    label = df_val$target == "Uncontrolled"
)
params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = 4,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 10
)

m3 <- xgb.train(
    params = params,
    data = xgb_train,
    nrounds = 500,
    watchlist = list(train = xgb_train, val = xgb_val),
    early_stopping_rounds = 30,
    print_every_n = 50,
    verbose = 1
)

p3_tr <- predict(m3, xgb_train)
p3_thresh <- best_f1_threshold(p3_tr, df_train$target == "Uncontrolled", df_train$mask)
p3_thresh

p3_val <- predict(m3, xgb_val, type = "response")
compute_metrics(p3_val > p3_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)
