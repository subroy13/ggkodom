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
    adi_state + adi_nation +
    cad + copd +
    race_black + race_white + race_asian + ethnicity_hl +
    value_hdl + value_ldl + value_chol +
    value_height + value_weight +
    time_weight + time_height + time_ldl + time_hdl +
    days_to_eval + time_gap +
    max_drop + max_rise + has_sudden_drop + has_sudden_rise +
    frac_above_7 + n_crossings_7 + slope_above_7 + slope_below_7 +
    frac_above_8 + n_crossings_8 + slope_above_8 + slope_below_8 +
    post_drop_drift + sudden_drop_still_high7 + sudden_drop_still_high8 +
    high7_insulin + high7_no_meds + still_high7_improving + a1c_change_if_high7 +
    high8_insulin + high8_no_meds + still_high8_improving + a1c_change_if_high8 +
    a1c_x_meds + a1c_x_ndrug +
    a1c_change + a1c_per_drug +
    improving_on_meds + worsening_on_meds + stable_on_meds +
    on_modern_drugs + n_a1c_x_ndrug +
    abs_change_miss + a1c_sd_miss + a1c_range_miss + a1c_rate_miss +
    value_hdl_miss + value_ldl_miss + value_chol_miss +
    value_height_miss + value_weight_miss +
    time_weight_miss + time_height_miss + time_ldl_miss + time_hdl_miss +
    adi_state_miss + adi_nation_miss +
    time_gap_miss + max_drop_miss + max_rise_miss +
    slope_above_7_miss + slope_below_7_miss +
    slope_above_8_miss + slope_below_8_miss +
    post_drop_drift_miss +
    bmi + bmi_x_male + bmi_miss
    # non_hdl + ldl_hdl_ratio + non_hdl_miss + ldl_hdl_ratio_miss +
    # adi_discrepancy +
    # log_ed_visits + log_pcp_visits + log_admissions + log_total_meds


# --------------
# Fit basic glm
# F1 score: ~ 56%

cat("\n------ GLM (basic) ---------\n")
m1 <- glm(f_glm, data = df_train, family = binomial)
p1_train <- predict(m1, df_train, type = "response")
p1_thresh <- best_f1_threshold(p1_train, df_train$target == "Uncontrolled", df_train$mask)
p1_thresh

p1_val <- predict(m1, df_val, type = "response")
compute_metrics(p1_val > p1_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)
prob_glm_val <- p1_val; th_glm <- p1_thresh["best_th"]


# ----------
# Fit glm with class weights to handle imbalance
# F1 score: ~ 56% - 58%

cat("\n------ GLM + class weights ---------\n")
wts <- ifelse(df_train$target == "Uncontrolled", 1 - mean(df_train$target == "Uncontrolled"), mean(df_train$target == "Uncontrolled"))

m2 <- glm(f_glm, data = df_train, family = binomial, weights = wts)
p2_train <- predict(m2, df_train, type = "response")
p2_thresh <- best_f1_threshold(p2_train, df_train$target == "Uncontrolled", df_train$mask)
p2_thresh

p2_val <- predict(m2, df_val, type = "response")
compute_metrics(p2_val > p2_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)
prob_glm_wt_val <- p2_val; th_glm_wt <- p2_thresh["best_th"]


# glmnet and XGBoost moved after ensemble (slower models run last)



# --------------
# Nonlinear model training
# Example: Decision Tree, GAM, GBM (xgboost)
# -------------
library(rpart)
library(rpart.plot)
library(mgcv)


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
    "a1c_range", "a1c_rate", "a1c_change",
    "n_a1c", "n_drug_classes", "total_meds",
    "sulfonylurea", "insulin", "metformin", "sglt2", "glp1", "dpp4",
    "ed_visits", "pcp_visits", "admissions",
    "age", "gender_male",
    "adi_state", "adi_nation",
    "cad", "copd",
    "race_black", "race_white", "race_asian", "ethnicity_hl",
    "value_hdl", "value_ldl", "value_chol",
    "value_height", "value_weight",
    "time_weight", "time_height", "time_ldl", "time_hdl",
    "days_to_eval", "time_gap",
    "max_drop", "max_rise", "has_sudden_drop", "has_sudden_rise",
    "frac_above_7", "n_crossings_7", "slope_above_7", "slope_below_7",
    "frac_above_8", "n_crossings_8", "slope_above_8", "slope_below_8",
    "post_drop_drift", "sudden_drop_still_high7", "sudden_drop_still_high8",
    "high7_insulin", "high7_no_meds", "still_high7_improving", "a1c_change_if_high7",
    "high8_insulin", "high8_no_meds", "still_high8_improving", "a1c_change_if_high8",
    "a1c_x_meds", "a1c_x_ndrug",
    "a1c_per_drug", "improving_on_meds", "worsening_on_meds",
    "stable_on_meds", "on_modern_drugs", "n_a1c_x_ndrug",
    "bmi", "bmi_x_male"
    # "non_hdl", "ldl_hdl_ratio",
    # "adi_discrepancy",
    # "log_ed_visits", "log_pcp_visits", "log_admissions", "log_total_meds"
)
dt_formula <- as.formula(paste("target ~", paste(feat_cols, collapse = " + ")))


# ---------------
# Model: Decision tree
# F1 Score: ~ 60-61%

cat("\n------ Decision Tree (rpart) ---------\n")
m1 <- rpart(dt_formula,
    data = df_train, method = "class",
    control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 50)
)

p1_tr <- predict(m1, df_train, type = "prob")[, "Uncontrolled"]
p1_thresh <- best_f1_threshold(p1_tr, df_train$target == "Uncontrolled", df_train$mask)
p1_thresh

p1_val <- predict(m1, df_val, type = "prob")[, "Uncontrolled"]
compute_metrics(p1_val > p1_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)
prob_tree_val <- p1_val; th_tree <- p1_thresh["best_th"]


# -------------
# Model: GAM
# F1 Score: ~ 60%

cat("\n------ GAM ---------\n")
m2 <- gam(
    target ~
        s(a1c_weighted, k = 8) + s(a1c_latest, k = 8) + s(value_a1c_1, k = 8) +
        s(a1c_per_drug, k = 6) +
        n_a1c + n_drug_classes + total_meds +
        sulfonylurea + insulin + metformin + sglt2 + glp1 +
        value_height + value_weight + bmi + bmi_x_male +
        # non_hdl + ldl_hdl_ratio + adi_discrepancy +
        # log_ed_visits + log_pcp_visits + log_admissions + log_total_meds +
        ed_visits + age + gender_male +
        adi_state + adi_nation + cad + copd +
        days_to_eval + time_gap +
        max_drop + max_rise + has_sudden_drop + has_sudden_rise +
        frac_above_7 + frac_above_8 +
        high7_insulin + high8_insulin +
        high7_no_meds + high8_no_meds +
        a1c_change_if_high7 + a1c_change_if_high8 +
        worsening_on_meds + improving_on_meds +
        stable_on_meds + on_modern_drugs,
    family = binomial,
    data = df_train_imputed
) # takes ~ 1 min
p2_tr <- predict(m2, df_train_imputed, type = "response")

p2_thresh <- best_f1_threshold(p2_tr, df_train_imputed$target == "Uncontrolled", df_train_imputed$mask)
p2_thresh
p2_val <- predict(m2, df_val_imputed, type = "response")
compute_metrics(p2_val > p2_thresh["best_th"], df_val_imputed$target == "Uncontrolled", df_val_imputed$mask)
prob_gam_val <- as.numeric(p2_val); th_gam <- p2_thresh["best_th"]


#### SAVE VALIDATION PREDICTIONS ####
# df_val here is the non-imputed version (from nonlinear section); has all features + id/target/mask
val_preds <- df_val %>%
    select(id, target, mask) %>%
    mutate(
        prob_glm = prob_glm_val,
        prob_glm_wt = prob_glm_wt_val,
        prob_tree = prob_tree_val,
        prob_gam = prob_gam_val,
        th_glm = th_glm,
        th_glm_wt = th_glm_wt,
        th_tree = th_tree,
        th_gam = th_gam
    )

saveRDS(list(val_preds = val_preds, val_features = df_val), file = "./data/processed/val_predictions.Rds")
cat("\n------ Saved val predictions to data/processed/val_predictions.Rds ---------\n")


#### ENSEMBLE ####
# 4-model (includes broken tree — for reference)
cat("\n------ Ensemble 4-model (GLM, GLM+wt, Tree, GAM) ---------\n")
prob_ens4_val <- (prob_glm_val + prob_glm_wt_val + prob_tree_val + prob_gam_val) / 4
ens4_thresh <- best_f1_threshold(prob_ens4_val, df_val$target == "Uncontrolled", df_val$mask)
ens4_thresh
compute_metrics(prob_ens4_val > ens4_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)

# 3-model (exclude tree — it went degenerate with dual-threshold features)
cat("\n------ Ensemble 3-model (GLM, GLM+wt, GAM) ---------\n")
prob_ens3_val <- (prob_glm_val + prob_glm_wt_val + prob_gam_val) / 3
ens3_thresh <- best_f1_threshold(prob_ens3_val, df_val$target == "Uncontrolled", df_val$mask)
ens3_thresh
compute_metrics(prob_ens3_val > ens3_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)

# 2-model (just the two GLMs — the strongest pair)
cat("\n------ Ensemble 2-model (GLM, GLM+wt) ---------\n")
prob_ens2_val <- (prob_glm_val + prob_glm_wt_val) / 2
ens2_thresh <- best_f1_threshold(prob_ens2_val, df_val$target == "Uncontrolled", df_val$mask)
ens2_thresh
compute_metrics(prob_ens2_val > ens2_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)


#### HEAVIER MODELS (slower — results from simple models already printed above) ####

# ------------------
# glmnet elastic net (tuned via 5-fold CV on train)
# ~2 mins

cat("\n------ GLMNET (elastic net, 5-fold CV) ---------\n")
library(caret)
library(glmnet)

# NOTE: df_train was reassigned to non-imputed version in nonlinear section above.
# glmnet needs _miss columns, so use df_train_imputed / df_val_imputed.
set.seed(2441139)
folds_tr <- createFolds(df_train_imputed$target, k = 5, returnTrain = TRUE)

f1_summary <- function(data, lev = NULL, model = NULL) {
    pos <- lev[2]
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

m_glmnet <- train(f_glm,
    data = df_train_imputed, method = "glmnet", family = "binomial",
    trControl = ctrl_cv, metric = "best_F1",
    tuneGrid = glmnet_grid,
    preProcess = c("center", "scale")
)

p_glmnet_train <- predict(m_glmnet, df_train_imputed, type = "prob")$Uncontrolled
p_glmnet_thresh <- best_f1_threshold(p_glmnet_train, df_train_imputed$target == "Uncontrolled", df_train_imputed$mask)
p_glmnet_thresh

p_glmnet_val <- predict(m_glmnet, df_val_imputed, type = "prob")$Uncontrolled
compute_metrics(p_glmnet_val > p_glmnet_thresh["best_th"], df_val_imputed$target == "Uncontrolled", df_val_imputed$mask)


# ---------------
# XGBoost
# xgboost handles NAs natively — uses non-imputed df_train/df_val

cat("\n------ XGBoost ---------\n")
library(xgboost)

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

m_xgb <- xgb.train(
    params = params,
    data = xgb_train,
    nrounds = 500,
    watchlist = list(train = xgb_train, val = xgb_val),
    early_stopping_rounds = 30,
    print_every_n = 50,
    verbose = 1
)

p_xgb_tr <- predict(m_xgb, xgb_train)
p_xgb_thresh <- best_f1_threshold(p_xgb_tr, df_train$target == "Uncontrolled", df_train$mask)
p_xgb_thresh

p_xgb_val <- predict(m_xgb, xgb_val)
compute_metrics(p_xgb_val > p_xgb_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)

#### XGBoost feature importance ####
cat("\n------ XGBoost Feature Importance (top 20) ---------\n")
imp <- xgb.importance(feature_names = feat_cols, model = m_xgb)
print(imp[1:min(20, nrow(imp)), ], row.names = FALSE)


# ---------------
# XGBoost (tighter regularization)
# Lower max_depth, higher min_child_weight to reduce train-val gap

cat("\n------ XGBoost (regularized) ---------\n")
params_reg <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = 3,
    eta = 0.05,
    subsample = 0.7,
    colsample_bytree = 0.6,
    min_child_weight = 20,
    gamma = 1
)

m_xgb_reg <- xgb.train(
    params = params_reg,
    data = xgb_train,
    nrounds = 500,
    watchlist = list(train = xgb_train, val = xgb_val),
    early_stopping_rounds = 30,
    print_every_n = 50,
    verbose = 1
)

p_xgb_reg_tr <- predict(m_xgb_reg, xgb_train)
p_xgb_reg_thresh <- best_f1_threshold(p_xgb_reg_tr, df_train$target == "Uncontrolled", df_train$mask)
p_xgb_reg_thresh

p_xgb_reg_val <- predict(m_xgb_reg, xgb_val)
compute_metrics(p_xgb_reg_val > p_xgb_reg_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)

cat("\n------ XGBoost (regularized) Feature Importance (top 20) ---------\n")
imp_reg <- xgb.importance(feature_names = feat_cols, model = m_xgb_reg)
print(imp_reg[1:min(20, nrow(imp_reg)), ], row.names = FALSE)
