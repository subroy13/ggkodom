# load packages and utility functions
library(tidyverse)
library(xgboost)

source("./scripts/utils.R")
source("./scripts/subrata_features.R")

# load the clean dataset
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
  "bmi", "bmi_x_male",
  "insulin_rate", "total_meds_rate", "visits_rate"
  # "non_hdl", "ldl_hdl_ratio",
  # "adi_discrepancy",
  # "log_ed_visits", "log_pcp_visits", "log_admissions", "log_total_meds"
)
dt_formula <- as.formula(paste("target ~", paste(feat_cols, collapse = " + ")))

# train xgboost
xgb_train <- xgb.DMatrix(
  data = as.matrix(df_train[, feat_cols]),
  label = df_train$target == "Uncontrolled"
)
xgb_val <- xgb.DMatrix(
  data = as.matrix(df_val[, feat_cols]),
  label = df_val$target == "Uncontrolled"
)


params_reg <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 5,
  eta = 0.05,
  subsample = 0.5,
  colsample_bytree = 0.5,
  min_child_weight = 10,
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

# calculate shapley values
library(fastshap)
library(ggplot2)
library(shapviz)

# Define a prediction wrapper for a specific transition (e.g., L to H)
# This forces the model to only use the self$head_L_to_H logic
pfun_xgb <- function(object, newdata) {
  xgb_new <- xgb.DMatrix(
    data = as.matrix(newdata[, feat_cols])
  )
  probs <- predict(object, xgb_new)
  return(as.numeric(probs))
}

X_train_matrix <- as.matrix(df_train[, feat_cols]) # Extract the feature matrix

# Create a background dataset (required for Shapley integration)
set.seed(42)
X_background <- X_train_matrix[sample(nrow(X_train_matrix), 200), ] 

# Filter for patients who actually started in State L for the evaluation
X_explain <- X_train_matrix[1:500, ] # Subset for computational speed

# Compute SHAP values
shap_L2H <- explain(
  m_xgb_reg,
  X = X_background,
  pred_wrapper = pfun_xgb,
  nsim = 50, # Increase for more stability, decrease for speed
  newdata = X_explain
)

# Illustrate the creation of important features (SHAP Summary Plot)
# We can use the SHAP matrix to create a traditional bee-swarm plot
sv <- shapviz(shap_L2H, X = X_explain)
sv_importance(sv, kind = "bee") + 
  theme_bw() #+
ggtitle("SHAP Values: Drivers of Low to High (Uncontrolled) Transition")
















