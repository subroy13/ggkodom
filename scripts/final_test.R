library(tidyverse)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")


train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")
test_dat <- readRDS("./data/processed/test.Rds")

full_train_dat <- merge_dataset(merge_dataset(train_dat, val_dat), test_dat)
final_test_dat <- readRDS("./data/processed/final_test.Rds")


# create the xgboost features
df_train <- create_features(full_train_dat)
df_test <- create_features(final_test_dat)


# make formula
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


# train regularized xgboost on full training data
xgb_train <- xgb.DMatrix(
    data = as.matrix(df_train[, feat_cols]),
    label = df_train$target == "Uncontrolled"
)
xgb_test <- xgb.DMatrix(
    data = as.matrix(df_test[, feat_cols]),
    label = df_test$target == "Uncontrolled"
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
    watchlist = list(train = xgb_train),
    early_stopping_rounds = 30,
    print_every_n = 50,
    verbose = 1
)

p_xgb_reg_tr <- predict(m_xgb_reg, xgb_train)
p_xgb_reg_thresh <- best_f1_threshold(p_xgb_reg_tr, df_train$target == "Uncontrolled", df_train$mask)
p_xgb_reg_thresh

# filter the mask and set prob to 0 if a1c_2025_time is missing
p_train <- ifelse(df_train$mask == TRUE, p_xgb_reg_tr, 0)
compute_metrics(p_train > p_xgb_reg_thresh["best_th"], df_train$target == "Uncontrolled")

# predict for the testing points
p_xgb_reg_test <- predict(m_xgb_reg, xgb_test)
p_test <- ifelse(df_test$mask == TRUE, p_xgb_reg_test, 0)

pred_df <- tibble(
    `Id` = df_test$id,
    `a1c 2025 Uncontrolled` = (p_test > p_xgb_reg_thresh["best_th"]),
    `Probability of Uncontrolled` = p_test
)
pred_df %>% write_csv("./data/final/xgboost_submission.csv")
