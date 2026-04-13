###############################################################################
## days_to_eval audit — what is this variable actually measuring?
##
## XGBoost regularized gives `days_to_eval` 28% gain, more than a1c_weighted (13%).
## Subrata's hypothesis: it's a confound with n_a1c / time_latest_a1c / time_gap,
## not a real clinical signal. This script tests that end-to-end.
##
## Tests:
##   1. Range of days_to_eval and time_a1c_1 — settles "days from reference"
##      interpretation (per-patient anchor vs global anchor).
##   2. Correlations of days_to_eval with n_a1c, time_latest_a1c, time_a1c_1,
##      time_gap, a1c_weighted. High correlation = confound.
##   3. Conditional mean of target across deciles of days_to_eval. Monotone?
##      U-shaped? Step-like?
##   4. XGBoost with days_to_eval alone vs without days_to_eval.
##      If removing it doesn't hurt F1, the signal lives elsewhere.
###############################################################################

library(tidyverse)
library(xgboost)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")

df_train <- create_features(train_dat)
df_val   <- create_features(val_dat)

cat("\n#### 1. RANGE AND DISTRIBUTION ####\n")
cat("\nTrain rows:", nrow(df_train), "  Val rows:", nrow(df_val), "\n")
cat("\ndays_to_eval summary (train):\n")
print(summary(df_train$days_to_eval))
cat("\ntime_a1c_1 summary (train):\n")
print(summary(df_train$time_a1c_1))
cat("\ntime_latest_a1c summary (train):\n")
print(summary(df_train$time_latest_a1c))
cat("\ntime_gap summary (train):\n")
print(summary(df_train$time_gap))

cat("\n--- Interpretation test ---\n")
cat("If reference = per-patient anchor (first appointment), time_a1c_1 should be ~0.\n")
cat("If reference = global anchor, time_a1c_1 should vary across the cohort.\n\n")
cat("time_a1c_1 == 0 count:", sum(df_train$time_a1c_1 == 0, na.rm = TRUE), "\n")
cat("time_a1c_1 IQR:", IQR(df_train$time_a1c_1, na.rm = TRUE), "\n")
cat("time_a1c_1 SD:", sd(df_train$time_a1c_1, na.rm = TRUE), "\n")


cat("\n#### 2. CORRELATIONS ####\n")
cor_vars <- c("days_to_eval", "time_a1c_1", "time_latest_a1c", "time_gap",
              "n_a1c", "a1c_weighted", "a1c_latest", "a1c_sd", "frac_above_8")
cor_df <- df_train %>% select(all_of(cor_vars))
cat("\nCorrelations (pairwise.complete.obs):\n")
print(round(cor(cor_df, use = "pairwise.complete.obs"), 3))


cat("\n#### 3. CONDITIONAL TARGET MEAN BY DAYS_TO_EVAL DECILE ####\n")
# returned patients only, to match competition scoring
df_ret <- df_train %>% filter(mask == TRUE)
df_ret$y <- as.integer(df_ret$target == "Uncontrolled")
df_ret$decile <- ntile(df_ret$days_to_eval, 10)

dec_summary <- df_ret %>%
    group_by(decile) %>%
    summarise(
        n = n(),
        d2e_min = min(days_to_eval, na.rm = TRUE),
        d2e_max = max(days_to_eval, na.rm = TRUE),
        mean_y  = mean(y),
        mean_a1c_weighted = mean(a1c_weighted, na.rm = TRUE),
        mean_n_a1c = mean(n_a1c, na.rm = TRUE),
        mean_time_latest = mean(time_latest_a1c, na.rm = TRUE),
        mean_time_gap = mean(time_gap, na.rm = TRUE)
    )
cat("\nReturned-patient target rate across days_to_eval deciles (train):\n")
print(dec_summary, n = 10)


cat("\n#### 4. XGBOOST WITH vs WITHOUT days_to_eval ####\n")

df_train_imp <- impute_and_flag(df_train)
df_val_imp   <- impute_and_flag(df_val)

feat_full <- c(
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
)

fit_xgb <- function(feat_cols, tag) {
    cat("\n---", tag, "(", length(feat_cols), "features) ---\n")
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
        max_depth = 5, eta = 0.05,
        subsample = 0.5, colsample_bytree = 0.5,
        min_child_weight = 10, gamma = 1
    )
    set.seed(1)
    m <- xgb.train(
        params = params, data = xgb_train, nrounds = 500,
        watchlist = list(train = xgb_train, val = xgb_val),
        early_stopping_rounds = 30, print_every_n = 100, verbose = 1
    )
    p_tr  <- predict(m, xgb_train)
    p_val <- predict(m, xgb_val)
    th <- best_f1_threshold(p_tr, df_train$target == "Uncontrolled", df_train$mask)
    cat("train-tuned threshold:", round(th["best_th"], 3), "\n")
    met <- compute_metrics(p_val > th["best_th"], df_val$target == "Uncontrolled", df_val$mask)
    # xgb.importance chokes on single-feature models — skip it in that case
    if (length(feat_cols) > 1) {
        imp <- try(xgb.importance(feature_names = feat_cols, model = m), silent = TRUE)
        if (!inherits(imp, "try-error")) {
            cat("top 10 features by gain:\n")
            print(imp[1:min(10, nrow(imp)), ], row.names = FALSE)
        }
    } else {
        imp <- NULL
        cat("(single-feature model — no importance table)\n")
    }
    invisible(list(model = m, metrics = met, imp = imp))
}

res_full <- fit_xgb(feat_full, "XGBoost FULL (baseline ~62.0%)")
res_nod2e <- fit_xgb(setdiff(feat_full, "days_to_eval"), "XGBoost FULL minus days_to_eval")
res_d2eonly <- fit_xgb("days_to_eval", "XGBoost days_to_eval ALONE")
res_gapnod2e <- fit_xgb(setdiff(feat_full, c("days_to_eval", "time_gap")),
                        "XGBoost FULL minus days_to_eval AND time_gap")

cat("\n#### 5. SUMMARY ####\n")
cat("If dropping days_to_eval doesn't move F1 by more than ~0.3 pp, it's a confound.\n")
cat("If dropping both days_to_eval and time_gap matters, the TEMPORAL signal is real\n")
cat("but is carried by time_gap (the clinically meaningful one), not days_to_eval.\n")
