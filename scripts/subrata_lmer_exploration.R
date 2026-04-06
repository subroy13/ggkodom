#### LMER LONGITUDINAL EXPLORATION ####
# Can mixed model BLUPs add predictive signal beyond hand-crafted features (EWMA, SD)?
#
# Key idea: fit lmer on A1c trajectory (a1c_1..a1c_5), extract patient-specific:
#   - blup_intercept: baseline A1c level (shrinkage estimate)
#   - blup_slope: trajectory direction (shrinkage estimate)
#   - lmer_pred_a1c: predicted A1c at 2025 eval time
#   - blup_int_se, blup_slope_se: conditional SDs (uncertainty in BLUPs)
#
# Why shrinkage matters: for single-reading patients (the hardest to classify),
# a1c_weighted = their one reading. BLUPs shrink toward the population mean,
# giving a more conservative estimate. For multi-reading patients, BLUPs ≈ OLS.
#
# No target leakage: lmer uses a1c_1..a1c_5 only, NOT a1c_2025.
# Fitting on combined train+val is like computing EWMA on all features.
#
# Also briefly tests GEE (user asked). GEE gives population-averaged effects,
# no patient-specific BLUPs — less useful for prediction.

library(tidyverse)
library(lme4)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")

cat("============================================\n")
cat("  LMER LONGITUDINAL EXPLORATION\n")
cat("============================================\n\n")


#### Step 0: Load data ####
train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")


#### Step 1: Create long-format A1c data ####
# One row per (patient, visit). Time is "days from reference" (from cleaning.R).
make_a1c_long <- function(dat) {
    dat$measurements %>%
        filter(variable %in% paste0("a1c_", 1:5)) %>%
        filter(!is.na(value)) %>%
        arrange(id, time) %>%
        mutate(time_yr = time / 365.25) %>%
        group_by(id) %>%
        mutate(
            visit = row_number(),
            n_visits = n(),
            prev_above_8 = replace_na(as.integer(lag(value) >= 8), 0L),
            prev_above_7 = replace_na(as.integer(lag(value) >= 7), 0L)
        ) %>%
        ungroup()
}

a1c_train <- make_a1c_long(train_dat)
a1c_val   <- make_a1c_long(val_dat)
a1c_all   <- bind_rows(
    a1c_train %>% mutate(split = "train"),
    a1c_val   %>% mutate(split = "val")
)

cat(sprintf("Long-format A1c data:\n"))
cat(sprintf("  Train: %d rows, %d patients\n", nrow(a1c_train), n_distinct(a1c_train$id)))
cat(sprintf("  Val:   %d rows, %d patients\n", nrow(a1c_val), n_distinct(a1c_val$id)))
cat(sprintf("  Total: %d rows, %d patients\n", nrow(a1c_all), n_distinct(a1c_all$id)))

cat("\nVisits per patient:\n")
print(table(a1c_all %>% distinct(id, n_visits) %>% pull(n_visits)))

cat("\nTime (years) summary:\n"); print(summary(a1c_all$time_yr))
cat("\nA1c value summary:\n"); print(summary(a1c_all$value))

# How many single-reading patients? These are where shrinkage matters most
n_single <- sum(a1c_all %>% distinct(id, n_visits) %>% pull(n_visits) == 1)
cat(sprintf("\nSingle-reading patients: %d (%.1f%%)\n", n_single, 100 * n_single / n_distinct(a1c_all$id)))


#### Step 2: Fit lmer models ####
cat("\n\n============ FITTING LMER MODELS ============\n")
cat("(Fitting on combined train+val — no target used)\n\n")

# M1: Random intercept only
cat("--- M1: value ~ time_yr + (1 | id) ---\n")
t0 <- Sys.time()
m1 <- lmer(value ~ time_yr + (1 | id), data = a1c_all, REML = FALSE)
cat(sprintf("  Time: %.1f sec\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(sprintf("  AIC = %.1f, BIC = %.1f\n", AIC(m1), BIC(m1)))
cat(sprintf("  Rand int SD = %.3f, Resid SD = %.3f\n\n", sqrt(VarCorr(m1)$id[1, 1]), sigma(m1)))

# M2: Random intercept + slope — FAILS if n_obs < n_random_effects
# 57.6% of patients have 1 reading → can't estimate per-patient slopes for them
# Try on subset with ≥ 3 readings only
a1c_multi <- a1c_all %>% filter(n_visits >= 3)
cat(sprintf("--- M2: int+slope on ≥3 reading patients (%d rows, %d patients) ---\n",
    nrow(a1c_multi), n_distinct(a1c_multi$id)))
t0 <- Sys.time()
m2_sub <- lmer(value ~ time_yr + (1 + time_yr | id), data = a1c_multi, REML = FALSE,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000)))
cat(sprintf("  Time: %.1f sec\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(sprintf("  AIC = %.1f, BIC = %.1f\n", AIC(m2_sub), BIC(m2_sub)))
vc2 <- VarCorr(m2_sub)$id
cat(sprintf("  Rand int SD = %.3f, Rand slope SD = %.3f, Corr = %.3f\n",
    sqrt(vc2[1, 1]), sqrt(vc2[2, 2]), attr(vc2, "correlation")[1, 2]))
cat(sprintf("  Resid SD = %.3f\n\n", sigma(m2_sub)))

# M3: Treatment flag (prev_above_8), random intercept only (on all patients)
cat("--- M3: value ~ time_yr + prev_above_8 + (1 | id) ---\n")
t0 <- Sys.time()
m3 <- lmer(value ~ time_yr + prev_above_8 + (1 | id), data = a1c_all, REML = FALSE)
cat(sprintf("  Time: %.1f sec\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(sprintf("  AIC = %.1f, BIC = %.1f\n", AIC(m3), BIC(m3)))
cat("  Fixed effects:\n"); print(round(fixef(m3), 4))
cat("\n")

# M4: Treatment × time, random intercept only
cat("--- M4: value ~ time_yr * prev_above_8 + (1 | id) ---\n")
t0 <- Sys.time()
m4 <- lmer(value ~ time_yr * prev_above_8 + (1 | id), data = a1c_all, REML = FALSE)
cat(sprintf("  Time: %.1f sec\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(sprintf("  AIC = %.1f, BIC = %.1f\n", AIC(m4), BIC(m4)))
cat("  Fixed effects:\n"); print(round(fixef(m4), 4))
cat("\n")

cat("\n============ MODEL COMPARISON ============\n")
cat(sprintf("  M1 (int only, all):    AIC = %8.1f   BIC = %8.1f\n", AIC(m1), BIC(m1)))
cat(sprintf("  M2 (int+slope, ≥3):    AIC = %8.1f   BIC = %8.1f  (on subset — not directly comparable)\n", AIC(m2_sub), BIC(m2_sub)))
cat(sprintf("  M3 (+treat, all):      AIC = %8.1f   BIC = %8.1f\n", AIC(m3), BIC(m3)))
cat(sprintf("  M4 (+treat×t, all):    AIC = %8.1f   BIC = %8.1f\n", AIC(m4), BIC(m4)))
cat("\n")
print(summary(m1))


#### Step 3: Extract BLUPs ####
# M1 has random intercept only (no random slope — 58% have 1 reading)
# M2_sub has both but only for ≥3 reading patients
# Strategy: use M1 for all patients, add M2_sub slope BLUPs where available
cat("\n\n============ EXTRACTING BLUPs ============\n")

# --- M1 BLUPs (intercept only, all patients) ---
cat("--- From M1 (all patients) ---\n")
best_mod <- m1
re1 <- ranef(best_mod, condVar = TRUE)$id
cv1 <- attr(re1, "postVar")  # [1, 1, n_patients]

blup_df <- tibble(
    id             = as.integer(rownames(re1)),
    blup_intercept = re1[, "(Intercept)"],
    blup_int_se    = sqrt(cv1[1, 1, ])
)

cat("BLUP intercept (deviation from pop mean):\n"); print(summary(blup_df$blup_intercept))
cat("BLUP int SE (uncertainty):\n");                print(summary(blup_df$blup_int_se))

# Shrinkage factor for single-reading patients
var_int <- VarCorr(m1)$id[1, 1]
var_res <- sigma(m1)^2
shrink_1 <- var_int / (var_int + var_res)
cat(sprintf("\nShrinkage factor for n=1 patients: %.3f\n", shrink_1))
cat(sprintf("  → BLUP = %.1f%% of raw value + %.1f%% of population mean\n", 100 * shrink_1, 100 * (1 - shrink_1)))

# --- M2_sub BLUPs (intercept + slope, ≥3 reading patients only) ---
cat("\n--- From M2_sub (≥3 reading patients only) ---\n")
re2 <- ranef(m2_sub, condVar = TRUE)$id
cv2 <- attr(re2, "postVar")

blup_slope_df <- tibble(
    id             = as.integer(rownames(re2)),
    blup_slope     = re2[, "time_yr"],
    blup_slope_se  = sqrt(cv2[2, 2, ])
)
cat("BLUP slope (≥3 readings only):\n"); print(summary(blup_slope_df$blup_slope))

# Merge: all patients get M1 intercept, ≥3 reading patients also get slope
blup_df <- blup_df %>%
    left_join(blup_slope_df, by = "id") %>%
    mutate(
        blup_slope    = replace_na(blup_slope, 0),
        blup_slope_se = replace_na(blup_slope_se, max(blup_slope_se, na.rm = TRUE))  # max SE = most uncertain
    )


#### Step 4: Predicted A1c at 2025 eval time ####
# For all patients: pred = (fixef_int + blup_int) + fixef_slope * t_eval
# For ≥3 reading patients: also add blup_slope * t_eval
cat("\n\n============ PREDICTED A1c AT 2025 ============\n")

eval_times <- bind_rows(
    train_dat$measurements %>% filter(variable == "a1c_2025") %>% select(id, time),
    val_dat$measurements   %>% filter(variable == "a1c_2025") %>% select(id, time)
) %>%
    rename(eval_time = time) %>%
    mutate(eval_time_yr = eval_time / 365.25)

fe <- fixef(m1)
cat("M1 fixed effects:\n"); print(fe)

fe2 <- fixef(m2_sub)
cat("M2_sub fixed effects:\n"); print(fe2)

blup_df <- blup_df %>%
    left_join(eval_times, by = "id") %>%
    mutate(
        total_intercept = fe["(Intercept)"] + blup_intercept,
        total_slope     = fe["time_yr"] + blup_slope,   # M1 fixed slope + M2_sub BLUP slope (0 for ≤2 readings)
        lmer_pred_a1c   = total_intercept + total_slope * eval_time_yr
    )

cat("\nPredicted A1c at 2025 (returned patients only):\n")
print(summary(blup_df %>% filter(!is.na(eval_time_yr)) %>% pull(lmer_pred_a1c)))


#### Step 5: How do BLUPs relate to existing features? ####
cat("\n\n============ BLUP vs EXISTING FEATURES ============\n")

df_train_raw <- create_features(train_dat)
df_val_raw   <- create_features(val_dat)

check_df <- bind_rows(df_train_raw, df_val_raw) %>%
    select(id, a1c_weighted, a1c_latest, value_a1c_1, a1c_sd, a1c_rate, n_a1c) %>%
    left_join(blup_df %>% select(id, blup_intercept, blup_slope, total_intercept,
                                 lmer_pred_a1c, blup_int_se), by = "id")

cm <- cor(check_df %>% select(-id), use = "pairwise.complete.obs")

cat("\nCorrelations with BLUPs:\n")
existing <- c("a1c_weighted", "a1c_latest", "value_a1c_1", "a1c_sd", "a1c_rate", "n_a1c")
blup_vars <- c("blup_intercept", "total_intercept", "blup_slope", "lmer_pred_a1c", "blup_int_se")
for (bv in blup_vars) {
    cat(sprintf("\n  %s:\n", bv))
    for (ev in existing) {
        cat(sprintf("    vs %-15s  r = %+.3f\n", ev, cm[bv, ev]))
    }
}

cat("\n*** If total_intercept vs a1c_weighted has r > 0.95, BLUPs are mostly redundant ***\n")
cat(sprintf("*** Actual r(total_intercept, a1c_weighted) = %.3f ***\n",
    cm["total_intercept", "a1c_weighted"]))


#### Step 6: Subgroup check — do BLUPs differ for single vs multi-reading patients? ####
cat("\n\n============ SHRINKAGE BY N_READINGS ============\n")
# For single-reading patients, BLUP is shrunk. Is it meaningfully different from raw value?

shrink_check <- check_df %>%
    group_by(n_a1c) %>%
    summarize(
        n = n(),
        mean_a1c_weighted = mean(a1c_weighted, na.rm = TRUE),
        mean_total_int    = mean(total_intercept, na.rm = TRUE),
        sd_a1c_weighted   = sd(a1c_weighted, na.rm = TRUE),
        sd_total_int      = sd(total_intercept, na.rm = TRUE),
        mean_int_se       = mean(blup_int_se, na.rm = TRUE),
        cor_aw_ti         = cor(a1c_weighted, total_intercept, use = "complete.obs"),
        .groups = "drop"
    )
cat("\nShrinkage pattern by number of A1c readings:\n")
print(shrink_check, width = 120)
cat("\nNote: lower cor for n_a1c=1 means more shrinkage (BLUP ≠ raw value)\n")
cat("Higher blup_int_se for n_a1c=1 means more uncertainty\n")


#### Step 7: Classification evaluation ####
cat("\n\n============ CLASSIFIER EVALUATION ============\n")

# Prepare features
add_blup_features <- function(df, blup_df) {
    df %>%
        left_join(
            blup_df %>% select(id, blup_intercept, blup_slope, total_intercept,
                              total_slope, blup_int_se, blup_slope_se, lmer_pred_a1c),
            by = "id"
        ) %>%
        mutate(across(c(blup_intercept, blup_slope, total_intercept, total_slope,
                       blup_int_se, blup_slope_se, lmer_pred_a1c),
                     ~replace_na(., 0)))
}

df_train_imp  <- impute_and_flag(df_train_raw)
df_val_imp    <- impute_and_flag(df_val_raw)
df_train_blup <- add_blup_features(df_train_imp, blup_df)
df_val_blup   <- add_blup_features(df_val_imp, blup_df)

# feat_cols for XGBoost (matches subrata_models.R)
feat_cols <- c(
    "a1c_latest", "value_a1c_1", "a1c_weighted", "abs_change", "a1c_sd",
    "a1c_range", "a1c_rate", "a1c_change",
    "n_a1c", "n_drug_classes", "total_meds",
    "sulfonylurea", "insulin", "metformin", "sglt2", "glp1", "dpp4",
    "ed_visits", "pcp_visits", "admissions",
    "age", "gender_male",
    "adi_state", "adi_nation", "cad", "copd",
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

blup_feat_names <- c("blup_intercept", "blup_slope", "total_intercept",
                     "total_slope", "blup_int_se", "blup_slope_se", "lmer_pred_a1c")
feat_cols_blup <- c(feat_cols, blup_feat_names)

# GLM formula (with missingness flags)
f_glm <- target ~
    a1c_latest + value_a1c_1 + a1c_weighted + abs_change + a1c_sd + a1c_range + a1c_rate +
    n_a1c + n_drug_classes + total_meds +
    sulfonylurea + insulin + metformin + sglt2 + glp1 + dpp4 +
    ed_visits + pcp_visits + admissions +
    age + gender_male +
    adi_state + adi_nation + cad + copd +
    race_black + race_white + race_asian + ethnicity_hl +
    value_hdl + value_ldl + value_chol + value_height + value_weight +
    time_weight + time_height + time_ldl + time_hdl +
    days_to_eval + time_gap +
    max_drop + max_rise + has_sudden_drop + has_sudden_rise +
    frac_above_7 + n_crossings_7 + slope_above_7 + slope_below_7 +
    frac_above_8 + n_crossings_8 + slope_above_8 + slope_below_8 +
    post_drop_drift + sudden_drop_still_high7 + sudden_drop_still_high8 +
    high7_insulin + high7_no_meds + still_high7_improving + a1c_change_if_high7 +
    high8_insulin + high8_no_meds + still_high8_improving + a1c_change_if_high8 +
    a1c_x_meds + a1c_x_ndrug + a1c_change + a1c_per_drug +
    improving_on_meds + worsening_on_meds + stable_on_meds +
    on_modern_drugs + n_a1c_x_ndrug +
    abs_change_miss + a1c_sd_miss + a1c_range_miss + a1c_rate_miss +
    value_hdl_miss + value_ldl_miss + value_chol_miss +
    value_height_miss + value_weight_miss +
    time_weight_miss + time_height_miss + time_ldl_miss + time_hdl_miss +
    adi_state_miss + adi_nation_miss +
    time_gap_miss + max_drop_miss + max_rise_miss +
    slope_above_7_miss + slope_below_7_miss + slope_above_8_miss + slope_below_8_miss +
    post_drop_drift_miss + bmi + bmi_x_male + bmi_miss +
    insulin_rate + total_meds_rate + visits_rate

f_glm_blup <- update(f_glm, . ~ . + blup_intercept + blup_slope + blup_int_se + blup_slope_se + lmer_pred_a1c)


# --- 7a: Baseline GLM (no BLUPs) for comparison ---
cat("\n------ GLM baseline (no BLUPs) ------\n")
m_base <- glm(f_glm, data = df_train_blup, family = binomial)
p_tr <- predict(m_base, df_train_blup, type = "response")
th_base <- best_f1_threshold(p_tr, df_train_blup$target == "Uncontrolled", df_train_blup$mask)
p_va <- predict(m_base, df_val_blup, type = "response")
cat("Val:\n")
compute_metrics(p_va > th_base["best_th"], df_val_blup$target == "Uncontrolled", df_val_blup$mask)


# --- 7b: GLM + BLUPs ---
cat("\n------ GLM + BLUPs ------\n")
m_glm_b <- glm(f_glm_blup, data = df_train_blup, family = binomial)
p_tr <- predict(m_glm_b, df_train_blup, type = "response")
th_b <- best_f1_threshold(p_tr, df_train_blup$target == "Uncontrolled", df_train_blup$mask)
cat(sprintf("Train best F1: %.4f (th=%.3f)\n", th_b["best_f1"], th_b["best_th"]))
p_va <- predict(m_glm_b, df_val_blup, type = "response")
cat("Val:\n")
compute_metrics(p_va > th_b["best_th"], df_val_blup$target == "Uncontrolled", df_val_blup$mask)

# Check: which BLUP coefficients are significant?
cat("\nBLUP feature coefficients in GLM:\n")
blup_coefs <- summary(m_glm_b)$coefficients
blup_rows <- rownames(blup_coefs) %in% c("blup_intercept", "blup_slope", "blup_int_se", "blup_slope_se", "lmer_pred_a1c")
print(round(blup_coefs[blup_rows, ], 4))


# --- 7c: GLM + class weights + BLUPs ---
cat("\n------ GLM + weights + BLUPs ------\n")
wts <- ifelse(df_train_blup$target == "Uncontrolled",
              1 - mean(df_train_blup$target == "Uncontrolled"),
              mean(df_train_blup$target == "Uncontrolled"))
m_glmw_b <- glm(f_glm_blup, data = df_train_blup, family = binomial, weights = wts)
p_tr <- predict(m_glmw_b, df_train_blup, type = "response")
th_w <- best_f1_threshold(p_tr, df_train_blup$target == "Uncontrolled", df_train_blup$mask)
p_va <- predict(m_glmw_b, df_val_blup, type = "response")
cat("Val:\n")
compute_metrics(p_va > th_w["best_th"], df_val_blup$target == "Uncontrolled", df_val_blup$mask)


# --- 7d: XGBoost + BLUPs (regularized) ---
cat("\n------ XGBoost (reg) + BLUPs ------\n")
library(xgboost)

xgb_tr <- xgb.DMatrix(data = as.matrix(df_train_blup[, feat_cols_blup]), label = df_train_blup$target == "Uncontrolled")
xgb_va <- xgb.DMatrix(data = as.matrix(df_val_blup[, feat_cols_blup]),   label = df_val_blup$target == "Uncontrolled")

params <- list(objective = "binary:logistic", eval_metric = "logloss",
               max_depth = 3, eta = 0.05, subsample = 0.7, colsample_bytree = 0.6,
               min_child_weight = 20, gamma = 1)

m_xgb_b <- xgb.train(params, xgb_tr, nrounds = 500,
                      watchlist = list(train = xgb_tr, val = xgb_va),
                      early_stopping_rounds = 30, print_every_n = 50, verbose = 1)

p_tr <- predict(m_xgb_b, xgb_tr)
th_xgb <- best_f1_threshold(p_tr, df_train_blup$target == "Uncontrolled", df_train_blup$mask)
p_va <- predict(m_xgb_b, xgb_va)
cat("XGBoost + BLUPs val:\n")
compute_metrics(p_va > th_xgb["best_th"], df_val_blup$target == "Uncontrolled", df_val_blup$mask)

cat("\n--- Feature importance (top 25) ---\n")
imp <- xgb.importance(feature_names = feat_cols_blup, model = m_xgb_b)
print(imp[1:min(25, nrow(imp)), ], row.names = FALSE)

# Highlight: where do BLUP features rank?
cat("\n--- BLUP features in importance ---\n")
blup_imp <- imp %>% filter(Feature %in% blup_feat_names)
if (nrow(blup_imp) > 0) print(blup_imp, row.names = FALSE) else cat("  (none in top features)\n")


# --- 7e: XGBoost baseline (no BLUPs) for clean comparison ---
cat("\n------ XGBoost (reg) baseline (no BLUPs) ------\n")
xgb_tr0 <- xgb.DMatrix(data = as.matrix(df_train_blup[, feat_cols]), label = df_train_blup$target == "Uncontrolled")
xgb_va0 <- xgb.DMatrix(data = as.matrix(df_val_blup[, feat_cols]),   label = df_val_blup$target == "Uncontrolled")

m_xgb_0 <- xgb.train(params, xgb_tr0, nrounds = 500,
                      watchlist = list(train = xgb_tr0, val = xgb_va0),
                      early_stopping_rounds = 30, print_every_n = 50, verbose = 1)
p_tr0 <- predict(m_xgb_0, xgb_tr0)
th0 <- best_f1_threshold(p_tr0, df_train_blup$target == "Uncontrolled", df_train_blup$mask)
p_va0 <- predict(m_xgb_0, xgb_va0)
cat("XGBoost baseline val:\n")
compute_metrics(p_va0 > th0["best_th"], df_val_blup$target == "Uncontrolled", df_val_blup$mask)


#### Step 8: Single-feature head-to-head ####
cat("\n\n============ SINGLE FEATURE COMPARISON ============\n")
cat("Which single feature predicts best?\n\n")

single_test <- function(feat_name, df_tr, df_va) {
    f <- as.formula(paste("target ~", feat_name))
    m <- tryCatch({
        glm(f, data = df_tr, family = binomial)
    }, error = function(e) NULL)
    if (is.null(m)) { cat(sprintf("  %-20s → FAILED\n", feat_name)); return(invisible()) }

    p_tr <- predict(m, df_tr, type = "response")
    th <- best_f1_threshold(p_tr, df_tr$target == "Uncontrolled", df_tr$mask)
    p_va <- predict(m, df_va, type = "response")
    res <- compute_metrics(p_va > th["best_th"], df_va$target == "Uncontrolled", df_va$mask, verbose = FALSE)
    cat(sprintf("  %-20s → Val F1 = %.4f  (Prec=%.3f, Rec=%.3f)\n",
        feat_name, res$metrics["F1"], res$metrics["Precision"], res$metrics["Recall"]))
}

single_test("a1c_weighted",    df_train_blup, df_val_blup)
single_test("a1c_latest",      df_train_blup, df_val_blup)
single_test("total_intercept",  df_train_blup, df_val_blup)
single_test("lmer_pred_a1c",   df_train_blup, df_val_blup)
single_test("blup_int_se",     df_train_blup, df_val_blup)
single_test("blup_intercept",  df_train_blup, df_val_blup)


#### Step 9: GEE for comparison ####
cat("\n\n============ GEE (geeglm) ============\n")
cat("GEE gives population-averaged effects, no patient-specific BLUPs.\n")
cat("For prediction, lmer is better. Fitting GEE for comparison of fixed effects.\n\n")

tryCatch({
    library(geepack)

    # geeglm needs sorted data by id
    a1c_sorted <- a1c_all %>% arrange(id, time_yr)

    t0 <- Sys.time()
    gee1 <- geeglm(value ~ time_yr, id = id, data = a1c_sorted,
                    family = gaussian, corstr = "ar1")
    cat(sprintf("GEE fit time: %.1f sec\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

    cat("\nGEE (AR-1) coefficients:\n")
    print(summary(gee1)$coefficients)

    cat("\nCompare with lmer fixed effects:\n")
    cat(sprintf("  lmer: intercept = %.4f, slope = %.4f\n", fe["(Intercept)"], fe["time_yr"]))
    gee_coef <- coef(gee1)
    cat(sprintf("  GEE:  intercept = %.4f, slope = %.4f\n", gee_coef["(Intercept)"], gee_coef["time_yr"]))
    cat("\nGEE and lmer fixed effects should be similar.\n")
    cat("GEE's advantage: robust (sandwich) SEs. But no patient-specific predictions.\n")

    # AR(1) correlation estimate
    cat(sprintf("\nEstimated AR(1) correlation: %.3f\n", summary(gee1)$corr[[1]]))

}, error = function(e) {
    cat(sprintf("GEE failed: %s\n", e$message))
    cat("Install geepack: install.packages('geepack')\n")
})


#### Summary ####
cat("\n\n============ SUMMARY ============\n")
cat("Key questions answered by this script:\n")
cat("  1. Do BLUPs correlate highly with a1c_weighted? → check Step 5\n")
cat("  2. Does shrinkage matter for sparse patients? → check Step 6\n")
cat("  3. Do BLUP features improve GLM/XGBoost? → check Step 7\n")
cat("  4. Is lmer_pred_a1c a better single predictor than a1c_weighted? → check Step 8\n")
cat("  5. How do GEE fixed effects compare? → check Step 9\n")
cat("\nIf BLUPs don't help: the hand-crafted features already capture trajectory well.\n")
cat("If BLUPs help: the shrinkage / uncertainty features add genuinely new info.\n")
cat("\n============ DONE ============\n")
