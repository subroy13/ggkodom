#### INTERPRETABLE MODEL: Residual-guided feature selection ####
#
# Story:
#   a1c_weighted alone → 61.25% F1 (1 feature captures 99% of signal)
#   Residual analysis identified 6 features with unique signal beyond a1c_weighted
#   This script fits the focused interpretable model and decomposes the signal
#
# For datathon presentation: explain WHY the ceiling is ~62%, not just "we got 62%"

library(tidyverse)
library(mgcv)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")

df_train <- impute_and_flag(create_features(train_dat))
df_val <- impute_and_flag(create_features(val_dat))

# train on ALL patients (including non-returned as known controlled)
# evaluate on returned patients only (competition metric)
y_train <- as.integer(df_train$target == "Uncontrolled")
y_val <- as.integer(df_val$target == "Uncontrolled")

cat("===================================================================\n")
cat("Train:", nrow(df_train), "(", sum(df_train$mask), "returned )\n")
cat("Val:", nrow(df_val), "(", sum(df_val$mask), "returned )\n")
cat("===================================================================\n\n")


#### MODEL 0: a1c_weighted alone (baseline) ####
cat("====== MODEL 0: a1c_weighted alone ======\n")
m0 <- glm(y_train ~ a1c_weighted, data = df_train, family = binomial)
p0_tr <- predict(m0, df_train, type = "response")
p0_va <- predict(m0, df_val, type = "response")
th0 <- best_f1_threshold(p0_tr, y_train, df_train$mask)
r0 <- compute_metrics(p0_va > th0["best_th"], y_val, df_val$mask)
f1_m0 <- r0$metrics["F1"]

cat("\nCoefficients:\n")
print(round(coef(m0), 4))
cat("Interpretation: each +1 unit in a1c_weighted → exp(0.86) = 2.4x odds of uncontrolled\n\n")


#### MODEL 1: a1c_weighted + top residual features (GLM) ####
# Features selected by residual analysis (LR test, BH-adjusted p < 1e-10):
#   frac_above_7:  "How often above clinical threshold?"     (LR = 659)
#   frac_above_8:  "How often above competition threshold?"  (LR = 299)
#   high7_insulin: "On insulin AND still above 7?"           (LR = 140)
#   n_crossings_8: "How often bouncing across 8?"            (LR = 94)
#   n_drug_classes:"How many drug types?"                    (LR = 77)
#   days_to_eval:  "When is the 2025 measurement?"           (LR = 27)

cat("====== MODEL 1: Focused interpretable GLM (7 features) ======\n")
m1 <- glm(y_train ~ a1c_weighted + frac_above_7 + frac_above_8 +
    high7_insulin + n_crossings_8 + n_drug_classes + days_to_eval,
    data = df_train, family = binomial)

p1_tr <- predict(m1, df_train, type = "response")
p1_va <- predict(m1, df_val, type = "response")
th1 <- best_f1_threshold(p1_tr, y_train, df_train$mask)
r1 <- compute_metrics(p1_va > th1["best_th"], y_val, df_val$mask)
f1_m1 <- r1$metrics["F1"]

cat("\nCoefficients:\n")
s1 <- summary(m1)
print(round(s1$coefficients[, c(1, 2, 4)], 4))


#### MODEL 1b: add the top interaction (a1c_weighted * frac_above_8) ####
cat("\n====== MODEL 1b: + a1c_weighted:frac_above_8 interaction ======\n")
m1b <- glm(y_train ~ a1c_weighted * frac_above_8 + frac_above_7 +
    high7_insulin + n_crossings_8 + n_drug_classes + days_to_eval,
    data = df_train, family = binomial)

p1b_tr <- predict(m1b, df_train, type = "response")
p1b_va <- predict(m1b, df_val, type = "response")
th1b <- best_f1_threshold(p1b_tr, y_train, df_train$mask)
r1b <- compute_metrics(p1b_va > th1b["best_th"], y_val, df_val$mask)
f1_m1b <- r1b$metrics["F1"]

cat("\nCoefficients:\n")
s1b <- summary(m1b)
print(round(s1b$coefficients[, c(1, 2, 4)], 4))


#### MODEL 2: GAM version (flexible a1c_weighted shape) ####
cat("\n====== MODEL 2: GAM with s(a1c_weighted) + focused features ======\n")
m2 <- gam(y_train ~ s(a1c_weighted, k = 10) + frac_above_7 + frac_above_8 +
    high7_insulin + n_crossings_8 + s(n_drug_classes, k = 5) + days_to_eval,
    data = df_train, family = binomial)

p2_tr <- predict(m2, df_train, type = "response")
p2_va <- predict(m2, df_val, type = "response")
th2 <- best_f1_threshold(p2_tr, y_train, df_train$mask)
r2 <- compute_metrics(p2_va > th2["best_th"], y_val, df_val$mask)
f1_m2 <- r2$metrics["F1"]

cat("\nParametric coefficients:\n")
s2 <- summary(m2)
print(round(s2$p.table[, c(1, 2, 4)], 4))
cat("\nSmooth terms:\n")
print(round(s2$s.table[, c(1, 3, 4)], 2))


#### MODEL 3: interpretable GLM expanded (~15 features) ####
# add the next tier from residual analysis: sulfonylurea, slope_above_8,
# gender_male, max_rise, insulin (as separate from high7_insulin)
cat("\n====== MODEL 3: Expanded interpretable GLM (13 features) ======\n")
m3 <- glm(y_train ~ a1c_weighted + frac_above_7 + frac_above_8 +
    high7_insulin + n_crossings_8 + n_drug_classes + days_to_eval +
    sulfonylurea + slope_above_8 + gender_male + max_rise + worsening_on_meds + n_a1c,
    data = df_train, family = binomial)

p3_tr <- predict(m3, df_train, type = "response")
p3_va <- predict(m3, df_val, type = "response")
th3 <- best_f1_threshold(p3_tr, y_train, df_train$mask)
r3 <- compute_metrics(p3_va > th3["best_th"], y_val, df_val$mask)
f1_m3 <- r3$metrics["F1"]

cat("\nCoefficients:\n")
s3 <- summary(m3)
print(round(s3$coefficients[, c(1, 2, 4)], 4))


#### INCREMENTAL ANALYSIS: what does each feature add? ####
cat("\n\n====== INCREMENTAL FEATURE CONTRIBUTION ======\n")
cat("Building up from a1c_weighted, adding one feature at a time\n")
cat("(order: by LR test statistic from residual analysis)\n\n")

feature_order <- c("frac_above_7", "frac_above_8", "high7_insulin",
    "n_crossings_8", "n_drug_classes", "days_to_eval",
    "sulfonylurea", "slope_above_8", "gender_male",
    "max_rise", "worsening_on_meds", "n_a1c")

cumulative_feats <- "a1c_weighted"
cat(sprintf("  %-5s %-25s  Val F1\n", "Step", "Feature added"))
cat(sprintf("  %-5s %-25s  %.2f%%\n", "0", "a1c_weighted", f1_m0 * 100))

for (i in seq_along(feature_order)) {
    feat <- feature_order[i]
    cumulative_feats <- c(cumulative_feats, feat)
    f_inc <- as.formula(paste("y_train ~", paste(cumulative_feats, collapse = " + ")))
    m_inc <- glm(f_inc, data = df_train, family = binomial)
    p_inc_tr <- predict(m_inc, df_train, type = "response")
    p_inc_va <- predict(m_inc, df_val, type = "response")
    th_inc <- best_f1_threshold(p_inc_tr, y_train, df_train$mask)
    r_inc <- compute_metrics(p_inc_va > th_inc["best_th"], y_val, df_val$mask, verbose = FALSE)
    cat(sprintf("  %-5d %-25s  %.2f%%\n", i, feat, r_inc$metrics["F1"] * 100))
}


#### CLINICAL INTERPRETATION ####
cat("\n\n====== CLINICAL INTERPRETATION (Model 1: 7 features) ======\n\n")

coefs <- coef(m1)
or <- exp(coefs)
cat("Feature                  | Coef    | Odds Ratio | Interpretation\n")
cat("-------------------------|---------|------------|---------------\n")
cat(sprintf("%-25s| %+.3f  | %.2f       | Overall A1c level (EWMA-weighted)\n",
    "a1c_weighted", coefs["a1c_weighted"], or["a1c_weighted"]))
cat(sprintf("%-25s| %+.3f  | %.2f       | Chronic uncontrol at clinical threshold\n",
    "frac_above_7", coefs["frac_above_7"], or["frac_above_7"]))
cat(sprintf("%-25s| %+.3f  | %.2f       | Chronic uncontrol at competition boundary\n",
    "frac_above_8", coefs["frac_above_8"], or["frac_above_8"]))
cat(sprintf("%-25s| %+.3f  | %.2f       | Treatment-resistant (insulin + still high)\n",
    "high7_insulin", coefs["high7_insulin"], or["high7_insulin"]))
cat(sprintf("%-25s| %+.3f  | %.2f       | Instability around competition threshold\n",
    "n_crossings_8", coefs["n_crossings_8"], or["n_crossings_8"]))
cat(sprintf("%-25s| %+.3f  | %.2f       | Treatment intensity (severity proxy)\n",
    "n_drug_classes", coefs["n_drug_classes"], or["n_drug_classes"]))
cat(sprintf("%-25s| %+.3f  | %.2f       | When 2025 measurement happens\n",
    "days_to_eval", coefs["days_to_eval"], or["days_to_eval"]))


#### FULL GLM REFERENCE ####
cat("\n\n====== REFERENCE: Full GLM (74+ features) ======\n")
f_full <- target ~
    a1c_latest + value_a1c_1 + a1c_weighted + abs_change + a1c_sd + a1c_range + a1c_rate +
    n_a1c + n_drug_classes + total_meds +
    sulfonylurea + insulin + metformin + sglt2 + glp1 + dpp4 +
    ed_visits + pcp_visits + admissions +
    age + gender_male +
    adi_state + adi_nation + cad + copd +
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

m_full <- glm(f_full, data = df_train, family = binomial)
pf_tr <- predict(m_full, df_train, type = "response")
pf_va <- predict(m_full, df_val, type = "response")
thf <- best_f1_threshold(pf_tr, df_train$target == "Uncontrolled", df_train$mask)
rf <- compute_metrics(pf_va > thf["best_th"], df_val$target == "Uncontrolled", df_val$mask)
f1_full <- rf$metrics["F1"]


#### FINAL SUMMARY ####
cat("\n\n====== FINAL SUMMARY ======\n\n")
cat("Model                              | Features | Val F1  | Delta from baseline\n")
cat("-----------------------------------|----------|---------|--------------------\n")
cat(sprintf("%-35s| %3d      | %.2f%%  | ---\n", "a1c_weighted alone", 1, f1_m0 * 100))
cat(sprintf("%-35s| %3d      | %.2f%%  | %+.2f%%\n", "Focused GLM (7 features)", 7, f1_m1 * 100, (f1_m1 - f1_m0) * 100))
cat(sprintf("%-35s| %3d      | %.2f%%  | %+.2f%%\n", "+ interaction", 8, f1_m1b * 100, (f1_m1b - f1_m0) * 100))
cat(sprintf("%-35s| %3d      | %.2f%%  | %+.2f%%\n", "GAM (flexible shape + features)", 7, f1_m2 * 100, (f1_m2 - f1_m0) * 100))
cat(sprintf("%-35s| %3d      | %.2f%%  | %+.2f%%\n", "Expanded GLM (13 features)", 13, f1_m3 * 100, (f1_m3 - f1_m0) * 100))
cat(sprintf("%-35s| %3s      | %.2f%%  | %+.2f%%\n", "Full GLM (reference)", "74+", f1_full * 100, (f1_full - f1_m0) * 100))
cat(sprintf("%-35s| %3s      | %.2f%%  | %+.2f%%\n", "XGBoost regularized (from log)", "74+", 62.2, (0.622 - f1_m0) * 100))

cat("\n")
cat("Key insight: a1c_weighted alone captures 99% of achievable signal.\n")
cat("6 additional features add ~0.5%. The remaining 67 features add another ~0.5%.\n")
cat("XGBoost's non-linearity adds another ~0.3%.\n")
cat("The ~62% ceiling is structural: FN = patients who deteriorate (unobserved),\n")
cat("FP = patients who respond to treatment (unobserved).\n")

cat("\n\n#### DONE ####\n")
