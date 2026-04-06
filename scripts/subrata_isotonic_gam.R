#### ISOTONIC REGRESSION + FOCUSED GAM: fixing the sigmoid ####
#
# The residual analysis showed:
#   1. The logistic sigmoid is too smooth (overpredicts low, underpredicts high)
#   2. Non-linear signal exists in residuals (a1c_x_ndrug, slope_above_8)
#   3. Massive interactions with a1c_weighted (frac_above_8, high8_insulin)
#
# Strategy: replace the smooth sigmoid with a flexible monotone function,
# then add a focused set of features (not 38, just the top ones)

library(tidyverse)
library(mgcv)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")

df_train <- impute_and_flag(create_features(train_dat))
df_val <- impute_and_flag(create_features(val_dat))

df_tr <- df_train %>% filter(mask)
df_va <- df_val %>% filter(mask)
y_tr <- as.integer(df_tr$target == "Uncontrolled")
y_va <- as.integer(df_va$target == "Uncontrolled")

cat("Train:", nrow(df_tr), "| Val:", nrow(df_va), "\n")
cat("Pos rate train:", round(mean(y_tr), 3), "| val:", round(mean(y_va), 3), "\n\n")


#### BASELINE: logistic sigmoid (from residual analysis) ####
cat("====== BASELINE: logistic sigmoid on a1c_weighted ======\n")
m_logistic <- glm(y_tr ~ a1c_weighted, data = df_tr, family = binomial)
p_log_tr <- predict(m_logistic, df_tr, type = "response")
p_log_va <- predict(m_logistic, df_va, type = "response")
th_log <- best_f1_threshold(p_log_tr, y_tr)
cat("Val:\n")
compute_metrics(p_log_va > th_log["best_th"], y_va)


#### APPROACH 1: ISOTONIC REGRESSION on a1c_weighted ####
cat("\n====== APPROACH 1: Isotonic regression on a1c_weighted ======\n")

# isotonic regression: monotone non-decreasing step function
# P(Y=1 | a1c_weighted) = f(a1c_weighted), f monotone
iso <- isoreg(df_tr$a1c_weighted, y_tr)

# create prediction function via linear interpolation of the step function
ord <- order(df_tr$a1c_weighted)
x_sorted <- df_tr$a1c_weighted[ord]
y_fitted <- iso$yf  # fitted values in sorted order

# use approxfun with linear interpolation (smoother than steps for prediction)
iso_predict <- approxfun(x_sorted, y_fitted, method = "linear", rule = 2, ties = mean)

p_iso_tr <- iso_predict(df_tr$a1c_weighted)
p_iso_va <- iso_predict(df_va$a1c_weighted)

th_iso <- best_f1_threshold(p_iso_tr, y_tr)
cat("Val:\n")
compute_metrics(p_iso_va > th_iso["best_th"], y_va)

# compare calibration: isotonic vs logistic at regime boundaries
cat("\nCalibration comparison (isotonic vs logistic):\n")
regimes <- cut(df_tr$a1c_weighted, breaks = c(0, 6, 7, 8, 20), labels = c("<6", "6-7", "7-8", ">=8"))
cal_comp <- data.frame(
    regime = levels(regimes),
    actual = tapply(y_tr, regimes, mean),
    logistic = tapply(p_log_tr, regimes, mean),
    isotonic = tapply(p_iso_tr, regimes, mean)
)
cal_comp$logistic_bias <- round(cal_comp$logistic - cal_comp$actual, 4)
cal_comp$isotonic_bias <- round(cal_comp$isotonic - cal_comp$actual, 4)
print(cal_comp)


#### APPROACH 2: GAM with flexible a1c_weighted shape ####
cat("\n====== APPROACH 2: GAM with s(a1c_weighted) ======\n")

# just a1c_weighted but with spline — fixes sigmoid bias
gam_data_tr <- data.frame(y = y_tr, a1c_weighted = df_tr$a1c_weighted)
gam_data_va <- data.frame(y = y_va, a1c_weighted = df_va$a1c_weighted)

m_gam1 <- gam(y ~ s(a1c_weighted, k = 10), family = binomial, data = gam_data_tr)
summary(m_gam1)

p_gam1_tr <- predict(m_gam1, gam_data_tr, type = "response")
p_gam1_va <- predict(m_gam1, gam_data_va, type = "response")
th_gam1 <- best_f1_threshold(p_gam1_tr, y_tr)
cat("Val:\n")
compute_metrics(p_gam1_va > th_gam1["best_th"], y_va)


#### APPROACH 3: GAM + focused features (top 5 from residual analysis) ####
cat("\n====== APPROACH 3: GAM + top 5 residual features ======\n")

# top features from LR test: frac_above_7, frac_above_8, high7_insulin, n_crossings_8, n_drug_classes
# plus the key interaction: a1c_weighted * frac_above_8 (LR=515)
m_gam3 <- gam(y_tr ~ s(a1c_weighted, k = 10) +
    frac_above_7 + frac_above_8 + high7_insulin + n_crossings_8 + n_drug_classes,
    family = binomial, data = df_tr)

p_gam3_tr <- predict(m_gam3, df_tr, type = "response")
p_gam3_va <- predict(m_gam3, df_va, type = "response")
th_gam3 <- best_f1_threshold(p_gam3_tr, y_tr)
cat("Val:\n")
compute_metrics(p_gam3_va > th_gam3["best_th"], y_va)


#### APPROACH 4: GAM + focused features + explicit interaction ####
cat("\n====== APPROACH 4: GAM + top features + a1c_weighted:frac_above_8 interaction ======\n")

m_gam4 <- gam(y_tr ~ s(a1c_weighted, k = 10) +
    frac_above_7 + frac_above_8 + high7_insulin + n_crossings_8 + n_drug_classes +
    a1c_weighted:frac_above_8,
    family = binomial, data = df_tr)

p_gam4_tr <- predict(m_gam4, df_tr, type = "response")
p_gam4_va <- predict(m_gam4, df_va, type = "response")
th_gam4 <- best_f1_threshold(p_gam4_tr, y_tr)
cat("Val:\n")
compute_metrics(p_gam4_va > th_gam4["best_th"], y_va)


#### APPROACH 5: GAM with s(a1c_weighted) + non-linear residual features ####
cat("\n====== APPROACH 5: GAM with non-linear residual features ======\n")

# the non-linearity check found a1c_x_ndrug and slope_above_8 have strong non-linear effects
# let them be splines too
m_gam5 <- gam(y_tr ~ s(a1c_weighted, k = 10) +
    frac_above_7 + frac_above_8 + high7_insulin + n_crossings_8 +
    s(n_drug_classes, k = 5) +
    a1c_weighted:frac_above_8,
    family = binomial, data = df_tr)

p_gam5_tr <- predict(m_gam5, df_tr, type = "response")
p_gam5_va <- predict(m_gam5, df_va, type = "response")
th_gam5 <- best_f1_threshold(p_gam5_tr, y_tr)
cat("Val:\n")
compute_metrics(p_gam5_va > th_gam5["best_th"], y_va)


#### APPROACH 6: Isotonic + residual features (two-stage) ####
cat("\n====== APPROACH 6: Isotonic calibration + residual GLM ======\n")

# Stage 1: isotonic on a1c_weighted (non-parametric)
# Stage 2: logistic on residuals with focused features
# Combined via offset

# use isotonic probabilities as offset in logistic regression
# logit(p) = logit(p_iso) + beta * features
offset_tr <- qlogis(pmax(pmin(p_iso_tr, 0.999), 0.001))  # clip to avoid Inf
offset_va <- qlogis(pmax(pmin(p_iso_va, 0.999), 0.001))

m_stage2 <- glm(y_tr ~ frac_above_7 + frac_above_8 + high7_insulin + n_crossings_8 + n_drug_classes,
    family = binomial, data = df_tr, offset = offset_tr)

p_s2_tr <- predict(m_stage2, df_tr, type = "response", newoffset = offset_tr)
# for prediction on val, need to manually compute
lp_va <- predict(m_stage2, df_va, type = "link") - coef(m_stage2)[1]  # remove intercept double-count
# actually, with offset, predict() adds the offset automatically if present in newdata
# but offset isn't in df_va, so we need to handle manually
eta_va <- model.matrix(~ frac_above_7 + frac_above_8 + high7_insulin + n_crossings_8 + n_drug_classes, data = df_va) %*% coef(m_stage2) + offset_va
p_s2_va <- plogis(as.numeric(eta_va))

th_s2 <- best_f1_threshold(p_s2_tr, y_tr)
cat("Val:\n")
compute_metrics(p_s2_va > th_s2["best_th"], y_va)


#### APPROACH 7: Full GLM for reference (current best GLM) ####
cat("\n====== REFERENCE: Full GLM (74 features, from subrata_models.R) ======\n")

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

m_full <- glm(f_glm, data = df_train, family = binomial)
p_full_tr <- predict(m_full, df_train, type = "response")
p_full_va <- predict(m_full, df_val, type = "response")
th_full <- best_f1_threshold(p_full_tr, df_train$target == "Uncontrolled", df_train$mask)
cat("Val:\n")
compute_metrics(p_full_va > th_full["best_th"], df_val$target == "Uncontrolled", df_val$mask)


#### SUMMARY TABLE ####
cat("\n\n====== SUMMARY ======\n")
cat("| Approach | Description | Val F1 |\n")
cat("|---|---|---|\n")
