###############################################################################
## Calibration analysis + regime-specific thresholds on global model
##
## Key idea: the global model (trained on all data) is our best predictor.
## But instead of one threshold for all patients, we apply different thresholds
## per regime (HIGH >=8, MID 7-8, LOW <7) — combining the global model's
## statistical power with mechanistic insight about different base rates.
###############################################################################

library(tidyverse)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")


#### LOAD ####
train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")

df_train <- impute_and_flag(create_features(train_dat))
df_val   <- impute_and_flag(create_features(val_dat))

# global model predictions (saved from subrata_models.R)
saved <- readRDS("./data/processed/val_predictions.Rds")
vp <- saved$val_preds


#### SECTION 1: Calibration plots ####
cat("====== CALIBRATION ANALYSIS ======\n\n")

# add regime labels
vp$regime <- case_when(
    df_val$a1c_latest >= 8 ~ "HIGH (>=8)",
    df_val$a1c_latest >= 7 ~ "MID (7-8)",
    TRUE                   ~ "LOW (<7)"
)
vp$a1c_latest <- df_val$a1c_latest

# calibration function: bin predictions, compute observed rate
calibration_table <- function(probs, truth, mask, n_bins = 10, label = "") {
    idx <- which(mask)
    p <- probs[idx]
    y <- (truth[idx] == "Uncontrolled")

    # equal-width bins
    breaks <- seq(0, 1, length.out = n_bins + 1)
    bin <- cut(p, breaks, include.lowest = TRUE)

    tb <- tibble(prob = p, outcome = y, bin = bin) %>%
        group_by(bin) %>%
        summarise(
            n = n(),
            mean_pred = mean(prob),
            obs_rate = mean(outcome),
            .groups = "drop"
        ) %>%
        filter(n >= 5)

    if (label != "") cat(sprintf("\n--- Calibration: %s ---\n", label))
    print(tb, width = 100)
    invisible(tb)
}

# overall calibration for each model
for (mod_name in c("prob_glm", "prob_glm_wt", "prob_tree", "prob_gam")) {
    calibration_table(vp[[mod_name]], vp$target, vp$mask, n_bins = 10, label = mod_name)
}

# ensemble calibration
vp$prob_ens <- (vp$prob_glm + vp$prob_glm_wt + vp$prob_tree + vp$prob_gam) / 4
calibration_table(vp$prob_ens, vp$target, vp$mask, n_bins = 10, label = "ensemble")


#### SECTION 2: Per-regime calibration ####
cat("\n\n====== PER-REGIME CALIBRATION (ensemble) ======\n\n")

for (reg in c("HIGH (>=8)", "MID (7-8)", "LOW (<7)")) {
    reg_mask <- vp$mask & (vp$regime == reg)
    calibration_table(vp$prob_ens, vp$target, reg_mask, n_bins = 8, label = paste("Ensemble |", reg))
}


#### SECTION 3: Regime-specific thresholds on global model ####
cat("\n\n====== REGIME-SPECIFIC THRESHOLDS (global model) ======\n\n")

# for each model, find optimal threshold PER REGIME on training data
# then combine per-regime predictions and evaluate overall

# first, refit the global models on train (or use saved? we need train predictions)
# refit GLM quickly
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
    post_drop_drift_miss

m_glm <- glm(f_glm, data = df_train, family = binomial)
p_tr <- predict(m_glm, df_train, type = "response")
p_vl <- predict(m_glm, df_val, type = "response")

# class-weighted GLM
wts <- ifelse(df_train$target == "Uncontrolled", 1 - mean(df_train$target == "Uncontrolled"), mean(df_train$target == "Uncontrolled"))
m_glm_wt <- glm(f_glm, data = df_train, family = binomial, weights = wts)
p_wt_tr <- predict(m_glm_wt, df_train, type = "response")
p_wt_vl <- predict(m_glm_wt, df_val, type = "response")

# train regime labels
train_regime <- case_when(
    df_train$a1c_latest >= 8 ~ "HIGH",
    df_train$a1c_latest >= 7 ~ "MID",
    TRUE                     ~ "LOW"
)
val_regime <- case_when(
    df_val$a1c_latest >= 8 ~ "HIGH",
    df_val$a1c_latest >= 7 ~ "MID",
    TRUE                   ~ "LOW"
)


#### helper: find per-regime thresholds on train, apply to val ####
regime_threshold_eval <- function(p_train, p_val, label) {
    cat(sprintf("\n------ %s: regime-specific thresholds ---------\n", label))

    # find optimal threshold per regime on train
    th <- list()
    for (reg in c("HIGH", "MID", "LOW")) {
        reg_mask_tr <- df_train$mask & (train_regime == reg)
        res <- best_f1_threshold(p_train, df_train$target == "Uncontrolled", reg_mask_tr)
        th[[reg]] <- res
        cat(sprintf("  %s: best_th=%.3f, train_F1=%.3f\n", reg, res["best_th"], res["best_f1"]))
    }

    # apply per-regime thresholds to val
    pred_val <- logical(nrow(df_val))
    pred_val[val_regime == "HIGH"] <- p_val[val_regime == "HIGH"] > th$HIGH["best_th"]
    pred_val[val_regime == "MID"]  <- p_val[val_regime == "MID"]  > th$MID["best_th"]
    pred_val[val_regime == "LOW"]  <- p_val[val_regime == "LOW"]  > th$LOW["best_th"]

    cat("  Combined val:\n")
    compute_metrics(pred_val, df_val$target == "Uncontrolled", df_val$mask)

    # also try: LOW = all controlled (ignore LOW threshold)
    pred_val_no_low <- pred_val
    pred_val_no_low[val_regime == "LOW"] <- FALSE
    cat(sprintf("  Combined val (LOW=all controlled):\n"))
    compute_metrics(pred_val_no_low, df_val$target == "Uncontrolled", df_val$mask)

    invisible(list(th = th, pred = pred_val, pred_no_low = pred_val_no_low))
}


# compare: single threshold vs regime-specific
cat("\n------ GLM: single global threshold ---------\n")
th_global <- best_f1_threshold(p_tr, df_train$target == "Uncontrolled", df_train$mask)
cat(sprintf("  global: best_th=%.3f, train_F1=%.3f\n", th_global["best_th"], th_global["best_f1"]))
compute_metrics(p_vl > th_global["best_th"], df_val$target == "Uncontrolled", df_val$mask)

res_glm <- regime_threshold_eval(p_tr, p_vl, "GLM")

cat("\n------ GLM+wt: single global threshold ---------\n")
th_wt_global <- best_f1_threshold(p_wt_tr, df_train$target == "Uncontrolled", df_train$mask)
cat(sprintf("  global: best_th=%.3f, train_F1=%.3f\n", th_wt_global["best_th"], th_wt_global["best_f1"]))
compute_metrics(p_wt_vl > th_wt_global["best_th"], df_val$target == "Uncontrolled", df_val$mask)

res_glm_wt <- regime_threshold_eval(p_wt_tr, p_wt_vl, "GLM+wt")


#### Ensemble of GLM + GLM_wt with regime thresholds ####
cat("\n\n====== ENSEMBLE + REGIME THRESHOLDS ======\n\n")
p_ens_tr <- (p_tr + p_wt_tr) / 2
p_ens_vl <- (p_vl + p_wt_vl) / 2

cat("------ Ensemble: single global threshold ---------\n")
th_ens_global <- best_f1_threshold(p_ens_tr, df_train$target == "Uncontrolled", df_train$mask)
cat(sprintf("  global: best_th=%.3f, train_F1=%.3f\n", th_ens_global["best_th"], th_ens_global["best_f1"]))
compute_metrics(p_ens_vl > th_ens_global["best_th"], df_val$target == "Uncontrolled", df_val$mask)

res_ens <- regime_threshold_eval(p_ens_tr, p_ens_vl, "Ensemble (GLM + GLM_wt)")


#### SECTION 4: Calibration plot (PDF) ####
cat("\n\n====== GENERATING CALIBRATION PLOTS ======\n\n")

# base R calibration plot
pdf("figures/calibration.pdf", width = 10, height = 8)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (mod_name in c("prob_glm", "prob_glm_wt", "prob_tree", "prob_gam")) {
    idx <- which(vp$mask)
    p <- vp[[mod_name]][idx]
    y <- (vp$target[idx] == "Uncontrolled")
    reg <- vp$regime[idx]

    # 10 equal-width bins
    breaks <- seq(0, max(p) + 0.01, length.out = 11)
    bin <- cut(p, breaks, include.lowest = TRUE)
    tb <- tapply(seq_along(p), bin, function(i) c(mean(p[i]), mean(y[i]), length(i)))
    tb <- tb[!sapply(tb, is.null)]
    tb <- tb[sapply(tb, function(x) x[3] >= 5)]

    mp <- sapply(tb, `[`, 1)
    or <- sapply(tb, `[`, 2)
    nn <- sapply(tb, `[`, 3)

    plot(mp, or, pch = 19, cex = sqrt(nn) / 15,
         xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Predicted probability", ylab = "Observed rate",
         main = mod_name)
    abline(0, 1, lty = 2, col = "grey50")
    text(0.8, 0.1, sprintf("n=%d", length(p)), cex = 0.8)
}

# regime-specific calibration for ensemble
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
cols <- c("HIGH (>=8)" = "#D95F02", "MID (7-8)" = "#7570B3", "LOW (<7)" = "#1B9E77")

for (reg in c("HIGH (>=8)", "MID (7-8)", "LOW (<7)")) {
    idx <- which(vp$mask & vp$regime == reg)
    p <- vp$prob_ens[idx]
    y <- (vp$target[idx] == "Uncontrolled")

    n_bins <- 8
    breaks <- seq(min(p) - 0.001, max(p) + 0.001, length.out = n_bins + 1)
    bin <- cut(p, breaks, include.lowest = TRUE)
    tb <- tapply(seq_along(p), bin, function(i) c(mean(p[i]), mean(y[i]), length(i)))
    tb <- tb[!sapply(tb, is.null)]
    tb <- tb[sapply(tb, function(x) x[3] >= 5)]

    mp <- sapply(tb, `[`, 1)
    or <- sapply(tb, `[`, 2)
    nn <- sapply(tb, `[`, 3)

    plot(mp, or, pch = 19, cex = sqrt(nn) / 12, col = cols[reg],
         xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Predicted probability", ylab = "Observed rate",
         main = paste("Ensemble |", reg))
    abline(0, 1, lty = 2, col = "grey50")
    text(0.7, 0.1, sprintf("n=%d\nbase=%.0f%%", length(p), 100*mean(y)), cex = 0.8)
}

dev.off()
cat("Saved calibration plots to figures/calibration.pdf\n")


#### SUMMARY ####
cat("\n\n====== SUMMARY TABLE ======\n\n")
cat("Model                              | Threshold type     | Val F1\n")
cat("-----------------------------------|--------------------|--------\n")
