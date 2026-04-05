###############################################################################
## Three-way stratified model: LOW (<7) / MID (7-8) / HIGH (>=8)
##
## Mechanistic rationale:
##   HIGH (>=8): Already above competition boundary. Question: "will treatment
##               pull them below 8?" 60% base rate, 57% of all positives.
##   MID (7-8):  Under clinical intervention (crossed 7), but competition says
##               "controlled." Question: "will treatment fail to hold below 8?"
##               25% base rate, 27% of positives.
##   LOW (<7):   Fragile treatment responders + stable patients. Question:
##               "were they treated down, and will they relapse?" 4.7% base rate.
##               Signal is in a1c history, not latest reading.
###############################################################################

library(tidyverse)
library(rpart)
library(mgcv)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")


#### LOAD & PREPARE ####
train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")

df_train <- impute_and_flag(create_features(train_dat))
df_val   <- impute_and_flag(create_features(val_dat))


#### THREE-WAY SPLIT ####
split_regime <- function(df) {
    list(
        high = df %>% filter(a1c_latest >= 8),
        mid  = df %>% filter(a1c_latest >= 7, a1c_latest < 8),
        low  = df %>% filter(a1c_latest < 7)
    )
}

tr <- split_regime(df_train)
vl <- split_regime(df_val)

# print summary
for (nm in c("high", "mid", "low")) {
    d <- tr[[nm]]
    n_ret <- sum(d$mask)
    n_unc <- sum(d$target == "Uncontrolled" & d$mask)
    cat(sprintf("%s: train n=%d, returned=%d, uncontrolled=%d (%.1f%%)\n",
                toupper(nm), nrow(d), n_ret, n_unc, 100 * n_unc / n_ret))
}
cat("\n")
for (nm in c("high", "mid", "low")) {
    d <- vl[[nm]]
    n_ret <- sum(d$mask)
    n_unc <- sum(d$target == "Uncontrolled" & d$mask)
    cat(sprintf("%s: val   n=%d, returned=%d, uncontrolled=%d (%.1f%%)\n",
                toupper(nm), nrow(d), n_ret, n_unc, 100 * n_unc / n_ret))
}
cat("\n")


###############################################################################
#### HIGH MODEL (>=8): "Will treatment pull them below 8?" ####
###############################################################################

# features: medication, trajectory, sudden changes, post-drop dynamics
f_high <- target ~
    a1c_latest + a1c_weighted + a1c_sd + a1c_range +
    a1c_change + a1c_rate + a1c_change_if_high8 +
    n_a1c + frac_above_7 + frac_above_8 + n_crossings_7 + n_crossings_8 +
    slope_above_7 + slope_above_8 + slope_below_8 +
    max_drop + max_rise + has_sudden_drop + has_sudden_rise +
    post_drop_drift + sudden_drop_still_high8 +
    n_drug_classes + total_meds +
    insulin + metformin + sulfonylurea + glp1 + sglt2 + dpp4 +
    on_modern_drugs + high8_insulin + high8_no_meds + a1c_per_drug +
    improving_on_meds + worsening_on_meds +
    days_to_eval + time_gap +
    age + gender_male + adi_state + adi_nation +
    cad + copd + ed_visits + pcp_visits + admissions +
    slope_above_7_miss + slope_above_8_miss + slope_below_8_miss +
    post_drop_drift_miss + time_gap_miss + max_drop_miss + max_rise_miss

feat_high <- c(
    "a1c_latest", "a1c_weighted", "a1c_sd", "a1c_range",
    "a1c_change", "a1c_rate", "a1c_change_if_high8",
    "n_a1c", "frac_above_7", "frac_above_8", "n_crossings_7", "n_crossings_8",
    "slope_above_7", "slope_above_8", "slope_below_8",
    "max_drop", "max_rise", "has_sudden_drop", "has_sudden_rise",
    "post_drop_drift", "sudden_drop_still_high8",
    "n_drug_classes", "total_meds",
    "insulin", "metformin", "sulfonylurea", "glp1", "sglt2", "dpp4",
    "on_modern_drugs", "high8_insulin", "high8_no_meds", "a1c_per_drug",
    "improving_on_meds", "worsening_on_meds",
    "days_to_eval", "time_gap",
    "age", "gender_male", "adi_state", "adi_nation",
    "cad", "copd", "ed_visits", "pcp_visits", "admissions"
)

cat("====== HIGH (>=8) MODELS ======\n\n")

## GLM
cat("------ HIGH: GLM ---------\n")
m_high_glm <- glm(f_high, data = tr$high, family = binomial)
p_high_glm_tr <- predict(m_high_glm, tr$high, type = "response")
th_high_glm <- best_f1_threshold(p_high_glm_tr, tr$high$target == "Uncontrolled", tr$high$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_high_glm["best_f1"], th_high_glm["best_th"]))

p_high_glm <- predict(m_high_glm, vl$high, type = "response")
compute_metrics(p_high_glm > th_high_glm["best_th"], vl$high$target == "Uncontrolled", vl$high$mask)

## Decision tree
cat("\n------ HIGH: Decision Tree ---------\n")
dt_high <- as.formula(paste("target ~", paste(feat_high, collapse = " + ")))
m_high_tree <- rpart(dt_high, data = tr$high, method = "class",
                     control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 30))
p_high_tree_tr <- predict(m_high_tree, tr$high, type = "prob")[, "Uncontrolled"]
th_high_tree <- best_f1_threshold(p_high_tree_tr, tr$high$target == "Uncontrolled", tr$high$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_high_tree["best_f1"], th_high_tree["best_th"]))

p_high_tree <- predict(m_high_tree, vl$high, type = "prob")[, "Uncontrolled"]
compute_metrics(p_high_tree > th_high_tree["best_th"], vl$high$target == "Uncontrolled", vl$high$mask)

## GAM (splines on key A1c features)
cat("\n------ HIGH: GAM ---------\n")
m_high_gam <- gam(
    target ~
        s(a1c_latest, k = 6) + s(a1c_weighted, k = 6) +
        s(a1c_per_drug, k = 5) +
        a1c_change + a1c_rate + n_a1c +
        frac_above_8 + n_crossings_8 + slope_above_8 +
        max_drop + has_sudden_drop + post_drop_drift +
        n_drug_classes + total_meds +
        insulin + metformin + sulfonylurea + glp1 + sglt2 + dpp4 +
        on_modern_drugs + improving_on_meds + worsening_on_meds +
        days_to_eval + time_gap +
        age + gender_male + cad + copd,
    family = binomial, data = tr$high
)
p_high_gam_tr <- predict(m_high_gam, tr$high, type = "response")
th_high_gam <- best_f1_threshold(p_high_gam_tr, tr$high$target == "Uncontrolled", tr$high$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_high_gam["best_f1"], th_high_gam["best_th"]))

p_high_gam <- predict(m_high_gam, vl$high, type = "response")
compute_metrics(p_high_gam > th_high_gam["best_th"], vl$high$target == "Uncontrolled", vl$high$mask)

## HIGH ensemble
cat("\n------ HIGH: Ensemble (GLM + Tree + GAM) ---------\n")
p_high_ens <- (p_high_glm + p_high_tree + as.numeric(p_high_gam)) / 3
th_high_ens <- best_f1_threshold(
    (p_high_glm_tr + p_high_tree_tr + as.numeric(p_high_gam_tr)) / 3,
    tr$high$target == "Uncontrolled", tr$high$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_high_ens["best_f1"], th_high_ens["best_th"]))
compute_metrics(p_high_ens > th_high_ens["best_th"], vl$high$target == "Uncontrolled", vl$high$mask)


###############################################################################
#### MID MODEL (7-8): "Will treatment fail to hold them below 8?" ####
###############################################################################

# features: proximity to 8, trajectory direction, volatility, medication intensity
f_mid <- target ~
    a1c_latest + value_a1c_1 + a1c_weighted + a1c_sd + a1c_range +
    a1c_change + a1c_rate + a1c_change_if_high7 +
    n_a1c + frac_above_7 + frac_above_8 + n_crossings_7 + n_crossings_8 +
    slope_above_7 + slope_below_7 + slope_above_8 + slope_below_8 +
    max_drop + max_rise + has_sudden_drop + has_sudden_rise +
    post_drop_drift + sudden_drop_still_high7 +
    n_drug_classes + total_meds +
    insulin + metformin + sulfonylurea + glp1 + sglt2 + dpp4 +
    on_modern_drugs + high7_insulin + a1c_per_drug +
    improving_on_meds + worsening_on_meds +
    days_to_eval + time_gap +
    age + gender_male + adi_state + adi_nation +
    cad + copd + ed_visits + pcp_visits + admissions +
    abs_change_miss + a1c_sd_miss + a1c_range_miss + a1c_rate_miss +
    slope_above_7_miss + slope_below_7_miss +
    slope_above_8_miss + slope_below_8_miss +
    post_drop_drift_miss + time_gap_miss + max_drop_miss + max_rise_miss

feat_mid <- c(
    "a1c_latest", "value_a1c_1", "a1c_weighted", "a1c_sd", "a1c_range",
    "a1c_change", "a1c_rate", "a1c_change_if_high7",
    "n_a1c", "frac_above_7", "frac_above_8", "n_crossings_7", "n_crossings_8",
    "slope_above_7", "slope_below_7", "slope_above_8", "slope_below_8",
    "max_drop", "max_rise", "has_sudden_drop", "has_sudden_rise",
    "post_drop_drift", "sudden_drop_still_high7",
    "n_drug_classes", "total_meds",
    "insulin", "metformin", "sulfonylurea", "glp1", "sglt2", "dpp4",
    "on_modern_drugs", "high7_insulin", "a1c_per_drug",
    "improving_on_meds", "worsening_on_meds",
    "days_to_eval", "time_gap",
    "age", "gender_male", "adi_state", "adi_nation",
    "cad", "copd", "ed_visits", "pcp_visits", "admissions"
)

cat("\n\n====== MID (7-8) MODELS ======\n\n")

## GLM
cat("------ MID: GLM ---------\n")
m_mid_glm <- glm(f_mid, data = tr$mid, family = binomial)
p_mid_glm_tr <- predict(m_mid_glm, tr$mid, type = "response")
th_mid_glm <- best_f1_threshold(p_mid_glm_tr, tr$mid$target == "Uncontrolled", tr$mid$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_mid_glm["best_f1"], th_mid_glm["best_th"]))

p_mid_glm <- predict(m_mid_glm, vl$mid, type = "response")
compute_metrics(p_mid_glm > th_mid_glm["best_th"], vl$mid$target == "Uncontrolled", vl$mid$mask)

## GLM + class weights (25% base rate — moderate imbalance)
cat("\n------ MID: GLM + weights ---------\n")
wts_mid <- ifelse(tr$mid$target == "Uncontrolled",
                  1 - mean(tr$mid$target == "Uncontrolled"),
                  mean(tr$mid$target == "Uncontrolled"))
m_mid_glm_wt <- glm(f_mid, data = tr$mid, family = binomial, weights = wts_mid)
p_mid_glm_wt_tr <- predict(m_mid_glm_wt, tr$mid, type = "response")
th_mid_glm_wt <- best_f1_threshold(p_mid_glm_wt_tr, tr$mid$target == "Uncontrolled", tr$mid$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_mid_glm_wt["best_f1"], th_mid_glm_wt["best_th"]))

p_mid_glm_wt <- predict(m_mid_glm_wt, vl$mid, type = "response")
compute_metrics(p_mid_glm_wt > th_mid_glm_wt["best_th"], vl$mid$target == "Uncontrolled", vl$mid$mask)

## Decision tree
cat("\n------ MID: Decision Tree ---------\n")
dt_mid <- as.formula(paste("target ~", paste(feat_mid, collapse = " + ")))
m_mid_tree <- rpart(dt_mid, data = tr$mid, method = "class",
                    control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 30))
p_mid_tree_tr <- predict(m_mid_tree, tr$mid, type = "prob")[, "Uncontrolled"]
th_mid_tree <- best_f1_threshold(p_mid_tree_tr, tr$mid$target == "Uncontrolled", tr$mid$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_mid_tree["best_f1"], th_mid_tree["best_th"]))

p_mid_tree <- predict(m_mid_tree, vl$mid, type = "prob")[, "Uncontrolled"]
compute_metrics(p_mid_tree > th_mid_tree["best_th"], vl$mid$target == "Uncontrolled", vl$mid$mask)

## MID ensemble
cat("\n------ MID: Ensemble (GLM + GLM_wt + Tree) ---------\n")
p_mid_ens <- (p_mid_glm + p_mid_glm_wt + p_mid_tree) / 3
p_mid_ens_tr <- (p_mid_glm_tr + p_mid_glm_wt_tr + p_mid_tree_tr) / 3
th_mid_ens <- best_f1_threshold(p_mid_ens_tr, tr$mid$target == "Uncontrolled", tr$mid$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_mid_ens["best_f1"], th_mid_ens["best_th"]))
compute_metrics(p_mid_ens > th_mid_ens["best_th"], vl$mid$target == "Uncontrolled", vl$mid$mask)


###############################################################################
#### LOW MODEL (<7): "Were they treated down? Will they relapse?" ####
###############################################################################

# focused features: history (a1c_weighted, first reading), volatility,
# medication intensity (fragility signal), trajectory
f_low <- target ~
    a1c_latest + value_a1c_1 + a1c_weighted + a1c_sd + a1c_range +
    a1c_change + a1c_rate +
    n_a1c + frac_above_7 + frac_above_8 + n_crossings_7 +
    slope_above_7 + slope_below_7 +
    max_drop + max_rise + has_sudden_rise +
    n_drug_classes + total_meds +
    insulin + metformin + on_modern_drugs +
    days_to_eval + time_gap +
    age + gender_male +
    abs_change_miss + a1c_sd_miss + a1c_range_miss + a1c_rate_miss +
    slope_above_7_miss + slope_below_7_miss +
    time_gap_miss + max_drop_miss + max_rise_miss

feat_low <- c(
    "a1c_latest", "value_a1c_1", "a1c_weighted", "a1c_sd", "a1c_range",
    "a1c_change", "a1c_rate",
    "n_a1c", "frac_above_7", "frac_above_8", "n_crossings_7",
    "slope_above_7", "slope_below_7",
    "max_drop", "max_rise", "has_sudden_rise",
    "n_drug_classes", "total_meds",
    "insulin", "metformin", "on_modern_drugs",
    "days_to_eval", "time_gap",
    "age", "gender_male"
)

cat("\n\n====== LOW (<7) MODELS ======\n\n")

## Baseline: predict all controlled
cat("------ LOW: Predict all controlled ---------\n")
cat(sprintf("  (predicting 0 positives: loses %d/%d = %.1f%% of LOW positives)\n",
            sum(vl$low$target == "Uncontrolled" & vl$low$mask), sum(vl$low$mask),
            100 * mean(vl$low$target[vl$low$mask] == "Uncontrolled")))
cat("  F1_pos = 0 (no positive predictions), but perfect specificity\n\n")

## GLM
cat("------ LOW: GLM ---------\n")
m_low_glm <- glm(f_low, data = tr$low, family = binomial)
p_low_glm_tr <- predict(m_low_glm, tr$low, type = "response")
th_low_glm <- best_f1_threshold(p_low_glm_tr, tr$low$target == "Uncontrolled", tr$low$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_low_glm["best_f1"], th_low_glm["best_th"]))

p_low_glm <- predict(m_low_glm, vl$low, type = "response")
compute_metrics(p_low_glm > th_low_glm["best_th"], vl$low$target == "Uncontrolled", vl$low$mask)

## GLM + heavy class weights (4.7% base rate — extreme imbalance)
cat("\n------ LOW: GLM + weights ---------\n")
wts_low <- ifelse(tr$low$target == "Uncontrolled",
                  1 - mean(tr$low$target == "Uncontrolled"),
                  mean(tr$low$target == "Uncontrolled"))
m_low_glm_wt <- glm(f_low, data = tr$low, family = binomial, weights = wts_low)
p_low_glm_wt_tr <- predict(m_low_glm_wt, tr$low, type = "response")
th_low_glm_wt <- best_f1_threshold(p_low_glm_wt_tr, tr$low$target == "Uncontrolled", tr$low$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_low_glm_wt["best_f1"], th_low_glm_wt["best_th"]))

p_low_glm_wt <- predict(m_low_glm_wt, vl$low, type = "response")
compute_metrics(p_low_glm_wt > th_low_glm_wt["best_th"], vl$low$target == "Uncontrolled", vl$low$mask)

## Decision tree
cat("\n------ LOW: Decision Tree ---------\n")
dt_low <- as.formula(paste("target ~", paste(feat_low, collapse = " + ")))
m_low_tree <- rpart(dt_low, data = tr$low, method = "class",
                    control = rpart.control(cp = 0.005, maxdepth = 5, minsplit = 50))
p_low_tree_tr <- predict(m_low_tree, tr$low, type = "prob")[, "Uncontrolled"]
th_low_tree <- best_f1_threshold(p_low_tree_tr, tr$low$target == "Uncontrolled", tr$low$mask)
cat(sprintf("  train best F1 = %.3f (th=%.3f)\n", th_low_tree["best_f1"], th_low_tree["best_th"]))

p_low_tree <- predict(m_low_tree, vl$low, type = "prob")[, "Uncontrolled"]
compute_metrics(p_low_tree > th_low_tree["best_th"], vl$low$target == "Uncontrolled", vl$low$mask)


###############################################################################
#### COMBINE: stitch HIGH + MID + LOW, evaluate overall ####
###############################################################################

cat("\n\n====== COMBINED THREE-WAY RESULTS (overall val) ======\n\n")

# helper: combine three-way predictions, evaluate on full val
combine_3way <- function(name, pred_high, pred_mid, pred_low) {
    pred_all <- logical(nrow(df_val))
    pred_all[df_val$a1c_latest >= 8]                          <- pred_high
    pred_all[df_val$a1c_latest >= 7 & df_val$a1c_latest < 8]  <- pred_mid
    pred_all[df_val$a1c_latest < 7]                            <- pred_low

    cat(sprintf("------ %s ---------\n", name))
    compute_metrics(pred_all, df_val$target == "Uncontrolled", df_val$mask)
}

# helper: combine probabilities for ensemble (works for train or val)
combine_3way_probs <- function(prob_high, prob_mid, prob_low, df) {
    prob_all <- numeric(nrow(df))
    prob_all[df$a1c_latest >= 8]                          <- prob_high
    prob_all[df$a1c_latest >= 7 & df$a1c_latest < 8]  <- prob_mid
    prob_all[df$a1c_latest < 7]                            <- prob_low
    prob_all
}


#### Approach 1: Best model from each regime, separate thresholds ####
# HIGH: GLM (strongest individual)
# MID: GLM
# LOW: predict all controlled (safest)
combine_3way("3way: HIGH=GLM, MID=GLM, LOW=all_controlled",
             p_high_glm > th_high_glm["best_th"],
             p_mid_glm > th_mid_glm["best_th"],
             rep(FALSE, nrow(vl$low)))

# LOW: GLM instead
combine_3way("3way: HIGH=GLM, MID=GLM, LOW=GLM",
             p_high_glm > th_high_glm["best_th"],
             p_mid_glm > th_mid_glm["best_th"],
             p_low_glm > th_low_glm["best_th"])

# LOW: GLM+weights
combine_3way("3way: HIGH=GLM, MID=GLM, LOW=GLM_wt",
             p_high_glm > th_high_glm["best_th"],
             p_mid_glm > th_mid_glm["best_th"],
             p_low_glm_wt > th_low_glm_wt["best_th"])


#### Approach 2: Ensembles per regime ####
combine_3way("3way: HIGH=Ens, MID=Ens, LOW=all_controlled",
             p_high_ens > th_high_ens["best_th"],
             p_mid_ens > th_mid_ens["best_th"],
             rep(FALSE, nrow(vl$low)))

combine_3way("3way: HIGH=Ens, MID=Ens, LOW=GLM_wt",
             p_high_ens > th_high_ens["best_th"],
             p_mid_ens > th_mid_ens["best_th"],
             p_low_glm_wt > th_low_glm_wt["best_th"])


#### Approach 3: Combined probs + single global threshold ####
#### NOTE on single-threshold approach: NOT appropriate here.
## Probabilities from models trained on different subsets have different scales
## (HIGH model: ~60% base rate, MID: ~25%, LOW: ~5%).
## A single threshold cannot be meaningful across these scales.
## The separate-threshold approach above is correct.


#### SAVE ####
saveRDS(list(
    # per-regime probabilities
    high = list(glm = p_high_glm, tree = p_high_tree, gam = as.numeric(p_high_gam), ens = p_high_ens,
                th_glm = th_high_glm, th_tree = th_high_tree, th_gam = th_high_gam, th_ens = th_high_ens),
    mid  = list(glm = p_mid_glm, glm_wt = p_mid_glm_wt, tree = p_mid_tree, ens = p_mid_ens,
                th_glm = th_mid_glm, th_glm_wt = th_mid_glm_wt, th_tree = th_mid_tree, th_ens = th_mid_ens),
    low  = list(glm = p_low_glm, glm_wt = p_low_glm_wt, tree = p_low_tree,
                th_glm = th_low_glm, th_glm_wt = th_low_glm_wt, th_tree = th_low_tree),
    # val metadata
    val_ids = df_val$id, val_target = df_val$target, val_mask = df_val$mask,
    val_a1c_latest = df_val$a1c_latest
), file = "./data/processed/val_predictions_3way.Rds")
cat("\n------ Saved 3-way predictions to data/processed/val_predictions_3way.Rds ---------\n")
