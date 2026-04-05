###############################################################################
## Tier 2: Stratified two-model approach
## Separate models for A1c >= 8 (competition boundary) vs < 8
## Plus all Tier 2 features (regime slopes, crossings, post-drop dynamics)
###############################################################################

library(tidyverse)
library(rpart)
library(rpart.plot)
library(mgcv)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")


#### LOAD & PREPARE ####
train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")

df_train <- impute_and_flag(create_features(train_dat))
df_val <- impute_and_flag(create_features(val_dat))

cat("Train: n =", nrow(df_train), " | returned:", sum(df_train$mask), "\n")
cat("Val:   n =", nrow(df_val), " | returned:", sum(df_val$mask), "\n\n")


#### SPLIT BY REGIME ####
train_high <- df_train %>% filter(a1c_latest >= 8)
train_low  <- df_train %>% filter(a1c_latest < 8)
val_high   <- df_val %>% filter(a1c_latest >= 8)
val_low    <- df_val %>% filter(a1c_latest < 8)

cat("HIGH regime (a1c >= 8):\n")
cat("  Train:", nrow(train_high), "| returned:", sum(train_high$mask),
    "| uncontrolled:", sum(train_high$target == "Uncontrolled" & train_high$mask),
    "(", round(mean(train_high$target[train_high$mask] == "Uncontrolled") * 100, 1), "%)\n")
cat("  Val:  ", nrow(val_high), "| returned:", sum(val_high$mask),
    "| uncontrolled:", sum(val_high$target == "Uncontrolled" & val_high$mask),
    "(", round(mean(val_high$target[val_high$mask] == "Uncontrolled") * 100, 1), "%)\n\n")

cat("LOW regime (a1c < 8):\n")
cat("  Train:", nrow(train_low), "| returned:", sum(train_low$mask),
    "| uncontrolled:", sum(train_low$target == "Uncontrolled" & train_low$mask),
    "(", round(mean(train_low$target[train_low$mask] == "Uncontrolled") * 100, 1), "%)\n")
cat("  Val:  ", nrow(val_low), "| returned:", sum(val_low$mask),
    "| uncontrolled:", sum(val_low$target == "Uncontrolled" & val_low$mask),
    "(", round(mean(val_low$target[val_low$mask] == "Uncontrolled") * 100, 1), "%)\n\n")


###############################################################################
#### HIGH A1C MODEL: "Will treatment bring them down?" ####
###############################################################################

# features that matter for high-a1c patients:
# medication intensity, trajectory, sudden changes, time gap, post-drop dynamics
f_high <- target ~
    a1c_latest + a1c_weighted + a1c_sd + a1c_range +
    a1c_change + a1c_rate + a1c_change_if_high7 + a1c_change_if_high8 +
    n_a1c + frac_above_7 + frac_above_8 + n_crossings_7 + n_crossings_8 +
    slope_above_7 + slope_below_7 + slope_above_8 + slope_below_8 +
    max_drop + max_rise + has_sudden_drop + has_sudden_rise +
    post_drop_drift + sudden_drop_still_high7 + sudden_drop_still_high8 +
    n_drug_classes + total_meds +
    insulin + metformin + sulfonylurea + glp1 + sglt2 + dpp4 +
    on_modern_drugs + high7_insulin + high8_insulin + a1c_per_drug +
    improving_on_meds + worsening_on_meds +
    days_to_eval + time_gap +
    age + gender_male + adi_state + adi_nation +
    cad + copd +
    ed_visits + pcp_visits + admissions +
    slope_above_7_miss + slope_below_7_miss +
    slope_above_8_miss + slope_below_8_miss +
    post_drop_drift_miss + time_gap_miss +
    max_drop_miss + max_rise_miss

feat_high <- c(
    "a1c_latest", "a1c_weighted", "a1c_sd", "a1c_range",
    "a1c_change", "a1c_rate", "a1c_change_if_high7", "a1c_change_if_high8",
    "n_a1c", "frac_above_7", "frac_above_8", "n_crossings_7", "n_crossings_8",
    "slope_above_7", "slope_below_7", "slope_above_8", "slope_below_8",
    "max_drop", "max_rise", "has_sudden_drop", "has_sudden_rise",
    "post_drop_drift", "sudden_drop_still_high7", "sudden_drop_still_high8",
    "n_drug_classes", "total_meds",
    "insulin", "metformin", "sulfonylurea", "glp1", "sglt2", "dpp4",
    "on_modern_drugs", "high7_insulin", "high8_insulin", "a1c_per_drug",
    "improving_on_meds", "worsening_on_meds",
    "days_to_eval", "time_gap",
    "age", "gender_male", "adi_state", "adi_nation",
    "cad", "copd",
    "ed_visits", "pcp_visits", "admissions"
)

cat("====== HIGH A1C MODELS ======\n\n")

# GLM for high a1c
cat("------ HIGH: GLM ---------\n")
m_high_glm <- glm(f_high, data = train_high, family = binomial)
p_high_glm_tr <- predict(m_high_glm, train_high, type = "response")
th_high_glm <- best_f1_threshold(p_high_glm_tr, train_high$target == "Uncontrolled", train_high$mask)
th_high_glm

p_high_glm_val <- predict(m_high_glm, val_high, type = "response")
compute_metrics(p_high_glm_val > th_high_glm["best_th"], val_high$target == "Uncontrolled", val_high$mask)

# Decision tree for high a1c
cat("\n------ HIGH: Decision Tree ---------\n")
dt_formula_high <- as.formula(paste("target ~", paste(feat_high, collapse = " + ")))
m_high_tree <- rpart(dt_formula_high, data = train_high, method = "class",
                     control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 30))

p_high_tree_tr <- predict(m_high_tree, train_high, type = "prob")[, "Uncontrolled"]
th_high_tree <- best_f1_threshold(p_high_tree_tr, train_high$target == "Uncontrolled", train_high$mask)
th_high_tree

p_high_tree_val <- predict(m_high_tree, val_high, type = "prob")[, "Uncontrolled"]
compute_metrics(p_high_tree_val > th_high_tree["best_th"], val_high$target == "Uncontrolled", val_high$mask)


###############################################################################
#### LOW A1C MODEL: "Will they deteriorate?" ####
###############################################################################

# features that matter for low-a1c patients:
# proximity to 7, volatility, medication fragility, trajectory
f_low <- target ~
    a1c_latest + value_a1c_1 + a1c_weighted + a1c_sd + a1c_range +
    a1c_change + a1c_rate +
    n_a1c + frac_above_7 + frac_above_8 + n_crossings_7 + n_crossings_8 +
    slope_above_7 + slope_below_7 + slope_above_8 + slope_below_8 +
    max_drop + max_rise + has_sudden_rise +
    n_drug_classes + total_meds +
    insulin + metformin + on_modern_drugs +
    days_to_eval + time_gap +
    age + gender_male + adi_state + adi_nation +
    cad + copd +
    ed_visits + pcp_visits + admissions +
    slope_above_7_miss + slope_below_7_miss +
    slope_above_8_miss + slope_below_8_miss +
    time_gap_miss + max_drop_miss + max_rise_miss

feat_low <- c(
    "a1c_latest", "value_a1c_1", "a1c_weighted", "a1c_sd", "a1c_range",
    "a1c_change", "a1c_rate",
    "n_a1c", "frac_above_7", "frac_above_8", "n_crossings_7", "n_crossings_8",
    "slope_above_7", "slope_below_7", "slope_above_8", "slope_below_8",
    "max_drop", "max_rise", "has_sudden_rise",
    "n_drug_classes", "total_meds",
    "insulin", "metformin", "on_modern_drugs",
    "days_to_eval", "time_gap",
    "age", "gender_male", "adi_state", "adi_nation",
    "cad", "copd",
    "ed_visits", "pcp_visits", "admissions"
)

cat("\n\n====== LOW A1C MODELS ======\n\n")

# GLM for low a1c
cat("------ LOW: GLM ---------\n")
m_low_glm <- glm(f_low, data = train_low, family = binomial)
p_low_glm_tr <- predict(m_low_glm, train_low, type = "response")
th_low_glm <- best_f1_threshold(p_low_glm_tr, train_low$target == "Uncontrolled", train_low$mask)
th_low_glm

p_low_glm_val <- predict(m_low_glm, val_low, type = "response")
compute_metrics(p_low_glm_val > th_low_glm["best_th"], val_low$target == "Uncontrolled", val_low$mask)

# Decision tree for low a1c
cat("\n------ LOW: Decision Tree ---------\n")
dt_formula_low <- as.formula(paste("target ~", paste(feat_low, collapse = " + ")))
m_low_tree <- rpart(dt_formula_low, data = train_low, method = "class",
                    control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 30))

p_low_tree_tr <- predict(m_low_tree, train_low, type = "prob")[, "Uncontrolled"]
th_low_tree <- best_f1_threshold(p_low_tree_tr, train_low$target == "Uncontrolled", train_low$mask)
th_low_tree

p_low_tree_val <- predict(m_low_tree, val_low, type = "prob")[, "Uncontrolled"]
compute_metrics(p_low_tree_val > th_low_tree["best_th"], val_low$target == "Uncontrolled", val_low$mask)


###############################################################################
#### COMBINE: stitch high + low predictions, evaluate overall ####
###############################################################################

cat("\n\n====== COMBINED STRATIFIED RESULTS (overall val) ======\n\n")

# helper: combine high + low predictions and evaluate
combine_and_eval <- function(name, prob_high, th_high, prob_low, th_low) {
    # reconstruct full val predictions in original order
    pred_high <- prob_high > th_high
    pred_low <- prob_low > th_low

    # combine
    all_ids <- df_val$id
    pred_all <- logical(nrow(df_val))
    pred_all[df_val$a1c_latest >= 8] <- pred_high
    pred_all[df_val$a1c_latest < 8] <- pred_low

    cat(sprintf("------ %s ---------\n", name))
    compute_metrics(pred_all, df_val$target == "Uncontrolled", df_val$mask)

    # also return probabilities for ensemble
    prob_all <- numeric(nrow(df_val))
    prob_all[df_val$a1c_latest >= 8] <- prob_high
    prob_all[df_val$a1c_latest < 8] <- prob_low
    invisible(prob_all)
}

prob_strat_glm <- combine_and_eval("Stratified GLM",
    p_high_glm_val, th_high_glm["best_th"],
    p_low_glm_val, th_low_glm["best_th"])

prob_strat_tree <- combine_and_eval("Stratified Tree",
    p_high_tree_val, th_high_tree["best_th"],
    p_low_tree_val, th_low_tree["best_th"])

# stratified ensemble: average of stratified GLM and tree
cat("\n------ Stratified Ensemble (GLM + Tree) ---------\n")
prob_strat_ens <- (prob_strat_glm + prob_strat_tree) / 2
ens_thresh <- best_f1_threshold(prob_strat_ens, df_val$target == "Uncontrolled", df_val$mask)
ens_thresh
compute_metrics(prob_strat_ens > ens_thresh["best_th"], df_val$target == "Uncontrolled", df_val$mask)


###############################################################################
#### HYBRID: stratified HIGH model + global model for LOW ####
## Use the specialized high-a1c GLM (F1=65%) where it's strong,
## fall back to the global models for low-a1c patients
###############################################################################

cat("\n\n====== HYBRID: Stratified HIGH + Global LOW ======\n\n")

# load global model predictions (from subrata_models.R run)
global_res <- readRDS("./data/processed/val_predictions.Rds")
global_vp <- global_res$val_preds

# global model probabilities for low-a1c patients, matched by id
low_ids <- val_low$id
global_low <- global_vp %>% filter(id %in% low_ids)

# make sure ordering matches val_low
global_low <- global_low[match(low_ids, global_low$id), ]

# try each global model as the LOW source
for (global_name in c("prob_glm", "prob_glm_wt", "prob_tree", "prob_gam")) {
    global_low_probs <- global_low[[global_name]]
    global_low_th <- global_low[[paste0("th_", sub("prob_", "", global_name))]][1]

    # combine: stratified HIGH GLM + global model for LOW
    pred_all <- logical(nrow(df_val))
    pred_all[df_val$a1c_latest >= 8] <- p_high_glm_val > th_high_glm["best_th"]
    pred_all[df_val$a1c_latest < 8] <- global_low_probs > global_low_th

    cat(sprintf("------ Hybrid: HIGH=Strat.GLM + LOW=%s ---------\n", global_name))
    compute_metrics(pred_all, df_val$target == "Uncontrolled", df_val$mask)
}

# also try: global ensemble for LOW
global_low_ens <- (global_low$prob_glm + global_low$prob_glm_wt + global_low$prob_tree + global_low$prob_gam) / 4
ens_low_th <- best_f1_threshold(global_low_ens, global_low$target == "Uncontrolled", global_low$mask)

pred_all <- logical(nrow(df_val))
pred_all[df_val$a1c_latest >= 8] <- p_high_glm_val > th_high_glm["best_th"]
pred_all[df_val$a1c_latest < 8] <- global_low_ens > ens_low_th["best_th"]

cat("------ Hybrid: HIGH=Strat.GLM + LOW=Global Ensemble ---------\n")
compute_metrics(pred_all, df_val$target == "Uncontrolled", df_val$mask)

# also try: hybrid with probability averaging instead of separate thresholds
# combine probs, then find one global threshold
prob_hybrid <- numeric(nrow(df_val))
prob_hybrid[df_val$a1c_latest >= 8] <- p_high_glm_val
prob_hybrid[df_val$a1c_latest < 8] <- global_low_ens

hybrid_th <- best_f1_threshold(prob_hybrid, df_val$target == "Uncontrolled", df_val$mask)
cat("\n------ Hybrid (single threshold on combined probs) ---------\n")
hybrid_th
compute_metrics(prob_hybrid > hybrid_th["best_th"], df_val$target == "Uncontrolled", df_val$mask)


#### SAVE ####
saveRDS(list(
    prob_strat_glm = prob_strat_glm,
    prob_strat_tree = prob_strat_tree,
    prob_strat_ens = prob_strat_ens,
    prob_hybrid = prob_hybrid,
    val_ids = df_val$id,
    val_target = df_val$target,
    val_mask = df_val$mask
), file = "./data/processed/val_predictions_stratified.Rds")
cat("\n------ Saved predictions to data/processed/val_predictions_stratified.Rds ---------\n")
