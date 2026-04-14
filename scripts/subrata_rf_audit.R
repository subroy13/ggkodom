## Standalone RF audit — mirrors the RF block in scripts/subrata_models.R
##
## Purpose: get the clean val F1 for ranger RF, separate from the
## df_train_imputed <-> df_val_imputed pointer swap in subrata_models.R.
## Subhrajyoty's printed metric was train F1 with a val-tuned threshold;
## this script reports four numbers so we can decide whether RF actually
## clears 62% on val (and therefore deserves a slot in the submission).
##
## Run from project root:  Rscript scripts/subrata_rf_audit.R

setwd("/home/subrata/Research/Research_Claude/washu-datathon-2026")
suppressPackageStartupMessages({
    library(tidyverse)
    library(ranger)
})
source("./scripts/utils.R")
source("./scripts/subrata_features.R")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")

df_train <- create_features(train_dat)
df_val   <- create_features(val_dat)
df_train_imputed <- impute_and_flag(df_train)
df_val_imputed   <- impute_and_flag(df_val)

cat("Train rows:", nrow(df_train_imputed),
    "| returned:", sum(df_train_imputed$mask, na.rm = TRUE),
    "| pos rate (returned):",
    round(mean((df_train_imputed$target == "Uncontrolled")[df_train_imputed$mask], na.rm = TRUE), 3),
    "\n")
cat("Val   rows:", nrow(df_val_imputed),
    "| returned:", sum(df_val_imputed$mask, na.rm = TRUE),
    "| pos rate (returned):",
    round(mean((df_val_imputed$target == "Uncontrolled")[df_val_imputed$mask], na.rm = TRUE), 3),
    "\n\n")

#### Feature columns (mirror subrata_models.R) ####
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
)
dt_formula <- as.formula(paste("target ~", paste(feat_cols, collapse = " + ")))

#### Fit RF — same hyperparameters as Subhrajyoty's block ####
cat("Fitting ranger ...\n")
set.seed(1)
t0 <- Sys.time()
rf_model <- ranger(
    dt_formula,
    data         = df_train_imputed,
    num.trees    = 200,
    importance   = "impurity",
    probability  = TRUE,
    mtry         = 10,
    max.depth    = 7,
    min.bucket   = 25,
    num.threads  = 6
)
cat("Fit took ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s\n\n", sep = "")

#### Predict on REAL train and REAL val (no pointer swap) ####
p_rf_train <- predict(rf_model, df_train_imputed, type = "response")$predictions[, "Uncontrolled"]
p_rf_val   <- predict(rf_model, df_val_imputed,   type = "response")$predictions[, "Uncontrolled"]

#### Threshold from REAL train (the only clean choice) ####
th_train <- best_f1_threshold(
    p_rf_train,
    df_train_imputed$target == "Uncontrolled",
    df_train_imputed$mask
)
cat("Train-tuned threshold (returned only): ", th_train, "\n", sep = "")

#### Threshold from REAL val (data leak — for comparison only) ####
th_val_leak <- best_f1_threshold(
    p_rf_val,
    df_val_imputed$target == "Uncontrolled",
    df_val_imputed$mask
)
cat("Val-tuned threshold (data-leaking, comparison only): ", th_val_leak, "\n\n", sep = "")

#### Reproduce Subhrajyoty's number: train F1 with val-tuned threshold ####
cat("\n=========== (A) Subhrajyoty's printed metric ===========\n")
cat("Train F1 with val-tuned threshold (what subrata_models.R currently prints):\n")
compute_metrics(
    p_rf_train > th_val_leak["best_th"],
    df_train_imputed$target == "Uncontrolled",
    df_train_imputed$mask
)

#### CLEAN: train-tuned threshold, eval on val ####
cat("\n=========== (B) CLEAN val F1 (train-tuned threshold) — the honest number ===========\n")
compute_metrics(
    p_rf_val > th_train["best_th"],
    df_val_imputed$target == "Uncontrolled",
    df_val_imputed$mask
)

#### Optimistic: val-tuned threshold, eval on val (upper bound) ####
cat("\n=========== (C) Optimistic val F1 (val-tuned threshold, eval on val) ===========\n")
compute_metrics(
    p_rf_val > th_val_leak["best_th"],
    df_val_imputed$target == "Uncontrolled",
    df_val_imputed$mask
)

#### Sanity: train F1 with train-tuned threshold (in-sample, expect optimism) ####
cat("\n=========== (D) In-sample train F1 (train-tuned threshold) ===========\n")
compute_metrics(
    p_rf_train > th_train["best_th"],
    df_train_imputed$target == "Uncontrolled",
    df_train_imputed$mask
)

#### Multi-seed sweep — bound seed variance for the 63% claim ####
cat("\n=========== (E) Seed sweep — clean val F1 across 5 seeds ===========\n")
sweep_seeds <- 1:5
sweep_res <- lapply(sweep_seeds, function(s) {
    set.seed(s)
    rf_s <- ranger(
        dt_formula,
        data         = df_train_imputed,
        num.trees    = 200,
        importance   = "none",
        probability  = TRUE,
        mtry         = 10,
        max.depth    = 7,
        min.bucket   = 25,
        num.threads  = 6
    )
    p_tr <- predict(rf_s, df_train_imputed, type = "response")$predictions[, "Uncontrolled"]
    p_va <- predict(rf_s, df_val_imputed,   type = "response")$predictions[, "Uncontrolled"]
    th_tr <- best_f1_threshold(p_tr, df_train_imputed$target == "Uncontrolled", df_train_imputed$mask)
    th_va <- best_f1_threshold(p_va, df_val_imputed$target   == "Uncontrolled", df_val_imputed$mask)

    # Clean val F1 (train-tuned threshold)
    m_clean <- compute_metrics(
        p_va > th_tr["best_th"],
        df_val_imputed$target == "Uncontrolled",
        df_val_imputed$mask,
        verbose = FALSE
    )$metrics

    # Optimistic val F1 (val-tuned threshold)
    m_opt <- compute_metrics(
        p_va > th_va["best_th"],
        df_val_imputed$target == "Uncontrolled",
        df_val_imputed$mask,
        verbose = FALSE
    )$metrics

    tibble(
        seed       = s,
        th_train   = th_tr["best_th"],
        th_val     = th_va["best_th"],
        clean_val_f1     = unname(m_clean["F1"]),
        clean_val_prec   = unname(m_clean["Precision"]),
        clean_val_rec    = unname(m_clean["Recall"]),
        opt_val_f1       = unname(m_opt["F1"])
    )
})
sweep_df <- bind_rows(sweep_res)
print(sweep_df, n = Inf)

cat("\nclean_val_f1 across seeds:\n")
cat("  mean: ", round(mean(sweep_df$clean_val_f1) * 100, 2), "%\n", sep = "")
cat("  sd:   ", round(sd(sweep_df$clean_val_f1)   * 100, 3), "%\n", sep = "")
cat("  range:", round(min(sweep_df$clean_val_f1) * 100, 2), "% - ",
    round(max(sweep_df$clean_val_f1) * 100, 2), "%\n", sep = "")
cat("  Subhrajyoty's claim: 63.00%\n")
cat("  Distance from claim to nearest seed: ",
    round((0.63 - max(sweep_df$clean_val_f1)) * 100, 2), " pp\n", sep = "")

cat("\nopt_val_f1 across seeds (val-tuned threshold = upper bound):\n")
cat("  mean: ", round(mean(sweep_df$opt_val_f1) * 100, 2), "%\n", sep = "")
cat("  range:", round(min(sweep_df$opt_val_f1) * 100, 2), "% - ",
    round(max(sweep_df$opt_val_f1) * 100, 2), "%\n", sep = "")

cat("\n#### DONE ####\n")
