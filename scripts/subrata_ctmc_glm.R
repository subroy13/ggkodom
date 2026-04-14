## Plan B — Frequentist closed-form CTMC + GLM
##
## The interpretable-model sanity check from ideas.md (Apr 7 night).
## Mirrors the data-generating-process intuition with minimal machinery:
##   1. Estimate CTMC rates (q_LH, q_HL) from observed train transitions
##      between consecutive A1c readings, treating H = (a1c >= 8).
##   2. For each patient, compute the closed-form CTMC prediction
##         ctmc_prob = pi_H + (state_last - pi_H) * exp(-lambda * time_gap_yr)
##      which is the optimal forecast under a 2-state CTMC given (last
##      observed state, time gap to 2025).
##   3. Fit four nested logistic models that combine a1c_weighted and
##      ctmc_prob, tune the threshold via best_f1_threshold on train,
##      and report val F1 for each.
##
## Decision rule (from ideas.md): if any model clears ~61.5% on val,
## the structural form (level + CTMC dynamics) is right, and the
## Bayesian retry (Plan A) is worth running. If all models miss, the
## structural form is wrong and Plan A would also fail.
##
## Run:  Rscript scripts/subrata_ctmc_glm.R

setwd("/home/subrata/Research/Research_Claude/washu-datathon-2026")
suppressPackageStartupMessages({
    library(tidyverse)
})
source("./scripts/utils.R")
source("./scripts/subrata_features.R")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")

df_train <- impute_and_flag(create_features(train_dat))
df_val   <- impute_and_flag(create_features(val_dat))

cat("Train rows:", nrow(df_train),
    "| returned:", sum(df_train$mask, na.rm = TRUE),
    "| pos rate (returned):",
    round(mean((df_train$target == "Uncontrolled")[df_train$mask], na.rm = TRUE), 3), "\n")
cat("Val   rows:", nrow(df_val),
    "| returned:", sum(df_val$mask, na.rm = TRUE),
    "| pos rate (returned):",
    round(mean((df_val$target == "Uncontrolled")[df_val$mask], na.rm = TRUE), 3), "\n\n")


#### Step 1 — Estimate CTMC rates from train transitions ####
##
## H = a1c >= 8.0 (competition threshold). For each pair of consecutive
## a1c readings (a1c_k, a1c_{k+1}) within a patient, record the start
## state, end state, and time delta (in years). MLE for the CTMC rates
## under the assumption that the process is observed at irregular times:
##
##   q_LH ≈ (number of L→H transitions) / (total person-time spent in L)
##   q_HL ≈ (number of H→L transitions) / (total person-time spent in H)
##
## This is the naive empirical estimator, which the audit's
## subrata_continuous_time.R reports as q_LH ≈ 0.24/yr, q_HL ≈ 1.25/yr.
## We re-estimate here so the script is self-contained.

a1c_long <- train_dat$measurements %>%
    filter(variable %in% paste0("a1c_", 1:5)) %>%
    drop_na(time, value) %>%
    arrange(id, time)

transitions <- a1c_long %>%
    group_by(id) %>%
    mutate(
        next_value = lead(value),
        next_time  = lead(time),
        state      = as.integer(value >= 8),
        next_state = as.integer(next_value >= 8),
        delta_t_yr = (next_time - time) / 365.25
    ) %>%
    ungroup() %>%
    filter(!is.na(next_state), delta_t_yr > 0)

n_LH <- sum(transitions$state == 0 & transitions$next_state == 1)
n_HL <- sum(transitions$state == 1 & transitions$next_state == 0)
time_in_L <- sum(transitions$delta_t_yr[transitions$state == 0])
time_in_H <- sum(transitions$delta_t_yr[transitions$state == 1])

q_LH <- n_LH / time_in_L
q_HL <- n_HL / time_in_H
lambda <- q_LH + q_HL
pi_H   <- q_LH / lambda

cat("CTMC parameters (estimated from train transitions):\n")
cat(sprintf("  Transitions used: %d\n", nrow(transitions)))
cat(sprintf("  L->H transitions: %d (over %.1f person-yrs in L) -> q_LH = %.3f /yr\n",
            n_LH, time_in_L, q_LH))
cat(sprintf("  H->L transitions: %d (over %.1f person-yrs in H) -> q_HL = %.3f /yr\n",
            n_HL, time_in_H, q_HL))
cat(sprintf("  lambda = q_LH + q_HL = %.3f /yr (relaxation time = %.2f yr = %.1f months)\n",
            lambda, 1 / lambda, 12 / lambda))
cat(sprintf("  pi_H (stationary fraction H) = %.3f\n\n", pi_H))


#### Step 2 — Compute ctmc_prob for each patient ####
##
## state_last = 1 if a1c_latest >= 8, else 0.
## time_gap_yr = days from latest a1c reading to the 2025 evaluation, in years.
## ctmc_prob = pi_H + (state_last - pi_H) * exp(-lambda * time_gap_yr)
##
## For patients with no time_gap (e.g. no 2025 eval date), set ctmc_prob = pi_H
## (revert to base rate).

compute_ctmc_prob <- function(df, q_LH, q_HL) {
    lambda <- q_LH + q_HL
    pi_H   <- q_LH / lambda
    state_last <- as.integer(df$a1c_latest >= 8)
    state_last[is.na(state_last)] <- 0
    time_gap_yr <- df$time_gap / 365.25
    time_gap_yr[is.na(time_gap_yr) | time_gap_yr < 0] <- 1 / lambda  # default = relaxation time
    pi_H + (state_last - pi_H) * exp(-lambda * time_gap_yr)
}

df_train$ctmc_prob <- compute_ctmc_prob(df_train, q_LH, q_HL)
df_val$ctmc_prob   <- compute_ctmc_prob(df_val, q_LH, q_HL)

cat("ctmc_prob distribution (train, returned only):\n")
print(round(quantile(df_train$ctmc_prob[df_train$mask], c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE), 4))
cat("ctmc_prob distribution (val, returned only):\n")
print(round(quantile(df_val$ctmc_prob[df_val$mask], c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE), 4))
cat("\n")


#### Step 3 — Fit four nested logistic models ####
##
## All targets/threshold tuning use the mask (returned patients only).

fit_and_eval <- function(formula, label, df_tr, df_va) {
    cat("\n========== ", label, " ==========\n", sep = "")
    m <- glm(formula, data = df_tr, family = binomial)
    cat("Coefficients:\n")
    print(round(coef(summary(m)), 4))
    cat(sprintf("AIC = %.1f | df = %d\n", AIC(m), length(coef(m))))

    p_tr <- predict(m, df_tr, type = "response")
    p_va <- predict(m, df_va, type = "response")
    th   <- best_f1_threshold(p_tr, df_tr$target == "Uncontrolled", df_tr$mask)
    cat(sprintf("Train-tuned threshold: %.3f | train F1: %.4f\n", th["best_th"], th["best_f1"]))

    cat("Val metrics:\n")
    res <- compute_metrics(p_va > th["best_th"],
                           df_va$target == "Uncontrolled",
                           df_va$mask,
                           verbose = TRUE)
    invisible(list(model = m, train_f1 = unname(th["best_f1"]),
                   val_f1 = unname(res$metrics["F1"]),
                   val_prec = unname(res$metrics["Precision"]),
                   val_rec  = unname(res$metrics["Recall"]),
                   th = unname(th["best_th"]),
                   n_params = length(coef(m))))
}

results <- list()

#### M1: a1c_weighted alone (the existing 1-feature baseline = 61.4%) ####
results$M1 <- fit_and_eval(
    target ~ a1c_weighted,
    "M1: a1c_weighted only (2 params; baseline)",
    df_train, df_val
)

#### M2: ctmc_prob alone (2-parameter "pure DGP" model) ####
results$M2 <- fit_and_eval(
    target ~ ctmc_prob,
    "M2: ctmc_prob only (2 params; pure DGP)",
    df_train, df_val
)

#### M3: a1c_weighted + ctmc_prob (3 params; level + dynamics, additive) ####
results$M3 <- fit_and_eval(
    target ~ a1c_weighted + ctmc_prob,
    "M3: a1c_weighted + ctmc_prob (3 params; level + dynamics)",
    df_train, df_val
)

#### M4: a1c_weighted * ctmc_prob (4 params; with interaction) ####
results$M4 <- fit_and_eval(
    target ~ a1c_weighted * ctmc_prob,
    "M4: a1c_weighted * ctmc_prob (4 params; level + dynamics + interaction)",
    df_train, df_val
)

#### Optional sanity: identity link, just thresholding ctmc_prob directly ####
cat("\n========== M0: ctmc_prob raw (no GLM, threshold on ctmc_prob itself) ==========\n")
th0 <- best_f1_threshold(df_train$ctmc_prob,
                          df_train$target == "Uncontrolled",
                          df_train$mask)
cat(sprintf("Train-tuned threshold on raw ctmc_prob: %.3f | train F1: %.4f\n",
            th0["best_th"], th0["best_f1"]))
res0 <- compute_metrics(df_val$ctmc_prob > th0["best_th"],
                        df_val$target == "Uncontrolled",
                        df_val$mask,
                        verbose = TRUE)
results$M0 <- list(train_f1 = unname(th0["best_f1"]),
                   val_f1 = unname(res0$metrics["F1"]),
                   val_prec = unname(res0$metrics["Precision"]),
                   val_rec  = unname(res0$metrics["Recall"]),
                   th = unname(th0["best_th"]),
                   n_params = 0)


#### Summary table ####
cat("\n\n========== SUMMARY ==========\n")
summary_df <- tibble(
    model = c("M0 (ctmc_prob raw, no GLM)",
              "M1 (a1c_weighted only)",
              "M2 (ctmc_prob only)",
              "M3 (a1c_weighted + ctmc_prob)",
              "M4 (a1c_weighted * ctmc_prob)"),
    n_params = sapply(results[c("M0","M1","M2","M3","M4")], function(x) x$n_params),
    train_f1 = sapply(results[c("M0","M1","M2","M3","M4")], function(x) x$train_f1),
    val_f1   = sapply(results[c("M0","M1","M2","M3","M4")], function(x) x$val_f1),
    val_prec = sapply(results[c("M0","M1","M2","M3","M4")], function(x) x$val_prec),
    val_rec  = sapply(results[c("M0","M1","M2","M3","M4")], function(x) x$val_rec),
    threshold = sapply(results[c("M0","M1","M2","M3","M4")], function(x) x$th)
)
print(summary_df, n = Inf)

cat("\nDecision rule from ideas.md (Apr 7 night):\n")
cat("  If any of M2/M3/M4 clears ~61.5% on val => structural form is right => run Plan A (Bayesian retry).\n")
cat("  If all miss => structural form is wrong => Plan A would also fail.\n\n")

best_dynamics_f1 <- max(c(results$M2$val_f1, results$M3$val_f1, results$M4$val_f1))
cat(sprintf("Best dynamics-aware model val F1: %.4f\n", best_dynamics_f1))
cat(sprintf("Reference (regularized XGBoost): 0.6200\n"))
cat(sprintf("Reference (a1c_weighted alone, M1): %.4f\n", results$M1$val_f1))

if (best_dynamics_f1 >= 0.615) {
    cat("\n>>> DECISION: Plan A (Bayesian retry) is worth running.\n")
} else if (best_dynamics_f1 >= 0.605) {
    cat("\n>>> DECISION: Borderline. The CTMC structure adds little. Plan A risk is moderate.\n")
} else {
    cat("\n>>> DECISION: Plan A would also miss. Need a different structural form.\n")
}

cat("\n#### DONE ####\n")
