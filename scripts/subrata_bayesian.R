## Bayesian retry — minimum-distance fix to subh_bayesian.R
##
## Subhrajyoty's original ran on 1000 sampled patients with diffuse priors and
## reported "bad F1". The audit on Apr 7 (night) found the F1 was bad for THREE
## reasons that have nothing to do with the Bayesian model itself:
##
##   1. target_labels was set to the numeric A1c value (e.g., 5.5, 8.2),
##      not the binary `value > A_star` indicator. Inside compute_metrics,
##      as.logical(numeric) is TRUE for any non-zero value, so almost every
##      returned patient was labeled "uncontrolled" -> base rate ~100%, F1 floor.
##   2. The prediction returned pnorm(A_star, mean, sd) = P(Y <= A_star) = P(controlled),
##      not P(Y > A_star) = P(uncontrolled). Variable was named "uncontrolled" but
##      held the wrong direction.
##   3. target_mask was set to the numeric `time` column, not !is.na(time).
##      Accidentally close to right but semantically wrong.
##
## This file fixes all three, increases the training subsample from 1000 to
## 2000 patients (still tractable for JAGS), and routes the predictions through
## scripts/subrata_bayesian_eval_harness.R for proper F1/log-loss/Brier/ECE/
## calibration reporting.
##
## What this file does NOT change (deliberate, one-thing-at-a-time):
##   - The JAGS model itself (still scripts/model.jags). If F1 is still bad
##     after the bug fixes, the next experiment is a separate
##     scripts/subrata_model.jags with tightened priors from the lmer/CTMC
##     audit. Doing both at once would make a failure unattributable.
##
## Run:  Rscript scripts/subrata_bayesian.R
## Output:  scripts/outfiles/subrata_bayesian.Rout

setwd("/home/subrata/Research/Research_Claude/washu-datathon-2026")
suppressPackageStartupMessages({
    library(tidyverse)
    library(runjags)
})
source("./scripts/utils.R")
source("./scripts/subrata_bayesian_eval_harness.R")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")


#### Step 1 — Prepare JAGS inputs (unchanged from subh_bayesian.R) ####

compute_bayesian_inputs <- function(train_dat, cov_formula, A_star = 8.0) {
    prepared_data <- train_dat$measurements %>%
        filter(variable %in% c(paste0("a1c_", 1:5), "a1c_2025")) %>%
        drop_na() %>%
        arrange(id, time) %>%
        group_by(id) %>%
        mutate(
            obs_number = row_number(),
            n_obs = n(),
            dt = time - lag(time)
        ) %>%
        ungroup() %>%
        filter(n_obs > 1) %>%
        mutate(
            seq_id = as.numeric(factor(id)),
            is_first = as.numeric(obs_number == 1),
            dt = if_else(is.na(dt), 1, dt),
            ## med_flag uses the previous reading. For continuous a1c readings (1..5),
            ## previous_value is in A1c units and "treatment kicks in if above 8" is
            ## the right condition. For the very rare case where the previous row is
            ## a1c_2025 (the binary indicator) — that doesn't happen because a1c_2025
            ## is always the LAST row by time-sort, so med_flag for it is correctly 1
            ## iff the prior a1c reading was > 8. Safe.
            med_flag = if_else(is_first == 0 & lag(value) > A_star, 1, 0),
            is_last = as.numeric(obs_number == n_obs),
            Y_jags = if_else(is_last == 1, NA_real_, value),
            ## CRITICAL FIX (Apr 8): the `value` column for variable == "a1c_2025"
            ## is the BINARY uncontrolled label (0/1) from dm_control, NOT the
            ## continuous A1c reading. So `value > A_star` (i.e., > 8) is always
            ## FALSE for a1c_2025 rows, and the original code labeled every
            ## returned patient as "controlled" in the interval-censored target.
            ## The correct comparison is `value > 0` (1 = uncontrolled, 0 = controlled).
            ##
            ## For the rare case where the last row is a non-a1c_2025 reading
            ## (i.e. non-returned patients), we keep the original `value > A_star`
            ## semantics so the random-walk likelihood still uses the continuous
            ## value as evidence. variable == "a1c_2025" is a clean discriminator.
            is_above_A = case_when(
                is_last == 1 & variable == "a1c_2025" & value > 0 ~ 1,
                is_last == 1 & variable == "a1c_2025" & value <= 0 ~ 0,
                is_last == 1 & variable != "a1c_2025" & value > A_star ~ 1,
                is_last == 1 & variable != "a1c_2025" & value <= A_star ~ 0,
                TRUE ~ NA_real_
            )
        )

    first_idx <- which(prepared_data$is_first == 1)
    subseq_idx <- which(prepared_data$is_first == 0)

    patient_data <- train_dat$basic %>%
        left_join(train_dat$counts, by = "id") %>%
        filter(id %in% unique(prepared_data$id)) %>%
        arrange(id)

    M_matrix <- model.matrix(cov_formula, data = patient_data)

    list(
        N_patients = length(unique(prepared_data$seq_id)),
        N_total    = nrow(prepared_data),
        N_subseq   = length(subseq_idx),
        Y          = prepared_data$Y_jags,
        is_above_A = prepared_data$is_above_A,
        A_star     = A_star,
        dt         = prepared_data$dt,
        id         = prepared_data$seq_id,
        first_idx  = first_idx,
        subseq_idx = subseq_idx,
        med_flag   = prepared_data$med_flag,
        X          = M_matrix,
        p          = ncol(M_matrix)
    )
}


covariate_formula <- ~ age + cad + copd + ed_visit + pcp_visit + admission +
    glp1 + insulin + metformin + sglt2 + sulfonylurea + dpp4 - 1


#### Step 2 — Fit JAGS on a 2000-patient subsample ####

SUBSAMPLE_N <- 2000
cat(sprintf("Subsampling %d training patients (seed=1)...\n", SUBSAMPLE_N))
train_sample_dat <- sample_dat(train_dat, sample_size = SUBSAMPLE_N, seed = 1)
jags_data <- compute_bayesian_inputs(train_sample_dat, covariate_formula, A_star = 8.0)
cat(sprintf("After multi-reading filter: %d patients, %d total observations\n",
            jags_data$N_patients, jags_data$N_total))

## Cache JAGS MCMC samples so re-runs (e.g. to fix downstream eval bugs) don't
## pay another 26 minutes of sampling. Cache key includes the subsample size
## AND the model file basename, so switching between model.jags and
## subrata_model.jags (informative priors) uses different caches.
USE_INFORMATIVE_PRIORS <- TRUE  # Apr 8: switched to subrata_model.jags
MODEL_FILE <- if (USE_INFORMATIVE_PRIORS) "./scripts/subrata_model.jags" else "./scripts/model.jags"
MODEL_TAG  <- if (USE_INFORMATIVE_PRIORS) "informative" else "diffuse"
mcmc_cache_path <- sprintf("./scripts/outfiles/subrata_bayesian_mcmc_n%d_%s.Rds", SUBSAMPLE_N, MODEL_TAG)

if (file.exists(mcmc_cache_path)) {
    cat(sprintf("Loading cached MCMC samples from %s ...\n", mcmc_cache_path))
    cached <- readRDS(mcmc_cache_path)
    mcmc_samples <- cached$mcmc_samples
    fit_summary  <- cached$summary
    cat(sprintf("Loaded %d samples x %d params\n", nrow(mcmc_samples), ncol(mcmc_samples)))
    cat("\n--- JAGS summary (from cache) ---\n")
    print(fit_summary)
} else {
    cat(sprintf("No cache found. Fitting JAGS using model = %s\n", MODEL_FILE))
    cat("(4 chains, 1000 adapt, 1000 burnin, 2000 samples)...\n")
    t0 <- Sys.time()
    fit <- run.jags(
        model   = MODEL_FILE,
        monitor = c("alpha_delta", "alpha_prec", "beta_delta", "beta_prec", "prec_Y"),
        data    = jags_data,
        n.chains = 4,
        adapt   = 1000,
        burnin  = 1000,
        sample  = 2000,
        method  = "parallel"
    )
    cat(sprintf("JAGS fit took %.1f minutes\n\n",
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))

    cat("\n--- JAGS summary ---\n")
    fit_summary <- summary(fit)
    print(fit_summary)

    mcmc_samples <- as.matrix(fit$mcmc)
    cat(sprintf("\nPosterior matrix: %d samples x %d params\n",
                nrow(mcmc_samples), ncol(mcmc_samples)))

    saveRDS(list(mcmc_samples = mcmc_samples, summary = fit_summary), mcmc_cache_path)
    cat(sprintf("Cached MCMC samples to %s\n", mcmc_cache_path))
}


#### Step 3 — Predict (with the pnorm direction FIXED) ####

predict_jags_fixed <- function(mcmc_samples, jags_sim, sim_dat) {
    ## CHANGE (Apr 8): use the EWMA of all a1c readings (a1c_weighted) as the
    ## starting state for prediction, not just the last reading. The original
    ## code used `slice_tail()` which discards the EWMA signal that drives the
    ## ~61.4% F1 floor for a1c_weighted alone. With only the last reading the
    ## prediction step throws away most of the patient-level information that
    ## the JAGS model could in principle use, and F1 falls below the GLM
    ## baseline (we observed 0.5737 in the previous run).
    ##
    ## The starting state for the latent state-space projection is now the
    ## EWMA of the patient's full A1c history, which approximates the BLUP /
    ## posterior mean of µ at the time of the last reading (modulo the JAGS
    ## random-walk shrinkage). The time-gap and treatment-drift logic is
    ## unchanged.
    sim_prepared <- sim_dat$measurements %>%
        filter(variable %in% paste0("a1c_", 1:5)) %>%
        drop_na() %>%
        arrange(id, time) %>%
        group_by(id) %>%
        summarise(
            n_a1c        = n(),
            last_value   = last(value),
            last_time    = last(time),
            a1c_weighted = {
                v <- value
                k <- length(v)
                w <- 0.5^rev(seq_len(k) - 1)
                sum(v * w) / sum(w)
            },
            .groups = "drop"
        ) %>%
        left_join(
            sim_dat$measurements %>%
                filter(variable == "a1c_2025") %>%
                select(id, a1c_2025_time = time, a1c_2025_value = value),
            by = "id"
        ) %>%
        mutate(
            dt    = a1c_2025_time - last_time,
            value = a1c_weighted     # alias so the rest of the code reads naturally
        )

    sim_patients <- sim_dat$basic %>%
        left_join(sim_dat$counts, by = "id") %>%
        filter(id %in% unique(sim_prepared$id)) %>%
        arrange(id)
    sim_X <- model.matrix(covariate_formula, data = sim_patients)

    p <- jags_sim$p
    A_star <- jags_sim$A_star
    exceedence_counts <- numeric(nrow(sim_prepared))

    pb <- txtProgressBar(min = 1, max = nrow(mcmc_samples), style = 3)
    for (b in seq_len(nrow(mcmc_samples))) {
        param <- mcmc_samples[b, ]
        alpha_delta <- param["alpha_delta"]
        alpha_prec  <- param["alpha_prec"]
        beta_delta  <- param[paste0("beta_delta[", 1:p, "]")]
        beta_prec   <- param[paste0("beta_prec[",  1:p, "]")]
        prec_Y      <- param["prec_Y"]

        delta_i <- as.numeric(exp(alpha_delta + (sim_X %*% beta_delta)))
        prec_i  <- as.numeric(exp(alpha_prec  + (sim_X %*% beta_prec)))

        ## Predicted mean of Y_2025 given last observation
        ##   - if currently above A_star: treatment kicks in -> mean = value - delta_i
        ##   - if currently below A_star: no treatment -> mean = value
        ## Predicted variance:
        ##   - latent random-walk variance: dt / prec_i
        ##   - observation noise: 1 / prec_Y
        ##   - total: dt / prec_i + 1 / prec_Y
        pred_mean <- ifelse(
            sim_prepared$value > A_star,
            sim_prepared$value - delta_i,
            sim_prepared$value
        )
        pred_var <- (sim_prepared$dt / prec_i) + (1 / prec_Y)
        pred_var[is.na(pred_var) | pred_var <= 0] <- 1 / prec_Y  # safety

        ## *** FIX: P(Y_2025 > A_star) = upper tail, NOT pnorm(A_star, ...) ***
        prob_uncontrolled <- ifelse(
            is.na(sim_prepared$a1c_2025_time),
            0,  # no exceedance if not measured (will be masked out anyway)
            pnorm(A_star, mean = pred_mean, sd = sqrt(pred_var), lower.tail = FALSE)
        )

        exceedence_counts <- exceedence_counts + prob_uncontrolled
        setTxtProgressBar(pb, b)
    }
    close(pb)

    pred_probs <- exceedence_counts / nrow(mcmc_samples)

    ## NOTE on target encoding: in cleaning.R, the `value` column for a1c_2025
    ## is the BINARY uncontrolled label (0/1) from `dm_control$\`a1c 2025 Uncontrolled\``,
    ## NOT the continuous A1c value. So target_uncontrolled = as.logical(a1c_2025_value),
    ## not (a1c_2025_value > A_star).
    sim_prepared %>%
        mutate(
            pred_prob = pred_probs,
            target_uncontrolled = as.logical(a1c_2025_value),
            mask = !is.na(a1c_2025_time)
        ) %>%
        select(id, pred_prob, target_uncontrolled, mask)
}


#### Step 4 — Predict on TRAIN (for threshold tuning) and VAL (for evaluation) ####

cat("\n--- Predicting on training set (for threshold tuning) ---\n")
train_pred <- predict_jags_fixed(
    mcmc_samples,
    compute_bayesian_inputs(train_dat, covariate_formula, A_star = 8.0),
    train_dat
)

cat("\n--- Predicting on validation set ---\n")
val_pred <- predict_jags_fixed(
    mcmc_samples,
    compute_bayesian_inputs(val_dat, covariate_formula, A_star = 8.0),
    val_dat
)

cat(sprintf("\nTrain pred rows: %d (returned: %d, base rate: %.4f)\n",
            nrow(train_pred), sum(train_pred$mask, na.rm = TRUE),
            mean(train_pred$target_uncontrolled[train_pred$mask], na.rm = TRUE)))
cat(sprintf("Val pred rows:   %d (returned: %d, base rate: %.4f)\n",
            nrow(val_pred), sum(val_pred$mask, na.rm = TRUE),
            mean(val_pred$target_uncontrolled[val_pred$mask], na.rm = TRUE)))


#### Step 5 — Run the eval harness ####

eval_res <- eval_predictions(
    probs        = val_pred$pred_prob,
    target       = val_pred$target_uncontrolled,
    mask         = val_pred$mask,
    train_probs  = train_pred$pred_prob,
    train_target = train_pred$target_uncontrolled,
    train_mask   = train_pred$mask
)

print_eval(eval_res, label = "Bayesian retry (bug-fixed, 2000-patient subsample, original priors)")


#### Step 6 — Cross-reference with the 62% ceiling and the AIC story ####

cat("\n========== Reference numbers (val split, returned only) ==========\n")
cat("XGBoost regularized (best black-box):     0.6200\n")
cat("a1c_weighted alone (M1 from Plan B):       0.6140\n")
cat("a1c_weighted * ctmc_prob (M4 from Plan B): 0.6131\n")
cat("RF (audited, 5-seed mean):                 0.6162\n")
cat(sprintf("Bayesian (this run, train-tuned th):       %.4f\n", eval_res$f1_train_tuned))

cat("\nReminder of the SPECULATIONS this run does NOT settle:\n")
cat("  - The 'tight 60-63% band' could be biology, not designed-competition.\n")
cat("  - The Bayesian model is expected to MATCH M1 on F1, not beat it,\n")
cat("    because F1 is rank-based and the dynamics show up in calibration.\n")
cat("  - If F1 is bad here, it's still informative: either (a) the bug-fix\n")
cat("    was insufficient, in which case we try priors, or (b) the JAGS model\n")
cat("    structure doesn't match the simulator, in which case we re-think.\n")

cat("\n#### DONE ####\n")
