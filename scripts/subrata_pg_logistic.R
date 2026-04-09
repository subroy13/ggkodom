## Sayan's Polya-Gamma Bayesian logistic + missing-data Gibbs, adapted to
## our train.Rds / val.Rds pipeline.
##
## ATTRIBUTION:
##   The core Gibbs sampler (`pg_logistic_gibbs_missing`, `update_continuous_pg`,
##   `update_binary_pg`, `draw_beta_pg`) is reproduced from `scripts/Bayesian.R`
##   by Sayan with no functional changes. Only the data loading, the feature
##   matrix construction, and the val-set posterior predictive are mine.
##
## What this file is for:
##   Sayan's original `Bayesian.R` reads from `~/Library/.../Box-Box/.../Fulldata.csv`
##   on his Mac (not in the repo) and evaluates F1 in-sample on the same `dat_small`
##   it fit to (no train/val split, no mask). This adaptation:
##     1. builds Sayan's 9-feature matrix from our train.Rds / val.Rds via
##        scripts/subrata_features.R, so it runs without any external CSVs;
##     2. fits the PG Gibbs on RETURNED training patients only (mask == TRUE),
##        so y is well-defined and the model isn't biased by trivial-Controlled
##        non-returned patients;
##     3. predicts on val by combining posterior beta_draws with prior-based
##        imputation of missing val covariates, the proper Bayesian out-of-sample
##        story instead of in-sample F1;
##     4. routes the val predictions through scripts/subrata_bayesian_eval_harness.R
##        for F1 + log-loss + Brier + ECE + calibration table.
##
## Why this matters:
##   - Sayan's approach treats the problem as cross-sectional logistic regression
##     with proper Bayesian missing-data imputation. It does NOT model trajectory
##     dynamics. This is COMPLEMENTARY to subrata_bayesian.R (longitudinal JAGS)
##     and to the GLM family — it's the "missing-data done right" angle.
##   - The most striking finding from Sayan's header comment is that
##     `a1c_prop_gt8` has the largest posterior mean (1.36), bigger than
##     `a1c_last` (0.17). This matches our residual analysis from Apr 5
##     (frac_above_8 x a1c_weighted had LR = 515) — two independent Bayesian
##     analyses converging on "fraction above the threshold beats the latest reading".
##   - Expected val F1: ~61% (matches the ceiling). Like the longitudinal
##     Bayesian, the win is calibration / log-loss / Brier, not F1.
##
## Run:  Rscript scripts/subrata_pg_logistic.R
## Output:  scripts/outfiles/subrata_pg_logistic.Rout

setwd("/home/subrata/Research/Research_Claude/washu-datathon-2026")
suppressPackageStartupMessages({
    library(tidyverse)
    library(BayesLogit)
})
source("./scripts/utils.R")
source("./scripts/subrata_features.R")
source("./scripts/subrata_bayesian_eval_harness.R")


train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")


#### Step 1 — Build Sayan's 9-feature matrix from our pipeline ####
##
## Sayan's feature list and how each maps to our pipeline:
##
##   a1c_last      <- a1c_latest                       (latest A1c reading)
##   a1c_log_gap   <- log1p(time_gap)                  (days from last A1c -> 2025 eval)
##   a1c_ever_gt8  <- as.numeric(any a1c reading >= 8) (binary)
##   a1c_prop_gt8  <- frac_above_8                     (fraction of readings >= 8)
##   height        <- value_height                     (latest height; mostly missing)
##   ldl_log_gap   <- log1p(days_to_eval - time_ldl)   (days from last LDL -> 2025 eval)
##   hdl_log_gap   <- log1p(days_to_eval - time_hdl)   (days from last HDL -> 2025 eval)
##   insulin_bin   <- as.numeric(insulin > 0)          (any insulin order)
##   insulin_log   <- log1p(insulin)                   (log cumulative insulin orders)
##
## NaNs in `_log_gap` features (when the lab was never measured) are kept as NA
## so the Gibbs sampler imputes them.

build_sayan_features <- function(dat) {
    df <- create_features(dat)  # NOTE: NOT impute_and_flag — keep NAs

    df %>%
        transmute(
            id,
            mask,
            target,
            ## y will be set later, after filtering
            a1c_last     = a1c_latest,
            a1c_log_gap  = log1p(pmax(time_gap, 0)),
            a1c_ever_gt8 = as.numeric(frac_above_8 > 0),
            a1c_prop_gt8 = frac_above_8,
            height       = value_height,
            ldl_log_gap  = log1p(pmax(days_to_eval - time_ldl, 0)),
            hdl_log_gap  = log1p(pmax(days_to_eval - time_hdl, 0)),
            insulin_bin  = as.numeric(insulin > 0),
            insulin_log  = log1p(insulin)
        )
}

train_feat <- build_sayan_features(train_dat)
val_feat   <- build_sayan_features(val_dat)

cat(sprintf("Train: %d rows, %d returned\n", nrow(train_feat), sum(train_feat$mask, na.rm = TRUE)))
cat(sprintf("Val:   %d rows, %d returned\n", nrow(val_feat),   sum(val_feat$mask,   na.rm = TRUE)))


#### Step 2 — Filter to RETURNED patients for training, set y ####
##
## Sayan's Gibbs requires y to be non-missing. Our `target` is NA for
## non-returned patients (since value_a1c_2025 is NA -> factor() returns NA).
## Strategy: train on returned only (mask == TRUE), evaluate on val returned only.
## Non-returned patients are trivially Controlled and predicted as 0 separately
## (handled by the eval harness via the mask, same as everywhere else).

train_returned <- train_feat %>%
    filter(mask) %>%
    mutate(y = as.numeric(target == "Uncontrolled"))

val_returned <- val_feat %>%
    filter(mask) %>%
    mutate(y = as.numeric(target == "Uncontrolled"))

cat(sprintf("\nTrain returned: %d (pos rate %.4f)\n",
            nrow(train_returned), mean(train_returned$y)))
cat(sprintf("Val   returned: %d (pos rate %.4f)\n",
            nrow(val_returned),   mean(val_returned$y)))


#### Step 3 — Standardize continuous features (means/sds from TRAIN only) ####

cont_vars <- c("a1c_last", "a1c_log_gap", "a1c_prop_gt8",
               "height", "ldl_log_gap", "hdl_log_gap", "insulin_log")
bin_vars  <- c("a1c_ever_gt8", "insulin_bin")

scale_info <- vector("list", length(cont_vars))
names(scale_info) <- cont_vars
for (v in cont_vars) {
    mu  <- mean(train_returned[[v]], na.rm = TRUE)
    sdv <- sd(train_returned[[v]], na.rm = TRUE)
    if (is.na(sdv) || sdv == 0) sdv <- 1
    scale_info[[v]] <- c(mean = mu, sd = sdv)
    train_returned[[v]] <- (train_returned[[v]] - mu) / sdv
    val_returned[[v]]   <- (val_returned[[v]]   - mu) / sdv
}

cat("\nScale info (train means / sds):\n")
print(do.call(rbind, scale_info))

cat("\nMissingness in train (returned):\n")
print(sapply(train_returned[c(cont_vars, bin_vars)], function(x) mean(is.na(x))))
cat("\nMissingness in val (returned):\n")
print(sapply(val_returned[c(cont_vars, bin_vars)], function(x) mean(is.na(x))))


#### Step 4 — Sayan's Gibbs sampler functions (verbatim, attributed) ####

## --- BEGIN code from scripts/Bayesian.R by Sayan ---

update_continuous_pg <- function(a, beta_j, omega_i, kappa_i, mu_j, sd_j) {
    if (is.na(sd_j) || sd_j <= 0) sd_j <- 1
    prec <- omega_i * beta_j^2 + 1 / (sd_j^2)
    var_new <- 1 / prec
    mean_new <- var_new * (beta_j * (kappa_i - omega_i * a) + mu_j / (sd_j^2))
    rnorm(1, mean = mean_new, sd = sqrt(var_new))
}

update_binary_pg <- function(a, beta_j, omega_i, kappa_i, pi_j) {
    pi_j <- min(max(pi_j, 1e-6), 1 - 1e-6)
    log_q1 <- log(pi_j) +
        (-(omega_i / 2) * (a + beta_j)^2 + kappa_i * (a + beta_j))
    log_q0 <- log(1 - pi_j) +
        (-(omega_i / 2) * a^2 + kappa_i * a)
    m <- max(log_q0, log_q1)
    q1 <- exp(log_q1 - m)
    q0 <- exp(log_q0 - m)
    p1 <- q1 / (q1 + q0)
    rbinom(1, 1, p1)
}

draw_beta_pg <- function(X, omega, kappa, b0, B0_inv) {
    XtOmega <- t(X) * as.numeric(omega)
    Q <- XtOmega %*% X + B0_inv
    Q <- (Q + t(Q)) / 2
    if (any(!is.finite(Q))) stop("Q contains NA/NaN/Inf")
    ridge_grid <- c(1e-8, 1e-6, 1e-4, 1e-2, 1e-1, 1)
    ok <- FALSE
    for (ridge in ridge_grid) {
        Q_try <- Q + diag(ridge, nrow(Q))
        R_try <- try(chol(Q_try), silent = TRUE)
        if (!inherits(R_try, "try-error")) {
            Q <- Q_try
            R <- R_try
            ok <- TRUE
            break
        }
    }
    if (!ok) {
        ev <- eigen(Q, symmetric = TRUE, only.values = TRUE)$values
        stop(sprintf("Cholesky failed. min eigen(Q)=%.6e, max eigen(Q)=%.6e",
                     min(ev), max(ev)))
    }
    rhs <- t(X) %*% kappa + B0_inv %*% b0
    m_beta <- solve(Q, rhs)
    z <- rnorm(ncol(X))
    beta <- as.vector(m_beta + backsolve(R, z))
    list(beta = beta, Q = Q)
}

pg_logistic_gibbs_missing <- function(dat, cont_vars, bin_vars,
                                      n_iter = 5000, burn = 1000,
                                      prior_var = 0.5, seed = 2026,
                                      verbose = TRUE) {
    set.seed(seed)
    dat <- as.data.frame(dat)
    dat$y <- as.numeric(dat$y)
    if (any(is.na(dat$y))) stop("y cannot be missing.")

    miss_cont <- lapply(cont_vars, function(v) which(is.na(dat[[v]])))
    names(miss_cont) <- cont_vars
    miss_bin <- lapply(bin_vars, function(v) which(is.na(dat[[v]])))
    names(miss_bin) <- bin_vars

    dat_current <- dat

    for (v in cont_vars) {
        idx <- miss_cont[[v]]
        if (length(idx) == 0) next
        obs <- dat_current[[v]][!is.na(dat_current[[v]])]
        fill_val <- if (length(obs) == 0) 0 else mean(obs)
        dat_current[[v]][idx] <- fill_val
    }
    for (v in bin_vars) {
        idx <- miss_bin[[v]]
        if (length(idx) == 0) next
        obs <- dat_current[[v]][!is.na(dat_current[[v]])]
        p_hat <- if (length(obs) == 0) 0.5 else mean(obs)
        p_hat <- min(max(p_hat, 1e-6), 1 - 1e-6)
        dat_current[[v]][idx] <- rbinom(length(idx), 1, p_hat)
    }

    cont_prior <- lapply(cont_vars, function(v) {
        x <- dat[[v]]
        mu <- mean(x, na.rm = TRUE)
        sdv <- sd(x, na.rm = TRUE)
        if (is.na(mu)) mu <- 0
        if (is.na(sdv) || sdv <= 0) sdv <- 1
        c(mu = mu, sd = sdv)
    })
    names(cont_prior) <- cont_vars
    bin_prior <- lapply(bin_vars, function(v) {
        x <- dat[[v]]
        p <- mean(x, na.rm = TRUE)
        if (is.na(p)) p <- 0.5
        min(max(p, 1e-6), 1 - 1e-6)
    })
    names(bin_prior) <- bin_vars

    X <- model.matrix(y ~ ., data = dat_current)
    p <- ncol(X); n <- nrow(X)
    b0 <- rep(0, p)
    B0 <- diag(prior_var, p)
    B0_inv <- solve(B0)
    beta <- rep(0, p)
    beta_save <- matrix(NA_real_, nrow = n_iter, ncol = p)
    colnames(beta_save) <- colnames(X)

    x_cols <- colnames(X)
    col_map <- list()
    for (v in c(cont_vars, bin_vars)) {
        col_map[[v]] <- if (v %in% x_cols) which(x_cols == v) else NA_integer_
    }

    for (iter in 1:n_iter) {
        X <- model.matrix(y ~ ., data = dat_current)
        kappa <- dat_current$y - 0.5
        psi <- as.vector(X %*% beta)
        omega <- BayesLogit::rpg(num = n, h = 1, z = psi)
        beta_out <- draw_beta_pg(X, omega, kappa, b0, B0_inv)
        beta <- beta_out$beta

        for (v in cont_vars) {
            idx_mis <- miss_cont[[v]]
            if (length(idx_mis) == 0) next
            col_j <- col_map[[v]]
            if (is.na(col_j)) next
            for (i in idx_mis) {
                X <- model.matrix(y ~ ., data = dat_current)
                a <- sum(X[i, -col_j] * beta[-col_j])
                mu_j <- cont_prior[[v]]["mu"]
                sd_j <- cont_prior[[v]]["sd"]
                dat_current[[v]][i] <- update_continuous_pg(
                    a = a, beta_j = beta[col_j],
                    omega_i = omega[i], kappa_i = kappa[i],
                    mu_j = mu_j, sd_j = sd_j
                )
            }
        }
        for (v in bin_vars) {
            idx_mis <- miss_bin[[v]]
            if (length(idx_mis) == 0) next
            col_j <- col_map[[v]]
            if (is.na(col_j)) next
            for (i in idx_mis) {
                X <- model.matrix(y ~ ., data = dat_current)
                a <- sum(X[i, -col_j] * beta[-col_j])
                dat_current[[v]][i] <- update_binary_pg(
                    a = a, beta_j = beta[col_j],
                    omega_i = omega[i], kappa_i = kappa[i],
                    pi_j = bin_prior[[v]]
                )
            }
        }

        beta_save[iter, ] <- beta
        if (verbose && iter %% 500 == 0) cat("Iteration:", iter, "\n")
    }

    keep <- (burn + 1):n_iter
    list(
        beta_draws = beta_save[keep, , drop = FALSE],
        beta_post_mean = colMeans(beta_save[keep, , drop = FALSE]),
        dat_completed = dat_current,
        cont_prior = cont_prior,
        bin_prior  = bin_prior
    )
}

## --- END code from scripts/Bayesian.R by Sayan ---


## --- BEGIN Subrata's fast variant ---
##
## Sayan's `pg_logistic_gibbs_missing` is O(N^2 * p * iter) at scale because
## the inner missing-imputation loops call `model.matrix(y ~ ., data = dat_current)`
## on EVERY missing cell, every iteration. With ~17k patients, ~14k missing
## height cells, and 5000 iterations, that's ~10 billion matrix builds. The
## function effectively never completes on this data.
##
## The fix is purely mechanical: keep `X` as a stateful matrix outside the
## inner loops, and when we impute `dat_current[[v]][i] <- new_val`, also do
## `X[i, col_j] <- new_val`. The matrix is in 1-1 correspondence with the
## data frame, so this stays consistent. The inner missing-cell update
## becomes O(1) instead of O(N*p), giving a ~17,000x speedup.
##
## Outer iteration still has one model.matrix call per iter (to refresh X
## before the omega/beta updates). That's O(N*p) per iter, total O(N*p*iter)
## = ~750M ops on this data, which is fine.
##
## Behaviorally identical to Sayan's function modulo the X update mechanics.

pg_logistic_gibbs_missing_fast <- function(dat, cont_vars, bin_vars,
                                           n_iter = 5000, burn = 1000,
                                           prior_var = 0.5, seed = 2026,
                                           verbose = TRUE) {
    set.seed(seed)
    dat <- as.data.frame(dat)
    dat$y <- as.numeric(dat$y)
    if (any(is.na(dat$y))) stop("y cannot be missing.")

    miss_cont <- lapply(cont_vars, function(v) which(is.na(dat[[v]])))
    names(miss_cont) <- cont_vars
    miss_bin <- lapply(bin_vars, function(v) which(is.na(dat[[v]])))
    names(miss_bin) <- bin_vars

    dat_current <- dat

    ## Mean-impute continuous, Bernoulli-impute binary as starting values
    for (v in cont_vars) {
        idx <- miss_cont[[v]]
        if (length(idx) == 0) next
        obs <- dat_current[[v]][!is.na(dat_current[[v]])]
        fill_val <- if (length(obs) == 0) 0 else mean(obs)
        dat_current[[v]][idx] <- fill_val
    }
    for (v in bin_vars) {
        idx <- miss_bin[[v]]
        if (length(idx) == 0) next
        obs <- dat_current[[v]][!is.na(dat_current[[v]])]
        p_hat <- if (length(obs) == 0) 0.5 else mean(obs)
        p_hat <- min(max(p_hat, 1e-6), 1 - 1e-6)
        dat_current[[v]][idx] <- rbinom(length(idx), 1, p_hat)
    }

    cont_prior <- lapply(cont_vars, function(v) {
        x <- dat[[v]]
        mu <- mean(x, na.rm = TRUE)
        sdv <- sd(x, na.rm = TRUE)
        if (is.na(mu)) mu <- 0
        if (is.na(sdv) || sdv <= 0) sdv <- 1
        c(mu = mu, sd = sdv)
    })
    names(cont_prior) <- cont_vars
    bin_prior <- lapply(bin_vars, function(v) {
        x <- dat[[v]]
        p <- mean(x, na.rm = TRUE)
        if (is.na(p)) p <- 0.5
        min(max(p, 1e-6), 1 - 1e-6)
    })
    names(bin_prior) <- bin_vars

    ## Build X ONCE up front. From here on we maintain it incrementally.
    X <- model.matrix(y ~ ., data = dat_current)
    p <- ncol(X); n <- nrow(X)
    b0 <- rep(0, p)
    B0 <- diag(prior_var, p)
    B0_inv <- solve(B0)
    beta <- rep(0, p)
    beta_save <- matrix(NA_real_, nrow = n_iter, ncol = p)
    colnames(beta_save) <- colnames(X)

    x_cols <- colnames(X)
    col_map <- list()
    for (v in c(cont_vars, bin_vars)) {
        col_map[[v]] <- if (v %in% x_cols) which(x_cols == v) else NA_integer_
    }

    kappa <- dat_current$y - 0.5  # constant across iterations since y doesn't change

    for (iter in 1:n_iter) {
        ## omega update — depends on current X and beta
        psi <- as.vector(X %*% beta)
        omega <- BayesLogit::rpg(num = n, h = 1, z = psi)

        ## beta update — depends on current X, omega, kappa
        beta_out <- draw_beta_pg(X, omega, kappa, b0, B0_inv)
        beta <- beta_out$beta

        ## Continuous missing-cell imputation. The cells of one variable are
        ## conditionally INDEPENDENT given X[, -col_j] and beta[col_j], so we
        ## can sample them all jointly via vectorized rnorm — exact Gibbs,
        ## not an approximation. ~1000x faster than the per-cell loop because
        ## we replace 20k R for-loop iterations with one BLAS call per variable.
        for (v in cont_vars) {
            idx_mis <- miss_cont[[v]]
            if (length(idx_mis) == 0) next
            col_j <- col_map[[v]]
            if (is.na(col_j)) next
            mu_j <- cont_prior[[v]]["mu"]
            sd_j <- cont_prior[[v]]["sd"]
            if (is.na(sd_j) || sd_j <= 0) sd_j <- 1
            beta_j <- beta[col_j]

            ## a_vec[i] = sum over k != col_j of X[i, k] * beta[k]
            a_vec <- as.vector(X[idx_mis, -col_j, drop = FALSE] %*% beta[-col_j])
            omega_v <- omega[idx_mis]
            kappa_v <- kappa[idx_mis]

            prec_v <- omega_v * beta_j^2 + 1 / sd_j^2
            var_v  <- 1 / prec_v
            mean_v <- var_v * (beta_j * (kappa_v - omega_v * a_vec) + mu_j / sd_j^2)
            new_vals <- rnorm(length(idx_mis), mean = mean_v, sd = sqrt(var_v))
            X[idx_mis, col_j] <- new_vals
        }

        ## Binary missing-cell imputation. Same vectorization.
        for (v in bin_vars) {
            idx_mis <- miss_bin[[v]]
            if (length(idx_mis) == 0) next
            col_j <- col_map[[v]]
            if (is.na(col_j)) next
            pi_v <- bin_prior[[v]]
            pi_v <- min(max(pi_v, 1e-6), 1 - 1e-6)
            beta_j <- beta[col_j]

            a_vec <- as.vector(X[idx_mis, -col_j, drop = FALSE] %*% beta[-col_j])
            omega_v <- omega[idx_mis]
            kappa_v <- kappa[idx_mis]

            log_q1 <- log(pi_v) +
                (-(omega_v / 2) * (a_vec + beta_j)^2 + kappa_v * (a_vec + beta_j))
            log_q0 <- log(1 - pi_v) +
                (-(omega_v / 2) * a_vec^2 + kappa_v * a_vec)
            m_vec <- pmax(log_q0, log_q1)
            q1 <- exp(log_q1 - m_vec)
            q0 <- exp(log_q0 - m_vec)
            p1_vec <- q1 / (q1 + q0)
            new_vals <- rbinom(length(idx_mis), 1, p1_vec)
            X[idx_mis, col_j] <- new_vals
        }

        beta_save[iter, ] <- beta
        if (verbose && iter %% 100 == 0) {
            cat(sprintf("Iteration: %d / %d\n", iter, n_iter))
        }
    }

    ## Reconstruct dat_completed from the final state of X (since we no longer
    ## update dat_current inside the loop). The intercept column of X is
    ## position 1; the rest match cont_vars + bin_vars in the order R built
    ## them via model.matrix.
    dat_completed <- dat_current
    for (v in c(cont_vars, bin_vars)) {
        cj <- col_map[[v]]
        if (!is.na(cj)) dat_completed[[v]] <- X[, cj]
    }

    keep <- (burn + 1):n_iter
    list(
        beta_draws = beta_save[keep, , drop = FALSE],
        beta_post_mean = colMeans(beta_save[keep, , drop = FALSE]),
        dat_completed = dat_completed,
        cont_prior = cont_prior,
        bin_prior  = bin_prior
    )
}

## --- END Subrata's fast variant ---


#### Step 5 — Fit the PG Gibbs on TRAIN ####
##
## Sayan ran 5000 iter / 1000 burnin. We use the same so the comparison is
## clean. With ~17k returned training patients × 9 features × 5000 iter,
## the inner Gibbs over missing covariates makes this slow — expect a few
## minutes if the missing-data set is small, longer if many height/lipid
## values are missing.

cat("\nFitting PG Gibbs on returned training patients...\n")
t0 <- Sys.time()

dat_for_fit <- train_returned %>%
    select(y, all_of(cont_vars), all_of(bin_vars))

fit_pg <- pg_logistic_gibbs_missing_fast(
    dat = dat_for_fit,
    cont_vars = cont_vars,
    bin_vars  = bin_vars,
    n_iter = 5000,
    burn   = 1000,
    prior_var = 0.5,
    seed = 2026,
    verbose = TRUE
)
cat(sprintf("PG Gibbs took %.1f minutes\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))


#### Step 6 — Posterior coefficient summary ####

beta_summary <- data.frame(
    variable  = colnames(fit_pg$beta_draws),
    post_mean = colMeans(fit_pg$beta_draws),
    post_sd   = apply(fit_pg$beta_draws, 2, sd),
    q025      = apply(fit_pg$beta_draws, 2, quantile, 0.025),
    q975      = apply(fit_pg$beta_draws, 2, quantile, 0.975)
)
cat("\n--- PG posterior coefficients ---\n")
print(beta_summary, row.names = FALSE)

cat("\nReference: Sayan's reported posterior means from his header comment:\n")
cat("  a1c_last 0.171, a1c_log_gap -0.012, a1c_ever_gt8 0.642, a1c_prop_gt8 1.362,\n")
cat("  height -0.005, ldl_log_gap -0.128, hdl_log_gap -0.040, insulin_bin 0.051, insulin_log 0.064\n")


#### Step 7 — Posterior predictive on val ####
##
## For each posterior sample of beta:
##   1. Take the val feature matrix as-is
##   2. Impute missing val covariates from the prior (cont: Normal(mu, sd^2),
##      bin: Bernoulli(p)). NOTE: this is the MARGINAL prior imputation, not
##      a full inner Gibbs over val missing covariates given the data — it
##      under-uses the information in the val patient's other observed features.
##      A full inner Gibbs would be more rigorous but requires running PG-style
##      updates per val patient per posterior sample, which is O(N_val * n_iter)
##      and impractical. Marginal prior imputation is the standard simplification
##      and is exactly what Sayan's `cont_prior` / `bin_prior` are designed for.
##   3. Compute eta = X*beta and prob = sigmoid(eta)
## Average across posterior samples to get the posterior predictive probability.

cat("\nComputing posterior predictive on val (returned patients)...\n")

predict_pg <- function(val_dat, beta_draws, cont_prior, bin_prior,
                       cont_vars, bin_vars, seed = 1) {
    set.seed(seed)

    ## Impute missing val covariates from the prior (one draw per missing cell).
    ## Standardization has already been applied to val_dat columns, so the
    ## prior mu/sd live in standardized space. cont_prior$mu/sd were computed
    ## inside the Gibbs from `dat` (which we passed standardized) so they
    ## should be ~0 and ~1 respectively.
    dat_imputed <- val_dat
    for (v in cont_vars) {
        idx <- which(is.na(dat_imputed[[v]]))
        if (length(idx) == 0) next
        mu_v  <- cont_prior[[v]]["mu"]
        sd_v  <- cont_prior[[v]]["sd"]
        dat_imputed[[v]][idx] <- rnorm(length(idx), mean = mu_v, sd = sd_v)
    }
    for (v in bin_vars) {
        idx <- which(is.na(dat_imputed[[v]]))
        if (length(idx) == 0) next
        dat_imputed[[v]][idx] <- rbinom(length(idx), 1, bin_prior[[v]])
    }

    ## Build the design matrix the same way the fit did
    dat_imputed$y <- 0  # placeholder for model.matrix
    X_val <- model.matrix(y ~ ., data = dat_imputed[, c("y", cont_vars, bin_vars)])

    ## Average sigmoid(X * beta) across posterior samples
    eta_mat <- X_val %*% t(beta_draws)
    prob_mat <- 1 / (1 + exp(-eta_mat))
    rowMeans(prob_mat)
}

val_pred_probs <- predict_pg(
    val_dat     = val_returned,
    beta_draws  = fit_pg$beta_draws,
    cont_prior  = fit_pg$cont_prior,
    bin_prior   = fit_pg$bin_prior,
    cont_vars   = cont_vars,
    bin_vars    = bin_vars,
    seed = 1
)

train_pred_probs <- predict_pg(
    val_dat     = train_returned,  # use the imputed `dat_completed` for train? No -
                                   # we want a posterior predictive, not in-sample.
                                   # Re-impute from the prior so train and val are
                                   # treated symmetrically.
    beta_draws  = fit_pg$beta_draws,
    cont_prior  = fit_pg$cont_prior,
    bin_prior   = fit_pg$bin_prior,
    cont_vars   = cont_vars,
    bin_vars    = bin_vars,
    seed = 2
)

cat(sprintf("Val pred range: [%.4f, %.4f] | mean %.4f\n",
            min(val_pred_probs), max(val_pred_probs), mean(val_pred_probs)))


#### Step 8 — Evaluate via the harness ####
##
## Pad the prediction back into a full-length vector that aligns with the
## original train/val tibbles, so the eval harness mask logic works.

train_full_probs <- rep(0, nrow(train_feat))
train_full_probs[train_feat$mask] <- train_pred_probs
train_full_target <- train_feat$target == "Uncontrolled"
train_full_target[!train_feat$mask] <- FALSE  # placeholder; mask filters out

val_full_probs <- rep(0, nrow(val_feat))
val_full_probs[val_feat$mask] <- val_pred_probs
val_full_target <- val_feat$target == "Uncontrolled"
val_full_target[!val_feat$mask] <- FALSE

eval_res <- eval_predictions(
    probs        = val_full_probs,
    target       = val_full_target,
    mask         = val_feat$mask,
    train_probs  = train_full_probs,
    train_target = train_full_target,
    train_mask   = train_feat$mask
)

print_eval(eval_res, label = "Sayan-style PG logistic + missing-data Gibbs (adapted)")


#### Step 9 — Reference numbers ####

cat("\n========== Reference numbers (val split, returned only) ==========\n")
cat("XGBoost regularized:                       0.6200\n")
cat("a1c_weighted alone (M1 from Plan B):       0.6140\n")
cat("a1c_weighted * ctmc_prob (M4 from Plan B): 0.6131\n")
cat("RF (audited, 5-seed mean):                 0.6162\n")
cat(sprintf("PG logistic (this run, train-tuned th):    %.4f\n", eval_res$f1_train_tuned))

cat("\nReminder of the SPECULATIONS this run does NOT settle:\n")
cat("  - The PG Gibbs is cross-sectional, not longitudinal.\n")
cat("  - Expected outcome: F1 ~ 61% (matches the ceiling),\n")
cat("    log-loss / Brier / ECE substantially better than naive a1c_weighted GLM.\n")
cat("  - The win is the missing-data story (height, lipids properly imputed),\n")
cat("    not an F1 improvement. The eval harness will tell us how well calibrated.\n")

cat("\n#### DONE ####\n")
