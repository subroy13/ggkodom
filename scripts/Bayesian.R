# a1c_last, a1c_log_gap, a1c_ever_gt8, a1c_prop_gt8
# height, ldl_log_gap, hdl_log_gap, insulin_bin, insulin_log
# [1]  0.171440065 -0.011815450  0.642013353  1.361892145 -0.005114988
# [6] -0.128110518 -0.039642556  0.050937338  0.064273997




library(dplyr)
library(mice)
library(glmnet)
{
  #============================================================
  # 1. Make a working copy
  #============================================================
  ### Change the file location accordingly
  Up_data <- read.csv("~/Library/CloudStorage/Box-Box/WashU Datathon 2026/KD/Fulldata/Fulldata.csv")
  
  ### Bayesian Imputation (Long rung-time)
  #read.csv("~/Library/CloudStorage/Box-Box/WashU Datathon 2026/KD/Up_data.csv")
  
  ### Imputed by MICE
  #read.csv("~/Library/CloudStorage/Box-Box/WashU Datathon 2026/KD/Fulldata/Fulldata.csv")
  
  dat_imp <- Up_data
  
  # Outcome should not be imputed
  dat_imp$y <- as.integer(dat_imp$y)
  
  #------------------------------------------------------------
  # Optional: make sure binary numeric variables are factors
  # Only do this for truly binary predictors you want imputed as binary.
  # Adjust this list to match your actual columns.
  #------------------------------------------------------------
  binary_vars <- intersect(
    c("gender", "ethnicity",
      "a1c_ever_gt8",
      grep("_bin$", names(dat_imp), value = TRUE)),
    names(dat_imp)
  )
  
  binary_vars = binary_vars[-which(binary_vars == "adi_nation_bin")]
  
  for (v in binary_vars) {
    # keep y numeric; convert predictors only
    if (v != "y") {
      dat_imp[[v]] <- factor(dat_imp[[v]], levels = c(0, 1))
    }
  }
  
  #------------------------------------------------------------
  # Optional: ensure categorical variables are factors
  #------------------------------------------------------------
  factor_vars <- intersect(c("race", "adi_state"), names(dat_imp))
  for (v in factor_vars) {
    dat_imp[[v]] <- factor(dat_imp[[v]])
  }
}



library(BayesLogit)

library(dplyr)

library(dplyr)

#============================================================
# 1. Build the reduced dataframe
#============================================================
dat_small <- dat_imp %>%
  transmute(
    y,
    a1c_last,
    a1c_log_gap,
    a1c_ever_gt8,
    a1c_prop_gt8,
    height,
    ldl_log_gap,
    hdl_log_gap,
    insulin_bin,
    insulin_log
  ) %>%
  mutate(
    y = as.numeric(y),
    a1c_ever_gt8 = as.numeric(a1c_ever_gt8),
    insulin_bin = as.numeric(insulin_bin)
  )

#============================================================
# 2. Standardize continuous variables
#    (keep means/sds for reference)
#============================================================
cont_vars <- c(
  "a1c_last",
  "a1c_log_gap",
  "a1c_prop_gt8",
  "height",
  "ldl_log_gap",
  "hdl_log_gap",
  "insulin_log"
)

bin_vars <- c(
  "a1c_ever_gt8",
  "insulin_bin"
)

scale_info <- vector("list", length(cont_vars))
names(scale_info) <- cont_vars

for (v in cont_vars) {
  mu <- mean(dat_small[[v]], na.rm = TRUE)
  sdv <- sd(dat_small[[v]], na.rm = TRUE)
  if (is.na(sdv) || sdv == 0) sdv <- 1
  scale_info[[v]] <- c(mean = mu, sd = sdv)
  dat_small[[v]] <- (dat_small[[v]] - mu) / sdv
}

#============================================================
# 3. Helper functions
#============================================================

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
  
  if (any(!is.finite(Q))) {
    stop("Q contains NA/NaN/Inf")
  }
  
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
    stop(sprintf(
      "Cholesky failed. min eigen(Q)=%.6e, max eigen(Q)=%.6e",
      min(ev), max(ev)
    ))
  }
  
  rhs <- t(X) %*% kappa + B0_inv %*% b0
  m_beta <- solve(Q, rhs)
  
  z <- rnorm(ncol(X))
  beta <- as.vector(m_beta + backsolve(R, z))
  
  list(beta = beta, Q = Q)
}

#============================================================
# 4. Bayesian PG logistic with missing covariates
#============================================================
pg_logistic_gibbs_missing <- function(dat,
                                      cont_vars,
                                      bin_vars,
                                      n_iter = 5000,
                                      burn = 1000,
                                      prior_var = 0.5,
                                      seed = 2026,
                                      verbose = TRUE) {
  set.seed(seed)
  
  dat <- as.data.frame(dat)
  dat$y <- as.numeric(dat$y)
  
  if (any(is.na(dat$y))) stop("y cannot be missing.")
  
  # save missingness pattern from original data
  miss_cont <- lapply(cont_vars, function(v) which(is.na(dat[[v]])))
  names(miss_cont) <- cont_vars
  
  miss_bin <- lapply(bin_vars, function(v) which(is.na(dat[[v]])))
  names(miss_bin) <- bin_vars
  
  # initialize missing values
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
  
  # working priors for missing predictors
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
  
  # fixed design matrix structure
  X <- model.matrix(y ~ ., data = dat_current)
  p <- ncol(X)
  n <- nrow(X)
  
  b0 <- rep(0, p)
  B0 <- diag(prior_var, p)
  B0_inv <- solve(B0)
  
  beta <- rep(0, p)
  beta_save <- matrix(NA_real_, nrow = n_iter, ncol = p)
  colnames(beta_save) <- colnames(X)
  
  # map variable names to design-matrix columns
  x_cols <- colnames(X)
  
  col_map <- list()
  for (v in c(cont_vars, bin_vars)) {
    if (v %in% x_cols) {
      col_map[[v]] <- which(x_cols == v)
    } else {
      col_map[[v]] <- NA_integer_
    }
  }
  
  for (iter in 1:n_iter) {
    #print(iter)
    X <- model.matrix(y ~ ., data = dat_current)
    kappa <- dat_current$y - 0.5
    
    #----------------------------
    # Step 1: sample omega
    #----------------------------
    psi <- as.vector(X %*% beta)
    omega <- BayesLogit::rpg(num = n, h = 1, z = psi)
    
    #----------------------------
    # Step 2: sample beta
    #----------------------------
    beta_out <- draw_beta_pg(X, omega, kappa, b0, B0_inv)
    beta <- beta_out$beta
    
    #----------------------------
    # Step 3: sample missing continuous covariates
    #----------------------------
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
          a = a,
          beta_j = beta[col_j],
          omega_i = omega[i],
          kappa_i = kappa[i],
          mu_j = mu_j,
          sd_j = sd_j
        )
      }
    }
    
    #----------------------------
    # Step 4: sample missing binary covariates
    #----------------------------
    for (v in bin_vars) {
      idx_mis <- miss_bin[[v]]
      if (length(idx_mis) == 0) next
      
      col_j <- col_map[[v]]
      if (is.na(col_j)) next
      
      for (i in idx_mis) {
        X <- model.matrix(y ~ ., data = dat_current)
        a <- sum(X[i, -col_j] * beta[-col_j])
        
        dat_current[[v]][i] <- update_binary_pg(
          a = a,
          beta_j = beta[col_j],
          omega_i = omega[i],
          kappa_i = kappa[i],
          pi_j = bin_prior[[v]]
        )
      }
    }
    
    beta_save[iter, ] <- beta
    
    if (verbose && iter %% 500 == 0) {
      cat("Iteration:", iter, "\n")
    }
  }
  
  keep <- (burn + 1):n_iter
  
  list(
    beta_draws = beta_save[keep, , drop = FALSE],
    beta_post_mean = colMeans(beta_save[keep, , drop = FALSE]),
    dat_completed = dat_current,
    scale_info = scale_info
  )
}

fit_pg_mis <- pg_logistic_gibbs_missing(
  dat = dat_small,
  cont_vars = cont_vars,
  bin_vars = bin_vars,
  n_iter = 5000,
  burn = 1000,
  prior_var = 0.5,
  verbose = TRUE
)

beta_summary <- data.frame(
  variable = colnames(fit_pg_mis$beta_draws),
  post_mean = colMeans(fit_pg_mis$beta_draws),
  post_sd = apply(fit_pg_mis$beta_draws, 2, sd),
  q025 = apply(fit_pg_mis$beta_draws, 2, quantile, 0.025),
  q975 = apply(fit_pg_mis$beta_draws, 2, quantile, 0.975)
)

beta_summary

X_final <- model.matrix(y ~ ., data = fit_pg_mis$dat_completed)
eta_mat <- X_final %*% t(fit_pg_mis$beta_draws)
prob_mat <- 1 / (1 + exp(-eta_mat))
post_prob <- rowMeans(prob_mat)

head(post_prob)

p3_thresh = best_f1_threshold(post_prob, dat_small$y)

# Prob <- read.csv("~/Desktop/Work/Datathon/Prob.csv")
# probs = cbind(Prob, NGLM)
# colnames(probs) = c("y","BIMP", "BMICE")
# write.csv(probs, "Prob.csv", row.names = F)
# 
# mdl = glm(as.factor(Up_data$y) ~ Up_data$a1c_last, family = binomial(link = "logit"))
# NGLM = predict(mdl, NULL, "response")



