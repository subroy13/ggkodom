library(tidyverse)
library(runjags)

source("./scripts/utils.R")

# read train + validation
train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")

# -----
# Prepare data format for running Bayesian model

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
            med_flag = if_else(is_first == 0 & lag(value) > A_star, 1, 0),
            is_last = as.numeric(obs_number == n_obs),
            Y_jags = if_else(is_last == 1, NA_real_, value), # continuous a1c value
            is_above_A = case_when(
                is_last == 1 & value > A_star ~ 1,
                is_last == 1 & value <= A_star ~ 0,
                TRUE ~ NA_real_
            )
        )

    first_idx <- which(prepared_data$is_first == 1)
    subseq_idx <- which(prepared_data$is_first == 0)

    # prepare patient data
    patient_data <- train_dat$basic %>%
        left_join(train_dat$counts, by = "id") %>%
        filter(id %in% unique(prepared_data$id)) %>% # Filter only to patients that exist in our cleaned longitudinal data
        arrange(id) # Ensure order matches the long_data ID indexing

    # Create the numeric design matrix for Stan. We remove the intercept (- 1) as that can be addded in stan implementation
    M_matrix <- model.matrix(cov_formula, data = patient_data)

    jags_data <- list(
        N_patients = length(unique(prepared_data$seq_id)),
        N_total = nrow(prepared_data),
        N_subseq = length(subseq_idx),
        Y = prepared_data$Y_jags,
        is_above_A = prepared_data$is_above_A,
        A_star = A_star,
        dt = prepared_data$dt,
        id = prepared_data$seq_id,
        first_idx = first_idx,
        subseq_idx = subseq_idx,
        med_flag = prepared_data$med_flag,
        X = M_matrix,
        p = ncol(M_matrix)
    )
    jags_data
}


covariate_formula <- ~ age + cad + copd + metformin - 1

train_sample_dat <- sample_dat(train_dat, sample_size = 1000)
jags_data <- compute_bayesian_inputs(train_sample_dat, covariate_formula, A_star = 8.0)
jags_data$N_patients

fit <- run.jags(
    model = "./scripts/model.jags",
    monitor = c("alpha_delta", "alpha_prec", "beta_delta", "beta_prec", "prec_Y"),
    data = jags_data,
    n.chains = 4,
    adapt = 2000, # Tuning phase
    burnin = 2000, # Warmup phase
    sample = 2000, # Post-warmup draws per chain
    method = "parallel" # Distributes chains across CPU cores
)

summary(fit)

# Let's look at the mcmc samples
mcmc_samples <- as.matrix(fit$mcmc)
dim(mcmc_samples)

# show sampling
# simulate the test scenario for each posterior sample
predict_jags <- function(mcmc_samples, jags_sim, sim_dat) {
    sim_prepared <- sim_dat$measurements %>%
        filter(variable %in% paste0("a1c_", 1:5)) %>%
        drop_na() %>%
        arrange(id, time) %>%
        group_by(id) %>%
        slice_tail() %>% # picks the last value
        ungroup() %>%
        left_join(
            test_dat$measurements %>%
                filter(variable == "a1c_2025") %>%
                drop_na() %>%
                select(id, a1c_2025_time = time, a1c_2025 = value),
            by = "id"
        ) %>%
        mutate(dt = a1c_2025_time - time)

    sim_patients <- sim_dat$basic %>%
        left_join(sim_dat$counts, by = "id") %>%
        filter(id %in% unique(sim_prepared$id)) %>%
        arrange(id)
    sim_X <- model.matrix(covariate_formula, data = sim_patients)

    p <- jags_sim$p
    A_star <- jags_sim$A_star
    exceedence_counts <- numeric(nrow(sim_prepared)) # is uncontrolled

    pb <- txtProgressBar(min = 1, max = nrow(mcmc_samples), style = 3) # progress bar
    for (b in seq_len(nrow(mcmc_samples))) {
        param <- mcmc_samples[b, ]
        alpha_delta <- param[1]
        alpha_prec <- param[2]
        beta_delta <- param[3:(p + 2)]
        beta_prec <- param[(p + 3):(2 * p + 2)]
        prec_Y <- param[2 * p + 3]

        # calculate the medication and precision parameter
        delta_i <- exp(alpha_delta + (sim_X %*% beta_delta)) # (n, )
        prec_i <- exp(alpha_prec + (sim_X %*% beta_prec)) # (n, )

        # simulate a1c_2025 observation
        uncontrolled <- ifelse(
            is.na(sim_prepared$a1c_2025_time),
            0, # no exceedance if not measured
            ifelse(
                sim_prepared$value > A_star,
                pnorm(A_star, mean = sim_prepared$value - delta_i, sd = sqrt(sim_prepared$time / prec_i)), # P(N(value_i - delta_i, time_i/prec_i) > A_star)
                pnorm(A_star, mean = sim_prepared$value, sd = sqrt(sim_prepared$time / prec_i)) # P(N(value_i, time_i/prec_i) > A_star)
            )
        )
        exceedence_counts <- exceedence_counts + uncontrolled

        setTxtProgressBar(pb, b)
    }
    close(pb)

    return(exceedence_counts / nrow(mcmc_samples))
}



# now sample from posterior and predict for validation data
jags_sim <- compute_bayesian_inputs(val_dat, covariate_formula, A_star = 8.0)
pred_probs <- predict_jags(mcmc_samples, jags_sim, val_dat)

target_labels <- (val_dat$measurements %>% filter(variable == "a1c_2025"))$value
target_mask <- (val_dat$measurements %>% filter(variable == "a1c_2025"))$time
best_th <- best_f1_threshold(pred_probs, target_labels, target_mask)
best_th
