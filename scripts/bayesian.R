library(tidyverse)

source("./scripts/utils.R")

# read train + validation
dat <- merge_dataset(
    readRDS("./data/processed/train.Rds"),
    readRDS("./data/processed/val.Rds")
)


long_data <- dat$measurements %>%
    filter(variable %in% c(paste0("a1c_", 1:5), "a1c_2025")) %>% # Extract only A1C measurements
    drop_na() %>% # Remove missing time/value pairs
    arrange(id, time) %>% # Sort for the Markov process
    group_by(id) %>%
    mutate(obs_number = row_number(), n_obs = n(), is_last = as.numeric(obs_number == n_obs)) %>%
    ungroup() %>%
    filter(n_obs > 1) %>%
    mutate(stan_id = as.numeric(factor(id)))

patient_data <- dat$basic %>%
    left_join(dat$counts, by = "id") %>%
    filter(id %in% unique(long_data$id)) %>% # Filter only to patients that exist in our cleaned longitudinal data
    arrange(id) # Ensure order matches the long_data ID indexing


# Create the numeric design matrix for Stan. We remove the intercept (- 1) as that can be addded in stan implementation
covariate_formula <- ~ cad + copd + metformin - 1
M_matrix <- model.matrix(covariate_formula, data = patient_data)
length(unique(long_data$id))
dim(M_matrix)


# -----------
# Testing with JAGS
# -----------

library(runjags)

A_star <- 7.0
prepared_data <- dat$measurements %>%
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
    filter(id %in% unique(id)[1:1000]) %>% # sample 50 patients
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

prepared_data
first_idx <- which(prepared_data$is_first == 1)
subseq_idx <- which(prepared_data$is_first == 0)

# prepare patient data
patient_data <- dat$basic %>%
    left_join(dat$counts, by = "id") %>%
    filter(id %in% unique(prepared_data$id)) %>% # Filter only to patients that exist in our cleaned longitudinal data
    arrange(id) # Ensure order matches the long_data ID indexing

# Create the numeric design matrix for Stan. We remove the intercept (- 1) as that can be addded in stan implementation
covariate_formula <- ~ age + cad + copd + metformin - 1
M_matrix <- model.matrix(covariate_formula, data = patient_data)
dim(M_matrix)


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


# for each mcmc samples, run for test data
test_dat <- readRDS("./data/processed/test.Rds")


# simulate the test scenarios
test_prepared <- test_dat$measurements %>%
    filter(variable %in% paste0("a1c_", 1:5)) %>%
    drop_na() %>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(obs_number = row_number(), n_obs = n()) %>%
    ungroup() %>%
    filter(obs_number == n_obs) %>% # picks the last observation for each patient
    left_join(
        test_dat$measurements %>%
            filter(variable == "a1c_2025") %>%
            drop_na() %>%
            select(id, a1c_2025_time = time, a1c_2025 = value),
        by = "id"
    ) %>%
    mutate(dt = a1c_2025_time - time)

test_prepared


test_patients <- test_dat$basic %>%
    left_join(test_dat$counts, by = "id") %>%
    filter(id %in% unique(test_prepared$id)) %>%
    arrange(id)
test_X <- model.matrix(covariate_formula, data = test_patients)

# simulate the test scenario for each posterior sample
{
    p <- ncol(test_X)
    exceedence_counts <- numeric(nrow(test_patients)) # is uncontrolled
    pb <- txtProgressBar(min = 1, max = nrow(mcmc_samples), style = 3) # progress bar
    for (b in 1:nrow(mcmc_samples)) {
        param <- mcmc_samples[b, ]
        alpha_delta <- param[1]
        alpha_prec <- param[2]
        beta_delta <- param[3:(p + 2)]
        beta_prec <- param[(p + 3):(2 * p + 2)]
        prec_Y <- param[2 * p + 3]

        # calculate the medication and precision parameter
        delta_i <- exp(alpha_delta + (test_X %*% beta_delta)) # (n, )
        prec_i <- exp(alpha_prec + (test_X %*% beta_prec)) # (n, )

        # simulate a1c_2025 observation
        uncontrolled <- ifelse(
            is.na(test_prepared$a1c_2025_time),
            0, # no exceedance if not measured
            ifelse(
                test_prepared$value > A_star,
                pnorm(A_star, mean = test_prepared$value - delta_i, sd = sqrt(test_prepared$time / prec_i)), # P(N(value_i - delta_i, time_i/prec_i) > A_star)
                pnorm(A_star, mean = test_prepared$value, sd = sqrt(test_prepared$time / prec_i)) # P(N(value_i, time_i/prec_i) > A_star)
            )
        )
        exceedence_counts <- exceedence_counts + uncontrolled

        setTxtProgressBar(pb, b)
    }
    close(pb)
}
probs <- exceedence_counts / nrow(mcmc_samples)
hist(probs)

# let's look at metrics
table(
    probs > 0.999,
    test_prepared$a1c_2025
)
