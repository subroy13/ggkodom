library(tidyverse)
library(cmdstanr)

source("./scripts/utils.R")

# read train + validation
dat <- merge_dataset(
    readRDS("./data/processed/train.Rds"),
    readRDS("./data/processed/val.Rds")
)

long_data <- dat$measurements %>%
    filter(variable %in% paste0("a1c_", 1:5)) %>% # Extract only A1C measurements
    drop_na() %>% # Remove missing time/value pairs
    arrange(id, time) %>% # Sort for the Markov process
    group_by(id) %>%
    mutate(obs_number = row_number(), n_obs = n()) %>%
    ungroup() %>%
    filter(n_obs > 1) %>% # Optional: drop patients with no transitions # TODO: handle later
    mutate(stan_id = as.numeric(factor(id))) # creates sequences of patient indices
long_data

patient_data <- dat$basic %>%
    left_join(dat$counts, by = "id") %>%
    filter(id %in% unique(long_data$id)) %>% # Filter only to patients that exist in our cleaned longitudinal data
    arrange(id) # Ensure order matches the long_data ID indexing

# Create the numeric design matrix for Stan. We remove the intercept (- 1) as that can be addded in stan implementation
covariate_formula <- ~ age + cad + copd + metformin - 1
M_matrix <- model.matrix(covariate_formula, data = patient_data)
dim(patient_data)
dim(M_matrix)

# Compile STAN model
mod <- cmdstan_model("./scripts/model.stan")

stan_data <- list(
    N_total = nrow(long_data),
    N_patients = length(unique(long_data$stan_id)),
    id = long_data$stan_id,
    Y = long_data$value,
    t = long_data$time,
    K = ncol(M_matrix),
    M = M_matrix,
    A_star = 7.0 # Clinical threshold for A1C
)

# Execute HMC
fit <- mod$sample(
    data = stan_data,
    seed = 42,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    adapt_delta = 0.95 # Increased to help the sampler navigate the volatility bounds
)

summary(fit)
