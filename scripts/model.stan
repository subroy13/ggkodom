data {
  int<lower=1> N_total;       // Total number of valid A1C measurements
  int<lower=1> N_patients;    // Number of unique patients
  array[N_total] int id;      // Mapped Patient ID (1 to N_patients)
  vector[N_total] Y;          // Observed A1C values
  vector[N_total] t;          // Time of measurement
  
  // Covariate Data
  int<lower=1> K;             // Number of covariates (columns in design matrix)
  matrix[N_patients, K] M;    // Patient-level design matrix
  
  real A_star;                // A1C Medication Threshold (e.g., 7.0 or 8.0)

  // New Censoring Indicators
  array[N_total] int<lower=0, upper=1> is_last;    // 1 if terminal observation
  array[N_total] int<lower=0, upper=1> is_above_A; // 1 if >= A*, 0 if < A*
}

parameters {
  vector[N_total] mu;         // Latent true A1C state
  real<lower=0> tau;          // Global measurement error
  
  // Base intercepts for the hierarchical parameters
  real alpha_delta;
  real alpha_sigma;
  
  // Coefficients for the covariates
  vector[K] theta_delta;      
  vector[K] theta_sigma;      
}

transformed parameters {
  // Compute patient-specific parameters via log-link matrix algebra
  // delta = exp(alpha + M * theta)
  vector<lower=0>[N_patients] delta = exp(alpha_delta + M * theta_delta);
  vector<lower=0>[N_patients] sigma_sq = exp(alpha_sigma + M * theta_sigma);
}

model {
  // Priors
  tau ~ exponential(1);
  alpha_delta ~ normal(0, 2);
  alpha_sigma ~ normal(0, 2);
  theta_delta ~ normal(0, 1); // Regularizing priors for high-dimensional M
  theta_sigma ~ normal(0, 1);
  
  // Measurement Error Layer (Handling Interval Censoring)
  for (k in 1:N_total) {
    if (is_last[k] == 0) {
      // Exact observation
      Y[k] ~ normal(mu[k], tau);
    } else {
      // Censored observation (Terminal node only)
      if (is_above_A[k] == 1) {
        target += normal_lccdf(A_star | mu[k], tau); // log complementary CDF, i.e, Log-probability that Y >= A*
      } else {
        target += normal_lcdf(A_star | mu[k], tau); // Log-probability that Y < A*
      }
    }
  }
  
  // Latent Markov Process Layer
  for (k in 1:N_total) {
    if (k == 1 || id[k] != id[k-1]) {
      // FIRST observation for a patient
      // We place a weak prior on the initial state, anchoring it near the observation
      mu[k] ~ normal(Y[k], 2.0); 
    } else {
      // SUBSEQUENT observations (Transition dynamics)
      real dt = t[k] - t[k-1];
      real current_sigma = sqrt(sigma_sq[id[k]] * dt);
      
      // Threshold Logic: If previous observed A1C was over threshold A*
      if (Y[k-1] >= A_star) {
         // Post-medication: applies negative drift proportional to time gap
         real expected_mu = mu[k-1] - delta[id[k]] * dt;
         mu[k] ~ normal(expected_mu, current_sigma);
      } else {
         // Pre-medication: random walk with 0 drift
         mu[k] ~ normal(mu[k-1], current_sigma);
      }
    }
  }
}
