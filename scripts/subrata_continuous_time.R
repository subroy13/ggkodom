#### CONTINUOUS-TIME A1c DYNAMICS ####
# Explore: variogram → estimate τ → CTMC feature → time-weighted EWMA → GP prediction
# See ideas.md (Apr 6 entry) for the theory.

library(tidyverse)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")

cat("============================================\n")
cat("  CONTINUOUS-TIME A1c DYNAMICS\n")
cat("============================================\n\n")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")


#### PART 1: Empirical variogram → estimate τ ####
cat("============ PART 1: VARIOGRAM & MEMORY TIMESCALE ============\n\n")

# Get all within-patient A1c pairs
a1c_long <- bind_rows(
    train_dat$measurements, val_dat$measurements
) %>%
    filter(variable %in% paste0("a1c_", 1:5), !is.na(value)) %>%
    arrange(id, time) %>%
    mutate(time_yr = time / 365.25)

# Create all within-patient pairs (not just consecutive)
pairs <- a1c_long %>%
    inner_join(a1c_long, by = "id", suffix = c("_1", "_2"), relationship = "many-to-many") %>%
    filter(time_1 < time_2) %>%
    mutate(
        dt_yr = time_yr_2 - time_yr_1,
        dy    = value_2 - value_1,
        dy2   = dy^2
    )

cat(sprintf("Total within-patient pairs: %d\n", nrow(pairs)))
cat(sprintf("Time gap range: %.3f to %.3f years\n", min(pairs$dt_yr), max(pairs$dt_yr)))

# Bin by time gap and compute variogram: γ(Δt) = E[(Y(t+Δt) - Y(t))²] / 2
bins <- c(0, 0.02, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1.0, 1.5)
pairs$dt_bin <- cut(pairs$dt_yr, breaks = bins, include.lowest = TRUE)

variogram <- pairs %>%
    filter(!is.na(dt_bin)) %>%
    group_by(dt_bin) %>%
    summarize(
        n = n(),
        dt_mid = median(dt_yr),
        gamma  = mean(dy2) / 2,      # semivariance
        cor_raw = cor(value_1, value_2),  # raw correlation (includes between-patient var)
        .groups = "drop"
    )

cat("\nEmpirical variogram:\n")
print(variogram, width = 120)

# Fit OU variogram: γ(Δt) = σ² (1 - exp(-Δt/τ))
# σ² = sill (asymptotic semivariance)
# Use NLS to estimate τ and σ²
cat("\n--- Fitting OU variogram: γ(Δt) = σ² (1 - exp(-Δt/τ)) ---\n")
fit_vario <- tryCatch({
    nls(gamma ~ sigma2 * (1 - exp(-dt_mid / tau)),
        data = variogram,
        start = list(sigma2 = max(variogram$gamma), tau = 0.3),
        lower = list(sigma2 = 0.1, tau = 0.01),
        algorithm = "port")
}, error = function(e) {
    cat(sprintf("NLS failed: %s\n", e$message))
    NULL
})

if (!is.null(fit_vario)) {
    tau_hat <- coef(fit_vario)["tau"]
    sigma2_hat <- coef(fit_vario)["sigma2"]
    cat(sprintf("\n*** Estimated memory timescale τ = %.3f years (%.0f days) ***\n", tau_hat, tau_hat * 365.25))
    cat(sprintf("*** Estimated sill σ² = %.3f ***\n", sigma2_hat))
    cat(sprintf("*** Half-life of autocorrelation: %.3f years (%.0f days) ***\n", tau_hat * log(2), tau_hat * log(2) * 365.25))

    # Predicted variogram
    dt_seq <- seq(0.01, 1.0, 0.01)
    pred_gamma <- sigma2_hat * (1 - exp(-dt_seq / tau_hat))

    cat("\nVariogram fit vs observed:\n")
    for (i in seq_len(nrow(variogram))) {
        pred <- sigma2_hat * (1 - exp(-variogram$dt_mid[i] / tau_hat))
        cat(sprintf("  Δt=%.3f yr: observed γ=%.3f, predicted γ=%.3f\n",
            variogram$dt_mid[i], variogram$gamma[i], pred))
    }
} else {
    # Fallback: estimate τ from lag-1 correlation
    cat("Falling back to AR(1) estimate of τ...\n")
    # Use consecutive pairs only
    consec <- a1c_long %>%
        group_by(id) %>%
        mutate(next_val = lead(value), next_time = lead(time_yr), dt = lead(time_yr) - time_yr) %>%
        filter(!is.na(next_val))
    rho1 <- cor(consec$value, consec$next_val)
    dt_median <- median(consec$dt)
    tau_hat <- -dt_median / log(rho1)
    sigma2_hat <- var(a1c_long$value) * (1 - rho1^2)  # residual from AR(1)
    cat(sprintf("AR(1) ρ = %.3f at median Δt = %.3f yr → τ = %.3f yr\n", rho1, dt_median, tau_hat))
}


#### PART 2: CTMC transition probabilities ####
cat("\n\n============ PART 2: CTMC TRANSITION PROBABILITIES ============\n\n")

# Estimate transition rates from observed state changes
# For each consecutive pair: (state_start, state_end, Δt)
transitions <- a1c_long %>%
    group_by(id) %>%
    mutate(
        state = if_else(value >= 8, "H", "L"),
        next_state = lead(state),
        dt = lead(time_yr) - time_yr
    ) %>%
    filter(!is.na(next_state), dt > 0) %>%
    ungroup()

cat(sprintf("Total observed transitions: %d\n", nrow(transitions)))

# Transition counts
trans_table <- table(transitions$state, transitions$next_state)
cat("\nTransition counts:\n"); print(trans_table)

# Estimate rates using MLE for CTMC
# For 2-state CTMC: q_ij = n_ij / (total_time_in_state_i)
time_in_L <- sum(transitions$dt[transitions$state == "L"])
time_in_H <- sum(transitions$dt[transitions$state == "H"])
n_LH <- sum(transitions$state == "L" & transitions$next_state == "H")
n_HL <- sum(transitions$state == "H" & transitions$next_state == "L")

q_LH <- n_LH / time_in_L
q_HL <- n_HL / time_in_H
lambda <- q_LH + q_HL
pi_H <- q_LH / lambda  # stationary probability of H

cat(sprintf("\nCTMC rate estimates:\n"))
cat(sprintf("  q_LH = %.4f /yr (rate of deterioration)\n", q_LH))
cat(sprintf("  q_HL = %.4f /yr (rate of treatment success)\n", q_HL))
cat(sprintf("  λ = q_LH + q_HL = %.4f /yr (total rate)\n", lambda))
cat(sprintf("  Stationary π_H = %.3f (long-run fraction uncontrolled)\n", pi_H))
cat(sprintf("  CTMC relaxation time 1/λ = %.3f years\n", 1/lambda))

# P(H | start_state, Δt) for various time gaps
cat("\nCTMC predictions P(H at t+Δt | state at t):\n")
cat(sprintf("  %-8s  P(H|L)   P(H|H)\n", "Δt"))
for (dt in c(0.01, 0.1, 0.25, 0.5, 1.0, 2.0, 5.0, 10.0)) {
    p_H_given_L <- pi_H * (1 - exp(-lambda * dt))
    p_H_given_H <- pi_H + (1 - pi_H) * exp(-lambda * dt)
    cat(sprintf("  %-8.2f  %.4f   %.4f\n", dt, p_H_given_L, p_H_given_H))
}

# Compute CTMC feature for each patient
# Need: last A1c state (H/L) and time_gap to 2025 eval
df_train <- create_features(train_dat)
df_val   <- create_features(val_dat)

add_ctmc_feature <- function(df) {
    df %>% mutate(
        last_state_H = as.integer(a1c_latest >= 8),
        time_gap_yr  = time_gap / 365.25,
        # P(H at 2025 | last state, time gap)
        ctmc_prob = if_else(
            last_state_H == 1,
            pi_H + (1 - pi_H) * exp(-lambda * pmax(time_gap_yr, 0)),  # P(H|H, Δt)
            pi_H * (1 - exp(-lambda * pmax(time_gap_yr, 0)))           # P(H|L, Δt)
        ),
        # For non-returned: time_gap is NA → ctmc_prob = NA (handled later)
        ctmc_prob = replace_na(ctmc_prob, pi_H)  # default to stationary
    )
}

df_train <- add_ctmc_feature(df_train)
df_val   <- add_ctmc_feature(df_val)

cat(sprintf("\nCTMC feature summary (returned patients):\n"))
cat("Train:\n"); print(summary(df_train$ctmc_prob[df_train$mask]))
cat("Val:\n");   print(summary(df_val$ctmc_prob[df_val$mask]))


#### PART 3: Time-weighted EWMA ####
cat("\n\n============ PART 3: TIME-WEIGHTED EWMA ============\n\n")
cat(sprintf("Using τ = %.3f years from variogram fit\n\n", tau_hat))

compute_tw_ewma <- function(dat, tau) {
    # For each patient: weighted average of A1c readings with exp(-Δt/τ) decay from latest
    a1c_wide <- dat$measurements %>%
        filter(variable %in% paste0("a1c_", 1:5)) %>%
        filter(!is.na(value)) %>%
        arrange(id, time) %>%
        group_by(id) %>%
        summarize(
            tw_ewma = {
                t_latest <- max(time) / 365.25
                t_yr <- time / 365.25
                w <- exp(-(t_latest - t_yr) / tau)
                sum(w * value) / sum(w)
            },
            # Also compute the GP-style prediction variance at t_latest
            # (how uncertain is our estimate based on temporal spread?)
            tw_ewma_n_eff = {
                t_latest <- max(time) / 365.25
                t_yr <- time / 365.25
                w <- exp(-(t_latest - t_yr) / tau)
                sum(w)^2 / sum(w^2)  # effective sample size
            },
            .groups = "drop"
        )
    a1c_wide
}

tw_train <- compute_tw_ewma(train_dat, tau_hat)
tw_val   <- compute_tw_ewma(val_dat, tau_hat)

df_train <- df_train %>% left_join(tw_train, by = "id")
df_val   <- df_val   %>% left_join(tw_val, by = "id")

# How does tw_ewma compare to regular ewma?
cat("Correlation tw_ewma vs a1c_weighted:\n")
cat(sprintf("  Train: r = %.4f\n", cor(df_train$tw_ewma, df_train$a1c_weighted, use = "complete.obs")))
cat(sprintf("  Val:   r = %.4f\n", cor(df_val$tw_ewma, df_val$a1c_weighted, use = "complete.obs")))

cat("\nSummary of tw_ewma:\n"); print(summary(df_train$tw_ewma))
cat("Summary of a1c_weighted:\n"); print(summary(df_train$a1c_weighted))


#### PART 4: GP prediction (OU kernel) ####
cat("\n\n============ PART 4: GP PREDICTION (OU KERNEL) ============\n\n")

# Hyperparameters from lmer + variogram:
# σ_f² = between-patient variance ≈ 1.950 (from lmer random intercept)
# σ_n² = measurement noise ≈ 1.352 (from lmer residual)
# ℓ = τ (memory timescale from variogram)
sigma_f2 <- 1.950  # from lmer
sigma_n2 <- 1.352  # from lmer
ell <- tau_hat     # from variogram

cat(sprintf("GP hyperparameters: σ_f²=%.3f, σ_n²=%.3f, ℓ=%.3f yr\n", sigma_f2, sigma_n2, ell))
pop_mean <- mean(a1c_long$value)  # population mean A1c
cat(sprintf("Population mean μ = %.3f\n\n", pop_mean))

# For each patient: compute GP posterior mean and variance at t_2025
gp_predict_patient <- function(t_obs, y_obs, t_star, sigma_f2, sigma_n2, ell, mu) {
    # t_obs, y_obs: vectors of observation times and values
    # t_star: prediction time (scalar)
    # Returns: list(mean, var)
    n <- length(t_obs)
    if (n == 0 || is.na(t_star)) return(list(mean = mu, var = sigma_f2 + sigma_n2))

    # Kernel matrix K(t_obs, t_obs) + σ_n² I
    K <- outer(t_obs, t_obs, function(a, b) sigma_f2 * exp(-abs(a - b) / ell))
    K_noise <- K + sigma_n2 * diag(n)

    # Cross-kernel k* = K(t_star, t_obs)
    k_star <- sigma_f2 * exp(-abs(t_star - t_obs) / ell)

    # GP posterior
    K_inv_y <- solve(K_noise, y_obs - mu)
    K_inv_k <- solve(K_noise, k_star)

    gp_mean <- mu + sum(k_star * K_inv_y)
    gp_var  <- (sigma_f2 + sigma_n2) - sum(k_star * K_inv_k)
    gp_var  <- max(gp_var, 0.001)  # numerical floor

    list(mean = gp_mean, var = gp_var)
}

# Get eval times for each patient
eval_times <- bind_rows(
    train_dat$measurements %>% filter(variable == "a1c_2025") %>% select(id, time),
    val_dat$measurements   %>% filter(variable == "a1c_2025") %>% select(id, time)
) %>%
    rename(eval_time = time) %>%
    mutate(eval_time_yr = eval_time / 365.25)

# Compute GP predictions for all patients
cat("Computing GP predictions for each patient...\n")
t0 <- Sys.time()

gp_results <- a1c_long %>%
    group_by(id) %>%
    summarize(
        t_obs = list(time_yr),
        y_obs = list(value),
        .groups = "drop"
    ) %>%
    left_join(eval_times, by = "id") %>%
    rowwise() %>%
    mutate(
        gp = list(gp_predict_patient(t_obs, y_obs, eval_time_yr, sigma_f2, sigma_n2, ell, pop_mean)),
        gp_mean = gp$mean,
        gp_var  = gp$var,
        gp_sd   = sqrt(gp_var)
    ) %>%
    ungroup() %>%
    select(id, gp_mean, gp_var, gp_sd)

cat(sprintf("GP prediction time: %.1f sec\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

cat("\nGP prediction mean (returned patients):\n")
gp_returned <- gp_results %>% semi_join(eval_times, by = "id")
print(summary(gp_returned$gp_mean))
cat("GP prediction SD:\n")
print(summary(gp_returned$gp_sd))

# Add GP features
df_train <- df_train %>% left_join(gp_results, by = "id") %>%
    mutate(across(c(gp_mean, gp_var, gp_sd), ~replace_na(., pop_mean)))
df_val <- df_val %>% left_join(gp_results, by = "id") %>%
    mutate(across(c(gp_mean, gp_var, gp_sd), ~replace_na(., pop_mean)))


#### PART 5: Correlations & single-feature evaluation ####
cat("\n\n============ PART 5: FEATURE EVALUATION ============\n\n")

# Correlations among new features and existing ones
check <- bind_rows(df_train, df_val) %>%
    select(id, a1c_weighted, a1c_latest, ctmc_prob, tw_ewma, gp_mean, gp_sd) %>%
    drop_na()
cm <- cor(check %>% select(-id))

cat("Correlation matrix:\n")
print(round(cm, 3))

# Single-feature GLM tests
cat("\n--- Single-feature head-to-head ---\n")
single_test <- function(feat_name, df_tr, df_va) {
    f <- as.formula(paste("target ~", feat_name))
    m <- tryCatch(glm(f, data = df_tr, family = binomial), error = function(e) NULL)
    if (is.null(m)) { cat(sprintf("  %-20s → FAILED\n", feat_name)); return(invisible()) }
    p_tr <- predict(m, df_tr, type = "response")
    th <- best_f1_threshold(p_tr, df_tr$target == "Uncontrolled", df_tr$mask)
    p_va <- predict(m, df_va, type = "response")
    res <- compute_metrics(p_va > th["best_th"], df_va$target == "Uncontrolled", df_va$mask, verbose = FALSE)
    cat(sprintf("  %-20s → Val F1 = %.4f  (Prec=%.3f, Rec=%.3f)\n",
        feat_name, res$metrics["F1"], res$metrics["Precision"], res$metrics["Recall"]))
}

# Impute for GLM
df_train_imp <- impute_and_flag(df_train)
df_val_imp   <- impute_and_flag(df_val)

single_test("a1c_weighted",  df_train_imp, df_val_imp)
single_test("a1c_latest",    df_train_imp, df_val_imp)
single_test("tw_ewma",       df_train_imp, df_val_imp)
single_test("ctmc_prob",     df_train_imp, df_val_imp)
single_test("gp_mean",       df_train_imp, df_val_imp)
single_test("gp_sd",         df_train_imp, df_val_imp)


#### PART 6: XGBoost with new features ####
cat("\n\n============ PART 6: XGBOOST EVALUATION ============\n\n")
library(xgboost)

feat_cols <- c(
    "a1c_latest", "value_a1c_1", "a1c_weighted", "abs_change", "a1c_sd",
    "a1c_range", "a1c_rate", "a1c_change",
    "n_a1c", "n_drug_classes", "total_meds",
    "sulfonylurea", "insulin", "metformin", "sglt2", "glp1", "dpp4",
    "ed_visits", "pcp_visits", "admissions",
    "age", "gender_male",
    "adi_state", "adi_nation", "cad", "copd",
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

new_feats <- c("ctmc_prob", "tw_ewma", "gp_mean", "gp_sd", "tw_ewma_n_eff")
feat_cols_new <- c(feat_cols, new_feats)

params <- list(objective = "binary:logistic", eval_metric = "logloss",
               max_depth = 3, eta = 0.05, subsample = 0.7, colsample_bytree = 0.6,
               min_child_weight = 20, gamma = 1)

# Baseline (no new features)
cat("--- XGBoost baseline ---\n")
xgb_tr0 <- xgb.DMatrix(data = as.matrix(df_train[, feat_cols]), label = df_train$target == "Uncontrolled")
xgb_va0 <- xgb.DMatrix(data = as.matrix(df_val[, feat_cols]),   label = df_val$target == "Uncontrolled")
m0 <- xgb.train(params, xgb_tr0, nrounds = 500, watchlist = list(train = xgb_tr0, val = xgb_va0),
                 early_stopping_rounds = 30, print_every_n = 100, verbose = 1)
p0_tr <- predict(m0, xgb_tr0); th0 <- best_f1_threshold(p0_tr, df_train$target == "Uncontrolled", df_train$mask)
p0_va <- predict(m0, xgb_va0)
cat("Baseline val:\n")
compute_metrics(p0_va > th0["best_th"], df_val$target == "Uncontrolled", df_val$mask)

# With new features
cat("\n--- XGBoost + continuous-time features ---\n")
xgb_tr1 <- xgb.DMatrix(data = as.matrix(df_train[, feat_cols_new]), label = df_train$target == "Uncontrolled")
xgb_va1 <- xgb.DMatrix(data = as.matrix(df_val[, feat_cols_new]),   label = df_val$target == "Uncontrolled")
m1 <- xgb.train(params, xgb_tr1, nrounds = 500, watchlist = list(train = xgb_tr1, val = xgb_va1),
                 early_stopping_rounds = 30, print_every_n = 100, verbose = 1)
p1_tr <- predict(m1, xgb_tr1); th1 <- best_f1_threshold(p1_tr, df_train$target == "Uncontrolled", df_train$mask)
p1_va <- predict(m1, xgb_va1)
cat("+ continuous-time features val:\n")
compute_metrics(p1_va > th1["best_th"], df_val$target == "Uncontrolled", df_val$mask)

cat("\n--- Feature importance (top 20) ---\n")
imp <- xgb.importance(feature_names = feat_cols_new, model = m1)
print(imp[1:min(20, nrow(imp)), ], row.names = FALSE)

cat("\n--- New features in importance ---\n")
imp_new <- imp %>% filter(Feature %in% new_feats)
if (nrow(imp_new) > 0) print(imp_new, row.names = FALSE) else cat("  (none in top features)\n")


cat("\n\n============ DONE ============\n")
cat("Key outputs:\n")
cat(sprintf("  τ (memory timescale) = %.3f years = %.0f days\n", tau_hat, tau_hat * 365.25))
cat(sprintf("  CTMC: q_LH=%.3f, q_HL=%.3f, π_H=%.3f\n", q_LH, q_HL, pi_H))
cat("  Check: does ctmc_prob or gp_mean beat a1c_weighted as single feature?\n")
cat("  Check: do new features improve XGBoost?\n")
