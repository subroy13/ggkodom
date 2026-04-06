#### RESIDUAL ANALYSIS: What signal exists beyond A1c level? ####
#
# The question: a1c_weighted dominates everything. If we remove its effect,
# what's left? Is the residual pure noise (ceiling is real) or does it
# have structure (there's signal XGBoost might be missing)?
#
# Approach:
#   Stage 1: fit P(Y=1 | a1c_weighted) — univariate sigmoid
#   Compute response residuals: r_i = y_i - p_hat_i
#   Correlate r_i with every other feature
#   Look for non-linear structure in the residuals

library(tidyverse)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")

df_train <- impute_and_flag(create_features(train_dat))
df_val <- impute_and_flag(create_features(val_dat))

# returned patients only (competition metric)
df_tr <- df_train %>% filter(mask)
df_va <- df_val %>% filter(mask)

y_tr <- as.integer(df_tr$target == "Uncontrolled")
y_va <- as.integer(df_va$target == "Uncontrolled")

cat("Train returned:", nrow(df_tr), "| Positive rate:", round(mean(y_tr), 3), "\n")
cat("Val returned:", nrow(df_va), "| Positive rate:", round(mean(y_va), 3), "\n")


#### STAGE 1: Univariate sigmoid on a1c_weighted ####

m_s1 <- glm(y_tr ~ a1c_weighted, data = df_tr, family = binomial)
summary(m_s1)

p_s1_tr <- predict(m_s1, df_tr, type = "response")
p_s1_va <- predict(m_s1, df_va, type = "response")

# how good is a1c_weighted alone?
th_s1 <- best_f1_threshold(p_s1_tr, y_tr)
cat("\n--- Stage 1 (a1c_weighted only) ---\n")
cat("Train:\n")
th_s1
cat("Val:\n")
compute_metrics(p_s1_va > th_s1["best_th"], y_va)


#### RESPONSE RESIDUALS ####
# r_i = y_i - p_hat_i
# positive = model underpredicts (patient is uncontrolled but model thinks controlled)
# negative = model overpredicts (patient is controlled but model thinks uncontrolled)

resid_tr <- y_tr - p_s1_tr

cat("\nResidual summary:\n")
cat("  Mean:", round(mean(resid_tr), 5), "(should be ~0)\n")
cat("  SD:", round(sd(resid_tr), 4), "\n")
cat("  Range:", round(range(resid_tr), 3), "\n")


#### CORRELATE RESIDUALS WITH ALL OTHER FEATURES ####
# Which features predict where the a1c_weighted-only model goes wrong?

other_feats <- c(
    "a1c_latest", "value_a1c_1", "abs_change", "a1c_sd", "a1c_range", "a1c_rate",
    "n_a1c", "a1c_change",
    "n_drug_classes", "total_meds",
    "sulfonylurea", "insulin", "metformin", "sglt2", "glp1", "dpp4",
    "ed_visits", "pcp_visits", "admissions",
    "age", "gender_male",
    "adi_state", "adi_nation", "cad", "copd",
    "race_black", "race_white", "race_asian", "ethnicity_hl",
    "value_hdl", "value_ldl", "value_chol",
    "value_height", "value_weight",
    "days_to_eval", "time_gap",
    "max_drop", "max_rise", "has_sudden_drop", "has_sudden_rise",
    "frac_above_7", "n_crossings_7", "slope_above_7", "slope_below_7",
    "frac_above_8", "n_crossings_8", "slope_above_8", "slope_below_8",
    "post_drop_drift", "sudden_drop_still_high7", "sudden_drop_still_high8",
    "high7_insulin", "high7_no_meds", "still_high7_improving", "a1c_change_if_high7",
    "high8_insulin", "high8_no_meds", "still_high8_improving", "a1c_change_if_high8",
    "a1c_x_meds", "a1c_x_ndrug", "a1c_per_drug",
    "improving_on_meds", "worsening_on_meds", "stable_on_meds",
    "on_modern_drugs", "n_a1c_x_ndrug",
    "bmi", "bmi_x_male"
)

# linear correlation with residuals
cors <- sapply(other_feats, function(col) {
    cor(df_tr[[col]], resid_tr, use = "complete.obs")
})
cors_df <- data.frame(feature = names(cors), cor = cors) %>%
    arrange(desc(abs(cor))) %>%
    mutate(cor = round(cor, 4))

cat("\n--- Correlation of features with Stage 1 residuals ---\n")
cat("(positive cor = feature predicts underprediction = patient worse than a1c_weighted suggests)\n\n")
print(cors_df, row.names = FALSE)


#### ADDED-VARIABLE F-TESTS ####
# For each feature, test whether it adds significant predictive power
# beyond a1c_weighted. This is the partial regression F-test.

cat("\n--- Added-variable significance (likelihood ratio test) ---\n")
ll_base <- logLik(m_s1)

lr_tests <- sapply(other_feats, function(col) {
    tmp <- data.frame(y = y_tr, a1c_weighted = df_tr$a1c_weighted, x = df_tr[[col]])
    m_add <- glm(y ~ a1c_weighted + x, data = tmp, family = binomial)
    ll_add <- logLik(m_add)
    lr_stat <- as.numeric(2 * (ll_add - ll_base))
    p_val <- pchisq(lr_stat, df = 1, lower.tail = FALSE)
    c(lr_stat = lr_stat, p_val = p_val)
})

lr_df <- data.frame(
    feature = colnames(lr_tests),
    lr_stat = round(lr_tests["lr_stat", ], 2),
    p_val = lr_tests["p_val", ]
) %>% arrange(desc(lr_stat))
lr_df$p_adj <- p.adjust(lr_df$p_val, method = "BH")
lr_df$sig <- ifelse(lr_df$p_adj < 0.01, "***", ifelse(lr_df$p_adj < 0.05, "**", ifelse(lr_df$p_adj < 0.1, "*", "")))

cat("\nTop 30 features by LR test statistic (after a1c_weighted):\n")
print(head(lr_df, 30), row.names = FALSE)


#### STAGE 2: FIT GLM ON RESIDUALS WITH TOP FEATURES ####
# Use response residuals as pseudo-response in a linear model
# This shows what direction/magnitude of effect each feature has in the residual space

# pick features with p_adj < 0.05
sig_feats <- lr_df$feature[lr_df$p_adj < 0.05]
cat("\n--- Significant features (BH-adjusted p < 0.05):", length(sig_feats), "---\n")
cat(sig_feats, sep = ", ")
cat("\n")


#### TWO-STAGE MODEL: does it beat global? ####
# Stage 1: a1c_weighted → probability
# Stage 2: logistic on residuals with significant features
# Combined: logit(p) = logit(p_stage1) + stage2_linear_predictor
# Equivalent to: just add the features to the model (but decomposed for understanding)

if (length(sig_feats) > 0) {
    # this is equivalent to adding sig_feats to the a1c_weighted model
    f_s2 <- as.formula(paste("y ~ a1c_weighted +", paste(sig_feats, collapse = " + ")))
    df_tr_s2 <- df_tr
    df_tr_s2$y <- y_tr
    df_va_s2 <- df_va
    df_va_s2$y <- y_va
    m_s2 <- glm(f_s2, data = df_tr_s2, family = binomial)

    p_s2_tr <- predict(m_s2, df_tr_s2, type = "response")
    p_s2_va <- predict(m_s2, df_va_s2, type = "response")

    th_s2 <- best_f1_threshold(p_s2_tr, y_tr)
    cat("\n--- Stage 1+2 (a1c_weighted + significant features) ---\n")
    cat("Train:\n")
    th_s2
    cat("Val:\n")
    compute_metrics(p_s2_va > th_s2["best_th"], y_va)

    cat("\nCoefficients of Stage 2 features (|coef|, sorted):\n")
    s2_coefs <- coef(m_s2)
    s2_coefs <- sort(abs(s2_coefs[!names(s2_coefs) %in% c("(Intercept)", "a1c_weighted")]), decreasing = TRUE)
    print(round(s2_coefs, 4))
}


#### RESIDUAL STRUCTURE BY A1C REGIME ####
# Is the residual signal different in different A1c regimes?

df_tr$regime <- cut(df_tr$a1c_weighted, breaks = c(0, 6, 7, 8, 20),
                    labels = c("<6", "6-7", "7-8", ">=8"))
df_tr$resid <- resid_tr

cat("\n--- Residual stats by A1c regime ---\n")
regime_stats <- df_tr %>%
    group_by(regime) %>%
    summarise(
        n = n(),
        mean_resid = round(mean(resid), 4),
        sd_resid = round(sd(resid), 4),
        pos_rate = round(mean(y_tr[cur_group_rows()]), 3),
        pred_rate = round(mean(p_s1_tr[cur_group_rows()]), 3),
        .groups = "drop"
    )
print(regime_stats)


#### NON-LINEAR CHECK ####
# For the top residual features, check if the relationship with the residual
# is non-linear (would a GAM spline help?)

library(mgcv)

cat("\n--- Non-linear signal check (GAM on top features) ---\n")
# take top 5 continuous features from LR test
top5_continuous <- lr_df$feature[1:min(10, nrow(lr_df))]
# remove binary features (GAM splines don't work on them)
top5_continuous <- top5_continuous[sapply(top5_continuous, function(x) length(unique(df_tr[[x]])) > 10)]
top5_continuous <- head(top5_continuous, 5)

cat("Testing non-linearity for:", paste(top5_continuous, collapse = ", "), "\n\n")

for (feat in top5_continuous) {
    gam_data <- data.frame(y = y_tr, aw = df_tr$a1c_weighted, x = df_tr[[feat]])
    # linear model: a1c_weighted + feat
    m_lin <- glm(y ~ aw + x, data = gam_data, family = binomial)
    # GAM: a1c_weighted + s(feat)
    m_gam <- gam(y ~ aw + s(x, k = 5), data = gam_data, family = binomial)

    aic_diff <- AIC(m_lin) - AIC(m_gam)
    edf <- sum(m_gam$edf) - 2  # effective df used by the spline (minus intercept + aw)

    cat(sprintf("  %-30s | AIC_linear - AIC_gam = %+.1f | spline edf = %.1f | %s\n",
        feat, aic_diff, edf,
        ifelse(aic_diff > 4, "NON-LINEAR SIGNAL", ifelse(aic_diff > 2, "marginal", "linear is fine"))))
}


#### INTERACTION CHECK ####
# Are there interactions between a1c_weighted and other features
# that the additive model misses?

cat("\n--- Interaction effects with a1c_weighted ---\n")
# test: does a1c_weighted * feature improve over a1c_weighted + feature?

top_interact <- head(lr_df$feature, 15)
for (feat in top_interact) {
    tmp <- data.frame(y = y_tr, a1c_weighted = df_tr$a1c_weighted, x = df_tr[[feat]])
    m_add <- glm(y ~ a1c_weighted + x, data = tmp, family = binomial)
    m_int <- glm(y ~ a1c_weighted * x, data = tmp, family = binomial)
    lr_int <- as.numeric(2 * (logLik(m_int) - logLik(m_add)))
    p_int <- pchisq(lr_int, df = 1, lower.tail = FALSE)
    sig <- ifelse(p_int < 0.001, "***", ifelse(p_int < 0.01, "**", ifelse(p_int < 0.05, "*", "")))
    cat(sprintf("  %-30s | interaction LR = %6.2f | p = %.4f %s\n", feat, lr_int, p_int, sig))
}


cat("\n\n#### DONE ####\n")
