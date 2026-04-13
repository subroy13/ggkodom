###############################################################################
## Slim medically-meaningful model — XGBoost + GLM on ~19 features
##
## Motivation: the 74-feature model has `days_to_eval` at 28% gain, but the
## audit (scripts/subrata_days_to_eval_audit.R) showed it is a confound with
## time_gap + n_a1c and carries essentially no unique signal (dropping it
## costs 0.19 pp, within noise). The slim feature list below drops every
## hand-crafted interaction (a1c_x_meds, a1c_x_ndrug, a1c_per_drug), every
## administrative leak (days_to_eval), lipid panels, race/ethnicity dummies,
## visit-count features, and keeps only features with a clear medical
## interpretation for the slide.
##
## Slide target: show this 19-feature XGBoost matches the 74-feature XGBoost
## within noise. The presentation story becomes "19 medically-meaningful
## features recover the entire 62% ceiling."
###############################################################################

library(tidyverse)
library(xgboost)
source("./scripts/utils.R")
source("./scripts/subrata_features.R")

train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")

df_train <- create_features(train_dat)
df_val   <- create_features(val_dat)
df_train_imp <- impute_and_flag(df_train)
df_val_imp   <- impute_and_flag(df_val)

#### SLIM FEATURE SET ####
feat_slim <- c(
    # A1c core (5) — the measurement and its summary statistics
    "a1c_weighted", "a1c_latest", "frac_above_7", "frac_above_8", "a1c_sd",
    # Trajectory (3) — regime dynamics and acute worsening
    "slope_above_7", "slope_below_8", "max_rise",
    # Treatment (3) — intensity
    "n_drug_classes", "insulin", "insulin_rate",
    # Time (1) — prediction horizon (CTMC-relevant, NOT days_to_eval)
    "time_gap",
    # Demographics (3)
    "age", "gender_male", "adi_nation",
    # Comorbidities (2) — diabetic cardiovascular risk
    "cad", "copd",
    # Body composition (2) — BMI with sex interaction
    "bmi", "bmi_x_male"
)
cat("\nSlim feature set (", length(feat_slim), "features):\n", paste(feat_slim, collapse = ", "), "\n\n")


#### XGBOOST REGULARIZED — matches subrata_models.R config exactly ####
cat("\n#### XGBoost (regularized, slim) ####\n")

xgb_train <- xgb.DMatrix(
    data = as.matrix(df_train[, feat_slim]),
    label = df_train$target == "Uncontrolled"
)
xgb_val <- xgb.DMatrix(
    data = as.matrix(df_val[, feat_slim]),
    label = df_val$target == "Uncontrolled"
)

params_reg <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = 5, eta = 0.05,
    subsample = 0.5, colsample_bytree = 0.5,
    min_child_weight = 10, gamma = 1
)

set.seed(1)
m_xgb <- xgb.train(
    params = params_reg, data = xgb_train, nrounds = 500,
    watchlist = list(train = xgb_train, val = xgb_val),
    early_stopping_rounds = 30, print_every_n = 50, verbose = 1
)

p_xgb_tr  <- predict(m_xgb, xgb_train)
p_xgb_val <- predict(m_xgb, xgb_val)
th_xgb <- best_f1_threshold(p_xgb_tr, df_train$target == "Uncontrolled", df_train$mask)
cat("train-tuned threshold:", round(th_xgb["best_th"], 3), "\n")

cat("\n--- XGBoost train metrics (slim) ---\n")
compute_metrics(p_xgb_tr > th_xgb["best_th"], df_train$target == "Uncontrolled", df_train$mask)

cat("\n--- XGBoost val metrics (slim) ---\n")
compute_metrics(p_xgb_val > th_xgb["best_th"], df_val$target == "Uncontrolled", df_val$mask)

cat("\n--- XGBoost feature importance (slim) ---\n")
imp_slim <- xgb.importance(feature_names = feat_slim, model = m_xgb)
print(imp_slim, row.names = FALSE)


#### GLM ON THE SAME SLIM SET ####
# GLM needs imputed data + optional missingness flags. Use df_train_imp which
# already has medians imputed. For the slide story, report both GLM and
# GLM+weights on the identical feature list.

cat("\n\n#### GLM (slim) ####\n")
f_slim <- as.formula(paste("target ~", paste(feat_slim, collapse = " + ")))

m_glm <- glm(f_slim, data = df_train_imp, family = binomial)
p_glm_tr <- predict(m_glm, df_train_imp, type = "response")
p_glm_val <- predict(m_glm, df_val_imp, type = "response")
th_glm <- best_f1_threshold(p_glm_tr, df_train_imp$target == "Uncontrolled", df_train_imp$mask)
cat("GLM train-tuned threshold:", round(th_glm["best_th"], 3), "\n")
cat("--- GLM val metrics (slim) ---\n")
compute_metrics(p_glm_val > th_glm["best_th"], df_val_imp$target == "Uncontrolled", df_val_imp$mask)

cat("\n--- GLM coefficients (slim) ---\n")
glm_coef <- summary(m_glm)$coefficients
print(round(glm_coef[, c("Estimate", "Std. Error", "Pr(>|z|)")], 4))


cat("\n\n#### GLM + class weights (slim) ####\n")
wts <- ifelse(df_train_imp$target == "Uncontrolled",
              1 - mean(df_train_imp$target == "Uncontrolled"),
              mean(df_train_imp$target == "Uncontrolled"))
m_glm_wt <- glm(f_slim, data = df_train_imp, family = binomial, weights = wts)
p_glm_wt_tr <- predict(m_glm_wt, df_train_imp, type = "response")
p_glm_wt_val <- predict(m_glm_wt, df_val_imp, type = "response")
th_glm_wt <- best_f1_threshold(p_glm_wt_tr, df_train_imp$target == "Uncontrolled", df_train_imp$mask)
cat("GLM+wt train-tuned threshold:", round(th_glm_wt["best_th"], 3), "\n")
cat("--- GLM+wt val metrics (slim) ---\n")
compute_metrics(p_glm_wt_val > th_glm_wt["best_th"], df_val_imp$target == "Uncontrolled", df_val_imp$mask)


#### SLIM PLUS: add clinically meaningful extras ####
# Add back the features that showed up in the 74-feature top 20 AND have a
# clear clinical story. These are the "just enough complexity to match the
# full model" candidates.
feat_slim_plus <- c(
    feat_slim,
    "value_a1c_1",       # explicit first A1c reading — clinical baseline
    "a1c_change",        # total change from first to last reading
    "n_a1c",             # number of readings — data richness / engagement
    "total_meds",        # total medication order burden
    "a1c_per_drug",      # A1c per drug class — treatment efficacy proxy
    "max_drop",          # biggest single-step improvement (complement to max_rise)
    "high8_insulin",     # on insulin AND still above 8 — treatment failure signature
    "high7_insulin",     # on insulin AND still above 7 — borderline failure
    "value_hdl",         # lipids — CVD risk
    "value_ldl",
    "value_chol"
)
cat("\n\n#### XGBoost SLIM PLUS (", length(feat_slim_plus), "features) ####\n")

xgb_train_p <- xgb.DMatrix(
    data = as.matrix(df_train[, feat_slim_plus]),
    label = df_train$target == "Uncontrolled"
)
xgb_val_p <- xgb.DMatrix(
    data = as.matrix(df_val[, feat_slim_plus]),
    label = df_val$target == "Uncontrolled"
)
set.seed(1)
m_xgb_p <- xgb.train(
    params = params_reg, data = xgb_train_p, nrounds = 500,
    watchlist = list(train = xgb_train_p, val = xgb_val_p),
    early_stopping_rounds = 30, print_every_n = 50, verbose = 1
)
p_xgb_p_tr  <- predict(m_xgb_p, xgb_train_p)
p_xgb_p_val <- predict(m_xgb_p, xgb_val_p)
th_xgb_p <- best_f1_threshold(p_xgb_p_tr, df_train$target == "Uncontrolled", df_train$mask)
cat("train-tuned threshold:", round(th_xgb_p["best_th"], 3), "\n")
cat("\n--- XGBoost val metrics (slim plus) ---\n")
compute_metrics(p_xgb_p_val > th_xgb_p["best_th"], df_val$target == "Uncontrolled", df_val$mask)
cat("\n--- XGBoost feature importance (slim plus) ---\n")
imp_slim_p <- xgb.importance(feature_names = feat_slim_plus, model = m_xgb_p)
print(imp_slim_p, row.names = FALSE)


#### SLIM NEW: single-threshold (8.0 only), clinically interpretable ####
# 2026-04-08 Subrata — presentation-side decision: having BOTH the 7.0 and
# 8.0 threshold features on the slide is confusing for a medical audience
# ("what about the 7-8 zone?"). For the final slide-ready model we keep only
# the 8.0-threshold features (competition target), drop the parallel 7.0
# features, and add `a1c_x_ndrug` — a hand-crafted interaction that a
# clinician can read as "A1c burden × number of drug classes, a treatment-
# resistance severity proxy".
#
# Removed from slim_plus: frac_above_7, slope_above_7, high7_insulin
# Added:                  a1c_x_ndrug
# Kept:                   everything else in slim_plus (including a1c_per_drug
#                         which was already there)
feat_slim_new <- c(
    setdiff(feat_slim_plus, c("frac_above_7", "slope_above_7", "high7_insulin")),
    "a1c_x_ndrug"
)
cat("\n\n#### XGBoost SLIM NEW (", length(feat_slim_new), "features) ####\n")
cat("Features:\n", paste(feat_slim_new, collapse = ", "), "\n\n")

xgb_train_n <- xgb.DMatrix(
    data = as.matrix(df_train[, feat_slim_new]),
    label = df_train$target == "Uncontrolled"
)
xgb_val_n <- xgb.DMatrix(
    data = as.matrix(df_val[, feat_slim_new]),
    label = df_val$target == "Uncontrolled"
)
set.seed(1)
m_xgb_n <- xgb.train(
    params = params_reg, data = xgb_train_n, nrounds = 500,
    watchlist = list(train = xgb_train_n, val = xgb_val_n),
    early_stopping_rounds = 30, print_every_n = 50, verbose = 1
)
p_xgb_n_tr  <- predict(m_xgb_n, xgb_train_n)
p_xgb_n_val <- predict(m_xgb_n, xgb_val_n)
th_xgb_n <- best_f1_threshold(p_xgb_n_tr, df_train$target == "Uncontrolled", df_train$mask)
cat("train-tuned threshold:", round(th_xgb_n["best_th"], 3), "\n")
cat("\n--- XGBoost val metrics (slim new) ---\n")
compute_metrics(p_xgb_n_val > th_xgb_n["best_th"], df_val$target == "Uncontrolled", df_val$mask)
cat("\n--- XGBoost feature importance (slim new) ---\n")
imp_slim_n <- xgb.importance(feature_names = feat_slim_new, model = m_xgb_n)
print(imp_slim_n, row.names = FALSE)


#### AUC + R² SUITE — for the slide (organisers also check R², not just F1) ####
# Compute returned-patients-only metrics on val for every model variant.
# Metrics reported:
#   AUC               — ROC AUC via Mann-Whitney U statistic (rank-based)
#   R2_efron          — classical R² = 1 - sum((y - p)^2) / sum((y - ybar)^2)
#                       (this is the "R²" most people mean if they don't qualify)
#   R2_mcfadden       — 1 - LL_model / LL_null (pseudo-R² on log-likelihood)
#   R2_tjur           — mean(p | y=1) - mean(p | y=0) (coef of discrimination)
#   brier             — mean((y - p)^2)
#
# AUC note: for ~18% positive rate, AUC can be deceptively high compared to F1.
# That's expected — AUC measures ranking, F1 measures threshold-based trade-off.

model_metrics <- function(p, y, msk, label = "") {
    # restrict to returned patients (msk == TRUE) to match competition scoring
    keep <- msk & !is.na(p) & !is.na(y)
    p  <- as.numeric(p[keep])
    y  <- as.integer(y[keep])

    n  <- length(y)
    n1 <- sum(y == 1)
    n0 <- sum(y == 0)
    pbar <- mean(y)

    # --- AUC via rank (Mann-Whitney) ---
    ranks <- rank(p)
    auc <- (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)

    # --- Efron's R² (classical coefficient of determination) ---
    r2_efron <- 1 - sum((y - p)^2) / sum((y - pbar)^2)

    # --- McFadden's pseudo-R² ---
    eps <- 1e-15
    ll_model <- sum(y * log(pmax(p, eps)) + (1 - y) * log(pmax(1 - p, eps)))
    ll_null  <- n * (pbar * log(pbar) + (1 - pbar) * log(1 - pbar))
    r2_mcfadden <- 1 - ll_model / ll_null

    # --- Tjur's R² (coefficient of discrimination) ---
    r2_tjur <- mean(p[y == 1]) - mean(p[y == 0])

    # --- Brier score ---
    brier <- mean((y - p)^2)

    out <- c(
        n         = n,
        base_rate = pbar,
        AUC       = auc,
        R2_efron  = r2_efron,
        R2_mcfadden = r2_mcfadden,
        R2_tjur   = r2_tjur,
        Brier     = brier
    )
    cat(sprintf("\n--- %s (n=%d, base_rate=%.3f) ---\n", label, n, pbar))
    cat(sprintf("  AUC               : %.4f\n", auc))
    cat(sprintf("  R² (Efron/classical): %.4f\n", r2_efron))
    cat(sprintf("  R² (McFadden)     : %.4f\n", r2_mcfadden))
    cat(sprintf("  R² (Tjur)         : %.4f\n", r2_tjur))
    cat(sprintf("  Brier score       : %.4f\n", brier))
    invisible(out)
}

cat("\n\n#### AUC + R² SUITE (val, returned patients only) ####\n")
y_val    <- df_val$target == "Uncontrolled"
msk_val  <- df_val$mask
y_val_imp   <- df_val_imp$target == "Uncontrolled"
msk_val_imp <- df_val_imp$mask

met_slim     <- model_metrics(p_xgb_val,    y_val, msk_val,
                              "XGBoost SLIM (19 feat)")
met_slim_p   <- model_metrics(p_xgb_p_val,  y_val, msk_val,
                              "XGBoost SLIM PLUS (30 feat)")
met_slim_n   <- model_metrics(p_xgb_n_val,  y_val, msk_val,
                              "XGBoost SLIM NEW (28 feat)  *** slide model ***")
met_glm      <- model_metrics(p_glm_val,    y_val_imp, msk_val_imp,
                              "GLM SLIM (19 feat)")
met_glm_wt   <- model_metrics(p_glm_wt_val, y_val_imp, msk_val_imp,
                              "GLM+wt SLIM (19 feat)")

cat("\n\n#### ONE-LINE HEADLINES FOR THE SLIDE ####\n")
.headline_f1 <- as.numeric(
    compute_metrics(p_xgb_n_val > th_xgb_n["best_th"], y_val, msk_val,
                    verbose = FALSE)$metrics["F1"]
)
cat(sprintf("XGBoost SLIM NEW, val, returned patients only (n=%d):\n",
            sum(msk_val & !is.na(p_xgb_n_val))))
cat(sprintf("  F1 = %.4f   AUC = %.4f   R² (Efron) = %.4f   Brier = %.4f\n",
            .headline_f1,
            met_slim_n["AUC"], met_slim_n["R2_efron"], met_slim_n["Brier"]))


#### FINAL SUMMARY TABLE ####
cat("\n\n#### FINAL SUMMARY ####\n")
cat(sprintf("%-30s  %-8s  %6s\n", "Model", "Features", "Val F1"))
cat(sprintf("%-30s  %-8s  %6s\n", "-----", "--------", "------"))
get_f1 <- function(p, th, targ, msk) {
    m <- compute_metrics(p > th, targ, msk, verbose = FALSE)
    as.numeric(m$metrics["F1"])
}
f1_xgb      <- get_f1(p_xgb_val,    th_xgb["best_th"],    df_val$target == "Uncontrolled", df_val$mask)
f1_xgb_p    <- get_f1(p_xgb_p_val,  th_xgb_p["best_th"],  df_val$target == "Uncontrolled", df_val$mask)
f1_xgb_n    <- get_f1(p_xgb_n_val,  th_xgb_n["best_th"],  df_val$target == "Uncontrolled", df_val$mask)
f1_glm      <- get_f1(p_glm_val,    th_glm["best_th"],    df_val_imp$target == "Uncontrolled", df_val_imp$mask)
f1_glm_wt   <- get_f1(p_glm_wt_val, th_glm_wt["best_th"], df_val_imp$target == "Uncontrolled", df_val_imp$mask)
cat(sprintf("%-30s  %-8d  %.4f\n", "XGBoost slim",          length(feat_slim),      f1_xgb))
cat(sprintf("%-30s  %-8d  %.4f\n", "XGBoost slim plus",     length(feat_slim_plus), f1_xgb_p))
cat(sprintf("%-30s  %-8d  %.4f\n", "XGBoost SLIM NEW",      length(feat_slim_new),  f1_xgb_n))
cat(sprintf("%-30s  %-8d  %.4f\n", "GLM slim",              length(feat_slim),      f1_glm))
cat(sprintf("%-30s  %-8d  %.4f\n", "GLM+wt slim",           length(feat_slim),      f1_glm_wt))
cat(sprintf("%-30s  %-8s  %.4f\n", "(ref) XGBoost full",    "77",                   0.6236))
cat(sprintf("%-30s  %-8s  %.4f\n", "(ref) XGBoost no d2e",  "76",                   0.6217))


#### SAVE PREDICTIONS FOR SLIDE-MAKING ####
slim_preds <- df_val %>%
    select(id, target, mask) %>%
    mutate(
        prob_xgb_slim      = p_xgb_val,
        prob_xgb_slim_plus = p_xgb_p_val,
        prob_xgb_slim_new  = p_xgb_n_val,
        prob_glm_slim      = p_glm_val,
        prob_glm_wt_slim   = p_glm_wt_val,
        th_xgb_slim        = th_xgb["best_th"],
        th_xgb_slim_plus   = th_xgb_p["best_th"],
        th_xgb_slim_new    = th_xgb_n["best_th"],
        th_glm_slim        = th_glm["best_th"],
        th_glm_wt_slim     = th_glm_wt["best_th"]
    )
saveRDS(
    list(
        slim_preds           = slim_preds,
        slim_importance      = imp_slim,
        slim_plus_importance = imp_slim_p,
        slim_new_importance  = imp_slim_n,
        feat_slim            = feat_slim,
        feat_slim_plus       = feat_slim_plus,
        feat_slim_new        = feat_slim_new
    ),
    file = "./data/processed/slim_predictions.Rds"
)
cat("\nSaved slim predictions to data/processed/slim_predictions.Rds\n")
