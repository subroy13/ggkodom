###############################################################################
## GLM on Subhrajyoti's train/val split — using our EDA-informed features
## Goal: beat Subhrajyoti's 65% F1 (simple logistic on train)
## Competition metric: F1-score
###############################################################################

library(tidyverse)
library(caret)
library(pROC)
library(PRROC)

#### LOAD DATA & GET SPLIT IDS ####

feat <- read_csv("./data/raw/DM Features.csv", show_col_types = FALSE) %>% rename(id = 1)
ctrl <- read_csv("./data/raw/DM Control_2025.csv", show_col_types = FALSE) %>% rename(id = 1)

train_ids <- readRDS("./data/processed/train.Rds")$basic$id
val_ids   <- readRDS("./data/processed/val.Rds")$basic$id

df <- left_join(feat, ctrl, by = "id")
df <- df %>% mutate(
  target = factor(`a1c 2025 Uncontrolled`, levels = c(FALSE, TRUE),
                  labels = c("Ctrl", "Unctrl"))
)


###############################################################################
## FEATURE ENGINEERING (matching model_glm.R + extras)
###############################################################################

df <- df %>% mutate(
  ## A1c readings
  a1c_1 = `a1c 1-estimated result`,
  a1c_2 = `a1c 2-estimated result`,
  a1c_3 = `a1c 3-estimated result`,
  a1c_4 = `a1c 4-estimated result`,
  a1c_5 = `a1c 5-estimated result`,

  ## Latest A1c (most recent available)
  a1c_latest = coalesce(a1c_5, a1c_4, a1c_3, a1c_2, a1c_1),

  ## Number of A1c readings
  n_a1c = (!is.na(a1c_1)) + (!is.na(a1c_2)) + (!is.na(a1c_3)) +
          (!is.na(a1c_4)) + (!is.na(a1c_5)),

  ## Absolute change between first two readings
  abs_change = abs(a1c_1 - a1c_2),

  ## A1c temporal features
  a1c_2_time = `a1c 2-collection date-time-days from reference`,
  a1c_3_time = `a1c 3-collection date-time-days from reference`,
  a1c_4_time = `a1c 4-collection date-time-days from reference`,
  a1c_5_time = `a1c 5-collection date-time-days from reference`,

  ## Rate of change (A1c per day) between first two readings
  a1c_rate = (a1c_2 - a1c_1) / pmax(a1c_2_time, 1),

  ## Demographics
  age = 2025 - `date of birth`,
  gender_male = as.integer(`gender at birth` == "MALE"),

  ## Medications
  insulin      = `insulin orders-count`,
  metformin    = `metformin orders-count`,
  sulfonylurea = `sulfonylurea orders-count`,
  glp1         = `glp-1 orders-count`,
  sglt2        = `sglt2 orders-count`,
  dpp4         = `dpp4 orders-count`,
  total_meds   = insulin + metformin + sulfonylurea + glp1 + sglt2 + dpp4,
  n_drug_classes = (insulin > 0) + (metformin > 0) + (sulfonylurea > 0) +
                   (glp1 > 0) + (sglt2 > 0) + (dpp4 > 0),

  ## Healthcare utilization
  ed_visits   = `ed vist count-count`,
  pcp_visits  = `pcp visit count-count`,
  admissions  = `admission count-count`,

  ## Lipids
  hdl        = `hdl-estimated result`,
  ldl        = `ldl-estimated result`,
  total_chol = `total cholesterol-estimated result`,

  ## Timing (days from reference)
  weight_timing = `weight-measurement date-time-days from reference`,
  ldl_timing    = `ldl-collection date-time-days from reference`,
  hdl_timing    = `hdl-collection date-time-days from reference`,

  ## Future evaluation date
  days_to_eval = `a1c 2025-collection date-time-days from reference`
)


## EWMA-0.5 weighted mean — all available readings (up to 5)
df <- df %>% mutate(
  a1c_weighted = case_when(
    !is.na(a1c_5) ~ {
      w <- 0.5^(4:0); w <- w / sum(w)
      w[1]*a1c_1 + w[2]*a1c_2 + w[3]*a1c_3 + w[4]*a1c_4 + w[5]*a1c_5
    },
    !is.na(a1c_4) ~ {
      w <- 0.5^(3:0); w <- w / sum(w)
      w[1]*a1c_1 + w[2]*a1c_2 + w[3]*a1c_3 + w[4]*a1c_4
    },
    !is.na(a1c_3) ~ 0.25 * a1c_1 + 0.25 * a1c_2 + 0.50 * a1c_3,
    !is.na(a1c_2) ~ 0.50 * a1c_1 + 0.50 * a1c_2,
    TRUE           ~ a1c_1
  )
)

## A1c volatility — sample SD across all available readings (3+ required)
a1c_mat  <- as.matrix(df[, c("a1c_1", "a1c_2", "a1c_3", "a1c_4", "a1c_5")])
a1c_n    <- rowSums(!is.na(a1c_mat))
a1c_mean <- rowMeans(a1c_mat, na.rm = TRUE)
a1c_ss   <- rowSums((a1c_mat - a1c_mean)^2, na.rm = TRUE)
df$a1c_sd <- ifelse(a1c_n >= 3, sqrt(a1c_ss / (a1c_n - 1)), NA_real_)

## A1c range (max - min across all readings)
a1c_max <- do.call(pmax, c(as.data.frame(a1c_mat), na.rm = TRUE))
a1c_min <- do.call(pmin, c(as.data.frame(a1c_mat), na.rm = TRUE))
df$a1c_range <- ifelse(a1c_n >= 2, a1c_max - a1c_min, NA_real_)

## Interactions (original)
df <- df %>% mutate(
  high_a1c_insulin = as.integer(a1c_latest >= 7.0 & insulin > 0),
  a1c_x_meds       = a1c_latest * total_meds,
  high_a1c_no_meds  = as.integer(a1c_latest >= 7.0 & total_meds == 0),
  a1c_x_ndrug       = a1c_latest * n_drug_classes
)

## Treatment-effect features
df <- df %>% mutate(
  ## Signed trajectory: positive = worsening, negative = improving
  a1c_change = a1c_latest - a1c_1,

  ## Treatment resistance: high A1c despite many drugs
  a1c_per_drug = a1c_latest / pmax(n_drug_classes, 1),

  ## Trajectory conditional on treatment
  improving_on_meds = as.integer(a1c_latest < a1c_1 & total_meds > 0),
  worsening_on_meds = as.integer(a1c_latest > a1c_1 & total_meds > 0),
  stable_on_meds    = as.integer(a1c_latest == a1c_1 & total_meds > 0),

  ## Modern vs older drugs (GLP-1/SGLT2 are newer, more effective)
  on_modern_drugs = as.integer(glp1 > 0 | sglt2 > 0),

  ## Monitoring × treatment intensity
  n_a1c_x_ndrug = n_a1c * n_drug_classes,

  ## "Medicated but not yet controlled" — high variability + improving + still high
  still_high_improving = as.integer(a1c_latest >= 7.0 & (a1c_latest - a1c_1) < 0),
  a1c_change_if_high   = ifelse(a1c_latest >= 7.0, a1c_latest - a1c_1, 0)
)


###############################################################################
## FILTER TO TRAIN IDS WHO RETURNED (have future eval date)
###############################################################################

df_train <- df %>% filter(id %in% train_ids, !is.na(days_to_eval))

cat("Train (returned only): n =", nrow(df_train),
    "| Unctrl =", sum(df_train$target == "Unctrl"),
    "(", round(mean(df_train$target == "Unctrl") * 100, 1), "%)\n")


###############################################################################
## IMPUTATION (median + missingness flags)
###############################################################################

impute_and_flag <- function(data) {
  flag_cols <- c("abs_change", "a1c_sd", "a1c_range", "a1c_rate",
                 "hdl", "ldl", "total_chol",
                 "weight_timing", "ldl_timing", "hdl_timing")

  for (col in flag_cols) {
    data[[paste0(col, "_miss")]] <- as.integer(is.na(data[[col]]))
  }

  feature_cols <- c("a1c_latest", "a1c_1", "a1c_weighted",
                    "abs_change", "a1c_sd", "a1c_range", "a1c_rate",
                    "n_a1c", "n_drug_classes", "total_meds",
                    "sulfonylurea", "insulin", "metformin", "sglt2", "glp1", "dpp4",
                    "ed_visits", "pcp_visits", "admissions",
                    "age", "gender_male",
                    "hdl", "ldl", "total_chol",
                    "weight_timing", "ldl_timing", "hdl_timing",
                    "high_a1c_insulin", "a1c_x_meds",
                    "high_a1c_no_meds", "a1c_x_ndrug",
                    "a1c_change", "a1c_per_drug",
                    "improving_on_meds", "worsening_on_meds", "stable_on_meds",
                    "on_modern_drugs", "n_a1c_x_ndrug",
                    "still_high_improving", "a1c_change_if_high")

  for (col in feature_cols) {
    med_val <- median(data[[col]], na.rm = TRUE)
    data[[col]] <- ifelse(is.na(data[[col]]), med_val, data[[col]])
  }

  data
}

df_train_glm <- impute_and_flag(df_train)


###############################################################################
## FORMULA
###############################################################################

f_glm <- target ~
  a1c_latest + a1c_1 + a1c_weighted + abs_change + a1c_sd + a1c_range + a1c_rate +
  n_a1c + n_drug_classes + total_meds +
  sulfonylurea + insulin + metformin + sglt2 + glp1 + dpp4 +
  ed_visits + pcp_visits + admissions +
  age + gender_male +
  hdl + ldl + total_chol +
  weight_timing + ldl_timing + hdl_timing +
  high_a1c_insulin + a1c_x_meds +
  high_a1c_no_meds + a1c_x_ndrug +
  a1c_change + a1c_per_drug +
  improving_on_meds + worsening_on_meds + stable_on_meds +
  on_modern_drugs + n_a1c_x_ndrug +
  still_high_improving + a1c_change_if_high +
  abs_change_miss + a1c_sd_miss + a1c_range_miss + a1c_rate_miss +
  hdl_miss + ldl_miss + total_chol_miss +
  weight_timing_miss + ldl_timing_miss + hdl_timing_miss


###############################################################################
## HELPER: compute best F1 at optimal threshold (both positive-class and macro)
###############################################################################

best_f1 <- function(probs, obs_factor, pos_label = "Unctrl") {
  obs_pos <- obs_factor == pos_label
  best <- list(f1_pos = 0, f1_macro = 0, th = 0.5, prec = 0, recall = 0)
  for (th in seq(0.03, 0.60, 0.005)) {
    pred_pos <- probs >= th
    tp <- sum(pred_pos & obs_pos)
    fp <- sum(pred_pos & !obs_pos)
    fn <- sum(!pred_pos & obs_pos)
    tn <- sum(!pred_pos & !obs_pos)

    ## Positive-class F1
    denom_pos <- 2 * tp + fp + fn
    f1_pos <- if (denom_pos > 0) 2 * tp / denom_pos else 0

    ## Negative-class F1
    denom_neg <- 2 * tn + fn + fp
    f1_neg <- if (denom_neg > 0) 2 * tn / denom_neg else 0

    ## Macro F1 (Subhrajyoti's metric)
    f1_macro <- (f1_pos + f1_neg) / 2

    if (f1_pos > best$f1_pos) {
      prec <- tp / max(tp + fp, 1)
      rec  <- tp / max(tp + fn, 1)
      best <- list(f1_pos = f1_pos, f1_macro = f1_macro,
                   th = th, prec = prec, recall = rec)
    }
  }
  best
}


###############################################################################
## MODEL 1: PLAIN GLM
###############################################################################

cat("\n====== MODEL 1: GLM (our features) ======\n")
m1 <- glm(f_glm, data = df_train_glm, family = binomial)

p1_train <- predict(m1, df_train_glm, type = "response")
r1_train <- best_f1(p1_train, df_train_glm$target)

cat(sprintf("  Train F1_pos=%.3f  F1_macro=%.3f (prec=%.3f rec=%.3f th=%.3f)\n",
            r1_train$f1_pos, r1_train$f1_macro, r1_train$prec, r1_train$recall, r1_train$th))


###############################################################################
## MODEL 2: GLM + CLASS WEIGHTS
###############################################################################

cat("\n====== MODEL 2: GLM + class weights ======\n")
wts <- ifelse(df_train_glm$target == "Unctrl",
              (1 - mean(df_train_glm$target == "Unctrl")) /
              mean(df_train_glm$target == "Unctrl"), 1)

m2 <- glm(f_glm, data = df_train_glm, family = binomial, weights = wts)

p2_train <- predict(m2, df_train_glm, type = "response")
r2_train <- best_f1(p2_train, df_train_glm$target)

cat(sprintf("  Train F1_pos=%.3f  F1_macro=%.3f (prec=%.3f rec=%.3f th=%.3f)\n",
            r2_train$f1_pos, r2_train$f1_macro, r2_train$prec, r2_train$recall, r2_train$th))


###############################################################################
## MODEL 3: glmnet elastic net (tuned via 5-fold CV on train)
###############################################################################

cat("\n====== MODEL 3: glmnet elastic net (CV-tuned) ======\n")

set.seed(2441139)
folds_tr <- createFolds(df_train_glm$target, k = 5, returnTrain = TRUE)

f1_summary <- function(data, lev = NULL, model = NULL) {
  pos <- lev[2]
  obs_pos <- data$obs == pos
  probs   <- data[, pos]
  best_f1_val <- -1
  for (th in seq(0.03, 0.60, 0.005)) {
    pred_pos <- probs >= th
    tp <- sum(pred_pos & obs_pos)
    fp <- sum(pred_pos & !obs_pos)
    fn <- sum(!pred_pos & obs_pos)
    denom <- 2 * tp + fp + fn
    if (denom > 0) {
      f1 <- 2 * tp / denom
      if (f1 > best_f1_val) best_f1_val <- f1
    }
  }
  c(best_F1 = best_f1_val)
}

ctrl_cv <- trainControl(method = "cv", index = folds_tr, classProbs = TRUE,
                        summaryFunction = f1_summary, savePredictions = "final")

glmnet_grid <- expand.grid(alpha = seq(0, 1, by = 0.1),
                           lambda = 10^seq(-4, 0, length = 30))

m3 <- train(f_glm, data = df_train_glm, method = "glmnet", family = "binomial",
            trControl = ctrl_cv, metric = "best_F1",
            tuneGrid = glmnet_grid,
            preProcess = c("center", "scale"))

cat("  Best tune: alpha =", m3$bestTune$alpha, " lambda =", m3$bestTune$lambda, "\n")

p3_train <- predict(m3, df_train_glm, type = "prob")$Unctrl
r3_train <- best_f1(p3_train, df_train_glm$target)

cat(sprintf("  Train F1_pos=%.3f  F1_macro=%.3f (prec=%.3f rec=%.3f th=%.3f)\n",
            r3_train$f1_pos, r3_train$f1_macro, r3_train$prec, r3_train$recall, r3_train$th))


###############################################################################
## SUMMARY
###############################################################################

cat("\n\n====== SUMMARY (train-on-train, same as Subhrajyoti's setup) ======\n")
cat(sprintf("%-30s  F1_pos   F1_macro\n", "Model"))
cat(sprintf("%-30s  ------   --------\n", ""))
cat(sprintf("%-30s  ???      %.1f%%\n", "Subhrajyoti baseline (macro)", 65.0))
cat(sprintf("%-30s  %.1f%%    %.1f%%\n", "M1: GLM (our features)",
            r1_train$f1_pos * 100, r1_train$f1_macro * 100))
cat(sprintf("%-30s  %.1f%%    %.1f%%\n", "M2: GLM + class weights",
            r2_train$f1_pos * 100, r2_train$f1_macro * 100))
cat(sprintf("%-30s  %.1f%%    %.1f%%\n", "M3: glmnet elastic net",
            r3_train$f1_pos * 100, r3_train$f1_macro * 100))


###############################################################################
## VARIABLE IMPORTANCE (best model)
###############################################################################

cat("\n\n====== MODEL 1 (plain GLM) RAW COEFFICIENTS (top 20) ======\n")
coef_df <- data.frame(feature = names(coef(m1)), coef = as.numeric(coef(m1)))
coef_df <- coef_df[!is.na(coef_df$coef) & coef_df$feature != "(Intercept)", ]
coef_df$abs_coef <- abs(coef_df$coef)
print(coef_df[order(-coef_df$abs_coef), ][1:20, c("feature", "coef")], row.names = FALSE)

## Standardized coefficients for M1: multiply raw coef by SD of each feature
## so coefficients are comparable across features (effect of 1-SD change)
cat("\n====== MODEL 1 (plain GLM) STANDARDIZED COEFFICIENTS (top 20) ======\n")
feat_names <- coef_df$feature
feat_sds <- sapply(feat_names, function(f) sd(df_train_glm[[f]], na.rm = TRUE))
std_coef_df <- data.frame(
  feature = feat_names,
  raw_coef = coef_df$coef,
  sd = feat_sds,
  std_coef = coef_df$coef * feat_sds
)
std_coef_df$abs_std <- abs(std_coef_df$std_coef)
print(std_coef_df[order(-std_coef_df$abs_std), ][1:20,
      c("feature", "std_coef", "raw_coef")], row.names = FALSE)

cat("\n====== MODEL 3 (glmnet) ALL COEFFICIENTS (standardized) ======\n")
coefs3 <- coef(m3$finalModel, m3$bestTune$lambda)
coef3_df <- data.frame(feature = rownames(coefs3), coef = as.numeric(coefs3))
coef3_df <- coef3_df[coef3_df$feature != "(Intercept)", ]
coef3_df <- coef3_df[order(-abs(coef3_df$coef)), ]
print(coef3_df, row.names = FALSE)


###############################################################################
## VALIDATION SET EVALUATION
###############################################################################

cat("\n\n====== VALIDATION SET ======\n")

df_val <- df %>% filter(id %in% val_ids, !is.na(days_to_eval))
df_val_glm <- impute_and_flag(df_val)

cat("Val (returned only): n =", nrow(df_val),
    "| Unctrl =", sum(df_val$target == "Unctrl"),
    "(", round(mean(df_val$target == "Unctrl") * 100, 1), "%)\n\n")

## Predict on val with each model
p1_val <- predict(m1, df_val_glm, type = "response")
p2_val <- predict(m2, df_val_glm, type = "response")
p3_val <- predict(m3, df_val_glm, type = "prob")$Unctrl

r1_val <- best_f1(p1_val, df_val_glm$target)
r2_val <- best_f1(p2_val, df_val_glm$target)
r3_val <- best_f1(p3_val, df_val_glm$target)

cat(sprintf("%-30s  F1_pos   F1_macro  (train F1_pos / F1_macro)\n", "Model"))
cat(sprintf("%-30s  ------   --------  -------------------------\n", ""))
cat(sprintf("%-30s  %.1f%%    %.1f%%     (%.1f%% / %.1f%%)\n", "M1: GLM",
            r1_val$f1_pos*100, r1_val$f1_macro*100, r1_train$f1_pos*100, r1_train$f1_macro*100))
cat(sprintf("%-30s  %.1f%%    %.1f%%     (%.1f%% / %.1f%%)\n", "M2: GLM + class wt",
            r2_val$f1_pos*100, r2_val$f1_macro*100, r2_train$f1_pos*100, r2_train$f1_macro*100))
cat(sprintf("%-30s  %.1f%%    %.1f%%     (%.1f%% / %.1f%%)\n", "M3: glmnet enet",
            r3_val$f1_pos*100, r3_val$f1_macro*100, r3_train$f1_pos*100, r3_train$f1_macro*100))
