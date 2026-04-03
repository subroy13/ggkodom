###############################################################################
## Nonlinear models on Subhrajyoti's train/val split
## Decision Tree, GAM, GBM (xgboost)
## Reuses feature engineering from model_glm_split.R
###############################################################################

library(tidyverse)
library(rpart)
library(rpart.plot)
library(mgcv)
library(xgboost)

#### LOAD DATA & GET SPLIT IDS ####

feat <- read_csv("./data/raw/DM Features.csv", show_col_types = FALSE) %>% rename(id = 1)
ctrl <- read_csv("./data/raw/DM Control_2025.csv", show_col_types = FALSE) %>% rename(id = 1)

train_ids <- readRDS("./data/processed/train.Rds")$basic$id
val_ids   <- readRDS("./data/processed/val.Rds")$basic$id

df <- left_join(feat, ctrl, by = "id")
df <- df %>% mutate(
  target = factor(`a1c 2025 Uncontrolled`, levels = c(FALSE, TRUE),
                  labels = c("Ctrl", "Unctrl")),
  target_num = as.integer(`a1c 2025 Uncontrolled`)
)


###############################################################################
## FEATURE ENGINEERING (same as model_glm_split.R)
###############################################################################

df <- df %>% mutate(
  a1c_1 = `a1c 1-estimated result`,
  a1c_2 = `a1c 2-estimated result`,
  a1c_3 = `a1c 3-estimated result`,
  a1c_4 = `a1c 4-estimated result`,
  a1c_5 = `a1c 5-estimated result`,
  a1c_latest = coalesce(a1c_5, a1c_4, a1c_3, a1c_2, a1c_1),
  n_a1c = (!is.na(a1c_1)) + (!is.na(a1c_2)) + (!is.na(a1c_3)) +
          (!is.na(a1c_4)) + (!is.na(a1c_5)),
  abs_change = abs(a1c_1 - a1c_2),
  a1c_2_time = `a1c 2-collection date-time-days from reference`,
  a1c_rate = (a1c_2 - a1c_1) / pmax(a1c_2_time, 1),
  age = 2025 - `date of birth`,
  gender_male = as.integer(`gender at birth` == "MALE"),
  insulin      = `insulin orders-count`,
  metformin    = `metformin orders-count`,
  sulfonylurea = `sulfonylurea orders-count`,
  glp1         = `glp-1 orders-count`,
  sglt2        = `sglt2 orders-count`,
  dpp4         = `dpp4 orders-count`,
  total_meds   = insulin + metformin + sulfonylurea + glp1 + sglt2 + dpp4,
  n_drug_classes = (insulin > 0) + (metformin > 0) + (sulfonylurea > 0) +
                   (glp1 > 0) + (sglt2 > 0) + (dpp4 > 0),
  ed_visits   = `ed vist count-count`,
  pcp_visits  = `pcp visit count-count`,
  admissions  = `admission count-count`,
  hdl        = `hdl-estimated result`,
  ldl        = `ldl-estimated result`,
  total_chol = `total cholesterol-estimated result`,
  weight_timing = `weight-measurement date-time-days from reference`,
  ldl_timing    = `ldl-collection date-time-days from reference`,
  hdl_timing    = `hdl-collection date-time-days from reference`,
  days_to_eval = `a1c 2025-collection date-time-days from reference`
)

df <- df %>% mutate(
  a1c_weighted = case_when(
    !is.na(a1c_5) ~ { w <- 0.5^(4:0); w <- w/sum(w); w[1]*a1c_1+w[2]*a1c_2+w[3]*a1c_3+w[4]*a1c_4+w[5]*a1c_5 },
    !is.na(a1c_4) ~ { w <- 0.5^(3:0); w <- w/sum(w); w[1]*a1c_1+w[2]*a1c_2+w[3]*a1c_3+w[4]*a1c_4 },
    !is.na(a1c_3) ~ 0.25*a1c_1 + 0.25*a1c_2 + 0.50*a1c_3,
    !is.na(a1c_2) ~ 0.50*a1c_1 + 0.50*a1c_2,
    TRUE           ~ a1c_1
  )
)

a1c_mat  <- as.matrix(df[, c("a1c_1","a1c_2","a1c_3","a1c_4","a1c_5")])
a1c_n    <- rowSums(!is.na(a1c_mat))
a1c_mean <- rowMeans(a1c_mat, na.rm = TRUE)
a1c_ss   <- rowSums((a1c_mat - a1c_mean)^2, na.rm = TRUE)
df$a1c_sd <- ifelse(a1c_n >= 3, sqrt(a1c_ss / (a1c_n - 1)), NA_real_)

a1c_max <- do.call(pmax, c(as.data.frame(a1c_mat), na.rm = TRUE))
a1c_min <- do.call(pmin, c(as.data.frame(a1c_mat), na.rm = TRUE))
df$a1c_range <- ifelse(a1c_n >= 2, a1c_max - a1c_min, NA_real_)

df <- df %>% mutate(
  high_a1c_insulin  = as.integer(a1c_latest >= 7.0 & insulin > 0),
  a1c_x_meds        = a1c_latest * total_meds,
  high_a1c_no_meds  = as.integer(a1c_latest >= 7.0 & total_meds == 0),
  a1c_x_ndrug       = a1c_latest * n_drug_classes,
  a1c_change         = a1c_latest - a1c_1,
  a1c_per_drug       = a1c_latest / pmax(n_drug_classes, 1),
  improving_on_meds  = as.integer(a1c_latest < a1c_1 & total_meds > 0),
  worsening_on_meds  = as.integer(a1c_latest > a1c_1 & total_meds > 0),
  stable_on_meds     = as.integer(a1c_latest == a1c_1 & total_meds > 0),
  on_modern_drugs    = as.integer(glp1 > 0 | sglt2 > 0),
  n_a1c_x_ndrug      = n_a1c * n_drug_classes,
  still_high_improving = as.integer(a1c_latest >= 7.0 & (a1c_latest - a1c_1) < 0),
  a1c_change_if_high   = ifelse(a1c_latest >= 7.0, a1c_latest - a1c_1, 0)
)


###############################################################################
## SPLIT — returned patients only
###############################################################################

df_train <- df %>% filter(id %in% train_ids, !is.na(days_to_eval))
df_val   <- df %>% filter(id %in% val_ids,   !is.na(days_to_eval))

cat("Train: n =", nrow(df_train), "| Unctrl =", sum(df_train$target == "Unctrl"),
    "(", round(mean(df_train$target == "Unctrl")*100, 1), "%)\n")
cat("Val:   n =", nrow(df_val),   "| Unctrl =", sum(df_val$target == "Unctrl"),
    "(", round(mean(df_val$target == "Unctrl")*100, 1), "%)\n\n")


###############################################################################
## FEATURE LIST (for tree/GBM — no imputation needed, they handle NAs)
###############################################################################

feat_cols <- c("a1c_latest", "a1c_1", "a1c_weighted", "abs_change", "a1c_sd",
               "a1c_range", "a1c_rate", "a1c_change", "a1c_change_if_high",
               "n_a1c", "n_drug_classes", "total_meds",
               "sulfonylurea", "insulin", "metformin", "sglt2", "glp1", "dpp4",
               "ed_visits", "pcp_visits", "admissions",
               "age", "gender_male",
               "hdl", "ldl", "total_chol",
               "weight_timing", "ldl_timing", "hdl_timing",
               "high_a1c_insulin", "a1c_x_meds",
               "high_a1c_no_meds", "a1c_x_ndrug",
               "a1c_per_drug", "improving_on_meds", "worsening_on_meds",
               "stable_on_meds", "on_modern_drugs", "n_a1c_x_ndrug",
               "still_high_improving")


###############################################################################
## HELPER: best F1 (both positive-class and macro)
###############################################################################

best_f1 <- function(probs, obs_factor, pos_label = "Unctrl") {
  obs_pos <- obs_factor == pos_label
  best <- list(f1_pos = 0, f1_macro = 0, th = 0.5, prec = 0, recall = 0)
  for (th in seq(0.03, 0.60, 0.005)) {
    pred_pos <- probs >= th
    tp <- sum(pred_pos & obs_pos); fp <- sum(pred_pos & !obs_pos)
    fn <- sum(!pred_pos & obs_pos); tn <- sum(!pred_pos & !obs_pos)
    denom_pos <- 2*tp + fp + fn
    f1_pos <- if (denom_pos > 0) 2*tp/denom_pos else 0
    denom_neg <- 2*tn + fn + fp
    f1_neg <- if (denom_neg > 0) 2*tn/denom_neg else 0
    f1_macro <- (f1_pos + f1_neg) / 2
    if (f1_pos > best$f1_pos) {
      best <- list(f1_pos = f1_pos, f1_macro = f1_macro, th = th,
                   prec = tp/max(tp+fp,1), recall = tp/max(tp+fn,1))
    }
  }
  best
}

report <- function(name, probs_tr, probs_val, target_tr, target_val) {
  rt <- best_f1(probs_tr, target_tr)
  rv <- best_f1(probs_val, target_val)
  cat(sprintf("  Train F1_pos=%.3f  F1_macro=%.3f (th=%.3f)\n",
              rt$f1_pos, rt$f1_macro, rt$th))
  cat(sprintf("  Val   F1_pos=%.3f  F1_macro=%.3f (th=%.3f)\n",
              rv$f1_pos, rv$f1_macro, rv$th))
  list(name = name, train = rt, val = rv)
}


###############################################################################
## MODEL 1: DECISION TREE (rpart)
###############################################################################

cat("====== MODEL 1: Decision Tree (rpart) ======\n")
dt_formula <- as.formula(paste("target ~", paste(feat_cols, collapse = " + ")))

m1 <- rpart(dt_formula, data = df_train, method = "class",
            control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 50))

cat("  Tree size:", sum(m1$frame$var == "<leaf>"), "leaves\n")

p1_tr  <- predict(m1, df_train, type = "prob")[, "Unctrl"]
p1_val <- predict(m1, df_val,   type = "prob")[, "Unctrl"]
r1 <- report("Decision Tree", p1_tr, p1_val, df_train$target, df_val$target)
flush.console()

## Save tree plot
pdf("./figures/tree_plot.pdf", width = 14, height = 8)
rpart.plot(m1, type = 4, extra = 104, fallen.leaves = TRUE,
           main = "Decision Tree: Uncontrolled A1c Prediction")
dev.off()
cat("  Tree plot saved to tree_plot.pdf\n\n")


###############################################################################
## MODEL 2: GAM (mgcv) — splines on continuous A1c features, linear rest
###############################################################################

cat("====== MODEL 2: GAM ======\n")

## GAM needs imputed data (no NA handling)
impute_median <- function(data, cols) {
  for (col in cols) {
    med_val <- median(data[[col]], na.rm = TRUE)
    data[[col]] <- ifelse(is.na(data[[col]]), med_val, data[[col]])
  }
  data
}

df_train_gam <- impute_median(df_train, feat_cols)
df_val_gam   <- impute_median(df_val,   feat_cols)

m2 <- gam(target_num ~
             s(a1c_weighted, k = 8) + s(a1c_latest, k = 8) + s(a1c_1, k = 8) +
             s(a1c_per_drug, k = 6) + s(a1c_change_if_high, k = 6) +
             n_a1c + n_drug_classes + total_meds +
             sulfonylurea + insulin + metformin + sglt2 + glp1 +
             ed_visits + age + gender_male +
             high_a1c_insulin + a1c_x_meds +
             high_a1c_no_meds + worsening_on_meds + improving_on_meds +
             stable_on_meds + still_high_improving + on_modern_drugs,
           family = binomial, data = df_train_gam)

p2_tr  <- predict(m2, df_train_gam, type = "response")
p2_val <- predict(m2, df_val_gam,   type = "response")
r2 <- report("GAM", p2_tr, p2_val, df_train_gam$target, df_val_gam$target)
flush.console()

## Save GAM smooth plots
pdf("./figures/gam_plots.pdf", width = 10, height = 8)
par(mfrow = c(2, 3))
plot(m2, shade = TRUE, scale = 0, pages = 0)
dev.off()
cat("  GAM plots saved to gam_plots.pdf\n\n")


###############################################################################
## MODEL 3: XGBoost — simple, fast
###############################################################################

cat("====== MODEL 3: XGBoost ======\n")

## xgboost handles NAs natively
xgb_train <- xgb.DMatrix(
  data = as.matrix(df_train[, feat_cols]),
  label = df_train$target_num
)
xgb_val <- xgb.DMatrix(
  data = as.matrix(df_val[, feat_cols]),
  label = df_val$target_num
)

params <- list(
  objective   = "binary:logistic",
  eval_metric = "logloss",
  max_depth   = 4,
  eta         = 0.05,
  subsample   = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 10
)

m3 <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 500,
  watchlist = list(train = xgb_train, val = xgb_val),
  early_stopping_rounds = 30,
  print_every_n = 50,
  verbose = 1
)

cat("  Best iteration:", m3$best_iteration, "\n")

p3_tr  <- predict(m3, xgb_train)
p3_val <- predict(m3, xgb_val)
r3 <- report("XGBoost", p3_tr, p3_val, df_train$target, df_val$target)
flush.console()


## Variable importance
cat("\n====== XGBoost Variable Importance (top 20) ======\n")
imp <- xgb.importance(feature_names = feat_cols, model = m3)
print(head(imp, 20))


###############################################################################
## SUMMARY
###############################################################################

cat("\n\n====== SUMMARY ======\n")
cat(sprintf("%-25s  Train_F1pos  Val_F1pos  Train_F1mac  Val_F1mac\n", "Model"))
cat(sprintf("%-25s  ----------  ---------  ----------   ---------\n", ""))

all_results <- list(r1, r2, r3)
for (r in all_results) {
  cat(sprintf("%-25s  %.1f%%        %.1f%%      %.1f%%        %.1f%%\n",
              r$name,
              r$train$f1_pos*100, r$val$f1_pos*100,
              r$train$f1_macro*100, r$val$f1_macro*100))
}
cat(sprintf("%-25s  60.4%%       61.7%%      74.7%%        75.7%%\n",
            "GLM (glmnet, prev best)"))
