###############################################################################
## GLM on Subhrajyoty's train/val split — using our EDA-informed features
## Goal: beat 65% F1
## Competition metric: F1-score
###############################################################################

# load packages and utility functions
library(tidyverse)
library(matrixStats)
source("./scripts/utils.R")

# load the clean dataset
train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")


create_feature_dat <- function(train_dat) {
    # -------------------------
    # Feature engineering
    # ----------------------

    df <- train_dat$measurements %>%
        filter(variable %in% paste0("a1c_", 1:5)) %>%
        pivot_wider(names_from = variable, values_from = c(time, value)) %>%
        mutate(
            a1c_latest = exec(coalesce, !!!syms(paste0("value_a1c_", 5:1))), # latest a1c value
            n_a1c = rowSums(!is.na(across(value_a1c_1:value_a1c_5))), # number of readings
            abs_change = abs(value_a1c_1 - value_a1c_2), # absolute change between first two readings
            a1c_rate = (value_a1c_2 - value_a1c_1) / pmax(time_a1c_2, 1) # rate of change (A1c per day) between first two readings
        ) %>%
        left_join(
            train_dat$basic %>%
                mutate(
                    gender_male = as.integer(gender == "M")
                ) %>%
                select(id, age, gender_male),
            by = "id"
        ) %>%
        left_join(
            train_dat$counts %>%
                mutate(
                    total_meds = insulin + metformin + sulfonylurea + glp1 + sglt2 + dpp4,
                    n_drug_classes = (insulin > 0) + (metformin > 0) + (sulfonylurea > 0) + (glp1 > 0) + (sglt2 > 0) + (dpp4 > 0)
                ) %>%
                select(
                    id, insulin, metformin, sulfonylurea, glp1, sglt2, dpp4,
                    total_meds, n_drug_classes,
                    ed_visits = ed_visit, pcp_visits = pcp_visit, admissions = admission
                ),
            by = "id"
        ) %>%
        left_join(
            train_dat$measurements %>%
                filter(variable %in% c("ldl", "hdl", "chol", "height", "weight", "a1c_2025")) %>%
                pivot_wider(names_from = variable, values_from = c(time, value)),
            by = "id"
        ) %>%
        mutate(
            days_to_eval = time_a1c_2025, # time when a1c_2025 will be collected
            a1c_weighted = case_when(
                !is.na(value_a1c_5) ~ {
                    w <- 0.5^(4:0)
                    w <- w / sum(w)
                    w[1] * value_a1c_1 + w[2] * value_a1c_2 + w[3] * value_a1c_3 + w[4] * value_a1c_4 + w[5] * value_a1c_5
                },
                !is.na(value_a1c_4) ~ {
                    w <- 0.5^(3:0)
                    w <- w / sum(w)
                    w[1] * value_a1c_1 + w[2] * value_a1c_2 + w[3] * value_a1c_3 + w[4] * value_a1c_4
                },
                !is.na(value_a1c_3) ~ 0.25 * value_a1c_1 + 0.25 * value_a1c_2 + 0.50 * value_a1c_3,
                !is.na(value_a1c_2) ~ 0.50 * value_a1c_1 + 0.50 * value_a1c_2,
                TRUE ~ value_a1c_1
            ),

            # a1c volatility
            a1c_sd = {
                mat <- pick(value_a1c_1:value_a1c_5)
                a1c_mean <- rowMeans(mat, na.rm = TRUE)
                ss <- rowSums((mat - a1c_mean)^2, na.rm = TRUE)
                if_else(n_a1c >= 3, sqrt(ss / (n_a1c - 1)), NA_real_)
            },
            a1c_range = {
                mat <- pick(value_a1c_1:value_a1c_5)
                a1c_max <- do.call(pmax, c(mat, na.rm = TRUE))
                a1c_min <- do.call(pmin, c(mat, na.rm = TRUE))
                if_else(n_a1c >= 2, a1c_max - a1c_min, NA_real_)
            },

            # add interactions
            high_a1c_insulin = as.integer(a1c_latest >= 7.0 & insulin > 0),
            a1c_x_meds = a1c_latest * total_meds,
            high_a1c_no_meds = as.integer(a1c_latest >= 7.0 & total_meds == 0),
            a1c_x_ndrug = a1c_latest * n_drug_classes,
            a1c_change = a1c_latest - value_a1c_1,
            a1c_per_drug = a1c_latest / pmax(n_drug_classes, 1),
            improving_on_meds = as.integer(a1c_latest < value_a1c_1 & total_meds > 0),
            worsening_on_meds = as.integer(a1c_latest > value_a1c_1 & total_meds > 0),
            stable_on_meds = as.integer(a1c_latest == value_a1c_1 & total_meds > 0),
            on_modern_drugs = as.integer(glp1 > 0 | sglt2 > 0), # (GLP-1/SGLT2 are newer, more effective)
            n_a1c_x_ndrug = n_a1c * n_drug_classes, # Monitoring × treatment intensity
            still_high_improving = as.integer(a1c_latest >= 7.0 & (a1c_latest - value_a1c_1) < 0),
            a1c_change_if_high = ifelse(a1c_latest >= 7.0, a1c_latest - value_a1c_1, 0)
        )

    # ---------------
    # Impute values and create missingness flags
    # ---------------
    flag_cols <- c(
        "abs_change", "a1c_sd", "a1c_range", "a1c_rate",
        "value_hdl", "value_ldl", "value_chol",
        "time_weight", "time_ldl", "time_hdl"
    )
    for (col in flag_cols) {
        df[[paste0(col, "_miss")]] <- as.integer(is.na(df[[col]]))
    }


    feature_cols <- c(
        "a1c_latest", "value_a1c_1", "a1c_weighted",
        "abs_change", "a1c_sd", "a1c_range", "a1c_rate",
        "n_a1c", "n_drug_classes", "total_meds",
        "sulfonylurea", "insulin", "metformin", "sglt2", "glp1", "dpp4",
        "ed_visits", "pcp_visits", "admissions",
        "age", "gender_male",
        "value_hdl", "value_ldl", "value_chol",
        "time_weight", "time_ldl", "time_hdl",
        "high_a1c_insulin", "a1c_x_meds",
        "high_a1c_no_meds", "a1c_x_ndrug",
        "a1c_change", "a1c_per_drug",
        "improving_on_meds", "worsening_on_meds", "stable_on_meds",
        "on_modern_drugs", "n_a1c_x_ndrug",
        "still_high_improving", "a1c_change_if_high"
    )
    setdiff(feature_cols, colnames(df))
    for (col in feature_cols) {
        med_val <- median(df[[col]], na.rm = TRUE)
        df[[col]] <- ifelse(is.na(df[[col]]), med_val, df[[col]])
    }

    # create target and mask variables
    df$target <- factor(ifelse(df$value_a1c_2025 > 0, "Uncontrolled", "Controlled"), levels = c("Controlled", "Uncontrolled"))
    df$mask <- !is.na(df$time_a1c_2025)

    return(df)
}

df_train_glm <- create_feature_dat(train_dat)
df_val_glm <- create_feature_dat(val_dat)

# ----------------
# Formula
# ----------------
f_glm <- target ~
    a1c_latest + value_a1c_1 + a1c_weighted + abs_change + a1c_sd + a1c_range + a1c_rate +
    n_a1c + n_drug_classes + total_meds +
    sulfonylurea + insulin + metformin + sglt2 + glp1 + dpp4 +
    ed_visits + pcp_visits + admissions +
    age + gender_male +
    value_hdl + value_ldl + value_chol +
    time_weight + time_ldl + time_hdl +
    high_a1c_insulin + a1c_x_meds +
    high_a1c_no_meds + a1c_x_ndrug +
    a1c_change + a1c_per_drug +
    improving_on_meds + worsening_on_meds + stable_on_meds +
    on_modern_drugs + n_a1c_x_ndrug +
    still_high_improving + a1c_change_if_high +
    abs_change_miss + a1c_sd_miss + a1c_range_miss + a1c_rate_miss +
    value_hdl_miss + value_ldl_miss + value_chol_miss +
    time_weight_miss + time_ldl_miss + time_hdl_miss


# --------------
# Fit basic glm
# F1 score: ~ 56%

m1 <- glm(f_glm, data = df_train_glm, family = binomial)
p1_train <- predict(m1, df_train_glm, type = "response")
p1_thresh <- best_f1_threshold(p1_train, df_train_glm$target == "Uncontrolled", df_train_glm$mask)
p1_thresh

p1_val <- predict(m1, df_val_glm, type = "response")
compute_metrics(p1_val > p1_thresh["best_th"], df_val_glm$target == "Uncontrolled", df_val_glm$mask)


# ----------
# Fit glm with class weights to handle imbalance
# F1 score: ~ 56% - 58%

wts <- ifelse(df_train_glm$target == "Uncontrolled", 1 - mean(df_train_glm$target == "Uncontrolled"), mean(df_train_glm$target == "Uncontrolled"))

m2 <- glm(f_glm, data = df_train_glm, family = binomial, weights = wts)
p2_train <- predict(m2, df_train_glm, type = "response")
p2_thresh <- best_f1_threshold(p2_train, df_train_glm$target == "Uncontrolled", df_train_glm$mask)
p2_thresh

p2_val <- predict(m2, df_val_glm, type = "response")
compute_metrics(p2_val > p2_thresh["best_th"], df_val_glm$target == "Uncontrolled", df_val_glm$mask)


# ------------------
# Fit glmnet elastic net (tuned via 5-fold cv on train)
# F1 score: ~ 60-61%

library(caret)
library(glmnet)

# create folds
set.seed(2441139)
folds_tr <- createFolds(df_train_glm$target, k = 5, returnTrain = TRUE)

f1_summary <- function(data, lev = NULL, model = NULL) {
    pos <- lev[2] # positive is the 2nd level
    obs_pos <- data$obs == pos
    probs <- data[, pos]

    best_f1_val <- -1
    for (th in seq(0.01, 0.99, 0.01)) {
        pred_pos <- probs >= th
        out <- compute_metrics(pred_pos, obs_pos, verbose = FALSE)
        best_f1_val <- max(best_f1_val, out$metrics["F1"])
    }
    c("best_F1" = best_f1_val)
}

ctrl_cv <- trainControl(
    method = "cv", index = folds_tr, classProbs = TRUE,
    summaryFunction = f1_summary, savePredictions = "final"
)

glmnet_grid <- expand.grid(
    alpha = seq(0, 1, by = 0.1),
    lambda = 10^seq(-4, 0, length = 30)
)

m3 <- train(f_glm,
    data = df_train_glm, method = "glmnet", family = "binomial",
    trControl = ctrl_cv, metric = "best_F1",
    tuneGrid = glmnet_grid,
    preProcess = c("center", "scale")
) # takes ~ 2 mins

p3_train <- predict(m3, df_train_glm, type = "prob")$Uncontrolled
p3_thresh <- best_f1_threshold(p3_train, df_train_glm$target == "Uncontrolled", df_train_glm$mask)
p3_thresh

p3_val <- predict(m3, df_val_glm, type = "prob")$Uncontrolled
compute_metrics(p3_val > p3_thresh["best_th"], df_val_glm$target == "Uncontrolled", df_val_glm$mask)
