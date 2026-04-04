# ============
# This script contains some very basic baseline models
# ===========

# load packages and datasets
library(tidyverse)
source("./scripts/utils.R")


# load the clean dataset
train_dat <- readRDS("./data/processed/train.Rds")
val_dat <- readRDS("./data/processed/val.Rds")
test_dat <- readRDS("./data/processed/test.Rds")

total_dat <- list(
    "train" = train_dat,
    "validation" = val_dat,
    "test" = test_dat
)

# --------------------------------
# Baseline 1
# Algo: Look at latest a1c values, if > 7.0, then uncontrolled
# F1 score: ~ 55%
get_pred_df_base1 <- function(dat) {
    dat$measurements %>%
        filter(variable %in% paste0("a1c_", 1:5)) %>%
        drop_na() %>%
        arrange(id, time) %>%
        group_by(id) %>%
        slice_tail(n = 1) %>% # the last row is indeed the latest a1c value
        ungroup() %>%
        mutate(
            a1c_latest = value,
            a1c_pred = (value > 8.0)
        ) %>%
        left_join(dat$measurements %>% filter(variable == "a1c_2025"), by = "id") %>%
        mutate(
            a1c_latest,
            a1c_pred,
            a1c_2025 = value.y,
            a1c_2025_time = time.y,
            a1c_2025_valid = !is.na(a1c_2025_time) # valid columns to check
        )
}

compute_metrics_on_split(
    total_dat,
    get_pred_df_base1,
    true_column = "a1c_2025",
    pred_column = "a1c_pred",
    mask_column = "a1c_2025_valid"
)


# ---------------
# Baseline 2
# Algo: simple glm
# F1 score ~ 60%
df_train <- get_pred_df_base1(train_dat)
df_val <- get_pred_df_base1(val_dat)

m1 <- glm(a1c_2025 ~ a1c_latest, data = df_train)
p1_train <- predict(m1, df_train, type = "response")
p1_thresh <- best_f1_threshold(p1_train, df_train$a1c_2025 == 1, df_train$a1c_2025_valid)
p1_thresh

p1_val <- predict(m1, df_val, type = "response")
compute_metrics(p1_val > p1_thresh["best_th"], df_val$a1c_2025 == 1, df_val$a1c_2025_valid)
