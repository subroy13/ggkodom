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


# -------------------------
# Feature engineering
# ----------------------
create_feature_dat <- function(train_dat) {
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
                select(id, age, ),
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


    return(df)
}
