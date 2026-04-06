# Subrata da's Feature engineering
library(tidyverse)
library(matrixStats)

# -------------------------
# Feature engineering
# ----------------------
create_features <- function(train_dat) {
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
                    gender_male = as.integer(gender == "M"),
                    race_black = as.integer(race == "Black"),
                    race_white = as.integer(race == "White"),
                    race_asian = as.integer(race == "Asian"),
                    ethnicity_hl = as.integer(ethnicity == "HL")
                ) %>%
                mutate(across(c(race_black, race_white, race_asian, ethnicity_hl), ~replace_na(., 0L))) %>%
                select(id, age, gender_male, adi_state, adi_nation, race_black, race_white, race_asian, ethnicity_hl),
            by = "id"
        ) %>%
        left_join(
            train_dat$counts %>%
                mutate(
                    total_meds = insulin + metformin + sulfonylurea + glp1 + sglt2 + dpp4,
                    n_drug_classes = (insulin > 0) + (metformin > 0) + (sulfonylurea > 0) + (glp1 > 0) + (sglt2 > 0) + (dpp4 > 0)
                ) %>%
                select(
                    id, cad, copd,
                    insulin, metformin, sulfonylurea, glp1, sglt2, dpp4,
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

            # time of latest a1c reading & gap to 2025 eval
            time_latest_a1c = exec(coalesce, !!!syms(paste0("time_a1c_", 5:1))),
            time_gap = days_to_eval - time_latest_a1c, # days between last reading and 2025 eval

            # sudden change features — detect likely medication intervention
            # consecutive changes: a1c_{j+1} - a1c_j for each pair
            delta_12 = value_a1c_2 - value_a1c_1,
            delta_23 = value_a1c_3 - value_a1c_2,
            delta_34 = value_a1c_4 - value_a1c_3,
            delta_45 = value_a1c_5 - value_a1c_4,
            # max single-step drop (most negative = biggest improvement)
            max_drop = {
                mat <- pick(delta_12, delta_23, delta_34, delta_45)
                do.call(pmin, c(mat, na.rm = TRUE))
            },
            # max single-step rise (biggest worsening)
            max_rise = {
                mat <- pick(delta_12, delta_23, delta_34, delta_45)
                do.call(pmax, c(mat, na.rm = TRUE))
            },
            # was there a sudden drop (>1.5 points) in a medicated patient? likely treatment effect
            has_sudden_drop = as.integer(!is.na(max_drop) & max_drop < -1.5 & total_meds > 0),
            # was there a sudden rise (>1.5 points)? treatment failure or non-adherence
            has_sudden_rise = as.integer(!is.na(max_rise) & max_rise > 1.5),

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

            #### TIER 2: Regime-dependent features ####

            # fraction of readings above 7 (proxy for chronic uncontrol)
            frac_above_7 = {
                mat <- pick(value_a1c_1:value_a1c_5)
                rowSums(mat >= 7, na.rm = TRUE) / n_a1c
            },

            # number of 7.0 crossings (instability around clinical threshold)
            n_crossings_7 = {
                c12 <- as.integer(!is.na(value_a1c_1) & !is.na(value_a1c_2) & ((value_a1c_1 >= 7) != (value_a1c_2 >= 7)))
                c23 <- as.integer(!is.na(value_a1c_2) & !is.na(value_a1c_3) & ((value_a1c_2 >= 7) != (value_a1c_3 >= 7)))
                c34 <- as.integer(!is.na(value_a1c_3) & !is.na(value_a1c_4) & ((value_a1c_3 >= 7) != (value_a1c_4 >= 7)))
                c45 <- as.integer(!is.na(value_a1c_4) & !is.na(value_a1c_5) & ((value_a1c_4 >= 7) != (value_a1c_5 >= 7)))
                c12 + c23 + c34 + c45
            },

            # regime-dependent slopes: mean delta when starting above vs below 7
            slope_above_7 = {
                d1 <- if_else(!is.na(delta_12) & value_a1c_1 >= 7, delta_12, NA_real_)
                d2 <- if_else(!is.na(delta_23) & value_a1c_2 >= 7, delta_23, NA_real_)
                d3 <- if_else(!is.na(delta_34) & value_a1c_3 >= 7, delta_34, NA_real_)
                d4 <- if_else(!is.na(delta_45) & value_a1c_4 >= 7, delta_45, NA_real_)
                m <- rowMeans(cbind(d1, d2, d3, d4), na.rm = TRUE)
                if_else(is.nan(m), NA_real_, m)
            },
            slope_below_7 = {
                d1 <- if_else(!is.na(delta_12) & value_a1c_1 < 7, delta_12, NA_real_)
                d2 <- if_else(!is.na(delta_23) & value_a1c_2 < 7, delta_23, NA_real_)
                d3 <- if_else(!is.na(delta_34) & value_a1c_3 < 7, delta_34, NA_real_)
                d4 <- if_else(!is.na(delta_45) & value_a1c_4 < 7, delta_45, NA_real_)
                m <- rowMeans(cbind(d1, d2, d3, d4), na.rm = TRUE)
                if_else(is.nan(m), NA_real_, m)
            },

            #### 8.0 threshold features (competition boundary) ####
            # Keep 7.0 features (clinical intervention threshold) AND add 8.0 (prediction boundary)

            frac_above_8 = {
                mat <- pick(value_a1c_1:value_a1c_5)
                rowSums(mat >= 8, na.rm = TRUE) / n_a1c
            },
            n_crossings_8 = {
                c12 <- as.integer(!is.na(value_a1c_1) & !is.na(value_a1c_2) & ((value_a1c_1 >= 8) != (value_a1c_2 >= 8)))
                c23 <- as.integer(!is.na(value_a1c_2) & !is.na(value_a1c_3) & ((value_a1c_2 >= 8) != (value_a1c_3 >= 8)))
                c34 <- as.integer(!is.na(value_a1c_3) & !is.na(value_a1c_4) & ((value_a1c_3 >= 8) != (value_a1c_4 >= 8)))
                c45 <- as.integer(!is.na(value_a1c_4) & !is.na(value_a1c_5) & ((value_a1c_4 >= 8) != (value_a1c_5 >= 8)))
                c12 + c23 + c34 + c45
            },
            slope_above_8 = {
                d1 <- if_else(!is.na(delta_12) & value_a1c_1 >= 8, delta_12, NA_real_)
                d2 <- if_else(!is.na(delta_23) & value_a1c_2 >= 8, delta_23, NA_real_)
                d3 <- if_else(!is.na(delta_34) & value_a1c_3 >= 8, delta_34, NA_real_)
                d4 <- if_else(!is.na(delta_45) & value_a1c_4 >= 8, delta_45, NA_real_)
                m <- rowMeans(cbind(d1, d2, d3, d4), na.rm = TRUE)
                if_else(is.nan(m), NA_real_, m)
            },
            slope_below_8 = {
                d1 <- if_else(!is.na(delta_12) & value_a1c_1 < 8, delta_12, NA_real_)
                d2 <- if_else(!is.na(delta_23) & value_a1c_2 < 8, delta_23, NA_real_)
                d3 <- if_else(!is.na(delta_34) & value_a1c_3 < 8, delta_34, NA_real_)
                d4 <- if_else(!is.na(delta_45) & value_a1c_4 < 8, delta_45, NA_real_)
                m <- rowMeans(cbind(d1, d2, d3, d4), na.rm = TRUE)
                if_else(is.nan(m), NA_real_, m)
            },

            # post-biggest-drop dynamics: after the largest drop, is the patient stable or bouncing back?
            # which transition had the biggest drop?
            drop_at_12 = if_else(!is.na(delta_12) & delta_12 == max_drop, TRUE, FALSE),
            drop_at_23 = if_else(!is.na(delta_23) & delta_23 == max_drop, TRUE, FALSE),
            drop_at_34 = if_else(!is.na(delta_34) & delta_34 == max_drop, TRUE, FALSE),
            # post-drop value: reading AFTER the biggest drop
            post_drop_value = case_when(
                drop_at_12 ~ value_a1c_2,
                drop_at_23 ~ value_a1c_3,
                drop_at_34 ~ value_a1c_4,
                TRUE ~ NA_real_
            ),
            # change from post-drop to latest (positive = bouncing back up, negative = still improving)
            post_drop_drift = if_else(!is.na(post_drop_value) & n_a1c >= 3, a1c_latest - post_drop_value, NA_real_),
            # was there a sudden drop AND patient is still above 7/8? (treatment started but not enough yet)
            sudden_drop_still_high7 = as.integer(has_sudden_drop == 1 & a1c_latest >= 7.0),
            sudden_drop_still_high8 = as.integer(has_sudden_drop == 1 & a1c_latest >= 8.0),

            # interactions at 7.0 (clinical intervention threshold)
            high7_insulin = as.integer(a1c_latest >= 7.0 & insulin > 0),
            high7_no_meds = as.integer(a1c_latest >= 7.0 & total_meds == 0),
            still_high7_improving = as.integer(a1c_latest >= 7.0 & (a1c_latest - value_a1c_1) < 0),
            a1c_change_if_high7 = ifelse(a1c_latest >= 7.0, a1c_latest - value_a1c_1, 0),

            # interactions at 8.0 (competition prediction boundary)
            high8_insulin = as.integer(a1c_latest >= 8.0 & insulin > 0),
            high8_no_meds = as.integer(a1c_latest >= 8.0 & total_meds == 0),
            still_high8_improving = as.integer(a1c_latest >= 8.0 & (a1c_latest - value_a1c_1) < 0),
            a1c_change_if_high8 = ifelse(a1c_latest >= 8.0, a1c_latest - value_a1c_1, 0),

            # shared interactions (threshold-independent)
            a1c_x_meds = a1c_latest * total_meds,
            a1c_x_ndrug = a1c_latest * n_drug_classes,
            a1c_change = a1c_latest - value_a1c_1,
            a1c_per_drug = a1c_latest / pmax(n_drug_classes, 1),
            improving_on_meds = as.integer(a1c_latest < value_a1c_1 & total_meds > 0),
            worsening_on_meds = as.integer(a1c_latest > value_a1c_1 & total_meds > 0),
            stable_on_meds = as.integer(a1c_latest == value_a1c_1 & total_meds > 0),
            on_modern_drugs = as.integer(glp1 > 0 | sglt2 > 0),
            n_a1c_x_ndrug = n_a1c * n_drug_classes,

            # BMI (derived from height + weight, filter extreme values)
            bmi = {
                raw <- value_weight / ((value_height / 100)^2)
                if_else(!is.na(raw) & raw >= 10 & raw <= 80, raw, NA_real_)
            },
            bmi_x_male = if_else(!is.na(bmi), bmi * gender_male, NA_real_),

            # Rate-normalized counts: count / observation window (per year)
            # Separates treatment intensity from observation duration
            obs_years = pmax(time_latest_a1c, 1) / 365.25,
            insulin_rate = insulin / obs_years,
            total_meds_rate = total_meds / obs_years,
            visits_rate = (ed_visits + pcp_visits + admissions) / obs_years

            # Derived lipid/log features (tested Apr 5 — hurt GLM/XGBoost, only helped glmnet)
            # Kept commented; uncomment for glmnet-specific runs
            # non_hdl = value_chol - value_hdl,
            # ldl_hdl_ratio = if_else(!is.na(value_ldl) & !is.na(value_hdl) & value_hdl > 0, value_ldl / value_hdl, NA_real_),
            # adi_discrepancy = adi_nation - adi_state,
            # log_ed_visits = log(1 + ed_visits),
            # log_pcp_visits = log(1 + pcp_visits),
            # log_admissions = log(1 + admissions),
            # log_total_meds = log(1 + total_meds)
        )

    # create target and mask variables
    df$target <- factor(ifelse(df$value_a1c_2025 > 0, "Uncontrolled", "Controlled"), levels = c("Controlled", "Uncontrolled"))
    df$mask <- !is.na(df$time_a1c_2025)
    return(df)
}


# ---------------
# Impute values and create missingness flags
# ---------------
impute_and_flag <- function(df) {
    flag_cols <- c(
        "abs_change", "a1c_sd", "a1c_range", "a1c_rate",
        "value_hdl", "value_ldl", "value_chol",
        "value_height", "value_weight",
        "time_weight", "time_height", "time_ldl", "time_hdl",
        "adi_state", "adi_nation",
        "time_gap", "max_drop", "max_rise",
        "slope_above_7", "slope_below_7",
        "slope_above_8", "slope_below_8",
        "post_drop_drift",
        "bmi"
        # "non_hdl", "ldl_hdl_ratio"
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
        "value_height", "value_weight",
        "time_weight", "time_height", "time_ldl", "time_hdl",
        "adi_state", "adi_nation",
        "cad", "copd",
        "race_black", "race_white", "race_asian", "ethnicity_hl",
        "days_to_eval", "time_gap",
        "max_drop", "max_rise", "has_sudden_drop", "has_sudden_rise",
        "frac_above_7", "n_crossings_7",
        "slope_above_7", "slope_below_7",
        "frac_above_8", "n_crossings_8",
        "slope_above_8", "slope_below_8",
        "post_drop_drift", "sudden_drop_still_high7", "sudden_drop_still_high8",
        "high7_insulin", "high7_no_meds",
        "still_high7_improving", "a1c_change_if_high7",
        "high8_insulin", "high8_no_meds",
        "still_high8_improving", "a1c_change_if_high8",
        "a1c_x_meds", "a1c_x_ndrug",
        "a1c_change", "a1c_per_drug",
        "improving_on_meds", "worsening_on_meds", "stable_on_meds",
        "on_modern_drugs", "n_a1c_x_ndrug",
        "bmi", "bmi_x_male",
        "insulin_rate", "total_meds_rate", "visits_rate"
        # "non_hdl", "ldl_hdl_ratio",
        # "adi_discrepancy",
        # "log_ed_visits", "log_pcp_visits", "log_admissions", "log_total_meds"
    )
    setdiff(feature_cols, colnames(df))
    for (col in feature_cols) {
        med_val <- median(df[[col]], na.rm = TRUE)
        df[[col]] <- ifelse(is.na(df[[col]]), med_val, df[[col]])
    }
    return(df)
}
