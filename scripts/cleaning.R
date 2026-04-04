# ====================
# This script contains the finalized data cleaning pipeline
# The idea is to split the feature data into 3 different dataframes.
#   - `basics` contains the static, patient level demographic and socioeconomic data
#   - `counts` contains the cumulative counts of specific healthcare metrics (this may or may not be used as intervention mechanism)
#   - `measurements` contains the measured variables, i.e., longitudinal, time-series clinical measurements in a tidy (long) format. Each row represents a single measurement event for a specific patient.
#
# ====================
library(tidyverse)
source("./scripts/utils.R")


clean_dataset <- function(dm_features, dm_control) {
    # extract the basic info
    basic_info <- tibble(
        id = dm_features$...1,
        dob = dm_features$`date of birth`,
        gender = dm_features$`gender at birth`,
        ethnicity = dm_features$ethnicity,
        race = dm_features$`race - primary`,
        adi_state = as.integer(dm_features$`adi-adi state rank`), #  Area Deprivation Index (State Level), 1 = best, 100 = worst
        adi_nation = as.integer(dm_features$`adi-adi national rank`)
    ) %>%
        mutate(
            age = 2025 - dob,
            gender = case_when(
                (gender == "MALE") ~ "M",
                (gender == "FEMALE") ~ "F",
                TRUE ~ NA # everything else is NA
            ),
            ethnicity = case_when(
                (ethnicity == "Hispanic or Latino") ~ "HL", # hispanic or latino
                (ethnicity == "Not Hispanic or Latino") ~ "NHL", # not hispanic or latino
                TRUE ~ NA # everything else is NA
            ),
            race = case_when(
                (race == "American Indian or Alaska Native") ~ "Native",
                (race == "Asian") ~ "Asian",
                (race == "Black or African American") ~ "Black",
                (race == "White") ~ "White",
                (race == "Other Pacific Islander") ~ "Other",
                TRUE ~ NA # everything else is NA
            )
        )

    # count informations
    count_info <- tibble(
        id = dm_features$...1,
        cad = dm_features$`cad-count`, # Coronary Artery Disease??
        copd = dm_features$`copd-count`, # Chronic Obstructive Pulmonary Disease??
        ed_visit = dm_features$`ed vist count-count`,
        pcp_visit = dm_features$`pcp visit count-count`,
        admission = dm_features$`admission count-count`, # Count of admission
        glp1 = dm_features$`glp-1 orders-count`,
        insulin = dm_features$`insulin orders-count`,
        metformin = dm_features$`metformin orders-count`,
        sglt2 = dm_features$`sglt2 orders-count`,
        sulfonylurea = dm_features$`sulfonylurea orders-count`,
        dpp4 = dm_features$`dpp4 orders-count`
    ) %>% mutate(across(-id, as.integer))

    # measurements
    a1c_measurements <- tibble(dm_features) %>%
        select(
            id = `...1`,
            dob = `date of birth`,
            a1c_1 = `a1c 1-collection date-time-days from reference`,
            a1c_2 = `a1c 2-collection date-time-days from reference`,
            a1c_3 = `a1c 3-collection date-time-days from reference`,
            a1c_4 = `a1c 4-collection date-time-days from reference`,
            a1c_5 = `a1c 5-collection date-time-days from reference`
        ) %>%
        pivot_longer(-id, names_to = "variable", values_to = "time") %>%
        inner_join(
            tibble(dm_features) %>%
                select(
                    id = `...1`,
                    a1c_1 = `a1c 1-estimated result`,
                    a1c_2 = `a1c 2-estimated result`,
                    a1c_3 = `a1c 3-estimated result`,
                    a1c_4 = `a1c 4-estimated result`,
                    a1c_5 = `a1c 5-estimated result`
                ) %>%
                pivot_longer(-id, names_to = "variable", values_to = "value"),
            by = c("id", "variable")
        ) %>%
        drop_na() %>%
        select(-variable) %>%
        group_by(id) %>%
        arrange(time) %>% # this fixes the time sorting issues
        mutate(variable = paste0("a1c_", row_number())) %>%
        ungroup() %>%
        right_join(
            tibble(
                id = rep(dm_control$`...1`, each = 5),
                variable = rep(paste0("a1c_", 1:5), nrow(dm_control))
            ),
            by = c("id", "variable")
        )

    # let's look at other measurments as well
    other_measurments <- tibble(dm_features) %>%
        select(
            id = `...1`,
            height = `height-measurement date-time-days from reference`,
            weight = `weight-measurement date-time-days from reference`,
            ldl = `ldl-collection date-time-days from reference`,
            hdl = `hdl-collection date-time-days from reference`,
            chol = `total cholesterol-collection date-time-days from reference`,
            a1c_2025 = `a1c 2025-collection date-time-days from reference`
        ) %>%
        pivot_longer(-id, names_to = "variable", values_to = "time") %>%
        inner_join(
            tibble(dm_features) %>%
                select(
                    id = `...1`,
                    height = `height-estimated result`,
                    weight = `weight-estimated result`,
                    ldl = `ldl-estimated result`,
                    hdl = `hdl-estimated result`,
                    chol = `total cholesterol-estimated result`
                ) %>%
                left_join(tibble(
                    id = dm_control$...1,
                    a1c_2025 = dm_control$`a1c 2025 Uncontrolled`
                ), by = "id") %>%
                pivot_longer(-id, names_to = "variable", values_to = "value"),
            by = c("id", "variable")
        )

    measurements <- bind_rows(a1c_measurements, other_measurments) %>%
        select(id, variable, time, value) %>%
        arrange(id, variable)

    return(list(
        basic = basic_info,
        counts = count_info,
        measurements = measurements
    ))
}


dm_features <- read_csv("./data/raw/DM Features.csv")
dm_control <- read_csv("./data/raw/DM Control_2025.csv")

cleaned_dat <- clean_dataset(dm_features, dm_control)
names(cleaned_dat)
saveRDS(cleaned_dat, file = "./data/processed/cleaned_dat.Rds")

splitted_dat <- split_train_test_val(cleaned_dat, id_column = "id", split_percentage = c(0.5, 0.25, 0.25), seed = 1)
names(splitted_dat)

# save each file
lapply(X = names(splitted_dat), FUN = function(x) {
    saveRDS(splitted_dat[[x]], file = paste0("./data/processed/", x, ".Rds"))
})

# Final test data cleaning
library(readr)
dm_features_final_test <- read_csv("/data/raw/TEST SET DM Features.csv")
dm_control_final_test <- tibble("...1" = dm_features_final_test$...1,
                                "a1c 2025 Uncontrolled" = NA)
cleaned_dat_final_test <- clean_dataset(dm_features_final_test,
                                        dm_control_final_test)

saveRDS(cleaned_dat_final_test, file = "./data/processed/final_test.Rds")

