# ===============================
# This script should mostly contain the exploratory data analysis related codes
# ===============================


library(tidyverse)
library(ggplot2)
library(stringr)

# ----------------
# Author: Subh
# ----------------

dm_features <- read_csv("./data/raw/DM Features.csv")
dm_control <- read_csv("./data/raw/DM Control_2025.csv")

# Look at the basic data view
colnames(dm_features)
colnames(dm_control)

# Q: proportion of true / false => highly imbalanced
table(dm_control$`a1c 2025 Uncontrolled`) / nrow(dm_control)

table(dm_features$`height-unit of measure`) # single unit = cm
table(dm_features$`weight-unit of measure`) # single unit = kg


# look at table of gender, race, etc.
table(dm_features$`gender at birth`, useNA = "ifany")
table(dm_features$ethnicity, useNA = "ifany")
table(dm_features$`race - primary`, useNA = "ifany")

# create a measurements table
measurements <- tibble(dm_features) %>%
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
    )

# let's look at a spaghetti plot
set.seed(1)
sample_id <- sample((
    tibble(dm_features) %>%
        select(id = `...1`, dob = `date of birth`) %>%
        mutate(age = 2025 - dob) %>%
        filter(age > 80)
)$id, size = 100)

measurements %>%
    filter(id %in% sample_id) %>%
    filter(variable %in% paste0("a1c_", 1:5)) %>%
    mutate(id = factor(id)) %>%
    ggplot() +
    geom_line(aes(x = time, y = value, group = id), alpha = 0.25) +
    theme_bw() +
    theme(legend.position = "none")


# let's look at the gap between the measurement time points
# this will give us a resolution in temporal space
tmp <- measurements %>%
    filter(variable %in% paste0("a1c_", 1:5)) %>%
    filter(id %in% (measurements %>% filter(variable == "a1c_2", !is.na(value)))$id) %>%
    group_by(id) %>%
    summarise(x = min(diff(time), na.rm = T)) %>%
    ungroup()
tmp # some group has diff(time) < 0 => need careful look

tmp %>% filter(x < 0)

# check original data for id = 43
dm_features %>%
    filter(`...1` == 43) %>%
    View() # issues with causality of time

tmp %>% filter((x > 0) & (x < 1))
dm_features %>%
    filter(id == 2534) %>%
    View() # there are visit too close to each other


# ----------------
# Author: AB
# ----------------

# RUN this after creating the measurements table from above.

# track the current and next value.
# look at conditional distribution of changes, | current a1c > 7 or < 7
a1c_flags <- measurements %>%
    filter(variable %in% paste0("a1c_", 1:5)) %>%
    mutate(visit = as.integer(str_extract(variable, "(?<=_)\\d+"))) %>%
    group_by(id) %>%
    arrange(time, .by_group = TRUE) %>%
    mutate(
        above_7 = if_else(!is.na(value) & value > 7, 1L, 0L, missing = NA),
        next_time = lead(time), # the next timepoint
        next_value = lead(value), # the next value
        next_higher = case_when(
            is.na(value) | is.na(next_value) ~ NA,
            next_value > value ~ 1L,
            TRUE ~ 0L
        ), # indicator whether the next value is higher or not
        diff_value = case_when(
            is.na(value) | is.na(next_value) ~ NA_real_,
            TRUE ~ next_value - value
        ),
        diff_time = case_when(
            is.na(time) | is.na(next_time) ~ NA_real_,
            TRUE ~ next_time - time
        ),
        diff_value_per_time = case_when(
            is.na(diff_value) | is.na(diff_time) ~ NA_real_,
            diff_time == 0 ~ NA_real_,
            TRUE ~ diff_value / diff_time
        )
    ) %>%
    ungroup()


# make a violin plot of slopes
ggplot(
    a1c_flags %>%
        filter(!is.na(diff_value_per_time), !is.na(above_7)),
    aes(
        x = factor(above_7,
            levels = c(0, 1),
            labels = c("< 7 (Controlled)", "> 7 (Uncontrolled)")
        ),
        y = diff_value_per_time
    )
) +
    geom_violin(trim = FALSE, fill = "#6baed6", alpha = 0.6) +
    theme_bw() +
    labs(
        x = "",
        y = "Change in A1C / Change in time",
        title = "A1C progression rate by current control status"
    ) +
    ylim(-0.035, 0.035) # this require manual tuning to zoom into the slopes

ggsave("./figures/eda_conditional_beta.pdf", dpi = 300)


# ---------------
# Q: Does missingness of height and weight has association with gender?

dm_features <- read_csv("./data/raw/DM Features.csv")
dm_control <- read_csv("./data/raw/DM Control_2025.csv")

table(
    dm_features$`gender at birth`,
    is.na(dm_features$`height-estimated result`)
)

table(
    dm_features$`gender at birth`,
    is.na(dm_features$`weight-estimated result`)
) # establishes missing data at random

# ----------------
library(tidyverse)

dat <- readRDS("./data/processed/cleaned_dat.Rds")

transitions <- dat$measurements %>%
  filter(variable %in% paste0("a1c_", 1:5)) %>%
  drop_na() %>%
  arrange(id, variable) %>%
  group_by(id) %>%
  mutate(
    t_start = time,
    value_start = value,
    t_end = lead(time, 1),
    value_end = lead(value, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(t_end)) %>%
  mutate(
    delta_t = t_end - t_start,
    delta_val = value_end - value_start,
    state_start = case_when(
      # value_start < 7 ~ "< 7",
      value_start >= 8 ~ "Current A1c ≥ 8",
      TRUE ~ "Current A1c < 8"
    ),
    beta = delta_val / delta_t,
    state_start = factor(
      state_start,
      levels = c("Current A1c < 8", "Current A1c ≥ 8")
    )
  ) %>%
  select(id, state_start, delta_t, delta_val, beta)

transitions

# make violin plot
# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3

trans2 <- transitions %>%
  left_join(
    dat$basic %>% 
      select(id, age) %>%
      mutate(
        age_group = case_when(
          age < 30 ~ "< 30",
          age >= 60 ~ "≥ 60",
          TRUE ~ "30 - 60"
        ),
        age_group = factor(age_group, levels = c("< 30", "30 - 60", "≥ 60"))
      )
  , by = "id") %>%
  filter(!is.na(age_group)) 

ggplot(trans2, aes(y = beta, x = age_group, fill = age_group)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  geom_hline(yintercept = 0, color = "black", linetype = "dotdash") +
  scale_fill_brewer(palette = "Dark2") +
  ylim(-2/30, 2/30) +
  # xlab("Baseline A1c group") +
  xlab("Age group") +
  ylab(expression(frac(Delta~A1c, Delta~t))) +
  facet_grid(cols = vars(state_start)) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14)
  )

ggsave("./figures/slope_by_age.png", dpi = 300, width = 12, height = 6)


trans2 %>%
  group_by(state_start, age_group) %>%
  summarise(
    beta_mean = mean(beta, na.rm = T),
    beta_med = median(beta, na.rm = T),
    beta_25 = quantile(beta, probs = 0.25, na.rm = T),
    beta_75 = quantile(beta, probs = 0.75, na.rm = T)
  )



transitions %>%
  left_join(dat$basic %>% select(id, gender), by = "id") %>%
  filter(!is.na(gender)) %>%
  ggplot() +
  geom_density(aes(x = beta, fill = state_start), alpha = 0.5) +
  theme_bw() +
  xlim(-2/30, 2/30) +
  facet_wrap(~ gender)
  














