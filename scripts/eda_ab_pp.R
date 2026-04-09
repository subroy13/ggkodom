library(dplyr)
library(tidyr)
library(patchwork)
library(ggplot2)
library(MetBrewer)

train_dat <- readRDS("./data/processed/cleaned_dat.Rds")

measurements <- train_dat$measurements

# prepare A1C flags (your existing code)
a1c_flags <- measurements %>%
  filter(variable %in% paste0("a1c_", 1:5)) %>%
  mutate(visit = as.integer(str_extract(variable, "(?<=_)\\d+"))) %>%
  group_by(id) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(
    above_8 = if_else(!is.na(value) & value > 8, 1L, 0L, missing = NA),
    next_time = lead(time),
    next_value = lead(value),
    diff_value = if_else(!is.na(value) & !is.na(next_value), next_value - value, NA_real_),
    diff_time = if_else(!is.na(time) & !is.na(next_time), next_time - time, NA_real_),
    diff_value_per_time = if_else(!is.na(diff_value) & diff_time > 0, diff_value / diff_time, NA_real_)
  ) %>%
  ungroup() %>%
  left_join(train_dat$basic %>% select(id, gender), by = "id") %>%
  filter(gender %in% c("M", "F")) %>%
  mutate(
    slope_plot = diff_value_per_time
  )

a1c_2025 <- measurements %>%
  filter(variable == "a1c_2025") %>%
  mutate(a1c_2025 = as.integer(value)) %>%
  filter(a1c_2025 %in% c(0, 1)) %>%
  select(id, a1c_2025)

a1c_flags <- a1c_flags %>%
  left_join(a1c_2025, by = "id") %>% 
  left_join(
    train_dat$basic %>% select(id, age),
    by = "id"
  )


density_age_overlay <- function(a1c_flags, control_val) {
  
  plot_dat <- a1c_flags %>%
    filter(!is.na(slope_plot), !is.na(gender), !is.na(age), !is.na(a1c_2025)) %>%
    filter(a1c_2025 == control_val) %>%
    mutate(
      age_group = case_when(
        age < 30 ~ "<30",
        age >= 30 & age < 60 ~ "30-60",
        age >= 60 ~ "60+",
        TRUE ~ NA_character_
      ),
      age_group = factor(age_group, levels = c("<30", "30-60", "60+")),
      group = paste(gender, age_group, sep = "_")
    ) %>%
    filter(!is.na(age_group))
  
  pp <- ggplot(plot_dat, aes(x = slope_plot, color = group, fill = group)) +
    
    # Male (positive)
    geom_density(
      data = plot_dat %>% filter(gender == "M"),
      aes(y = after_stat(density)),
      alpha = 0.20,
      linewidth = 1
    ) +
    
    # Female (negative)
    geom_density(
      data = plot_dat %>% filter(gender == "F"),
      aes(y = -after_stat(density)),
      alpha = 0.20,
      linewidth = 1
    ) +
    
    geom_hline(yintercept = 0, linetype = "dashed") +
    
    scale_color_manual(
      values = c(
        # 🔵 Male palette (blue shades)
        "M_<30"   = "#08519c",
        "M_30-60" = "#3182bd",
        "M_60+"   = "#9ecae1",
        
        # 🌸 Female palette (pink shades)
        "F_<30"   = "#c51b8a",
        "F_30-60" = "#f768a1",
        "F_60+"   = "#fbb4b9"
      ),
      breaks = c("M_<30","M_30-60","M_60+","F_<30","F_30-60","F_60+"),
      labels = c(
        "M <30",
        "M 30–60",
        "M 60+",
        "F <30",
        "F 30–60",
        "F 60+"
      )
    ) +
    
    scale_fill_manual(
      values = c(
        "M_<30"   = "#08519c",
        "M_30-60" = "#3182bd",
        "M_60+"   = "#9ecae1",
        
        "F_<30"   = "#c51b8a",
        "F_30-60" = "#f768a1",
        "F_60+"   = "#fbb4b9"
      ),
      breaks = c("M_<30","M_30-60","M_60+","F_<30","F_30-60","F_60+"),
      labels = c(
        "M <30",
        "M 30–60",
        "M 60+",
        "F <30",
        "F 30–60",
        "F 60+"
      )
    ) +
    
    scale_y_continuous(labels = function(x) abs(x)) +
    xlim(-0.035, 0.035) +
    ylim(-120, 120) +
    theme_bw() +
    labs(
      x = "",
      y = "",
      color = "Gender × Age",
      fill = "Gender × Age",
      title = ""
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
    )
  outfile <- paste0("./figures/eda_pp_",control_val, ".png")
  ggsave(
    filename = outfile,
    plot = pp,
    dpi = 300,
    height = 4,
    width = 12,
    units = "in"
  )
}

density_age_overlay(a1c_flags, 1)
density_age_overlay(a1c_flags, 0)

