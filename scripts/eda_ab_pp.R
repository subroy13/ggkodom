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


# theme(
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#    
#                              hjust = 2.25, vjust = 0.775),
#   axis.text.x = element_text(size = 18, face = "bold"),
#   
#   legend.position = leg_pos
# ) 
