train_dat <- readRDS("./data/processed/cleaned_dat.Rds")

library(dplyr)
library(tidyr)
library(patchwork)
library(ggplot2)
library(MetBrewer)

plot_a1c_subset <- function(train_dat,
                            gender_keep = NULL,
                            age_group_keep = NULL,
                            n_sample = 50,
                            seed = 123) {
  
  
  a1c_long <- train_dat$measurements %>%
    filter(variable %in% paste0("a1c_", 1:5)) %>%
    filter(!is.na(time), !is.na(value))
  
  basic_use <- train_dat$basic %>%
    select(id, gender, age) %>%
    mutate(
      age_group = case_when(
        age < 30 ~ "_30",
        age < 60 ~ "30_60",
        TRUE ~ "60_"
      )
    )
  
  a1c_long <- a1c_long %>%
    left_join(basic_use, by = "id")
  
  if (!is.null(gender_keep)) {
    a1c_long <- a1c_long %>%
      filter(gender == gender_keep)
  }
  
  if (!is.null(age_group_keep)) {
    a1c_long <- a1c_long %>%
      filter(age_group == age_group_keep)
  }
  
  ids_avail <- unique(a1c_long$id)
  if (length(ids_avail) == 0) {
    stop("No patients left after filtering.")
  }
  
  set.seed(seed)
  sample_ids <- sample(ids_avail, min(n_sample, length(ids_avail)))
  
  a1c_plot <- a1c_long %>%
    filter(id %in% sample_ids) %>%
    arrange(id, time)
  
  baseline_order <- a1c_plot %>%
    filter(variable == "a1c_1") %>%
    select(id, baseline = value)
  
  a1c_outcome <- train_dat$measurements %>%
    filter(variable == "a1c_2025") %>%
    select(id, outcome = value) %>%
    mutate(outcome_label = ifelse(outcome == 1, "T", "F"))
  
  a1c_plot <- a1c_plot %>%
    left_join(baseline_order, by = "id") %>%
    left_join(a1c_outcome, by = "id")
  
  id_order <- a1c_plot %>%
    distinct(id, baseline) %>%
    arrange(baseline) %>%
    pull(id)
  
  a1c_plot$id <- factor(a1c_plot$id, levels = id_order)
  
  y_labels <- a1c_plot %>%
    distinct(id, outcome_label) %>%
    arrange(match(id, id_order))
  
  label_vec <- ifelse(y_labels$outcome_label == "F", "", "*")
  names(label_vec) <- y_labels$id
  
  title_text <- paste(
    c(
      if (!is.null(gender_keep)) gender_keep else NULL,
      if (!is.null(age_group_keep)) paste("Age", age_group_keep) else NULL
    ),
    collapse = ", "
  )
  
  if (title_text == "") {
    title_text <- "A1C trajectories"
  }
  
  leg_pos <- ifelse(gender_keep == "M", "top", "bottom")
  
  # min_a1c <- floor(min(a1c_long$value, na.rm = TRUE))
  # max_a1c <- ceiling(max(a1c_long$value, na.rm = TRUE))
  
  p <- ggplot(a1c_plot, aes(x = time, y = id, group = id)) +
    geom_path(aes(color = value), linewidth = 1, alpha = 0.7) +
    geom_point(aes(color = value), size = 4) +
    # scale_color_gradientn(
    #   colors = c("darkgreen", "gold", "red"),
    #   values = scales::rescale(c(5.7, 6.5, 8)),
    #   na.value = "grey90"
    # ) +
    # scale_color_gradientn(
    #   colors = c("darkgreen", "gold", "red"),
    #   values = scales::rescale(c(5.7, 6.5, 8)),
    #   limits = c(min_a1c, max_a1c),
    #   breaks = c(min_a1c, 5.7, 6.5, max_a1c),
    #   labels = c(min_a1c, "5.7", "6.5", max_a1c),
    #   name = "A1C"
    # ) +
    scale_color_gradientn(
      colors = c("#008d98", "#ffcc3dff", "#d7433b"), #95caa6ff
      values = scales::rescale(c(5.7, 6.5, 8)),
      na.value = "grey90"
    ) +
    scale_y_discrete(labels = label_vec) +
    labs(
      x = "",
      y = "",
      color = "A1C" #,
      # title = title_text
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(face = "bold", size = 20, 
                                 hjust = 2.25, vjust = 0.775),
      axis.text.x = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.position = leg_pos
    ) +
    guides(
      color = guide_colorbar(
        barwidth = 25,   # 🔥 increase width
        barheight = 0.75  # thickness
      )
    )
  
  
  outfile <- paste0("./figures/eda_kodom_", gender_keep, "_", age_group_keep, ".png")
  ggsave(
    filename = outfile,
    plot = p,
    dpi = 300,
    height = 6,
    width = 10,
    units = "in"
  )
}


plot_a1c_subset(train_dat, gender_keep = "M", age_group_keep = "_30")
plot_a1c_subset(train_dat, gender_keep = "M", age_group_keep = "30_60")
plot_a1c_subset(train_dat, gender_keep = "M", age_group_keep = "60_")
plot_a1c_subset(train_dat, gender_keep = "F", age_group_keep = "_30")
plot_a1c_subset(train_dat, gender_keep = "F", age_group_keep = "30_60")
plot_a1c_subset(train_dat, gender_keep = "F", age_group_keep = "60_")


# p11 <- plot_a1c_subset(train_dat, gender_keep = "M", age_group_keep = "<30")
# p12 <- plot_a1c_subset(train_dat, gender_keep = "F", age_group_keep = "<30")
# 
# p21 <- plot_a1c_subset(train_dat, gender_keep = "M", age_group_keep = "30-60")
# p22 <- plot_a1c_subset(train_dat, gender_keep = "F", age_group_keep = "30-60")
# 
# p31 <- plot_a1c_subset(train_dat, gender_keep = "M", age_group_keep = "60+")
# p32 <- plot_a1c_subset(train_dat, gender_keep = "F", age_group_keep = "60+")



