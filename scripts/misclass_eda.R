###############################################################################
## Secondary EDA: Who gets misclassified and why?
## Goal: Understand the ~62% F1 ceiling by examining FP/FN patients
###############################################################################

library(tidyverse)
source("./scripts/utils.R")

#### LOAD DATA ####
res <- readRDS("./data/processed/val_predictions.Rds")
vp <- res$val_preds
vf <- res$val_features

# focus on returned patients only (those with a 2025 eval)
vp <- vp %>% filter(mask)
vf <- vf %>% filter(mask)

cat("Returned val patients:", nrow(vp), "\n")
cat("Uncontrolled:", sum(vp$target == "Uncontrolled"), "(", round(mean(vp$target == "Uncontrolled") * 100, 1), "%)\n\n")


#### CLASSIFY TP/TN/FP/FN USING GLM+WEIGHTS ####
# (simplest model that handles imbalance — patterns here generalize to fancier models)
vp <- vp %>% mutate(
    pred_glm_wt = prob_glm_wt >= th_glm_wt,
    actual = target == "Uncontrolled",
    group = case_when(
        actual & pred_glm_wt ~ "TP",
        !actual & !pred_glm_wt ~ "TN",
        !actual & pred_glm_wt ~ "FP",
        actual & !pred_glm_wt ~ "FN"
    )
)

cat("------ Classification counts (GLM+weights) ------\n")
print(table(vp$group))
cat("\n")


#### MERGE FEATURES WITH CLASSIFICATION ####
df <- vf %>%
    left_join(vp %>% select(id, group, prob_glm_wt, prob_glm, prob_tree, prob_gam, actual), by = "id")


#### 1. A1c LATEST BY GROUP ####
cat("------ a1c_latest by group ------\n")
df %>%
    group_by(group) %>%
    summarise(
        n = n(),
        mean = mean(a1c_latest, na.rm = TRUE),
        median = median(a1c_latest, na.rm = TRUE),
        q25 = quantile(a1c_latest, 0.25, na.rm = TRUE),
        q75 = quantile(a1c_latest, 0.75, na.rm = TRUE),
        pct_6to8 = mean(a1c_latest >= 6 & a1c_latest <= 8, na.rm = TRUE) * 100
    ) %>% print()
cat("\n")


#### 2. A1c WEIGHTED BY GROUP ####
cat("------ a1c_weighted by group ------\n")
df %>%
    group_by(group) %>%
    summarise(
        n = n(),
        mean = mean(a1c_weighted, na.rm = TRUE),
        median = median(a1c_weighted, na.rm = TRUE),
        q25 = quantile(a1c_weighted, 0.25, na.rm = TRUE),
        q75 = quantile(a1c_weighted, 0.75, na.rm = TRUE)
    ) %>% print()
cat("\n")


#### 3. HOW CLOSE TO THE 7.0 BOUNDARY? ####
cat("------ Distance from 7.0 boundary ------\n")
df %>%
    mutate(dist_from_7 = abs(a1c_latest - 7.0)) %>%
    group_by(group) %>%
    summarise(
        mean_dist = mean(dist_from_7, na.rm = TRUE),
        median_dist = median(dist_from_7, na.rm = TRUE),
        pct_within_1 = mean(dist_from_7 <= 1.0, na.rm = TRUE) * 100,
        pct_within_0.5 = mean(dist_from_7 <= 0.5, na.rm = TRUE) * 100
    ) %>% print()
cat("\n")


#### 4. NUMBER OF A1C READINGS ####
cat("------ n_a1c by group ------\n")
df %>%
    group_by(group) %>%
    summarise(
        n = n(),
        mean_n_a1c = mean(n_a1c, na.rm = TRUE),
        pct_1_reading = mean(n_a1c == 1, na.rm = TRUE) * 100,
        pct_2_readings = mean(n_a1c == 2, na.rm = TRUE) * 100,
        pct_3plus = mean(n_a1c >= 3, na.rm = TRUE) * 100
    ) %>% print()
cat("\n")


#### 5. MEDICATION PATTERNS ####
cat("------ Medication patterns by group ------\n")
df %>%
    group_by(group) %>%
    summarise(
        n = n(),
        mean_n_drugs = mean(n_drug_classes, na.rm = TRUE),
        pct_no_meds = mean(total_meds == 0, na.rm = TRUE) * 100,
        pct_insulin = mean(insulin > 0, na.rm = TRUE) * 100,
        pct_modern = mean(on_modern_drugs == 1, na.rm = TRUE) * 100,
        pct_improving = mean(improving_on_meds == 1, na.rm = TRUE) * 100,
        pct_worsening = mean(worsening_on_meds == 1, na.rm = TRUE) * 100
    ) %>% print()
cat("\n")


#### 6. A1C TRAJECTORY ####
cat("------ A1c trajectory by group ------\n")
df %>%
    group_by(group) %>%
    summarise(
        n = n(),
        mean_change = mean(a1c_change, na.rm = TRUE),
        mean_sd = mean(a1c_sd, na.rm = TRUE),
        mean_range = mean(a1c_range, na.rm = TRUE),
        pct_still_high_improving = mean(still_high_improving == 1, na.rm = TRUE) * 100
    ) %>% print()
cat("\n")


#### 7. DEMOGRAPHICS ####
cat("------ Demographics by group ------\n")
df %>%
    group_by(group) %>%
    summarise(
        n = n(),
        mean_age = mean(age, na.rm = TRUE),
        pct_male = mean(gender_male, na.rm = TRUE) * 100
    ) %>% print()
cat("\n")


#### 8. HEIGHT / WEIGHT ####
cat("------ Height / Weight by group ------\n")
df %>%
    group_by(group) %>%
    summarise(
        n = n(),
        mean_height = mean(value_height, na.rm = TRUE),
        mean_weight = mean(value_weight, na.rm = TRUE),
        pct_height_miss = mean(is.na(value_height)) * 100,
        pct_weight_miss = mean(is.na(value_weight)) * 100
    ) %>% print()
cat("\n")


#### 9. CROSS-MODEL AGREEMENT ####
# Are the same patients misclassified by all models, or does it depend on the model?
cat("------ Cross-model agreement on misclassification ------\n")
df <- df %>% mutate(
    pred_glm = prob_glm >= vp$th_glm[1],
    pred_glm_wt = prob_glm_wt >= vp$th_glm_wt[1],
    pred_tree = prob_tree >= vp$th_tree[1],
    pred_gam = prob_gam >= vp$th_gam[1],
    n_models_correct = (pred_glm_wt == actual) + (pred_glm == actual) + (pred_tree == actual) + (pred_gam == actual)
)

cat("How many models get it right (out of 4):\n")
df %>%
    group_by(group, n_models_correct) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = n_models_correct, values_from = n, values_fill = 0, names_prefix = "correct_") %>%
    print()
cat("\n")

cat("Patients ALL 4 models get wrong:\n")
hard_cases <- df %>% filter(n_models_correct == 0)
cat("  n =", nrow(hard_cases), "\n")
cat("  FN:", sum(hard_cases$actual), " FP:", sum(!hard_cases$actual), "\n")
cat("  a1c_latest — mean:", round(mean(hard_cases$a1c_latest, na.rm = TRUE), 2),
    " median:", round(median(hard_cases$a1c_latest, na.rm = TRUE), 2), "\n")
cat("  a1c_weighted — mean:", round(mean(hard_cases$a1c_weighted, na.rm = TRUE), 2),
    " median:", round(median(hard_cases$a1c_weighted, na.rm = TRUE), 2), "\n")
cat("  n_a1c — mean:", round(mean(hard_cases$n_a1c, na.rm = TRUE), 2), "\n")
cat("  n_drug_classes — mean:", round(mean(hard_cases$n_drug_classes, na.rm = TRUE), 2), "\n\n")


#### 10. SIMPLE PLOTS ####
# base R plots for quick look

par(mfrow = c(2, 2))

# a1c_latest by group
boxplot(a1c_latest ~ group, data = df, main = "a1c_latest by TP/TN/FP/FN",
        col = c("tomato", "lightblue", "gold", "lightgreen"), ylab = "a1c_latest")
abline(h = 7.0, lty = 2, col = "red")

# a1c_weighted by group
boxplot(a1c_weighted ~ group, data = df, main = "a1c_weighted by TP/TN/FP/FN",
        col = c("tomato", "lightblue", "gold", "lightgreen"), ylab = "a1c_weighted")
abline(h = 7.0, lty = 2, col = "red")

# n_a1c by group
boxplot(n_a1c ~ group, data = df, main = "n_a1c by TP/TN/FP/FN",
        col = c("tomato", "lightblue", "gold", "lightgreen"), ylab = "# A1c readings")

# predicted probability by group
boxplot(prob_glm_wt ~ group, data = df, main = "GLM+wt predicted prob by group",
        col = c("tomato", "lightblue", "gold", "lightgreen"), ylab = "P(Uncontrolled)")
abline(h = vp$th_glm_wt[1], lty = 2, col = "red")

cat("------ Plots saved to figures/misclass_eda.pdf ------\n")
pdf("./figures/misclass_eda.pdf", width = 10, height = 8)
par(mfrow = c(2, 2))
boxplot(a1c_latest ~ group, data = df, main = "a1c_latest by TP/TN/FP/FN",
        col = c("tomato", "lightblue", "gold", "lightgreen"), ylab = "a1c_latest")
abline(h = 7.0, lty = 2, col = "red")
boxplot(a1c_weighted ~ group, data = df, main = "a1c_weighted by TP/TN/FP/FN",
        col = c("tomato", "lightblue", "gold", "lightgreen"), ylab = "a1c_weighted")
abline(h = 7.0, lty = 2, col = "red")
boxplot(n_a1c ~ group, data = df, main = "n_a1c by TP/TN/FP/FN",
        col = c("tomato", "lightblue", "gold", "lightgreen"), ylab = "# A1c readings")
boxplot(prob_glm_wt ~ group, data = df, main = "GLM+wt predicted prob by group",
        col = c("tomato", "lightblue", "gold", "lightgreen"), ylab = "P(Uncontrolled)")
abline(h = vp$th_glm_wt[1], lty = 2, col = "red")
dev.off()
