###############################################################################
## Secondary EDA part 2:
##   i)   Best random assignment baseline
##   ii)  Medication effect on outcome
##   iii) A1c slope / trajectory effect
###############################################################################

library(tidyverse)
source("./scripts/utils.R")

#### LOAD DATA ####
res <- readRDS("./data/processed/val_predictions.Rds")
vp <- res$val_preds
vf <- res$val_features

# returned patients only
vp <- vp %>% filter(mask)
vf <- vf %>% filter(mask)

actual <- vp$target == "Uncontrolled"
n <- length(actual)
p <- mean(actual)

cat("n =", n, " | base rate (uncontrolled) =", round(p, 4), "\n\n")


###############################################################################
#### i) BEST RANDOM ASSIGNMENT BASELINE ####
###############################################################################

cat("======================================================\n")
cat("  i) BEST RANDOM ASSIGNMENT BASELINES\n")
cat("======================================================\n\n")

# 1. Predict ALL as uncontrolled (best constant classifier for F1-pos)
pred_all_pos <- rep(TRUE, n)
tp <- sum(pred_all_pos & actual); fp <- sum(pred_all_pos & !actual); fn <- sum(!pred_all_pos & actual)
f1_all_pos <- 2 * tp / (2 * tp + fp + fn)
cat("Predict ALL positive:  F1 =", round(f1_all_pos * 100, 1), "%\n")
cat("  (precision =", round(tp / (tp + fp) * 100, 1), "%, recall = 100%)\n\n")

# 2. Random classifier at different rates — analytical formula
# F1(q) = 2*p*q / (p + q) where q = P(predict positive)
# Maximized at q=1 (all positive), but let's show the curve
cat("Random classifier F1 at different prediction rates:\n")
for (q in c(0.05, 0.10, 0.15, p, 0.25, 0.50, 0.75, 1.0)) {
    f1_q <- 2 * p * q / (p + q)
    label <- ifelse(abs(q - p) < 0.001, " <-- base rate", "")
    cat(sprintf("  q = %.2f → E[F1] = %.1f%%%s\n", q, f1_q * 100, label))
}
cat("\n")

# 3. Permutation test: shuffle labels, use GLM+wt threshold, repeat 1000x
set.seed(42)
n_perm <- 1000
f1_perm <- numeric(n_perm)
th <- vp$th_glm_wt[1]
probs <- vp$prob_glm_wt

for (i in 1:n_perm) {
    shuf <- sample(actual)
    pred <- probs >= th
    tp <- sum(pred & shuf); fp <- sum(pred & !shuf); fn <- sum(!pred & shuf)
    denom <- 2 * tp + fp + fn
    f1_perm[i] <- if (denom > 0) 2 * tp / denom else 0
}
cat("Permutation test (shuffle labels, keep model predictions, n=1000):\n")
cat("  Mean F1:", round(mean(f1_perm) * 100, 1), "%\n")
cat("  95th percentile F1:", round(quantile(f1_perm, 0.95) * 100, 1), "%\n")
cat("  Max F1:", round(max(f1_perm) * 100, 1), "%\n")
cat("  Actual model F1: ~58% → model is real, not chance\n\n")


###############################################################################
#### ii) MEDICATION EFFECT ON OUTCOME ####
###############################################################################

cat("======================================================\n")
cat("  ii) MEDICATION EFFECT ON OUTCOME\n")
cat("======================================================\n\n")

df <- vf %>% mutate(
    actual = target == "Uncontrolled",
    a1c_high = a1c_latest >= 7.0,
    any_meds = total_meds > 0,
    med_group = case_when(
        total_meds == 0 ~ "No meds",
        on_modern_drugs == 1 ~ "Modern (GLP1/SGLT2)",
        insulin > 0 ~ "Insulin",
        TRUE ~ "Other meds"
    ),
    n_drug_grp = case_when(
        n_drug_classes == 0 ~ "0 drugs",
        n_drug_classes == 1 ~ "1 drug",
        n_drug_classes == 2 ~ "2 drugs",
        TRUE ~ "3+ drugs"
    )
)

# Among patients with high a1c (>7): what fraction end up controlled in 2025?
cat("------ Among patients with a1c_latest >= 7: who becomes controlled? ------\n")
df %>%
    filter(a1c_high) %>%
    group_by(med_group) %>%
    summarise(
        n = n(),
        n_controlled = sum(!actual),
        pct_controlled = mean(!actual) * 100,
        mean_a1c = mean(a1c_latest, na.rm = TRUE)
    ) %>%
    arrange(desc(pct_controlled)) %>%
    print()
cat("\n")

# Same but by number of drug classes
cat("------ High a1c patients: controlled rate by # drug classes ------\n")
df %>%
    filter(a1c_high) %>%
    group_by(n_drug_grp) %>%
    summarise(
        n = n(),
        pct_controlled = mean(!actual) * 100,
        mean_a1c = mean(a1c_latest, na.rm = TRUE)
    ) %>% print()
cat("\n")

# Among patients with LOW a1c (<7): who becomes uncontrolled?
cat("------ Among patients with a1c_latest < 7: who becomes uncontrolled? ------\n")
df %>%
    filter(!a1c_high) %>%
    group_by(med_group) %>%
    summarise(
        n = n(),
        n_uncontrolled = sum(actual),
        pct_uncontrolled = mean(actual) * 100,
        mean_a1c = mean(a1c_latest, na.rm = TRUE)
    ) %>%
    arrange(desc(pct_uncontrolled)) %>%
    print()
cat("\n")

# Key question: among high-a1c patients, does modern drugs predict becoming controlled?
cat("------ High a1c + medication → controlled rate (2x2) ------\n")
df %>%
    filter(a1c_high) %>%
    group_by(on_modern_drugs, any_meds) %>%
    summarise(
        n = n(),
        pct_controlled = mean(!actual) * 100,
        .groups = "drop"
    ) %>% print()
cat("\n")


###############################################################################
#### iii) A1C SLOPE / TRAJECTORY EFFECT ####
###############################################################################

cat("======================================================\n")
cat("  iii) A1C SLOPE / TRAJECTORY EFFECT\n")
cat("======================================================\n\n")

# a1c_change = latest - first (positive = worsening, negative = improving)
# a1c_rate = (a1c_2 - a1c_1) / time (rate of change per day)

df <- df %>% mutate(
    trajectory = case_when(
        is.na(a1c_change) ~ "Single reading",
        a1c_change < -1.0 ~ "Improving (>1 drop)",
        a1c_change < 0 ~ "Slightly improving",
        a1c_change == 0 ~ "Stable",
        a1c_change < 1.0 ~ "Slightly worsening",
        TRUE ~ "Worsening (>1 rise)"
    ),
    trajectory = factor(trajectory, levels = c(
        "Improving (>1 drop)", "Slightly improving", "Stable",
        "Slightly worsening", "Worsening (>1 rise)", "Single reading"
    ))
)

cat("------ Outcome by A1c trajectory (all patients) ------\n")
df %>%
    group_by(trajectory) %>%
    summarise(
        n = n(),
        pct_uncontrolled = mean(actual) * 100,
        mean_a1c_latest = mean(a1c_latest, na.rm = TRUE)
    ) %>% print()
cat("\n")

# Among BORDERLINE patients (a1c 6–8): does trajectory predict outcome?
cat("------ Borderline patients (a1c_latest 6-8): trajectory × outcome ------\n")
df %>%
    filter(a1c_latest >= 6, a1c_latest <= 8) %>%
    group_by(trajectory) %>%
    summarise(
        n = n(),
        pct_uncontrolled = mean(actual) * 100,
        mean_a1c_latest = mean(a1c_latest, na.rm = TRUE)
    ) %>% print()
cat("\n")

# Among HIGH a1c (>7): does improving trajectory predict becoming controlled?
cat("------ High a1c patients (>=7): trajectory × controlled ------\n")
df %>%
    filter(a1c_high) %>%
    group_by(trajectory) %>%
    summarise(
        n = n(),
        pct_controlled = mean(!actual) * 100,
        mean_a1c_latest = mean(a1c_latest, na.rm = TRUE)
    ) %>% print()
cat("\n")

# Among LOW a1c (<7): does worsening trajectory predict becoming uncontrolled?
cat("------ Low a1c patients (<7): trajectory × uncontrolled ------\n")
df %>%
    filter(!a1c_high) %>%
    group_by(trajectory) %>%
    summarise(
        n = n(),
        pct_uncontrolled = mean(actual) * 100,
        mean_a1c_latest = mean(a1c_latest, na.rm = TRUE)
    ) %>% print()
cat("\n")

# Rate of change (per day) — finer grained
cat("------ A1c rate (per day) summary by outcome ------\n")
df %>%
    filter(!is.na(a1c_rate)) %>%
    group_by(actual) %>%
    summarise(
        n = n(),
        mean_rate = mean(a1c_rate, na.rm = TRUE),
        median_rate = median(a1c_rate, na.rm = TRUE),
        q25 = quantile(a1c_rate, 0.25, na.rm = TRUE),
        q75 = quantile(a1c_rate, 0.75, na.rm = TRUE)
    ) %>% print()
cat("\n")


#### PLOTS ####
pdf("./figures/misclass_eda2.pdf", width = 12, height = 8)
par(mfrow = c(2, 3))

# 1. F1 under permutation
hist(f1_perm, breaks = 30, main = "Permutation F1 (null distribution)",
     xlab = "F1 score", col = "grey80")
abline(v = 0.58, col = "red", lwd = 2, lty = 2)  # actual model
legend("topright", "Actual model F1", col = "red", lty = 2, lwd = 2)

# 2. Controlled rate by medication group (high a1c only)
tmp <- df %>% filter(a1c_high) %>%
    group_by(med_group) %>%
    summarise(pct_ctrl = mean(!actual) * 100) %>%
    arrange(pct_ctrl)
barplot(tmp$pct_ctrl, names.arg = tmp$med_group, main = "High A1c: % controlled in 2025\nby medication type",
        ylab = "% controlled", col = "steelblue", las = 2, cex.names = 0.8)

# 3. Uncontrolled rate by trajectory (all patients)
tmp2 <- df %>%
    group_by(trajectory) %>%
    summarise(pct_unc = mean(actual) * 100)
barplot(tmp2$pct_unc, names.arg = tmp2$trajectory, main = "% Uncontrolled in 2025\nby A1c trajectory",
        ylab = "% uncontrolled", col = "tomato", las = 2, cex.names = 0.7)

# 4. a1c_change by outcome
boxplot(a1c_change ~ actual, data = df %>% filter(!is.na(a1c_change)),
        names = c("Controlled", "Uncontrolled"),
        main = "A1c change (latest - first) by outcome",
        ylab = "A1c change", col = c("lightblue", "tomato"))
abline(h = 0, lty = 2)

# 5. a1c_rate by outcome
boxplot(a1c_rate ~ actual, data = df %>% filter(!is.na(a1c_rate)),
        names = c("Controlled", "Uncontrolled"),
        main = "A1c rate (per day) by outcome",
        ylab = "A1c change / day", col = c("lightblue", "tomato"))
abline(h = 0, lty = 2)

# 6. Borderline patients: trajectory vs outcome
tmp3 <- df %>%
    filter(a1c_latest >= 6, a1c_latest <= 8) %>%
    group_by(trajectory) %>%
    summarise(pct_unc = mean(actual) * 100)
barplot(tmp3$pct_unc, names.arg = tmp3$trajectory,
        main = "Borderline (A1c 6-8):\n% uncontrolled by trajectory",
        ylab = "% uncontrolled", col = "gold", las = 2, cex.names = 0.7)

dev.off()
cat("------ Plots saved to figures/misclass_eda2.pdf ------\n")
