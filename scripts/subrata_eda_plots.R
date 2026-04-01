library(ggplot2)

# Load and merge data
feat <- read.csv("./data/raw/DM Features.csv")
ctrl <- read.csv("./data/raw/DM Control_2025.csv")
df <- merge(feat, ctrl, by = "X")

# Derived columns
df$age <- 2025 - df$date.of.birth
df$label <- ifelse(df$a1c.2025.Uncontrolled == "True", "Uncontrolled", "Controlled")

# Latest A1c per patient (most recent available)
df$a1c_latest <- ifelse(!is.na(df$a1c.5.estimated.result), df$a1c.5.estimated.result,
                 ifelse(!is.na(df$a1c.4.estimated.result), df$a1c.4.estimated.result,
                 ifelse(!is.na(df$a1c.3.estimated.result), df$a1c.3.estimated.result,
                 ifelse(!is.na(df$a1c.2.estimated.result), df$a1c.2.estimated.result,
                        df$a1c.1.estimated.result))))

# Number of A1c readings per patient
df$n_a1c <- rowSums(!is.na(df[, c("a1c.1.estimated.result", "a1c.2.estimated.result",
                                    "a1c.3.estimated.result", "a1c.4.estimated.result",
                                    "a1c.5.estimated.result")]))

# Total medication orders
med_cols <- c("glp.1.orders.count", "insulin.orders.count", "metformin.orders.count",
              "sglt2.orders.count", "sulfonylurea.orders.count", "dpp4.orders.count")
df$total_meds <- rowSums(df[, med_cols], na.rm = TRUE)

# Abs change (a1c-1 minus a1c-2, for patients with 2+ readings)
df$abs_change_12 <- abs(df$a1c.1.estimated.result - df$a1c.2.estimated.result)


###############################################################################
## SUMMARY STATS (printed to console)
###############################################################################

cat("\n========== TARGET DISTRIBUTION ==========\n")
tbl <- table(df$a1c.2025.Uncontrolled)
print(data.frame(label = names(tbl), n = as.integer(tbl),
                 pct = round(as.integer(tbl) / sum(tbl) * 100, 1)))

cat("\n========== A1c-1 BY TARGET ==========\n")
print(aggregate(a1c.1.estimated.result ~ label, data = df,
                FUN = function(x) round(c(mean = mean(x), median = median(x),
                                           sd = sd(x), min = min(x), max = max(x)), 2)))

cat("\n========== VISITS BY TARGET ==========\n")
print(aggregate(cbind(ed.vist.count.count, pcp.visit.count.count, admission.count.count) ~ label,
                data = df, FUN = mean))

cat("\n========== MEDICATIONS BY TARGET ==========\n")
print(aggregate(cbind(glp.1.orders.count, insulin.orders.count, metformin.orders.count,
                      sglt2.orders.count, sulfonylurea.orders.count, dpp4.orders.count) ~ label,
                data = df, FUN = mean))

cat("\n========== N_A1C BY TARGET ==========\n")
print(table(df$label, df$n_a1c))

cat("\n========== GENDER vs TARGET ==========\n")
gender_tbl <- table(df$gender.at.birth, df$a1c.2025.Uncontrolled)
print(round(prop.table(gender_tbl, margin = 1) * 100, 1))

cat("\n========== RACE vs TARGET (uncontrolled rate) ==========\n")
race_tbl <- table(df$race...primary, df$a1c.2025.Uncontrolled)
race_rate <- data.frame(
  race = rownames(race_tbl),
  n = rowSums(race_tbl),
  uncontrolled_pct = round(race_tbl[, "True"] / rowSums(race_tbl) * 100, 1)
)
print(race_rate[order(-race_rate$uncontrolled_pct), ])

cat("\n========== A1c CHANGE (a1c-1 minus a1c-2, patients with 2+ readings) ==========\n")
df2 <- df[!is.na(df$a1c.2.estimated.result), ]
df2$raw_change <- df2$a1c.1.estimated.result - df2$a1c.2.estimated.result
print(aggregate(cbind(raw_change, abs_change_12) ~ label, data = df2,
                FUN = function(x) round(c(mean = mean(x), sd = sd(x)), 3)))

cat("\n========== CORRELATION: a1c_level vs a1c_change vs target ==========\n")
df2$target_num <- as.integer(df2$a1c.2025.Uncontrolled == "True")
print(round(cor(df2[, c("target_num", "a1c.1.estimated.result", "raw_change")],
                use = "complete.obs"), 3))


###############################################################################
## VOLATILITY FEATURES (patients with 3+ readings)
###############################################################################

df3 <- df[!is.na(df$a1c.3.estimated.result), ]
df3$r1 <- df3$a1c.1.estimated.result
df3$r2 <- df3$a1c.2.estimated.result
df3$r3 <- df3$a1c.3.estimated.result
df3$a1c_mean <- (df3$r1 + df3$r2 + df3$r3) / 3
df3$a1c_median <- apply(cbind(df3$r1, df3$r2, df3$r3), 1, median)
df3$a1c_sd <- sqrt(((df3$r1 - df3$a1c_mean)^2 + (df3$r2 - df3$a1c_mean)^2 +
                     (df3$r3 - df3$a1c_mean)^2) / 2)
df3$a1c_range <- pmax(df3$r1, df3$r2, df3$r3) - pmin(df3$r1, df3$r2, df3$r3)

# EWMA (lambda = 0.5, recent-weighted)
lambda <- 0.5
df3$ewma <- lambda * df3$r1 + lambda * (1 - lambda) * df3$r2 + (1 - lambda)^2 * df3$r3
df3$ewma_sd <- sqrt(lambda * (df3$r1 - df3$ewma)^2 +
                    lambda * (1 - lambda) * (df3$r2 - df3$ewma)^2 +
                    (1 - lambda)^2 * (df3$r3 - df3$ewma)^2)

cat("\n========== VOLATILITY CORRELATIONS (3+ readings subgroup) ==========\n")
df3$target_num <- as.integer(df3$a1c.2025.Uncontrolled == "True")
vol_cors <- c(
  cor_a1c1   = cor(df3$target_num, df3$r1),
  cor_mean   = cor(df3$target_num, df3$a1c_mean),
  cor_median = cor(df3$target_num, df3$a1c_median),
  cor_sd     = cor(df3$target_num, df3$a1c_sd),
  cor_ewma_sd = cor(df3$target_num, df3$ewma_sd),
  cor_range  = cor(df3$target_num, df3$a1c_range)
)
print(round(sort(vol_cors, decreasing = TRUE), 3))

cat("\n========== LAG-1 AUTOCORRELATION (consecutive changes) ==========\n")
df3$change_12 <- df3$r2 - df3$r1  # first to second
df3$change_23 <- df3$r3 - df3$r2  # second to third
print(aggregate(cbind(change_12, change_23) ~ label, data = df3, FUN = mean))
auto_cor <- aggregate(cbind(change_12, change_23) ~ label, data = df3,
                      FUN = function(x) x)
for (g in unique(df3$label)) {
  sub <- df3[df3$label == g, ]
  cat(sprintf("  %s: cor(change_12, change_23) = %.3f, cor(r1, r2) = %.3f\n",
              g, cor(sub$change_12, sub$change_23), cor(sub$r1, sub$r2)))
}


###############################################################################
## CORRELATION & MUTUAL INFORMATION
###############################################################################

df$target_num <- as.integer(df$a1c.2025.Uncontrolled == "True")
df$n_drug_classes <- rowSums(df[, med_cols] > 0, na.rm = TRUE)

# Helper: print cor and MI for a list of columns
print_cor_mi <- function(data, cols, labels, target_col = "target_num") {
  cat(sprintf("%-20s %10s %10s\n", "Variable", "Cor", "MI"))
  cat(strrep("-", 42), "\n")
  for (i in seq_along(cols)) {
    x <- data[[cols[i]]]
    y <- data[[target_col]]
    ok <- !is.na(x) & !is.na(y)
    cr <- cor(y[ok], x[ok])
    mi <- infotheo::mutinformation(infotheo::discretize(data.frame(x = x[ok])), y[ok])
    cat(sprintf("%-20s %10.4f %10.4f\n", labels[i], cr, mi))
  }
}

if (requireNamespace("infotheo", quietly = TRUE)) {
  library(infotheo)

  cat("\n========== MEDICATIONS & UTILIZATION: COR & MI (full dataset) ==========\n")
  print_cor_mi(df,
    cols = c(med_cols, "total_meds", "n_drug_classes",
             "ed.vist.count.count", "pcp.visit.count.count", "admission.count.count"),
    labels = c("GLP-1", "Insulin", "Metformin", "SGLT2", "Sulfonylurea", "DPP4",
               "Total meds", "N drug classes", "ED visits", "PCP visits", "Admissions"))

  cat("\n========== MUTUAL INFORMATION (full dataset, all key features) ==========\n")
  mi_cols_all <- c("a1c.1.estimated.result", "insulin.orders.count", "ed.vist.count.count",
               "pcp.visit.count.count", "admission.count.count", "date.of.birth",
               "ldl.estimated.result", "hdl.estimated.result")
  mi_results <- sapply(mi_cols_all, function(col) {
    x <- df[[col]]; y <- df$target_num
    ok <- !is.na(x) & !is.na(y)
    mutinformation(discretize(data.frame(x = x[ok])), y[ok])
  })
  mi_results <- c(mi_results,
    n_a1c = mutinformation(discretize(data.frame(x = df$n_a1c)), df$target_num),
    abs_change = {
      ok <- !is.na(df$abs_change_12)
      mutinformation(discretize(data.frame(x = df$abs_change_12[ok])), df$target_num[ok])
    }
  )
  print(round(sort(mi_results, decreasing = TRUE), 4))

  cat("\n========== VOLATILITY: COR & MI (2+ readings, n=", sum(!is.na(df$abs_change_12)), ") ==========\n")
  df2_mi <- df[!is.na(df$abs_change_12), ]
  df2_mi$raw_change <- df2_mi$a1c.1.estimated.result - df2_mi$a1c.2.estimated.result
  print_cor_mi(df2_mi,
    cols = c("abs_change_12", "raw_change"),
    labels = c("abs_change", "raw_change"))

  cat("\n========== VOLATILITY: COR & MI (3+ readings, n=", nrow(df3), ") ==========\n")
  df3$target_num <- as.integer(df3$a1c.2025.Uncontrolled == "True")
  print_cor_mi(df3,
    cols = c("a1c_mean", "a1c_median", "r3", "r1", "a1c_range", "a1c_sd", "ewma_sd", "abs_change_12"),
    labels = c("Mean", "Median", "a1c-3 (latest)", "a1c-1 (earliest)",
               "Range", "SD", "EWMA-SD", "abs_change"))

  cat("\n========== N_A1C: COR & MI (full dataset) ==========\n")
  print_cor_mi(df, cols = "n_a1c", labels = "n_a1c")

  cat("\n========== WEIGHTED MEANS: COR & MI (3+ readings) ==========\n")
  # r1 = earliest, r3 = most recent (latest)
  df3$wm_last_light   <- 0.2 * df3$r1 + 0.3 * df3$r2 + 0.5 * df3$r3
  df3$wm_last_heavy   <- 0.1 * df3$r1 + 0.2 * df3$r2 + 0.7 * df3$r3
  df3$wm_last_extreme <- 0.05 * df3$r1 + 0.1 * df3$r2 + 0.85 * df3$r3
  df3$wm_first_light  <- 0.5 * df3$r1 + 0.3 * df3$r2 + 0.2 * df3$r3
  df3$wm_first_heavy  <- 0.7 * df3$r1 + 0.2 * df3$r2 + 0.1 * df3$r3
  df3$ewma_05_latest  <- 0.5 * df3$r3 + 0.25 * df3$r2 + 0.25 * df3$r1
  df3$ewma_07_latest  <- 0.7 * df3$r3 + 0.21 * df3$r2 + 0.09 * df3$r1

  print_cor_mi(df3,
    cols = c("r1", "r3", "a1c_mean",
             "wm_last_light", "wm_last_heavy", "wm_last_extreme",
             "wm_first_light", "wm_first_heavy",
             "ewma_05_latest", "ewma_07_latest"),
    labels = c("a1c-1 (earliest)", "a1c-3 (latest)", "equal (1/3)",
               "last-light (.2/.3/.5)", "last-heavy (.1/.2/.7)", "last-extreme (.05/.1/.85)",
               "first-light (.5/.3/.2)", "first-heavy (.7/.2/.1)",
               "EWMA-0.5 (latest)", "EWMA-0.7 (latest)"))

} else {
  cat("\n[SKIPPED] infotheo package not installed — MI sections skipped.\n")
}

cat("\n========== ADI BY TARGET ==========\n")
df$adi_state <- suppressWarnings(as.numeric(df$adi.adi.state.rank))
df$adi_national <- suppressWarnings(as.numeric(df$adi.adi.national.rank))
adi_sub <- df[!is.na(df$adi_state), ]
print(aggregate(cbind(adi_state, adi_national) ~ label, data = adi_sub,
                FUN = function(x) round(c(mean = mean(x, na.rm = TRUE),
                                           median = median(x, na.rm = TRUE)), 2)))


###############################################################################
## COMORBIDITIES, BMI, INSURANCE, TIMING, INTERACTIONS
###############################################################################

cat("\n========== COMORBIDITIES: CAD & COPD ==========\n")
df$has_cad <- df$cad.count > 0
df$has_copd <- df$copd.count > 0
for (v in c("cad.count", "copd.count")) {
  cr <- cor(df$target_num, df[[v]], use = "complete.obs")
  cat(sprintf("  %s: cor=%.4f\n", v, cr))
}
cat("Uncontrolled rate: CAD present =",
    round(mean(df$target_num[df$has_cad]) * 100, 1), "%, absent =",
    round(mean(df$target_num[!df$has_cad]) * 100, 1), "%\n")
cat("Uncontrolled rate: COPD present =",
    round(mean(df$target_num[df$has_copd]) * 100, 1), "%, absent =",
    round(mean(df$target_num[!df$has_copd]) * 100, 1), "%\n")

cat("\n========== BMI ==========\n")
has_both <- !is.na(df$weight.estimated.result) & !is.na(df$height.estimated.result)
df$bmi <- ifelse(has_both,
                 df$weight.estimated.result / (df$height.estimated.result / 100)^2, NA)
df$bmi <- ifelse(!is.na(df$bmi) & df$bmi >= 10 & df$bmi <= 80, df$bmi, NA)
bmi_sub <- df[!is.na(df$bmi), ]
cat("n =", nrow(bmi_sub), "\n")
print(aggregate(bmi ~ label, data = bmi_sub,
                FUN = function(x) round(c(mean = mean(x), median = median(x)), 2)))
cat(sprintf("  BMI: cor=%.4f\n", cor(bmi_sub$target_num, bmi_sub$bmi)))

cat("\n========== INSURANCE TYPE ==========\n")
ins <- df[df$payor.first.op.visit.primary.insurance.plan != "", ]
cat("n =", nrow(ins), "(", round(nrow(ins) / nrow(df) * 100, 1), "% )\n")
ins_rate <- aggregate(target_num ~ payor.first.op.visit.primary.insurance.plan, data = ins,
                      FUN = function(x) c(rate = round(mean(x), 3), n = length(x)))
ins_df <- data.frame(insurance = ins_rate[[1]],
                     rate = ins_rate[[2]][, "rate"], n = ins_rate[[2]][, "n"])
print(ins_df[order(-ins_df$rate), ])

cat("\n========== LIPID & VITALS TIMING ==========\n")
timing_cols <- c("ldl.collection.date.time.days.from.reference",
                 "hdl.collection.date.time.days.from.reference",
                 "total.cholesterol.collection.date.time.days.from.reference",
                 "weight.measurement.date.time.days.from.reference",
                 "height.measurement.date.time.days.from.reference")
timing_labels <- c("LDL timing", "HDL timing", "TC timing", "Weight timing", "Height timing")
for (i in seq_along(timing_cols)) {
  x <- df[[timing_cols[i]]]
  ok <- !is.na(x)
  cr <- cor(df$target_num[ok], x[ok])
  cat(sprintf("  %s: cor=%.4f (n=%d)\n", timing_labels[i], cr, sum(ok)))
}

cat("\n========== INTERACTIONS ==========\n")
df$high_a1c <- df$a1c_latest >= 7.0
df$on_insulin <- df$insulin.orders.count > 0
cat("Uncontrolled rate by A1c level x insulin:\n")
print(aggregate(target_num ~ high_a1c + on_insulin, data = df,
                FUN = function(x) c(rate = round(mean(x), 3), n = length(x))))
df$a1c_x_meds <- df$a1c_latest * df$total_meds
cr <- cor(df$target_num, df$a1c_x_meds, use = "complete.obs")
cat(sprintf("  a1c_latest * total_meds: cor=%.4f\n", cr))

cat("\n========== AGE-STRATIFIED ANALYSIS ==========\n")
df$age_group <- cut(df$age, breaks = c(20, 35, 45, 55, 65, 75, 85, 105),
                    labels = c("20-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
cat("Uncontrolled rate by age group:\n")
ag <- aggregate(target_num ~ age_group, data = df, FUN = function(x) c(rate = round(mean(x), 3), n = length(x)))
print(ag)
cat("\nMean A1c gap (uncontrolled - controlled) by age:\n")
for (g in levels(df$age_group)) {
  sub <- df[df$age_group == g, ]
  m0 <- mean(sub$a1c_latest[sub$target_num == 0], na.rm = TRUE)
  m1 <- mean(sub$a1c_latest[sub$target_num == 1], na.rm = TRUE)
  cat(sprintf("  %s: gap=%.2f (ctrl=%.2f, unctrl=%.2f)\n", g, m1 - m0, m0, m1))
}
cat(sprintf("\nAge: cor=%.4f with target\n", cor(df$age, df$target_num)))


###############################################################################
## COMPREHENSIVE COR & MI FOR ALL REMAINING VARIABLES
###############################################################################

if (requireNamespace("infotheo", quietly = TRUE)) {
  library(infotheo)

  cat("\n========== COMORBIDITIES: COR & MI ==========\n")
  print_cor_mi(df,
    cols = c("cad.count", "copd.count"),
    labels = c("CAD count", "COPD count"))

  cat("\n========== BMI: COR & MI (n=", sum(!is.na(df$bmi)), ") ==========\n")
  bmi_mi_sub <- df[!is.na(df$bmi), ]
  if (nrow(bmi_mi_sub) > 0) {
    print_cor_mi(bmi_mi_sub, cols = "bmi", labels = "BMI")
  } else {
    cat("  No valid BMI values to compute.\n")
  }

  cat("\n========== ADI: COR & MI ==========\n")
  adi_sub2 <- df[!is.na(df$adi_state) & !is.na(df$adi_national), ]
  cat("n =", nrow(adi_sub2), "\n")
  print_cor_mi(adi_sub2,
    cols = c("adi_state", "adi_national"),
    labels = c("ADI state rank", "ADI national rank"))

  cat("\n========== AGE & LATEST A1c: COR & MI ==========\n")
  print_cor_mi(df,
    cols = c("age", "a1c_latest"),
    labels = c("Age", "Latest A1c"))

  cat("\n========== LIPID VALUES: COR & MI ==========\n")
  print_cor_mi(df,
    cols = c("ldl.estimated.result", "hdl.estimated.result",
             "total.cholesterol.estimated.result"),
    labels = c("LDL", "HDL", "Total cholesterol"))

  cat("\n========== LIPID & VITALS TIMING: COR & MI ==========\n")
  print_cor_mi(df,
    cols = c("ldl.collection.date.time.days.from.reference",
             "hdl.collection.date.time.days.from.reference",
             "total.cholesterol.collection.date.time.days.from.reference",
             "weight.measurement.date.time.days.from.reference",
             "height.measurement.date.time.days.from.reference"),
    labels = c("LDL timing", "HDL timing", "TC timing",
               "Weight timing", "Height timing"))

  cat("\n========== INTERACTIONS: COR & MI ==========\n")
  df$high_a1c_insulin <- as.integer(df$high_a1c & df$on_insulin)
  print_cor_mi(df,
    cols = c("high_a1c_insulin", "a1c_x_meds"),
    labels = c("high_a1c AND on_insulin", "a1c_latest * total_meds"))

  cat("\n========== CATEGORICAL VARIABLES: MI ==========\n")
  # Gender
  df$gender_num <- as.integer(factor(df$gender.at.birth))
  ok_g <- !is.na(df$gender_num)
  mi_gender <- mutinformation(discretize(data.frame(x = df$gender_num[ok_g])),
                              df$target_num[ok_g])
  cat(sprintf("  Gender:    MI = %.4f (n=%d)\n", mi_gender, sum(ok_g)))

  # Race
  df$race_num <- as.integer(factor(df$race...primary))
  ok_r <- !is.na(df$race_num) & df$race...primary != ""
  mi_race <- mutinformation(discretize(data.frame(x = df$race_num[ok_r])),
                            df$target_num[ok_r])
  cat(sprintf("  Race:      MI = %.4f (n=%d)\n", mi_race, sum(ok_r)))

  # Ethnicity
  df$eth_num <- as.integer(factor(df$ethnicity))
  ok_e <- !is.na(df$eth_num) & df$ethnicity != ""
  mi_eth <- mutinformation(discretize(data.frame(x = df$eth_num[ok_e])),
                           df$target_num[ok_e])
  cat(sprintf("  Ethnicity: MI = %.4f (n=%d)\n", mi_eth, sum(ok_e)))

  # Insurance
  ok_ins <- df$payor.first.op.visit.primary.insurance.plan != ""
  if (sum(ok_ins) > 0) {
    df$ins_num <- as.integer(factor(df$payor.first.op.visit.primary.insurance.plan))
    mi_ins <- mutinformation(discretize(data.frame(x = df$ins_num[ok_ins])),
                             df$target_num[ok_ins])
    cat(sprintf("  Insurance: MI = %.4f (n=%d)\n", mi_ins, sum(ok_ins)))
  }
}


###############################################################################
## PLOTS
###############################################################################

# Crisp theme: minimal + thinner lines throughout
theme_crisp <- function(sz = 14) {
  theme_minimal(base_size = sz, base_line_size = 0.2, base_rect_size = 0.2) +
    theme(panel.grid.minor = element_blank())
}

# --- Plot 1: Age vs Latest A1c by outcome ---
cat("\nPlot 1: Age vs Latest A1c...\n")
p1 <- ggplot(df, aes(x = age, y = a1c_latest, color = label)) +
  geom_point(alpha = 0.03, size = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, linewidth = 0.8) +
  scale_color_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  labs(x = "Age (2025 - birth year)", y = "Latest A1c (%)",
       color = "2025 Status",
       title = "Age vs Latest A1c by Uncontrolled Status") +
  theme_crisp(14) +
  theme(legend.position = "top")
ggsave("./figures/plot1_age_vs_a1c.png", p1, width = 10, height = 7, dpi = 150)


# --- Plot 2: A1c trajectory spaghetti plot ---
cat("Plot 2: A1c trajectory spaghetti...\n")
set.seed(42)
has3 <- df[df$n_a1c >= 3, ]
samp_ctrl <- has3[has3$label == "Controlled", ][sample(sum(has3$label == "Controlled"), 200), ]
samp_uncl <- has3[has3$label == "Uncontrolled", ][sample(sum(has3$label == "Uncontrolled"), 200), ]
samp <- rbind(samp_ctrl, samp_uncl)

long <- do.call(rbind, lapply(1:5, function(i) {
  day_col <- paste0("a1c.", i, ".collection.date.time.days.from.reference")
  val_col <- paste0("a1c.", i, ".estimated.result")
  out <- data.frame(
    id = samp$X, label = samp$label,
    day = samp[[day_col]], a1c = samp[[val_col]]
  )
  out[!is.na(out$a1c) & !is.na(out$day), ]
}))

p2 <- ggplot(long, aes(x = day, y = a1c, group = id, color = label)) +
  geom_line(alpha = 0.15, linewidth = 0.25) +
  geom_smooth(aes(group = label), method = "loess", se = TRUE, linewidth = 0.9) +
  scale_color_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  labs(x = "Days from first appointment", y = "A1c (%)",
       color = "2025 Status",
       title = "A1c Trajectory Over Time (sample, patients with 3+ readings)") +
  theme_crisp(14) +
  theme(legend.position = "top")
ggsave("./figures/plot2_trajectory_spaghetti.png", p2, width = 10, height = 7, dpi = 150)


# --- Plot 3: Number of readings vs uncontrolled rate ---
cat("Plot 3: n_a1c vs uncontrolled rate...\n")
rate_by_n <- aggregate(a1c.2025.Uncontrolled == "True" ~ n_a1c, data = df, FUN = function(x) {
  c(rate = mean(x), n = length(x))
})
rate_df <- data.frame(
  n_a1c = rate_by_n$n_a1c,
  rate = rate_by_n[[2]][, "rate"],
  n = rate_by_n[[2]][, "n"]
)
p3 <- ggplot(rate_df, aes(x = factor(n_a1c), y = rate)) +
  geom_col(fill = "#1B9E77", width = 0.6) +
  geom_text(aes(label = paste0("n=", format(n, big.mark = ","))), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, max(rate_df$rate) * 1.15)) +
  labs(x = "Number of A1c Readings", y = "Uncontrolled Rate (%)",
       title = "Uncontrolled Rate by Number of Historical A1c Readings") +
  theme_crisp(14)
ggsave("./figures/plot3_n_a1c_vs_rate.png", p3, width = 8, height = 6, dpi = 150)


# --- Plot 3b: N_a1c proportions by target (dodged bar) ---
cat("Plot 3b: n_a1c proportions by target...\n")
n_a1c_pct <- as.data.frame(table(df$label, df$n_a1c))
names(n_a1c_pct) <- c("label", "n_a1c", "n")
n_a1c_pct <- do.call(rbind, lapply(split(n_a1c_pct, n_a1c_pct$label), function(x) {
  x$pct <- x$n / sum(x$n) * 100; x
}))
p3b <- ggplot(n_a1c_pct, aes(x = n_a1c, y = pct, fill = label)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  labs(x = "Number of A1c Readings", y = "Percent within group",
       fill = "2025 Status",
       title = "% Patients by Number of A1c Readings") +
  theme_crisp(14) +
  theme(legend.position = "top")
ggsave("./figures/plot3b_n_a1c_proportions.png", p3b, width = 8, height = 6, dpi = 150)


# --- Plot 4: Days to 2025 evaluation ---
cat("Plot 4: Days to 2025 evaluation...\n")
df_with_date <- df[!is.na(df$a1c.2025.collection.date.time.days.from.reference), ]
p4 <- ggplot(df_with_date, aes(x = a1c.2025.collection.date.time.days.from.reference, fill = label)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  labs(x = "Days from first appointment to 2025 evaluation",
       y = "Count", fill = "2025 Status",
       title = "Distribution of Future Evaluation Timing by Outcome") +
  theme_crisp(14) +
  theme(legend.position = "top")
ggsave("./figures/plot4_days_to_eval.png", p4, width = 10, height = 6, dpi = 150)


# --- Plot 5a: Total medication orders boxplot ---
cat("Plot 5a: Total meds boxplot...\n")
p5a <- ggplot(df, aes(x = label, y = total_meds, fill = label)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.3, linewidth = 0.3) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  labs(x = "", y = "Total Medication Orders",
       title = "Total Diabetes Medication Orders by Outcome") +
  theme_crisp(14) +
  theme(legend.position = "none")
ggsave("./figures/plot5a_total_meds_boxplot.png", p5a, width = 7, height = 6, dpi = 150)


# --- Plot 5b: Mean medication orders by drug class ---
cat("Plot 5b: Meds by drug class...\n")
med_long <- do.call(rbind, lapply(med_cols, function(mc) {
  nice_name <- gsub("\\.orders\\.count", "", mc)
  nice_name <- gsub("\\.", "-", nice_name)
  data.frame(drug = nice_name, label = df$label, orders = df[[mc]])
}))
med_means <- aggregate(orders ~ drug + label, data = med_long, FUN = mean)
p5b <- ggplot(med_means, aes(x = reorder(drug, orders), y = orders, fill = label)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  coord_flip() +
  labs(x = "", y = "Mean Order Count", fill = "2025 Status",
       title = "Mean Medication Orders by Drug Class and Outcome") +
  theme_crisp(14) +
  theme(legend.position = "top")
ggsave("./figures/plot5b_med_by_class.png", p5b, width = 9, height = 6, dpi = 150)


# --- Plot 6: A1c-1 histogram by target ---
cat("Plot 6: A1c-1 histogram...\n")
p6 <- ggplot(df, aes(x = a1c.1.estimated.result, fill = label)) +
  geom_histogram(bins = 50, position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  labs(x = "A1c-1 (earliest reading, %)", y = "Count", fill = "2025 Status",
       title = "A1c-1 Distribution by Target") +
  theme_crisp(14) +
  theme(legend.position = "top")
ggsave("./figures/plot6_a1c1_histogram.png", p6, width = 10, height = 6, dpi = 150)


# --- Plot 7: A1c-1 vs Birth Year scatter ---
cat("Plot 7: A1c-1 vs birth year...\n")
p7 <- ggplot(df, aes(x = a1c.1.estimated.result, y = date.of.birth)) +
  geom_point(alpha = 0.03, size = 0.3) +
  labs(x = "A1c-1 (%)", y = "Birth Year",
       title = "A1c-1 vs Birth Year") +
  theme_crisp(14)
ggsave("./figures/plot7_a1c1_vs_birthyear.png", p7, width = 8, height = 7, dpi = 150)


# --- Plot 8: Boxplots — key numerics vs target (2x3 grid) ---
cat("Plot 8: Key numeric boxplots...\n")
box_vars <- data.frame(
  col = c("a1c.1.estimated.result", "date.of.birth", "ed.vist.count.count",
          "pcp.visit.count.count", "insulin.orders.count", "admission.count.count"),
  title = c("A1c-1", "Birth Year", "ED Visits", "PCP Visits", "Insulin Orders", "Admissions")
)
box_long <- do.call(rbind, lapply(1:nrow(box_vars), function(i) {
  data.frame(
    variable = box_vars$title[i],
    label = df$label,
    value = df[[box_vars$col[i]]]
  )
}))
box_long$variable <- factor(box_long$variable, levels = box_vars$title)
p8 <- ggplot(box_long, aes(x = label, y = value, fill = label)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2, linewidth = 0.3) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  labs(x = "", y = "") +
  theme_crisp(12) +
  theme(legend.position = "none")
ggsave("./figures/plot8_key_boxplots.png", p8, width = 12, height = 8, dpi = 150)


# --- Plot 9: Lipid boxplots ---
cat("Plot 9: Lipid boxplots...\n")
lipid_vars <- data.frame(
  col = c("ldl.estimated.result", "hdl.estimated.result", "total.cholesterol.estimated.result"),
  title = c("LDL", "HDL", "Total Cholesterol")
)
lipid_long <- do.call(rbind, lapply(1:nrow(lipid_vars), function(i) {
  data.frame(variable = lipid_vars$title[i], label = df$label, value = df[[lipid_vars$col[i]]])
}))
lipid_long$variable <- factor(lipid_long$variable, levels = lipid_vars$title)
p9 <- ggplot(lipid_long, aes(x = label, y = value, fill = label)) +
  geom_violin(linewidth = 0.3, alpha = 0.8, trim = TRUE) +
  geom_boxplot(width = 0.12, outlier.shape = NA, linewidth = 0.2, fill = "white", alpha = 0.6) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  labs(x = "", y = "") +
  theme_crisp(12) +
  theme(legend.position = "none")
ggsave("./figures/plot9_lipid_boxplots.png", p9, width = 12, height = 5, dpi = 150)


# --- Plot 10: A1c-1 vs A1c-2 scatter (faceted by target) ---
cat("Plot 10: A1c-1 vs A1c-2 scatter...\n")
df2_plot <- df[!is.na(df$a1c.2.estimated.result), ]
p10 <- ggplot(df2_plot, aes(x = a1c.2.estimated.result, y = a1c.1.estimated.result)) +
  geom_point(alpha = 0.15, size = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 0.3) +
  facet_wrap(~label) +
  coord_cartesian(xlim = c(4, 15), ylim = c(4, 15)) +
  labs(x = "A1c-2 (later reading)", y = "A1c-1 (earliest reading)",
       title = "A1c-1 vs A1c-2 by Target") +
  theme_crisp(14)
ggsave("./figures/plot10_a1c1_vs_a1c2.png", p10, width = 12, height = 6, dpi = 150)


# --- Plot 11: Absolute A1c change histogram ---
cat("Plot 11: Absolute A1c change...\n")
df2_plot$abs_change <- abs(df2_plot$a1c.1.estimated.result - df2_plot$a1c.2.estimated.result)
p11 <- ggplot(df2_plot, aes(x = abs_change, fill = label)) +
  geom_histogram(bins = 50, position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  coord_cartesian(xlim = c(0, 8)) +
  labs(x = "|A1c-1 - A1c-2|", y = "Count", fill = "2025 Status",
       title = "Absolute A1c Change Between Readings 1 and 2") +
  theme_crisp(14) +
  theme(legend.position = "top")
ggsave("./figures/plot11_abs_change_histogram.png", p11, width = 10, height = 6, dpi = 150)


# --- Plot 12: Mean A1c trajectory with error bars (3+ readings) ---
cat("Plot 12: Mean trajectory with SE...\n")
# Reshape 3+ reading patients to long format (reading number, not days)
long3 <- do.call(rbind, lapply(1:3, function(i) {
  val_col <- paste0("a1c.", i, ".estimated.result")
  data.frame(label = df3$label, reading = i, a1c = df3[[val_col]])
}))
p12 <- ggplot(long3, aes(x = reading, y = a1c, color = label)) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, linewidth = 0.3) +
  scale_color_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  scale_x_continuous(breaks = 1:3, labels = c("1 (earliest)", "2", "3 (latest)")) +
  labs(x = "Reading", y = "A1c (%)", color = "2025 Status",
       title = "Mean A1c Trajectory (patients with 3+ readings)") +
  theme_crisp(14) +
  theme(legend.position = "top")
ggsave("./figures/plot12_mean_trajectory.png", p12, width = 8, height = 6, dpi = 150)


# --- Plot 13: Faceted mean trajectory with SE ribbon ---
cat("Plot 13: Faceted trajectory with SE ribbon...\n")
p13 <- ggplot(long3, aes(x = reading, y = a1c)) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.7) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.3) +
  facet_wrap(~label) +
  scale_x_continuous(breaks = 1:3, labels = c("1 (earliest)", "2", "3 (latest)")) +
  labs(x = "Reading", y = "A1c (%)",
       title = "Mean A1c Trajectory +/- SE") +
  theme_crisp(14)
ggsave("./figures/plot13_trajectory_faceted.png", p13, width = 10, height = 5, dpi = 150)


# --- Plot 14: Volatility boxplots (SD, EWMA-SD, Range) ---
cat("Plot 14: Volatility boxplots...\n")
vol_long <- rbind(
  data.frame(measure = "SD", label = df3$label, value = df3$a1c_sd),
  data.frame(measure = "EWMA-SD (lambda=0.5)", label = df3$label, value = df3$ewma_sd),
  data.frame(measure = "Range", label = df3$label, value = df3$a1c_range)
)
vol_long$measure <- factor(vol_long$measure, levels = c("SD", "EWMA-SD (lambda=0.5)", "Range"))
p14 <- ggplot(vol_long, aes(x = label, y = value, fill = label)) +
  geom_violin(linewidth = 0.3, alpha = 0.8, trim = TRUE) +
  geom_boxplot(width = 0.12, outlier.shape = NA, linewidth = 0.2, fill = "white", alpha = 0.6) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  facet_wrap(~measure, scales = "free_y", ncol = 3) +
  labs(x = "", y = "",
       title = "A1c Volatility Measures by Outcome (3+ readings)") +
  theme_crisp(12) +
  theme(legend.position = "none")
ggsave("./figures/plot14_volatility_boxplots.png", p14, width = 12, height = 5, dpi = 150)


# --- Plot 15: ADI boxplot ---
cat("Plot 15: ADI boxplot...\n")
adi_plot <- df[!is.na(df$adi_state), ]
p15 <- ggplot(adi_plot, aes(x = label, y = adi_state, fill = label)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2, linewidth = 0.3) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  labs(x = "", y = "ADI State Rank (higher = more deprived)",
       title = "Area Deprivation Index (State Rank) by Outcome") +
  theme_crisp(14) +
  theme(legend.position = "none")
ggsave("./figures/plot15_adi_boxplot.png", p15, width = 7, height = 6, dpi = 150)


# --- Plot 16: Latest A1c boxplot by target ---
cat("Plot 16: Latest A1c boxplot...\n")
p16 <- ggplot(df, aes(x = label, y = a1c_latest, fill = label)) +
  geom_violin(linewidth = 0.3, alpha = 0.8, trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, linewidth = 0.2, fill = "white", alpha = 0.6) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  labs(x = "", y = "Latest A1c (%)",
       title = "Latest Available A1c by Uncontrolled Status") +
  theme_crisp(14) +
  theme(legend.position = "none")
ggsave("./figures/plot16_latest_a1c_boxplot.png", p16, width = 7, height = 6, dpi = 150)



# --- Check: does n_a1c = 4 exist? (plot3 skips it) ---
cat("\n========== N_A1C DISTRIBUTION CHECK ==========\n")
print(table(df$n_a1c))
cat("(If n_a1c=4 has 0 patients, that explains the gap in plot3.)\n")


# --- Plot 17: Missingness heatmap by HasFutureEval vs NoFutureEval ---
cat("Plot 17: Missingness heatmap by group...\n")
n_has <- sum(!is.na(df$a1c.2025.collection.date.time.days.from.reference))
n_no  <- sum(is.na(df$a1c.2025.collection.date.time.days.from.reference))
df$has_future_eval <- ifelse(!is.na(df$a1c.2025.collection.date.time.days.from.reference),
                             paste0("HasFutureEval (", format(n_has, big.mark = ","), ")"),
                             paste0("NoFutureEval (", format(n_no, big.mark = ","), ")"))
miss_cols <- c("weight.estimated.result", "height.estimated.result",
               "weight.measurement.date.time.days.from.reference",
               "height.measurement.date.time.days.from.reference",
               "ldl.estimated.result", "hdl.estimated.result",
               "total.cholesterol.estimated.result",
               "ldl.collection.date.time.days.from.reference",
               "hdl.collection.date.time.days.from.reference",
               "total.cholesterol.collection.date.time.days.from.reference")
miss_labels <- c("Weight", "Height", "Weight timing", "Height timing",
                 "LDL", "HDL", "Total Chol", "LDL timing", "HDL timing", "TC timing")
miss_long <- do.call(rbind, lapply(seq_along(miss_cols), function(i) {
  data.frame(
    variable = miss_labels[i],
    group = df$has_future_eval,
    missing = is.na(df[[miss_cols[i]]])
  )
}))
miss_rate <- aggregate(missing ~ variable + group, data = miss_long, FUN = mean)
miss_rate$variable <- factor(miss_rate$variable, levels = rev(miss_labels))
p17 <- ggplot(miss_rate, aes(x = group, y = variable, fill = missing)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(missing * 100, 1), "%")), size = 3.5) +
  scale_fill_gradient(low = "#1B9E77", high = "#D95F02",
                      labels = scales::percent_format(), name = "% Missing") +
  labs(x = "", y = "",
       title = "Missingness Rates: HasFutureEval vs NoFutureEval",
       subtitle = "Systematic differences suggest the two groups are structurally different") +
  theme_crisp(13) +
  theme(legend.position = "right", panel.grid = element_blank())
ggsave("./figures/plot17_missingness_heatmap.png", p17, width = 10, height = 6, dpi = 150)


# --- Plot 18: Insurance type uncontrolled rate bar chart ---
cat("Plot 18: Insurance uncontrolled rate...\n")
ins_sub <- df[df$payor.first.op.visit.primary.insurance.plan != "", ]
ins_rate <- aggregate(target_num ~ payor.first.op.visit.primary.insurance.plan,
                      data = ins_sub,
                      FUN = function(x) c(rate = mean(x), n = length(x)))
ins_df2 <- data.frame(
  insurance = ins_rate[[1]],
  rate = ins_rate[[2]][, "rate"],
  n = ins_rate[[2]][, "n"]
)
ins_df2 <- ins_df2[ins_df2$n >= 50, ]  # drop tiny groups
p18 <- ggplot(ins_df2, aes(x = reorder(insurance, rate), y = rate)) +
  geom_col(fill = "#1B9E77", width = 0.6) +
  geom_text(aes(label = paste0("n=", format(n, big.mark = ","))),
            hjust = -0.1, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, max(ins_df2$rate) * 1.25)) +
  coord_flip() +
  labs(x = "", y = "Uncontrolled Rate",
       title = "Uncontrolled Rate by Insurance Type",
       subtitle = "Groups with n >= 50 only") +
  theme_crisp(13)
ggsave("./figures/plot18_insurance_rate.png", p18, width = 10, height = 6, dpi = 150)


###############################################################################
## RETURNED vs DIDN'T RETURN — Two subpopulation comparison
## (Office hours 2026-03-27: both groups have real labels, train on all 62,425)
###############################################################################

df$returned_2025 <- !is.na(df$a1c.2025.collection.date.time.days.from.reference)

## Cross-tab: returned x uncontrolled
cat("\n====== TWO SUBPOPULATIONS: returned vs didn't return ======\n")
print(table(returned = df$returned_2025, uncontrolled = df$a1c.2025.Uncontrolled))

## Uncontrolled rate among those who returned
cat("\n====== UNCONTROLLED RATE AMONG RETURNED ======\n")
returned_tab <- table(df$a1c.2025.Uncontrolled[df$returned_2025])
print(returned_tab)
cat("Uncontrolled rate:", round(returned_tab["True"] / sum(returned_tab) * 100, 1), "%\n")

## Clinical profile comparison
cat("\n====== CLINICAL PROFILE BY GROUP ======\n")
profile <- aggregate(
  cbind(a1c.1.estimated.result, insulin.orders.count,
        ed.vist.count.count, n_a1c, total_meds, a1c_latest) ~ returned_2025,
  data = df, FUN = mean, na.rm = TRUE
)
names(profile) <- c("returned", "mean_a1c1", "mean_insulin", "mean_ed",
                     "mean_n_a1c", "mean_total_meds", "mean_a1c_latest")
print(profile)

## Plot: A1c distribution by returned status
p19 <- ggplot(df, aes(x = a1c.1.estimated.result,
                       fill = ifelse(returned_2025, "Returned", "Didn't return"))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Returned" = "#D95F02", "Didn't return" = "#7570B3")) +
  labs(x = "Baseline A1c", y = "Density", fill = "2025 status",
       title = "Baseline A1c Distribution: Returned vs Didn't Return") +
  theme_crisp(13)
ggsave("./figures/plot19_returned_a1c_dist.png", p19, width = 10, height = 6, dpi = 150)

## Plot: uncontrolled rate by returned status (stacked bar)
cross_df <- as.data.frame(table(
  returned = ifelse(df$returned_2025, "Returned", "Didn't return"),
  uncontrolled = df$label
))
cross_df$pct <- ave(cross_df$Freq, cross_df$returned, FUN = function(x) x / sum(x))
p20 <- ggplot(cross_df, aes(x = returned, y = pct, fill = uncontrolled)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(format(Freq, big.mark = ","), "\n(",
                                round(pct * 100, 1), "%)")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Controlled" = "#1B9E77", "Uncontrolled" = "#D95F02")) +
  labs(x = "", y = "", fill = "",
       title = "Outcome by 2025 Return Status",
       subtitle = "All 62,425 patients — didn't return = genuinely controlled (per organizers)") +
  theme_crisp(13)
ggsave("./figures/plot20_returned_outcome.png", p20, width = 8, height = 6, dpi = 150)


cat("\nAll plots saved to ./figures/\n")
