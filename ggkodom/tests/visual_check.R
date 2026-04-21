#### VISUAL CHECK FOR ggkodom ####
##
## Run from package root:  source("tests/visual_check.R")
## Writes tests/visual_check.pdf — one plot per page, titled with what's shown.
## Open the PDF and flip through to eyeball every function.
##
## Synthetic data has three latent cohorts so the FPCA / cluster plots have
## something to actually find:
##   "stable"      baseline ~6.5, ~flat
##   "deteriorate" baseline ~7,   positive slope
##   "responder"   baseline ~9,   negative slope

options(warn = 1)
suppressPackageStartupMessages({
  library(ggplot2)
})

pkg_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in pkg_files) source(f, local = FALSE)
cat("Loaded", length(pkg_files), "R files.\n")


#### SYNTHETIC LONGITUDINAL DATA ####
set.seed(2441139)
n_per   <- 20
cohorts <- c("stable", "deteriorate", "responder")

make_one <- function(i, cohort) {
  n_obs <- sample(4:8, 1L)
  t <- sort(runif(n_obs, 0, 1000))           # no round() — fdapace rejects ties
  y <- switch(cohort,
    stable      = 6.5 + rnorm(1, 0, 0.4) + 0.0001 * t,
    deteriorate = 7.0 + rnorm(1, 0, 0.4) + 0.0025 * t,
    responder   = 9.0 + rnorm(1, 0, 0.5) - 0.0030 * t)
  data.frame(id     = sprintf("p%03d", i),
             time   = t,
             value  = y + rnorm(n_obs, 0, 0.4),
             cohort = cohort,
             stringsAsFactors = FALSE)
}
ids    <- seq_len(n_per * length(cohorts))
labels <- rep(cohorts, each = n_per)
df     <- do.call(rbind, Map(make_one, ids, labels))


#### OPEN PDF ####
## cairo_pdf handles UTF-8 (em-dash, greek letters from kodom_perturbation
## labels, ±, √, …) — the default pdf() device silently substitutes them.
out_pdf <- "tests/visual_check.pdf"
if (isTRUE(capabilities("cairo"))) {
  grDevices::cairo_pdf(out_pdf, width = 9, height = 6, onefile = TRUE)
} else {
  warning("Cairo not available; UTF-8 chars (em-dashes, greek letters) ",
          "may render as boxes or get substituted.", call. = FALSE)
  grDevices::pdf(out_pdf, width = 9, height = 6)
}

## show() forces lazy arg inside tryCatch so one failure doesn't kill the PDF.
show <- function(p_expr, label) {
  p <- tryCatch({ p_expr },
                error = function(e) {
                  cat("FAIL  ", label, "  --  ", conditionMessage(e), "\n")
                  NULL
                })
  if (inherits(p, "ggplot")) {
    print(p + ggtitle(label))
    cat("PAGE  ", label, "\n")
  } else if (!is.null(p)) {
    cat("SKIP  ", label, "  (returned ", class(p)[1L], ")\n", sep = "")
  }
}


#### kodom_swimlane variants ####
cat("\n--- swimlane ---\n")
show(kodom_swimlane(df), "swimlane: path / linear (default)")
show(kodom_swimlane(df, discretize = TRUE, color_breaks = c(6.5, 7, 8)),
     "swimlane: path / linear + discretize at 6.5/7/8")
show(kodom_swimlane(df, geom = "tile", time_bins = 8),
     "swimlane: tile / linear (8 time bins)")
show(kodom_swimlane(df, geom = "tile", time_bins = 8, facet_rows = "cohort"),
     "swimlane: tile / linear + facet by cohort")
show(kodom_swimlane(df, coord = "polar"),
     "swimlane: path / polar (full circle, the kodom flower)")
show(kodom_swimlane(df, coord = "polar", arc_degrees = 270),
     "swimlane: polar fan (arc_degrees = 270)")
show(kodom_swimlane(df, coord = "polar", arc_degrees = 180),
     "swimlane: polar fan (arc_degrees = 180, half-circle)")
show(kodom_swimlane(df, coord = "polar", discretize = TRUE,
                    color_breaks = c(6.5, 7, 8)),
     "swimlane: polar + discretize")


#### kodom_state ####
cat("\n--- state ---\n")
show(kodom_state(df, thresholds = c(6.5, 8)),
     "state: thresholds 6.5 / 8")


#### kodom_swimlane_multi (multivariate) ####
cat("\n--- swimlane_multi ---\n")
have_patchwork <- requireNamespace("patchwork", quietly = TRUE)
cat("patchwork:", have_patchwork, "\n")
if (have_patchwork) {
  ## Build a 2nd variable correlated with cohort (BMI-ish), same ids/times.
  bmi_offset <- c(stable = 0, deteriorate = 2, responder = 5)
  df_bmi <- df
  df_bmi$value <- 25 +
    bmi_offset[df$cohort] +
    0.002 * df$time +
    rnorm(nrow(df), 0, 1.2)

  long <- rbind(
    transform(df,     variable = "a1c"),
    transform(df_bmi, variable = "bmi")
  )

  show(kodom_swimlane_multi(long, anchor = "a1c"),
       "swimlane_multi: a1c | bmi (anchor = a1c)")
  show(kodom_swimlane_multi(long, anchor = "a1c",
                            color_breaks_list = list(a1c = c(6.5, 7, 8),
                                                     bmi = c(25, 30)),
                            discretize = list(a1c = TRUE, bmi = TRUE)),
       "swimlane_multi: discretized at clinical thresholds (a1c 6.5/7/8, bmi 25/30)")
  show(kodom_swimlane_multi(long, anchor = "bmi"),
       "swimlane_multi: anchored on BMI (lanes resorted)")
} else {
  cat("SKIP   swimlane_multi (patchwork not installed)\n")
}


#### kodom_multi (polar concentric bands) — alternative to swimlane_multi ####
cat("\n--- kodom_multi (polar concentric) ---\n")
if (!exists("long")) {
  ## same long-format multivariate data as the swimlane_multi block
  bmi_offset <- c(stable = 0, deteriorate = 2, responder = 5)
  df_bmi <- df
  df_bmi$value <- 25 + bmi_offset[df$cohort] + 0.002 * df$time +
                  rnorm(nrow(df), 0, 1.2)
  long <- rbind(transform(df, variable = "a1c"),
                transform(df_bmi, variable = "bmi"))
}
show(kodom_multi(long, anchor = "a1c"),
     "kodom_multi: a1c | bmi concentric bands (full circle)")
show(kodom_multi(long, anchor = "a1c", arc_degrees = 270),
     "kodom_multi: concentric bands + fan (arc_degrees = 270)")
show(kodom_multi(long, anchor = "bmi"),
     "kodom_multi: anchored on BMI (lanes resorted, bands swap inside-out)")
## third variable to make it visually richer
ldl <- df
ldl$value <- 100 + c(stable = 0, deteriorate = 20, responder = 40)[df$cohort] +
             0.05 * df$time + rnorm(nrow(df), 0, 10)
long3 <- rbind(transform(df, variable = "a1c"),
               transform(df_bmi, variable = "bmi"),
               transform(ldl, variable = "ldl"))
show(kodom_multi(long3, anchor = "a1c"),
     "kodom_multi: 3 variables (a1c | bmi | ldl) on one flower")


#### FPCA-driven views ####
cat("\n--- FPCA views ---\n")
have_fdapace <- requireNamespace("fdapace", quietly = TRUE)
have_lme4    <- requireNamespace("lme4",    quietly = TRUE)
have_mgcv    <- requireNamespace("mgcv",    quietly = TRUE)
cat("fdapace:", have_fdapace, "  lme4:", have_lme4, "  mgcv:", have_mgcv, "\n")

fp <- NULL
if (have_fdapace) {
  Ly <- split(df$value, df$id)
  Lt <- split(df$time,  df$id)
  fp <- fdapace::FPCA(Ly = Ly, Lt = Lt,
                      optns = list(dataType = "Sparse", verbose = FALSE,
                                   methodSelectK = "FVE", FVEthreshold = 0.95))

  show(kodom_components(fp), "components: faceted eigenfunctions")
  show(kodom_components(fp, overlay = TRUE, show_mean = TRUE),
       "components: overlay + mean curve")
  show(kodom_perturbation(fp),                "perturbation: default")
  show(kodom_perturbation(fp, K = 1, sd_mult = 1),
       "perturbation: K=1, sd_mult=1")
  show(kodom_scores(fp),                      "scores: FPC1 vs FPC2")

  cl <- cluster_scores(fp, k = 3)
  show(kodom_scores(fp, groups = cl),         "scores: colored by k=3 cluster")

  key <- kodom_sort_scores(fp)
  show(kodom_swimlane(df, sort_by = key),
       "swimlane: lanes sorted by FPC1 score")

  ## Bootstrap envelopes — small B so this stays snappy.
  sm    <- kodom_smooth(df, method = "fpca")
  boots <- fpca_boot_subjects(df, B = 8, refit_fn = sm$refit_fn, seed = 1)
  show(kodom_components(sm$fit, boot_fits = boots),
       "components: with subject-level bootstrap envelopes (B=8)")

  ## --- glyph score scatter ---
  ## univariate (single variable, one line per subject)
  show(kodom_scores_glyph(fp, df),
       "scores_glyph: univariate (a1c sparkline at each PC location)")
  ## multivariate — needs the long-format frame from the swimlane_multi block
  if (exists("long")) {
    show(kodom_scores_glyph(fp, long, variable = "variable"),
         "scores_glyph: multivariate (a1c + bmi sparklines per subject)")
    show(kodom_scores_glyph(fp, long, variable = "variable",
                            glyph_size = 0.08),
         "scores_glyph: multivariate, larger glyphs (size 0.08)")
  }
}


#### kodom_fit — smoothed swimlanes from a model fit ####
cat("\n--- kodom_fit ---\n")
m_lmer <- NULL; m_gam <- NULL; df_g <- NULL; df_l <- NULL
if (have_lme4) {
  ## scale time so the random-slope model is numerically well-conditioned
  df_l <- df; df_l$time <- df_l$time / 1000
  m_lmer <- suppressMessages(lme4::lmer(value ~ time + (1 + time | id), data = df_l))
  show(kodom_fit(m_lmer, id = "id", time = "time"),
       "fit: lmer (random intercept + slope, time/1000)")
}
if (have_mgcv) {
  df_g <- df; df_g$id_f <- factor(df_g$id)
  m_gam <- mgcv::gam(value ~ s(time, id_f, bs = "fs", k = 4), data = df_g)
  show(kodom_fit(m_gam, id = "id_f", time = "time"),
       "fit: gam factor-smooth (bs='fs', k=4)")
}
if (have_fdapace) {
  show(kodom_fit(fp), "fit: FPCA reconstruction")
}


#### kodom_residuals ####
cat("\n--- residuals ---\n")
if (have_lme4) {
  show(kodom_residuals(m_lmer, df_l), "residuals: lmer (continuous)")
  show(kodom_residuals(m_lmer, df_l, discretize = TRUE),
       "residuals: lmer + discretize")
}
if (have_mgcv)    show(kodom_residuals(m_gam, df_g, id = "id_f"),
                       "residuals: gam")
if (have_fdapace) show(kodom_residuals(fp, df), "residuals: FPCA")


#### kodom_compare ####
cat("\n--- compare ---\n")
if (have_lme4) {
  show(kodom_compare(m_lmer, df_l),
       "compare overlay: lmer (default — points on top of fit lines)")
  show(kodom_compare(m_lmer, df_l, layout = "side"),
       "compare side: lmer (raw | fit two-panel)")
}
if (have_fdapace) {
  key <- kodom_sort_scores(fp)
  show(kodom_compare(fp, df, sort_by = key),
       "compare overlay: FPCA (sorted by FPC1)")
  show(kodom_compare(fp, df, layout = "side", sort_by = key),
       "compare side: FPCA (sorted by FPC1)")
  cl   <- cluster_scores(fp, k = 3)
  df_c <- df; df_c$cluster <- cl[as.character(df_c$id)]
  show(kodom_compare(fp, df_c, facet_rows = "cluster"),
       "compare overlay: FPCA (facet by k=3 cluster)")
}


dev.off()
cat("\nDONE. Wrote ", out_pdf, "\n", sep = "")
