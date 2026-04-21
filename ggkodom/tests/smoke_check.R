#### SMOKE CHECK FOR ggkodom ####
##
## Run from package root: Rscript tests/smoke_check.R
## Sources every R/*.R (no devtools needed) and exercises:
##   (a) merged kodom_swimlane: geom = path/tile, coord = linear/polar
##   (b) shared FPCA helpers: kodom_components, kodom_perturbation use fpca_extract
##   (c) kodom_residuals (lmer / gam / fpca / FPCA)
##   (d) kodom_compare   (lmer / FPCA)
##   (e) kodom_smooth    (fpca / lmer / gam) + bootstrap roundtrip
##   regression check that kodom_state still works.

options(warn = 1)
suppressPackageStartupMessages({
  library(ggplot2)
})

pkg_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in pkg_files) source(f, local = FALSE)

cat("Loaded", length(pkg_files), "R files.\n")

#### TINY FAKE LONGITUDINAL DATA ####
set.seed(1)
n_subj <- 20
n_obs  <- 6
df <- data.frame(
  id    = rep(seq_len(n_subj), each = n_obs),
  time  = as.vector(replicate(n_subj, sort(round(runif(n_obs, 0, 120))))),
  value = rnorm(n_subj * n_obs, 7, 1.5)
)

ok <- function(label, expr) {
  out <- tryCatch(expr, error = function(e) {
    cat("FAIL  ", label, "  --  ", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(out)) {
    cls <- class(out)[1L]
    cat("OK    ", label, "  -> ", cls, "\n")
  }
  invisible(out)
}

cat("\n--- (a) merged kodom_swimlane ---\n")
ok("path / linear (default)", kodom_swimlane(df))
ok("path / linear + discretize", kodom_swimlane(df, discretize = TRUE,
                                                color_breaks = c(5.5, 7, 8)))
ok("tile / linear", kodom_swimlane(df, geom = "tile", time_bins = 5))
ok("tile / linear + facet", {
  df2 <- df; df2$grp <- factor(df2$id %% 2)
  kodom_swimlane(df2, geom = "tile", time_bins = 5, facet_rows = "grp")
})
ok("path / polar", kodom_swimlane(df, coord = "polar"))
ok("rejects tile + polar (expected error)", {
  e <- tryCatch(kodom_swimlane(df, geom = "tile", coord = "polar"),
                error = function(e) e)
  if (!inherits(e, "error")) stop("did not error as expected")
  "errored as expected"
})

cat("\n--- regression: kodom_state still works ---\n")
ok("kodom_state with thresholds", kodom_state(df, thresholds = c(6.5, 8)))

cat("\n--- (b) shared FPCA drawers ---\n")
have_fdapace <- requireNamespace("fdapace", quietly = TRUE)
have_refund  <- requireNamespace("refund",  quietly = TRUE)
cat("fdapace available:", have_fdapace, "\n")
cat("refund  available:", have_refund,  "\n")

if (have_fdapace) {
  set.seed(7)
  n_subj_f <- 50
  Lt <- replicate(n_subj_f, sort(runif(sample(4:8, 1L), 0, 120)),
                  simplify = FALSE)
  Ly <- lapply(Lt, function(t) 7 + 0.01 * t + rnorm(length(t), 0, 0.6))
  names(Lt) <- names(Ly) <- paste0("s", seq_len(n_subj_f))
  fp <- fdapace::FPCA(Ly = Ly, Lt = Lt,
                      optns = list(dataType = "Sparse", verbose = FALSE,
                                   methodSelectK = "FVE", FVEthreshold = 0.95))

  ok("fpca_extract (fdapace) returns expected fields", {
    ext <- fpca_extract(fp)
    stopifnot(all(c("mu","phi","lambdas","args","scores","ids","backend") %in% names(ext)))
    stopifnot(ext$backend == "fdapace")
    "ok"
  })
  ok("kodom_components (faceted)", kodom_components(fp))
  ok("kodom_components (overlay + show_mean)",
     kodom_components(fp, overlay = TRUE, show_mean = TRUE))
  ok("kodom_perturbation (default)", kodom_perturbation(fp))
  ok("kodom_perturbation (K = 1, sd_mult = 1)",
     kodom_perturbation(fp, K = 1, sd_mult = 1))
  ok("kodom_scores (default)", kodom_scores(fp))
  ok("kodom_scores (groups = numeric)",
     kodom_scores(fp, groups = sapply(Ly, mean)))
  ok("cluster_scores -> kodom_scores (groups = factor)", {
    cl <- cluster_scores(fp, k = 3)
    kodom_scores(fp, groups = cl)
  })
  ok("kodom_sort_scores -> kodom_swimlane(sort_by = key)", {
    df_f <- do.call(rbind, lapply(seq_along(Ly), function(i) {
      data.frame(id = names(Ly)[i], time = Lt[[i]], value = Ly[[i]])
    }))
    key <- kodom_sort_scores(fp)
    kodom_swimlane(df_f, sort_by = key)
  })
} else {
  cat("SKIP   fdapace tests (package not installed)\n")
}


cat("\n--- (c) kodom_residuals ---\n")

## need a non-trivial dataset with subject-level structure
set.seed(11)
n_subj_r <- 30
n_obs_r  <- 6
df_r <- do.call(rbind, lapply(seq_len(n_subj_r), function(i) {
  t  <- sort(runif(n_obs_r, 0, 120))     # no round() — fdapace rejects ties
  b0 <- rnorm(1, 0, 1.5); b1 <- rnorm(1, 0.01, 0.005)
  data.frame(id = i, time = t, value = 7 + b0 + b1 * t + rnorm(n_obs_r, 0, 0.5))
}))

have_lme4 <- requireNamespace("lme4", quietly = TRUE)
have_mgcv <- requireNamespace("mgcv", quietly = TRUE)
cat("lme4 available:", have_lme4, "\n")
cat("mgcv available:", have_mgcv, "\n")

if (have_lme4) {
  m_lmer <- suppressMessages(lme4::lmer(value ~ time + (1 + time | id), data = df_r))
  ok("kodom_residuals (lmer)", kodom_residuals(m_lmer, df_r))
  ok("kodom_residuals (lmer, discretize)",
     kodom_residuals(m_lmer, df_r, discretize = TRUE))
}
if (have_mgcv) {
  df_r$id_f <- factor(df_r$id)
  m_gam <- mgcv::gam(value ~ s(time, id_f, bs = "fs", k = 4), data = df_r)
  ok("kodom_residuals (gam)",
     kodom_residuals(m_gam, df_r, id = "id_f"))
}
if (have_fdapace) {
  Ly_r <- split(df_r$value, df_r$id)
  Lt_r <- split(df_r$time,  df_r$id)
  fp_r <- fdapace::FPCA(Ly = Ly_r, Lt = Lt_r,
                        optns = list(dataType = "Sparse", verbose = FALSE))
  ok("kodom_residuals (FPCA)", kodom_residuals(fp_r, df_r))
}


cat("\n--- (d) kodom_compare ---\n")
if (have_lme4) {
  ok("kodom_compare (lmer, default)", kodom_compare(m_lmer, df_r))
}
if (have_fdapace) {
  ok("kodom_compare (FPCA, sort_by = score)", {
    key <- kodom_sort_scores(fp_r, ids = as.character(unique(df_r$id)))
    kodom_compare(fp_r, df_r, sort_by = key)
  })
  ok("kodom_compare (FPCA, facet by cluster)", {
    cl <- cluster_scores(fp_r, k = 3, ids = as.character(unique(df_r$id)))
    df_rc <- df_r
    df_rc$cluster <- cl[as.character(df_rc$id)]
    kodom_compare(fp_r, df_rc, facet_rows = "cluster")
  })
}


cat("\n--- (e) kodom_smooth ---\n")
if (have_fdapace) {
  ok("kodom_smooth (fpca) returns fit + refit_fn", {
    sm <- kodom_smooth(df_r, method = "fpca")
    stopifnot(inherits(sm$fit, "FPCA"))
    stopifnot(is.function(sm$refit_fn))
    ## roundtrip: refit on a tiny resample, must produce another FPCA fit
    ids_use <- sample(unique(df_r$id), 20, replace = TRUE)
    long <- do.call(rbind, lapply(seq_along(ids_use), function(i) {
      sub <- df_r[df_r$id == ids_use[i], , drop = FALSE]
      sub$id <- paste0(ids_use[i], "_b", i); sub
    }))
    refit <- sm$refit_fn(long)
    stopifnot(inherits(refit, "FPCA"))
    "ok"
  })
  ok("kodom_smooth (fpca) -> fpca_boot_subjects roundtrip", {
    sm <- kodom_smooth(df_r, method = "fpca")
    boots <- fpca_boot_subjects(df_r, B = 3, refit_fn = sm$refit_fn, seed = 1)
    stopifnot(length(boots) >= 1L)
    kodom_components(sm$fit, boot_fits = boots)
  })
}
if (have_lme4) {
  ok("kodom_smooth (lmer)", {
    sm <- kodom_smooth(df_r, method = "lmer")
    stopifnot(inherits(sm$fit, "lmerMod"))
    "ok"
  })
}
if (have_mgcv) {
  ok("kodom_smooth (gam)", {
    sm <- kodom_smooth(df_r, method = "gam")
    stopifnot(inherits(sm$fit, "gam"))
    "ok"
  })
}

cat("\nDONE.\n")
