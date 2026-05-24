#### TESTS FOR geom_kodom_branch ####

# 3 subjects, 4 pre-branch observations each (medication = NA).
# Each subject then gets predictions under two arms: "M1" and "M2".
# Branch timepoints differ: A branches at x=4, B at x=6, C at x=8.
branch_df <- data.frame(
  id = c(
    rep("A", 8), rep("B", 8), rep("C", 8)
  ),
  x = c(
    # A: 2 pre-branch obs, 2 post-obs, 2 M1 preds, 2 M2 preds
    0, 2, 4, 6,   4, 6,   4, 6,
    # B: 3 pre-branch obs, 2 post-obs, 2 M1 preds, 2 M2 preds
    0, 3, 6, 9,   6, 9,   6, 9,
    # C: 2 pre-branch obs, 2 post-obs, 2 M1 preds, 2 M2 preds
    0, 4, 8, 12,  8, 12,  8, 12
  ),
  value = c(
    # A
    5, 5.5, 6, 6.2,   6.3, 6.5,   5.8, 5.6,
    # B
    7, 7.5, 8, 8.2,   8.3, 8.5,   7.8, 7.6,
    # C
    5, 5.2, 5.4, 5.6, 5.7, 5.9,   5.3, 5.1
  ),
  medication = c(
    # A
    NA, NA, NA, NA,   NA, NA,   "M1", "M1",   # wait - need M2 too
    NA, NA, NA, NA,   NA, NA,   "M1", "M1",
    NA, NA, NA, NA,   NA, NA,   "M1", "M1"
  ),
  stringsAsFactors = FALSE
)

# Rebuild with two medication arms properly
branch_df <- data.frame(
  id = c(
    rep("A", 10), rep("B", 10), rep("C", 10)
  ),
  x = c(
    # A: 4 observed (NA), 3 M1 predictions, 3 M2 predictions
    0, 2, 4, 6,   4, 6, 8,   4, 6, 8,
    # B
    0, 3, 6, 9,   6, 9, 12,  6, 9, 12,
    # C
    0, 4, 8, 12,  8, 12, 16, 8, 12, 16
  ),
  value = c(
    5, 5.5, 6, 6.2,   6.5, 6.8, 7.0,   5.8, 5.6, 5.4,
    7, 7.5, 8, 8.2,   8.5, 8.8, 9.0,   7.8, 7.6, 7.4,
    5, 5.2, 5.4, 5.6, 5.9, 6.1, 6.3,   5.3, 5.1, 4.9
  ),
  medication = c(
    NA, NA, NA, NA,   "M1", "M1", "M1",   "M2", "M2", "M2",
    NA, NA, NA, NA,   "M1", "M1", "M1",   "M2", "M2", "M2",
    NA, NA, NA, NA,   "M1", "M1", "M1",   "M2", "M2", "M2"
  ),
  stringsAsFactors = FALSE
)

base_plot <- function(...) {
  ggplot2::ggplot(
    branch_df,
    ggplot2::aes(
      x          = .data$x,
      id         = .data$id,
      colour     = .data$value,
      medication = .data$medication
    )
  ) + geom_kodom_branch(...)
}

# ---- stat test data ----

stat_data <- data.frame(
  id         = c(rep("A", 10), rep("B", 10), rep("C", 10)),
  x          = c(
    0, 2, 4, 6, 4, 6, 8, 4, 6, 8,
    0, 3, 6, 9, 6, 9, 12, 6, 9, 12,
    0, 4, 8, 12, 8, 12, 16, 8, 12, 16
  ),
  colour = c(
    5, 5.5, 6, 6.2, 6.5, 6.8, 7.0, 5.8, 5.6, 5.4,
    7, 7.5, 8, 8.2, 8.5, 8.8, 9.0, 7.8, 7.6, 7.4,
    5, 5.2, 5.4, 5.6, 5.9, 6.1, 6.3, 5.3, 5.1, 4.9
  ),
  medication = c(
    NA, NA, NA, NA, "M1", "M1", "M1", "M2", "M2", "M2",
    NA, NA, NA, NA, "M1", "M1", "M1", "M2", "M2", "M2",
    NA, NA, NA, NA, "M1", "M1", "M1", "M2", "M2", "M2"
  ),
  PANEL  = factor(1),
  group  = c(rep(1L, 10), rep(2L, 10), rep(3L, 10)),
  stringsAsFactors = FALSE
)

default_args <- list(
  sort_by = "none", n_max = Inf, lane_width = 1, branch_fraction = 0.7
)

compute <- function(...) {
  ggkodom:::StatKodomBranch$compute_panel(
    stat_data, scales = list(), ...
  )
}

# ---- layer object ----

test_that("geom_kodom_branch() returns a valid ggplot2 layer", {
  layer <- geom_kodom_branch()
  expect_true(ggplot2::is_ggproto(layer$geom))
  expect_true(ggplot2::is_ggproto(layer$stat))
})

test_that("geom_kodom_branch() can be added to a ggplot", {
  p <- ggplot2::ggplot() + geom_kodom_branch()
  expect_s3_class(p, "gg")
})

# ---- stat output structure ----

test_that("StatKodomBranch$compute_panel returns a data frame", {
  out <- do.call(compute, default_args)
  expect_s3_class(out, "data.frame")
})

test_that("compute_panel keeps x and y columns", {
  out <- do.call(compute, default_args)
  expect_true("x" %in% names(out))
  expect_true("y" %in% names(out))
})

test_that("compute_panel y is strictly positive", {
  out <- do.call(compute, default_args)
  expect_true(all(out$y > 0))
})

test_that("compute_panel y is finite for all rows", {
  out <- do.call(compute, default_args)
  expect_true(all(is.finite(out$y)))
})

test_that("compute_panel row count equals input for n_max >= n_subjects", {
  out <- do.call(compute, default_args)
  expect_equal(nrow(out), nrow(stat_data))
})

# ---- linetype assignment ----

# When linetype is not user-mapped the stat writes grid-compatible strings
# directly ("solid" for observed, arm-indexed dashes for predicted).
test_that("observed rows get linetype 'solid' in auto mode", {
  out      <- do.call(compute, default_args)
  obs_rows <- is.na(stat_data$medication)
  expect_true(all(out$linetype[obs_rows] == "solid"))
})

test_that("predicted rows get non-solid linetype in auto mode", {
  out       <- do.call(compute, default_args)
  pred_rows <- !is.na(stat_data$medication)
  expect_true(all(out$linetype[pred_rows] != "solid"))
})

# When linetype IS user-mapped (data$linetype has NAs from medication column),
# the stat replaces NA with "observed" and leaves arm values unchanged.
test_that("mapped mode: NA linetype becomes 'observed', arms kept as-is", {
  mapped_data           <- stat_data
  mapped_data$linetype  <- mapped_data$medication  # NA where observed
  out <- ggkodom:::StatKodomBranch$compute_panel(
    mapped_data, scales = list(),
    sort_by = "none", n_max = Inf, lane_width = 1, branch_fraction = 0.7
  )
  obs_rows  <- is.na(stat_data$medication)
  pred_rows <- !is.na(stat_data$medication)
  expect_true(all(out$linetype[obs_rows] == "observed"))
  expect_true(all(
    out$linetype[pred_rows] == stat_data$medication[pred_rows]
  ))
})

# ---- sub-lane layout ----

test_that("observed y is below every predicted y for same subject", {
  out <- do.call(compute, default_args)
  for (sid in unique(out$id)) {
    is_sid  <- stat_data$id == sid
    is_obs  <- is_sid & is.na(stat_data$medication)
    is_pred <- is_sid & !is.na(stat_data$medication)
    expect_true(all(out$y[is_pred] > min(out$y[is_obs])))
  }
})

test_that("two medication arms produce two distinct y offsets per subject", {
  out <- do.call(compute, default_args)
  for (sid in unique(out$id)) {
    is_sid     <- stat_data$id == sid
    is_pred    <- is_sid & !is.na(stat_data$medication)
    sub        <- out[is_pred, ]
    med_labels <- stat_data$medication[is_pred]
    y_by_med   <- tapply(sub$y, med_labels, unique)
    expect_equal(length(unique(unlist(y_by_med))), 2L)
  }
})

test_that("sub-lane step equals branch_fraction / K * lane_width", {
  out  <- do.call(compute, default_args)
  sid  <- "A"
  is_sid <- stat_data$id == sid
  is_obs <- is_sid & is.na(stat_data$medication)
  is_m1  <- is_sid & !is.na(stat_data$medication) &
    stat_data$medication == "M1"
  is_m2  <- is_sid & !is.na(stat_data$medication) &
    stat_data$medication == "M2"
  y_obs  <- unique(out$y[is_obs])
  y_m1   <- unique(out$y[is_m1])
  y_m2   <- unique(out$y[is_m2])
  step   <- 0.7 / 2
  expect_equal(y_m1 - y_obs, step,      tolerance = 1e-9)
  expect_equal(y_m2 - y_obs, 2 * step,  tolerance = 1e-9)
})

test_that("branch_fraction = 0 places all predictions at observed y", {
  args <- modifyList(default_args, list(branch_fraction = 0))
  out  <- do.call(compute, args)
  for (sid in unique(out$id)) {
    is_sid <- stat_data$id == sid
    y_obs  <- unique(out$y[is_sid & is.na(stat_data$medication)])
    y_pred <- unique(out$y[is_sid & !is.na(stat_data$medication)])
    expect_equal(y_obs, y_pred, tolerance = 1e-9)
  }
})

# ---- lane_width scaling ----

test_that("lane_width = 2 doubles all y values relative to lane_width = 1", {
  out1 <- do.call(compute, modifyList(default_args, list(lane_width = 1)))
  out2 <- do.call(compute, modifyList(default_args, list(lane_width = 2)))
  expect_equal(out2$y, out1$y * 2, tolerance = 1e-9)
})

test_that("inter-subject gap (observed y) scales linearly with lane_width", {
  out1 <- do.call(compute, modifyList(default_args, list(lane_width = 1)))
  out3 <- do.call(compute, modifyList(default_args, list(lane_width = 3)))
  y_obs1 <- tapply(out1$y[is.na(stat_data$medication)],
                   out1$id[is.na(stat_data$medication)], min)
  y_obs3 <- tapply(out3$y[is.na(stat_data$medication)],
                   out3$id[is.na(stat_data$medication)], min)
  expect_equal(as.numeric(diff(sort(y_obs3))[1] / diff(sort(y_obs1))[1]),
               3, tolerance = 1e-9)
})

# ---- group uniqueness ----

test_that("each (subject, medication) pair gets a unique group value", {
  out <- do.call(compute, default_args)
  # 3 subjects × 3 tracks (observed, M1, M2) = 9 groups
  combos <- paste(out$id, ifelse(is.na(stat_data$medication), "obs",
                                 stat_data$medication))
  n_combos <- length(unique(combos))
  n_groups <- length(unique(out$group))
  expect_equal(n_groups, n_combos)
})

test_that("observed and M1 for same subject have different group values", {
  out    <- do.call(compute, default_args)
  is_a   <- stat_data$id == "A"
  is_obs <- is_a & is.na(stat_data$medication)
  is_m1  <- is_a & !is.na(stat_data$medication) &
    stat_data$medication == "M1"
  grp_obs <- unique(out$group[is_obs])
  grp_m1  <- unique(out$group[is_m1])
  expect_false(any(grp_obs %in% grp_m1))
})

# ---- sorting ----

test_that("sort_by = 'mean' assigns smallest y to highest-mean subject", {
  args <- modifyList(default_args, list(sort_by = "mean"))
  out  <- do.call(compute, args)
  # B mean ~= 8.0 > A ~= 6.1 > C ~= 5.4
  min_obs <- tapply(
    out$y[is.na(stat_data$medication)],
    out$id[is.na(stat_data$medication)],
    min
  )
  expect_lt(min_obs["B"], min_obs["A"])
  expect_lt(min_obs["A"], min_obs["C"])
})

test_that("n_max subsamples subjects", {
  args <- modifyList(default_args, list(n_max = 2L))
  out  <- do.call(compute, args)
  expect_lte(length(unique(out$id)), 2L)
})

# ---- full plot builds ----

test_that("basic geom_kodom_branch plot builds without error", {
  expect_no_error(ggplot2::ggplot_build(base_plot()))
})

test_that("plot builds with show_points = FALSE", {
  expect_no_error(ggplot2::ggplot_build(base_plot(show_points = FALSE)))
})

test_that("plot builds with shape = NA (suppress points)", {
  expect_no_error(ggplot2::ggplot_build(base_plot(shape = NA)))
})

test_that("plot builds with varied lane_width values", {
  for (lw in c(0.5, 1, 2, 4)) {
    expect_no_error(ggplot2::ggplot_build(base_plot(lane_width = lw)))
  }
})

test_that("plot builds with varied branch_fraction values", {
  for (bf in c(0, 0.3, 0.7, 0.9)) {
    expect_no_error(ggplot2::ggplot_build(base_plot(branch_fraction = bf)))
  }
})

test_that("plot builds with each sort_by option", {
  for (sb in c("none", "mean", "mean_asc", "first", "last")) {
    expect_no_error(ggplot2::ggplot_build(base_plot(sort_by = sb)))
  }
})

test_that("plot builds with n_max restricting subjects", {
  expect_no_error(ggplot2::ggplot_build(base_plot(n_max = 2L)))
})

test_that("single-visit subject does not cause an error", {
  df_extra <- rbind(
    branch_df,
    data.frame(id = "D", x = 1, value = 6.0, medication = NA,
               stringsAsFactors = FALSE)
  )
  p <- ggplot2::ggplot(
    df_extra,
    ggplot2::aes(x = .data$x, id = .data$id,
                 colour = .data$value, medication = .data$medication)
  ) + geom_kodom_branch()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("geom_kodom_branch composes with scale_colour_kodom", {
  p <- base_plot() + scale_colour_kodom()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("geom_kodom_branch composes with theme_kodom", {
  p <- base_plot() + theme_kodom()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("geom_kodom_branch composes with facet_wrap", {
  df2      <- branch_df
  df2$site <- rep(c("S1", "S2"), length.out = nrow(df2))
  p <- ggplot2::ggplot(
    df2,
    ggplot2::aes(x = .data$x, id = .data$id,
                 colour = .data$value, medication = .data$medication)
  ) + geom_kodom_branch() + ggplot2::facet_wrap(~site)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("all-observed data (no predictions) builds without error", {
  df_obs <- branch_df[is.na(branch_df$medication), ]
  p <- ggplot2::ggplot(
    df_obs,
    ggplot2::aes(x = .data$x, id = .data$id,
                 colour = .data$value, medication = .data$medication)
  ) + geom_kodom_branch()
  expect_no_error(ggplot2::ggplot_build(p))
})
