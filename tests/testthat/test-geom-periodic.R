#### TESTS FOR geom_kodom_periodic ####

# 3 subjects × 6 visits spanning 2 periods (period = 12)
# Subject means: A ≈ 6.17, B ≈ 7.83, C = 5.5
peri_df <- data.frame(
  id = rep(c("A", "B", "C"), each = 6),
  x = rep(c(0, 4, 8, 12, 16, 20), 3),
  value = c(
    5, 6, 7, 6, 7, 6,
    7, 8, 9, 8, 7, 8,
    5, 5.5, 6, 5.5, 5, 6
  ),
  stringsAsFactors = FALSE
)

base_plot <- function(...) {
  ggplot2::ggplot(
    peri_df,
    ggplot2::aes(x = .data$x, id = .data$id, colour = .data$value)
  ) +
    geom_kodom_periodic(...) +
    coord_kodom_periodic()
}

# ---- layer object ----

test_that("geom_kodom_periodic() returns a valid ggplot2 layer", {
  layer <- geom_kodom_periodic()
  expect_true(ggplot2::is_ggproto(layer$geom))
  expect_true(ggplot2::is_ggproto(layer$stat))
})

test_that("geom_kodom_periodic() can be added to a ggplot", {
  p <- ggplot2::ggplot() +
    geom_kodom_periodic()
  expect_s3_class(p, "gg")
})

# ---- stat output ----

stat_data <- data.frame(
  id = rep(c("A", "B", "C"), each = 6),
  x = rep(c(0, 4, 8, 12, 16, 20), 3),
  colour = c(
    5, 6, 7, 6, 7, 6,
    7, 8, 9, 8, 7, 8,
    5, 5.5, 6, 5.5, 5, 6
  ),
  PANEL = factor(1),
  group = rep(1:3, each = 6),
  stringsAsFactors = FALSE
)

default_args <- list(
  sort_by = "none", n_max = Inf,
  period = 12, inner_fraction = 0.3, spiral_fraction = 0.1
)

compute <- function(...) {
  ggkodom:::StatKodomPeriodic$compute_panel(
    stat_data,
    scales = list(), ...
  )
}

test_that("StatKodomPeriodic$compute_panel returns a data frame", {
  out <- do.call(compute, default_args)
  expect_s3_class(out, "data.frame")
})

test_that("compute_panel adds x and y columns", {
  out <- do.call(compute, default_args)
  expect_true("x" %in% names(out))
  expect_true("y" %in% names(out))
})

test_that("compute_panel leaves x continuous (no modulo)", {
  out <- do.call(compute, default_args)
  expect_true(all(out$x == stat_data$x))
})

test_that("compute_panel y (radius) is strictly positive", {
  out <- do.call(compute, default_args)
  expect_true(all(out$y > 0))
})

test_that("compute_panel row count equals input for n_max >= n_subjects", {
  out <- do.call(compute, default_args)
  expect_equal(nrow(out), nrow(stat_data))
})

test_that("compute_panel y is finite for all rows", {
  out <- do.call(compute, default_args)
  expect_true(all(is.finite(out$y)))
})

# ---- lane ordering via radius ----

test_that("sort_by='mean' assigns smallest base radius to highest-mean subject", {
  # B mean ≈ 7.83 → lane 1 → smallest inner radius
  args <- modifyList(default_args, list(sort_by = "mean"))
  out <- do.call(compute, args)
  # Minimum radius per subject (at x_mod=0, spiral term = 0)
  min_y <- tapply(out$y, out$id, min)
  expect_lt(min_y["B"], min_y["A"])
  expect_lt(min_y["A"], min_y["C"])
})

test_that("sort_by='mean_asc' assigns smallest base radius to lowest-mean subject", {
  # C mean = 5.5 is lowest → lane 1 → smallest radius
  args <- modifyList(default_args, list(sort_by = "mean_asc"))
  out <- do.call(compute, args)
  min_y <- tapply(out$y, out$id, min)
  expect_lt(min_y["C"], min_y["B"])
})

test_that("n_max subsamples subjects", {
  args <- modifyList(default_args, list(n_max = 2L))
  out <- do.call(compute, args)
  expect_lte(length(unique(out$id)), 2L)
})

# ---- lane_width ----

test_that("lane_width > 1 produces larger inter-ring gaps", {
  out1 <- do.call(compute, modifyList(default_args, list(lane_width = 1)))
  out3 <- do.call(compute, modifyList(default_args, list(lane_width = 3)))
  # Gap between lane-1 and lane-2 minimum radii should be 3× larger
  min1 <- sort(tapply(out1$y, out1$id, min))
  min3 <- sort(tapply(out3$y, out3$id, min))
  expect_equal(as.numeric(diff(min3)[1] / diff(min1)[1]), 3, tolerance = 1e-9)
})

test_that("lane_width does not affect inner_radius (hole stays anchored)", {
  # inner_radius = 0.3 * 3 = 0.9, unchanged regardless of lane_width.
  # Innermost lane (lane=1) at time=0 (spiral=0):
  #   lane_width=1 → y = 0.9 + 1*1 = 1.9
  #   lane_width=5 → y = 0.9 + 1*5 = 5.9  (diff = 4, not 4*0.9+4=7.6)
  out1 <- do.call(compute, modifyList(default_args, list(lane_width = 1)))
  out5 <- do.call(compute, modifyList(default_args, list(lane_width = 5)))
  expect_equal(min(out5$y) - min(out1$y), 4, tolerance = 1e-6)
})

test_that("lane_width = 1 is identical to the default output", {
  out_default <- do.call(compute, default_args)
  out_w1 <- do.call(compute, modifyList(default_args, list(lane_width = 1)))
  expect_equal(out_default$y, out_w1$y)
})

test_that("plot builds with varied lane_width values", {
  for (lw in c(0.5, 1, 2, 4)) {
    expect_no_error(ggplot2::ggplot_build(base_plot(lane_width = lw)))
  }
})

# ---- period-boundary group-breaking (now in stat) ----

test_that("compute_panel does NOT break groups at period boundaries", {
  # Groups are preserved across the entire continuous trajectory
  out <- do.call(compute, default_args)
  expect_equal(length(unique(out$group[out$id == "A"])), 1L)
})

test_that("single-period data has no spurious group breaks", {
  # With period = 24, all x < 24, so every observation stays in period 0.
  args <- modifyList(default_args, list(period = 24))
  out <- do.call(compute, args)
  for (gid in unique(stat_data$group)) {
    expect_length(unique(out$group[stat_data$group == gid]), 1L)
  }
})

test_that("number of distinct groups equals number of subjects", {
  # 3 subjects = 3 distinct groups
  out <- do.call(compute, default_args)
  expect_equal(length(unique(out$group)), 3L)
})

test_that("spiral_fraction = 0 gives constant radius per subject", {
  args <- modifyList(default_args, list(spiral_fraction = 0))
  out <- do.call(compute, args)
  for (sid in unique(out$id)) {
    expect_equal(length(unique(out$y[out$id == sid])), 1L)
  }
})

test_that("larger spiral_fraction produces larger max radius", {
  args_lo <- modifyList(default_args, list(spiral_fraction = 0.0))
  args_hi <- modifyList(default_args, list(spiral_fraction = 0.5))
  expect_lt(
    max(do.call(compute, args_lo)$y),
    max(do.call(compute, args_hi)$y)
  )
})

# ---- full plot build ----

test_that("basic geom_kodom_periodic plot builds without error", {
  expect_no_error(ggplot2::ggplot_build(base_plot()))
})

test_that("multi-period data builds without error (no chord artifact)", {
  expect_no_error(ggplot2::ggplot_build(base_plot(period = 12)))
})

test_that("plot builds with varied period values", {
  for (p in c(6, 12, 24)) {
    expect_no_error(ggplot2::ggplot_build(base_plot(period = p)))
  }
})

test_that("plot builds with varied spiral_fraction values", {
  for (sf in c(0, 0.1, 0.5)) {
    expect_no_error(ggplot2::ggplot_build(base_plot(spiral_fraction = sf)))
  }
})

test_that("plot builds with varied inner_fraction values", {
  for (inf_val in c(0, 0.3, 0.6)) {
    expect_no_error(
      ggplot2::ggplot_build(base_plot(inner_fraction = inf_val))
    )
  }
})

test_that("plot builds with show_points = FALSE", {
  expect_no_error(ggplot2::ggplot_build(base_plot(show_points = FALSE)))
})

test_that("plot builds with shape = NA (suppress points)", {
  expect_no_error(ggplot2::ggplot_build(base_plot(shape = NA)))
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
  df_single <- rbind(
    peri_df,
    data.frame(id = "D", x = 3, value = 6.0, stringsAsFactors = FALSE)
  )
  p <- ggplot2::ggplot(
    df_single,
    ggplot2::aes(x = .data$x, id = .data$id, colour = .data$value)
  ) +
    geom_kodom_periodic() +
    coord_kodom_periodic()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("coord_kodom_periodic clockwise = FALSE builds without error", {
  p <- base_plot() + coord_kodom_periodic(clockwise = FALSE)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("scale_x_continuous labels compose with geom_kodom_periodic", {
  p <- base_plot(period = 12) +
    ggplot2::scale_x_continuous(breaks = seq(0, 12, by = 3))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("geom_kodom_periodic composes with scale_colour_kodom", {
  p <- base_plot() + scale_colour_kodom()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("geom_kodom_periodic composes with theme_kodom_periodic", {
  p <- base_plot() + theme_kodom_periodic()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("geom_kodom_periodic composes with facet_wrap", {
  df2 <- peri_df
  df2$arm <- rep(c("T", "C"), length.out = nrow(df2))
  p <- ggplot2::ggplot(
    df2,
    ggplot2::aes(x = .data$x, id = .data$id, colour = .data$value)
  ) +
    geom_kodom_periodic() +
    coord_kodom_periodic() +
    ggplot2::facet_wrap(~arm)
  expect_no_error(ggplot2::ggplot_build(p))
})
