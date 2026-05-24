#### TESTS FOR geom_kodom_circular ####

circ_df <- data.frame(
  id    = rep(c("A", "B", "C", "D"), each = 5),
  x     = rep(seq(0, 24, length.out = 5), 4),
  value = c(
    5, 6, 7, 6, 5, 8, 9, 8, 7, 8,
    5.5, 6, 5, 6, 5.5, 7, 7.5, 8, 7, 7.5
  ),
  stringsAsFactors = FALSE
)

base_plot <- function(...) {
  ggplot2::ggplot(
    circ_df,
    ggplot2::aes(x = x, id = id, colour = value)
  ) + geom_kodom_circular(...) + ggplot2::coord_fixed()
}

# ---- layer object ----

test_that("geom_kodom_circular() returns a valid ggplot2 layer", {
  layer <- geom_kodom_circular()
  expect_true(ggplot2::is_ggproto(layer$geom))
  expect_true(ggplot2::is_ggproto(layer$stat))
})

test_that("geom_kodom_circular() can be added to a ggplot", {
  p <- ggplot2::ggplot() + geom_kodom_circular()
  expect_s3_class(p, "gg")
})

# ---- stat output ----

stat_data <- data.frame(
  id     = rep(c("A", "B", "C", "D"), each = 5),
  x      = rep(seq(0, 24, length.out = 5), 4),
  colour = c(
    5, 6, 7, 6, 5, 8, 9, 8, 7, 8,
    5.5, 6, 5, 6, 5.5, 7, 7.5, 8, 7, 7.5
  ),
  PANEL  = factor(1),
  group  = rep(1:4, each = 5),
  stringsAsFactors = FALSE
)

compute <- function(...) {
  ggkodom:::StatKodomCircular$compute_panel(
    stat_data, scales = list(), ...
  )
}

default_args <- list(
  sort_by = "none", n_max = Inf,
  gap_fraction = 0.15, inner_fraction = 0.3, direction = 1L
)

test_that("StatKodomCircular$compute_panel returns a data frame", {
  out <- do.call(compute, default_args)
  expect_s3_class(out, "data.frame")
})

test_that("compute_panel replaces original x/y with Cartesian coordinates", {
  out <- do.call(compute, default_args)
  expect_true("x" %in% names(out))
  expect_true("y" %in% names(out))
  expect_false(all(out$x == stat_data$x))
})

test_that("compute_panel output x and y are all finite", {
  out <- do.call(compute, default_args)
  expect_true(all(is.finite(out$x)))
  expect_true(all(is.finite(out$y)))
})

test_that("inner_fraction > 0 produces strictly larger radii than 0", {
  args_inner <- modifyList(default_args, list(inner_fraction = 0.5))
  args_zero  <- modifyList(default_args, list(inner_fraction = 0.0))
  out_inner  <- do.call(compute, args_inner)
  out_zero   <- do.call(compute, args_zero)
  rad_inner  <- sqrt(out_inner$x^2 + out_inner$y^2)
  rad_zero   <- sqrt(out_zero$x^2  + out_zero$y^2)
  expect_true(all(rad_inner >= rad_zero - 1e-9))
})

test_that("direction=1 and direction=-1 mirror x, preserve y", {
  # theta = pi/2 - d * angle → flipping d negates sin(pi/2-…)=cos(…) for x,
  # but sin stays the same for y.
  args_cw  <- modifyList(default_args, list(direction =  1L))
  args_ccw <- modifyList(default_args, list(direction = -1L))
  out_cw   <- do.call(compute, args_cw)
  out_ccw  <- do.call(compute, args_ccw)
  expect_equal(out_cw$x,  -out_ccw$x, tolerance = 1e-9)
  expect_equal(out_cw$y,   out_ccw$y, tolerance = 1e-9)
})

test_that("gap_fraction does not change the number of rows", {
  args_small <- modifyList(default_args, list(gap_fraction = 0.05))
  args_large <- modifyList(default_args, list(gap_fraction = 0.40))
  expect_equal(nrow(do.call(compute, args_small)),
               nrow(do.call(compute, args_large)))
})

test_that("sort_by='mean' does not error and returns correct row count", {
  args <- modifyList(default_args, list(sort_by = "mean"))
  out  <- do.call(compute, args)
  expect_equal(nrow(out), nrow(stat_data))
})

test_that("compute_panel n_max subsamples subjects", {
  args <- modifyList(default_args, list(n_max = 2L))
  out  <- do.call(compute, args)
  expect_lte(length(unique(out$id)), 2L)
})

test_that("all observations of one subject share a single angle", {
  out    <- do.call(compute, default_args)
  by_id  <- split(out, out$id)
  for (gd in by_id) {
    angles <- round(atan2(gd$y, gd$x), 8)
    expect_equal(length(unique(angles)), 1L)
  }
})

test_that("radius increases monotonically with time within each subject", {
  out   <- do.call(compute, default_args)
  by_id <- split(out, out$id)
  for (gd in by_id) {
    orig_x <- stat_data$x[stat_data$id == gd$id[1L]]
    gd     <- gd[order(orig_x), ]
    rad    <- sqrt(gd$x^2 + gd$y^2)
    expect_true(all(diff(rad) >= -1e-9))
  }
})

# ---- full plot build ----

test_that("basic geom_kodom_circular plot builds without error", {
  expect_no_error(ggplot2::ggplot_build(base_plot()))
})

test_that("plot builds with show_points = FALSE", {
  expect_no_error(ggplot2::ggplot_build(base_plot(show_points = FALSE)))
})

test_that("plot builds with direction = -1L", {
  expect_no_error(ggplot2::ggplot_build(base_plot(direction = -1L)))
})

test_that("plot builds with varied gap_fraction values", {
  for (gf in c(0.01, 0.15, 0.35)) {
    expect_no_error(ggplot2::ggplot_build(base_plot(gap_fraction = gf)))
  }
})

test_that("plot builds with varied inner_fraction values", {
  for (inf_val in c(0, 0.3, 0.6)) {
    expect_no_error(
      ggplot2::ggplot_build(base_plot(inner_fraction = inf_val))
    )
  }
})

test_that("plot builds with each sort_by option", {
  for (sb in c("none", "mean", "mean_asc", "first", "last")) {
    expect_no_error(ggplot2::ggplot_build(base_plot(sort_by = sb)))
  }
})

test_that("plot builds with shape = NA (suppress points)", {
  expect_no_error(ggplot2::ggplot_build(base_plot(shape = NA)))
})

test_that("geom_kodom_circular composes with scale_colour_kodom", {
  p <- base_plot() + scale_colour_kodom()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("geom_kodom_circular composes with theme_kodom_circular", {
  p <- base_plot() + theme_kodom_circular()
  expect_no_error(ggplot2::ggplot_build(p))
})
