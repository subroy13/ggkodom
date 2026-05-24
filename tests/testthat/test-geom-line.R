#### TESTS FOR geom_kodom_line ####

# Minimal dataset: 3 subjects × 4 visits.
# Subject means: A=6.5, B=7.5, C=5.5 → desc order: B, A, C
line_df <- data.frame(
  id    = rep(c("A", "B", "C"), each = 4),
  x     = rep(c(0, 8, 16, 24), 3),
  value = c(5, 6.5, 7, 7.5,   7, 7.5, 8, 7.5,   5, 5.5, 6, 5.5),
  stringsAsFactors = FALSE
)

base_plot <- function(...) {
  ggplot2::ggplot(line_df, ggplot2::aes(x = x, id = id, colour = value)) +
    geom_kodom_line(...)
}

# ---- layer object ----

test_that("geom_kodom_line() returns a valid ggplot2 layer", {
  layer <- geom_kodom_line()
  expect_true(ggplot2::is_ggproto(layer$geom))
  expect_true(ggplot2::is_ggproto(layer$stat))
})

test_that("geom_kodom_line() can be added to a ggplot", {
  p <- ggplot2::ggplot() + geom_kodom_line()
  expect_s3_class(p, "gg")
})

# ---- stat output: lane assignment ----

stat_data <- data.frame(
  id     = rep(c("A", "B", "C"), each = 4),
  x      = rep(c(0, 8, 16, 24), 3),
  colour = c(5, 6.5, 7, 7.5,   7, 7.5, 8, 7.5,   5, 5.5, 6, 5.5),
  PANEL  = factor(1),
  group  = rep(1:3, each = 4),
  stringsAsFactors = FALSE
)

test_that("StatKodomLine$compute_panel adds a y column", {
  out <- ggkodom:::StatKodomLine$compute_panel(
    stat_data, scales = list(), sort_by = "none", n_max = Inf
  )
  expect_true("y" %in% names(out))
})

test_that("StatKodomLine$compute_panel y values are consecutive integers 1:n", {
  out <- ggkodom:::StatKodomLine$compute_panel(
    stat_data, scales = list(), sort_by = "none", n_max = Inf
  )
  expect_setequal(unique(out$y), 1:3)
})

test_that("StatKodomLine sort_by='mean' puts highest-mean subject in lane 1", {
  # B mean = 7.5 > A mean = 6.5 > C mean = 5.5
  out <- ggkodom:::StatKodomLine$compute_panel(
    stat_data, scales = list(), sort_by = "mean", n_max = Inf
  )
  y_b <- unique(out$y[out$id == "B"])
  y_a <- unique(out$y[out$id == "A"])
  y_c <- unique(out$y[out$id == "C"])
  expect_lt(y_b, y_a)
  expect_lt(y_a, y_c)
})

test_that("StatKodomLine n_max subsamples subjects", {
  out <- ggkodom:::StatKodomLine$compute_panel(
    stat_data, scales = list(), sort_by = "none", n_max = 2L
  )
  expect_lte(length(unique(out$id)), 2L)
})

# ---- full plot build ----

test_that("basic geom_kodom_line plot builds without error", {
  expect_no_error(ggplot2::ggplot_build(base_plot()))
})

test_that("plot builds with each sort_by option", {
  for (sb in c("none", "mean", "mean_asc", "first", "last")) {
    expect_no_error(ggplot2::ggplot_build(base_plot(sort_by = sb)))
  }
})

test_that("plot builds with show_points = FALSE", {
  expect_no_error(ggplot2::ggplot_build(base_plot(show_points = FALSE)))
})

test_that("plot builds with shape = NA (implicit point suppression)", {
  expect_no_error(ggplot2::ggplot_build(base_plot(shape = NA)))
})

test_that("plot builds with size = 0 (implicit point suppression)", {
  expect_no_error(ggplot2::ggplot_build(base_plot(size = 0)))
})

test_that("plot builds with custom linewidth and alpha", {
  expect_no_error(ggplot2::ggplot_build(base_plot(linewidth = 2, alpha = 0.5)))
})

test_that("plot builds with n_max restricting subjects", {
  expect_no_error(ggplot2::ggplot_build(base_plot(n_max = 2L)))
})

test_that("built plot y-range spans n_subjects lanes", {
  built <- ggplot2::ggplot_build(base_plot())
  d     <- built$data[[1L]]
  expect_equal(length(unique(d$y)), 3L)
})

test_that("geom_kodom_line composes with scale_colour_kodom", {
  p <- base_plot() + scale_colour_kodom()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("geom_kodom_line composes with facet_wrap", {
  df2      <- line_df
  df2$arm  <- rep(c("T", "C"), length.out = nrow(df2))
  p <- ggplot2::ggplot(
    df2,
    ggplot2::aes(x = x, id = id, colour = value)
  ) + geom_kodom_line() + ggplot2::facet_wrap(~arm)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("single-visit subjects do not cause an error", {
  df_single <- rbind(
    line_df,
    data.frame(id = "D", x = 5, value = 6.0, stringsAsFactors = FALSE)
  )
  p <- ggplot2::ggplot(
    df_single,
    ggplot2::aes(x = x, id = id, colour = value)
  ) + geom_kodom_line()
  expect_no_error(ggplot2::ggplot_build(p))
})
