#### TESTS FOR geom_kodom_heatmap ####

heat_df <- data.frame(
  id    = rep(c("A", "B", "C"), each = 6),
  x     = rep(seq(2, 22, length.out = 6), 3),
  value = c(5, 6, 7, 8, 7, 6, 9, 8, 7, 8, 9, 8, 5, 5.5, 6, 5, 5.5, 6),
  stringsAsFactors = FALSE
)

base_plot <- function(...) {
  ggplot2::ggplot(
    heat_df,
    ggplot2::aes(x = x, id = id, fill = value)
  ) + geom_kodom_heatmap(...)
}

# ---- layer object ----

test_that("geom_kodom_heatmap() returns a valid ggplot2 layer", {
  layer <- geom_kodom_heatmap()
  expect_true(ggplot2::is_ggproto(layer$geom))
  expect_true(ggplot2::is_ggproto(layer$stat))
})

test_that("geom_kodom_heatmap() can be added to a ggplot", {
  p <- ggplot2::ggplot() + geom_kodom_heatmap()
  expect_s3_class(p, "gg")
})

# ---- stat output ----

# group 1 = A (fill mean 6.5), group 2 = B (mean 8.17), group 3 = C (5.5)
stat_data <- data.frame(
  id    = rep(c("A", "B", "C"), each = 6),
  x     = rep(seq(2, 22, length.out = 6), 3),
  fill  = c(5, 6, 7, 8, 7, 6, 9, 8, 7, 8, 9, 8, 5, 5.5, 6, 5, 5.5, 6),
  PANEL = factor(1),
  group = rep(1:3, each = 6),
  stringsAsFactors = FALSE
)

compute <- function(...) {
  ggkodom:::StatKodomHeatmap$compute_panel(
    stat_data, scales = list(), ...
  )
}

test_that("StatKodomHeatmap$compute_panel returns a data frame", {
  out <- compute(sort_by = "mean", n_max = Inf, bins = 4L,
                 breaks = NULL, fun = "mean")
  expect_s3_class(out, "data.frame")
})

test_that("compute_panel output has tile geometry columns", {
  out <- compute(sort_by = "mean", n_max = Inf, bins = 4L,
                 breaks = NULL, fun = "mean")
  expect_true(
    all(c("x", "y", "fill", "width", "height") %in% names(out))
  )
})

test_that("compute_panel bins controls time resolution", {
  out_low  <- compute(sort_by = "none", n_max = Inf,
                      bins = 5L, breaks = NULL, fun = "mean")
  out_high <- compute(sort_by = "none", n_max = Inf,
                      bins = 10L, breaks = NULL, fun = "mean")
  expect_gte(length(unique(out_high$x)), length(unique(out_low$x)))
})

test_that("compute_panel breaks overrides bins", {
  out <- compute(sort_by = "none", n_max = Inf,
                 bins = 99L, breaks = c(0, 12, 24), fun = "mean")
  expect_lte(length(unique(out$x)), 2L)
})

test_that("compute_panel fun='mean' aggregates to correct value", {
  # A (group 1) collapsed to one bin: mean(5,6,7,8,7,6) = 6.5
  out <- compute(sort_by = "none", n_max = Inf,
                 bins = 1L, breaks = NULL, fun = "mean")
  expect_equal(
    out$fill[out$group == 1L],
    mean(c(5, 6, 7, 8, 7, 6)),
    tolerance = 1e-6
  )
})

test_that("compute_panel fun='min' returns minimum fill per bin", {
  # B (group 2) collapsed to one bin: min(9,8,7,8,9,8) = 7
  out <- compute(sort_by = "none", n_max = Inf,
                 bins = 1L, breaks = NULL, fun = "min")
  expect_equal(out$fill[out$group == 2L], 7, tolerance = 1e-6)
})

test_that("compute_panel fun='max' returns maximum fill per bin", {
  # B (group 2) collapsed to one bin: max(9,8,7,8,9,8) = 9
  out <- compute(sort_by = "none", n_max = Inf,
                 bins = 1L, breaks = NULL, fun = "max")
  expect_equal(out$fill[out$group == 2L], 9, tolerance = 1e-6)
})

test_that("sort_by='mean' puts highest-mean subject in lane 1", {
  # B (group 2) has fill mean 8.17 — highest of the three
  out <- compute(sort_by = "mean", n_max = Inf,
                 bins = 4L, breaks = NULL, fun = "mean")
  expect_equal(out$y[out$group == 2L][1L], 1L)
})

test_that("compute_panel n_max subsamples subjects", {
  out <- compute(sort_by = "none", n_max = 2L,
                 bins = 4L, breaks = NULL, fun = "mean")
  expect_lte(length(unique(out$y)), 2L)
})

test_that("compute_panel height column is always 1", {
  out <- compute(sort_by = "none", n_max = Inf,
                 bins = 4L, breaks = NULL, fun = "mean")
  expect_true(all(out$height == 1))
})

test_that("compute_panel width is uniform within a run", {
  out <- compute(sort_by = "none", n_max = Inf,
                 bins = 4L, breaks = NULL, fun = "mean")
  expect_equal(length(unique(out$width)), 1L)
})

test_that("invalid fun raises an error", {
  expect_error(
    compute(sort_by = "none", n_max = Inf,
            bins = 4L, breaks = NULL, fun = "variance")
  )
})

# ---- full plot build ----

test_that("basic geom_kodom_heatmap plot builds without error", {
  expect_no_error(ggplot2::ggplot_build(base_plot()))
})

test_that("plot builds with each fun option", {
  for (f in c("mean", "median", "first", "last", "min", "max")) {
    expect_no_error(ggplot2::ggplot_build(base_plot(fun = f)))
  }
})

test_that("plot builds with explicit breaks", {
  expect_no_error(
    ggplot2::ggplot_build(base_plot(breaks = c(0, 12, 24)))
  )
})

test_that("plot builds with each sort_by option", {
  for (sb in c("none", "mean", "mean_asc", "first", "last")) {
    expect_no_error(ggplot2::ggplot_build(base_plot(sort_by = sb)))
  }
})

test_that("plot builds with custom tile border aesthetics", {
  expect_no_error(
    ggplot2::ggplot_build(base_plot(colour = "grey50", linewidth = 1))
  )
})

test_that("geom_kodom_heatmap composes with scale_fill_kodom", {
  p <- base_plot() + scale_fill_kodom()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("geom_kodom_heatmap composes with facet_wrap", {
  df2     <- heat_df
  df2$arm <- rep(c("T", "C"), length.out = nrow(df2))
  p <- ggplot2::ggplot(
    df2,
    ggplot2::aes(x = x, id = id, fill = value)
  ) + geom_kodom_heatmap() + ggplot2::facet_wrap(~arm)
  expect_no_error(ggplot2::ggplot_build(p))
})
