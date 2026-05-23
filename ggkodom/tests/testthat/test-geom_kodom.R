#### TESTS: geom_kodom / stat_kodom / scale_*_kodom ####

test_that("geom_kodom() returns a list with layer + scale", {
  out <- geom_kodom()
  expect_type(out, "list")
  expect_length(out, 2L)
  expect_s3_class(out[[1]], "LayerInstance")
  expect_s3_class(out[[2]], "ScaleDiscrete")
})

test_that("geom_kodom() builds without error", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, colour = value)) +
    geom_kodom()
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), nrow(test_df))
})

test_that("sort_by = 'mean' matches prep_trajectory_data", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, colour = value)) +
    geom_kodom(sort_by = "mean")
  b <- ggplot2::ggplot_build(p)
  geom_order <- b$layout$panel_params[[1]]$y$get_labels()

  prep <- prep_trajectory_data(test_df, "id", "time", "value", sort_by = "mean")
  prep_order <- as.character(prep$id_order)

  expect_identical(geom_order, prep_order)
})

test_that("sort_by variants work without error", {
  for (sb in c("first", "last", "mean", "median")) {
    p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, colour = value)) +
      geom_kodom(sort_by = sb)
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("named sort_by vector works", {
  scores <- stats::setNames(seq_len(15), as.character(seq_len(15)))
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, colour = value)) +
    geom_kodom(sort_by = scores)
  b <- ggplot2::ggplot_build(p)
  labels <- b$layout$panel_params[[1]]$y$get_labels()
  expect_equal(labels, as.character(seq_len(15)))
})

test_that("no colour mapped produces monochrome plot", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id)) +
    geom_kodom()
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), nrow(test_df))
})

test_that("n_sample limits subjects", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, colour = value)) +
    geom_kodom(n_sample = 5)
  b <- ggplot2::ggplot_build(p)
  n_subj <- length(unique(b$data[[1]]$group))
  expect_equal(n_subj, 5L)
})

test_that("seed produces reproducible sampling", {
  build_labels <- function(s) {
    p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, colour = value)) +
      geom_kodom(n_sample = 5, seed = s)
    b <- ggplot2::ggplot_build(p)
    sort(b$layout$panel_params[[1]]$y$get_labels())
  }
  expect_identical(build_labels(42), build_labels(42))
  expect_false(identical(build_labels(1), build_labels(999)))
})

test_that("point_size = 0 does not error", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, colour = value)) +
    geom_kodom(point_size = 0)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("composability with scale_color_kodom + theme_kodom", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, colour = value)) +
    geom_kodom() +
    scale_color_kodom(color_breaks = c(5.7, 7, 10)) +
    theme_kodom()
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("composability with facet_wrap", {
  df2 <- test_df
  df2$grp <- factor(df2$id %% 3)
  p <- ggplot2::ggplot(df2, ggplot2::aes(x = time, y = id, colour = value)) +
    geom_kodom() +
    ggplot2::facet_wrap(~ grp, scales = "free_y")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("stat_kodom() builds without error", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, colour = value)) +
    stat_kodom(n_sample = 10) +
    ggplot2::scale_y_discrete()
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("scale_colour_kodom is an alias for scale_color_kodom", {
  expect_identical(scale_colour_kodom, scale_color_kodom)
})

test_that("scale_fill_kodom returns a fill scale", {
  s <- scale_fill_kodom()
  expect_s3_class(s, "ScaleContinuous")
  expect_equal(s$aesthetics, "fill")
})


#### TESTS: geom_kodom_tile ####

test_that("geom_kodom_tile() returns a list with layer + scale", {
  out <- geom_kodom_tile()
  expect_type(out, "list")
  expect_length(out, 2L)
  expect_s3_class(out[[1]], "LayerInstance")
  expect_s3_class(out[[2]], "ScaleDiscrete")
})

test_that("geom_kodom_tile() builds without error", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, fill = value)) +
    geom_kodom_tile()
  b <- ggplot2::ggplot_build(p)
  expect_gt(nrow(b$data[[1]]), 0L)
})

test_that("tile sort_by = 'mean' matches prep_trajectory_data", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, fill = value)) +
    geom_kodom_tile(sort_by = "mean")
  b <- ggplot2::ggplot_build(p)
  tile_order <- b$layout$panel_params[[1]]$y$get_labels()

  prep <- prep_trajectory_data(test_df, "id", "time", "value", sort_by = "mean")
  prep_order <- as.character(prep$id_order)

  expect_identical(tile_order, prep_order)
})

test_that("tile time_bins controls binning", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, fill = value)) +
    geom_kodom_tile(time_bins = 5)
  b <- ggplot2::ggplot_build(p)
  x_unique <- length(unique(round(b$data[[1]]$x, 4)))
  expect_lte(x_unique, 5L)
})

test_that("tile custom time_breaks work", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, fill = value)) +
    geom_kodom_tile(time_breaks = c(0, 40, 80, 120))
  b <- ggplot2::ggplot_build(p)
  mids <- sort(unique(round(b$data[[1]]$x, 1)))
  expect_equal(mids, c(20, 60, 100))
})

test_that("tile aggregation reduces rows", {
  set.seed(99)
  df_dense <- data.frame(
    id    = rep(1:5, each = 20),
    time  = runif(100, 0, 120),
    value = rnorm(100, 7, 1.5)
  )
  p <- ggplot2::ggplot(df_dense, ggplot2::aes(x = time, y = id, fill = value)) +
    geom_kodom_tile(time_bins = 5)
  b <- ggplot2::ggplot_build(p)
  expect_lte(nrow(b$data[[1]]), 25L)
})

test_that("tile n_sample limits subjects", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, fill = value)) +
    geom_kodom_tile(n_sample = 5)
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$data[[1]]$group)), 5L)
})

test_that("tile no fill mapped works", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id)) +
    geom_kodom_tile()
  b <- ggplot2::ggplot_build(p)
  expect_gt(nrow(b$data[[1]]), 0L)
})

test_that("tile composability with scale_fill_kodom + theme_kodom", {
  p <- ggplot2::ggplot(test_df, ggplot2::aes(x = time, y = id, fill = value)) +
    geom_kodom_tile() +
    scale_fill_kodom(color_breaks = c(5.7, 7, 10)) +
    theme_kodom()
  expect_silent(ggplot2::ggplot_build(p))
})
