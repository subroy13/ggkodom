test_that("default call (path / linear) returns a ggplot", {
  expect_s3_class(kodom_swimlane(test_df), "ggplot")
})

test_that("discretize = TRUE returns a ggplot", {
  p <- kodom_swimlane(test_df, discretize = TRUE, color_breaks = c(5.5, 7.0, 8.0))
  expect_s3_class(p, "ggplot")
})

test_that("geom = 'tile' returns a ggplot", {
  p <- kodom_swimlane(test_df, geom = "tile", time_bins = 5L)
  expect_s3_class(p, "ggplot")
})

test_that("geom = 'tile' with facet_rows returns a ggplot", {
  df2 <- test_df
  df2$grp <- factor(df2$id %% 2L)
  p <- kodom_swimlane(df2, geom = "tile", time_bins = 5L, facet_rows = "grp")
  expect_s3_class(p, "ggplot")
})

test_that("coord = 'polar' returns a ggplot", {
  p <- kodom_swimlane(test_df, coord = "polar")
  expect_s3_class(p, "ggplot")
})

test_that("arc_degrees = 270 (fan layout) returns a ggplot", {
  p <- kodom_swimlane(test_df, coord = "polar", arc_degrees = 270)
  expect_s3_class(p, "ggplot")
})

test_that("tile + polar is rejected with an informative error", {
  expect_error(
    kodom_swimlane(test_df, geom = "tile", coord = "polar"),
    "coord = 'polar' is supported only with geom = 'path'"
  )
})

test_that("arc_degrees = 0 is rejected", {
  expect_error(kodom_swimlane(test_df, coord = "polar", arc_degrees = 0),
               "arc_degrees must be in")
})

test_that("arc_degrees > 360 is rejected", {
  expect_error(kodom_swimlane(test_df, coord = "polar", arc_degrees = 400),
               "arc_degrees must be in")
})

test_that("sort_by string options all work", {
  for (s in c("first", "last", "mean", "median")) {
    expect_s3_class(kodom_swimlane(test_df, sort_by = s), "ggplot")
  }
})

test_that("sort_by named numeric vector works", {
  key <- stats::setNames(seq_len(15L), as.character(seq_len(15L)))
  p <- kodom_swimlane(test_df, sort_by = key)
  expect_s3_class(p, "ggplot")
})

test_that("custom color_breaks are accepted", {
  p <- kodom_swimlane(test_df, color_breaks = c(5.0, 7.0, 10.0))
  expect_s3_class(p, "ggplot")
})

test_that("tile agg_fun options all work", {
  for (fn in c("mean", "median", "first", "last", "min", "max")) {
    p <- kodom_swimlane(test_df, geom = "tile", time_bins = 5L, agg_fun = fn)
    expect_s3_class(p, "ggplot")
  }
})
