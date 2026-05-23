test_that("kodom_colors() returns the 3 brand anchors", {
  cols <- kodom_colors()
  expect_length(cols, 3L)
  expect_equal(cols, c("#008D98", "#FFCC3D", "#D7433B"))
})

test_that("kodom_colors(1) returns just the first anchor", {
  expect_equal(kodom_colors(1L), "#008D98")
})

test_that("kodom_colors(2) returns first two anchors", {
  expect_equal(kodom_colors(2L), c("#008D98", "#FFCC3D"))
})

test_that("kodom_colors(7) returns 7 valid hex colors", {
  cols <- kodom_colors(7L)
  expect_length(cols, 7L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that("kodom_colors interpolation is monotone: first/last anchors preserved", {
  cols <- kodom_colors(7L)
  expect_equal(cols[1L], "#008D98")
  expect_equal(cols[7L], "#D7433B")
})

test_that("kodom_gradient_scale returns a ggplot2 Scale for color", {
  s <- kodom_gradient_scale("color")
  expect_s3_class(s, "Scale")
})

test_that("kodom_gradient_scale returns a ggplot2 Scale for fill", {
  s <- kodom_gradient_scale("fill")
  expect_s3_class(s, "Scale")
})

test_that("kodom_gradient_scale accepts color_breaks without error", {
  expect_no_error(kodom_gradient_scale("color", color_breaks = c(5.5, 7.0, 10.0)))
})

test_that("kodom_step_scale returns a ggplot2 Scale", {
  s <- kodom_step_scale("color", color_breaks = c(5.5, 7.0, 8.0))
  expect_s3_class(s, "Scale")
})

test_that("kodom_step_scale works with fill aesthetic", {
  s <- kodom_step_scale("fill", color_breaks = c(6.5, 8.0))
  expect_s3_class(s, "Scale")
})

test_that("kodom_step_scale auto-expands colors to cover all bands", {
  # 2 breaks → 3 bands; only 1 color supplied → should not error
  expect_no_error(kodom_step_scale("color",
    colors = "#008D98",
    color_breaks = c(6.5, 8.0)
  ))
})
