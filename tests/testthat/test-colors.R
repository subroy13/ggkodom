#### TESTS FOR colors.R ####

# ---- kodom_colors ----

test_that("kodom_colors(3) returns the three palette anchors in order", {
  cols <- kodom_colors(3)
  expect_equal(cols[[1L]], "#008D98")
  expect_equal(cols[[2L]], "#FFCC3D")
  expect_equal(cols[[3L]], "#D7433B")
})

test_that("kodom_colors returns a character vector of the requested length", {
  for (n in c(1L, 3L, 5L, 10L)) {
    expect_length(kodom_colors(n), n)
    expect_type(kodom_colors(n), "character")
  }
})

test_that("kodom_colors output values are valid hex colour strings", {
  cols <- kodom_colors(7L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that("kodom_colors(1) returns the teal anchor", {
  expect_equal(kodom_colors(1L), "#008D98")
})

test_that("kodom_colors interpolates smoothly — no duplicate hex values for n > 3", {
  cols <- kodom_colors(9L)
  expect_equal(length(unique(cols)), 9L)
})

# ---- scale_colour_kodom / scale_color_kodom ----

test_that("scale_colour_kodom returns a ggplot2 Scale object", {
  sc <- scale_colour_kodom()
  expect_true(inherits(sc, "Scale"))
})

test_that("scale_color_kodom is an alias for scale_colour_kodom", {
  expect_equal(
    scale_colour_kodom()$aesthetics,
    scale_color_kodom()$aesthetics
  )
})

test_that("scale_colour_kodom maps to the colour/color aesthetic", {
  sc <- scale_colour_kodom()
  expect_true(any(sc$aesthetics %in% c("colour", "color")))
})

test_that("scale_colour_kodom accepts a custom name", {
  sc <- scale_colour_kodom(name = "HbA1c (%)")
  expect_equal(sc$name, "HbA1c (%)")
})

test_that("scale_colour_kodom(discretize = TRUE) returns a discrete-friendly scale", {
  expect_no_error(scale_colour_kodom(discretize = TRUE, color_breaks = c(5.7, 6.5, 8)))
})

test_that("scale_colour_kodom integrates with a ggplot without error", {
  df <- data.frame(x = 1:3, y = 1:3, col = c(5, 7, 9))
  p  <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, colour = col)) +
    ggplot2::geom_point() +
    scale_colour_kodom()
  expect_no_error(ggplot2::ggplot_build(p))
})

# ---- scale_fill_kodom ----

test_that("scale_fill_kodom returns a ggplot2 Scale object", {
  sc <- scale_fill_kodom()
  expect_true(inherits(sc, "Scale"))
})

test_that("scale_fill_kodom maps to the fill aesthetic", {
  sc <- scale_fill_kodom()
  expect_true("fill" %in% sc$aesthetics)
})

test_that("scale_fill_kodom integrates with a ggplot without error", {
  df <- data.frame(x = 1:3, y = 1:3, f = c(5, 7, 9))
  p  <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = f)) +
    ggplot2::geom_col() +
    scale_fill_kodom()
  expect_no_error(ggplot2::ggplot_build(p))
})
