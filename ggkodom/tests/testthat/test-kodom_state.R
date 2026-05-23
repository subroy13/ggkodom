test_that("kodom_state with thresholds returns a ggplot", {
  p <- kodom_state(test_df, thresholds = c(6.5, 8.0))
  expect_s3_class(p, "ggplot")
})

test_that("kodom_state with a single threshold (2 states) works", {
  p <- kodom_state(test_df, thresholds = 7.0)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_state with pre-computed state column works", {
  df2 <- test_df
  df2$state_col <- ifelse(df2$value < 7.0, "low", "high")
  p <- kodom_state(df2, state = "state_col")
  expect_s3_class(p, "ggplot")
})

test_that("kodom_state with custom state_labels works", {
  p <- kodom_state(test_df,
                   thresholds = c(6.5, 8.0),
                   state_labels = c("controlled", "borderline", "uncontrolled"))
  expect_s3_class(p, "ggplot")
})

test_that("kodom_state with custom state_colors works", {
  p <- kodom_state(test_df,
                   thresholds = c(6.5, 8.0),
                   state_colors = c("green", "orange", "red"))
  expect_s3_class(p, "ggplot")
})

test_that("kodom_state errors when neither thresholds nor state is given", {
  expect_error(kodom_state(test_df), "Provide either 'thresholds' or 'state'")
})

test_that("kodom_state errors when state column does not exist", {
  expect_error(kodom_state(test_df, state = "no_such_col"))
})
