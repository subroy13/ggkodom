## Tests for kodom_scores() and kodom_sort_scores() — use mock objects.

# ── kodom_scores ─────────────────────────────────────────────────────────────

test_that("kodom_scores.FPCA returns a ggplot", {
  expect_s3_class(kodom_scores(mock_fpca), "ggplot")
})

test_that("kodom_scores.fpca.sc returns a ggplot", {
  expect_s3_class(kodom_scores(mock_fpca_sc), "ggplot")
})

test_that("kodom_scores with numeric groups returns a ggplot", {
  grp <- rnorm(10L)
  p <- kodom_scores(mock_fpca, groups = grp)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_scores with factor groups returns a ggplot", {
  grp <- factor(rep(c("A", "B"), 5L))
  p <- kodom_scores(mock_fpca, groups = grp)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_scores with character groups (coerced to factor) works", {
  grp <- rep(c("A", "B"), 5L)
  p <- kodom_scores(mock_fpca, groups = grp)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_scores show_pve = FALSE works", {
  p <- kodom_scores(mock_fpca, show_pve = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_scores with custom pcs = c(2, 1) works", {
  p <- kodom_scores(mock_fpca, pcs = c(2L, 1L))
  expect_s3_class(p, "ggplot")
})

test_that("kodom_scores.default errors on unrecognized class", {
  expect_error(kodom_scores(list()), "No kodom_scores\\(\\) method")
})

test_that("kodom_scores errors when groups length mismatches number of subjects", {
  expect_error(kodom_scores(mock_fpca, groups = 1:3))
})

# ── kodom_sort_scores ────────────────────────────────────────────────────────

test_that("kodom_sort_scores returns a named numeric vector", {
  key <- kodom_sort_scores(mock_fpca)
  expect_true(is.numeric(key))
  expect_false(is.null(names(key)))
})

test_that("kodom_sort_scores length equals number of subjects", {
  key <- kodom_sort_scores(mock_fpca)
  expect_length(key, 10L)
})

test_that("kodom_sort_scores names equal subject ids in order", {
  key <- kodom_sort_scores(mock_fpca)
  expect_equal(names(key), paste0("s", 1:10))
})

test_that("kodom_sort_scores values equal the raw score column", {
  ext <- ggkodom:::fpca_extract(mock_fpca)
  key <- kodom_sort_scores(mock_fpca, pc = 1L)
  expect_equal(unname(key), ext$scores[, 1L])
})

test_that("kodom_sort_scores pc = 2 returns second PC column", {
  key1 <- kodom_sort_scores(mock_fpca, pc = 1L)
  key2 <- kodom_sort_scores(mock_fpca, pc = 2L)
  expect_false(identical(unname(key1), unname(key2)))
})

test_that("kodom_sort_scores errors when pc exceeds K", {
  expect_error(
    kodom_sort_scores(mock_fpca, pc = 5L),
    "Requested PC 5 but only 2 available"
  )
})

test_that("kodom_sort_scores errors on ids length mismatch", {
  expect_error(
    kodom_sort_scores(mock_fpca, ids = c("a", "b")),
    "length\\(ids\\) must equal nrow\\(scores\\)"
  )
})

test_that("kodom_sort_scores key is usable as kodom_swimlane sort_by", {
  # build long-format data using the same subject ids as the mock
  df <- do.call(rbind, lapply(paste0("s", 1:10), function(id) {
    data.frame(id = id, time = seq(0, 120, 30), value = rnorm(5L, 7, 1))
  }))
  key <- kodom_sort_scores(mock_fpca)
  p <- kodom_swimlane(df, sort_by = key)
  expect_s3_class(p, "ggplot")
})
