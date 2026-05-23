## Tests for cluster_scores() — uses mock FPCA objects, no optional packages.

test_that("cluster_scores returns a factor", {
  cl <- cluster_scores(mock_fpca, k = 2L, seed = 1L)
  expect_s3_class(cl, "factor")
})

test_that("cluster_scores returns exactly k levels", {
  cl2 <- cluster_scores(mock_fpca, k = 2L, seed = 1L)
  cl3 <- cluster_scores(mock_fpca, k = 3L, seed = 1L)
  expect_equal(nlevels(cl2), 2L)
  expect_equal(nlevels(cl3), 3L)
})

test_that("cluster_scores output is named by subject ids", {
  cl <- cluster_scores(mock_fpca, k = 2L, seed = 1L)
  expect_equal(names(cl), paste0("s", 1:10))
})

test_that("cluster_scores length equals number of subjects", {
  cl <- cluster_scores(mock_fpca, k = 2L, seed = 1L)
  expect_length(cl, 10L)
})

test_that("cluster_scores kmeans is reproducible with the same seed", {
  cl1 <- cluster_scores(mock_fpca, k = 3L, seed = 42L)
  cl2 <- cluster_scores(mock_fpca, k = 3L, seed = 42L)
  expect_equal(cl1, cl2)
})

test_that("cluster_scores hclust method returns a valid factor", {
  cl <- cluster_scores(mock_fpca, k = 2L, method = "hclust")
  expect_s3_class(cl, "factor")
  expect_equal(nlevels(cl), 2L)
  expect_equal(names(cl), paste0("s", 1:10))
})

test_that("cluster_scores n_pcs = 1 uses only the first PC", {
  cl_1pc <- cluster_scores(mock_fpca, k = 2L, n_pcs = 1L, seed = 7L)
  cl_2pc <- cluster_scores(mock_fpca, k = 2L, n_pcs = 2L, seed = 7L)
  expect_s3_class(cl_1pc, "factor")
  # 1-PC and 2-PC clusterings may differ — just confirm both are valid
  expect_equal(nlevels(cl_1pc), 2L)
  expect_equal(nlevels(cl_2pc), 2L)
})

test_that("cluster_scores silently caps n_pcs at K_avail", {
  # Requesting more PCs than available (K = 2) should not error
  cl <- cluster_scores(mock_fpca, k = 2L, n_pcs = 99L, seed = 1L)
  expect_s3_class(cl, "factor")
})

test_that("cluster_scores errors when ids length mismatches score rows", {
  expect_error(
    cluster_scores(mock_fpca, k = 2L, ids = c("a", "b")),
    "length\\(ids\\) must equal nrow\\(scores\\)"
  )
})

test_that("cluster_scores works on a refund fpca.sc mock", {
  cl <- cluster_scores(mock_fpca_sc, k = 2L, seed = 1L)
  expect_s3_class(cl, "factor")
  expect_equal(nlevels(cl), 2L)
  expect_equal(names(cl), paste0("s", 1:10))
})

test_that("cluster_scores factor is usable as kodom_scores groups", {
  cl <- cluster_scores(mock_fpca, k = 2L, seed = 1L)
  p  <- kodom_scores(mock_fpca, groups = cl)
  expect_s3_class(p, "ggplot")
})

test_that("cluster_scores factor levels are sorted integers", {
  cl <- cluster_scores(mock_fpca, k = 3L, seed = 1L)
  expect_equal(levels(cl), as.character(sort(unique(as.integer(cl)))))
})
