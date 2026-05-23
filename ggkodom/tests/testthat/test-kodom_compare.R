## Tests for kodom_compare() S3 generic and its methods.

test_that("kodom_compare.lmerMod overlay (default) returns a ggplot", {
  skip_if_not_installed("lme4")
  m <- suppressMessages(
    lme4::lmer(value ~ time + (1 + time | id), data = test_df_r)
  )
  p <- kodom_compare(m, test_df_r)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_compare.lmerMod side layout returns a plot object", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("patchwork")
  m <- suppressMessages(
    lme4::lmer(value ~ time + (1 + time | id), data = test_df_r)
  )
  p <- kodom_compare(m, test_df_r, layout = "side")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("kodom_compare.FPCA overlay returns a ggplot", {
  skip_if_not_installed("fdapace")
  Ly <- split(test_df_r$value, test_df_r$id)
  Lt <- split(test_df_r$time,  test_df_r$id)
  fp <- fdapace::FPCA(Ly = Ly, Lt = Lt,
                      optns = list(dataType = "Sparse", verbose = FALSE))
  p <- kodom_compare(fp, test_df_r)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_compare.FPCA with sort_by = kodom_sort_scores returns a ggplot", {
  skip_if_not_installed("fdapace")
  Ly  <- split(test_df_r$value, test_df_r$id)
  Lt  <- split(test_df_r$time,  test_df_r$id)
  fp  <- fdapace::FPCA(Ly = Ly, Lt = Lt,
                       optns = list(dataType = "Sparse", verbose = FALSE))
  key <- kodom_sort_scores(fp, ids = as.character(unique(test_df_r$id)))
  p   <- kodom_compare(fp, test_df_r, sort_by = key)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_compare.FPCA with facet_rows = cluster returns a ggplot", {
  skip_if_not_installed("fdapace")
  Ly   <- split(test_df_r$value, test_df_r$id)
  Lt   <- split(test_df_r$time,  test_df_r$id)
  fp   <- fdapace::FPCA(Ly = Ly, Lt = Lt,
                        optns = list(dataType = "Sparse", verbose = FALSE))
  cl   <- cluster_scores(fp, k = 3L,
                         ids = as.character(unique(test_df_r$id)))
  df2  <- test_df_r
  df2$cluster <- cl[as.character(df2$id)]
  p    <- kodom_compare(fp, df2, facet_rows = "cluster")
  expect_s3_class(p, "ggplot")
})

test_that("kodom_compare errors on unsupported fit class", {
  expect_error(kodom_compare(list(), test_df_r),
               "no method for fit class")
})
