## Tests for kodom_residuals() S3 generic and its methods.

test_that("kodom_residuals.default errors with an informative message", {
  expect_error(kodom_residuals(list()), "No kodom_residuals\\(\\) method for class")
})

test_that("kodom_residuals.lmerMod returns a ggplot", {
  skip_if_not_installed("lme4")
  m <- suppressMessages(
    lme4::lmer(value ~ time + (1 + time | id), data = test_df_r)
  )
  p <- kodom_residuals(m, test_df_r)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_residuals.lmerMod with discretize = TRUE returns a ggplot", {
  skip_if_not_installed("lme4")
  m <- suppressMessages(
    lme4::lmer(value ~ time + (1 + time | id), data = test_df_r)
  )
  p <- kodom_residuals(m, test_df_r, discretize = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_residuals.gam returns a ggplot", {
  skip_if_not_installed("mgcv")
  df2      <- test_df_r
  df2$id_f <- factor(df2$id)
  m <- mgcv::gam(value ~ s(time, id_f, bs = "fs", k = 4), data = df2)
  p <- kodom_residuals(m, df2, id = "id_f")
  expect_s3_class(p, "ggplot")
})

test_that("kodom_residuals.FPCA returns a ggplot", {
  skip_if_not_installed("fdapace")
  Ly  <- split(test_df_r$value, test_df_r$id)
  Lt  <- split(test_df_r$time,  test_df_r$id)
  fp  <- fdapace::FPCA(Ly = Ly, Lt = Lt,
                       optns = list(dataType = "Sparse", verbose = FALSE))
  p <- kodom_residuals(fp, test_df_r)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_residuals.fpca.sc returns a ggplot", {
  skip_if_not_installed("refund")
  fp  <- refund::fpca.sc(Y = with(test_df_r, {
    # fpca.sc needs a subjects × time matrix — use dense grid via interpolation
    ids <- unique(id); n <- length(ids)
    tgrid <- seq(min(time), max(time), length.out = 20L)
    m <- matrix(NA_real_, n, 20L)
    for (i in seq_along(ids)) {
      sub <- test_df_r[test_df_r$id == ids[i], ]
      m[i, ] <- approx(sub$time, sub$value, tgrid, rule = 2)$y
    }
    m
  }))
  # fpca.sc returns an object without inputData — ids auto-indexed
  expect_s3_class(kodom_residuals(fp, test_df_r), "ggplot")
})
