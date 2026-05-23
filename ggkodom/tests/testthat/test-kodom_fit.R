## Tests for kodom_fit() S3 generic and its methods.

test_that("kodom_fit.default errors with an informative message", {
  expect_error(kodom_fit(list()), "No kodom_fit\\(\\) method for class")
})

test_that("kodom_fit.lmerMod returns a ggplot", {
  skip_if_not_installed("lme4")
  m <- suppressMessages(
    lme4::lmer(value ~ time + (1 + time | id), data = test_df_r)
  )
  p <- kodom_fit(m, id = "id", time = "time")
  expect_s3_class(p, "ggplot")
})

test_that("kodom_fit.lmerMod with show_points = FALSE returns a ggplot", {
  skip_if_not_installed("lme4")
  m <- suppressMessages(
    lme4::lmer(value ~ time + (1 + time | id), data = test_df_r)
  )
  p <- kodom_fit(m, id = "id", time = "time", show_points = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_fit.gam returns a ggplot", {
  skip_if_not_installed("mgcv")
  df2      <- test_df_r
  df2$id_f <- factor(df2$id)
  m <- mgcv::gam(value ~ s(time, id_f, bs = "fs", k = 4), data = df2)
  # data defaults to fit$model (which contains id_f and time)
  p <- kodom_fit(m, id = "id_f", time = "time")
  expect_s3_class(p, "ggplot")
})

test_that("kodom_fit.FPCA returns a ggplot", {
  skip_if_not_installed("fdapace")
  Ly <- split(test_df_r$value, test_df_r$id)
  Lt <- split(test_df_r$time,  test_df_r$id)
  fp <- fdapace::FPCA(Ly = Ly, Lt = Lt,
                      optns = list(dataType = "Sparse", verbose = FALSE))
  # pass ids explicitly: nrow(fp$xiEst) subjects, same order as Ly
  p <- kodom_fit(fp, ids = names(Ly)[seq_len(nrow(fp$xiEst))])
  expect_s3_class(p, "ggplot")
})
