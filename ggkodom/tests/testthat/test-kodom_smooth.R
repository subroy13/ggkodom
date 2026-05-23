## Tests for kodom_smooth() — all require optional packages.

test_that("kodom_smooth errors on an unrecognised method", {
  expect_error(kodom_smooth(test_df_r, method = "bayes"))
})

test_that("kodom_smooth fpca returns a list with fit, refit_fn, method, call_args", {
  skip_if_not_installed("fdapace")
  sm <- kodom_smooth(test_df_r, method = "fpca")
  expect_true(is.list(sm))
  expect_true(all(c("fit", "refit_fn", "method", "call_args") %in% names(sm)))
})

test_that("kodom_smooth fpca fit is an FPCA object", {
  skip_if_not_installed("fdapace")
  sm <- kodom_smooth(test_df_r, method = "fpca")
  expect_s3_class(sm$fit, "FPCA")
})

test_that("kodom_smooth fpca refit_fn is a function", {
  skip_if_not_installed("fdapace")
  sm <- kodom_smooth(test_df_r, method = "fpca")
  expect_true(is.function(sm$refit_fn))
})

test_that("kodom_smooth fpca method field is 'fpca'", {
  skip_if_not_installed("fdapace")
  sm <- kodom_smooth(test_df_r, method = "fpca")
  expect_equal(sm$method, "fpca")
})

test_that("kodom_smooth fpca refit_fn produces an FPCA object on new data", {
  skip_if_not_installed("fdapace")
  sm      <- kodom_smooth(test_df_r, method = "fpca")
  ids_use <- sample(unique(test_df_r$id), 15L, replace = TRUE)
  long    <- do.call(rbind, lapply(seq_along(ids_use), function(i) {
    sub <- test_df_r[test_df_r$id == ids_use[i], , drop = FALSE]
    sub$id <- paste0(ids_use[i], "_b", i)
    sub
  }))
  refit <- sm$refit_fn(long)
  expect_s3_class(refit, "FPCA")
})

test_that("kodom_smooth lmer fit is a lmerMod", {
  skip_if_not_installed("lme4")
  sm <- suppressMessages(kodom_smooth(test_df_r, method = "lmer"))
  expect_true(inherits(sm$fit, "lmerMod"))
})

test_that("kodom_smooth lmer refit_fn is a function", {
  skip_if_not_installed("lme4")
  sm <- suppressMessages(kodom_smooth(test_df_r, method = "lmer"))
  expect_true(is.function(sm$refit_fn))
})

test_that("kodom_smooth gam fit is a gam", {
  skip_if_not_installed("mgcv")
  sm <- kodom_smooth(test_df_r, method = "gam")
  expect_s3_class(sm$fit, "gam")
})

test_that("kodom_smooth gam refit_fn is a function", {
  skip_if_not_installed("mgcv")
  sm <- kodom_smooth(test_df_r, method = "gam")
  expect_true(is.function(sm$refit_fn))
})

test_that("kodom_smooth fpca + fpca_boot_subjects roundtrip produces boot fits", {
  skip_if_not_installed("fdapace")
  sm    <- kodom_smooth(test_df_r, method = "fpca")
  boots <- fpca_boot_subjects(test_df_r, B = 3L,
                              refit_fn = sm$refit_fn, seed = 1L)
  expect_gte(length(boots), 1L)
  expect_true(all(vapply(boots, function(x) inherits(x, "FPCA"), logical(1L))))
})

test_that("kodom_smooth fpca + fpca_boot_subjects feeds into kodom_components", {
  skip_if_not_installed("fdapace")
  sm    <- kodom_smooth(test_df_r, method = "fpca")
  boots <- fpca_boot_subjects(test_df_r, B = 3L,
                              refit_fn = sm$refit_fn, seed = 1L)
  p <- kodom_components(sm$fit, boot_fits = boots)
  expect_s3_class(p, "ggplot")
})
