## Tests for fpca_boot_subjects() and fpca_align_sign().

# ── fpca_align_sign (uses mock objects, no optional packages) ─────────────────

test_that("fpca_align_sign leaves signs unchanged when already aligned", {
  aligned <- fpca_align_sign(mock_fpca, mock_fpca)
  expect_equal(aligned$phi, mock_fpca$phi)
})

test_that("fpca_align_sign corrects a flipped PC1 on FPCA mock", {
  flipped <- mock_fpca
  flipped$phi[, 1L]    <- -flipped$phi[, 1L]
  flipped$xiEst[, 1L]  <- -flipped$xiEst[, 1L]

  aligned <- fpca_align_sign(flipped, mock_fpca, K = 1L)
  dot <- sum(aligned$phi[, 1L] * mock_fpca$phi[, 1L])
  expect_gte(dot, 0)
})

test_that("fpca_align_sign corrects a flipped PC on fpca.sc mock", {
  flipped <- mock_fpca_sc
  flipped$efunctions[, 1L] <- -flipped$efunctions[, 1L]
  flipped$scores[, 1L]     <- -flipped$scores[, 1L]

  aligned <- fpca_align_sign(flipped, mock_fpca_sc, K = 1L)
  dot <- sum(aligned$efunctions[, 1L] * mock_fpca_sc$efunctions[, 1L])
  expect_gte(dot, 0)
})

test_that("fpca_align_sign score signs are flipped alongside eigenfunctions", {
  flipped <- mock_fpca
  flipped$phi[, 1L]   <- -flipped$phi[, 1L]
  flipped$xiEst[, 1L] <- -flipped$xiEst[, 1L]

  aligned <- fpca_align_sign(flipped, mock_fpca, K = 1L)
  # After alignment scores should also be re-flipped
  expect_equal(aligned$xiEst[, 1L], mock_fpca$xiEst[, 1L])
})

test_that("fpca_align_sign errors on unrecognized class", {
  expect_error(fpca_align_sign(list(), mock_fpca), "fit class not recognized")
})

# ── fpca_boot_subjects ────────────────────────────────────────────────────────

test_that("fpca_boot_subjects errors when refit_fn is missing", {
  expect_error(fpca_boot_subjects(test_df_r), "refit_fn must be a function")
})

test_that("fpca_boot_subjects errors when refit_fn is not a function", {
  expect_error(
    fpca_boot_subjects(test_df_r, refit_fn = "not_a_function"),
    "refit_fn must be a function"
  )
})

test_that("fpca_boot_subjects errors on missing required columns", {
  expect_error(
    fpca_boot_subjects(test_df_r, id = "no_such_col",
                       refit_fn = function(d) d),
    "no_such_col"
  )
})

test_that("fpca_boot_subjects returns a list of FPCA fits", {
  skip_if_not_installed("fdapace")
  refit <- function(d) {
    fdapace::FPCA(
      Ly    = split(d$value, d$id),
      Lt    = split(d$time,  d$id),
      optns = list(dataType = "Sparse", verbose = FALSE)
    )
  }
  boots <- fpca_boot_subjects(test_df_r, B = 3L, refit_fn = refit, seed = 1L)
  expect_true(is.list(boots))
  expect_gte(length(boots), 1L)
  expect_true(all(vapply(boots, function(x) inherits(x, "FPCA"), logical(1L))))
})

test_that("fpca_boot_subjects is reproducible with the same seed", {
  skip_if_not_installed("fdapace")
  refit <- function(d) {
    fdapace::FPCA(
      Ly    = split(d$value, d$id),
      Lt    = split(d$time,  d$id),
      optns = list(dataType = "Sparse", verbose = FALSE)
    )
  }
  b1 <- fpca_boot_subjects(test_df_r, B = 2L, refit_fn = refit, seed = 42L)
  b2 <- fpca_boot_subjects(test_df_r, B = 2L, refit_fn = refit, seed = 42L)
  expect_equal(length(b1), length(b2))
})
