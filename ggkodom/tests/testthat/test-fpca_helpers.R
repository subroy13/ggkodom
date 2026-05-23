## Tests for the backend-agnostic FPCA helpers (fpca_helpers.R).
## Internal functions accessed via ggkodom:::

test_that("fpca_extract on fdapace FPCA returns all expected fields", {
  ext <- ggkodom:::fpca_extract(mock_fpca)
  expect_named(ext,
    c("mu", "phi", "lambdas", "args", "scores", "ids",
      "phi_slot", "score_slot", "backend"),
    ignore.order = TRUE
  )
  expect_equal(ext$backend,     "fdapace")
  expect_equal(ext$phi_slot,    "phi")
  expect_equal(ext$score_slot,  "xiEst")
})

test_that("fpca_extract on fdapace FPCA returns correct dimensions", {
  ext <- ggkodom:::fpca_extract(mock_fpca)
  expect_equal(nrow(ext$phi),    5L)   # T = 5 time points
  expect_equal(ncol(ext$phi),    2L)   # K = 2 PCs
  expect_equal(nrow(ext$scores), 10L)  # n = 10 subjects
  expect_equal(ncol(ext$scores), 2L)
  expect_length(ext$lambdas,     2L)
  expect_length(ext$ids,         10L)
})

test_that("fpca_extract on fdapace FPCA returns correct subject ids", {
  ext <- ggkodom:::fpca_extract(mock_fpca)
  expect_equal(ext$ids, paste0("s", 1:10))
})

test_that("fpca_extract on refund fpca.sc returns all expected fields", {
  ext <- ggkodom:::fpca_extract(mock_fpca_sc)
  expect_equal(ext$backend,    "refund")
  expect_equal(ext$phi_slot,   "efunctions")
  expect_equal(ext$score_slot, "scores")
})

test_that("fpca_extract on refund fpca.sc returns correct ids from rownames", {
  ext <- ggkodom:::fpca_extract(mock_fpca_sc)
  expect_equal(ext$ids, paste0("s", 1:10))
})

test_that("fpca_extract errors on unrecognized class", {
  expect_error(ggkodom:::fpca_extract(list()), "Not a recognized FPCA fit")
  expect_error(ggkodom:::fpca_extract(structure(list(), class = "lm")),
               "Not a recognized FPCA fit")
})

test_that("make_pc_label k = 0 returns 'mu(t)'", {
  expect_equal(ggkodom:::make_pc_label(0L, c(0.6, 0.3)), "mu(t)")
})

test_that("make_pc_label formats PVE percentage correctly", {
  expect_equal(ggkodom:::make_pc_label(1L, c(0.6, 0.3), TRUE),  "PC 1 (60.0%)")
  expect_equal(ggkodom:::make_pc_label(2L, c(0.6, 0.3), TRUE),  "PC 2 (30.0%)")
})

test_that("make_pc_label suppresses PVE when show_pve = FALSE", {
  expect_equal(ggkodom:::make_pc_label(1L, c(0.6, 0.3), FALSE), "PC 1")
})

test_that("%||% returns left operand when non-NULL", {
  expect_equal(ggkodom:::`%||%`("a", "b"), "a")
  expect_equal(ggkodom:::`%||%`(0L, 99L), 0L)
})

test_that("%||% returns right operand when left is NULL", {
  expect_equal(ggkodom:::`%||%`(NULL, "fallback"), "fallback")
})

test_that("fpca_scores_matrix wrapper returns the score matrix", {
  s <- ggkodom:::fpca_scores_matrix(mock_fpca)
  expect_true(is.matrix(s))
  expect_equal(dim(s), c(10L, 2L))
})

test_that("fpca_evalues wrapper returns eigenvalues", {
  ev <- ggkodom:::fpca_evalues(mock_fpca)
  expect_equal(ev, c(2.0, 0.8))
})

test_that("fpca_ids wrapper returns character ids", {
  ids <- ggkodom:::fpca_ids(mock_fpca)
  expect_type(ids, "character")
  expect_equal(ids, paste0("s", 1:10))
})

# ── Downstream functions that rely on fpca_extract ───────────────────────────

test_that("kodom_components.FPCA returns a ggplot (faceted)", {
  p <- kodom_components(mock_fpca)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_components.FPCA overlay mode returns a ggplot", {
  p <- kodom_components(mock_fpca, overlay = TRUE, show_mean = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_components.fpca.sc returns a ggplot", {
  p <- kodom_components(mock_fpca_sc)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_perturbation.FPCA returns a ggplot", {
  p <- kodom_perturbation(mock_fpca)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_perturbation K = 1 and sd_mult = 1 works", {
  p <- kodom_perturbation(mock_fpca, K = 1L, sd_mult = 1)
  expect_s3_class(p, "ggplot")
})

test_that("kodom_perturbation.fpca.sc returns a ggplot", {
  p <- kodom_perturbation(mock_fpca_sc)
  expect_s3_class(p, "ggplot")
})
