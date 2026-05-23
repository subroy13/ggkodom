## Shared fixtures — loaded automatically by testthat before each test file.

# ── Basic longitudinal data ──────────────────────────────────────────────────

set.seed(42)
test_df <- data.frame(
  id    = rep(seq_len(15L), each = 5L),
  time  = as.vector(replicate(15L, sort(round(runif(5L, 0, 120))))),
  value = rnorm(75L, 7, 1.5)
)

# Richer dataset with no tied times — used for model fitting
set.seed(11)
test_df_r <- do.call(rbind, lapply(seq_len(20L), function(i) {
  t  <- sort(runif(6L, 0, 120))
  b0 <- rnorm(1, 0, 1.5)
  b1 <- rnorm(1, 0.01, 0.005)
  data.frame(id = i, time = t, value = 7 + b0 + b1 * t + rnorm(6L, 0, 0.5))
}))

# ── Mock FPCA objects (no optional packages required) ────────────────────────
# 10 subjects, 5 time points, 2 PCs — used for testing FPCA-layer code.

set.seed(99)
.n  <- 10L
.T  <- 5L
.grid <- c(0, 30, 60, 90, 120)
.mu   <- c(7.0, 7.1, 7.2, 7.1, 7.0)

# Normalized eigenfunctions: PC1 = symmetric bump, PC2 = monotone decrease
.phi_raw <- matrix(
  c(0.0, 0.5, 1.0, 0.5, 0.0,   # PC1
    1.0, 0.5, 0.0, -0.5, -1.0), # PC2
  nrow = .T, ncol = 2L
)
.phi <- apply(.phi_raw, 2L, function(x) x / sqrt(sum(x^2)))

.xi   <- matrix(rnorm(.n * 2L, sd = rep(c(1.4, 0.9), each = .n)),
                nrow = .n, ncol = 2L)
.ids  <- paste0("s", seq_len(.n))

# fdapace::FPCA-style object
mock_fpca <- structure(
  list(
    mu       = .mu,
    phi      = .phi,
    lambda   = c(2.0, 0.8),
    workGrid = .grid,
    xiEst    = .xi,
    inputData = list(Ly = stats::setNames(vector("list", .n), .ids))
  ),
  class = "FPCA"
)

# refund::fpca.sc-style object (same underlying data)
mock_fpca_sc <- structure(
  list(
    mu         = .mu,
    efunctions = .phi,
    evalues    = c(2.0, 0.8),
    argvals    = .grid,
    scores     = `rownames<-`(.xi, .ids)
  ),
  class = "fpca.sc"
)

rm(.n, .T, .grid, .mu, .phi_raw, .phi, .xi, .ids)
