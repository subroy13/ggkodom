#### TESTS FOR aaa_kodom_internals.R ####

assign_lanes <- ggkodom:::.kodom_assign_lanes
build_segs   <- ggkodom:::.kodom_build_segments
interp_pair  <- ggkodom:::.kodom_interp_pair

# subject means: A=6, B=8.5, C=5.5
lane_df <- data.frame(
  id     = c("A", "A", "B", "B", "C", "C"),
  x      = c(1, 2, 1, 2, 1, 2),
  colour = c(5, 7, 9, 8, 5, 6),
  stringsAsFactors = FALSE
)

# ---- .kodom_assign_lanes: basic correctness ----

test_that("assign_lanes adds a numeric y column", {
  out <- assign_lanes(lane_df)
  expect_true("y" %in% names(out))
  expect_true(is.numeric(out$y))
})

test_that("assign_lanes produces one distinct y value per subject", {
  out <- assign_lanes(lane_df)
  by_id <- split(out$y, out$id)
  for (vals in by_id) expect_length(unique(vals), 1L)
})

test_that("assign_lanes y values are consecutive integers 1:n_subjects", {
  out <- assign_lanes(lane_df)
  expect_setequal(unique(out$y), 1:3)
})

test_that("assign_lanes preserves all input rows", {
  out <- assign_lanes(lane_df)
  expect_equal(nrow(out), nrow(lane_df))
})

# ---- .kodom_assign_lanes: sort_by ----

test_that("sort_by='mean' puts highest-mean subject in lane 1", {
  # B mean=8.5 > A mean=6 > C mean=5.5
  out  <- assign_lanes(lane_df, sort_by = "mean")
  y_B  <- unique(out$y[out$id == "B"])
  y_A  <- unique(out$y[out$id == "A"])
  y_C  <- unique(out$y[out$id == "C"])
  expect_lt(y_B, y_A)
  expect_lt(y_A, y_C)
})

test_that("sort_by='mean_asc' puts lowest-mean subject in lane 1", {
  out <- assign_lanes(lane_df, sort_by = "mean_asc")
  y_C <- unique(out$y[out$id == "C"])
  y_B <- unique(out$y[out$id == "B"])
  expect_lt(y_C, y_B)
})

test_that("sort_by='first' orders by value at earliest x, descending", {
  # at x=1: B=9, A=5, C=5 → B gets lane 1
  out <- assign_lanes(lane_df, sort_by = "first")
  expect_equal(unique(out$y[out$id == "B"]), 1L)
})

test_that("sort_by='last' orders by value at latest x, descending", {
  # at x=2: B=8, A=7, C=6 → B gets lane 1
  out <- assign_lanes(lane_df, sort_by = "last")
  y_B <- unique(out$y[out$id == "B"])
  y_C <- unique(out$y[out$id == "C"])
  expect_lt(y_B, y_C)
})

test_that("sort_by='none' does not error and produces valid lanes", {
  out <- assign_lanes(lane_df, sort_by = "none")
  expect_setequal(unique(out$y), 1:3)
})

test_that("unknown sort_by falls back to first-appearance order", {
  out <- expect_no_error(assign_lanes(lane_df, sort_by = "foobar"))
  expect_setequal(unique(out$y), 1:3)
})

# ---- .kodom_assign_lanes: n_max ----

test_that("n_max=2 keeps at most 2 subjects", {
  out <- assign_lanes(lane_df, n_max = 2L)
  expect_lte(length(unique(out$id)), 2L)
})

test_that("n_max >= n_subjects keeps all subjects", {
  out <- assign_lanes(lane_df, n_max = 10L)
  expect_equal(length(unique(out$id)), 3L)
})

# ---- .kodom_assign_lanes: fill preference ----

test_that("fill is preferred over colour for sorting when fill is mapped", {
  d        <- lane_df
  d$fill   <- c(10, 10, 1, 1, 5, 5)  # A=10, B=1, C=5
  out      <- assign_lanes(d, sort_by = "mean")
  y_A      <- unique(out$y[out$id == "A"])
  y_B      <- unique(out$y[out$id == "B"])
  expect_lt(y_A, y_B)   # A has highest fill mean → lowest lane number
})

test_that("colour is used when fill column is absent", {
  d   <- lane_df[, c("id", "x", "colour")]
  out <- assign_lanes(d, sort_by = "mean")
  expect_setequal(unique(out$y), 1:3)
})

# ---- .kodom_build_segments ----

seg_df <- data.frame(
  group     = c(1L, 1L, 2L, 2L),
  x         = c(0, 1, 0, 2),
  y         = c(0, 0, 1, 1),
  colour    = c("red", "blue", "green", "yellow"),
  alpha     = 0.8,
  linewidth = 0.5,
  linetype  = 1L,
  PANEL     = factor(1),
  stringsAsFactors = FALSE
)

test_that("build_segs returns a data frame", {
  out <- build_segs(seg_df, n_interp = 5L)
  expect_s3_class(out, "data.frame")
})

test_that("build_segs output has required GeomSegment columns", {
  out <- build_segs(seg_df, n_interp = 5L)
  expect_true(all(c("x", "xend", "y", "yend", "colour",
                    "alpha", "linewidth", "linetype") %in% names(out)))
})

test_that("build_segs creates n_interp rows per inter-observation gap", {
  # 2 groups × 1 gap each × 5 sub-segments = 10
  out <- build_segs(seg_df, n_interp = 5L)
  expect_equal(nrow(out), 10L)
})

test_that("build_segs with 3 observations creates 2 * n_interp rows", {
  three_pt <- rbind(
    seg_df[seg_df$group == 1L, ],
    data.frame(group = 1L, x = 2, y = 0,
               colour = "purple", alpha = 0.8,
               linewidth = 0.5, linetype = 1L,
               PANEL = factor(1), stringsAsFactors = FALSE)
  )
  out <- build_segs(three_pt[three_pt$group == 1L, ], n_interp = 3L)
  expect_equal(nrow(out), 6L)
})

test_that("build_segs returns NULL or empty for a single-point group", {
  out <- build_segs(seg_df[1L, ], n_interp = 5L)
  expect_true(is.null(out) || nrow(out) == 0L)
})

test_that("build_segs n_interp parameter controls sub-segment count", {
  out4 <- build_segs(seg_df, n_interp = 4L)
  out8 <- build_segs(seg_df, n_interp = 8L)
  expect_equal(nrow(out4), 8L)   # 2 groups × 4
  expect_equal(nrow(out8), 16L)  # 2 groups × 8
})

# ---- .kodom_interp_pair ----

test_that("interp_pair returns n_interp rows", {
  out <- interp_pair(0, 1, 0, 1, "red", "blue", 1, 1,
                     0.5, 1L, factor(1), 1L, n_interp = 7L)
  expect_equal(nrow(out), 7L)
})

test_that("interp_pair first segment starts at (x0, y0)", {
  out <- interp_pair(2, 5, 3, 7, "red", "blue", 1, 1,
                     0.5, 1L, factor(1), 1L, n_interp = 4L)
  expect_equal(out$x[1L], 2)
  expect_equal(out$y[1L], 3)
})

test_that("interp_pair last segment ends at (x1, y1)", {
  out <- interp_pair(2, 5, 3, 7, "red", "blue", 1, 1,
                     0.5, 1L, factor(1), 1L, n_interp = 4L)
  expect_equal(out$xend[4L], 5)
  expect_equal(out$yend[4L], 7)
})

test_that("interp_pair colour values are valid hex strings", {
  out <- interp_pair(0, 1, 0, 0, "#FF0000", "#0000FF", 1, 1,
                     0.5, 1L, factor(1), 1L, n_interp = 5L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", out$colour)))
})

test_that("interp_pair alpha interpolates between a0 and a1", {
  out <- interp_pair(0, 1, 0, 0, "red", "blue", 0, 1,
                     0.5, 1L, factor(1), 1L, n_interp = 4L)
  expect_gte(out$alpha[1L], 0)
  expect_lte(out$alpha[4L], 1)
  # alpha should be non-decreasing (going from 0 to 1)
  expect_true(all(diff(out$alpha) >= 0))
})
