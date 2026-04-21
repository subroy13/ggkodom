#### HEATMAP PLOT ####

#' Heatmap trajectory plot
#'
#' Subjects-by-time-bin grid where each cell is filled by the
#' aggregated measurement value.  Good for large \eqn{N} where
#' individual swimlane lines would overlap.
#'
#' @inheritParams kodom_swimlane
#' @param n_sample Max subjects (default 100 — heatmaps tolerate
#'   more rows than swimlanes).
#' @param sort_by Default \code{"mean"} (sensible for binned data).
#' @param time_bins Number of equal-width time bins (default 10).
#'   Ignored if \code{time_breaks} is given.
#' @param time_breaks Numeric vector of custom bin boundaries.
#'   Overrides \code{time_bins}.
#' @param agg_fun Aggregation within bins: \code{"mean"} (default),
#'   \code{"median"}, \code{"first"}, \code{"last"}, \code{"min"},
#'   or \code{"max"}.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   id    = rep(1:30, each = 8),
#'   time  = rep(seq(0, 210, 30), 30),
#'   value = rnorm(240, 7, 1.5)
#' )
#' kodom_heatmap(df, n_sample = 30, time_bins = 7)
kodom_heatmap <- function(data, id = "id", time = "time", value = "value",
                          n_sample = 100, sort_by = "mean",
                          time_bins = 10, time_breaks = NULL,
                          agg_fun = "mean",
                          colors = kodom_colors(), color_breaks = NULL,
                          outcome = NULL, outcome_label = "*",
                          facet_rows = NULL, facet_cols = NULL,
                          seed = 123) {

  agg_fun <- match.arg(agg_fun,
    c("mean", "median", "first", "last", "min", "max")
  )

  prep <- prep_trajectory_data(
    data, id, time, value,
    n_sample = n_sample, sort_by = sort_by,
    outcome = outcome, outcome_label = outcome_label,
    seed = seed
  )

  df   <- prep$data
  cols <- prep$cols

  ## time bins
  time_vals <- df[[cols$time]]
  if (!is.null(time_breaks)) {
    breaks <- time_breaks
  } else {
    tr <- range(time_vals, na.rm = TRUE)
    breaks <- seq(tr[1], tr[2], length.out = time_bins + 1L)
  }
  mids       <- (breaks[-length(breaks)] + breaks[-1]) / 2
  bin_labels <- round(mids)

  df$.time_bin <- cut(time_vals, breaks = breaks,
                      include.lowest = TRUE, labels = bin_labels)

  ## aggregation function
  agg_fn <- switch(agg_fun,
    mean   = mean,
    median = stats::median,
    first  = function(x) x[1L],
    last   = function(x) x[length(x)],
    min    = min,
    max    = max
  )

  ## aggregate (base R, no dplyr dependency)
  agg_df <- stats::aggregate(
    df[[cols$value]],
    by = list(.id = df[[cols$id]], .time_bin = df$.time_bin),
    FUN = agg_fn
  )
  names(agg_df) <- c(cols$id, ".time_bin", cols$value)
  agg_df[[cols$id]] <- factor(agg_df[[cols$id]], levels = prep$id_order)

  ## carry faceting columns through (take first value per subject)
  if (!is.null(facet_rows) || !is.null(facet_cols)) {
    facet_vars <- c(facet_rows, facet_cols)
    facet_vars <- facet_vars[!is.null(facet_vars)]
    facet_map  <- df[!duplicated(df[[cols$id]]), c(cols$id, facet_vars), drop = FALSE]
    agg_df     <- merge(agg_df, facet_map, by = cols$id, all.x = TRUE)
  }

  ## plot
  p <- ggplot2::ggplot(agg_df, ggplot2::aes(
    x    = .data$.time_bin,
    y    = .data[[cols$id]],
    fill = .data[[cols$value]]
  )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.25)

  ## fill gradient
  p <- p + kodom_gradient_scale("fill", colors, color_breaks, name = cols$value)

  ## y-axis labels
  if (!is.null(prep$y_labels)) {
    p <- p + ggplot2::scale_y_discrete(labels = prep$y_labels)
  }

  ## faceting
  facet <- build_facet(facet_rows, facet_cols)
  if (!is.null(facet)) p <- p + facet

  p + theme_kodom() +
    ggplot2::labs(x = cols$time, y = "")
}
