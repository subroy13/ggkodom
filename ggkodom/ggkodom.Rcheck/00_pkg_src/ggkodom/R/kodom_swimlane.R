#### SWIMLANE PLOT ####

#' Swimlane trajectory plot
#'
#' One row per subject; segments and points colored by measurement value.
#' This single function covers three layouts via the \code{geom} and
#' \code{coord} arguments:
#' \describe{
#'   \item{\code{geom = "path"}, \code{coord = "linear"}}{(default) — the
#'     classic swimlane: lines + points along time.}
#'   \item{\code{geom = "tile"}, \code{coord = "linear"}}{rectangular
#'     heatmap: time is binned and each subject-bin cell is filled by an
#'     aggregate.  Useful for large \eqn{N}.}
#'   \item{\code{geom = "path"}, \code{coord = "polar"}}{the circular
#'     ("Kodom") variant: each subject is a radial spoke from the center,
#'     time increases outward.  Resembles a Kadam flower
#'     (\emph{Neolamarckia cadamba}).}
#' }
#'
#' \code{geom = "tile"} with \code{coord = "polar"} (a sunburst) is not
#' supported.
#'
#' @param data Data frame in long format (one row per measurement).
#' @param id Column name (string) for subject identifier.
#' @param time Column name (string) for time variable (numeric).
#' @param value Column name (string) for measurement value (numeric).
#' @param geom \code{"path"} (default) or \code{"tile"}.
#' @param coord \code{"linear"} (default) or \code{"polar"} — only valid
#'   with \code{geom = "path"}.
#' @param n_sample Max number of subjects to display.  \code{NULL} = all.
#'   Default 50; consider raising for \code{geom = "tile"}.
#' @param sort_by Order subjects by their \code{"first"}, \code{"last"},
#'   \code{"mean"}, or \code{"median"} value, OR a named numeric vector
#'   (e.g. from \code{\link{kodom_sort_scores}}).  \code{NULL} (default)
#'   picks \code{"first"} for path and \code{"mean"} for tile.
#' @param colors Gradient colors; default \code{\link{kodom_colors}()}.
#' @param color_breaks Numeric breakpoints anchoring the gradient.
#'   E.g.\ \code{c(5.7, 7, 10)} places the three default colors at those
#'   values.  \code{NULL} spaces them evenly.
#' @param discretize \code{FALSE} (default) = smooth gradient;
#'   \code{TRUE} = solid step bands separated at \code{color_breaks}
#'   (see \code{\link{kodom_step_scale}}).  Useful when fitted/smoothed
#'   values wash out into a single hue.
#' @param outcome Column name (string) for binary outcome.
#'   Positive subjects get \code{outcome_label} on the y-axis.  Ignored
#'   when \code{coord = "polar"} (no y-axis text).
#' @param outcome_label Marker for positive outcomes (default \code{"*"}).
#' @param point_size Point size.  \code{NULL} (default) picks 3 for
#'   linear paths, 2 for polar paths, and 0 (no points) for tiles.
#' @param line_width Line width (default 0.5).  Ignored for tiles.
#' @param alpha Line transparency (default 0.7).  Ignored for tiles.
#' @param facet_rows,facet_cols Column names for row/column faceting
#'   (\code{NULL} = none).
#' @param time_bins Number of equal-width time bins (default 10).  Used
#'   only when \code{geom = "tile"}.  Ignored if \code{time_breaks} is
#'   supplied.
#' @param time_breaks Numeric vector of custom bin boundaries.  Used only
#'   when \code{geom = "tile"}.  Overrides \code{time_bins}.
#' @param agg_fun Aggregation within tile bins: \code{"mean"} (default),
#'   \code{"median"}, \code{"first"}, \code{"last"}, \code{"min"}, or
#'   \code{"max"}.  Used only when \code{geom = "tile"}.
#' @param arc_degrees Angular extent of the polar layout, in degrees
#'   (default 360 = full circle).  Use e.g.\ \code{arc_degrees = 270} for a
#'   fan layout that preserves ordering without a wrap seam.  When less
#'   than 360, this overrides \code{gap_fraction}.  Polar only.
#' @param gap_fraction Fraction of the circle left empty as a visual gap
#'   at the ordering seam (default 0.15).  Polar only; overridden when
#'   \code{arc_degrees < 360}.
#' @param inner_fraction Fraction of the time range used as a hollow
#'   inner buffer so short-follow-up subjects remain visible (default 0.3).
#'   Polar only.
#' @param direction \code{1} = clockwise (default), \code{-1} = counter-
#'   clockwise.  Polar only.
#' @param show_rings Draw concentric reference circles for time?
#'   (default \code{FALSE}).  Polar only.
#' @param seed Random seed for subject sampling.
#' @return A \code{ggplot} object that can be further customized with \code{+}.
#' @export
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   id    = rep(1:20, each = 5),
#'   time  = rep(seq(0, 120, 30), 20),
#'   value = rnorm(100, 7, 1.5)
#' )
#'
#' ## classic swimlane
#' kodom_swimlane(df, color_breaks = c(5, 7, 10))
#'
#' ## heatmap variant
#' kodom_swimlane(df, geom = "tile", time_bins = 7)
#'
#' ## circular variant
#' kodom_swimlane(df, coord = "polar")
kodom_swimlane <- function(data, id = "id", time = "time", value = "value",
                           geom  = c("path", "tile"),
                           coord = c("linear", "polar"),
                           n_sample = 50, sort_by = NULL,
                           colors = kodom_colors(), color_breaks = NULL,
                           discretize = FALSE,
                           outcome = NULL, outcome_label = "*",
                           point_size = NULL, line_width = 0.5, alpha = 0.7,
                           facet_rows = NULL, facet_cols = NULL,
                           time_bins = 10, time_breaks = NULL,
                           agg_fun = c("mean", "median", "first", "last", "min", "max"),
                           arc_degrees = 360, gap_fraction = 0.15,
                           inner_fraction = 0.3,
                           direction = 1, show_rings = FALSE,
                           seed = 123) {

  geom    <- match.arg(geom)
  coord   <- match.arg(coord)
  agg_fun <- match.arg(agg_fun)
  if (coord == "polar" && geom == "tile") {
    stop("coord = 'polar' is supported only with geom = 'path'.", call. = FALSE)
  }
  if (arc_degrees <= 0 || arc_degrees > 360) {
    stop("arc_degrees must be in (0, 360].", call. = FALSE)
  }
  ## arc_degrees < 360 takes over the gap so the missing arc IS the empty wedge
  if (arc_degrees < 360) {
    gap_fraction <- (360 - arc_degrees) / 360
  }

  ## sensible defaults that depend on layout
  if (is.null(sort_by))    sort_by    <- if (geom == "tile") "mean" else "first"
  if (is.null(point_size)) point_size <- if (geom == "tile") 0
                                         else if (coord == "polar") 2
                                         else 3

  prep <- prep_trajectory_data(
    data, id, time, value,
    n_sample = n_sample, sort_by = sort_by,
    outcome = outcome, outcome_label = outcome_label,
    seed = seed
  )

  df   <- prep$data
  cols <- prep$cols

  ## dispatch on geom × coord
  if (geom == "tile") {
    p <- draw_swimlane_tile(df, cols, prep,
                            time_bins, time_breaks, agg_fun,
                            facet_rows, facet_cols)
    p <- p + kodom_value_scale("fill", colors, color_breaks,
                               name = cols$value, discretize = discretize)
  } else if (coord == "polar") {
    p <- draw_swimlane_polar(df, cols, prep,
                             line_width, alpha, point_size,
                             gap_fraction, inner_fraction, direction)
    p <- p + kodom_value_scale("color", colors, color_breaks,
                               name = cols$value, discretize = discretize)
  } else {
    p <- draw_swimlane_path(df, cols, line_width, alpha, point_size)
    p <- p + kodom_value_scale("color", colors, color_breaks,
                               name = cols$value, discretize = discretize)
  }

  ## y-axis outcome markers (linear layouts only)
  if (!is.null(prep$y_labels) && coord != "polar") {
    p <- p + ggplot2::scale_y_discrete(labels = prep$y_labels)
  }

  ## faceting
  facet <- build_facet(facet_rows, facet_cols)
  if (!is.null(facet)) p <- p + facet

  ## theme + legend polish
  if (coord == "polar") {
    p <- p + theme_kodom_circular()
    if (!show_rings) {
      p <- p + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    }
    p + ggplot2::guides(
      color = ggplot2::guide_colorbar(barwidth = 15, barheight = 0.75)
    )
  } else {
    p <- p + theme_kodom() + ggplot2::labs(x = cols$time, y = "")
    if (geom == "path") {
      p <- p + ggplot2::guides(
        color = ggplot2::guide_colorbar(barwidth = 15, barheight = 0.75)
      )
    }
    p
  }
}


#### INTERNAL DRAWERS ####

#' @keywords internal
draw_swimlane_path <- function(df, cols, line_width, alpha, point_size) {
  p <- ggplot2::ggplot(df, ggplot2::aes(
      x     = .data[[cols$time]],
      y     = .data[[cols$id]],
      group = .data[[cols$id]]
    )) +
    ggplot2::geom_path(
      ggplot2::aes(color = .data[[cols$value]]),
      linewidth = line_width, alpha = alpha
    )
  if (point_size > 0) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = .data[[cols$value]]),
      size = point_size
    )
  }
  p
}


#' @keywords internal
draw_swimlane_tile <- function(df, cols, prep,
                               time_bins, time_breaks, agg_fun,
                               facet_rows, facet_cols) {
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

  agg_fn <- switch(agg_fun,
    mean   = mean,
    median = stats::median,
    first  = function(x) x[1L],
    last   = function(x) x[length(x)],
    min    = min,
    max    = max
  )

  agg_df <- stats::aggregate(
    df[[cols$value]],
    by  = list(.id = df[[cols$id]], .time_bin = df$.time_bin),
    FUN = agg_fn
  )
  names(agg_df) <- c(cols$id, ".time_bin", cols$value)
  agg_df[[cols$id]] <- factor(agg_df[[cols$id]], levels = prep$id_order)

  ## carry faceting columns through
  if (!is.null(facet_rows) || !is.null(facet_cols)) {
    facet_vars <- c(facet_rows, facet_cols)
    facet_vars <- facet_vars[!is.null(facet_vars)]
    facet_map  <- df[!duplicated(df[[cols$id]]),
                     c(cols$id, facet_vars), drop = FALSE]
    agg_df     <- merge(agg_df, facet_map, by = cols$id, all.x = TRUE)
  }

  ggplot2::ggplot(agg_df, ggplot2::aes(
      x    = .data$.time_bin,
      y    = .data[[cols$id]],
      fill = .data[[cols$value]]
    )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.25)
}


#' @keywords internal
draw_swimlane_polar <- function(df, cols, prep,
                                line_width, alpha, point_size,
                                gap_fraction, inner_fraction, direction) {
  ## angular position: factor level → numeric
  df$.id_pos <- as.numeric(df[[cols$id]])

  ## leave a gap at the ordering seam
  n_ids   <- length(prep$id_order)
  gap_n   <- max(1L, ceiling(n_ids * gap_fraction))
  ang_max <- n_ids + gap_n

  ## radial axis: time + inner buffer (hollow center)
  time_vals    <- df[[cols$time]]
  time_range   <- range(time_vals, na.rm = TRUE)
  inner_buffer <- diff(time_range) * inner_fraction
  df$.radius   <- time_vals + inner_buffer

  p <- ggplot2::ggplot(df, ggplot2::aes(
      x     = .data$.id_pos,
      y     = .data$.radius,
      group = .data[[cols$id]]
    )) +
    ggplot2::geom_path(
      ggplot2::aes(color = .data[[cols$value]]),
      linewidth = line_width, alpha = alpha
    )
  if (point_size > 0) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = .data[[cols$value]]),
      size = point_size
    )
  }

  p +
    ggplot2::scale_x_continuous(
      limits = c(0.5, ang_max + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(df$.radius, na.rm = TRUE) * 1.05)
    ) +
    ggplot2::coord_polar(theta = "x", start = 0, direction = direction)
}
