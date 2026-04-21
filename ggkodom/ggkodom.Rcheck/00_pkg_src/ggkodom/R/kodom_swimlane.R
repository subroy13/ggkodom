#### SWIMLANE PLOT ####

#' Swimlane trajectory plot
#'
#' Each subject occupies a horizontal lane.
#' Segments and points between consecutive measurements are colored by
#' the measurement value, revealing individual trajectories across time.
#'
#' @param data Data frame in long format (one row per measurement).
#' @param id Column name (string) for subject identifier.
#' @param time Column name (string) for time variable (numeric).
#' @param value Column name (string) for measurement value (numeric).
#' @param n_sample Max number of subjects to display. \code{NULL} = all.
#' @param sort_by Order subjects by their \code{"first"} (default),
#'   \code{"last"}, \code{"mean"}, or \code{"median"} value.
#' @param colors Gradient colors; default \code{\link{kodom_colors}()}.
#' @param color_breaks Numeric breakpoints anchoring the gradient.
#'   E.g.\ \code{c(5.7, 7, 10)} places the three default colors at
#'   those values.  \code{NULL} spaces them evenly.
#' @param outcome Column name (string) for binary outcome.
#'   Positive subjects get \code{outcome_label} on the y-axis.
#' @param outcome_label Marker for positive outcomes (default \code{"*"}).
#' @param point_size Point size (default 3).
#' @param line_width Line width (default 0.8).
#' @param alpha Line transparency (default 0.7).
#' @param facet_rows Column name for row faceting (\code{NULL} = none).
#' @param facet_cols Column name for column faceting (\code{NULL} = none).
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
#' kodom_swimlane(df)
#'
#' # With color anchored at clinical thresholds
#' kodom_swimlane(df, color_breaks = c(5, 7, 10))
kodom_swimlane <- function(data, id = "id", time = "time", value = "value",
                           n_sample = 50, sort_by = "first",
                           colors = kodom_colors(), color_breaks = NULL,
                           outcome = NULL, outcome_label = "*",
                           point_size = 3, line_width = 0.8, alpha = 0.7,
                           facet_rows = NULL, facet_cols = NULL,
                           seed = 123) {

  prep <- prep_trajectory_data(
    data, id, time, value,
    n_sample = n_sample, sort_by = sort_by,
    outcome = outcome, outcome_label = outcome_label,
    seed = seed
  )

  df   <- prep$data
  cols <- prep$cols

  ## base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(
    x     = .data[[cols$time]],
    y     = .data[[cols$id]],
    group = .data[[cols$id]]
  )) +
    ggplot2::geom_path(
      ggplot2::aes(color = .data[[cols$value]]),
      linewidth = line_width, alpha = alpha
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data[[cols$value]]),
      size = point_size
    )

  ## gradient
  p <- p + kodom_gradient_scale("color", colors, color_breaks, name = cols$value)

  ## y-axis outcome markers
  if (!is.null(prep$y_labels)) {
    p <- p + ggplot2::scale_y_discrete(labels = prep$y_labels)
  }

  ## faceting
  facet <- build_facet(facet_rows, facet_cols)
  if (!is.null(facet)) p <- p + facet

  ## polish
  p + theme_kodom() +
    ggplot2::labs(x = cols$time, y = "") +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(barwidth = 15, barheight = 0.75)
    )
}
