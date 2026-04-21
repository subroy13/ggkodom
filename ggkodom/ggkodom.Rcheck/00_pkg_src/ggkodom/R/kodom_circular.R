#### CIRCULAR (KODOM) PLOT ####

#' Circular trajectory plot — the Kodom
#'
#' Polar variant of the swimlane: each subject is a radial spoke
#' emanating from the center.
#' Time increases outward (radius), color encodes measurement value.
#' The result resembles a Kadam flower (\emph{Neolamarckia cadamba}).
#'
#' Subjects are arranged by \code{sort_by} going clockwise.
#' A configurable gap (\code{gap_fraction}) separates the lowest- and
#' highest-ordered subjects so the angular wrap is visible.
#'
#' @inheritParams kodom_swimlane
#' @param gap_fraction Fraction of the circle left empty as a visual
#'   gap at the ordering seam (default 0.15).
#' @param inner_fraction Fraction of the time range used as a hollow
#'   inner buffer so short-follow-up subjects remain visible (default 0.3).
#' @param direction \code{1} = clockwise (default), \code{-1} = counter-clockwise.
#' @param show_rings Draw concentric reference circles for time? (default TRUE).
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' set.seed(42)
#' n_subj <- 40
#' df <- data.frame(
#'   id    = rep(seq_len(n_subj), each = 5),
#'   time  = rep(seq(0, 120, 30), n_subj),
#'   value = rnorm(n_subj * 5, 7, 1.5)
#' )
#' kodom_circular(df, n_sample = 40)
kodom_circular <- function(data, id = "id", time = "time", value = "value",
                           n_sample = 50, sort_by = "first",
                           colors = kodom_colors(), color_breaks = NULL,
                           outcome = NULL, outcome_label = "*",
                           point_size = 2, line_width = 0.8, alpha = 0.7,
                           gap_fraction = 0.15, inner_fraction = 0.3,
                           direction = 1, show_rings = TRUE,
                           seed = 123) {

  prep <- prep_trajectory_data(
    data, id, time, value,
    n_sample = n_sample, sort_by = sort_by,
    outcome = outcome, outcome_label = outcome_label,
    seed = seed
  )

  df   <- prep$data
  cols <- prep$cols

  ## angular position: map factor level → numeric
  df$.id_pos <- as.numeric(df[[cols$id]])

  ## gap: extend angular range so part of the circle is empty

  n_ids   <- length(prep$id_order)
  gap_n   <- max(1L, ceiling(n_ids * gap_fraction))
  ang_max <- n_ids + gap_n

  ## radial axis: time + inner buffer (hollow center)
  time_vals    <- df[[cols$time]]
  time_range   <- range(time_vals, na.rm = TRUE)
  inner_buffer <- diff(time_range) * inner_fraction
  df$.radius   <- time_vals + inner_buffer

  ## build plot
  p <- ggplot2::ggplot(df, ggplot2::aes(
    x     = .data$.id_pos,
    y     = .data$.radius,
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

  ## scales
  p <- p +
    kodom_gradient_scale("color", colors, color_breaks, name = cols$value) +
    ggplot2::scale_x_continuous(
      limits = c(0.5, ang_max + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(df$.radius, na.rm = TRUE) * 1.05)
    )

  ## polar transform — gap sits at the top (12 o'clock)
  p <- p + ggplot2::coord_polar(theta = "x", start = 0, direction = direction)

  ## theme
  p <- p + theme_kodom_circular()

  if (!show_rings) {
    p <- p + ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank()
    )
  }

  p + ggplot2::guides(
    color = ggplot2::guide_colorbar(barwidth = 15, barheight = 0.75)
  )
}
