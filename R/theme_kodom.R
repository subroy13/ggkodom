#### THEMES ####

#' Clean theme for ggkodom linear and heatmap plots
#'
#' Based on [ggplot2::theme_minimal()]: vertical x-axis grid lines mark time
#' positions, horizontal y-axis grid lines are suppressed (they would bisect
#' subject lanes and add clutter). Restrained axis text, wide horizontal
#' color-bar legend. Suitable for [geom_kodom_line()] and
#' [geom_kodom_heatmap()].
#'
#' The vertical grid lines can be suppressed or restyled via the usual ggplot2
#' `theme()` override:
#' ```r
#' + theme(panel.grid.major.x = element_blank())
#' ```
#'
#' @param base_size Base font size (default 12).
#' @param legend_position One of `"top"`, `"bottom"`, `"left"`, `"right"`,
#'   `"none"`.
#' @return A [ggplot2::theme()] object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = time, id = subject_id, colour = value)) +
#'   geom_kodom_line() +
#'   scale_colour_kodom() +
#'   theme_kodom()
#' }
theme_kodom <- function(base_size = 12, legend_position = "top") {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(
        colour    = "grey88",
        linewidth = 0.3,
        linetype  = "dashed"
      ),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = base_size * 0.7),
      axis.text.x = ggplot2::element_text(size = base_size * 0.85),
      legend.position = legend_position,
      strip.text = ggplot2::element_text(face = "bold", size = base_size)
    )
}


#' Theme for circular ggkodom plots
#'
#' Fully minimal Cartesian theme for [geom_kodom_circular()]: no axis text,
#' ticks, or grid lines. Reference rings are drawn by the geom itself as
#' annotation layers, so this theme suppresses all panel grid elements.
#'
#' Unlike the original `coord_polar()`-based approach, [geom_kodom_circular()]
#' performs its coordinate transformation inside the stat and renders in
#' ordinary Cartesian space. This means `panel.grid.major.y` lines would
#' appear as horizontal rules, not concentric circles — so both major axes
#' are suppressed here.
#'
#' @param base_size Base font size (default 12).
#' @return A [ggplot2::theme()] object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = time, id = subject_id, colour = value)) +
#'   geom_kodom_circular() +
#'   scale_colour_kodom() +
#'   coord_fixed() +
#'   theme_kodom_circular()
#' }
theme_kodom_circular <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text        = ggplot2::element_blank(),
      axis.title       = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom",
      plot.title       = ggplot2::element_text(hjust = 0.5)
    )
}

#' Theme for periodic Kodom plots
#'
#' Designed for use with [geom_kodom_periodic()] and
#' [coord_kodom_periodic()]. Suppresses the radial (y) axis — which shows
#' lane numbers that are not meaningful to readers — while keeping the angular
#' (x) axis so that time labels added via [ggplot2::scale_x_continuous()] are
#' visible around the ring.
#'
#' @inheritParams theme_kodom_circular
#' @return A [ggplot2::theme()] object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = visit_month, id = subject_id, colour = hba1c)) +
#'   geom_kodom_periodic(period = 12) +
#'   scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(0, 12)) +
#'   scale_colour_kodom() +
#'   coord_kodom_periodic() +
#'   theme_kodom_periodic()
#' }
theme_kodom_periodic <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.title       = ggplot2::element_blank(),
      axis.text.y      = ggplot2::element_blank(),
      axis.ticks.y     = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom",
      plot.title       = ggplot2::element_text(hjust = 0.5)
    )
}


#' Polar coordinate system for periodic Kodom plots
#'
#' A convenience wrapper around [ggplot2::coord_polar()] with defaults
#' appropriate for [geom_kodom_periodic()]: `theta = "x"` (time maps to
#' angle), `start = pi/2` (x = 0 at 12 o'clock), and `direction = -1`
#' (clockwise). Pass `clockwise = FALSE` for a counter-clockwise layout.
#'
#' @param clockwise Logical. `TRUE` (default) places time clockwise from the
#'   top, matching the convention of most clock and calendar displays.
#' @return A [ggplot2::coord_polar()] coordinate object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = visit_month, id = subject_id, colour = hba1c)) +
#'   geom_kodom_periodic(period = 12) +
#'   scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(0, 12)) +
#'   scale_colour_kodom() +
#'   coord_kodom_periodic() +
#'   theme_kodom_periodic()
#' }
coord_kodom_periodic <- function(clockwise = TRUE) {
  ggplot2::coord_polar(
    theta     = "x",
    start     = pi / 2,
    direction = if (clockwise) -1L else 1L
  )
}
