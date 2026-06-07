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
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5)
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


#' Radial scale for periodic Kodom plots
#'
#' In `coord_polar(theta = "x")`, ggplot2 sets the center of the plot to the
#' *minimum data y value*, not to zero. Because [geom_kodom_periodic()] stores
#' the hollow-center gap as an offset above zero (`inner_fraction * n_lanes`),
#' the gap is invisible without this scale: the auto-range simply absorbs it.
#'
#' This function pins `y = 0` at the center by setting `limits = c(0, NA)`,
#' so the offset computed by `inner_fraction` becomes a visible donut hole.
#' It should be added to every plot that uses [geom_kodom_periodic()].
#'
#' @param expand Passed to [ggplot2::scale_y_continuous()]. Default adds 5%
#'   padding beyond the outermost ring and no padding at the center.
#' @return A [ggplot2::scale_y_continuous()] object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = visit_month, id = subject_id, colour = hba1c)) +
#'   geom_kodom_periodic(period = 12, inner_fraction = 0.3) +
#'   scale_x_continuous(breaks = 0:11, labels = month.abb) +
#'   scale_y_kodom_periodic() +
#'   scale_colour_kodom() +
#'   coord_kodom_periodic() +
#'   theme_kodom_periodic()
#' }
scale_y_kodom_periodic <- function(
    expand = ggplot2::expansion(mult = c(0, 0.05))) {
  ggplot2::scale_y_continuous(limits = c(0, NA), expand = expand)
}

#' X-scale for periodic longitudinal plots
#'
#' This is a convenience wrapper around [ggplot2::scale_x_continuous()] that
#' enforces `limits = c(0, period)` and uses `oob = scales::oob_keep`.
#'
#' **Why is this necessary?** To make exactly one cycle span exactly one
#' 360-degree rotation in `coord_polar`, the scale limits *must* be set to the
#' period length (e.g., `c(0, 12)`). However, standard ggplot2 `scale_x_continuous`
#' will drop any data outside those limits. By setting `oob = scales::oob_keep`,
#' we instruct ggplot2 to keep the data that exceeds the period. `coord_polar`
#' then natively wraps those out-of-bounds values around the circle, creating
#' beautiful, continuous Archimedean spirals!
#'
#' @param period The length of one complete cycle (e.g. `12` for months).
#'   Must match the `period` argument passed to `geom_kodom_periodic()`.
#'   Default `12`.
#' @param breaks Passed to `scale_x_continuous()`. Default provides integer
#'   breaks for the period.
#' @param ... Additional arguments passed to [ggplot2::scale_x_continuous()],
#'   such as `labels`.
#' @return A [ggplot2::scale_x_continuous()] object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = visit_month, id = subject_id, colour = hba1c)) +
#'   geom_kodom_periodic(period = 12) +
#'   scale_x_kodom_periodic(period = 12, labels = month.abb) +
#'   scale_y_kodom_periodic() +
#'   coord_kodom_periodic() +
#'   theme_kodom_periodic()
#' }
scale_x_kodom_periodic <- function(period = 12, breaks = 1:period, ...) {
  ggplot2::scale_x_continuous(
    breaks = breaks,
    limits = c(0, period),
    oob = scales::oob_keep,
    ...
  )
}
