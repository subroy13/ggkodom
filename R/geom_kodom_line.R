#### STAT AND GEOM FOR geom_kodom_line ####
# Shared internals (.kodom_assign_lanes, .kodom_build_segments, StatKodomBase)
# live in aaa_kodom_internals.R, which loads first alphabetically.


#' Stat for geom_kodom_line
#'
#' Computes integer lane positions (`y`) from the `id` aesthetic and applies
#' optional sorting and subject sampling. Called automatically by
#' [geom_kodom_line()]; rarely needed directly.
#'
#' @keywords internal
#' @format A ggproto object.
StatKodomLine <- ggplot2::ggproto("StatKodomLine", StatKodomBase,
  compute_panel = function(data, scales, sort_by = "none", n_max = Inf) {
    .kodom_assign_lanes(data, sort_by = sort_by, n_max = n_max)
  }
)


#' Geom for geom_kodom_line
#'
#' Extends [ggplot2::GeomPath] with kodom defaults. Drops the `id` column in
#' `setup_data()` so ggplot2 does not warn about an unknown aesthetic at draw
#' time. Draws interpolated colour-gradient segments via [ggplot2::GeomSegment]
#' and optional observation-point markers via [ggplot2::GeomPoint].
#'
#' `size` and `linewidth` are intentionally routed to separate graphical
#' elements: `size` → points only, `linewidth` → path segments only.
#'
#' @keywords internal
#' @format A ggproto object.
GeomKodomLine <- ggplot2::ggproto("GeomKodomLine", ggplot2::GeomPath,

  # GeomPath defaults extended with point aesthetics so draw_panel can pass
  # the same data frame to both GeomSegment and GeomPoint without munging.
  # linewidth is kept thin so the path reads as a connector; size = 2.5 makes
  # individual observations clearly visible even in narrow lanes.
  default_aes = modifyList(
    ggplot2::GeomPath$default_aes,
    list(
      linewidth = 0.5,
      alpha     = 0.8,
      size      = 2.5,
      shape     = 19,
      fill      = NA,
      stroke    = 0.5
    )
  ),
  rename_size = FALSE,
  setup_data = function(data, params) {
    data$id <- NULL
    data
  },
  draw_panel = function(data, panel_params, coord, show_points = TRUE,
                        lineend = "butt", na.rm = FALSE) {
    # PATH — linewidth drives segment stroke width.
    # .kodom_build_segments() copies only linewidth/linetype/colour/alpha into
    # seg_data, so size/shape/fill/stroke never reach GeomSegment.
    seg_data <- .kodom_build_segments(data)
    line_grob <- if (is.null(seg_data) || nrow(seg_data) == 0L) {
      grid::nullGrob()
    } else {
      ggplot2::GeomSegment$draw_panel(seg_data, panel_params, coord,
        lineend = lineend, na.rm = na.rm
      )
    }

    # Suppress points when show_points = FALSE, or when the user signals
    # "no points" via shape = NA/NULL or size = NA/NULL/0.
    draw_pts <- show_points &&
      !all(is.na(data$shape)) &&
      !all(is.na(data$size) | data$size <= 0)

    if (!draw_pts) {
      return(line_grob)
    }

    # POINTS — size/shape/fill/stroke drive the markers.
    # Drop linewidth: GeomPoint uses stroke for its border width, not linewidth.
    point_data <- data
    point_data$linewidth <- NULL
    point_grob <- ggplot2::GeomPoint$draw_panel(point_data, panel_params,
      coord,
      na.rm = na.rm
    )
    grid::grobTree(line_grob, point_grob)
  },
  draw_key = function(data, params, size) {
    line_data <- data
    line_data$size <- NULL
    line_grob <- ggplot2::draw_key_path(line_data, params, size)

    show_points <- if (is.null(params$show_points)) TRUE else params$show_points
    draw_pts <- show_points &&
      !all(is.na(data$shape)) &&
      !all(is.na(data$size) | data$size <= 0)

    if (!draw_pts) {
      return(line_grob)
    }

    point_data <- data
    point_data$linewidth <- NULL
    point_grob <- ggplot2::draw_key_point(point_data, params, size)
    grid::grobTree(line_grob, point_grob)
  }
)


#' Swimlane line plot for longitudinal trajectories
#'
#' Draws one horizontal colored path per subject. Time maps to `x`, subject
#' identity maps to `id` (which the stat converts to integer lane positions on
#' the `y` axis), and a measured value maps to `colour`. The colour transitions
#' smoothly between consecutive observations via interpolated sub-segments.
#'
#' Lane ordering is controlled by `sort_by`. With many subjects, consider
#' suppressing y-axis labels via `theme(axis.text.y = element_blank())`.
#'
#' @section Aesthetics:
#' Required aesthetics are shown in **bold**. `size` and `linewidth` are
#' intentionally independent: each controls a different graphical element.
#'
#' - **`x`** — time variable (numeric or Date)
#' - **`id`** — subject identifier; determines lane position on the y axis
#' - `colour` — color of both the connecting path and the point border
#' - `fill` — point interior color for filled shapes (21–25); no effect on path
#' - `size` — **point size only**; set to `NA` or `0` to suppress points
#' - `linewidth` — **path width only**; does not affect observation points
#' - `alpha` — transparency applied to both path and points
#' - `shape` — point shape (default 19). Set to `NA` to suppress points.
#' - `stroke`, `linetype` — point border width / path line type
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data A data frame. If `NULL`, inherits from the plot.
#' @param stat The stat to use. Defaults to [StatKodomLine].
#' @param position Position adjustment, almost always `"identity"`.
#' @param sort_by Lane ordering. One of `"none"` (default), `"mean"`,
#'   `"mean_asc"`, `"first"`, `"last"` — all refer to the `colour` variable.
#' @param n_max Maximum number of subjects to display. A random sample is taken
#'   when exceeded. Default `Inf` (all subjects).
#' @param show_points If `TRUE` (default), draws a point at every observation.
#'   Set to `FALSE`, or map `shape = NA` / `size = 0`, to suppress points.
#' @param na.rm If `TRUE`, silently remove rows with missing required aesthetics.
#' @param show.legend Logical. Should this layer appear in the legend?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = time, id = subject_id, colour = hba1c)) +
#'   geom_kodom_line(sort_by = "mean", n_max = 50) +
#'   scale_colour_kodom() +
#'   theme_kodom()
#' }
geom_kodom_line <- function(mapping = NULL,
                            data = NULL,
                            stat = StatKodomLine,
                            position = "identity",
                            ...,
                            sort_by = "none",
                            n_max = Inf,
                            show_points = TRUE,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomKodomLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sort_by     = sort_by,
      n_max       = n_max,
      show_points = show_points,
      na.rm       = na.rm,
      ...
    )
  )
}
