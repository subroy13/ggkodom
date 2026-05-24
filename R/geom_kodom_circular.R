#### STAT AND GEOM FOR geom_kodom_circular ####
# Shared internals (.kodom_assign_lanes, .kodom_build_segments, StatKodomBase)
# live in aaa_kodom_internals.R, which loads first alphabetically.
# GeomKodomCircular inherits GeomPath directly (NOT GeomKodomLine) because
# 'c' < 'l' alphabetically, so this file loads before geom_kodom_line.R.


#' Stat for geom_kodom_circular
#'
#' Assigns lane positions, maps each lane to an angular position and the
#' time variable to a radial position, then converts to Cartesian
#' coordinates for rendering in ordinary Cartesian space (no `coord_polar`).
#' Called automatically by [geom_kodom_circular()]; rarely needed directly.
#'
#' @keywords internal
#' @format A ggproto object.
StatKodomCircular <- ggplot2::ggproto("StatKodomCircular", StatKodomBase,

  compute_panel = function(data, scales,
                           sort_by = "none", n_max = Inf,
                           gap_fraction = 0.15, inner_fraction = 0.3,
                           direction = 1L) {
    data <- .kodom_assign_lanes(data, sort_by = sort_by, n_max = n_max)

    n_ids   <- length(unique(data$y))
    gap_n   <- max(1L, ceiling(n_ids * gap_fraction))
    ang_max <- n_ids + gap_n

    time_range   <- range(data$x, na.rm = TRUE)
    inner_buffer <- diff(time_range) * inner_fraction
    radius       <- data$x + inner_buffer

    theta   <- pi / 2 - direction * 2 * pi * data$y / ang_max
    data$x  <- radius * cos(theta)
    data$y  <- radius * sin(theta)

    data
  }
)


#' Geom for geom_kodom_circular
#'
#' Extends [ggplot2::GeomPath] with kodom defaults for the circular layout.
#' Drops the `id` column in `setup_data()` so ggplot2 does not warn about
#' an unknown aesthetic. Draws interpolated colour-gradient segments via
#' [ggplot2::GeomSegment] and optional observation-point markers via
#' [ggplot2::GeomPoint].
#'
#' `size` and `linewidth` are routed to separate graphical elements:
#' `size` → points only, `linewidth` → path segments only.
#'
#' @keywords internal
#' @format A ggproto object.
GeomKodomCircular <- ggplot2::ggproto("GeomKodomCircular", ggplot2::GeomPath,

  default_aes = modifyList(
    ggplot2::GeomPath$default_aes,
    list(
      linewidth = 0.5,
      alpha     = 0.8,
      size      = 2.0,
      shape     = 19,
      fill      = NA,
      stroke    = 0.5
    )
  ),

  setup_data = function(data, params) {
    data$id <- NULL
    data
  },

  draw_panel = function(data, panel_params, coord, show_points = TRUE,
                        lineend = "butt", na.rm = FALSE) {
    seg_data  <- .kodom_build_segments(data)
    line_grob <- if (is.null(seg_data) || nrow(seg_data) == 0L) {
      grid::nullGrob()
    } else {
      ggplot2::GeomSegment$draw_panel(seg_data, panel_params, coord,
                                      lineend = lineend, na.rm = na.rm)
    }

    draw_pts <- show_points &&
      !all(is.na(data$shape)) &&
      !all(is.na(data$size) | data$size <= 0)

    if (!draw_pts) return(line_grob)

    point_data           <- data
    point_data$linewidth <- NULL
    point_grob <- ggplot2::GeomPoint$draw_panel(point_data, panel_params,
                                                coord, na.rm = na.rm)
    grid::grobTree(line_grob, point_grob)
  }
)


#' Circular ("Kodom") swimlane plot for longitudinal trajectories
#'
#' Draws a radial plot where each subject occupies an angular spoke and
#' time increases outward from the centre. Measurements are encoded as a
#' colour gradient along each radial path, interpolated between consecutive
#' observations. The layout resembles a Kadam flower
#' (*Neolamarckia cadamba*), giving the package its name.
#'
#' Unlike the original `coord_polar()` variant in the archive package,
#' this geom performs the polar-to-Cartesian transform inside the stat and
#' renders in ordinary Cartesian space, so all ggplot2 scales, themes, and
#' facets work normally. Use `coord_fixed()` to preserve the circular shape.
#'
#' @section Aesthetics:
#' Required aesthetics are shown in **bold**. `size` and `linewidth` are
#' intentionally independent: each controls a different graphical element.
#'
#' - **`x`** — time variable (numeric); mapped to radial distance
#' - **`id`** — subject identifier; mapped to angular position
#' - `colour` — color of both the connecting path and the point border
#' - `fill` — point interior color for filled shapes (21–25)
#' - `size` — **point size only**; set to `NA` or `0` to suppress points
#' - `linewidth` — **path width only**
#' - `alpha` — transparency applied to both path and points
#' - `shape` — point shape (default 19). Set to `NA` to suppress points.
#' - `stroke`, `linetype` — point border width / path line type
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data A data frame. If `NULL`, inherits from the plot.
#' @param stat The stat to use. Defaults to [StatKodomCircular].
#' @param position Position adjustment, almost always `"identity"`.
#' @param sort_by Lane ordering (angular order). One of `"none"` (default),
#'   `"mean"`, `"mean_asc"`, `"first"`, `"last"`.
#' @param n_max Maximum number of subjects to display. Default `Inf`.
#' @param gap_fraction Fraction of the full circle left empty as a visual
#'   gap at the ordering seam. Default `0.15`.
#' @param inner_fraction Fraction of the time range used as a hollow inner
#'   buffer so short-follow-up subjects remain visible. Default `0.3`.
#' @param direction `1L` = clockwise (default), `-1L` = counter-clockwise.
#' @param show_points If `TRUE` (default), draws a point at every
#'   observation. Set to `FALSE`, or map `shape = NA` / `size = 0`, to
#'   suppress points.
#' @param na.rm If `TRUE`, silently remove rows with missing required
#'   aesthetics.
#' @param show.legend Logical. Should this layer appear in the legend?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = time, id = subject_id, colour = hba1c)) +
#'   geom_kodom_circular(sort_by = "mean", gap_fraction = 0.1) +
#'   scale_colour_kodom() +
#'   coord_fixed() +
#'   theme_kodom_circular()
#' }
geom_kodom_circular <- function(mapping = NULL,
                                data = NULL,
                                stat = StatKodomCircular,
                                position = "identity",
                                ...,
                                sort_by = "none",
                                n_max = Inf,
                                gap_fraction = 0.15,
                                inner_fraction = 0.3,
                                direction = 1L,
                                show_points = TRUE,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  ggplot2::layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomKodomCircular,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      sort_by        = sort_by,
      n_max          = n_max,
      gap_fraction   = gap_fraction,
      inner_fraction = inner_fraction,
      direction      = direction,
      show_points    = show_points,
      na.rm          = na.rm,
      ...
    )
  )
}
