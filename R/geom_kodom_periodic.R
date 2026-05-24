#### STAT AND GEOM FOR geom_kodom_periodic ####
# Shared internals (.kodom_assign_lanes, .kodom_build_segments, StatKodomBase)
# live in aaa_kodom_internals.R, which loads first alphabetically.

#' Stat for geom_kodom_periodic
#'
#' Assigns lane positions, converts time to an angle within one period
#' (`x = time mod period`), and converts lane rank to an Archimedean spiral
#' radius (`y = inner_radius + lane + spiral_fraction * total_time / period`).
#' The stat also breaks groups at period boundaries so that
#' `.kodom_build_segments` never connects the end of one cycle to the start
#' of the next. After this transform, `coord_polar(theta = "x")` renders the
#' data as a star trail with no extra manual Cartesian conversion required.
#'
#' @keywords internal
#' @format A ggproto object.
StatKodomPeriodic <- ggplot2::ggproto("StatKodomPeriodic", StatKodomBase,
    required_aes = c("x", "id", "colour"),
    compute_panel = function(data, scales,
                             sort_by = "none", n_max = Inf,
                             period = 12,
                             inner_fraction = 0.3,
                             spiral_fraction = 0.1) {
        data         <- .kodom_assign_lanes(data, sort_by = sort_by, n_max = n_max)
        n_lanes      <- max(data$y, na.rm = TRUE)
        inner_radius <- inner_fraction * n_lanes

        # Break groups at period boundaries (uses original total time) so
        # .kodom_build_segments stops at the seam rather than drawing a
        # backward-going chord across the ring.
        data$group <- data$group * 1000L + as.integer(floor(data$x / period))

        # Archimedean spiral radius: base lane position + outward drift.
        # Uses original total time before it is reduced modulo period.
        data$y <- inner_radius + data$y + spiral_fraction * (data$x / period)

        # Angle within one cycle: map total time to [0, period).
        data$x <- data$x %% period

        data
    }
)


#' Geom for geom_kodom_periodic
#'
#' Extends [ggplot2::GeomPath] to draw concentric "star trail" arcs.
#' Interpolates colours/alpha in the (angle, radius) space that the stat
#' already produces, then delegates the polar projection to
#' `coord_polar(theta = "x")`. This means standard ggplot2 scales and
#' coordinates work as expected: use [ggplot2::scale_x_continuous()] for
#' time-axis labels, [ggplot2::coord_polar()] for direction, and
#' [theme_kodom_periodic()] for a clean panel.
#'
#' @keywords internal
#' @format A ggproto object.
GeomKodomPeriodic <- ggplot2::ggproto("GeomKodomPeriodic", ggplot2::GeomPath,
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
    draw_panel = function(data, panel_params, coord,
                          show_points = TRUE, lineend = "butt", na.rm = FALSE) {
        # Data is already in (angle = x mod period, radius = y) space.
        # coord_polar(theta = "x") handles the actual projection.
        seg_data  <- .kodom_build_segments(data)
        line_grob <- grid::nullGrob()

        if (!is.null(seg_data) && nrow(seg_data) > 0L) {
            line_grob <- ggplot2::GeomSegment$draw_panel(
                seg_data, panel_params, coord,
                lineend = lineend, na.rm = na.rm
            )
        }

        draw_pts <- show_points &&
            !all(is.na(data$shape)) &&
            !all(is.na(data$size) | data$size <= 0)

        if (!draw_pts) return(line_grob)

        point_data           <- data
        point_data$linewidth <- NULL
        point_grob <- ggplot2::GeomPoint$draw_panel(
            point_data, panel_params, coord, na.rm = na.rm
        )

        grid::grobTree(line_grob, point_grob)
    }
)


#' Periodic "Star Trail" swimlane plot for longitudinal trajectories
#'
#' Draws concentric arcs representing cyclical longitudinal data. Time maps to
#' angle (one full revolution equals one `period`), and each subject's base
#' radius is determined by its lane rank. To separate successive cycles of the
#' same subject the radius drifts outward slightly as total time increases,
#' forming a star trail or Archimedean spiral.
#'
#' **Coordinate system.** This geom is designed for use with
#' `coord_polar(theta = "x")`. Use [coord_kodom_periodic()] as a convenient
#' wrapper that sets `start = pi/2` (12 o'clock at `x = 0`) and
#' `direction = -1` (clockwise) by default.
#'
#' **Axis labels.** Because `x` is time within one cycle, standard ggplot2
#' scale functions work directly:
#' ```r
#' scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(0, 12))
#' ```
#'
#' @inheritParams geom_kodom_line
#' @param period Numeric. Length of one complete cycle (e.g. 12 for months,
#'   24 for hours). Default `12`.
#' @param spiral_fraction Numeric. Radial expansion per full cycle as a
#'   fraction of one lane width. `0` keeps cycles on the same ring;
#'   `0.1` (default) drifts outward by 10% of a lane per cycle.
#' @param inner_fraction Fraction of total lanes used as a hollow inner buffer.
#'   Default `0.3`.
#' @export
geom_kodom_periodic <- function(mapping = NULL, data = NULL,
                                stat = StatKodomPeriodic, position = "identity",
                                ...,
                                period = 12,
                                spiral_fraction = 0.1,
                                inner_fraction = 0.3,
                                sort_by = "none",
                                n_max = Inf,
                                show_points = TRUE,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
    ggplot2::layer(
        data = data, mapping = mapping, stat = stat, geom = GeomKodomPeriodic,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(
            period = period, spiral_fraction = spiral_fraction,
            inner_fraction = inner_fraction,
            sort_by = sort_by, n_max = n_max,
            show_points = show_points, na.rm = na.rm, ...
        )
    )
}
