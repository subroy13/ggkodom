#### STAT AND GEOM FOR geom_kodom_heatmap ####
# Shared internals (.kodom_assign_lanes, StatKodomBase)
# live in aaa_kodom_internals.R, which loads first alphabetically.


#' Stat for geom_kodom_heatmap
#'
#' Bins time into equal-width intervals, aggregates the `fill` aesthetic
#' within each (subject × bin) cell, and returns a data frame suitable for
#' [ggplot2::GeomTile]. Called automatically by [geom_kodom_heatmap()];
#' rarely needed directly.
#'
#' @keywords internal
#' @format A ggproto object.
StatKodomHeatmap <- ggplot2::ggproto("StatKodomHeatmap", StatKodomBase,

  required_aes = c("x", "id", "fill"),

  compute_panel = function(data, scales,
                           sort_by = "mean", n_max = Inf,
                           bins = 10L, breaks = NULL,
                           fun = "mean") {
    data <- .kodom_assign_lanes(data, sort_by = sort_by, n_max = n_max)

    time_vals <- data$x
    if (!is.null(breaks)) {
      bin_breaks <- breaks
    } else {
      tr         <- range(time_vals, na.rm = TRUE)
      bin_breaks <- seq(tr[1L], tr[2L], length.out = as.integer(bins) + 1L)
    }

    mids        <- (bin_breaks[-length(bin_breaks)] + bin_breaks[-1L]) / 2
    bin_width   <- bin_breaks[2L] - bin_breaks[1L]
    data$.bin   <- cut(time_vals, breaks = bin_breaks,
                       include.lowest = TRUE, labels = FALSE)
    data$.mid_x <- mids[data$.bin]

    agg_fn <- switch(fun,
      mean   = mean,
      median = stats::median,
      first  = function(x) x[[1L]],
      last   = function(x) x[[length(x)]],
      min    = min,
      max    = max,
      stop("fun must be one of: mean, median, first, last, min, max",
           call. = FALSE)
    )

    groups  <- split(data, list(data$group, data$.bin), drop = TRUE)
    tile_df <- do.call(rbind, lapply(groups, function(gd) {
      data.frame(
        x         = gd$.mid_x[[1L]],
        y         = gd$y[[1L]],
        fill      = agg_fn(gd$fill),
        width     = bin_width,
        height    = 1,
        PANEL     = gd$PANEL[[1L]],
        group     = gd$group[[1L]],
        stringsAsFactors = FALSE
      )
    }))

    tile_df
  }
)


#' Geom for geom_kodom_heatmap
#'
#' Extends [ggplot2::GeomTile] with kodom defaults. Drops `id` in
#' `setup_data()` to avoid an unknown-aesthetic warning from ggplot2.
#'
#' @keywords internal
#' @format A ggproto object.
GeomKodomHeatmap <- ggplot2::ggproto("GeomKodomHeatmap", ggplot2::GeomTile,

  default_aes = modifyList(
    ggplot2::GeomTile$default_aes,
    list(colour = "white", linewidth = 0.25)
  ),

  setup_data = function(data, params) {
    data$id <- NULL
    ggplot2::GeomTile$setup_data(data, params)
  }
)


#' Swimlane heatmap for longitudinal trajectories
#'
#' Divides the time axis into equal-width bins and fills each
#' (subject × bin) cell with an aggregate of the measured value. Each
#' subject occupies one horizontal lane. Use this layout when the cohort
#' is large or when you prefer a compact, aligned grid over individual
#' paths.
#'
#' Lane ordering is controlled by `sort_by` (defaults to `"mean"`, which
#' places the highest-mean subjects at the top). Time bins can be
#' customised via `bins` (number of equal-width intervals) or `breaks`
#' (explicit boundaries).
#'
#' @section Aesthetics:
#' Required aesthetics are shown in **bold**.
#'
#' - **`x`** — time variable (numeric or Date)
#' - **`id`** — subject identifier; determines lane position on the y axis
#' - **`fill`** — measurement value used for cell color and sorting
#' - `colour` — tile border color (default `"white"`)
#' - `linewidth` — tile border width (default `0.25`)
#' - `alpha` — transparency
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data A data frame. If `NULL`, inherits from the plot.
#' @param stat The stat to use. Defaults to [StatKodomHeatmap].
#' @param position Position adjustment, almost always `"identity"`.
#' @param sort_by Lane ordering. One of `"none"`, `"mean"` (default),
#'   `"mean_asc"`, `"first"`, `"last"` — all refer to the `fill` variable.
#' @param n_max Maximum number of subjects to display. A random sample is
#'   taken when exceeded. Default `Inf` (all subjects).
#' @param bins Number of equal-width time bins. Default `10L`. Ignored if
#'   `breaks` is supplied.
#' @param breaks Numeric vector of explicit bin boundaries. Overrides `bins`.
#' @param fun Aggregation function per cell. One of `"mean"` (default),
#'   `"median"`, `"first"`, `"last"`, `"min"`, `"max"`.
#' @param na.rm If `TRUE`, silently remove rows with missing required
#'   aesthetics.
#' @param show.legend Logical. Should this layer appear in the legend?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = time, id = subject_id, fill = hba1c)) +
#'   geom_kodom_heatmap(sort_by = "mean", bins = 12) +
#'   scale_fill_kodom() +
#'   theme_kodom()
#' }
geom_kodom_heatmap <- function(mapping = NULL,
                               data = NULL,
                               stat = StatKodomHeatmap,
                               position = "identity",
                               ...,
                               sort_by = "mean",
                               n_max = Inf,
                               bins = 10L,
                               breaks = NULL,
                               fun = "mean",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  ggplot2::layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomKodomHeatmap,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      sort_by = sort_by,
      n_max   = n_max,
      bins    = bins,
      breaks  = breaks,
      fun     = fun,
      na.rm   = na.rm,
      ...
    )
  )
}
