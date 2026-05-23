#### GEOM_KODOM -- ggplot2 EXTENSION FOR SWIMLANE TRAJECTORIES ####


#### StatKodom ####

#' @rdname geom_kodom
#' @format NULL
#' @usage NULL
#' @export
StatKodom <- ggplot2::ggproto("StatKodom", ggplot2::Stat,

  required_aes = c("x", "y"),

  extra_params = c("na.rm", "n_sample", "sort_by", "seed"),

  setup_data = function(data, params) {
    data$x <- as.numeric(data$x)
    data$y <- as.character(data$y)

    keep <- !is.na(data$x)
    if ("colour" %in% names(data)) keep <- keep & !is.na(data$colour)
    if ("fill" %in% names(data))   keep <- keep & !is.na(data$fill)
    data <- data[keep, , drop = FALSE]
    if (nrow(data) == 0L) return(data)

    ids <- unique(data$y)
    n_sample <- params$n_sample
    if (!is.null(n_sample) && length(ids) > n_sample) {
      if (!is.null(params$seed)) set.seed(params$seed)
      keep_ids <- sample(ids, n_sample)
      data <- data[data$y %in% keep_ids, , drop = FALSE]
    }

    data <- data[order(data$y, data$x), , drop = FALSE]

    sort_by <- params$sort_by
    val_col <- if ("colour" %in% names(data)) "colour"
               else if ("fill" %in% names(data)) "fill"
               else NULL
    use_named <- is.numeric(sort_by) && !is.null(names(sort_by))

    if (is.null(sort_by)) {
      data$y <- factor(data$y, levels = unique(data$y))
    } else if (use_named) {
      ids_now   <- as.character(unique(data$y))
      matched   <- intersect(ids_now, names(sort_by))
      unmatched <- setdiff(ids_now, names(sort_by))
      if (length(matched) > 0L) {
        sort_key <- sort_by[matched]
        if (length(unmatched) > 0L) {
          sort_key <- c(sort_key, stats::setNames(rep(Inf, length(unmatched)), unmatched))
        }
        id_order <- names(sort(sort_key))
      } else {
        id_order <- ids_now
      }
      data$y <- factor(data$y, levels = id_order)
    } else if (!is.null(val_col)) {
      sort_fn <- switch(as.character(sort_by),
        first  = function(v) v[1L],
        last   = function(v) v[length(v)],
        mean   = mean,
        median = stats::median,
        function(v) v[1L]
      )
      sort_key <- tapply(data[[val_col]], data$y, sort_fn)
      data$y <- factor(data$y, levels = names(sort(sort_key)))
    } else {
      data$y <- factor(data$y, levels = unique(data$y))
    }

    data <- data[order(data$y, data$x), , drop = FALSE]
    data$group <- as.integer(data$y)
    data
  },

  compute_panel = function(data, scales) {
    data
  }
)


#### GeomKodomPath ####

#' @rdname geom_kodom
#' @format NULL
#' @usage NULL
#' @export
GeomKodomPath <- ggplot2::ggproto("GeomKodomPath", ggplot2::GeomPath,

  default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = 0.7),

  draw_panel = function(self, data, panel_params, coord,
                        point_size = 0, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {
    path_grob <- ggplot2::GeomPath$draw_panel(data, panel_params, coord,
                                               arrow = arrow, lineend = lineend,
                                               linejoin = linejoin, linemitre = linemitre,
                                               na.rm = na.rm)

    if (point_size > 0) {
      point_data <- data
      point_data$size   <- point_size
      point_data$shape  <- 19L
      point_data$stroke <- 0.5
      point_data$fill   <- NA
      point_grob <- ggplot2::GeomPoint$draw_panel(point_data, panel_params, coord, na.rm = na.rm)
      grid::grobTree(path_grob, point_grob)
    } else {
      path_grob
    }
  }
)


#### geom_kodom() ####

#' Swimlane trajectory layer
#'
#' A ggplot2 geom that draws one path per subject, ordered and optionally
#' sampled by a summary of the colour aesthetic.  The ggplot2-native
#' equivalent of \code{\link{kodom_swimlane}}.
#'
#' The \code{y} aesthetic is always treated as discrete (one lane per
#' subject).  A \code{scale_y_discrete()} is included automatically so
#' integer subject IDs work without manual \code{factor()} conversion.
#' You can override it with your own \code{scale_y_discrete()} added
#' after \code{geom_kodom()}.
#'
#' @section Aesthetics:
#' \code{geom_kodom()} understands the following aesthetics (required in
#' bold):
#' \itemize{
#'   \item \strong{\code{x}} -- time axis
#'   \item \strong{\code{y}} -- subject identifier (becomes a discrete axis)
#'   \item \code{colour} -- measurement value mapped to a color gradient
#'   \item \code{alpha}
#'   \item \code{linewidth}
#'   \item \code{linetype}
#'   \item \code{group} (auto-set to \code{y})
#' }
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}.
#' @param data A data frame.  If \code{NULL}, uses the plot data.
#' @param stat The statistical transformation to use.  Default
#'   \code{"kodom"} applies subject sampling and sorting.
#' @param position Position adjustment.  Default \code{"identity"}.
#' @param ... Other arguments passed to the layer: \code{linewidth},
#'   \code{alpha}, \code{linetype}, etc.
#' @param n_sample Maximum number of subjects to display.  \code{NULL}
#'   (default) shows all.
#' @param sort_by How to order subjects on the y axis.  One of
#'   \code{"first"} (default), \code{"last"}, \code{"mean"},
#'   \code{"median"} -- applies that summary to the \code{colour}
#'   aesthetic per subject.  Or a named numeric vector whose names match
#'   subject ids (e.g.\code{\link{kodom_sort_scores}}).
#' @param seed Random seed for subject sampling.
#' @param point_size Size of observation markers drawn on top of the path.
#'   0 = no points.
#' @param arrow Arrow specification for line ends (see
#'   \code{\link[grid]{arrow}}).
#' @param lineend,linejoin,linemitre Line end/join style (passed to
#'   \code{\link[ggplot2]{GeomPath}}).
#' @param na.rm Remove missing values silently?
#' @param show.legend Include this layer in legends?
#' @param inherit.aes Inherit aesthetics from the plot?
#' @return A list containing a ggplot2 \code{\link[ggplot2]{layer}} and
#'   a discrete y-scale.
#' @export
#' @examples
#' library(ggplot2)
#' set.seed(42)
#' df <- data.frame(
#'   id    = rep(1:20, each = 5),
#'   time  = rep(seq(0, 120, 30), 20),
#'   value = rnorm(100, 7, 1.5)
#' )
#'
#' ggplot(df, aes(x = time, y = id, colour = value)) +
#'   geom_kodom()
#'
#' ggplot(df, aes(x = time, y = id, colour = value)) +
#'   geom_kodom(sort_by = "mean", n_sample = 10, point_size = 0) +
#'   scale_color_kodom(color_breaks = c(5.7, 7, 10)) +
#'   theme_kodom()
geom_kodom <- function(mapping = NULL, data = NULL,
                       stat = "kodom",
                       position = "identity",
                       ...,
                       n_sample = NULL,
                       sort_by = "first",
                       seed = 123,
                       point_size = 3,
                       arrow = NULL,
                       lineend = "butt",
                       linejoin = "round",
                       linemitre = 10,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer_obj <- ggplot2::layer(
    data        = data,
    mapping     = mapping,
    stat        = StatKodom,
    geom        = GeomKodomPath,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = rlang::list2(
      n_sample   = n_sample,
      sort_by    = sort_by,
      seed       = seed,
      point_size = point_size,
      arrow      = arrow,
      lineend    = lineend,
      linejoin   = linejoin,
      linemitre  = linemitre,
      na.rm      = na.rm,
      ...
    )
  )
  list(layer_obj, ggplot2::scale_y_discrete())
}


#### stat_kodom() ####

#' @rdname geom_kodom
#' @param geom The geometric object to use.  Default uses
#'   \code{GeomKodomPath} (path + optional points).  Use \code{"path"}
#'   or \code{"point"} for standard ggplot2 geoms with kodom sorting.
#' @export
stat_kodom <- function(mapping = NULL, data = NULL,
                       geom = "kodom_path",
                       position = "identity",
                       ...,
                       n_sample = NULL,
                       sort_by = "first",
                       seed = 123,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data        = data,
    mapping     = mapping,
    stat        = StatKodom,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = rlang::list2(
      n_sample = n_sample,
      sort_by  = sort_by,
      seed     = seed,
      na.rm    = na.rm,
      ...
    )
  )
}


#### StatKodomTile ####

#' @rdname geom_kodom_tile
#' @format NULL
#' @usage NULL
#' @export
StatKodomTile <- ggplot2::ggproto("StatKodomTile", StatKodom,

  extra_params = c("na.rm", "n_sample", "sort_by", "seed",
                   "time_bins", "time_breaks", "agg_fun"),

  setup_data = function(data, params) {
    data <- StatKodom$setup_data(data, params)
    if (nrow(data) == 0L) return(data)

    time_bins <- if (is.null(params$time_bins)) 10L else params$time_bins
    if (!is.null(params$time_breaks)) {
      breaks <- params$time_breaks
    } else {
      tr <- range(data$x, na.rm = TRUE)
      breaks <- seq(tr[1], tr[2], length.out = time_bins + 1L)
    }
    mids <- (breaks[-length(breaks)] + breaks[-1]) / 2
    bin_idx <- as.integer(cut(data$x, breaks, include.lowest = TRUE))
    data$x <- mids[bin_idx]

    val_col <- if ("fill" %in% names(data)) "fill"
               else if ("colour" %in% names(data)) "colour"
               else NULL

    agg_fun <- if (is.null(params$agg_fun)) "mean" else params$agg_fun
    agg_fn <- switch(agg_fun,
      mean   = mean,
      median = stats::median,
      first  = function(v) v[1L],
      last   = function(v) v[length(v)],
      min    = min,
      max    = max,
      mean
    )

    data <- data[order(data$PANEL, data$y, data$x), , drop = FALSE]
    keys <- paste(data$PANEL, data$group, data$x, sep = "\x1f")
    first_idx <- !duplicated(keys)

    if (!is.null(val_col)) {
      agg_vals <- tapply(data[[val_col]], keys, agg_fn)
      data <- data[first_idx, , drop = FALSE]
      data[[val_col]] <- as.numeric(agg_vals[keys[first_idx]])
    } else {
      data <- data[first_idx, , drop = FALSE]
    }

    x_vals <- sort(unique(data$x))
    x_width <- if (length(x_vals) > 1L) min(diff(x_vals)) else 1
    y_int <- as.integer(data$y)
    data$xmin <- data$x - x_width / 2
    data$xmax <- data$x + x_width / 2
    data$ymin <- y_int - 0.5
    data$ymax <- y_int + 0.5

    data
  }
)


#### GeomKodomTile ####

#' @rdname geom_kodom_tile
#' @format NULL
#' @usage NULL
#' @export
GeomKodomTile <- ggplot2::ggproto("GeomKodomTile", ggplot2::GeomRect,
  default_aes = ggplot2::aes(fill = "grey35", colour = "white",
                             linewidth = 0.25, linetype = 1, alpha = NA),

  required_aes = c("x", "y")
)


#### geom_kodom_tile() ####

#' Swimlane heatmap layer
#'
#' A ggplot2 geom that draws one row of colored tiles per subject,
#' with time binned and values aggregated within each bin.  The
#' ggplot2-native equivalent of
#' \code{\link{kodom_swimlane}(geom = "tile")}.
#'
#' Unlike \code{\link{geom_kodom}} (which uses the \code{colour}
#' aesthetic), this geom uses \code{fill} for the value channel.
#' The \code{y} aesthetic is treated as discrete; a
#' \code{scale_y_discrete()} is included automatically.
#'
#' @section Aesthetics:
#' \code{geom_kodom_tile()} understands the following aesthetics
#' (required in bold):
#' \itemize{
#'   \item \strong{\code{x}} -- time axis (binned automatically)
#'   \item \strong{\code{y}} -- subject identifier (discrete)
#'   \item \code{fill} -- measurement value mapped to tile color
#'   \item \code{alpha}
#'   \item \code{colour} -- tile border color (default white)
#'   \item \code{linewidth} -- tile border width
#'   \item \code{group} (auto-set to \code{y})
#' }
#'
#' @inheritParams geom_kodom
#' @param time_bins Number of equal-width time bins (default 10).
#'   Ignored when \code{time_breaks} is given.
#' @param time_breaks Custom numeric vector of bin boundaries.
#'   Overrides \code{time_bins}.
#' @param agg_fun How to aggregate values within a bin.  One of
#'   \code{"mean"} (default), \code{"median"}, \code{"first"},
#'   \code{"last"}, \code{"min"}, \code{"max"}.
#' @return A list containing a ggplot2 \code{\link[ggplot2]{layer}}
#'   and a discrete y-scale.
#' @export
#' @examples
#' library(ggplot2)
#' set.seed(42)
#' df <- data.frame(
#'   id    = rep(1:30, each = 10),
#'   time  = rep(seq(0, 120, length.out = 10), 30),
#'   value = rnorm(300, 7, 1.5)
#' )
#'
#' ggplot(df, aes(x = time, y = id, fill = value)) +
#'   geom_kodom_tile()
#'
#' ggplot(df, aes(x = time, y = id, fill = value)) +
#'   geom_kodom_tile(sort_by = "mean", n_sample = 15, time_bins = 6) +
#'   scale_fill_kodom(color_breaks = c(5.7, 7, 10)) +
#'   theme_kodom()
geom_kodom_tile <- function(mapping = NULL, data = NULL,
                            stat = "kodom_tile",
                            position = "identity",
                            ...,
                            n_sample = NULL,
                            sort_by = "mean",
                            seed = 123,
                            time_bins = 10,
                            time_breaks = NULL,
                            agg_fun = "mean",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer_obj <- ggplot2::layer(
    data        = data,
    mapping     = mapping,
    stat        = StatKodomTile,
    geom        = GeomKodomTile,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = rlang::list2(
      n_sample    = n_sample,
      sort_by     = sort_by,
      seed        = seed,
      time_bins   = time_bins,
      time_breaks = time_breaks,
      agg_fun     = agg_fun,
      na.rm       = na.rm,
      ...
    )
  )
  list(layer_obj, ggplot2::scale_y_discrete())
}
