#### MULTIVARIATE SWIMLANE — STACKED PANELS, SHARED LANE ORDER ####

#' Multivariate functional swimlane
#'
#' Stack one swimlane per variable, all sharing the same subject (lane)
#' ordering computed from a single anchor variable.  Each variable gets
#' its own color scale — A1c, BMI, LDL etc. live on different ranges and
#' should not be forced onto a common gradient.
#'
#' This is the "show me all the trajectories at once" view for multivariate
#' longitudinal data.  Lane alignment across panels lets you read joint
#' dynamics by eye: the subject who deteriorates on A1c, does she also
#' gain weight?  Sort lanes by A1c (the anchor) and the BMI panel will
#' tell you immediately.
#'
#' Requires the \pkg{patchwork} package.
#'
#' @param data Long-format data frame with one row per
#'   (subject, variable, time).
#' @param id Column name for subject id (default \code{"id"}).
#' @param time Column name for time (default \code{"time"}).
#' @param value Column name for measurement value (default \code{"value"}).
#' @param variable Column name distinguishing variables
#'   (default \code{"variable"}).
#' @param variables Character vector of variables to plot, in the desired
#'   row order.  \code{NULL} (default) uses all variables in the data,
#'   in their natural (factor / first-seen) order.
#' @param anchor Variable that determines the shared lane ordering.
#'   \code{NULL} (default) = first entry of \code{variables}.
#' @param sort_by See \code{\link{kodom_swimlane}} — applied to the anchor
#'   variable only.
#' @param colors_list Optional named list \code{list(varname = color_vec)}
#'   to override per-variable color palettes.  Variables not named in the
#'   list use \code{\link{kodom_colors}()}.
#' @param color_breaks_list Optional named list
#'   \code{list(varname = numeric_breaks)} to set per-variable color
#'   breakpoints (e.g. clinical thresholds: A1c at 7/8, BMI at 25/30).
#' @param discretize Either a single logical (applied to every panel) or
#'   a named logical list (per-variable).
#' @param n_sample Max subjects (computed on the anchor variable; the same
#'   set is then carried into every other panel).
#' @param point_size,line_width,alpha Cosmetic args, passed to every panel.
#' @param ncol Patchwork column count (default 1 = vertical stack).
#' @param seed Random seed for subject sampling on the anchor.
#' @return A \pkg{patchwork} composite plot.
#' @export
#' @examples
#' \dontrun{
#'   ## datathon-style: long with id / variable / time / value
#'   ## variables: a1c, bmi
#'   kodom_swimlane_multi(
#'     measurements,
#'     anchor = "a1c",
#'     color_breaks_list = list(a1c = c(6.5, 7, 8),
#'                              bmi = c(25, 30)),
#'     discretize = list(a1c = TRUE, bmi = FALSE)
#'   )
#' }
kodom_swimlane_multi <- function(data,
                                 id = "id", time = "time", value = "value",
                                 variable = "variable",
                                 variables = NULL,
                                 anchor    = NULL,
                                 sort_by   = "first",
                                 colors_list       = NULL,
                                 color_breaks_list = NULL,
                                 discretize = FALSE,
                                 n_sample   = 50,
                                 point_size = 1.5,
                                 line_width = 0.5,
                                 alpha      = 0.7,
                                 ncol       = 1,
                                 seed       = 123) {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Install 'patchwork' to use kodom_swimlane_multi().", call. = FALSE)
  }
  check_columns(data, c(id, time, value, variable))

  if (is.null(variables)) variables <- as.character(unique(data[[variable]]))
  if (is.null(anchor))    anchor    <- variables[1L]
  if (!anchor %in% variables) {
    stop("anchor '", anchor, "' is not in variables.", call. = FALSE)
  }

  ## anchor lane order
  anchor_dat <- data[as.character(data[[variable]]) == anchor, , drop = FALSE]
  if (nrow(anchor_dat) == 0L) {
    stop("Anchor variable '", anchor, "' has no rows in data.", call. = FALSE)
  }
  prep <- prep_trajectory_data(anchor_dat, id, time, value,
                               n_sample = n_sample, sort_by = sort_by,
                               seed = seed)
  shared_order <- prep$id_order

  ## one panel per variable, forcing shared lane order
  cols <- list(id = id, time = time, value = value)
  last_var <- variables[length(variables)]

  plots <- lapply(variables, function(v) {
    sub <- data[as.character(data[[variable]]) == v &
                as.character(data[[id]]) %in% shared_order, , drop = FALSE]

    cb <- if (!is.null(color_breaks_list)) color_breaks_list[[v]] else NULL
    cl <- if (!is.null(colors_list))       colors_list[[v]]       else kodom_colors()
    dz <- if (is.list(discretize)) isTRUE(discretize[[v]]) else isTRUE(discretize)

    ## empty panel placeholder if this variable has no overlap with anchor ids
    if (nrow(sub) == 0L) {
      return(ggplot2::ggplot() +
             ggplot2::ggtitle(paste0(v, "  (no data)")) +
             ggplot2::theme_void())
    }

    sub[[time]]  <- as.numeric(sub[[time]])
    sub[[value]] <- as.numeric(sub[[value]])
    sub <- sub[!is.na(sub[[time]]) & !is.na(sub[[value]]), , drop = FALSE]
    sub <- sub[order(sub[[id]], sub[[time]]), , drop = FALSE]
    sub[[id]] <- factor(as.character(sub[[id]]), levels = shared_order)

    p <- draw_swimlane_path(sub, cols, line_width, alpha, point_size) +
      kodom_value_scale("color", cl, cb, name = v, discretize = dz) +
      ggplot2::scale_y_discrete(limits = shared_order, drop = FALSE) +
      theme_kodom() +
      ggplot2::labs(x = if (identical(v, last_var)) time else NULL, y = v) +
      ggplot2::guides(
        color = ggplot2::guide_colorbar(barwidth = 12, barheight = 0.6)
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(2, 6, 2, 6),
        axis.text.y = if (length(shared_order) > 30L)
                        ggplot2::element_blank()
                      else ggplot2::element_text(size = 6)
      )
    p
  })

  patchwork::wrap_plots(plots, ncol = ncol)
}
