#### KODOM_COMPARE — RAW SWIMLANE NEXT TO FIT SWIMLANE ####

#' Compare raw observations against a model's fitted curves
#'
#' Two visual idioms for the "what did the smoother buy you" picture:
#' \describe{
#'   \item{\code{layout = "overlay"} (default)}{Raw points and the fitted
#'     curve share a single set of lanes.  Both are colored on the same
#'     value scale, so a residual reads off as a color mismatch between a
#'     raw point and the underlying fit line.  Compact and direct.}
#'   \item{\code{layout = "side"}}{Two side-by-side panels — raw points on
#'     the left, fit curves on the right — sharing lane ordering and color
#'     scale.  Better when the overlay would be too dense.}
#' }
#'
#' Sorting by an FPC score (\code{sort_by = kodom_sort_scores(fp)}) and
#' faceting by a cluster (\code{facet_rows = "cluster"}) is the canonical
#' diagnostic.
#'
#' @param fit A fitted model (any class supported by
#'   \code{\link{kodom_fit}}).
#' @param data Long-format raw data (\code{id}, \code{time}, \code{value}).
#'   For lmer/gam, defaults to the model frame stored in \code{fit}.
#' @param id,time,value Column names (strings).  \code{value} defaults
#'   to the LHS of \code{formula(fit)} for lmer/gam.
#' @param layout \code{"overlay"} (default) or \code{"side"}.
#' @param n_grid Per-subject prediction-grid resolution (default 60).
#' @param panel_labels Length-2 character vector for the panel strip
#'   labels (\code{layout = "side"} only).  Default \code{c("raw", "fit")}.
#' @param ... Passed through to the drawer for cosmetic controls
#'   (\code{n_sample}, \code{sort_by}, \code{colors}, \code{color_breaks},
#'   \code{discretize}, \code{point_size}, \code{line_width}, \code{alpha},
#'   \code{facet_rows}, \code{seed}).
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' \dontrun{
#'   library(fdapace)
#'   Ly <- split(df$value, df$id); Lt <- split(df$time, df$id)
#'   fp <- FPCA(Ly = Ly, Lt = Lt, optns = list(dataType = "Sparse"))
#'   key <- kodom_sort_scores(fp)
#'   cl  <- cluster_scores(fp, k = 3)
#'   df$cluster <- cl[as.character(df$id)]
#'   kodom_compare(fp, df, sort_by = key, facet_rows = "cluster")
#'   kodom_compare(fp, df, layout = "side", sort_by = key)
#' }
kodom_compare <- function(fit, data = NULL,
                          id = "id", time = "time", value = NULL,
                          layout = c("overlay", "side"),
                          n_grid = 60,
                          panel_labels = c("raw", "fit"),
                          ...) {
  layout <- match.arg(layout)
  if (length(panel_labels) != 2L) {
    stop("panel_labels must be length 2.", call. = FALSE)
  }
  bits <- compare_bits(fit, data = data, id = id, time = time, value = value,
                       n_grid = n_grid)

  if (layout == "overlay") {
    draw_compare_overlay(bits$raw, bits$fit_grid,
                         id = bits$id, time = bits$time, value = bits$value,
                         ...)
  } else {
    draw_compare(bits$raw, bits$fit_grid,
                 id = bits$id, time = bits$time, value = bits$value,
                 panel_labels = panel_labels, ...)
  }
}


#### INTERNAL: COLLECT (raw, fit-grid) PER FIT CLASS ####

#' @keywords internal
compare_bits <- function(fit, data, id, time, value, n_grid) {
  UseMethod("compare_bits")
}

#' @keywords internal
#' @export
compare_bits.default <- function(fit, data, id, time, value, n_grid) {
  stop("kodom_compare(): no method for fit class ",
       paste(class(fit), collapse = "/"), ".", call. = FALSE)
}

#' @keywords internal
#' @export
compare_bits.lmerMod <- function(fit, data, id, time, value, n_grid) {
  if (is.null(data))  data  <- fit@frame
  if (is.null(value)) value <- response_name(stats::formula(fit))
  grid_df <- build_subject_grid(data, id, time, n_grid)
  grid_df[[value]] <- stats::predict(fit, newdata = grid_df, re.form = NULL,
                                     allow.new.levels = TRUE)
  list(raw = data, fit_grid = grid_df, id = id, time = time, value = value)
}

#' @keywords internal
#' @export
compare_bits.glmerMod <- function(fit, data, id, time, value, n_grid) {
  if (is.null(data))  data  <- fit@frame
  if (is.null(value)) value <- response_name(stats::formula(fit))
  grid_df <- build_subject_grid(data, id, time, n_grid)
  grid_df[[value]] <- stats::predict(fit, newdata = grid_df, re.form = NULL,
                                     type = "response", allow.new.levels = TRUE)
  list(raw = data, fit_grid = grid_df, id = id, time = time, value = value)
}

#' @keywords internal
#' @export
compare_bits.gam <- function(fit, data, id, time, value, n_grid) {
  if (is.null(data))  data  <- fit$model
  if (is.null(value)) value <- response_name(stats::formula(fit))
  grid_df <- build_subject_grid(data, id, time, n_grid)
  if (is.factor(data[[id]])) {
    grid_df[[id]] <- factor(grid_df[[id]], levels = levels(data[[id]]))
  }
  grid_df[[value]] <- as.numeric(stats::predict(fit, newdata = grid_df,
                                                type = "response"))
  list(raw = data, fit_grid = grid_df, id = id, time = time, value = value)
}

#' @keywords internal
#' @export
compare_bits.fpca <- function(fit, data, id, time, value, n_grid) {
  if (is.null(data)) {
    stop("kodom_compare(): supply 'data' for FPCA fits.", call. = FALSE)
  }
  if (is.null(value)) value <- "value"
  ext  <- fpca_extract(fit)
  Yhat <- if (!is.null(fit$Yhat)) fit$Yhat
          else sweep(ext$scores %*% t(ext$phi), 2L, ext$mu, "+")
  ids  <- if (!is.null(rownames(Yhat))) rownames(Yhat) else ext$ids
  grid_df <- fpca_long(Yhat, ext$args, ids, id, time, value)
  list(raw = data, fit_grid = grid_df, id = id, time = time, value = value)
}

#' @keywords internal
#' @export
compare_bits.fpca.sc <- function(fit, ...) compare_bits.fpca(fit, ...)

#' @keywords internal
#' @export
compare_bits.FPCA <- function(fit, data, id, time, value, n_grid) {
  if (is.null(data)) {
    stop("kodom_compare(): supply 'data' for FPCA fits.", call. = FALSE)
  }
  if (is.null(value)) value <- "value"
  ext  <- fpca_extract(fit)
  Yhat <- sweep(ext$scores %*% t(ext$phi), 2L, ext$mu, "+")
  grid_df <- fpca_long(Yhat, ext$args, ext$ids, id, time, value)
  list(raw = data, fit_grid = grid_df, id = id, time = time, value = value)
}


#### INTERNAL: TWO-PANEL DRAWER ####

#' @keywords internal
draw_compare <- function(raw_df, fit_grid_df, id, time, value,
                         panel_labels = c("raw", "fit"),
                         n_sample = 50, sort_by = NULL,
                         colors = kodom_colors(), color_breaks = NULL,
                         discretize = FALSE,
                         point_size = 2, line_width = 0.8, alpha = 0.85,
                         facet_rows = NULL, seed = 123) {

  ## pick subjects + sort ORDER from the raw data
  if (is.null(sort_by)) sort_by <- "first"
  prep <- prep_trajectory_data(
    raw_df, id, time, value,
    n_sample = n_sample, sort_by = sort_by, seed = seed
  )
  id_order <- prep$id_order
  raw_use  <- prep$data

  ## carry id ordering into the fit grid; filter to chosen subjects
  fit_grid_df[[id]]   <- as.character(fit_grid_df[[id]])
  fit_grid_df         <- fit_grid_df[fit_grid_df[[id]] %in% id_order, , drop = FALSE]
  fit_grid_df[[id]]   <- factor(fit_grid_df[[id]], levels = id_order)
  fit_grid_df[[time]] <- as.numeric(fit_grid_df[[time]])

  ## carry facet_rows from raw data into fit grid (one value per subject)
  if (!is.null(facet_rows)) {
    if (!facet_rows %in% names(raw_df)) {
      stop("facet_rows column '", facet_rows, "' not found in 'data'.",
           call. = FALSE)
    }
    fmap <- raw_df[!duplicated(raw_df[[id]]), c(id, facet_rows), drop = FALSE]
    fit_grid_df <- merge(fit_grid_df, fmap, by = id, all.x = TRUE)
  }

  ## tag panels
  raw_use$.panel     <- factor(panel_labels[1L], levels = panel_labels)
  fit_grid_df$.panel <- factor(panel_labels[2L], levels = panel_labels)

  ## stitch
  keep <- c(id, time, value, ".panel", facet_rows)
  combined <- rbind(raw_use[, keep, drop = FALSE],
                    fit_grid_df[, keep, drop = FALSE])

  p <- ggplot2::ggplot(combined, ggplot2::aes(
      x     = .data[[time]],
      y     = .data[[id]],
      group = .data[[id]]
    )) +
    ## raw panel: just points
    ggplot2::geom_point(
      data = combined[combined$.panel == panel_labels[1L], , drop = FALSE],
      ggplot2::aes(color = .data[[value]]),
      size = point_size
    ) +
    ## fit panel: dense path
    ggplot2::geom_path(
      data = combined[combined$.panel == panel_labels[2L], , drop = FALSE],
      ggplot2::aes(color = .data[[value]]),
      linewidth = line_width, alpha = alpha
    ) +
    kodom_value_scale("color", colors, color_breaks,
                      name = value, discretize = discretize)

  ## facet: panel as columns; optional facet_rows as rows
  if (!is.null(facet_rows)) {
    p <- p + ggplot2::facet_grid(
      stats::as.formula(paste(facet_rows, "~ .panel")),
      scales = "free_y", space = "free_y"
    )
  } else {
    p <- p + ggplot2::facet_wrap(~ .panel, ncol = 2L)
  }

  if (!is.null(prep$y_labels)) {
    p <- p + ggplot2::scale_y_discrete(labels = prep$y_labels)
  }

  p + theme_kodom() +
    ggplot2::labs(x = time, y = "") +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(barwidth = 15, barheight = 0.75)
    )
}


#### INTERNAL: OVERLAY DRAWER (default layout) ####

#' @keywords internal
draw_compare_overlay <- function(raw_df, fit_grid_df, id, time, value,
                                 n_sample = 50, sort_by = NULL,
                                 colors = kodom_colors(), color_breaks = NULL,
                                 discretize = FALSE,
                                 point_size = 2, line_width = 0.5, alpha = 0.85,
                                 facet_rows = NULL, seed = 123) {

  if (is.null(sort_by)) sort_by <- "first"
  prep <- prep_trajectory_data(
    raw_df, id, time, value,
    n_sample = n_sample, sort_by = sort_by, seed = seed
  )
  id_order <- prep$id_order
  raw_use  <- prep$data

  fit_grid_df[[id]]   <- as.character(fit_grid_df[[id]])
  fit_grid_df         <- fit_grid_df[fit_grid_df[[id]] %in% id_order, , drop = FALSE]
  fit_grid_df[[id]]   <- factor(fit_grid_df[[id]], levels = id_order)
  fit_grid_df[[time]] <- as.numeric(fit_grid_df[[time]])

  if (!is.null(facet_rows)) {
    if (!facet_rows %in% names(raw_df)) {
      stop("facet_rows column '", facet_rows, "' not found in 'data'.",
           call. = FALSE)
    }
    fmap <- raw_df[!duplicated(raw_df[[id]]), c(id, facet_rows), drop = FALSE]
    fit_grid_df <- merge(fit_grid_df, fmap, by = id, all.x = TRUE)
  }

  ## fit lines underneath, raw points on top.  Both keyed by value
  ## color scale → residual = visible color mismatch.
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = fit_grid_df,
      ggplot2::aes(x = .data[[time]], y = .data[[id]],
                   group = .data[[id]],
                   color = .data[[value]]),
      linewidth = line_width, alpha = alpha
    ) +
    ggplot2::geom_point(
      data = raw_use,
      ggplot2::aes(x = .data[[time]], y = .data[[id]],
                   color = .data[[value]]),
      size = point_size, shape = 21, fill = "white", stroke = 0.6
    ) +
    kodom_value_scale("color", colors, color_breaks,
                      name = value, discretize = discretize)

  if (!is.null(facet_rows)) {
    p <- p + ggplot2::facet_grid(
      stats::as.formula(paste(facet_rows, "~ .")),
      scales = "free_y", space = "free_y"
    )
  }

  if (!is.null(prep$y_labels)) {
    p <- p + ggplot2::scale_y_discrete(labels = prep$y_labels)
  }

  p + theme_kodom() +
    ggplot2::labs(x = time, y = "") +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(barwidth = 15, barheight = 0.75)
    )
}
