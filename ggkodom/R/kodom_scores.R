#### KODOM_SCORES — FPC SCORE SCATTER ####

#' Scatter plot of FPC scores
#'
#' Visualize subjects in the score space of an FPCA fit
#' (\eqn{\xi_1, \xi_2, \ldots}).  Useful for spotting clusters,
#' phenotypes, or outliers in the functional decomposition.
#'
#' @param fit An FPCA fit (\code{refund::fpca.sc} or
#'   \code{fdapace::FPCA}).
#' @param pcs Length-2 integer vector — which PCs go on the x and y axes
#'   (default \code{c(1, 2)}).
#' @param groups Optional vector (length = number of subjects).
#'   Numeric → kodom continuous gradient; factor/character → discrete
#'   palette.
#' @param ids Subject ids in row order of the score matrix.
#'   Defaults: \code{rownames(fit$scores)} for refund;
#'   \code{names(fit$inputData$Ly)} for fdapace; integer index otherwise.
#' @param point_size Point size (default 2.5).
#' @param alpha Point transparency (default 0.85).
#' @param show_pve Annotate axes with proportion of variance explained.
#' @param colors Optional colors (gradient anchors for numeric groups,
#'   one per level for discrete).
#' @param ... Unused.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' \dontrun{
#'   library(fdapace)
#'   Ly <- split(df$value, df$id); Lt <- split(df$time, df$id)
#'   fp <- FPCA(Ly = Ly, Lt = Lt, optns = list(dataType = "Sparse"))
#'   kodom_scores(fp)
#'   kodom_scores(fp, groups = sapply(Ly, mean))   # color by mean A1c
#' }
kodom_scores <- function(fit, ...) UseMethod("kodom_scores")

#' @export
kodom_scores.default <- function(fit, ...) {
  stop("No kodom_scores() method for class ",
       paste(class(fit), collapse = "/"), ".", call. = FALSE)
}

#' @rdname kodom_scores
#' @export
kodom_scores.fpca <- function(fit, pcs = c(1, 2), groups = NULL, ids = NULL,
                              point_size = 2.5, alpha = 0.85, show_pve = TRUE,
                              colors = NULL, ...) {
  draw_scores(fpca_scores_matrix(fit), fpca_evalues(fit),
              ids = if (is.null(ids)) fpca_ids(fit) else ids,
              pcs = pcs, groups = groups, point_size = point_size,
              alpha = alpha, show_pve = show_pve, colors = colors)
}

#' @rdname kodom_scores
#' @export
kodom_scores.fpca.sc <- function(fit, ...) kodom_scores.fpca(fit, ...)

#' @rdname kodom_scores
#' @export
kodom_scores.FPCA <- function(fit, pcs = c(1, 2), groups = NULL, ids = NULL,
                              point_size = 2.5, alpha = 0.85, show_pve = TRUE,
                              colors = NULL, ...) {
  draw_scores(fpca_scores_matrix(fit), fpca_evalues(fit),
              ids = if (is.null(ids)) fpca_ids(fit) else ids,
              pcs = pcs, groups = groups, point_size = point_size,
              alpha = alpha, show_pve = show_pve, colors = colors)
}


#### SORT-KEY HELPER ####

#' Build a named-numeric sort key from FPC scores
#'
#' Returns a vector suitable to pass as \code{sort_by} in
#' \code{\link{kodom_swimlane}}, ordering subjects by their score on a
#' chosen functional principal component.
#'
#' @param fit An FPCA fit.
#' @param pc Component index (default 1).
#' @param ids Subject ids in score-row order.  Auto-detected when NULL.
#' @return A named numeric vector.
#' @export
#' @examples
#' \dontrun{
#'   key <- kodom_sort_scores(fp, pc = 1, ids = names(Ly))
#'   kodom_swimlane(df, sort_by = key)         # lanes ordered by PC1 score
#' }
kodom_sort_scores <- function(fit, pc = 1, ids = NULL) {
  scores <- fpca_scores_matrix(fit)
  if (is.null(ids)) ids <- fpca_ids(fit)
  if (length(ids) != nrow(scores)) {
    stop("length(ids) must equal nrow(scores) (", nrow(scores), ").",
         call. = FALSE)
  }
  if (pc > ncol(scores)) {
    stop("Requested PC ", pc, " but only ", ncol(scores), " available.",
         call. = FALSE)
  }
  stats::setNames(scores[, pc], as.character(ids))
}


#### INTERNAL ####
##
## fpca_scores_matrix(), fpca_evalues(), fpca_ids() now live in
## R/fpca_helpers.R (thin wrappers around fpca_extract()).

#' @keywords internal
draw_scores <- function(scores, evs, ids, pcs, groups,
                        point_size, alpha, show_pve, colors) {
  if (length(pcs) != 2L) stop("pcs must be length 2.", call. = FALSE)
  if (max(pcs) > ncol(scores)) {
    stop("Requested PC ", max(pcs), " but only ", ncol(scores), " available.",
         call. = FALSE)
  }
  pve <- evs / sum(evs)

  df <- data.frame(
    id   = ids,
    pc_x = scores[, pcs[1L]],
    pc_y = scores[, pcs[2L]]
  )

  ax_label <- function(k) {
    if (show_pve) sprintf("PC %d  (%.1f%%)", k, 100 * pve[k])
    else          sprintf("PC %d", k)
  }

  p <- ggplot2::ggplot(df,
      ggplot2::aes(x = .data$pc_x, y = .data$pc_y)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey80", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, color = "grey80", linewidth = 0.3)

  if (!is.null(groups)) {
    if (length(groups) != nrow(scores)) {
      stop("length(groups) (", length(groups),
           ") must equal nrow(scores) (", nrow(scores), ").", call. = FALSE)
    }
    df$group <- groups
    if (is.numeric(groups)) {
      if (is.null(colors)) colors <- kodom_colors()
      p <- p + ggplot2::geom_point(data = df,
                ggplot2::aes(color = .data$group),
                size = point_size, alpha = alpha) +
        kodom_gradient_scale("color", colors, name = "group")
    } else {
      df$group <- factor(df$group)
      n_g <- nlevels(df$group)
      if (is.null(colors)) colors <- kodom_colors(max(n_g, 3L))[seq_len(n_g)]
      p <- p + ggplot2::geom_point(data = df,
                ggplot2::aes(color = .data$group),
                size = point_size, alpha = alpha) +
        ggplot2::scale_color_manual(values = colors, name = "group")
    }
  } else {
    p <- p + ggplot2::geom_point(size = point_size, alpha = alpha,
                                 color = kodom_colors()[1L])
  }

  p +
    ggplot2::labs(x = ax_label(pcs[1L]), y = ax_label(pcs[2L])) +
    theme_kodom()
}
