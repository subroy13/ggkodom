#### CLUSTER SCORES FROM FPCA ####

#' Cluster subjects in FPC score space
#'
#' Runs k-means or Ward hierarchical clustering on the subject-level
#' FPC score matrix and returns a named factor you can pass directly to
#' \code{kodom_scores(groups = ...)}, \code{kodom_swimlane(outcome = ...)},
#' or any facet/color argument in the package.
#'
#' Clustering functional data via FPC scores is a standard and
#' reasonable workflow when the leading few PCs carry most of the
#' variance: k-means on \eqn{\xi_1, \ldots, \xi_{n_{pcs}}} is (up to the
#' low-rank truncation) clustering on \eqn{L^2} distance between the
#' reconstructed curves.
#'
#' @param fit An FPCA fit (\code{refund} or \code{fdapace}).
#' @param k Number of clusters (default 3).
#' @param n_pcs Number of PCs to use.  \code{NULL} (default) = all
#'   retained.  Set to a small integer to cluster on leading components
#'   only.
#' @param method \code{"kmeans"} (default) or \code{"hclust"} (Ward.D2).
#' @param ids Subject ids in score-row order; auto-detected when NULL.
#' @param seed Random seed for k-means.
#' @return A named factor of length \eqn{n}.
#' @export
#' @examples
#' \dontrun{
#'   cl <- cluster_scores(fp, k = 3)
#'   kodom_scores(fp, groups = cl)
#'   # overlay onto swimlane: cluster as the "outcome" channel
#'   df$cluster <- cl[as.character(df$id)]
#'   kodom_swimlane(df, facet_rows = "cluster",
#'                  color_breaks = c(5.5, 7, 8, 10))
#' }
cluster_scores <- function(fit, k = 3, n_pcs = NULL,
                           method = c("kmeans", "hclust"),
                           ids = NULL, seed = 1) {
  method <- match.arg(method)

  scores <- fpca_scores_matrix(fit)
  if (is.null(ids)) ids <- fpca_ids(fit)
  if (length(ids) != nrow(scores)) {
    stop("length(ids) must equal nrow(scores) (", nrow(scores), ").",
         call. = FALSE)
  }
  K_avail <- ncol(scores)
  if (is.null(n_pcs)) n_pcs <- K_avail
  n_pcs <- min(n_pcs, K_avail)
  X <- scores[, seq_len(n_pcs), drop = FALSE]

  cl <- switch(method,
    kmeans = {
      set.seed(seed)
      stats::kmeans(X, centers = k, nstart = 10L)$cluster
    },
    hclust = {
      d <- stats::dist(X)
      h <- stats::hclust(d, method = "ward.D2")
      stats::cutree(h, k = k)
    }
  )

  factor(stats::setNames(cl, as.character(ids)),
         levels = sort(unique(cl)))
}
