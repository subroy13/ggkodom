#### FPCA HELPERS (BACKEND-AGNOSTIC) ####
##
## A small adapter layer so the visualization functions don't need to
## branch on refund vs fdapace.  Add a new backend by writing one
## fpca_extract() branch — every viz function picks it up for free.

#' Extract everything a kodom FPCA viz function needs from a fit
#'
#' @param fit An FPCA fit (\code{refund::fpca}/\code{fpca.sc} or
#'   \code{fdapace::FPCA}).
#' @return A list:
#'   \describe{
#'     \item{mu}{Mean function on the working grid.}
#'     \item{phi}{Eigenfunction matrix (T x K).}
#'     \item{lambdas}{Eigenvalues (length K).}
#'     \item{args}{Working time grid (length T).}
#'     \item{scores}{Subject scores (n x K), or \code{NULL}.}
#'     \item{ids}{Subject ids in score-row order, character.}
#'     \item{phi_slot, score_slot}{Names of the eigenfunction / score
#'       slots in the original fit (used by bootstrap helpers).}
#'     \item{backend}{\code{"refund"} or \code{"fdapace"}.}
#'   }
#' @keywords internal
fpca_extract <- function(fit) {
  if (inherits(fit, c("fpca", "fpca.sc"))) {
    phi  <- fit$efunctions
    if (is.null(phi)) {
      stop("fit$efunctions is missing — this doesn't look like a refund FPCA fit.",
           call. = FALSE)
    }
    args <- if (!is.null(fit$argvals)) fit$argvals else seq_len(nrow(phi))
    ids  <- if (!is.null(rownames(fit$scores))) rownames(fit$scores)
            else as.character(seq_len(nrow(fit$scores) %||% 0L))
    return(list(
      mu = fit$mu, phi = phi, lambdas = fit$evalues, args = args,
      scores = fit$scores, ids = ids,
      phi_slot = "efunctions", score_slot = "scores",
      backend = "refund"
    ))
  }
  if (inherits(fit, "FPCA")) {
    ids <- if (!is.null(fit$inputData) && !is.null(names(fit$inputData$Ly)))
             names(fit$inputData$Ly)
           else as.character(seq_len(nrow(fit$xiEst)))
    return(list(
      mu = fit$mu, phi = fit$phi, lambdas = fit$lambda, args = fit$workGrid,
      scores = fit$xiEst, ids = ids,
      phi_slot = "phi", score_slot = "xiEst",
      backend = "fdapace"
    ))
  }
  stop("Not a recognized FPCA fit (refund::fpca / fdapace::FPCA).",
       call. = FALSE)
}


#' Build a single PC facet label
#' @param k PC index.  Use 0 for the mean panel.
#' @param pve Numeric vector of proportions of variance explained.
#' @param show_pve Annotate with PVE? (default \code{TRUE}).
#' @keywords internal
make_pc_label <- function(k, pve, show_pve = TRUE) {
  if (k == 0L) return("mu(t)")
  if (show_pve && length(pve) >= k && !is.na(pve[k])) {
    sprintf("PC %d (%.1f%%)", k, 100 * pve[k])
  } else {
    sprintf("PC %d", k)
  }
}


#' Build a factor of PC labels with sensible levels
#'
#' Levels follow PC index order (0 first if present, then 1, 2, ...),
#' so facets render in the natural order.
#' @param pcs Integer vector of pc indices, one per row.
#' @inheritParams make_pc_label
#' @keywords internal
pc_label_factor <- function(pcs, pve, show_pve = TRUE) {
  pc_levels    <- sort(unique(pcs))
  level_labels <- vapply(pc_levels, make_pc_label, character(1),
                         pve = pve, show_pve = show_pve)
  row_labels   <- vapply(pcs, make_pc_label, character(1),
                         pve = pve, show_pve = show_pve)
  factor(row_labels, levels = level_labels)
}


#' Tiny null-coalescing operator (avoids a rlang dependency for one use)
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a


#### BACK-COMPAT WRAPPERS (used by kodom_scores.R) ####

#' @keywords internal
fpca_scores_matrix <- function(fit) fpca_extract(fit)$scores

#' @keywords internal
fpca_evalues <- function(fit) fpca_extract(fit)$lambdas

#' @keywords internal
fpca_ids <- function(fit) fpca_extract(fit)$ids
