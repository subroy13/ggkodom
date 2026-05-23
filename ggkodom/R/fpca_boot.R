#### CLUSTER BOOTSTRAP FOR FPCA + SIGN ALIGNMENT ####

#' Subject-level (cluster) bootstrap for FPCA
#'
#' Resamples \emph{subjects} (not observations) with replacement and
#' refits an FPCA model on each resampled cohort.  This is the
#' appropriate bootstrap for sparse / irregular longitudinal data: it
#' preserves within-subject correlation, which a naive observation-level
#' bootstrap would destroy and give falsely tight uncertainty bands.
#'
#' The user supplies \code{refit_fn} — a function that takes a long-format
#' data frame and returns an FPCA fit object.  This keeps the helper
#' backend-agnostic; you choose \code{fdapace::FPCA} or
#' \code{refund::fpca.sc}.
#'
#' Bootstrap-resampled subjects are re-labelled (\code{id_b1}, \code{id_b2},
#' ...) so duplicate draws are treated as independent "patients" by the
#' refit.
#'
#' @param data Long-format data with one row per observation.
#' @param id,time,value Column names (strings).
#' @param B Number of bootstrap replicates (default 50).
#' @param refit_fn A \code{function(data)} that returns an FPCA fit object.
#'   Required.
#' @param seed Random seed.
#' @return A list of length \code{B} of FPCA fits.
#' @export
#' @examples
#' \dontrun{
#'   library(fdapace)
#'   refit <- function(d) {
#'     FPCA(Ly = split(d$value, d$id),
#'          Lt = split(d$time,  d$id),
#'          optns = list(dataType = "Sparse", methodMuCovEst = "smooth"))
#'   }
#'   boots <- fpca_boot_subjects(df, "id", "time", "value",
#'                               B = 50, refit_fn = refit, seed = 1)
#'   kodom_components(fp, boot_fits = boots)
#' }
fpca_boot_subjects <- function(data, id = "id", time = "time", value = "value",
                               B = 50, refit_fn, seed = 1) {
  if (missing(refit_fn) || !is.function(refit_fn)) {
    stop("refit_fn must be a function(data) -> FPCA fit.", call. = FALSE)
  }
  check_columns(data, c(id, time, value))
  set.seed(seed)
  ids_all <- unique(data[[id]])

  out <- vector("list", B)
  for (b in seq_len(B)) {
    samp <- sample(ids_all, replace = TRUE)
    long <- do.call(rbind, lapply(seq_along(samp), function(i) {
      sub <- data[data[[id]] == samp[i], , drop = FALSE]
      sub[[id]] <- paste0(samp[i], "_b", i)
      sub
    }))
    out[[b]] <- tryCatch(refit_fn(long),
                         error = function(e) {
                           warning("Bootstrap replicate ", b, " failed: ",
                                   conditionMessage(e), call. = FALSE)
                           NULL
                         })
  }
  out[!vapply(out, is.null, logical(1))]
}


#' Align eigenfunction signs against a reference fit
#'
#' FPCA eigenfunctions are identified only up to sign.  Across bootstrap
#' replicates the same component can flip sign arbitrarily, which would
#' make a bootstrap envelope look bimodal.  This helper flips the sign
#' of each \code{phi_k} in \code{fit} so that
#' \eqn{\langle \phi_k^{\text{boot}}, \phi_k^{\text{ref}} \rangle > 0}.
#'
#' Assumes both fits live on the same time grid (the standard case when
#' \code{refit_fn} reuses the same FPCA call).  Different grids would
#' need interpolation, which we do not attempt here.
#'
#' @param fit FPCA fit (refund \code{fpca}/\code{fpca.sc} or fdapace
#'   \code{FPCA}).
#' @param ref_fit Reference fit of the same class.
#' @param K Number of components to align (default = all available).
#' @return The fit object with sign-flipped \code{efunctions} / \code{phi}
#'   and (if present) \code{scores} / \code{xiEst}.
#' @export
fpca_align_sign <- function(fit, ref_fit, K = NULL) {
  is_refund <- inherits(fit,     c("fpca", "fpca.sc"))
  is_pace   <- inherits(fit,     "FPCA")
  if (!(is_refund || is_pace)) stop("fit class not recognized.", call. = FALSE)
  if (is_refund != inherits(ref_fit, c("fpca", "fpca.sc")) &&
      is_pace   != inherits(ref_fit, "FPCA")) {
    warning("fit and ref_fit are different FPCA backends; alignment may be wrong.",
            call. = FALSE)
  }

  phi_slot   <- if (is_refund) "efunctions" else "phi"
  score_slot <- if (is_refund) "scores"     else "xiEst"

  phi_b   <- fit[[phi_slot]]
  phi_ref <- ref_fit[[phi_slot]]
  K_avail <- min(ncol(phi_b), ncol(phi_ref))
  if (is.null(K)) K <- K_avail
  K <- min(K, K_avail)

  for (k in seq_len(K)) {
    if (sum(phi_b[, k] * phi_ref[, k]) < 0) {
      phi_b[, k] <- -phi_b[, k]
      if (!is.null(fit[[score_slot]])) {
        fit[[score_slot]][, k] <- -fit[[score_slot]][, k]
      }
    }
  }
  fit[[phi_slot]] <- phi_b
  fit
}
