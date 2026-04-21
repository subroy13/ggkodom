#### KODOM_SMOOTH — ONE-CALL SMOOTHER + REFIT_FN BUNDLE ####

#' Convenience smoother for sparse longitudinal data
#'
#' Wraps the three usable smoothers — \code{fdapace::FPCA},
#' \code{lme4::lmer}, and \code{mgcv::gam} — behind one call, so you
#' don't have to remember the boilerplate.  Returns the underlying fit
#' AND a \code{refit_fn} closure suitable for direct use with
#' \code{\link{fpca_boot_subjects}} (or any cluster-bootstrap loop).
#'
#' Defaults are deliberately minimal:
#' \describe{
#'   \item{\code{"fpca"}}{\code{fdapace::FPCA(Ly, Lt, optns = list(dataType = "Sparse"))}.}
#'   \item{\code{"lmer"}}{\code{lme4::lmer(value ~ time + (1 + time | id))}.}
#'   \item{\code{"gam"}}{\code{mgcv::gam(value ~ s(time, id, bs = "fs", k = 4))} with \code{id} coerced to factor.}
#' }
#' Override any of these by passing \code{formula =} (lmer/gam) or
#' \code{optns =} (fpca), or any extra method arguments through \code{...}.
#'
#' @param data Long-format data frame.
#' @param method One of \code{"fpca"}, \code{"lmer"}, \code{"gam"}.
#' @param id,time,value Column names (strings).
#' @param formula Optional formula override (lmer / gam only).
#' @param optns Optional named list for \code{fdapace::FPCA(optns=)}
#'   (fpca only).
#' @param k Spline basis dimension for the gam default formula
#'   (default 4; ignored if \code{formula} is supplied).
#' @param ... Passed to the underlying fitter.
#' @return A list with components:
#'   \describe{
#'     \item{fit}{The fitted object (\code{FPCA}, \code{lmerMod}, or
#'       \code{gam}).}
#'     \item{refit_fn}{\code{function(data) -> fit} — refits the same
#'       model on a new long-format data frame.  Pass to
#'       \code{\link{fpca_boot_subjects}} for FPCA bootstrap envelopes.}
#'     \item{method}{The method string.}
#'     \item{call_args}{The arguments captured for refitting.}
#'   }
#' @export
#' @examples
#' \dontrun{
#'   sm    <- kodom_smooth(df, method = "fpca")
#'   boots <- fpca_boot_subjects(df, B = 50, refit_fn = sm$refit_fn)
#'   kodom_components(sm$fit, boot_fits = boots)
#'
#'   sm2 <- kodom_smooth(df, method = "lmer")
#'   kodom_fit(sm2$fit, id = "id", time = "time")
#'   kodom_residuals(sm2$fit, df)
#' }
kodom_smooth <- function(data,
                         method = c("fpca", "lmer", "gam"),
                         id = "id", time = "time", value = "value",
                         formula = NULL, optns = NULL, k = 4, ...) {
  method <- match.arg(method)
  check_columns(data, c(id, time, value))
  extra <- list(...)

  switch(method,
    fpca = smooth_fpca(data, id, time, value, optns, extra),
    lmer = smooth_lmer(data, id, time, value, formula, extra),
    gam  = smooth_gam (data, id, time, value, formula, k, extra)
  )
}


#### INTERNAL FITTERS ####

#' @keywords internal
smooth_fpca <- function(data, id, time, value, optns, extra) {
  if (!requireNamespace("fdapace", quietly = TRUE)) {
    stop("Install 'fdapace' for method = 'fpca'.", call. = FALSE)
  }
  optns <- optns %||% list(dataType = "Sparse", verbose = FALSE)

  Ly <- split(as.numeric(data[[value]]), data[[id]])
  Lt <- split(as.numeric(data[[time]]),  data[[id]])
  fit <- do.call(fdapace::FPCA,
                 c(list(Ly = Ly, Lt = Lt, optns = optns), extra))

  refit_fn <- function(d) {
    Ly <- split(as.numeric(d[[value]]), d[[id]])
    Lt <- split(as.numeric(d[[time]]),  d[[id]])
    do.call(fdapace::FPCA,
            c(list(Ly = Ly, Lt = Lt, optns = optns), extra))
  }

  list(fit = fit, refit_fn = refit_fn, method = "fpca",
       call_args = list(id = id, time = time, value = value, optns = optns))
}


#' @keywords internal
smooth_lmer <- function(data, id, time, value, formula, extra) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Install 'lme4' for method = 'lmer'.", call. = FALSE)
  }
  if (is.null(formula)) {
    formula <- stats::as.formula(
      sprintf("%s ~ %s + (1 + %s | %s)", value, time, time, id)
    )
  }
  fit <- do.call(lme4::lmer, c(list(formula = formula, data = data), extra))

  refit_fn <- function(d) {
    do.call(lme4::lmer, c(list(formula = formula, data = d), extra))
  }

  list(fit = fit, refit_fn = refit_fn, method = "lmer",
       call_args = list(id = id, time = time, value = value, formula = formula))
}


#' @keywords internal
smooth_gam <- function(data, id, time, value, formula, k, extra) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Install 'mgcv' for method = 'gam'.", call. = FALSE)
  }
  if (is.null(formula)) {
    formula <- stats::as.formula(
      sprintf("%s ~ s(%s, %s, bs = 'fs', k = %d)", value, time, id, k)
    )
  }
  data[[id]] <- factor(data[[id]])
  fit <- do.call(mgcv::gam, c(list(formula = formula, data = data), extra))

  refit_fn <- function(d) {
    d[[id]] <- factor(d[[id]])
    do.call(mgcv::gam, c(list(formula = formula, data = d), extra))
  }

  list(fit = fit, refit_fn = refit_fn, method = "gam",
       call_args = list(id = id, time = time, value = value, formula = formula))
}
