#### KODOM_FIT — VISUALIZE FITTED LONGITUDINAL/FUNCTIONAL MODELS ####

#' Visualize a fitted longitudinal model as a kodom-swimlane
#'
#' Generic that takes a fitted model object (e.g.\ from \code{lme4::lmer},
#' \code{mgcv::gam}, an FPCA fit, ...) and produces a swimlane plot of the
#' subject-specific fitted curves on a dense time grid.
#' Raw observations can be overlaid as points.
#'
#' Each method extracts (a) the per-subject prediction grid and (b) the
#' fitted value on that grid, then hands them to a shared drawer.
#' This separation lets new model classes be added by writing one
#' \code{kodom_fit.<class>()} method without touching the plotting code.
#'
#' @param fit A fitted model object.
#' @param ... Passed to method.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' \dontrun{
#'   # lme4
#'   set.seed(1)
#'   df <- data.frame(
#'     id   = factor(rep(1:30, each = 6)),
#'     time = rep(seq(0, 5, length.out = 6), 30)
#'   )
#'   df$y <- 5 + 0.3 * df$time +
#'           rnorm(30, 0, 0.6)[df$id] +
#'           rnorm(30, 0, 0.2)[df$id] * df$time +
#'           rnorm(nrow(df), 0, 0.5)
#'   m <- lme4::lmer(y ~ time + (1 + time | id), data = df)
#'   kodom_fit(m, id = "id", time = "time")
#'
#'   # mgcv hierarchical GAM (one smooth per subject)
#'   g <- mgcv::gam(y ~ s(time, id, bs = "fs", k = 4), data = df)
#'   kodom_fit(g, id = "id", time = "time")
#' }
kodom_fit <- function(fit, ...) UseMethod("kodom_fit")

#' @export
kodom_fit.default <- function(fit, ...) {
  stop("No kodom_fit() method for class ",
       paste(class(fit), collapse = "/"),
       ". Implement kodom_fit.<class>().", call. = FALSE)
}


#### LMER METHOD ####

#' @rdname kodom_fit
#' @param id Column name (string) for subject identifier.
#' @param time Column name (string) for the time variable.
#' @param value Column name (string) for the response.
#'   Defaults to the LHS of \code{formula(fit)}.
#' @param data Data frame with the original observations (used for the per-id
#'   time range and the optional raw-point overlay).
#'   Defaults to the model frame stored in \code{fit}.
#' @param newdata Optional caller-supplied prediction grid.
#'   When \code{NULL}, a per-id grid of length \code{n_grid} is built that
#'   spans each subject's observed time range; non-time covariates are
#'   filled from the first observation per subject.
#' @param n_grid Per-subject grid resolution (default 60).
#' @param show_points Overlay raw observations as points (default \code{TRUE}).
#' @param re_form Random-effect formula passed through to \code{predict()}.
#'   Default \code{NULL} = include all random effects (subject-specific curves).
#'   Use \code{NA} for the population-level (fixed-effect-only) prediction.
#' @param n_sample,sort_by,colors,color_breaks,discretize,point_size,line_width,alpha,seed
#'   See \code{\link{kodom_swimlane}}.
#' @export
kodom_fit.lmerMod <- function(fit, id, time, value = NULL,
                              data = NULL, newdata = NULL, n_grid = 60,
                              re_form = NULL, show_points = TRUE,
                              n_sample = 50, sort_by = "first",
                              colors = kodom_colors(), color_breaks = NULL,
                              discretize = FALSE,
                              point_size = 1.5, line_width = 0.8, alpha = 0.85,
                              seed = 123, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for kodom_fit.lmerMod().", call. = FALSE)
  }
  if (is.null(data))  data  <- fit@frame
  if (is.null(value)) value <- response_name(stats::formula(fit))

  grid_df <- if (is.null(newdata)) {
    build_subject_grid(data, id, time, n_grid)
  } else newdata

  grid_df[[value]] <- stats::predict(fit, newdata = grid_df, re.form = re_form,
                                     allow.new.levels = TRUE)

  draw_fit_swimlane(grid_df, raw_df = if (show_points) data else NULL,
                    id, time, value,
                    n_sample = n_sample, sort_by = sort_by,
                    colors = colors, color_breaks = color_breaks,
                    discretize = discretize,
                    point_size = point_size, line_width = line_width,
                    alpha = alpha, seed = seed)
}

#' @rdname kodom_fit
#' @export
kodom_fit.glmerMod <- function(fit, id, time, value = NULL,
                               data = NULL, newdata = NULL, n_grid = 60,
                               re_form = NULL, show_points = TRUE,
                               n_sample = 50, sort_by = "first",
                               colors = kodom_colors(), color_breaks = NULL,
                               discretize = FALSE,
                               point_size = 1.5, line_width = 0.8, alpha = 0.85,
                               seed = 123, ...) {
  ## same as lmerMod — predict() handles the inverse link via type = "response"
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for kodom_fit.glmerMod().", call. = FALSE)
  }
  if (is.null(data))  data  <- fit@frame
  if (is.null(value)) value <- response_name(stats::formula(fit))

  grid_df <- if (is.null(newdata)) {
    build_subject_grid(data, id, time, n_grid)
  } else newdata

  grid_df[[value]] <- stats::predict(fit, newdata = grid_df, re.form = re_form,
                                     type = "response", allow.new.levels = TRUE)

  draw_fit_swimlane(grid_df, raw_df = if (show_points) data else NULL,
                    id, time, value,
                    n_sample = n_sample, sort_by = sort_by,
                    colors = colors, color_breaks = color_breaks,
                    discretize = discretize,
                    point_size = point_size, line_width = line_width,
                    alpha = alpha, seed = seed)
}


#### GAM METHOD ####

#' @rdname kodom_fit
#' @param type Prediction scale for GAMs: \code{"response"} (default)
#'   or \code{"link"}.
#' @export
kodom_fit.gam <- function(fit, id, time, value = NULL,
                          data = NULL, newdata = NULL, n_grid = 60,
                          type = "response", show_points = TRUE,
                          n_sample = 50, sort_by = "first",
                          colors = kodom_colors(), color_breaks = NULL,
                          discretize = FALSE,
                          point_size = 1.5, line_width = 0.8, alpha = 0.85,
                          seed = 123, ...) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' required for kodom_fit.gam().", call. = FALSE)
  }
  if (is.null(data))  data  <- fit$model
  if (is.null(value)) value <- response_name(stats::formula(fit))

  grid_df <- if (is.null(newdata)) {
    build_subject_grid(data, id, time, n_grid)
  } else newdata

  ## ensure id is the same factor type as in fit (GAMs with bs="fs"/by= are picky)
  if (is.factor(data[[id]])) {
    grid_df[[id]] <- factor(grid_df[[id]], levels = levels(data[[id]]))
  }

  grid_df[[value]] <- as.numeric(stats::predict(fit, newdata = grid_df, type = type))

  draw_fit_swimlane(grid_df, raw_df = if (show_points) data else NULL,
                    id, time, value,
                    n_sample = n_sample, sort_by = sort_by,
                    colors = colors, color_breaks = color_breaks,
                    discretize = discretize,
                    point_size = point_size, line_width = line_width,
                    alpha = alpha, seed = seed)
}


#### FPCA METHODS ####

#' @rdname kodom_fit
#' @param ids Character/integer vector of subject identifiers, in row order
#'   of the FPCA reconstruction matrix. Used to align the fitted curves with
#'   subjects in \code{data}. Defaults: \code{rownames(fit$Yhat)} for
#'   \code{refund::fpca.sc}; \code{names(fit$inputData$Ly)} for
#'   \code{fdapace::FPCA}; integer index otherwise.
#' @export
kodom_fit.fpca <- function(fit, ids = NULL,
                           data = NULL, id = "id", time = "time", value = "value",
                           show_points = TRUE,
                           n_sample = 50, sort_by = "first",
                           colors = kodom_colors(), color_breaks = NULL,
                           discretize = FALSE,
                           point_size = 1.5, line_width = 0.8, alpha = 0.85,
                           seed = 123, ...) {
  Yhat <- fit$Yhat
  if (is.null(Yhat)) {
    stop("fit$Yhat is missing; refit with refund::fpca.sc(..., var = TRUE) ",
         "or pass an object that stores the reconstruction.", call. = FALSE)
  }
  argvals <- if (!is.null(fit$argvals)) fit$argvals else seq_len(ncol(Yhat))
  if (is.null(ids)) {
    ids <- if (!is.null(rownames(Yhat))) rownames(Yhat) else as.character(seq_len(nrow(Yhat)))
  }
  if (length(ids) != nrow(Yhat)) {
    stop("length(ids) must equal nrow(fit$Yhat) (", nrow(Yhat), ").",
         call. = FALSE)
  }

  grid_df <- fpca_long(Yhat, argvals, ids, id, time, value)

  raw_df <- if (show_points && !is.null(data)) data else NULL

  draw_fit_swimlane(grid_df, raw_df = raw_df,
                    id, time, value,
                    n_sample = n_sample, sort_by = sort_by,
                    colors = colors, color_breaks = color_breaks,
                    discretize = discretize,
                    point_size = point_size, line_width = line_width,
                    alpha = alpha, seed = seed)
}

## refund sometimes assigns class "fpca.sc"; alias.
#' @rdname kodom_fit
#' @export
kodom_fit.fpca.sc <- function(fit, ...) kodom_fit.fpca(fit, ...)

#' @rdname kodom_fit
#' @param K Number of FPCs to use in the reconstruction
#'   (\code{fdapace::FPCA} only). Defaults to all retained components.
#' @export
kodom_fit.FPCA <- function(fit, ids = NULL,
                           data = NULL, id = "id", time = "time", value = "value",
                           K = NULL, show_points = TRUE,
                           n_sample = 50, sort_by = "first",
                           colors = kodom_colors(), color_breaks = NULL,
                           discretize = FALSE,
                           point_size = 1.5, line_width = 0.8, alpha = 0.85,
                           seed = 123, ...) {
  ## reconstruction: mu(t) + sum_k xi_k phi_k(t), evaluated on workGrid
  K_avail <- ncol(fit$phi)
  if (is.null(K)) K <- K_avail
  K <- min(K, K_avail)

  xi  <- fit$xiEst[, seq_len(K), drop = FALSE]
  phi <- fit$phi[ , seq_len(K), drop = FALSE]
  recon <- sweep(xi %*% t(phi), 2L, fit$mu, "+")

  argvals <- fit$workGrid
  if (is.null(ids)) {
    ids <- if (!is.null(fit$inputData) && !is.null(names(fit$inputData$Ly))) {
      names(fit$inputData$Ly)
    } else as.character(seq_len(nrow(recon)))
  }
  if (length(ids) != nrow(recon)) {
    stop("length(ids) must equal nrow(reconstruction) (", nrow(recon), ").",
         call. = FALSE)
  }

  grid_df <- fpca_long(recon, argvals, ids, id, time, value)

  raw_df <- if (show_points && !is.null(data)) data else NULL

  draw_fit_swimlane(grid_df, raw_df = raw_df,
                    id, time, value,
                    n_sample = n_sample, sort_by = sort_by,
                    colors = colors, color_breaks = color_breaks,
                    discretize = discretize,
                    point_size = point_size, line_width = line_width,
                    alpha = alpha, seed = seed)
}


#### INTERNAL HELPERS ####

#' Reshape an N x T FPCA reconstruction matrix to long (id, time, value)
#' @keywords internal
fpca_long <- function(mat, argvals, ids, id, time, value) {
  N  <- nrow(mat)
  Tg <- ncol(mat)
  out <- data.frame(
    .id_tmp    = rep(ids,     times = Tg),
    .time_tmp  = rep(argvals, each  = N),
    .value_tmp = as.vector(mat),
    stringsAsFactors = FALSE
  )
  names(out) <- c(id, time, value)
  out
}


#' Response variable name from a model formula
#' @keywords internal
response_name <- function(form) {
  lhs <- form[[2L]]
  if (length(lhs) > 1L) {
    ## e.g.\ cbind(succ, fail) — fall back to deparse
    return(deparse(lhs))
  }
  as.character(lhs)
}

#' Build a per-subject prediction grid spanning each subject's observed
#' time range. Non-(id,time) covariates are filled from the first row per id.
#' @keywords internal
build_subject_grid <- function(data, id, time, n_grid) {
  check_columns(data, c(id, time))
  ids <- unique(data[[id]])
  other_vars <- setdiff(names(data), c(id, time))

  out_list <- lapply(ids, function(i) {
    sub <- data[data[[id]] == i, , drop = FALSE]
    sub <- sub[!is.na(sub[[time]]), , drop = FALSE]
    if (nrow(sub) < 2L) return(NULL)
    tr <- range(sub[[time]])
    if (diff(tr) <= 0) return(NULL)
    grid_t <- seq(tr[1L], tr[2L], length.out = n_grid)
    g <- data.frame(.id_tmp = i, .time_tmp = grid_t,
                    stringsAsFactors = FALSE)
    names(g) <- c(id, time)
    ## fill non-time covariates from the first observation
    for (v in other_vars) g[[v]] <- sub[[v]][1L]
    g
  })
  out <- do.call(rbind, out_list)
  if (is.null(out) || nrow(out) == 0L) {
    stop("No subject has >= 2 distinct time points; nothing to predict on.",
         call. = FALSE)
  }
  out
}

#' Shared swimlane drawer for fitted-curve outputs
#' @keywords internal
draw_fit_swimlane <- function(grid_df, raw_df = NULL,
                              id, time, value,
                              n_sample, sort_by,
                              colors, color_breaks, discretize = FALSE,
                              point_size, line_width, alpha, seed) {
  prep <- prep_trajectory_data(grid_df, id, time, value,
                               n_sample = n_sample, sort_by = sort_by,
                               seed = seed)
  df   <- prep$data
  cols <- prep$cols

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x     = .data[[cols$time]],
    y     = .data[[cols$id]],
    group = .data[[cols$id]]
  )) +
    ggplot2::geom_path(
      ggplot2::aes(color = .data[[cols$value]]),
      linewidth = line_width, alpha = alpha
    )

  if (!is.null(raw_df)) {
    raw_df <- raw_df[raw_df[[id]] %in% prep$id_order, , drop = FALSE]
    raw_df[[id]] <- factor(raw_df[[id]], levels = prep$id_order)
    raw_df[[time]]  <- as.numeric(raw_df[[time]])
    raw_df[[value]] <- as.numeric(raw_df[[value]])
    raw_df <- raw_df[stats::complete.cases(raw_df[, c(time, value)]), , drop = FALSE]
    p <- p + ggplot2::geom_point(
      data = raw_df,
      ggplot2::aes(x = .data[[time]], y = .data[[id]],
                   color = .data[[value]]),
      size = point_size
    )
  }

  p +
    kodom_value_scale("color", colors, color_breaks,
                      name = cols$value, discretize = discretize) +
    theme_kodom() +
    ggplot2::labs(x = cols$time, y = "") +
    ggplot2::guides(color = ggplot2::guide_colorbar(barwidth = 15,
                                                    barheight = 0.75))
}
