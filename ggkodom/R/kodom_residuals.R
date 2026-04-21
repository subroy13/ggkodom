#### KODOM_RESIDUALS — RAW MINUS FIT, AT OBSERVATION TIMES ####

#' Swimlane of residuals (observed - predicted)
#'
#' Predicts the supplied fit at each subject's actual observation times,
#' then plots a swimlane colored by the residual on a divergent palette
#' centered at zero.  The "what did the model miss" view.
#'
#' Methods are provided for the same fit classes as \code{\link{kodom_fit}}:
#' \itemize{
#'   \item \code{lme4::lmer}, \code{lme4::glmer}
#'   \item \code{mgcv::gam}
#'   \item \code{refund::fpca.sc}, \code{fdapace::FPCA}
#' }
#'
#' For FPCA fits the reconstruction lives on \code{fit$workGrid} (or
#' \code{fit$argvals}); residuals at observed times are obtained by
#' linear interpolation per subject.
#'
#' @param fit A fitted model object.
#' @param data Long-format data frame with the original observations
#'   (\code{id}, \code{time}, \code{value}).  For lmer/gam, defaults to
#'   the model frame stored in \code{fit}.
#' @param id,time,value Column names (strings).
#' @param signed_breaks Numeric vector of cut points for the divergent
#'   color scale (default \code{c(-2, 0, 2)}).  Centered at zero.
#' @param colors Length-3 colors for (negative, zero, positive)
#'   residuals.  Default = blue / grey / red.
#' @param discretize \code{FALSE} (default) = smooth divergent gradient;
#'   \code{TRUE} = step bands at \code{signed_breaks}.
#' @param n_sample,sort_by,point_size,line_width,alpha,seed,outcome,outcome_label,facet_rows,facet_cols
#'   See \code{\link{kodom_swimlane}}.
#' @param ... Passed to method.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' \dontrun{
#'   library(lme4)
#'   m <- lmer(value ~ time + (1 + time | id), data = df)
#'   kodom_residuals(m, df)
#'
#'   library(fdapace)
#'   Ly <- split(df$value, df$id); Lt <- split(df$time, df$id)
#'   fp <- FPCA(Ly = Ly, Lt = Lt, optns = list(dataType = "Sparse"))
#'   kodom_residuals(fp, df)
#' }
kodom_residuals <- function(fit, ...) UseMethod("kodom_residuals")

#' @export
kodom_residuals.default <- function(fit, ...) {
  stop("No kodom_residuals() method for class ",
       paste(class(fit), collapse = "/"),
       ". Implement kodom_residuals.<class>().", call. = FALSE)
}


#### LMER / GLMER ####

#' @rdname kodom_residuals
#' @export
kodom_residuals.lmerMod <- function(fit, data = NULL,
                                    id = "id", time = "time", value = NULL,
                                    ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required.", call. = FALSE)
  }
  if (is.null(data))  data  <- fit@frame
  if (is.null(value)) value <- response_name(stats::formula(fit))
  pred <- stats::predict(fit, newdata = data, re.form = NULL,
                         allow.new.levels = TRUE)
  draw_residual_swimlane(data, pred, id, time, value, ...)
}

#' @rdname kodom_residuals
#' @export
kodom_residuals.glmerMod <- function(fit, data = NULL,
                                     id = "id", time = "time", value = NULL,
                                     ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required.", call. = FALSE)
  }
  if (is.null(data))  data  <- fit@frame
  if (is.null(value)) value <- response_name(stats::formula(fit))
  pred <- stats::predict(fit, newdata = data, re.form = NULL,
                         type = "response", allow.new.levels = TRUE)
  draw_residual_swimlane(data, pred, id, time, value, ...)
}


#### GAM ####

#' @rdname kodom_residuals
#' @export
kodom_residuals.gam <- function(fit, data = NULL,
                                id = "id", time = "time", value = NULL,
                                type = "response", ...) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' required.", call. = FALSE)
  }
  if (is.null(data))  data  <- fit$model
  if (is.null(value)) value <- response_name(stats::formula(fit))
  if (is.factor(fit$model[[id]])) {
    data[[id]] <- factor(data[[id]], levels = levels(fit$model[[id]]))
  }
  pred <- as.numeric(stats::predict(fit, newdata = data, type = type))
  draw_residual_swimlane(data, pred, id, time, value, ...)
}


#### FPCA METHODS ####

#' @rdname kodom_residuals
#' @export
kodom_residuals.fpca <- function(fit, data,
                                 id = "id", time = "time", value = "value",
                                 ...) {
  pred <- fpca_predict_at_obs(fit, data, id, time)
  draw_residual_swimlane(data, pred, id, time, value, ...)
}

#' @rdname kodom_residuals
#' @export
kodom_residuals.fpca.sc <- function(fit, ...) kodom_residuals.fpca(fit, ...)

#' @rdname kodom_residuals
#' @export
kodom_residuals.FPCA <- function(fit, data,
                                 id = "id", time = "time", value = "value",
                                 ...) {
  pred <- fpca_predict_at_obs(fit, data, id, time)
  draw_residual_swimlane(data, pred, id, time, value, ...)
}


#### INTERNAL: PREDICT AN FPCA FIT AT OBSERVATION TIMES ####

#' Linearly interpolate an FPCA reconstruction onto each subject's
#' observed time points.
#'
#' For \code{refund::fpca.sc} uses \code{fit$Yhat}; for
#' \code{fdapace::FPCA} reconstructs via \eqn{\mu + \xi \phi^T}.  The
#' resulting per-subject curve is then \code{stats::approx}-ed onto the
#' observed times in \code{data}.
#' @keywords internal
fpca_predict_at_obs <- function(fit, data, id, time) {
  ext <- fpca_extract(fit)
  args <- ext$args

  if (inherits(fit, c("fpca", "fpca.sc"))) {
    Yhat <- fit$Yhat
    if (is.null(Yhat)) {
      stop("fit$Yhat is missing; refit refund::fpca.sc(..., var = TRUE).",
           call. = FALSE)
    }
    fit_ids <- if (!is.null(rownames(Yhat))) rownames(Yhat) else as.character(seq_len(nrow(Yhat)))
  } else {
    xi  <- ext$scores
    phi <- ext$phi
    Yhat <- sweep(xi %*% t(phi), 2L, ext$mu, "+")
    fit_ids <- ext$ids
  }

  data_ids <- as.character(data[[id]])
  out <- rep(NA_real_, nrow(data))
  for (sid in unique(data_ids)) {
    row <- match(sid, fit_ids)
    if (is.na(row)) next
    rows_data <- which(data_ids == sid)
    out[rows_data] <- stats::approx(args, Yhat[row, ], xout = data[[time]][rows_data],
                                    rule = 2L)$y
  }
  out
}


#### INTERNAL: DRAW THE RESIDUAL SWIMLANE ####

#' @keywords internal
draw_residual_swimlane <- function(data, pred, id, time, value,
                                   signed_breaks = c(-2, 0, 2),
                                   colors = c("#2c7fb8", "grey92", "#d7301f"),
                                   discretize = FALSE,
                                   n_sample = 50,
                                   sort_by = NULL,
                                   outcome = NULL, outcome_label = "*",
                                   point_size = 3, line_width = 0.8, alpha = 0.7,
                                   facet_rows = NULL, facet_cols = NULL,
                                   seed = 123) {
  if (length(colors) != 3L) {
    stop("colors must be length 3 (negative, zero, positive).", call. = FALSE)
  }
  resid_col <- paste0(value, "_resid")
  data[[resid_col]] <- as.numeric(data[[value]]) - as.numeric(pred)

  ## default sort: by mean residual (which subjects the model misses on)
  if (is.null(sort_by)) sort_by <- "mean"

  prep <- prep_trajectory_data(
    data, id, time, resid_col,
    n_sample = n_sample, sort_by = sort_by,
    outcome = outcome, outcome_label = outcome_label,
    seed = seed
  )
  df   <- prep$data
  cols <- prep$cols

  scale_fn <- if (isTRUE(discretize)) {
    ggplot2::scale_color_stepsn(colors = colors, breaks = signed_breaks,
                                name = "residual", na.value = "grey90")
  } else {
    ggplot2::scale_color_gradientn(colors = colors,
                                   values = scales::rescale(signed_breaks),
                                   name = "residual", na.value = "grey90",
                                   limits = range(signed_breaks),
                                   oob = scales::squish)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(
      x     = .data[[cols$time]],
      y     = .data[[cols$id]],
      group = .data[[cols$id]]
    )) +
    ggplot2::geom_path(
      ggplot2::aes(color = .data[[cols$value]]),
      linewidth = line_width, alpha = alpha
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data[[cols$value]]),
      size = point_size
    ) +
    scale_fn

  if (!is.null(prep$y_labels)) {
    p <- p + ggplot2::scale_y_discrete(labels = prep$y_labels)
  }
  facet <- build_facet(facet_rows, facet_cols)
  if (!is.null(facet)) p <- p + facet

  p + theme_kodom() +
    ggplot2::labs(x = time, y = "") +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(barwidth = 15, barheight = 0.75)
    )
}
