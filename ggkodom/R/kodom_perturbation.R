#### KODOM_PERTURBATION -- RAMSAY-SILVERMAN PLUS/MINUS PLOT ####

#' Mean +/- c * sqrt(lambda_k) * phi_k(t) perturbation plot
#'
#' For each functional principal component, plots the mean function
#' \eqn{\mu(t)} together with \eqn{\mu(t) \pm c \sqrt{\lambda_k}\phi_k(t)}.
#' This is the third standard FPCA visualization
#' (cf.\ Ramsay & Silverman): it shows what each component physically
#' \emph{does} to the mean curve when the corresponding score is moved
#' \code{sd_mult} standard deviations away from zero.
#'
#' Methods:
#' \itemize{
#'   \item \code{refund::fpca.sc} (class \code{"fpca"}/\code{"fpca.sc"})
#'   \item \code{fdapace::FPCA}   (class \code{"FPCA"})
#' }
#'
#' @param fit An FPCA fit object.
#' @param K Number of components to plot (default = all).
#' @param sd_mult \eqn{c} in the formula above (default 2, i.e.\ a 2-SD
#'   perturbation in score space).
#' @param show_pve Annotate each panel with proportion of variance
#'   explained (default \code{TRUE}).
#' @param line_width Line width (default 1).
#' @param colors Length-3 color vector for
#'   (\eqn{\mu - c\sqrt{\lambda}\phi}, \eqn{\mu}, \eqn{\mu + c\sqrt{\lambda}\phi}).
#'   Default = teal / grey / red, consistent with \code{kodom_colors()}.
#' @param ... Passed to method.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' \dontrun{
#'   library(fdapace)
#'   Ly <- split(df$value, df$id); Lt <- split(df$time, df$id)
#'   fp <- FPCA(Ly = Ly, Lt = Lt, optns = list(dataType = "Sparse"))
#'   kodom_perturbation(fp)                # one panel per PC, c = 2
#'   kodom_perturbation(fp, K = 2, sd_mult = 1)
#' }
kodom_perturbation <- function(fit, ...) UseMethod("kodom_perturbation")

#' @export
kodom_perturbation.default <- function(fit, ...) {
  stop("No kodom_perturbation() method for class ",
       paste(class(fit), collapse = "/"), ".", call. = FALSE)
}

#' @rdname kodom_perturbation
#' @export
kodom_perturbation.fpca    <- function(fit, ...) kodom_perturbation_impl(fit, ...)

#' @rdname kodom_perturbation
#' @export
kodom_perturbation.fpca.sc <- function(fit, ...) kodom_perturbation_impl(fit, ...)

#' @rdname kodom_perturbation
#' @export
kodom_perturbation.FPCA    <- function(fit, K = NULL, sd_mult = 2,
                                       show_pve = TRUE, line_width = 1,
                                       colors = NULL, ...) {
  kodom_perturbation_impl(fit, K = K, sd_mult = sd_mult, show_pve = show_pve,
                          line_width = line_width, colors = colors, ...)
}


#### SHARED IMPLEMENTATION ####

#' @keywords internal
kodom_perturbation_impl <- function(fit, K = NULL, sd_mult = 2,
                                    show_pve = TRUE, line_width = 1,
                                    colors = NULL, ...) {
  ext <- fpca_extract(fit)
  if (is.null(ext$mu) || is.null(ext$phi) || is.null(ext$lambdas)) {
    stop("fit needs mu / phi / lambdas -- for refund, refit with var = TRUE.",
         call. = FALSE)
  }
  draw_perturbation(mu = ext$mu, phi = ext$phi, lambdas = ext$lambdas,
                    args = ext$args, K = K, sd_mult = sd_mult,
                    show_pve = show_pve, line_width = line_width,
                    colors = colors)
}


#' @keywords internal
draw_perturbation <- function(mu, phi, lambdas, args,
                              K = NULL, sd_mult = 2,
                              show_pve = TRUE, line_width = 1,
                              colors = NULL) {
  K_avail <- ncol(phi)
  if (is.null(K)) K <- K_avail
  K <- min(K, K_avail)

  pve <- lambdas / sum(lambdas)

  if (is.null(colors)) {
    anchors <- kodom_colors()        # teal, gold, red
    colors  <- c(anchors[1L], "grey30", anchors[3L])
  }
  if (length(colors) != 3L) {
    stop("colors must be length 3 (minus, mean, plus).", call. = FALSE)
  }

  long_list <- lapply(seq_len(K), function(k) {
    delta <- sd_mult * sqrt(lambdas[k]) * phi[, k]
    rbind(
      data.frame(pc = k, time = args, value = mu - delta, kind = "minus"),
      data.frame(pc = k, time = args, value = mu,         kind = "mean"),
      data.frame(pc = k, time = args, value = mu + delta, kind = "plus")
    )
  })
  long_df <- do.call(rbind, long_list)

  long_df$pc_label <- pc_label_factor(long_df$pc, pve, show_pve)
  long_df$kind     <- factor(long_df$kind, levels = c("minus", "mean", "plus"))

  legend_labels <- c(
    minus = sprintf("\u03bc \u2212 %g\u221a\u03bb\u00b7\u03c6", sd_mult),
    mean  = "\u03bc(t)",
    plus  = sprintf("\u03bc + %g\u221a\u03bb\u00b7\u03c6", sd_mult)
  )

  ggplot2::ggplot(long_df,
      ggplot2::aes(x = .data$time, y = .data$value,
                   color = .data$kind, group = .data$kind)) +
    ggplot2::geom_line(linewidth = line_width) +
    ggplot2::scale_color_manual(
      values = stats::setNames(colors, c("minus", "mean", "plus")),
      labels = legend_labels, name = NULL
    ) +
    ggplot2::facet_wrap(~ pc_label, ncol = 1, scales = "free_y",
                        strip.position = "right") +
    ggplot2::labs(x = "time", y = "value") +
    theme_kodom() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0))
}
