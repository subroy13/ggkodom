#### KODOM_COMPONENTS — VISUALIZE FPCA EIGENFUNCTIONS ####

#' Plot the eigenfunctions of an FPCA fit
#'
#' Each functional principal component is itself a curve
#' \eqn{\phi_k(t)}.  This generic visualizes the first \code{K} of them
#' as a small-multiples panel (default) or as overlaid lines.
#'
#' Methods are provided for:
#' \itemize{
#'   \item \code{refund::fpca.sc} (class \code{"fpca"} or \code{"fpca.sc"})
#'   \item \code{fdapace::FPCA}   (class \code{"FPCA"})
#' }
#'
#' @param fit An FPCA fit object.
#' @param K Number of components to plot.  \code{NULL} = all retained.
#' @param overlay \code{FALSE} (default) = one panel per PC, stacked
#'   vertically; \code{TRUE} = all PCs overlaid in a single panel.
#' @param show_pve Annotate each PC with its proportion of variance
#'   explained (default \code{TRUE}).
#' @param colors Color vector for the PCs.  Default = ramped
#'   \code{\link{kodom_colors}()} of length \code{K}.
#' @param line_width Line width (default 1).
#' @param show_mean Add a panel for the mean function \eqn{\mu(t)} at the
#'   top (default \code{FALSE}).  The mean isn't an eigenfunction but is
#'   often shown alongside for context.
#' @param boot_fits Optional list of bootstrap FPCA fits (e.g.\ from
#'   \code{\link{fpca_boot_subjects}}).  Each fit's eigenfunctions are
#'   sign-aligned to the main fit and drawn as light traces behind the
#'   main curve, giving a visual variability envelope.
#' @param boot_alpha Transparency for bootstrap traces (default 0.15).
#' @param ... Passed to method.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' \dontrun{
#'   library(fdapace)
#'   Ly <- split(df$value, df$id); Lt <- split(df$time, df$id)
#'   fp <- FPCA(Ly = Ly, Lt = Lt, optns = list(dataType = "Sparse"))
#'   kodom_components(fp)                          # 3 stacked panels
#'   kodom_components(fp, K = 2, overlay = TRUE)   # overlaid
#'   kodom_components(fp, show_mean = TRUE)        # with mu(t) panel
#' }
kodom_components <- function(fit, ...) UseMethod("kodom_components")

#' @export
kodom_components.default <- function(fit, ...) {
  stop("No kodom_components() method for class ",
       paste(class(fit), collapse = "/"), ".", call. = FALSE)
}

#' @rdname kodom_components
#' @export
kodom_components.fpca    <- function(fit, ...) kodom_components_impl(fit, ...)

#' @rdname kodom_components
#' @export
kodom_components.fpca.sc <- function(fit, ...) kodom_components_impl(fit, ...)

#' @rdname kodom_components
#' @export
kodom_components.FPCA    <- function(fit, K = NULL, overlay = FALSE,
                                     show_pve = TRUE, colors = NULL,
                                     line_width = 1, show_mean = FALSE,
                                     boot_fits = NULL, boot_alpha = 0.15,
                                     ...) {
  kodom_components_impl(fit, K = K, overlay = overlay, show_pve = show_pve,
                        colors = colors, line_width = line_width,
                        show_mean = show_mean, boot_fits = boot_fits,
                        boot_alpha = boot_alpha, ...)
}


#### SHARED IMPLEMENTATION ####

#' @keywords internal
kodom_components_impl <- function(fit, K = NULL, overlay = FALSE,
                                  show_pve = TRUE, colors = NULL,
                                  line_width = 1, show_mean = FALSE,
                                  boot_fits = NULL, boot_alpha = 0.15, ...) {
  ext <- fpca_extract(fit)
  boot_phi_list <- collect_boot_phi(boot_fits, fit, slot = ext$phi_slot,
                                    K = K, n_args = length(ext$args))
  draw_components(ext$phi, ext$args, ext$lambdas, ext$mu,
                  K = K, overlay = overlay, show_pve = show_pve,
                  colors = colors, line_width = line_width,
                  show_mean = show_mean,
                  boot_phi_list = boot_phi_list, boot_alpha = boot_alpha)
}


#' Sign-align bootstrap eigenfunctions to the main fit
#' @keywords internal
collect_boot_phi <- function(boot_fits, ref_fit, slot, K, n_args) {
  if (is.null(boot_fits) || length(boot_fits) == 0L) return(NULL)
  ref_phi <- ref_fit[[slot]]
  if (is.null(K)) K <- ncol(ref_phi)
  K <- min(K, ncol(ref_phi))
  out <- lapply(boot_fits, function(bf) {
    bphi <- bf[[slot]]
    if (is.null(bphi)) return(NULL)
    if (nrow(bphi) != n_args) return(NULL)        # grid mismatch -> drop
    K_b <- min(K, ncol(bphi))
    bphi <- bphi[, seq_len(K_b), drop = FALSE]
    for (k in seq_len(K_b)) {
      if (sum(bphi[, k] * ref_phi[, k]) < 0) bphi[, k] <- -bphi[, k]
    }
    bphi
  })
  out[!vapply(out, is.null, logical(1))]
}


#' Internal: draw eigenfunctions as faceted or overlaid lines
#' @keywords internal
draw_components <- function(phi, args, evs, mu,
                            K = NULL, overlay = FALSE, show_pve = TRUE,
                            colors = NULL, line_width = 1,
                            show_mean = FALSE,
                            boot_phi_list = NULL, boot_alpha = 0.15) {
  K_avail <- ncol(phi)
  if (is.null(K)) K <- K_avail
  K <- min(K, K_avail)

  pve <- if (!is.null(evs) && length(evs) >= K) evs / sum(evs) else rep(NA_real_, K)
  if (is.null(colors)) colors <- kodom_colors(max(K, 3L))[seq_len(K)]

  ## long format: (pc, time, phi)
  long_df <- data.frame(
    pc   = rep(seq_len(K), each = length(args)),
    time = rep(args, times = K),
    phi  = as.vector(phi[, seq_len(K), drop = FALSE])
  )

  ## optional mean panel — encoded as pc = 0
  if (show_mean) {
    if (is.null(mu)) {
      warning("show_mean = TRUE but fit$mu is missing; ignoring.", call. = FALSE)
    } else {
      mu_df <- data.frame(pc = 0L, time = args, phi = as.numeric(mu))
      long_df <- rbind(mu_df, long_df)
    }
  }

  ## reshape bootstrap traces (if any) to the same long shape
  boot_long <- NULL
  if (!is.null(boot_phi_list) && length(boot_phi_list) > 0L) {
    boot_long <- do.call(rbind, lapply(seq_along(boot_phi_list), function(b) {
      bphi <- boot_phi_list[[b]]
      Kb   <- ncol(bphi)
      data.frame(
        boot = b,
        pc   = rep(seq_len(Kb), each = length(args)),
        time = rep(args, times = Kb),
        phi  = as.vector(bphi)
      )
    }))
  }

  pc_levels   <- sort(unique(long_df$pc))
  line_colors <- if (any(pc_levels == 0L)) c("grey30", colors) else colors

  if (overlay) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3)
    if (!is.null(boot_long)) {
      p <- p + ggplot2::geom_line(
        data = boot_long,
        ggplot2::aes(x = .data$time, y = .data$phi,
                     group = interaction(.data$boot, .data$pc),
                     color = factor(.data$pc)),
        linewidth = 0.3, alpha = boot_alpha
      )
    }
    p <- p + ggplot2::geom_line(
      data = long_df,
      ggplot2::aes(x = .data$time, y = .data$phi,
                   color = factor(.data$pc), group = .data$pc),
      linewidth = line_width
    )

    legend_labels <- vapply(pc_levels, make_pc_label, character(1),
                            pve = pve, show_pve = show_pve)

    return(p +
      ggplot2::scale_color_manual(values = line_colors,
                                  labels = legend_labels, name = NULL) +
      ggplot2::labs(x = "time", y = expression(phi[k](t))) +
      theme_kodom())
  }

  ## faceted (default)
  long_df$pc_label <- pc_label_factor(long_df$pc, pve, show_pve)

  ## bootstrap traces need pc_label too, matched to the main facets
  if (!is.null(boot_long)) {
    label_lookup <- unique(long_df[, c("pc", "pc_label")])
    boot_long <- merge(boot_long, label_lookup, by = "pc", all.x = TRUE)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3)
  if (!is.null(boot_long)) {
    p <- p + ggplot2::geom_line(
      data = boot_long,
      ggplot2::aes(x = .data$time, y = .data$phi,
                   group = interaction(.data$boot, .data$pc),
                   color = factor(.data$pc)),
      linewidth = 0.3, alpha = boot_alpha
    )
  }
  p +
    ggplot2::geom_line(
      data = long_df,
      ggplot2::aes(x = .data$time, y = .data$phi,
                   color = factor(.data$pc), group = .data$pc),
      linewidth = line_width
    ) +
    ggplot2::facet_wrap(~ pc_label, ncol = 1, strip.position = "right",
                        scales = "free_y") +
    ggplot2::scale_color_manual(values = line_colors, guide = "none") +
    ggplot2::labs(x = "time", y = expression(phi[k](t))) +
    theme_kodom() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0))
}
