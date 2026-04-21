#### MULTIVARIATE KODOM — POLAR, CONCENTRIC BANDS PER VARIABLE ####

#' Polar multivariate kodom — concentric bands, one per variable
#'
#' One spoke per subject (lane), split into \eqn{J} radial bands
#' (one per variable).  Time runs radially within each band.  Color
#' is the per-variable z-score, so all bands share a single gradient
#' even though A1c, BMI, LDL etc.\ live on completely different ranges.
#'
#' This is the genuine multivariate extension of the kodom flower —
#' each "petal" gets striped by variable.  Compare to
#' \code{\link{kodom_swimlane_multi}}, which is the stacked / faceted
#' alternative (less visually integrated; better for raw-units reading).
#'
#' @param data Long-format data frame: one row per (subject, variable, time).
#' @param id,time,value,variable Column names (strings).
#' @param variables Character vector of variables to plot, in the desired
#'   inside-out band order.  \code{NULL} (default) = all in natural order.
#' @param anchor Variable defining the lane (spoke) ordering.  \code{NULL}
#'   (default) = first entry of \code{variables}.
#' @param sort_by See \code{\link{kodom_swimlane}} — applied to the anchor.
#' @param n_sample Max subjects (computed on the anchor).
#' @param line_width,alpha,point_size Cosmetics.
#' @param arc_degrees Angular extent in degrees (default 360).  See
#'   \code{\link{kodom_swimlane}}.
#' @param gap_fraction Empty-arc fraction at the wrap seam (default 0.15);
#'   overridden when \code{arc_degrees < 360}.
#' @param inner_fraction Hollow-center fraction (default 0.3).
#' @param band_gap_fraction Fraction of each band's radial width left as
#'   a gap between bands (default 0.1).
#' @param direction \code{1} = clockwise (default), \code{-1} = ccw.
#' @param show_band_labels Print variable names at the seam, one per band
#'   (default \code{TRUE}).
#' @param show_band_rings Faint dotted ring at each band boundary
#'   (default \code{TRUE}).
#' @param seed Random seed for sampling.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' \dontrun{
#'   long <- rbind(
#'     transform(a1c_df, variable = "a1c"),
#'     transform(bmi_df, variable = "bmi")
#'   )
#'   kodom_multi(long, anchor = "a1c")
#'   kodom_multi(long, anchor = "a1c", arc_degrees = 270)
#' }
kodom_multi <- function(data,
                        id = "id", time = "time", value = "value",
                        variable = "variable",
                        variables = NULL, anchor = NULL,
                        sort_by = "first", n_sample = 50,
                        line_width = 0.5, alpha = 0.8, point_size = 1.2,
                        arc_degrees = 360, gap_fraction = 0.15,
                        inner_fraction = 0.3, band_gap_fraction = 0.1,
                        direction = 1,
                        show_band_labels = TRUE,
                        show_band_rings  = TRUE,
                        seed = 123) {

  check_columns(data, c(id, time, value, variable))
  if (arc_degrees <= 0 || arc_degrees > 360) {
    stop("arc_degrees must be in (0, 360].", call. = FALSE)
  }
  if (arc_degrees < 360) gap_fraction <- (360 - arc_degrees) / 360

  if (is.null(variables)) variables <- as.character(unique(data[[variable]]))
  if (is.null(anchor))    anchor    <- variables[1L]
  if (!anchor %in% variables) {
    stop("anchor '", anchor, "' is not in variables.", call. = FALSE)
  }

  ## --- shared lane order from anchor ---
  anchor_dat <- data[as.character(data[[variable]]) == anchor, , drop = FALSE]
  if (nrow(anchor_dat) == 0L) {
    stop("Anchor variable '", anchor, "' has no rows.", call. = FALSE)
  }
  prep <- prep_trajectory_data(anchor_dat, id, time, value,
                               n_sample = n_sample, sort_by = sort_by,
                               seed = seed)
  shared_order <- prep$id_order
  n_ids <- length(shared_order)

  ## --- filter + reshape ---
  d <- data[as.character(data[[id]]) %in% shared_order &
            as.character(data[[variable]]) %in% variables, , drop = FALSE]
  d[[id]]       <- factor(as.character(d[[id]]),       levels = shared_order)
  d[[variable]] <- factor(as.character(d[[variable]]), levels = variables)
  d[[time]]     <- as.numeric(d[[time]])
  d[[value]]    <- as.numeric(d[[value]])
  d <- d[!is.na(d[[time]]) & !is.na(d[[value]]), , drop = FALSE]

  ## --- z-score per variable so they share one gradient ---
  d$.z <- stats::ave(d[[value]], d[[variable]], FUN = function(v) {
    s <- stats::sd(v, na.rm = TRUE)
    if (!is.finite(s) || s == 0) return(rep(0, length(v)))
    (v - mean(v, na.rm = TRUE)) / s
  })

  ## --- angular position: lane index ---
  d$.id_pos <- as.numeric(d[[id]])
  gap_n     <- max(1L, ceiling(n_ids * gap_fraction))
  ang_max   <- n_ids + gap_n

  ## --- band layout: J equal-width radial bands between inner_fraction and 1 ---
  J          <- length(variables)
  avail      <- 1 - inner_fraction
  band_total <- avail / J
  band_gap   <- band_total * band_gap_fraction
  band_inner <- inner_fraction + (seq_len(J) - 1) * band_total + band_gap / 2
  band_outer <- inner_fraction +  seq_len(J)      * band_total - band_gap / 2
  names(band_inner) <- variables
  names(band_outer) <- variables

  ## --- per-variable time range, used to scale into each band ---
  t_split <- split(d[[time]], d[[variable]])
  t_min   <- vapply(t_split, min, numeric(1))
  t_max   <- vapply(t_split, max, numeric(1))

  ## --- radial coordinate per row ---
  var_chr   <- as.character(d[[variable]])
  inner_row <- band_inner[var_chr]
  outer_row <- band_outer[var_chr]
  tmin_row  <- t_min[var_chr]
  tmax_row  <- t_max[var_chr]
  span      <- pmax(tmax_row - tmin_row, .Machine$double.eps)
  frac      <- (d[[time]] - tmin_row) / span
  d$.radius <- inner_row + frac * (outer_row - inner_row)

  ## --- group key: (id, variable) so each subject-variable is its own polyline ---
  d$.group <- paste(as.character(d[[id]]), var_chr, sep = "__")
  d <- d[order(d[[id]], d[[variable]], d[[time]]), , drop = FALSE]

  ## --- annotation data: band-boundary rings and seam labels ---
  ring_df <- if (show_band_rings) {
    data.frame(
      theta = rep(seq(0, ang_max, length.out = 100L), J),
      r     = rep(band_outer, each = 100L),
      band  = rep(variables, each = 100L)
    )
  } else NULL

  label_df <- if (show_band_labels) {
    data.frame(
      theta    = n_ids + gap_n / 2,
      r        = (band_inner + band_outer) / 2,
      variable = variables,
      stringsAsFactors = FALSE
    )
  } else NULL

  ## --- build plot ---
  p <- ggplot2::ggplot()

  if (!is.null(ring_df)) {
    p <- p + ggplot2::geom_path(
      data = ring_df,
      ggplot2::aes(x = .data$theta, y = .data$r, group = .data$band),
      color = "grey75", linewidth = 0.2, linetype = "dotted"
    )
  }

  p <- p +
    ggplot2::geom_path(
      data = d,
      ggplot2::aes(x = .data$.id_pos, y = .data$.radius,
                   group = .data$.group, color = .data$.z),
      linewidth = line_width, alpha = alpha
    )

  if (point_size > 0) {
    p <- p + ggplot2::geom_point(
      data = d,
      ggplot2::aes(x = .data$.id_pos, y = .data$.radius,
                   color = .data$.z),
      size = point_size
    )
  }

  if (!is.null(label_df)) {
    p <- p + ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(x = .data$theta, y = .data$r, label = .data$variable),
      size = 2.7, color = "grey20"
    )
  }

  p +
    ggplot2::scale_color_gradient2(
      low  = "#2c7fb8", mid = "grey90", high = "#d7301f",
      midpoint = 0,
      name = "z\n(per var)"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0.5, ang_max + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(band_outer) * 1.02)
    ) +
    ggplot2::coord_polar(theta = "x", start = 0, direction = direction) +
    theme_kodom_circular() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(barwidth = 12, barheight = 0.6)
    )
}
