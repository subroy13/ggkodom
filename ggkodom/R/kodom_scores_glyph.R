#### KODOM_SCORES_GLYPH — TINY MULTIVARIATE SPARKLINE AT EACH PC LOCATION ####

#' FPC score scatter where each subject is a multivariate sparkline glyph
#'
#' Replaces each dot in a (FPC\eqn{j}, FPC\eqn{k}) score scatter with a
#' tiny line glyph showing the subject's actual trajectory.  When
#' \code{data} carries multiple variables (long format with a
#' \code{variable} column), each variable becomes its own colored line
#' inside the glyph, so you can read joint dynamics at a glance:
#' "the patient over here in the corner — what does her A1c, BMI, LDL
#' look like over time?"
#'
#' Time → small-x within glyph (left = early, right = late).
#' Value → small-y within glyph (bottom = low, top = high), normalized
#' per variable to its global range so the glyphs are comparable.
#'
#' Glyph dimensions are a fraction of the score-axes ranges
#' (\code{glyph_size} per side), so the visual scale is automatic and
#' independent of FPC magnitudes.
#'
#' @param fit An FPCA fit (\code{refund::fpca.sc} \code{fpca}/\code{fpca.sc}
#'   class, or \code{fdapace::FPCA} \code{FPCA} class).
#' @param data Long-format data frame with subject id, time, value,
#'   and an optional variable column.
#' @param id,time,value Column names (strings).  Defaults
#'   \code{"id"}/\code{"time"}/\code{"value"}.
#' @param variable Column name for the variable label, or \code{NULL}
#'   (default) for univariate input.
#' @param pc Length-2 integer vector picking the PCs for the x and y
#'   axes (default \code{c(1, 2)}).
#' @param glyph_size Half-width of each glyph as a fraction of the
#'   score-axis range (default 0.05 = 5\% of axis on each side).
#' @param glyph_colors Named character vector \code{c(varname = color)} for
#'   per-variable line colors.  \code{NULL} (default) auto-picks a
#'   colorblind-friendly palette (\code{"Dark2"} for 2–8 variables).
#' @param alpha,linewidth Glyph cosmetics.
#' @param show_centers Draw a faint dot at each subject's score location
#'   (default \code{TRUE}).
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' \dontrun{
#'   long <- rbind(
#'     transform(a1c_df, variable = "a1c"),
#'     transform(bmi_df, variable = "bmi")
#'   )
#'   ## univariate FPCA on a1c, glyphs show both a1c and bmi
#'   fp <- fdapace::FPCA(Ly = split(a1c_df$value, a1c_df$id),
#'                       Lt = split(a1c_df$time,  a1c_df$id))
#'   kodom_scores_glyph(fp, long, variable = "variable")
#' }
kodom_scores_glyph <- function(fit, data,
                               id = "id", time = "time", value = "value",
                               variable = NULL,
                               pc = c(1, 2),
                               glyph_size = 0.05,
                               glyph_colors = NULL,
                               alpha = 0.8,
                               linewidth = 0.4,
                               show_centers = TRUE) {

  ext    <- fpca_extract(fit)
  scores <- ext$scores
  ids    <- ext$ids
  if (length(pc) != 2L || any(pc < 1L) || max(pc) > ncol(scores)) {
    stop("pc must be a length-2 vector of valid PC indices for this fit.",
         call. = FALSE)
  }

  pve     <- ext$lambdas / sum(ext$lambdas)
  pc_lab  <- function(k) sprintf("FPC%d (%.1f%%)", k, 100 * pve[k])

  score_df <- data.frame(
    .id = as.character(ids),
    .x  = scores[, pc[1L]],
    .y  = scores[, pc[2L]],
    stringsAsFactors = FALSE
  )

  ## --- prep data ---
  needed <- c(id, time, value)
  if (!is.null(variable)) needed <- c(needed, variable)
  check_columns(data, needed)

  d <- data
  d[[time]]  <- as.numeric(d[[time]])
  d[[value]] <- as.numeric(d[[value]])
  d <- d[!is.na(d[[time]]) & !is.na(d[[value]]), , drop = FALSE]
  d$.id <- as.character(d[[id]])

  ## drop subjects without scores; warn if non-trivial
  in_scores <- d$.id %in% score_df$.id
  if (any(!in_scores)) {
    n_drop <- length(unique(d$.id[!in_scores]))
    warning(n_drop, " subject(s) in data have no score; dropped.",
            call. = FALSE)
    d <- d[in_scores, , drop = FALSE]
  }

  if (is.null(variable)) {
    d$.var <- "value"
  } else {
    d$.var <- as.character(d[[variable]])
  }
  vars <- sort(unique(d$.var))

  ## --- glyph dimensions in score units ---
  s_xr <- range(score_df$.x, na.rm = TRUE)
  s_yr <- range(score_df$.y, na.rm = TRUE)
  dx   <- diff(s_xr) * glyph_size
  dy   <- diff(s_yr) * glyph_size

  ## --- per-variable global ranges so glyph y-axis is comparable across subjects ---
  t_split <- split(d[[time]],  d$.var)
  v_split <- split(d[[value]], d$.var)
  t_min <- vapply(t_split, min, numeric(1))
  t_max <- vapply(t_split, max, numeric(1))
  v_min <- vapply(v_split, min, numeric(1))
  v_max <- vapply(v_split, max, numeric(1))

  ## --- merge in score positions ---
  d <- merge(d, score_df, by = ".id")

  tn <- (d[[time]]  - t_min[d$.var]) / pmax(t_max[d$.var] - t_min[d$.var],
                                            .Machine$double.eps) - 0.5
  vn <- (d[[value]] - v_min[d$.var]) / pmax(v_max[d$.var] - v_min[d$.var],
                                            .Machine$double.eps) - 0.5

  d$.gx    <- d$.x + tn * dx
  d$.gy    <- d$.y + vn * dy
  d$.group <- paste(d$.id, d$.var, sep = "__")
  d <- d[order(d$.id, d$.var, d[[time]]), , drop = FALSE]

  ## --- colors ---
  if (is.null(glyph_colors)) {
    if (length(vars) == 1L) {
      glyph_colors <- stats::setNames("#1b9e77", vars)
    } else if (length(vars) <= 8L) {
      pal <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
               "#66a61e", "#e6ab02", "#a6761d", "#666666")
      glyph_colors <- stats::setNames(pal[seq_along(vars)], vars)
    } else {
      glyph_colors <- stats::setNames(scales::hue_pal()(length(vars)), vars)
    }
  }

  ## --- plot ---
  p <- ggplot2::ggplot()

  if (show_centers) {
    p <- p + ggplot2::geom_point(
      data = score_df,
      ggplot2::aes(x = .data$.x, y = .data$.y),
      color = "grey60", size = 0.6, alpha = 0.5
    )
  }

  p <- p + ggplot2::geom_path(
    data = d,
    ggplot2::aes(x = .data$.gx, y = .data$.gy,
                 group = .data$.group, color = .data$.var),
    linewidth = linewidth, alpha = alpha
  ) +
    ggplot2::scale_color_manual(values = glyph_colors,
                                name   = if (is.null(variable)) NULL else variable)

  p +
    ggplot2::labs(x = pc_lab(pc[1L]), y = pc_lab(pc[2L])) +
    theme_kodom()
}
