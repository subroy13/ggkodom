#### COLORS AND SCALES ####

#' Kodom color palette
#'
#' Returns colors inspired by the Kadam flower (*Neolamarckia cadamba*):
#' teal (low), gold (mid), red (high). Passing `n > 3` interpolates between
#' the three anchors via [grDevices::colorRampPalette()].
#'
#' @param n Number of colors. 3 returns the anchor set; >3 interpolates.
#' @return Character vector of hex color codes.
#' @export
#' @examples
#' kodom_colors()
#' kodom_colors(7)
kodom_colors <- function(n = 3L) {
  anchors <- c("#008D98", "#FFCC3D", "#D7433B")
  if (n <= 3L) {
    return(anchors[seq_len(min(n, 3L))])
  }
  grDevices::colorRampPalette(anchors)(n)
}


#' @keywords internal
kodom_gradient_scale <- function(aesthetic = c("color", "fill"),
                                 colors = kodom_colors(),
                                 color_breaks = NULL,
                                 name = "value") {
  aesthetic <- match.arg(aesthetic)
  scale_fn <- switch(aesthetic,
    color = ggplot2::scale_color_gradientn,
    fill  = ggplot2::scale_fill_gradientn
  )
  args <- list(colors = colors, na.value = "grey90", name = name)
  if (!is.null(color_breaks)) args$values <- scales::rescale(color_breaks)
  do.call(scale_fn, args)
}


#' @keywords internal
kodom_step_scale <- function(aesthetic = c("color", "fill"),
                             colors = kodom_colors(),
                             color_breaks = NULL,
                             name = "value") {
  aesthetic <- match.arg(aesthetic)
  scale_fn <- switch(aesthetic,
    color = ggplot2::scale_color_stepsn,
    fill  = ggplot2::scale_fill_stepsn
  )
  args <- list(na.value = "grey90", name = name)
  if (!is.null(color_breaks)) {
    n_bands <- length(color_breaks) + 1L
    if (length(colors) < n_bands) colors <- kodom_colors(n_bands)
    args$breaks <- color_breaks
  }
  args$colors <- colors
  do.call(scale_fn, args)
}


#' Kodom color scale for the colour aesthetic
#'
#' A ggplot2 color scale using the Kadam flower palette (teal -> gold -> red).
#' Designed to compose with [geom_kodom_line()], [geom_kodom_heatmap()], and
#' [geom_kodom_circular()].
#'
#' Set `discretize = TRUE` to switch from a smooth gradient to solid color
#' bands separated at `color_breaks` — useful when values change slowly and
#' a continuous gradient washes out into a single hue.
#'
#' @param colors Color vector. Defaults to [kodom_colors()].
#' @param color_breaks Numeric breakpoints anchoring the gradient via
#'   [scales::rescale()]. For `discretize = TRUE`, each break adds one band:
#'   `k` breaks produce `k + 1` bands. `NULL` = evenly spaced.
#' @param name Legend title. Defaults to `"value"`.
#' @param discretize `FALSE` (default) = smooth gradient; `TRUE` = step bands
#'   at `color_breaks`.
#' @param ... Ignored.
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = time, id = subject_id, colour = value)) +
#'   geom_kodom_line() +
#'   scale_colour_kodom()
#'
#' # Discrete bands at clinical thresholds
#' ggplot(df, aes(x = time, id = subject_id, colour = hba1c)) +
#'   geom_kodom_line() +
#'   scale_colour_kodom(discretize = TRUE, color_breaks = c(5.7, 6.5))
#' }
scale_color_kodom <- function(colors = kodom_colors(),
                              color_breaks = NULL,
                              name = ggplot2::waiver(),
                              discretize = FALSE,
                              ...) {
  nm <- if (inherits(name, "waiver")) "value" else name
  if (isTRUE(discretize)) {
    kodom_step_scale("color", colors, color_breaks, nm)
  } else {
    kodom_gradient_scale("color", colors, color_breaks, nm)
  }
}

#' @rdname scale_color_kodom
#' @export
scale_colour_kodom <- scale_color_kodom


#' Kodom fill scale for the fill aesthetic
#'
#' Identical behaviour to [scale_color_kodom()] but targets the `fill`
#' aesthetic. Intended for use with [geom_kodom_heatmap()].
#'
#' @inheritParams scale_color_kodom
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = time, id = subject_id, fill = value)) +
#'   geom_kodom_heatmap() +
#'   scale_fill_kodom()
#' }
scale_fill_kodom <- function(colors = kodom_colors(),
                             color_breaks = NULL,
                             name = ggplot2::waiver(),
                             discretize = FALSE,
                             ...) {
  nm <- if (inherits(name, "waiver")) "value" else name
  if (isTRUE(discretize)) {
    kodom_step_scale("fill", colors, color_breaks, nm)
  } else {
    kodom_gradient_scale("fill", colors, color_breaks, nm)
  }
}
