#### COLORS AND SCALES ####

#' Kodom color palette
#'
#' Returns colors inspired by the Kadam flower
#' (\emph{Neolamarckia cadamba}): teal (low), gold (mid), red (high).
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


#' Continuous gradient color/fill scale with kodom defaults
#'
#' Smooth gradient — color changes continuously between
#' \code{color_breaks}.  See \code{\link{kodom_step_scale}} for the
#' discrete-band variant.
#'
#' @param aesthetic \code{"color"} or \code{"fill"}.
#' @param colors Gradient color vector.
#' @param color_breaks Numeric breakpoints anchoring the gradient via
#'   \code{\link[scales]{rescale}}.  \code{NULL} = evenly spaced.
#' @param name Legend title.
#' @return A ggplot2 scale object.
#' @export
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
  if (!is.null(color_breaks)) {
    args$values <- scales::rescale(color_breaks)
  }
  do.call(scale_fn, args)
}


#' Discrete-band (step) color/fill scale with kodom defaults
#'
#' Solid color bands separated at \code{color_breaks} — useful when the
#' fitted/smoothed values vary slowly and a continuous gradient washes
#' out into a single hue.  Each break adds one band: \code{k} breaks
#' produce \code{k + 1} bands.  Colors are auto-expanded from the
#' \code{kodom_colors()} anchors when fewer colors than bands are passed.
#'
#' @inheritParams kodom_gradient_scale
#' @return A ggplot2 scale object (binned).
#' @export
#' @examples
#' \dontrun{
#' kodom_swimlane(df) + kodom_step_scale("color",
#'   color_breaks = c(5.5, 7, 8, 10)
#' )
#' }
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


#' Pick gradient or step scale based on the \code{discretize} flag
#'
#' Internal router used by all \code{kodom_*} plot functions.
#' Public functions expose \code{discretize} so users don't have to
#' know about the helper directly.
#' @inheritParams kodom_gradient_scale
#' @param discretize \code{FALSE} (default) = continuous gradient;
#'   \code{TRUE} = step bands.
#' @keywords internal
kodom_value_scale <- function(aesthetic = c("color", "fill"),
                              colors = kodom_colors(),
                              color_breaks = NULL,
                              name = "value",
                              discretize = FALSE) {
  if (isTRUE(discretize)) {
    kodom_step_scale(aesthetic, colors, color_breaks, name)
  } else {
    kodom_gradient_scale(aesthetic, colors, color_breaks, name)
  }
}
