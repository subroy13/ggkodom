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
  anchors <- c("#008d98", "#ffcc3d", "#d7433b")
  if (n <= 3L) return(anchors[seq_len(min(n, 3L))])
  grDevices::colorRampPalette(anchors)(n)
}


#' Build a continuous gradient color/fill scale with kodom defaults
#'
#' @param aesthetic \code{"color"} or \code{"fill"}.
#' @param colors Gradient color vector.
#' @param color_breaks Numeric breakpoints to anchor the gradient via
#'   \code{\link[scales]{rescale}}.  \code{NULL} = evenly spaced.
#' @param name Legend title.
#' @return A ggplot2 scale object.
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
  if (!is.null(color_breaks)) {
    args$values <- scales::rescale(color_breaks)
  }
  do.call(scale_fn, args)
}
