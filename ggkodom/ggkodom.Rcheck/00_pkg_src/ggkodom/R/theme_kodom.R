#### THEMES ####

#' Clean theme for ggkodom linear plots
#'
#' Based on \code{\link[ggplot2]{theme_minimal}}: no grid lines,
#' restrained axis text, wide horizontal color-bar legend.
#'
#' @param base_size Base font size (default 12).
#' @param legend_position One of \code{"top"}, \code{"bottom"},
#'   \code{"left"}, \code{"right"}, \code{"none"}.
#' @return A \code{\link[ggplot2]{theme}} object.
#' @export
theme_kodom <- function(base_size = 12, legend_position = "top") {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.text.y       = ggplot2::element_text(size = base_size * 0.7),
      axis.text.x       = ggplot2::element_text(size = base_size * 0.85),
      legend.position   = legend_position,
      strip.text        = ggplot2::element_text(face = "bold", size = base_size)
    )
}


#' Theme for circular ggkodom plots
#'
#' Minimal polar theme: no axis text/ticks, subtle concentric
#' reference circles, bottom legend.
#'
#' @param base_size Base font size (default 12).
#' @return A \code{\link[ggplot2]{theme}} object.
#' @export
theme_kodom_circular <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text         = ggplot2::element_blank(),
      axis.title        = ggplot2::element_blank(),
      axis.ticks        = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        color = "grey85", linewidth = 0.3
      ),
      legend.position   = "bottom",
      plot.title        = ggplot2::element_text(hjust = 0.5)
    )
}
