# =========================
# Logo generation for ggkodom
# Run once from the package root: source("data-raw/logo.R")
# Requires: ggplot2, hexSticker (development-only; not in DESCRIPTION)
# Output: man/figures/logo.png
# ========================

library(ggplot2)
library(hexSticker)


# ---- Helper: leaf polygon + midrib vein ----
# Returns a list with $polygon and $vein data frames, both with columns x, y, id.
# The leaf is built along the x-axis then rotated by `angle` radians.
make_leaf <- function(x0, y0, angle = 0, len = 1.4,
                      width_top = 0.32, width_bot = 0.13, id = 1L) {
    t <- seq(0, 1, length.out = 400)
    x_top <- len * t
    y_top <- width_top * sin(pi * t)
    x_bot <- rev(len * t)
    y_bot <- rev(-width_bot * sin(pi * t))
    x <- c(x_top, x_bot)
    y <- c(y_top, y_bot)
    xr <- x * cos(angle) - y * sin(angle)
    yr <- x * sin(angle) + y * cos(angle)
    list(
        polygon = data.frame(x = x0 + xr, y = y0 + yr, id = id),
        vein = data.frame(
            x  = x0 + len * t * cos(angle),
            y  = y0 + len * t * sin(angle),
            id = id
        )
    )
}


# ---- Helper: orange inner stamens ----
# Each stamen is a short outward-pointing segment from a random interior point.
make_flower_inner <- function(n = 3500, seed = 1234) {
    set.seed(seed)
    theta <- runif(n, 0, 2 * pi)
    r <- 0.6 * sqrt(runif(n)) # uniform disk density
    x <- r * cos(theta)
    y <- r * sin(theta)
    ang <- atan2(y, x)
    len <- runif(n, 0.03, 0.09)
    cols <- c("#FF4500", "#FF5A00", "#FF6A00", "#FF7F11", "#FFA500")
    data.frame(
        x    = x,
        y    = y,
        xend = x + len * cos(ang),
        yend = y + len * sin(ang),
        col  = sample(cols, n, replace = TRUE),
        lwd  = runif(n, 0.3, 1.0) # ggplot2 linewidth in mm
    )
}


# ---- Helper: white outer petals ----
# Short radial segments just outside the stamen disk.
make_flower_petals <- function(n = 200, seed = 123) {
    set.seed(seed)
    theta <- runif(n, 0, 2 * pi)
    r <- runif(n, 0.6, 0.7)
    x <- r * cos(theta)
    y <- r * sin(theta)
    ang <- atan2(y, x)
    data.frame(
        x    = x,
        y    = y,
        xend = x + 0.2 * cos(ang),
        yend = y + 0.2 * sin(ang)
    )
}


# ---- Assemble layer data ----

leaf_r <- make_leaf(x0 = 0.04, y0 = -1.45, angle = pi / 4, len = 1.35, id = 1L)
leaf_l <- make_leaf(x0 = 0, y0 = -1.55, angle = 3 * pi / 4, len = 1.90, id = 2L)

polys <- rbind(leaf_r$polygon, leaf_l$polygon)
veins <- rbind(leaf_r$vein, leaf_l$vein)
inner <- make_flower_inner()
petals <- make_flower_petals()


# ---- ggplot2 flower ----

p_flower <- ggplot() +
    # Leaves — filled asymmetric polygon + midrib
    geom_polygon(
        data = polys,
        aes(x = x, y = y, group = id),
        fill = "#2E7D32",
        colour = "darkgreen",
        linewidth = 0.3
    ) +
    geom_path(
        data = veins,
        aes(x = x, y = y, group = id),
        colour = "darkgreen",
        linewidth = 0.3
    ) +
    # Stem
    geom_segment(
        aes(x = 0, y = -0.6, xend = 0, yend = -1.9),
        colour = "darkgreen",
        linewidth = 0.8
    ) +
    # Orange stamens — colour and linewidth both come from the data
    geom_segment(
        data = inner,
        aes(
            x = x, y = y, xend = xend, yend = yend,
            colour = col, linewidth = lwd
        ),
        lineend = "round",
        show.legend = FALSE
    ) +
    scale_colour_identity() +
    scale_linewidth_identity() +
    # White outer petals
    geom_segment(
        data = petals,
        aes(x = x, y = y, xend = xend, yend = yend),
        colour = rgb(1, 1, 1, alpha = 0.45),
        linewidth = 0.5,
        lineend = "round"
    ) +
    coord_fixed(xlim = c(-1.5, 1.5), ylim = c(-2.0, 1.2)) +
    theme_void() +
    theme(
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)
    )


# ---- hexSticker ----
# h_fill / h_color use the ggkodom dark-background palette.
# p_y = 0.38 places "ggkodom" near the bottom of the hex.
# s_y = 1.05 shifts the flower slightly above centre so the stem
# sits in the lower half and the bloom occupies the upper half.

sticker(
    subplot    = p_flower,
    package    = "ggkodom",
    p_size     = 18,
    p_color    = "#FFCC3D",
    p_fontface = "bold",
    s_x        = 1.0,
    s_y        = 0.75,
    s_width    = 1.25,
    s_height   = 1.25,
    h_fill     = "#06130A",
    h_color    = "#FFCC3D",
    h_size     = 1.0,
    dpi        = 300,
    filename   = "man/figures/logo.png"
)

message("Logo written to man/figures/logo.png")
