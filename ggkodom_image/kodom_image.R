# Kadam Flower in R
# Orange center + white outer petals
# Dark background

# set.seed(123)

# leaves -------------------------

leaf_angle <- function(x0, y0,
                       angle = 0,
                       len = 1.4,
                       width_top = 0.32,
                       width_bot = 0.13,
                       col = "#2E7D32") { #"#1B5E20"
  
  t <- seq(0, 1, length.out = 400)
  
  # leaf shape along x-axis first
  x_top <- len * t
  y_top <- width_top * sin(pi*t)
  
  x_bot <- rev(len * t)
  y_bot <- rev(-width_bot * sin(pi*t))
  
  x <- c(x_top, x_bot)
  y <- c(y_top, y_bot)
  
  # rotate
  xr <- x*cos(angle) - y*sin(angle)
  yr <- x*sin(angle) + y*cos(angle)
  
  polygon(
    x0 + xr, y0 + yr,
    col = col,
    border = "darkgreen", #7CFC90",
    lwd = 2
  )
  
  # middle vein
  xv <- len*t
  yv <- rep(0, length(t))
  
  xvr <- xv*cos(angle) - yv*sin(angle)
  yvr <- xv*sin(angle) + yv*cos(angle)
  
  lines(
    x0 + xvr, y0 + yvr,
    col = "darkgreen",#C8FFD4",
    lwd = 2
  )
}
hexagon <- function(cx = 0, cy = -0.25, r = 1.75,
                    fill = "#06130A",
                    border = "goldenrod",
                    lwd = 4) {
  ang <- seq(pi/6, 2*pi + pi/6, length.out = 7)
  x <- cx + r*cos(ang)
  y <- cy + r*sin(ang)
  polygon(
    x, y,
    col = fill,
    border = border,
    lwd = lwd
  )
}

# canvas ====================
# par(bg = "#06130A")
par(bg = "white")
plot(
  0, 0,
  type = "n",
  xlim = c(-1.5, 1.5),
  ylim = c(-2.0, 1.5),
  asp  = 1,
  axes = F,
  xlab = "",
  ylab = "",
  main = ""
)

hexagon()

# right leaf
leaf_angle(
  x0 = 0.04, y0 = -1.45,
  angle = pi/4,#210*pi/180,
  len = 1.35
)

# stem 
segments(
  0, -0.6,
  0, -1.9,
  col = "darkgreen",
  lwd = 4
)

# left leaf
leaf_angle(
  x0 = 0, y0 = -1.55,
  angle = pi/2 + pi/4,
  len = 1.9
)


# FLOWER ==========================================
# dense orange center

n_center <- 3500

theta <- runif(n_center, 0, 2*pi)

# spherical density
r <- 0.6 * sqrt(runif(n_center))

x <- r*cos(theta)
y <- r*sin(theta)

# orange spikes
for(i in 1:n_center) {
  
  rr <- sqrt(x[i]^2 + y[i]^2)
  
  angle <- atan2(y[i], x[i])
  
  len <- runif(1, 0.03, 0.09)
  
  x2 <- x[i] + len*cos(angle)
  y2 <- y[i] + len*sin(angle)
  
  segments(
    x[i], y[i],
    x2, y2,
    col = sample(
      c("#FF4500",
        "#FF5A00",
        "#FF6A00",
        "#FF7F11",
        "#FFA500"),
      1
    ),
    lwd = runif(1, 1.5, 3.5),
    lend = "round"
  )
}

# white outer petals
n_petals <- 200

theta2 <- runif(n_petals, 0, 2*pi)

r2 <- runif(n_petals, 0.6, 0.7) #runif(n_petals, 0.7, 0.85)

x2 <- r2*cos(theta2)
y2 <- r2*sin(theta2)

for(i in 1:n_petals) {
  
  angle <- atan2(y2[i], x2[i])
  
  len <- 0.2 #runif(1, 0.08, 0.23)
  
  x3 <- x2[i] + len*cos(angle)
  y3 <- y2[i] + len*sin(angle)
  
  segments(
    x2[i], y2[i],
    x3, y3,
    col = rgb(1, 1, 1,
              alpha = 0.4),
    lwd = 2, #runif(1, 2, 5),
    lend = "round"
  )
}

# points(x = 1, y = 0.7, col = "red", pch = 19)

# par(bg = "white")
rm(list = ls())