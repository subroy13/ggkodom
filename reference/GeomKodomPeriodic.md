# Geom for geom_kodom_periodic

Extends
[ggplot2::GeomPath](https://ggplot2.tidyverse.org/reference/Geom.html)
to draw concentric "star trail" arcs. Interpolates colours/alpha in the
(angle, radius) space that the stat already produces, then delegates the
polar projection to `coord_polar(theta = "x")`. This means standard
ggplot2 scales and coordinates work as expected: use
[`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
for time-axis labels,
[`ggplot2::coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
for direction, and [`theme_kodom_periodic()`](theme_kodom_periodic.md)
for a clean panel.

## Usage

``` r
GeomKodomPeriodic
```

## Format

A ggproto object.
