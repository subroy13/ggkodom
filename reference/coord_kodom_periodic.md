# Polar coordinate system for periodic Kodom plots

A convenience wrapper around
[`ggplot2::coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
with defaults appropriate for
[`geom_kodom_periodic()`](geom_kodom_periodic.md): `theta = "x"` (time
maps to angle), `start = pi/2` (x = 0 at 12 o'clock), and
`direction = -1` (clockwise). Pass `clockwise = FALSE` for a
counter-clockwise layout.

## Usage

``` r
coord_kodom_periodic(clockwise = TRUE)
```

## Arguments

- clockwise:

  Logical. `TRUE` (default) places time clockwise from the top, matching
  the convention of most clock and calendar displays.

## Value

A
[`ggplot2::coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
coordinate object.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(df, aes(x = visit_month, id = subject_id, colour = hba1c)) +
  geom_kodom_periodic(period = 12) +
  scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(0, 12)) +
  scale_colour_kodom() +
  coord_kodom_periodic() +
  theme_kodom_periodic()
} # }
```
