# X-scale for periodic longitudinal plots

This is a convenience wrapper around
[`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
that enforces `limits = c(0, period)` and uses `oob = scales::oob_keep`.

## Usage

``` r
scale_x_kodom_periodic(period = 12, breaks = 1:period, ...)
```

## Arguments

- period:

  The length of one complete cycle (e.g. `12` for months). Must match
  the `period` argument passed to
  [`geom_kodom_periodic()`](geom_kodom_periodic.md). Default `12`.

- breaks:

  Passed to
  [`scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).
  Default provides integer breaks for the period.

- ...:

  Additional arguments passed to
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
  such as `labels`.

## Value

A
[`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
object.

## Details

**Why is this necessary?** To make exactly one cycle span exactly one
360-degree rotation in `coord_polar`, the scale limits *must* be set to
the period length (e.g., `c(0, 12)`). However, standard ggplot2
`scale_x_continuous` will drop any data outside those limits. By setting
`oob = scales::oob_keep`, we instruct ggplot2 to keep the data that
exceeds the period. `coord_polar` then natively wraps those
out-of-bounds values around the circle, creating beautiful, continuous
Archimedean spirals!

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(df, aes(x = visit_month, id = subject_id, colour = hba1c)) +
  geom_kodom_periodic(period = 12) +
  scale_x_kodom_periodic(period = 12, labels = month.abb) +
  scale_y_kodom_periodic() +
  coord_kodom_periodic() +
  theme_kodom_periodic()
} # }
```
