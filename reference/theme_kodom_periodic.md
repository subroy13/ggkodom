# Theme for periodic Kodom plots

Designed for use with [`geom_kodom_periodic()`](geom_kodom_periodic.md)
and [`coord_kodom_periodic()`](coord_kodom_periodic.md). Suppresses the
radial (y) axis — which shows lane numbers that are not meaningful to
readers — while keeping the angular (x) axis so that time labels added
via
[`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
are visible around the ring.

## Usage

``` r
theme_kodom_periodic(base_size = 12)
```

## Arguments

- base_size:

  Base font size (default 12).

## Value

A
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
object.

## Examples

``` r
# \donttest{
library(ggplot2)
df <- data.frame(
  subject_id = rep(1:5, each = 4),
  time = rep(1:4, 5),
  visit_month = rep(1:4, 5),
  value = rep(1:4, 5),
  hba1c = rep(1:4, 5),
  arm = rep(c("Treatment", "Control"), c(12, 8))
)
ggplot(df, aes(x = visit_month, id = subject_id, colour = hba1c)) +
  geom_kodom_periodic(period = 12) +
  scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(0, 12)) +
  scale_colour_kodom() +
  coord_kodom_periodic() +
  theme_kodom_periodic()

# }
```
