# Theme for circular ggkodom plots

Fully minimal Cartesian theme for
[`geom_kodom_circular()`](geom_kodom_circular.md): no axis text, ticks,
or grid lines. Reference rings are drawn by the geom itself as
annotation layers, so this theme suppresses all panel grid elements.

## Usage

``` r
theme_kodom_circular(base_size = 12)
```

## Arguments

- base_size:

  Base font size (default 12).

## Value

A
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
object.

## Details

Unlike the original
[`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)-based
approach, [`geom_kodom_circular()`](geom_kodom_circular.md) performs its
coordinate transformation inside the stat and renders in ordinary
Cartesian space. This means `panel.grid.major.y` lines would appear as
horizontal rules, not concentric circles — so both major axes are
suppressed here.

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
ggplot(df, aes(x = time, id = subject_id, colour = value)) +
  geom_kodom_circular() +
  scale_colour_kodom() +
  coord_fixed() +
  theme_kodom_circular()

# }
```
