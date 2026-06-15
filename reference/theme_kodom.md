# Clean theme for ggkodom linear and heatmap plots

Based on
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html):
vertical x-axis grid lines mark time positions, horizontal y-axis grid
lines are suppressed (they would bisect subject lanes and add clutter).
Restrained axis text, wide horizontal color-bar legend. Suitable for
[`geom_kodom_line()`](geom_kodom_line.md) and
[`geom_kodom_heatmap()`](geom_kodom_heatmap.md).

## Usage

``` r
theme_kodom(base_size = 12, legend_position = "top")
```

## Arguments

- base_size:

  Base font size (default 12).

- legend_position:

  One of `"top"`, `"bottom"`, `"left"`, `"right"`, `"none"`.

## Value

A
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
object.

## Details

The vertical grid lines can be suppressed or restyled via the usual
ggplot2 [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
override:

    + theme(panel.grid.major.x = element_blank())

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
  geom_kodom_line() +
  scale_colour_kodom() +
  theme_kodom()

# }
```
