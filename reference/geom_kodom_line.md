# Swimlane line plot for longitudinal trajectories

Draws one horizontal colored path per subject. Time maps to `x`, subject
identity maps to `id` (which the stat converts to integer lane positions
on the `y` axis), and a measured value maps to `colour`. The colour
transitions smoothly between consecutive observations via interpolated
sub-segments.

## Usage

``` r
geom_kodom_line(
  mapping = NULL,
  data = NULL,
  stat = StatKodomLine,
  position = "identity",
  ...,
  sort_by = "none",
  n_max = Inf,
  show_points = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

- data:

  A data frame. If `NULL`, inherits from the plot.

- stat:

  The stat to use. Defaults to [StatKodomLine](StatKodomLine.md).

- position:

  Position adjustment, almost always `"identity"`.

- ...:

  Other arguments passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- sort_by:

  Lane ordering. One of `"none"` (default), `"mean"`, `"mean_asc"`,
  `"first"`, `"last"` — all refer to the `colour` variable.

- n_max:

  Maximum number of subjects to display. A random sample is taken when
  exceeded. Default `Inf` (all subjects).

- show_points:

  If `TRUE` (default), draws a point at every observation. Set to
  `FALSE`, or map `shape = NA` / `size = 0`, to suppress points.

- na.rm:

  If `TRUE`, silently remove rows with missing required aesthetics.

- show.legend:

  Logical. Should this layer appear in the legend?

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

## Value

A ggplot2 layer object.

## Details

Lane ordering is controlled by `sort_by`. With many subjects, consider
suppressing y-axis labels via `theme(axis.text.y = element_blank())`.

## Aesthetics

Required aesthetics are shown in **bold**. `size` and `linewidth` are
intentionally independent: each controls a different graphical element.

- **`x`** — time variable (numeric or Date)

- **`id`** — subject identifier; determines lane position on the y axis

- `colour` — color of both the connecting path and the point border

- `fill` — point interior color for filled shapes (21–25); no effect on
  path

- `size` — **point size only**; set to `NA` or `0` to suppress points

- `linewidth` — **path width only**; does not affect observation points

- `alpha` — transparency applied to both path and points

- `shape` — point shape (default 19). Set to `NA` to suppress points.

- `stroke`, `linetype` — point border width / path line type

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
ggplot(df, aes(x = time, id = subject_id, colour = hba1c)) +
  geom_kodom_line(sort_by = "mean", n_max = 50) +
  scale_colour_kodom() +
  theme_kodom()

# }
```
