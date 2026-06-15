# Circular ("Kodom") swimlane plot for longitudinal trajectories

Draws a radial plot where each subject occupies an angular spoke and
time increases outward from the center. Measurements are encoded as a
colour gradient along each radial path, interpolated between consecutive
observations. The layout resembles a Kadam flower (*Neolamarckia
cadamba*), giving the package its name.

## Usage

``` r
geom_kodom_circular(
  mapping = NULL,
  data = NULL,
  stat = StatKodomCircular,
  position = "identity",
  ...,
  sort_by = "none",
  n_max = Inf,
  gap_fraction = 0.15,
  inner_fraction = 0.3,
  direction = 1L,
  show_points = TRUE,
  reference_rings = c(0),
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

  The stat to use. Defaults to
  [StatKodomCircular](StatKodomCircular.md).

- position:

  Position adjustment, almost always `"identity"`.

- ...:

  Other arguments passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- sort_by:

  Lane ordering (angular order). One of `"none"` (default), `"mean"`,
  `"mean_asc"`, `"first"`, `"last"`.

- n_max:

  Maximum number of subjects to display. Default `Inf`.

- gap_fraction:

  Fraction of the full circle left empty as a visual gap at the ordering
  seam. Default `0.15`.

- inner_fraction:

  Fraction of the time range used as a hollow inner buffer so
  short-follow-up subjects remain visible. Default `0.3`.

- direction:

  `1L` = clockwise (default), `-1L` = counter-clockwise.

- show_points:

  If `TRUE` (default), draws a point at every observation. Set to
  `FALSE`, or map `shape = NA` / `size = 0`, to suppress points.

- reference_rings:

  Numeric vector of time points where concentric reference rings should
  be drawn behind the data paths. By default, draws a ring at `0`
  (baseline). Set to `NULL` to suppress rings.

- na.rm:

  If `TRUE`, silently remove rows with missing required aesthetics.

- show.legend:

  Logical. Should this layer appear in the legend?

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

## Value

A ggplot2 layer object.

## Details

Unlike the original
[`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
variant in the archive package, this geom performs the
polar-to-Cartesian transform inside the stat and renders in ordinary
Cartesian space, so all ggplot2 scales, themes, and facets work
normally. Use
[`coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)
to preserve the circular shape.

## Aesthetics

Required aesthetics are shown in **bold**. `size` and `linewidth` are
intentionally independent: each controls a different graphical element.

- **`x`** — time variable (numeric); mapped to radial distance

- **`id`** — subject identifier; mapped to angular position

- `colour` — color of both the connecting path and the point border

- `fill` — point interior color for filled shapes (21–25)

- `size` — **point size only**; set to `NA` or `0` to suppress points

- `linewidth` — **path width only**

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
  geom_kodom_circular(sort_by = "mean", gap_fraction = 0.1) +
  scale_colour_kodom() +
  coord_fixed() +
  theme_kodom_circular()

# }
```
