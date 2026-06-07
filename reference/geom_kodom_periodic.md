# Periodic "Star Trail" swimlane plot for longitudinal trajectories

Draws concentric arcs representing cyclical longitudinal data. Time maps
to angle (one full revolution equals one `period`), and each subject's
base radius is determined by its lane rank. To separate successive
cycles of the same subject the radius drifts outward slightly as total
time increases, forming a star trail or Archimedean spiral.

## Usage

``` r
geom_kodom_periodic(
  mapping = NULL,
  data = NULL,
  stat = StatKodomPeriodic,
  position = "identity",
  ...,
  period = 12,
  spiral_fraction = 0.1,
  inner_fraction = 0.3,
  lane_width = 1,
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

- period:

  Numeric. Length of one complete cycle (e.g. 12 for months, 24 for
  hours). Default `12`.

- spiral_fraction:

  Numeric. Radial expansion per full cycle as a fraction of one
  *original* lane width (i.e. before `lane_width` scaling). `0` keeps
  cycles on the same ring; `0.1` (default) drifts outward by 10% of one
  lane per cycle.

- inner_fraction:

  Fraction of total lanes used as a hollow inner buffer. Default `0.3`.
  Pair with [`scale_y_kodom_periodic()`](scale_y_kodom_periodic.md) to
  make the hole visible.

- lane_width:

  Positive numeric. Multiplier for the radial distance between adjacent
  subject rings. Default `1` packs rings at unit spacing. Increase (e.g.
  `3`) to spread rings apart so each subject's arc is more legible —
  particularly useful when using `n_max` to show a small subset. The
  hollow center (`inner_fraction`) is intentionally not scaled so the
  hole size stays proportional to the cohort, not the spacing.

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

## Details

**Coordinate system.** This geom is designed for use with
`coord_polar(theta = "x")`. Use
[`coord_kodom_periodic()`](coord_kodom_periodic.md) as a convenient
wrapper that sets `start = pi/2` (12 o'clock at `x = 0`) and
`direction = -1` (clockwise) by default.

**Axis labels.** Because `x` is continuous time, `ggplot2` will
automatically expand the limits to cover the entire time range if you
use standard scales, which alters the length of a full 360-degree
rotation. To make one rotation exactly equal one `period`, use the
included wrapper
[`scale_x_kodom_periodic()`](scale_x_kodom_periodic.md):

    scale_x_kodom_periodic(period = 12, breaks = 1:12, labels = month.abb)
