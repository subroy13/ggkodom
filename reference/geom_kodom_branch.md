# Branching swimlane plot for observed paths and counterfactual predictions

Draws one horizontal path per subject for the observed trajectory, then
fans out into sub-lanes — one per medication or intervention arm — for
predicted (counterfactual) trajectories. The branching time may differ
across subjects.

## Usage

``` r
geom_kodom_branch(
  mapping = NULL,
  data = NULL,
  stat = StatKodomBranch,
  position = "identity",
  ...,
  sort_by = "none",
  n_max = Inf,
  lane_width = 1,
  branch_fraction = 0.7,
  show_points = TRUE,
  show_fork = TRUE,
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

- lane_width:

  Positive numeric. Vertical distance between adjacent subject lanes.
  Default `1`.

- branch_fraction:

  Fraction of `lane_width` devoted to prediction sub-lanes. Default
  `0.7`. With K arms the sub-lane step is `branch_fraction / K`.

- show_points:

  If `TRUE` (default), draws a point at every observation. Set to
  `FALSE`, or map `shape = NA` / `size = 0`, to suppress points.

- show_fork:

  Logical. If `TRUE` (default), draws a vertical fork connector at each
  subject's branch point.

- na.rm:

  If `TRUE`, silently remove rows with missing required aesthetics.

- show.legend:

  Logical. Should this layer appear in the legend?

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

## Value

A ggplot2 layer object.

## Details

**Data format.** Supply `NA` in the `medication` column for all observed
rows (pre- and post-branch). Supply a non-NA arm label (e.g. `"DrugA"`)
for every predicted row. The stat replaces `NA` with `"observed"` in its
output so that the linetype scale receives a clean string for every row.

**Linetype.** Map `linetype = <arm_column>` in
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) **and**
supply a
[`scale_linetype_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
so that the legend appears and the linetypes are exactly what you want.
The stat converts `NA` (observed rows) to the string `"observed"` before
the scale is applied, so target that key explicitly:

    aes(linetype = arm, medication = arm)   # both point to the same column
    scale_linetype_manual(
      values = c("observed" = "solid", "DrugA" = "dashed", "DrugB" = "dotted")
    )

**Fork connector.** A short vertical segment is drawn at each subject's
branch point (the first x that appears in a predicted arm), connecting
the observed lane to the topmost arm. Suppress with `show_fork = FALSE`.

**Lane layout.** Each subject occupies a primary band of width
`lane_width`. Within that band the observed path is at the base; each
prediction arm sits at an equal sub-lane fraction above it controlled by
`branch_fraction`. With two arms and `branch_fraction = 0.7`, arm 1 is
at `0.35 * lane_width` above the base and arm 2 is at
`0.70 * lane_width`, leaving 30% clearance before the next subject.

**Y-axis labels.** Subject IDs can be added by setting custom breaks:

    scale_y_continuous(
      breaks = seq_len(n_subjects) * lane_width,
      labels = subject_ids
    )

## Aesthetics

- **`x`** — time (numeric or Date)

- **`id`** — subject identifier; determines the primary lane position

- **`colour`** — measured value mapped to colour (interpolated along
  path)

- `medication` — `NA` for observed rows; a character/factor arm label
  for predicted rows. Each unique non-NA value becomes one sub-lane.

- `size`, `linewidth`, `alpha`, `shape`, `stroke` — standard path/point
  aesthetics. `linetype` is set by the stat (medication name /
  `"observed"`).

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(df, aes(
  x = time, id = subject_id,
  colour = hba1c, linetype = arm, medication = arm
)) +
  geom_kodom_branch(sort_by = "mean", lane_width = 2) +
  scale_linetype_manual(
    values = c("observed" = "solid", "DrugA" = "dashed", "DrugB" = "dotted")
  ) +
  scale_colour_kodom() +
  theme_kodom()
} # }
```
