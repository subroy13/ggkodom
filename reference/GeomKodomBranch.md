# Geom for geom_kodom_branch

Extends
[ggplot2::GeomPath](https://ggplot2.tidyverse.org/reference/Geom.html)
to draw branching trajectory plots. In addition to the interpolated
colour-gradient path (via `.kodom_build_segments()`), draws a short
vertical **fork connector** at each subject's branch point — the first x
that appears in a predicted arm — connecting the observed lane to the
topmost prediction arm. Toggle with `show_fork`.

## Usage

``` r
GeomKodomBranch
```

## Format

A ggproto object.
