# Stat for geom_kodom_heatmap

Bins time into equal-width intervals, aggregates the `fill` aesthetic
within each (subject × bin) cell, and returns a data frame suitable for
[ggplot2::GeomTile](https://ggplot2.tidyverse.org/reference/Geom.html).
Called automatically by [`geom_kodom_heatmap()`](geom_kodom_heatmap.md);
rarely needed directly.

## Usage

``` r
StatKodomHeatmap
```

## Format

A ggproto object.
