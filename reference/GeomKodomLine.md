# Geom for geom_kodom_line

Extends
[ggplot2::GeomPath](https://ggplot2.tidyverse.org/reference/Geom.html)
with kodom defaults. Drops the `id` column in `setup_data()` so ggplot2
does not warn about an unknown aesthetic at draw time. Draws
interpolated colour-gradient segments via
[ggplot2::GeomSegment](https://ggplot2.tidyverse.org/reference/Geom.html)
and optional observation-point markers via
[ggplot2::GeomPoint](https://ggplot2.tidyverse.org/reference/Geom.html).

## Usage

``` r
GeomKodomLine
```

## Format

A ggproto object.

## Details

`size` and `linewidth` are intentionally routed to separate graphical
elements: `size` → points only, `linewidth` → path segments only.
