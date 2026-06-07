# Kodom fill scale for the fill aesthetic

Identical behavior to [`scale_color_kodom()`](scale_color_kodom.md) but
targets the `fill` aesthetic. Intended for use with
[`geom_kodom_heatmap()`](geom_kodom_heatmap.md).

## Usage

``` r
scale_fill_kodom(
  colors = kodom_colors(),
  color_breaks = NULL,
  name = ggplot2::waiver(),
  discretize = FALSE,
  ...
)
```

## Arguments

- colors:

  Color vector. Defaults to [`kodom_colors()`](kodom_colors.md).

- color_breaks:

  Numeric breakpoints anchoring the gradient via
  [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html).
  For `discretize = TRUE`, each break adds one band: `k` breaks produce
  `k + 1` bands. `NULL` = evenly spaced.

- name:

  Legend title. Defaults to `"value"`.

- discretize:

  `FALSE` (default) = smooth gradient; `TRUE` = step bands at
  `color_breaks`.

- ...:

  Ignored.

## Value

A ggplot2 scale object.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(df, aes(x = time, id = subject_id, fill = value)) +
  geom_kodom_heatmap() +
  scale_fill_kodom()
} # }
```
