# Kodom color scale for the color aesthetic

A ggplot2 color scale using the Kadam flower palette (teal -\> gold -\>
red). Designed to compose with
[`geom_kodom_line()`](geom_kodom_line.md),
[`geom_kodom_heatmap()`](geom_kodom_heatmap.md), and
[`geom_kodom_circular()`](geom_kodom_circular.md).

## Usage

``` r
scale_color_kodom(
  colors = kodom_colors(),
  color_breaks = NULL,
  name = ggplot2::waiver(),
  discretize = FALSE,
  ...
)

scale_colour_kodom(
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

## Details

Set `discretize = TRUE` to switch from a smooth gradient to solid color
bands separated at `color_breaks` — useful when values change slowly and
a continuous gradient washes out into a single hue.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(df, aes(x = time, id = subject_id, color = value)) +
  geom_kodom_line() +
  scale_color_kodom()

# Discrete bands at clinical thresholds
ggplot(df, aes(x = time, id = subject_id, color = hba1c)) +
  geom_kodom_line() +
  scale_color_kodom(discretize = TRUE, color_breaks = c(5.7, 6.5))
} # }
```
