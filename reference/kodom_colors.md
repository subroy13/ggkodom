# Kodom color palette

Returns colors inspired by the Kadam flower (*Neolamarckia cadamba*):
teal (low), gold (mid), red (high). Passing `n > 3` interpolates between
the three anchors via
[`grDevices::colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).

## Usage

``` r
kodom_colors(n = 3L)
```

## Arguments

- n:

  Number of colors. 3 returns the anchor set; \>3 interpolates.

## Value

Character vector of hex color codes.

## Examples

``` r
kodom_colors()
#> [1] "#008D98" "#FFCC3D" "#D7433B"
kodom_colors(7)
#> [1] "#008D98" "#55A279" "#AAB75B" "#FFCC3D" "#F19E3C" "#E4703B" "#D7433B"
```
