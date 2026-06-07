# Stat for geom_kodom_periodic

Assigns lane positions and converts lane rank to an Archimedean spiral
radius
(`y = inner_radius + lane * lane_width + spiral_fraction * total_time / period`).
Time `x` is left continuous, and relies on `coord_polar(theta = "x")` to
natively wrap the data in a 360 degree space for values extending past
the `period`.

## Usage

``` r
StatKodomPeriodic
```

## Format

A ggproto object.
