# Stat for geom_kodom_branch

Assigns integer base lanes (one per subject) then stacks prediction arms
as fractional sub-lanes above the observed path within each subject's
lane.

## Usage

``` r
StatKodomBranch
```

## Format

A ggproto object.

## Details

Rows where `medication` is `NA` are the observed path; rows with a
non-NA `medication` value are prediction arms. The stat replaces `NA` in
`data$medication` with `"observed"` so that the linetype scale can be
driven directly from the medication column:

    scale_linetype_manual(
      values = c("observed" = "solid", "DrugA" = "dashed", "DrugB" = "dotted")
    )

Two internal columns — `kodom_lane` (integer lane rank) and
`kodom_track` (cleaned medication value) — are passed through to
`draw_panel` so that fork connectors can be placed at each subject's
branch point.
