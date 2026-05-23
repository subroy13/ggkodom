# ggkodom — info

Named after কদম ফুল (Kadam flower, *Neolamarckia cadamba*) — the circular trajectory plot, when viewed as radial spokes from a center, resembles the flower's spherical head of radiating stamens.

## Origin

Born from the WashU I2DB Datathon 2026. Ayoushman Bhattacharya wrote the original A1c trajectory visualization (`scripts/eda_ab_kodom.R`) for the presentation: each patient = a horizontal row, x = days from baseline, color = A1c value. Generalized into a package by Subrata Pal.

## What it does

A small ggplot2-based toolkit for individual-level longitudinal data, with optional FDA hooks (lmer / GAM / FPCA) so model fits live in the same visual idiom as the raw data.

### Trajectory plots

| Function | Description |
|---|---|
| `kodom_swimlane()` | Horizontal lanes per subject, colored by value. Three layouts via `geom`/`coord`: `geom = "path"` (default), `geom = "tile"` (binned heatmap for large N), `coord = "polar"` (the "kodom" flower). Polar supports `arc_degrees` for fan layouts (e.g. 270, 180). |
| `kodom_swimlane_multi()` | **(stack view)** Stacked swimlane, one panel per variable, lanes share an ordering computed from a chosen anchor variable. Per-variable color scales. Patchwork composition. |
| `kodom_multi()` | **(polar view)** Multivariate kodom — each subject = one spoke, spoke split into J radial bands (one per variable). Time radial within band. Color = z-score per variable on a single gradient. Genuine multivariate extension of the flower. |
| `kodom_state()` | Like swimlane but segments colored by discrete state (threshold-based). Thin wrapper-style helper. |

### ggplot2 extension layer (composable API)

| Function | Description |
|---|---|
| `geom_kodom()` | ggplot2 geom: one path per subject, sorted/sampled via `StatKodom`. Returns layer + `scale_y_discrete()`. Compose with `+` like any ggplot2 geom. |
| `geom_kodom_tile()` | Heatmap variant: tiles binned by `time_bins`/`time_breaks`, aggregated by `agg_fun`. Uses `fill` aesthetic. |
| `stat_kodom()` | Standalone stat for pairing kodom sorting/sampling with any geom (e.g. `geom_path(stat = "kodom")`). |
| `scale_color_kodom()` / `scale_colour_kodom()` | Continuous color scale wrapping the kodom teal→gold→red palette. `discretize = TRUE` for step bands. |
| `scale_fill_kodom()` | Same as above, for the fill aesthetic. |
| `StatKodom` | ggproto Stat — subject sampling (`n_sample`), sorting (`sort_by`), y-factoring. All logic in `setup_data()`. |
| `StatKodomTile` | ggproto Stat extending StatKodom — adds time binning (`time_bins`/`time_breaks`) and value aggregation (`agg_fun`). |
| `GeomKodomPath` | ggproto Geom extending GeomPath — path + optional observation points (`point_size`). |
| `GeomKodomTile` | ggproto Geom extending GeomRect — tiles with white borders, computes rect boundaries from factor y. |

The earlier `kodom_circular()` and `kodom_heatmap()` collapsed into `kodom_swimlane()`.

### FDA / model-fit views

| Function | Description |
|---|---|
| `kodom_fit()` | Smoothed swimlanes from a fit object: methods for `lmerMod`, `glmerMod`, `gam`, `refund::fpca.sc`, `fdapace::FPCA`. |
| `kodom_residuals()` | Same fit classes as `kodom_fit()`, but plots observed − predicted on a divergent (blue / grey / red) scale centered at 0. The "what did the model miss" view. |
| `kodom_compare()` | Raw vs. fitted curves, two layouts: `"overlay"` (default) puts raw points on top of fit lines on a single lane set (residual = color mismatch); `"side"` puts them in two facets sharing lane ordering and color scale. |
| `kodom_smooth()` | One-call smoother (`method = c("fpca","lmer","gam")`). Returns `list(fit, refit_fn, method, call_args)` — `refit_fn` plugs straight into `fpca_boot_subjects()`. |
| `kodom_components()` | Eigenfunctions \(\phi_k(t)\) from an FPCA fit, with optional bootstrap envelopes. |
| `kodom_perturbation()` | Ramsay–Silverman perturbation: \(\mu(t) \pm c\sqrt{\lambda_k}\,\phi_k(t)\) per panel. |
| `kodom_scores()` | Scatter of FPC scores \(\xi_1, \xi_2\) — color by group, cluster, or any per-subject vector. |
| `kodom_scores_glyph()` | FPC score scatter where each point becomes a tiny multivariate sparkline glyph (time → small-x, value → small-y, color → variable). Read each subject's life at their location in PC space. |
| `kodom_sort_scores()` | Build a named-numeric sort key from any FPC, to pass into `sort_by =` in the trajectory plots. |
| `cluster_scores()` | k-means or Ward.D2 on the FPC score matrix; returns a named factor for `groups =` / `facet_rows =`. |
| `fpca_boot_subjects()` | Subject-level (cluster) bootstrap for sparse FDA. Pair with `kodom_components(boot_fits = ...)` for visual envelopes. |
| `fpca_align_sign()` | Flip eigenfunction signs to match a reference fit. |

All trajectory functions accept a long-format data frame with (id, time, value) columns. All return a ggplot object.

## Design decisions

- **String column names** (not NSE/tidyeval) for the API — simpler, no quoting headaches.
- **Base R internally** — only hard dependencies are ggplot2, rlang (`.data`), scales. lme4/mgcv/refund/fdapace are Suggests.
- **S3 dispatch** for fit-aware functions (`kodom_fit`, `kodom_components`, `kodom_perturbation`, `kodom_scores`) so adding a new backend means writing one method, not editing the drawing code.
- **`discretize` flag** on swimlane / fit (and the underlying `kodom_value_scale`): smoothed-fit colors otherwise wash out into a single hue. Stepped bands at clinical thresholds keep the picture readable.
- **Subject-level (cluster) bootstrap**, not observation-level — the only correct resampling unit for sparse irregular longitudinal data. Eigenfunction sign-aligned against the reference fit before drawing.
- **Circular ordering problem**: angular layout sorts subjects clockwise; `gap_fraction` leaves an empty arc at the wrap point so the seam is visible. `arc_degrees` (default 360) lets the user collapse the layout to a fan (e.g. 270, 180) — the missing arc IS the gap, so ordering is preserved without a wrap seam.
- **Architecture: moving from wrappers to ggplot2 grammar.** Started with wrappers (`kodom_swimlane()` etc.) that return complete ggplots. Now graduating to `geom_kodom()` + `scale_color_kodom()` + `theme_kodom()` composable API. Long-term direction: only `geom_*` / `scale_*` / `stat_*` / `theme_*` names are exported; wrappers become internal or deprecated.

## Package structure

```
R/
  ggkodom-package.R    # package doc + .data import
  utils.R              # prep_trajectory_data(), check_columns(), build_facet()
  colors.R             # kodom_colors(), kodom_gradient_scale(), kodom_step_scale(), scale_color_kodom(), scale_fill_kodom()
  theme_kodom.R        # theme_kodom(), theme_kodom_circular()
  geom_kodom.R         # StatKodom, GeomKodomPath, geom_kodom(), stat_kodom()
  kodom_swimlane.R     # path/tile × linear/polar (workhorse)
  kodom_state.R        # discrete-state ribbon
  kodom_fit.R          # generic + lmer/gam/FPCA methods → smoothed swimlanes
  kodom_components.R   # FPCA eigenfunctions (with bootstrap envelopes)
  kodom_perturbation.R # μ ± c√λ φ
  kodom_scores.R       # FPC score scatter + kodom_sort_scores helper
  cluster_scores.R     # k-means / hclust on FPC scores
  fpca_boot.R          # subject-level cluster bootstrap + sign alignment
```

## Roadmap

Concrete next adds (planned, in order):
1. `kodom_residuals(fit, data)` — raw lane minus fit lane, same color scale.
2. `kodom_compare(fit, data)` — side-by-side raw + reconstruction, sortable by score.
3. `kodom_smooth(data, method = c("fpca", "lmer", "gam"))` — one-call smoother that returns a fit AND a default `refit_fn` for `fpca_boot_subjects()`.

Interested-but-not-yet:
- Functional boxplot / outlier highlighting on the lane view (Sun & Genton 2011).
- Missingness layer (per-subject observation-density strip).
- Linked plotly/crosstalk between `kodom_scores` and `kodom_swimlane`.

Maybe later:
- Fan layout (partial circle, e.g. 270°) to preserve ordering without wrap. *(arc_degrees already supported in kodom_swimlane)*
- Built-in example dataset.
- Vignette with clinical examples.

### Done (moved from roadmap)
- ~~Custom geoms (`geom_swimlane`, `geom_kodom`) for full ggplot2 composability.~~ → `geom_kodom()`, `stat_kodom()`, `scale_color_kodom()` (2026-05-22).
