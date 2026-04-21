# ggkodom — TODO

Persistent task list. Survives conversation compactification. Update as you finish items.

## Plan agreed (2026-04-20)

Refactor first, then add features. Roadmap based on go-away/merge ideas + concrete adds (1–3) Subrata picked. Items 4–6 deferred but listed.

## Done

- [x] **(a) Merge `kodom_heatmap` and `kodom_circular` into `kodom_swimlane`.**
  Single function with `geom = c("path","tile")` × `coord = c("linear","polar")`.
  Internal drawers: `draw_swimlane_path`, `draw_swimlane_tile`, `draw_swimlane_polar`.
  Tile + polar errors out (sunburst not supported).
  Deleted: `R/kodom_heatmap.R`, `R/kodom_circular.R`, `man/kodom_heatmap.Rd`, `man/kodom_circular.Rd`.
  `NAMESPACE` cleaned. `info.md` updated. `kodom_state` left as thin wrapper (per Subrata).
- [x] **(b) Share components/perturbation drawer.**
  New `R/fpca_helpers.R`: `fpca_extract(fit)`, `make_pc_label()`, `pc_label_factor()`.
  Old `fpca_scores_matrix` / `fpca_evalues` / `fpca_ids` (in `kodom_scores.R`) now thin wrappers around `fpca_extract`.
  `kodom_components.R` and `kodom_perturbation.R` each have one `_impl()` that calls `fpca_extract`; `.fpca` / `.fpca.sc` / `.FPCA` methods are one-liners.
- [x] **Smoke check.** `tests/smoke_check.R` — 26 OK across (a)–(e) plus the `kodom_state` regression. Re-run with `Rscript tests/smoke_check.R`.
- [x] **(c) `kodom_residuals(fit, data)`.** S3 generic. Methods for `lmerMod`, `glmerMod`, `gam`, `fpca`/`fpca.sc`, `FPCA`. Predicts at observation times (interpolating onto each subject's actual times for FPCA via `stats::approx`), plots a swimlane on a divergent blue/grey/red scale centered at zero. Defaults: `signed_breaks = c(-2, 0, 2)`, sort by mean residual.
- [x] **(d) `kodom_compare(fit, data)`.** Side-by-side `raw` and `fit` panels via `facet_wrap(~ .panel)` (or `facet_grid(facet_rows ~ .panel)` if a cluster facet is supplied). Internal S3 helper `compare_bits()` extracts (raw, fit-grid) per fit class so the drawer is class-agnostic. Single `prep_trajectory_data` call on the raw side fixes lane order; fit grid is filtered + factor-leveled to match.
- [x] **(e) `kodom_smooth(data, method = c("fpca","lmer","gam"))`.** Returns `list(fit, refit_fn, method, call_args)`. Defaults: `fdapace::FPCA(... dataType = "Sparse")`, `lmer(value ~ time + (1+time|id))`, `gam(value ~ s(time, id, bs="fs", k=4))`. `refit_fn` closes over the same options for clean bootstrap roundtrip with `fpca_boot_subjects()`.
- [x] **Visual check.** `tests/visual_check.R` — single multi-page PDF demoing every function with structured 3-cohort synthetic data. Re-run with `source("tests/visual_check.R")`.
- [x] **Polar fan layout (`arc_degrees`).** `kodom_swimlane(coord = "polar", arc_degrees = 270)` collapses the circle to a fan; the missing arc replaces the wrap-seam gap. Default 360 = full circle (back-compat).
- [x] **Thinner default lines.** `kodom_swimlane`: 0.8 → 0.5. `kodom_state`: 1.2 → 0.7. Per Subrata, all defaults felt heavy.
- [x] **`kodom_swimlane_multi()` — multivariate functional swimlane.** Long-format input with a `variable` column. Stacks one swimlane per variable via patchwork, all lanes ordered by a chosen anchor variable (e.g. sort by a1c, see bmi follow). Per-variable color scales (so a1c and bmi don't share a gradient). Lanes are kept (`drop = FALSE`) so missingness is visible. New Suggests: `patchwork`. The MFPCA-fit-aware companions (kodom_components_multi etc.) are a separate item — see (7) below.
- [x] **`kodom_compare(layout = c("overlay", "side"))` — overlay default.** Overlay puts raw points (white-filled, value-colored stroke) on top of the fit path on a single lane set: residuals read off as color mismatch between point and underlying line. `layout = "side"` is the original two-facet view, kept for cases where overlay clutters.
- [x] **`kodom_multi()` — polar concentric multivariate (prototype).** Each subject = one spoke; spoke split into J radial bands, one per variable. Time radial within each band. Color = per-variable z-score on a divergent gradient (so a1c, bmi, ldl share one scale meaningfully). Optional band-boundary rings + variable labels at the seam. Supports `arc_degrees` for fan layout. Subrata flagged `kodom_swimlane_multi` as "just faceting" — `kodom_multi` is the kodom-flavored alternative to A/B test against it.
- [x] **`kodom_scores_glyph()` — sparkline-glyph score scatter (prototype).** FPC score scatter where each point becomes a tiny line glyph showing the subject's actual trajectory; multivariate input (long with `variable`) renders one colored line per variable inside each glyph. "Each patient's whole life at their PC location." Glyph size = fraction of score-axis range (default 5%).

## Next (in order)

(All of c/d/e done as of 2026-04-20. Open queue starts at 4.)

## Deferred but interested (4–7)

- [ ] **(4) Functional boxplot / outlier highlighting on the lane view.** Sun & Genton (2011) style — central 50% band, outlier lanes flagged. Probably needs MBD (modified band depth).
- [ ] **(5) Missingness layer.** Per-subject observation-density strip rendered next to the lanes. Critical for sparse cohorts where the gaps *are* the signal (the A1c data is exactly this).
- [ ] **(6) Linked plotly/crosstalk: `kodom_scores` ↔ `kodom_swimlane`.** Click a point in PC space, highlight that lane. Cheap with htmlwidgets, very high impact for exploration.
- [ ] **(7) MFPCA-aware fit views — multivariate functional companions to chunk (b).**
  - `kodom_swimlane_multi()` (the visualization-only entry point) is already done. Next layer is the MFPCA *fit-aware* generics: `kodom_components_multi()`, `kodom_perturbation_multi()`, `kodom_scores_multi()`, plus an `mfpca_extract()` adapter.
  - **Backend recommendation:** start with `refund::mfpca.sc` as the primary (sparse-friendly, mirrors the existing `refund::fpca.sc` idiom — minimum new code in the adapter). Add `MFPCA::MFPCA` (Happ-Kurz JSS 2018) as the dense / general-purpose secondary; it operates on `multiFunData` objects and supports each variable on its own basis (FPCA / spline / image), which is the right machinery if anyone wants to go beyond pointwise multivariate.
  - **Data structure to expose via `mfpca_extract()`:** `list(mu_j, phi_jk, lambdas, scores, ids, args_j, backend)`, where `mu_j` is a list of mean curves indexed by variable, `phi_jk` is a 3-tensor (variable × time × PC), `lambdas` is shared across variables, `scores` is N × K (subject by PC), `args_j` is the time grid per variable.
  - **`kodom_components_multi()`** then plots a K × J grid (rows = PCs, columns = variables) so "PC1 loads positively on a1c late, negatively on bmi early" reads off directly.
  - **Open question for later:** how to render bootstrap envelopes when each variable can have a different time grid. Probably interpolate to a common grid before sign-aligning.

## Maybe later

- Custom geoms (`geom_swimlane`, `geom_kodom`) for full ggplot2 composability.
- Built-in example dataset.
- Vignette stitching raw → fit → components → scores → cluster → re-ordered lanes.
- Tests so FPCA code doesn't break on fdapace updates.
- README.

## Carried-over caveats

- **Run `devtools::document()` in RStudio** before `load_all()`. Stale .Rd files: `kodom_swimlane.Rd` (signature changed substantially in chunk (a)); component/perturbation method-dispatch Rds may also need refresh.
- `tile + polar` (sunburst) is intentionally rejected. If we want it, it's a real layout decision (radius = id-rank? angle = bin-index?), not just a switch.
- Bootstrap envelopes only render when the bootstrap fit's grid matches the main fit's grid. `collect_boot_phi()` silently drops mismatches. Document this in the eventual vignette.
- `kodom_state.R` was kept as-is (thin wrapper status). If we want to fold it into `kodom_swimlane(discretize=TRUE)` later, the only blocker is the per-segment color (state of segment start) vs. ggplot's vertex-color interpolation along `geom_path`.
- `kodom_fit.fpca` / `.FPCA` were NOT migrated to `fpca_extract()` in chunk (b). Their reconstruction logic is genuinely different (`fit$Yhat` for refund, `xi %*% t(phi) + mu` with K-truncation for fdapace). Worth a future pass — would need `fpca_extract` to expose `Yhat` (or compute on demand for fdapace).
- The "67% F1" figure from the datathon is in-sample; the honest val ceiling is ~62%. Not a ggkodom issue but matters if we use the A1c story in a vignette — don't quote 67% as out-of-sample.

## Conventions reminders

- Section headers: `#### HEADING ####`.
- ~150–200 char width, no hard wrap at 80.
- String column names (no NSE/tidyeval) in user-facing API.
- Base R internally; lme4 / mgcv / refund / fdapace are Suggests, gated by `requireNamespace()`.
- Add a new FPCA backend by extending `fpca_extract()` only.
