# CRAN Submission Action Items for ggkodom

## 0. Run `R CMD check --as-cran` First

Before anything else, run:

```r
devtools::check(args = "--as-cran")
```

or from terminal:

```bash
R CMD build .
R CMD check --as-cran ggkodom_0.1.0.tar.gz
```

**Must achieve**: 0 ERRORs, 0 WARNINGs, ≤1 NOTE (the "new submission" NOTE is expected and acceptable).
Fix everything this reports before proceeding.

---

## 1. BLOCKING — Must Fix Before Submission

### 1.1 Complete README.Rmd (or replace with README.md)

`README.Rmd` currently has placeholder text ("The goal of ggkodom is to ...") and an empty installation section.

Options:
- **Option A (recommended)**: Fill in `README.Rmd`, then render with `devtools::build_readme()` to produce `README.md`.
- **Option B**: Delete `README.Rmd`, write `README.md` directly, add `^README\.md$` removal from `.Rbuildignore`.

README must include at minimum:
- Package purpose (1–2 sentences).
- Installation instructions (CRAN once published; GitHub dev version via `remotes::install_github()`).
- One working code example with a plot.
- Badges (optional but conventional).

### 1.2 Verify LICENSE file exists (plain text, not just LICENSE.md)

DESCRIPTION says `License: MIT + file LICENSE`. CRAN expects a plain `LICENSE` file (no extension) with the copyright year and holder. The `.Rbuildignore` shows `LICENSE.md` is excluded from the build — confirm the bare `LICENSE` file is present:

```bash
ls ggkodom/LICENSE
```

If missing, create it:

```
YEAR: 2026
COPYRIGHT HOLDER: Ayoushman Bhattacharya, Subhrajyoty Roy, Sayan Das, Subrata Pal
```

### 1.3 Replace informal tests with formal `testthat` structure

`R CMD check --as-cran` expects tests to live in `tests/testthat/` using `test_that()` blocks. The current `tests/smoke_check.R` is not picked up automatically.

Steps:
1. Run `usethis::use_testthat()` — creates `tests/testthat.R` and `tests/testthat/` directory.
2. Convert the 26 smoke checks into testthat tests, e.g.:

```r
# tests/testthat/test-kodom_swimlane.R
test_that("kodom_swimlane path/linear returns a ggplot", {
  p <- kodom_swimlane(dat, id, time, value)
  expect_s3_class(p, "ggplot")
})

test_that("tile + polar is rejected with an error", {
  expect_error(kodom_swimlane(dat, id, time, value, geom = "tile", coord = "polar"))
})
```

3. Add `testthat (>= 3.0.0)` to `Suggests` in DESCRIPTION (already there — keep it).
4. Add `Config/testthat/edition: 3` to DESCRIPTION.

Keep `tests/smoke_check.R` and `tests/visual_check.R` for local dev use, but add them to `.Rbuildignore` so they don't confuse `R CMD check`.

### 1.4 `\dontrun{}` audit in examples

CRAN requires that all examples that are NOT wrapped in `\dontrun{}` must run without error in < 5 seconds on CRAN's servers (no network, limited memory). Review every `@examples` block:

- Wrap any example needing `lme4`, `mgcv`, `refund`, `fdapace`, or `patchwork` in `\dontrun{}` OR add a `requireNamespace()` guard inside the example.
- Synthetic data generation in examples is fine and preferred — keep them self-contained.
- For functions where examples can run fast and without optional deps (e.g., `kodom_swimlane`, `kodom_state`, `kodom_colors`, themes), make sure they run.

Run `devtools::run_examples()` locally to catch failures.

### 1.5 Check for `T`/`F` shorthand and `1:nrow()` anti-patterns

CRAN reviewers commonly flag:
- `T` / `F` instead of `TRUE` / `FALSE`.
- `1:length(x)` or `1:nrow(x)` (use `seq_along()` / `seq_len()` instead).
- `print()` calls left in package code.

Run:

```bash
grep -rn "\bT\b\|\bF\b" R/
grep -rn "1:nrow\|1:length\|1:ncol" R/
grep -rn "^print(" R/
```

---

## 2. STRONGLY RECOMMENDED — Will Improve Review Outcome

### 2.1 Add a vignette

CRAN reviewers expect a data viz package to demonstrate its use. A single vignette covering the core workflow is sufficient:

```r
usethis::use_vignette("ggkodom")
```

Suggested outline:
1. Load synthetic data (define inline or from a shipped dataset).
2. `kodom_swimlane()` — basic swimlane.
3. `kodom_smooth()` → `kodom_fit()` — fit overlay.
4. `kodom_components()` + `kodom_perturbation()` — FPCA interpretation.
5. `kodom_scores()` + `cluster_scores()` → re-ordered swimlane.

Add the vignette to `.Rbuildignore` only if it cannot run on CRAN (due to optional deps). Otherwise, add `Suggests: knitr, rmarkdown` and a `VignetteBuilder: knitr` line in DESCRIPTION.

If optional deps make it CRAN-unsafe, use:
```yaml
---
title: "..."
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{...}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
```
... and wrap sections using `lme4` etc. in `if (requireNamespace(...))` blocks.

### 2.2 Ship a small example dataset

Package currently has no `data/` directory. Functions in examples generate synthetic data inline, which is fine but less convenient for users.

Create a minimal dataset (≈50 subjects, 5–8 observations each, 2–3 clinical variables):

```r
# Creates data/ggkodom_example.rda and R/data.R with @docType data
usethis::use_data(ggkodom_example, compress = "xz")
```

Document it in `R/data.R`:

```r
#' Example longitudinal data
#'
#' A synthetic dataset of 50 subjects with repeated measures.
#'
#' @format A data frame with columns: `id`, `time`, `value`, `variable`, `group`.
"ggkodom_example"
```

This also lets you simplify the vignette and examples.

### 2.3 Add a `CITATION` file

Since this package emerged from WashU Datathon 2026 / academic research:

```r
usethis::use_citation()
# Creates inst/CITATION
```

Template content:

```r
bibentry(
  bibtype  = "Manual",
  title    = "ggkodom: Visualize Individual Longitudinal Trajectories",
  author   = c(
    person("Ayoushman", "Bhattacharya"),
    person("Subhrajyoty", "Roy"),
    person("Sayan", "Das"),
    person("Subrata", "Pal")
  ),
  year     = "2026",
  note     = "R package version 0.1.0",
  url      = "https://CRAN.R-project.org/package=ggkodom"
)
```

### 2.4 Spell-check documentation

```r
devtools::spell_check()
```

Add legitimate technical words (FPCA, swimlane, eigenfunction, etc.) to `inst/WORDLIST` to suppress false positives.

---

## 3. GOOD PRACTICE — Quality Polish

### 3.1 `urlchecker` — validate all URLs in documentation

```r
install.packages("urlchecker")
urlchecker::url_check()
```

Fix any broken or HTTP (non-HTTPS) links.

### 3.2 `rhub` / `win-builder` — test on multiple platforms

Before submitting, test on Windows and different R versions:

```r
# Test on Windows (CRAN's primary platform)
devtools::check_win_devel()
devtools::check_win_release()

# Test on multiple platforms via R-hub
rhub::check_for_cran()
```

These are free services. Fix any platform-specific issues (encoding, path separators, locale-sensitive behavior).

### 3.3 Increase test coverage

The smoke tests cover breadth (all functions, no errors). Add tests for correctness:

- `kodom_colors(1)` returns a single hex color.
- `kodom_colors(3)` returns exactly the 3 brand colors.
- `kodom_smooth(..., method = "lmer")$method == "lmer"`.
- `cluster_scores()` returns a factor with `k` levels.
- `kodom_sort_scores()` returns a named numeric.
- `fpca_align_sign()` dot product with reference is positive.

```r
covr::package_coverage()
```

Aim for > 70% line coverage. CRAN does not enforce this but reviewers notice low-coverage packages.

### 3.4 NEWS.md — document changes

Create `NEWS.md` at the package root:

```markdown
# ggkodom 0.1.0

* Initial CRAN release.
* `kodom_swimlane()` for path, tile, and polar trajectory plots.
* `kodom_fit()`, `kodom_residuals()`, `kodom_compare()` with S3 dispatch for lmer/gam/FPCA.
* `kodom_components()`, `kodom_perturbation()`, `kodom_scores()` for FPCA visualization.
* `kodom_smooth()` one-call smoother with bootstrap support via `fpca_boot_subjects()`.
* Multivariate support: `kodom_swimlane_multi()` and `kodom_multi()`.
```

### 3.5 Remove build artifacts from the package directory

Before submission, clean up:

```bash
# Remove check directory and built tarball from source tree
rm -rf ggkodom.Rcheck/
rm -f ggkodom_0.1.0.tar.gz
rm -f ggkodom-Ex.pdf
```

Add them to `.Rbuildignore`:
```
^ggkodom\.Rcheck$
^ggkodom.*\.tar\.gz$
^.*-Ex\.pdf$
^tests/visual_check\.pdf$
^tests/smoke_check\.R$
^tests/visual_check\.R$
^TODO\.md$
^info\.md$
```

### 3.6 Confirm no global variable warnings

Run:
```r
devtools::check_man()
globalVariables()  # should be empty or only declared intentionally
```

If `R CMD check` reports "no visible binding for global variable '...'", add a `utils::globalVariables()` call in `R/ggkodom-package.R` for any column names used with `.data$` in NSE contexts.

---

## 4. SUBMISSION PROCESS

### Step 1 — Final local check
```r
devtools::check(args = c("--as-cran", "--no-manual"))
# Must be: 0 errors, 0 warnings, ≤1 note
```

### Step 2 — Build the tarball
```r
devtools::build()
# Produces: ../ggkodom_0.1.0.tar.gz
```

### Step 3 — win-builder check
```r
devtools::check_win_devel()
```
Wait for email (~30 min) and fix any issues.

### Step 4 — rhub check
```r
rhub::check_for_cran()
```

### Step 5 — Submit to CRAN
```r
devtools::release()
# Interactive wizard: checks, builds, opens submission form
```

Or manually at: https://cran.r-project.org/submit.html

### Step 6 — CRAN submission checklist (manual review notes)

When submitting, CRAN will ask you to confirm:
- [ ] Version number is correct (0.1.0 for first submission).
- [ ] Package passes `R CMD check --as-cran` cleanly.
- [ ] Maintainer email in DESCRIPTION is monitored.
- [ ] All dependencies are on CRAN or Bioconductor (or listed as Suggests for non-CRAN packages).
- [ ] No installation of packages not listed in DESCRIPTION.
- [ ] No `library()` / `require()` calls in package code (use `requireNamespace()` instead).
- [ ] No `setwd()` in package code.
- [ ] Examples do not write to the user's file system (wrap file-writing demos in `\dontrun{}`).
- [ ] All internet access is gated (none expected in this package).

---

## 5. PRIORITY ORDER

| Priority | Item | Blocking? |
|----------|------|-----------|
| P0 | Run `R CMD check --as-cran` and fix all errors/warnings | Yes |
| P0 | Complete README and render to README.md | Yes |
| P0 | Verify LICENSE file exists (plain text) | Yes |
| P0 | Add formal `tests/testthat/` structure | Yes |
| P0 | Audit `\dontrun{}` in examples; run `devtools::run_examples()` | Yes |
| P1 | Check for T/F, 1:nrow() patterns in R/ | Soft |
| P1 | win-builder + rhub cross-platform checks | Soft |
| P1 | Spell-check with `devtools::spell_check()` | Soft |
| P1 | URL validation with `urlchecker::url_check()` | Soft |
| P1 | Create NEWS.md | Soft |
| P1 | Clean build artifacts from source tree | Soft |
| P2 | Add vignette | Recommended |
| P2 | Ship example dataset in `data/` | Recommended |
| P2 | Add `inst/CITATION` | Recommended |
| P2 | Increase test coverage (correctness assertions) | Recommended |
