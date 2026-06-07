# TODO List 

<!-- Never let any LLM touch this -->
<!-- Only touch this file if you are a human and you know what you are doing! -->


- [x] Create the startup message.
- [x] Check if the circular plot should be circular because time / year repeat is circular.
- [x] Build a tree-type plot that shows forks at different places showing some sort of event timeline. 
- [x] Fix non-ASCII characters in R/zzz.R by converting literal Bengali text to Unicode escape sequences.
- [x] Regenerate package documentation (run devtools::document()) to sync show_fork and other code changes with .Rd files.
- [x] Resolve NOTE about grid package imported but not used in NAMESPACE (e.g., add importFrom or clean description).

## CRAN Submission Checklist (Short)

- [x] **Description File**: Title is Title Case, Description is a proper paragraph ending in a period, single quotes for package names, no "This package...".
- [x] **Authors**: `Authors@R` correctly formatted with at least one `[cre, aut]`.
- [ ] **License**: Standard license (e.g., `MIT + file LICENSE`).
- [ ] **Local Checks**: Run `devtools::spell_check()`, `devtools::check(remote = TRUE, manual = TRUE)`, and rebuild `README.md`.
- [ ] **Cross-OS Remote Checks**:
  - Windows: `devtools::check_win_devel()`
  - macOS: `devtools::check_mac_release()`
  - Linux/Others: `rhub::check_for_cran()`
- [ ] **Documentation**: Update `NEWS.md` for the release.
- [ ] **CRAN Comments**: Create `cran-comments.md` outlining the test environments and 0 errors/warnings/notes results.
- [ ] **Submission**: Run `devtools::submit_cran()` and confirm the email link.


# CRAN Submission Checklist (Detailed)

Even though `devtools::check(cran = TRUE)` passes with 0 errors, warnings, and notes locally, CRAN has strict policies and requires testing across multiple operating systems. Follow this checklist before submitting.

## 1. Description File Checks
- [x] **Title**: Must be in Title Case (e.g., "Circular and Periodic Visualizations for Longitudinal Data").
- [x] **Description**: 
  - Should be a paragraph of text.
  - Do NOT start with "A package to..." or "This package...".
  - If you mention other packages, put them in single quotes (e.g., 'ggplot2').
  - Ensure it ends with a period.
- [x] **Authors**: Ensure `Authors@R` is correctly formatted, with at least one `[cre, aut]` (creator/maintainer). Consider adding ORCID IDs.
- [x] **License**: Make sure the license is a standard one (e.g., `MIT + file LICENSE`, `GPL-3`).

## 2. Comprehensive Local Checks
- [x] Run `devtools::spell_check()`. CRAN is very strict about spelling in the DESCRIPTION and documentation.
- [x] Run `devtools::check(remote = TRUE, manual = TRUE)` to check for remote URLs and build PDF manuals.
- [x] Rebuild `README.md` if you use `README.Rmd` (`devtools::build_readme()`).

## 3. Remote Checks (CRAN environments)
CRAN will test your package on Windows, macOS, and Linux (Debian/Fedora). You must verify it works on all of them.
- [x] **Windows**: Run `devtools::check_win_devel()`. This sends an email with the build results on Windows. (You can also run `check_win_release()`).
- [x] **macOS**: Run `devtools::check_mac_release()`.
- [x] **R-hub (Linux & Others)**: Run `rhub::check_for_cran()`. This runs the package against multiple CRAN-like platforms.

## 4. Prepare Documentation
- [x] **NEWS.md**: Add a section for the new version (e.g., `# ggkodom 0.1.0 \n * Initial CRAN release.`).
- [x] **cran-comments.md**: Create a `cran-comments.md` file in the root directory. This is sent to the CRAN volunteers. Use this template:
  ```markdown
  ## Test environments
  * local macOS install, R 4.3.0
  * ubuntu 20.04 (on rhub), R-release, R-devel
  * win-builder (devel and release)

  ## R CMD check results
  0 errors | 0 warnings | 0 notes

  ## Downstream dependencies
  There are currently no downstream dependencies. (Since this is a new release)
  ```

## 5. Submission
- [ ] Once all remote checks come back green, build the tarball.
- [ ] Run `devtools::submit_cran()`.
- [ ] **IMPORTANT**: You will receive an email asking you to confirm your submission. Click the link in the email to finalize it!


