# Subrata's Changes (to merge into changes.md)

## March 27, 2026

- Comprehensive EDA with correlation and mutual information analysis across all features.
  - A1c readings are the dominant signal. `a1c_weighted` (EWMA-0.5 mean of all readings, recency-weighted) has the highest MI with the target. Averaging multiple readings beats any single reading.
  - Volatility features (SD, range across readings) carry independent information from A1c level (~0.23 correlation). Uncontrolled patients bounce around more.
  - Medications: number of distinct drug classes (cor 0.152) is stronger than any single drug count or total count. Sulfonylurea and insulin stand out individually.
  - Lipids, BMI, ADI, comorbidities (CAD/COPD), demographics: all weak to negligible. Kept for tree models but don't drive prediction.
  - Missingness is informative: whether weight/lipids were measured at all predicts outcome (healthcare engagement proxy).
  - Source file: `./scripts/subrata_eda_plots.R`. Extends `./scripts/eda.R` with MI and comprehensive feature-level correlations.

- Engineered 50 features from raw data. Key groups:
  - **A1c summaries:** `a1c_weighted` (EWMA-0.5), `a1c_latest`, `a1c_1`, `a1c_sd`, `a1c_range`, `a1c_rate`, `n_a1c`
  - **Treatment-effect features** (from Subhrajyoti's clinical insight about medication confounding): `worsening_on_meds`, `high_a1c_no_meds`, `improving_on_meds`, `still_high_improving`, `a1c_per_drug`, `a1c_x_meds`
  - **Missingness flags:** 8 binary indicators for missing values in labs/vitals
  - Feature engineering is inline in both model scripts below.

- Fitted GLM, glmnet, decision tree, GAM, XGBoost on train/val split. Evaluated on returned patients only (those with a 2025 eval date; n=17,845 train / 9,056 val, base rate ~18.5%). The 26,617 no-return patients are all controlled and trivially predicted, so val metrics focus on the harder subset.

  | Model | Val F1_pos | Val F1_macro |
  |---|---|---|
  | Baseline (simple logistic) | ??? | 65.0% |
  | GLM (50 features) | 60.0% | 74.4% |
  | glmnet elastic net | 61.7% | 75.7% |
  | Decision Tree (3 leaves) | 61.1% | 75.6% |
  | GAM (splines on A1c) | 62.0% | 75.9% |
  | **XGBoost** | **62.3%** | **75.9%** |

  - No overfitting: val >= train across all models.
  - `a1c_weighted` dominates everything: 57% of XGBoost gain, largest GLM coefficient, one of only 3 features glmnet keeps.
  - Treatment-effect features have large GLM effects but are redundant with A1c level under regularization/trees.
  - A 3-leaf decision tree nearly matches XGBoost -- the problem has a simple core (threshold A1c near 7).
  - Ceiling around 62-63% F1_pos. All model types converge to similar val performance.
  - Source files: `./scripts/subrata_model_glm_split.R`, `./scripts/subrata_model_nonlinear_split.R`
  - Relevant plots: `./figures/tree_plot.pdf`, `./figures/gam_plots.pdf`

- **F1 metric note:** Subhrajyoti's baseline 65% is macro F1 (average of both classes). We report both positive-class F1 (harder) and macro F1. Need to confirm which the competition uses.

> TODO: Stratified two-model approach (separate models for A1c >= 7 vs < 7). Deeper XGBoost tuning. Ensemble/stacking.
