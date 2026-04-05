# Changes Log

## March 24, 2026

- Data released from I2DB platform.
- Initial look at data reveals a longitudinal study pattern. Have lots of questions, to be cleared during office hours. Also, there is highly imbalance in target response: $90\%$ is FALSE, and $10\%$ is TRUE.
- Also have missing data, variables like `gender`, `race`, `ethnicity` all needs some form of cleaning.

## March 27, 2026

- Attended office hours. Learned about dataset overview and confirmed that the target response variable is whether `a1c_2025` is more or less than 7.0.

- Observation: Found that the spaghetti plot for various patients (even in same age group) shows both up / down movements. Source file `./scripts/eda.R`

- Observation (Issues with causality of time): Found patients (e.g., id = 43) for which the 4th visit happened at a date before the 3rd visit. 
    - **NOTE: We have decided to sort the visits and treat the times as truth.** 
    - Source file: `./scripts/eda.R`

- Observation: Found visits that are too close to each other (within day apart), but radically different measurements. 
    - **NOTE: Need a robust procedure that should ignore such rapid changes.**
    - Source file: `./scripts/eda.R`

- Observation: Looked at the distribution of slope changes from current a1c measurement to next a1c measurement, conditional on whether current a1c is greater than 7 or less than 7.
    - Plot shows, when current a1c is less than 7 (the patient is normal), so the next point is equally likely to be up or down. If the current a1c is greater than 7, there is some sort of intervention, it is more likely to go down.
    - Source file: `./scripts/eda.R`
    - Relevant plots: `./figures/eda_conditional_beta.pdf`.

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


## March 31, 2026

- Created a data cleaning pipeline based on all EDA and sorting mechanism so far. Split into 50-25-25 train/val/test partitions.
    - Converted the non-identifiable factor levels to NA (e.g., unknown gender, race, etc.)
    - Source file: `./scripts/cleaning.R`

- Built JAGS Bayesian model for an interpretable validation. Need to test accuracy.

## Apr 3, 2026

-   Added support for F1 metric calculation and several best thresholding parameter calculation in `./scripts/utils.R`.
-   Recreated the feature engineering pipeline.
-   Validated baseline models, GLM, GAM, GLMnet, Random forest, Xgboost, etc.
-   Moved @Subrata's scripts to `./archive`, and put cleaned (common strategy) version as `./subrata_features.R` and `./subrata_models.R`.

## Apr 3, 2026 (evening) — Misclassification EDA

- Added height/weight features to `scripts/subrata_features.R` and `scripts/subrata_models.R`. Height/weight values were already loaded from the cleaning pipeline but not passed through to models. Turned out useless: 72-83% missing across all groups, no discriminative power.

- Re-ran models (GLM, GLM+weights, Decision Tree, GAM) on validation. Results consistent with earlier runs. Saved predicted probabilities to `data/processed/val_predictions.Rds` for downstream analysis.

- **Secondary EDA on misclassified patients** (`scripts/misclass_eda.R`, `scripts/misclass_eda2.R`). Classified validation patients into TP/TN/FP/FN using GLM+weights. Key findings:

  - **Classification breakdown** (val, returned patients, n=9056): TP=1249, TN=5999, FP=1366, FN=442. 1047 patients (12%) misclassified by ALL four models.

  - **FN patients (missed uncontrolled, n=442):** Median a1c_latest = 6.9, 81% within the 6-8 range, 60% have only 1 A1c reading. These patients looked controlled on their last reading but deteriorated by 2025. No features predict who will worsen from a controlled state.

  - **FP patients (false alarms, n=1366):** Median a1c_latest = 7.7, clearly high A1c at last reading. These are treatment responders — the model correctly sees high A1c but can't predict that treatment will succeed by 2025.

  - **Random baseline:** Permutation test gives mean F1 = 22.6% (95th percentile: 24%). Our model at ~58% is 2.5x above chance. The model captures real signal, but is bounded by structural unpredictability.

  - **Medication is confounded by severity (confounding by indication):** Among high-A1c patients (>=7), no-meds patients have the *highest* controlled rate (64%), insulin patients the *lowest* (51%). More drug classes → lower controlled rate. This is NOT "meds are harmful" — sicker patients receive more aggressive treatment. Medication features are severity proxies, not treatment effects.

  - **Trajectory is non-monotonic:** "Improving >1 drop" patients have 34% uncontrolled rate — *higher* than stable (16%). These patients came from very high A1c, dropped a lot, but many still land above 7. Among borderline patients (A1c 6-8), the "improving >1 drop" subgroup has 31% uncontrolled rate. The model cannot distinguish "improving toward controlled" from "improving but not enough."

  - **The ~62% F1 ceiling is structural.** Two irreducible error sources: (1) future deterioration from controlled state (FN), and (2) future treatment success from uncontrolled state (FP). Without treatment timing/adherence data, this ceiling is baked in.

  - Source files: `scripts/misclass_eda.R`, `scripts/misclass_eda2.R`
  - Plots: `figures/misclass_eda.pdf`, `figures/misclass_eda2.pdf`

## Apr 3, 2026 (night) — Tier 1 & 2 features, stratified model, threshold correction

- **CRITICAL: Competition threshold is A1c > 8, not > 7.** Confirmed by organizers. "Uncontrolled" = A1c > 8.0. The raw labels in `DM Control_2025.csv` already use this definition, so our target variable is correct. But ALL feature engineering used 7.0 as the clinical boundary (e.g., `high_a1c_insulin`, `frac_above_7`, `slope_above_7`, stratified model split). Note: 7.0 is still meaningful — it's the ADA clinical intervention threshold (doctors act at 7). But the prediction boundary is 8.0. **Action needed: add parallel features at 8.0 threshold, change stratified model split to 8.0.**

  - Subhrajyoty's Bayesian model (`subh_bayesian.R`) already used `A_star = 8.0` — he had it right from the start.

- Tier 1 features added to `subrata_features.R`: time gap (last reading → 2025 eval), `days_to_eval`, ADI scores, CAD/COPD, race/ethnicity dummies, sudden drop/rise detection. GLM F1 improved from 56.5% → 59.9%. Ensemble of 4 models: 61.6%.

- Tier 2 features added: `frac_above_7`, `n_crossings_7`, regime-dependent slopes (`slope_above_7`, `slope_below_7`), post-drop dynamics (`post_drop_drift`, `sudden_drop_still_high`).

- Stratified two-model approach (`scripts/subrata_models_stratified.R`): separate models for A1c >= 7 vs < 7. HIGH model F1 = 65% (strong), LOW model F1 = 24% (terrible — only 4.7% base rate, no signal). Combined: 58% (worse than global). Hybrid approach (stratified HIGH + global LOW) under testing.

## Apr 3, 2026 (late night) — Three-way split model & LOW group analysis

- **Three-way mechanistic split** (`scripts/subrata_models_3way.R`): separate models for LOW (<7) / MID (7–8) / HIGH (≥8). Rationale: each regime has different causal dynamics.

  | Regime | Val returned | Base rate | Mechanism |
  |---|---|---|---|
  | HIGH (≥8) | 1,610 (18%) | 60.1% | Will treatment pull them below 8? |
  | MID (7–8) | 1,838 (20%) | 25.0% | Will treatment fail to hold below 8? |
  | LOW (<7) | 5,608 (62%) | 4.7% | Fragile treatment responders relapsing? |

- **Per-regime results (val F1):**

  | Regime | GLM | Tree | GAM | Ensemble |
  |---|---|---|---|---|
  | HIGH (≥8) | **75.4%** | 74.9% | 75.1% | 75.3% |
  | MID (7–8) | **44.4%** | 40.0% | — | 44.1% |
  | LOW (<7) | 24.4% | 24.5% | — | — |

  - HIGH model F1 = 75% is strong, but mostly because 60% base rate → the model essentially predicts "all uncontrolled" with a few corrections. Precision 61%, Recall 99%.
  - MID model is the weak link at 44%. 25% base rate, moderate signal but insufficient for high precision.
  - LOW model stays hopeless at ~24%.

- **Combined three-way results (overall val):**

  | Configuration | Val F1 |
  |---|---|
  | HIGH=GLM, MID=GLM, LOW=all_controlled | **60.4%** |
  | HIGH=GLM, MID=GLM, LOW=GLM | 59.4% |
  | HIGH=Ens, MID=Ens, LOW=all_controlled | 59.6% |
  | *(compare: global GLM)* | *59.9%* |
  | *(compare: global ensemble)* | *61.6%* |

  Three-way (60.4%) is comparable to global GLM (59.9%) but does not beat global ensemble (61.6%). **The per-regime models are better at what they do, but the global model already implicitly learns regime boundaries through features like `high8_insulin`, `a1c_change_if_high8`, etc.**

- **Deep dive into LOW uncontrolled patients** (265 patients who go from A1c < 7 to > 8 in 2025):

  Among patients with 2+ readings:

  | Trajectory | n | Uncontrolled | Rate |
  |---|---|---|---|
  | Always low (first < 7 → latest < 7) | 2,315 | 67 | 2.9% |
  | Came from high (first 7–8 → latest < 7) | 447 | 50 | 11.2% |
  | Came from very high (first ≥ 8 → latest < 7) | 246 | 55 | **22.4%** |

  These are **fragile treatment responders** — patients who were high, got treated down below 7, but the response doesn't hold. Signatures: a1c_weighted 6.76 vs 6.17, volatility SD 1.54 vs 0.60, 40% had first reading ≥ 7, 35% on insulin (vs 20%). The "always low" patients who deteriorate (2.9%) are noise — no signal can predict which stable patient will jump 1+ point.

  Simple screen: `a1c_weighted ≥ 6.5` captures 68% of LOW positives (9.6% base rate in that subgroup), but at 4.7% overall base rate any classifier drowns in false positives.

- **Key insight: the mechanism-based split clarifies WHY the ceiling exists but doesn't break it.** The global model already captures regime structure through feature interactions. What would break the ceiling is data we don't have: medication adherence, treatment timing, lifestyle changes.

## Apr 4, 2026 — Competition scoring confirmed, calibration analysis

- **Competition confirmed: returned patients only, positive-class F1.**

- **Calibration analysis** (`scripts/calibration.R`, `figures/calibration.pdf`):
  - **GLM is the only well-calibrated model.** Predicted probabilities closely match observed rates across the full range. When it says P=0.5, the actual rate IS ~50%.
  - **GLM+wt is miscalibrated** — class weights stretch the probability scale. Systematically overestimates (says P=0.8, actual ~0.29). Ranking preserved, but probabilities are not interpretable.
  - **Decision tree is degenerate** — predicts ~0.106 for all 9,056 patients. Single leaf. Dead weight.
  - **GAM is underconfident** — systematically underestimates risk. Says P=0.25, actual ~0.40. Probabilities compressed toward zero.
  - **Ensemble is dragged down by tree + GAM.** The GLM's good calibration is diluted.

- **Per-regime calibration (ensemble):** HIGH patients consistently underestimated — ensemble says P=0.45, truth is 0.53. Tree + GAM compress the HIGH predictions.

- **Regime-specific thresholds on global model: WORSE than single threshold.** Per-regime thresholds for MID/LOW get too aggressive (trying to capture sparse positives), adding false positives that tank precision.

  | Model | Threshold type | Val F1 |
  |---|---|---|
  | GLM | single global | 61.3% |
  | GLM+wt | single global | **61.7%** |
  | Ensemble (GLM + GLM_wt only) | single global | **61.7%** |
  | GLM | regime-specific | 57.9% (worse) |
  | GLM+wt | regime-specific | 57.6% (worse) |

- **Recommendation: drop the tree and GAM.** Use GLM+wt (single threshold) or 2-model ensemble (GLM + GLM_wt). Both give 61.7% F1. The tree and GAM hurt calibration without improving F1.

## Apr 5, 2026 — Multi-head NN evaluation & diagnosis

- **Subhrajyoty's multi-head NN** (`subh_multihead_nn.R`): models A1c state transitions (H/L at threshold 8.0) with shared layers + two heads (L→H and H→H). Trains on all consecutive A1c pairs (~36k transitions). Architecture: 55→32→32→2 heads, ~2,914 params.

- **Subrata added patient-level evaluation** (`subrata_multihead_nn.R`): Subhrajyoty's code only reported transition-level metrics. Added code to extract last transition per patient, filter to returned patients, compute competition metric.

- **Results:**

  | Level | Val F1 |
  |---|---|
  | Transition-level (all pairs) | 69.4% |
  | **Patient-level (competition metric)** | **60.4%** |
  | *(compare: GLM+wt)* | *61.7%* |

- **Diagnosis: training-evaluation mismatch.** Most transitions are easy same-state predictions (H→H, L→L between visits). The model spends capacity on short-term stability while competition only scores the final transition (→ a1c_2025). The architecture is sound; the training signal is diluted. Fixes planned: loss weighting on final transitions, two-stage training, trajectory summary features. Details in `ideas.md`.

