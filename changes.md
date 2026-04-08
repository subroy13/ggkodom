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

## Apr 5, 2026 (continued) — NN fixes & GLM improvements

### NN systematic fix testing

Subhrajyoty found ID mismatch in patient-level evaluation — `val_returned_ids` (from raw data) didn't align with `val_states` rows (from transition pipeline). Fixed by adding `is_target` flag inside `create_state_transitions()`. Old 60.4% was slightly inflated.

| Step | Config | Patient F1 | Notes |
|---|---|---|---|
| Step 0 | Baseline (corrected eval, 200ep) | 59.7% | True baseline after ID fix |
| Step 1 | Fix 1 (loss weighting 10x, 200ep) | 60.1% | +0.4%, modest |
| Step 1b | Fix 1 (400ep) | 58.5% | Overfitting |
| Step 2 | Fix 1 + Fix 2 (200+100ep fine-tune) | 58.8% | Stage 2 hurt |
| Step 3 | **Fix 1 + Fix 3 (trajectory features, 200ep)** | **61.0%** | Best NN |
| Step 3b | Fix 1 + Fix 3 (250ep) | 60.3% | Overfitting again |

**Best NN: 61.0% (Fix 1 + Fix 3, 200ep).** Fix 1 (loss weighting) helps slightly. Fix 2 (fine-tuning) hurts. Fix 3 (EWMA, SD, frac_above_8) gives the biggest boost (+1.3% from baseline). 200 epochs is the sweet spot — more always overfits.

### GLM pipeline: BMI features added

- Added `bmi = weight / (height/100)^2` (filtered to 10-80) and `bmi_x_male` (BMI × gender interaction) to `subrata_features.R`. Rationale: men/women have different obesity-diabetes risk profiles.
- Added `bmi`, `bmi_x_male`, `bmi_miss` to GLM formula, feat_cols, and GAM formula.
- Uncommented glmnet and XGBoost, moved to end of `subrata_models.R` (heavy models run last).

**Preliminary results (with BMI, before glmnet/XGBoost):**

| Model | Val F1 |
|---|---|
| GLM | 61.9% |
| GLM+wt | 61.5% |
| Tree | degenerate |
| GAM | 59.0% |
| Ensemble 2 (GLM + GLM+wt) | **62.0%** |

GLM improved from 59.9% → 61.9%. Ensemble 2-model hit 62.0%. Bug fix: glmnet needed `df_train_imputed` (not reassigned `df_train` which lacked `_miss` columns).

**Full results with BMI (`subrata_models_5_12_00.Rout`):**

| Model | Val F1 | Notes |
|---|---|---|
| GLM | 61.9% | +2.0% from BMI addition |
| GLM+wt | 61.5% | |
| Tree | 31.5% | Degenerate (all positive) |
| GAM | 59.0% | |
| Ensemble 2 (GLM+GLM_wt) | 62.0% | |
| glmnet | 61.5% | Regularization hurts — worse than raw GLM |
| **XGBoost** | **62.1%** | New best. Early-stopped 171/500 rounds |

XGBoost is the new best at 62.1%. GLM jumped +2.0% from BMI — `bmi_x_male` interaction doing real work. glmnet underperforms raw GLM (regularization shrinks useful signals with 50+ features).

### Additional features added (from Subhrajyoty's NN, not yet tested)

Added to `subrata_features.R` and `subrata_models.R`:
- `non_hdl = chol - hdl` — better CVD risk predictor than LDL alone
- `ldl_hdl_ratio = ldl / hdl` — lipid ratio
- `adi_discrepancy = adi_nation - adi_state` — localized deprivation context
- `log_ed_visits`, `log_pcp_visits`, `log_admissions`, `log_total_meds` — log-transformed counts (right-skewed, helps linear models; XGBoost benefits from log-spacing for threshold splits)

**Results with ALL new features (`subrata_models_5_12_34.Rout`):** hurt GLM/XGBoost, only helped glmnet.

| Model | BMI only | +lipid/log | Change |
|---|---|---|---|
| GLM | 61.9% | 61.5% | -0.4% |
| GLM+wt | 61.5% | 61.6% | flat |
| Ensemble 2 | 62.0% | 61.8% | -0.2% |
| glmnet | 61.5% | 62.1% | +0.6% |
| XGBoost | 62.1% | 62.0% | -0.1% |

**Reverted lipid/log features** (commented out). The log transforms likely added noise — the lipid ratios and ADI discrepancy are clinically grounded and might help on their own without the logs. To be tested separately after feature importance analysis.

### XGBoost regularization & feature importance (`subrata_models_5_12_45.Rout`)

Regularized XGBoost (max_depth=3, min_child_weight=20, subsample=0.7, colsample=0.6, gamma=1):

| XGBoost | Train F1 | Val F1 | Train-Val gap |
|---|---|---|---|
| Original | 63.2% | 62.1% | 1.1% |
| **Regularized** | 62.3% | **62.2%** | **0.1%** |

Train-val gap nearly vanished. Val F1 ticked up to **62.2%** — new best overall.

**Feature importance (top 6 account for ~90% of gain):**

| Rank | Feature | Gain |
|---|---|---|
| 1 | `days_to_eval` | 27-32% |
| 2 | `time_gap` | 13-21% |
| 3 | `a1c_weighted` | 17-18% |
| 4 | `frac_above_8` | 10% |
| 5 | `a1c_latest` | 8-11% |
| 6 | `frac_above_7` | 4% |

Everything else <1% gain. Lipid values (`value_hdl`, `value_ldl`) at <0.5%, ADI at <0.3%, BMI not in top 20. The signal is concentrated in A1c trajectory + timing features. Demographics, lipids, comorbidities are noise at the margin. Decided not to add derived lipid ratios or ADI discrepancy — feature importance shows the base features they'd derive from carry near-zero signal.

### Time-based validation NN (`subrata_nn_timesplit.R`)

Tested Subrata's idea: instead of subject-based split, train on ALL patients' intermediate transitions + train patients' target transitions, evaluate on val patients' target transitions.

| Approach | Val Patient F1 | Precision | Recall |
|---|---|---|---|
| Subject-split (Fix 1+3) | **61.0%** | 54.9% | 68.5% |
| Time-split | 58.0% | 45.3% | 80.7% |

**Failed.** Model over-predicts positives (recall 81%, precision 45%). The 28k additional intermediate transitions diluted the signal. Intermediate transitions (between regular visits, short gaps) are genuinely different from target transitions (last reading → 2025 eval, potentially years). Adding more of the former doesn't teach the latter. The competition question isn't "what's the next A1c state" — it's "what's the state after potentially years of treatment."

This confirms Fix 2's failure: intermediate and target transitions are different tasks.

### Summary — the 62.2% ceiling

**The 62.2% ceiling is structural.** All model classes and approaches converge:

| Model | Best Val F1 |
|---|---|
| XGBoost regularized | **62.2%** |
| XGBoost original | 62.1% |
| Ensemble 2 (GLM+GLM_wt) | 62.0% |
| GLM (with BMI) | 61.9% |
| glmnet (with lipid/log features) | 62.1% |
| NN (Fix 1 + Fix 3) | 61.0% |
| NN time-split | 58.0% |

Feature importance (XGBoost) confirms: top 6 features (days_to_eval, time_gap, a1c_weighted, frac_above_8, a1c_latest, frac_above_7) account for ~90% of gain. Everything else <1%. The remaining errors are future deterioration (FN) and future treatment success (FP) — both require unobserved data (treatment adherence, lifestyle changes).


## Apr 5, 2026 (night) — lmer longitudinal exploration & GEE

### lmer exploration (`scripts/subrata_lmer_exploration.R`)

Tested whether mixed model BLUPs (best linear unbiased predictors) from longitudinal A1c trajectory add predictive signal beyond hand-crafted EWMA/SD features.

**Data sparsity killed random slopes.** 57.6% of patients have exactly 1 A1c reading. Random intercept + slope model (M2) is unidentifiable: 93,636 random effects > 74,981 observations. Only random intercept model (M1) is feasible on all patients. Random slope model fit successfully on ≥3 reading patients only (5,665 patients = 12%).

**lmer M1 results:**
- Fixed intercept = 7.11 (population mean A1c), fixed slope = −0.21/yr (slight average decline)
- Random intercept SD = 1.40, Residual SD = 1.16
- Shrinkage for 1-reading patients: BLUP = 59% raw + 41% population mean
- Treatment flag (M3: prev_above_8) significant but small AIC improvement (288782 → 288708)

**BLUPs are 98.8% redundant with a1c_weighted** (r = 0.988). For single-reading patients, correlation is 1.0 (BLUP is a linear rescaling of the single value). Shrinkage compresses the distribution (SD: 1.88 → 1.11) but doesn't improve ranking.

**Single-feature comparison:**

| Feature | Val F1 |
|---|---|
| a1c_weighted (EWMA) | **61.4%** |
| total_intercept (BLUP) | 60.9% |
| blup_intercept | 60.9% |
| a1c_latest | 60.3% |
| lmer_pred_a1c | 60.0% |

EWMA wins because recency weighting matters more than shrinkage for predicting 2025 outcome.

**Multi-feature evaluation:**

| Model | Baseline | + BLUPs | Change |
|---|---|---|---|
| GLM | 61.6% | 61.3% | −0.3% (multicollinearity) |
| XGBoost (reg) | 61.9% | 62.1% | +0.2% (noise) |

In XGBoost + BLUPs, `lmer_pred_a1c` captured 49% of feature importance — but this just reorganizes how the same information enters the tree, without meaningful F1 gain.

### GEE (geeglm) comparison

- AR(1) within-patient correlation = 0.691 (strong — consecutive A1c readings highly correlated)
- Fixed effects nearly identical to lmer: GEE intercept = 7.11, slope = −0.18 (vs lmer −0.21; marginal vs conditional)
- GEE gives population-averaged effects only — no patient-specific predictions. Useless for individual prediction.

### Conclusion

Longitudinal mixed modeling can't help because:
1. 58% have 1 reading — no within-patient trajectory to model
2. EWMA already captures level information with better recency weighting
3. BLUPs add shrinkage but shrinkage hurts ranking (compresses extremes that should be extreme)
4. GEE gives no patient-specific output

The F1 ceiling isn't a modeling problem — it's a data problem (sparse observations + unobserved treatment dynamics).

## Apr 6, 2026 — Continuous-time dynamics: variogram, CTMC, GP, multi-scale ACF

### Variogram & memory timescale (`scripts/subrata_continuous_time.R`)

Computed empirical variogram from 40,453 within-patient A1c pairs. **The variogram is non-monotonic:**

| Δt (yr) | γ(Δt) | Pattern |
|---|---|---|
| 0.008 | 1.25 | rising |
| 0.074 | 1.67 | rising |
| 0.175 | **2.04** | PEAK |
| 0.256 | 1.47 | drops |
| 0.459 | 1.32 | still lower |
| 0.533 | 1.15 | minimum |
| 0.689 | 1.71 | rises again |

**The OU (single-scale) model is wrong.** NLS fit forced τ = 4 days (nonsensical). The non-monotonic pattern is a "treatment effect ridge":
1. Δt < 2 months: treatment actively changing A1c → large differences → high γ
2. Δt = 3-6 months: treatment stabilized → readings converge → γ drops
3. Δt > 6 months: new perturbations (adherence decay, new conditions) → γ rises

**The process has at least 2-3 timescales:** measurement noise (days), treatment response (months), patient-level traits (years).

### CTMC transition rates

Estimated continuous-time Markov chain rates from 28,163 observed transitions:

| Parameter | Value | Meaning |
|---|---|---|
| q_LH | 0.24/yr | Annual deterioration rate |
| q_HL | 1.25/yr | Annual treatment success rate |
| q_HL/q_LH | **5.2×** | Treatment 5× faster than deterioration |
| π_H (stationary) | 0.160 | Long-run 16% uncontrolled |
| 1/λ (relaxation) | **0.67 yr** | After ~8 months, current state uninformative |

P(uncontrolled at t+Δt | current state):

| Δt | P(H\|L) | P(H\|H) | Gap |
|---|---|---|---|
| 0.1 yr | 2.2% | 88.4% | 86pp |
| 0.5 yr | 8.4% | 55.8% | 47pp |
| 1.0 yr | 12.4% | 34.9% | 22pp |
| 2.0 yr | 15.2% | 20.3% | 5pp |
| 5.0 yr | 16.0% | 16.1% | ~0 |

**The 8-month relaxation time explains why `time_gap` is such a powerful feature.** Patients with long time gaps are essentially unpredictable from their last state.

### GP prediction (OU kernel)

GP with OU kernel failed because τ = 4 days → kernel decays to zero for any practical time gap → predictions collapse to population mean (7.126) for everyone. GP needs a multi-scale kernel or manually-set lengthscale to work.

### Time-weighted EWMA

With τ = 4 days, tw_ewma ≈ a1c_latest (only latest reading gets weight). Correlation tw_ewma vs a1c_weighted = 0.97. As single feature: tw_ewma 60.4% ≈ a1c_latest 60.3% < a1c_weighted 61.4%.

### Feature evaluation

**Single feature:**

| Feature | Val F1 |
|---|---|
| a1c_weighted | **61.4%** |
| tw_ewma | 60.4% |
| a1c_latest | 60.3% |
| ctmc_prob | 58.6% |
| gp_mean | 31.5% (degenerate) |

**XGBoost:**

| Model | Val F1 |
|---|---|
| Baseline | **62.5%** |
| + continuous-time features | 62.3% |

`ctmc_prob` became XGBoost's #1 feature (25.1% gain) but didn't improve F1 — captures the same state × time_gap interaction the tree was already approximating.

### Key insight

The prediction ceiling is quantitatively explained by the CTMC relaxation time: after ~8 months, current A1c state is uninformative → prediction approaches the base rate (~16%). Most patients in the competition have time gaps > 8 months from their last reading to the 2025 evaluation. The model can only improve within the residual information not erased by time decay + treatment dynamics.

### Multi-scale ACF & GP (`scripts/subrata_multiscale_acf.R`)

**The most important finding of the entire project:**

#### ICC = 0.924

92.4% of A1c variance is BETWEEN patients, only 7.6% is WITHIN patients. This is why `a1c_weighted` (a patient-level summary) captures 99% of the predictive signal. Within-patient trajectory dynamics are bounded to 7.6% of variance — they CANNOT move F1 more than that.

#### Within-patient ACF is NEGATIVE

After demeaning within patient, ALL autocorrelations are negative (ρ = −0.10 at Δt=0.01yr to ρ = −0.66 at Δt=0.5yr). This means within a patient, consecutive readings tend to be on OPPOSITE sides of the patient's mean:
- High reading → treatment → next reading drops BELOW patient mean (overcorrection)
- Low reading → no intervention → drifts back up ABOVE patient mean

This is treatment-induced oscillation. Standard time-series models (AR, OU, GP) assume POSITIVE decaying autocorrelation — they are fundamentally wrong for within-patient A1c dynamics. The correct model needs treatment as a mean-reverting force with overshoot.

**Simpson's paradox:** The RAW (un-demeaned) correlation is positive (0.72 at short lags, 0.46 at long lags) because between-patient variance dominates. A patient at 9 today will likely read below 9 next time (negative within-patient), but still above the population mean 7.1 (positive raw correlation). The between-patient signal overwhelms the within-patient signal.

#### Multi-scale variogram fit

| Component | Variance | Timescale | % of within-patient |
|---|---|---|---|
| Nugget (noise) | c₀ = 1.178 | instantaneous | 73% |
| Short scale | c₁ = 0.409 | τ₁ = 8 days | 25% |
| Long scale | c₂ = 0.028 | τ₂ = 5+ years | 2% |

73% of within-patient variance is instantaneous noise. Only 27% has temporal structure, and that structure has an 8-day timescale (too fast to matter for prediction).

Residuals show a periodic pattern (peaks at Δt ≈ 2 months and ≈ 8 months) — possibly medication cycles or scheduled follow-ups.

#### Feature evaluation

| Feature | Val F1 | Notes |
|---|---|---|
| a1c_weighted | **61.40%** | Still the best single feature |
| tw_ewma_long (τ=5yr) | 61.49% | ≈ equal weight to all readings (r=0.997 with EWMA) |
| gp_ms_mean | 60.46% | Better than single-scale GP (31.5%) but still worse |
| gp_ms_sd | 31.47% | Degenerate alone |

| XGBoost | Val F1 |
|---|---|
| Baseline | 61.89% |
| + multi-scale GP | 62.24% |

`gp_ms_mean` captured 54% of XGBoost gain, `gp_ms_sd` 3.2%. But overall F1 within noise of previous best (62.2%).

#### Theoretical significance

The analysis reveals three layers of A1c variation:

1. **Between-patient (92.4%):** Patient "type" — some patients are chronically high, others chronically low. Captured by a1c_weighted. This is the signal.
2. **Within-patient noise (5.5%):** Measurement error, daily fluctuations, instantaneous treatment effects. This is irreducible noise.
3. **Within-patient temporal (2%):** Structured treatment dynamics (negative ACF, oscillation). Too weak to improve prediction, but explains WHY trajectory features add so little.

The ceiling is set by between-patient variation: once you know a patient's "typical A1c level" (a1c_weighted), the within-patient dynamics (7.6% of variance) can only move the needle by at most ~1-2% F1. This is consistent with what we observe: a1c_weighted alone = 61.4%, best model = 63%, maximum possible = ~65%.

### Apr 6, 2026 (late) — Subhrajyoty full-data XGBoost

- **Subhrajyoty: XGBoost trained on whole current data = 67% F1** (same XGBoost parameters). New best overall.
