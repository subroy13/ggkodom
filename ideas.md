# Ideas for Improving Prediction (Brainstorm)

Date: 2026-04-03

Context: All models (GLM, glmnet, tree, GAM, XGBoost) converge to ~62% F1_pos on validation. Misclassification EDA shows two structural error sources: (1) FN = controlled patients who deteriorate (median a1c_latest = 6.9, no warning signal), (2) FP = high-A1c patients who respond to treatment (median a1c_latest = 7.7). Medication is confounded by severity. Trajectory is non-monotonic (big improvers still land above 7). The ceiling appears to be about unobserved treatment dynamics.

---

## Tier 1 — High impact, quick to implement

### 1.1 Time gap: last A1c reading → 2025 measurement
`days_to_eval` and `time_a1c_*` exist but nobody computed `gap = days_to_eval - time_of_latest_reading`. A patient measured 3 months before 2025 eval is far more predictable than one measured 3 years before. This feature directly captures the *uncertainty horizon* — longer gap = more time for treatment effects, deterioration, or regression to mean. Probably the single most important missing feature.

### 1.2 `days_to_eval` as a model feature
Created in `create_features()` but never included in any formula or `feat_cols`. When the 2025 eval happens matters: early measurement may catch patients mid-treatment (higher FP risk), late measurement gives more time for treatment to work (higher TN rate among treated patients).

### 1.3 Simple ensemble (average probabilities)
Average `prob_glm`, `prob_glm_wt`, `prob_tree`, `prob_gam` predictions. Free lunch — models disagree on ~20% of patients (cross-model analysis showed this). Averaging smooths out individual model quirks. Could also try weighted average with weights from validation F1.

### 1.4 Unused covariates already in the data
- **CAD/COPD counts** — in `counts` tibble, never used. Comorbid patients may have different A1c trajectories and treatment responses.
- **ADI (Area Deprivation Index)** — state and national rank in `basic`, never used. Socioeconomic deprivation → worse medication adherence → worse outcomes. Could explain some FN cases (patients who can't sustain treatment).
- **Race/ethnicity** — in `basic`, never used. Known disparities in diabetes outcomes and treatment access.

---

## Tier 2 — High potential, requires modeling thought

### 2.1 Stratified two-model approach (A1c >= 7 vs < 7)
The misclassification EDA proved these are fundamentally different populations:
- **A1c >= 7 (uncontrolled now):** The question is "will treatment bring them down?" Relevant features: medication type/intensity, trajectory direction, time on treatment.
- **A1c < 7 (controlled now):** The question is "will they deteriorate?" Relevant features: volatility, number of readings, proximity to 7.0, medication fragility (on insulin = higher risk of losing control).

Different feature sets, different thresholds, possibly different models entirely.

### 2.2 Regime-dependent slopes (above vs below 7)
From EDA (`eda.R`): when A1c > 7, next reading tends to decrease (intervention kicks in). When A1c < 7, it's symmetric (no intervention). The transition dynamics are regime-dependent, but current models treat the slope as one number.

Could compute:
- `slope_above_7`: rate of change between consecutive readings where the starting value was > 7
- `slope_below_7`: rate of change when starting value was < 7
- `n_crossings_7`: how many times the patient crossed the 7.0 threshold (indicator of instability)
- `time_above_7`: fraction of readings spent above 7

These capture the *dynamics* around the clinical boundary, not just the current level. The Bayesian model (`subh_bayesian.R`) captures this via the medication flag in Markov transitions, but the frequentist models don't.

### 2.3 Estimate medication timing from sudden A1c drops
Key insight: we don't have treatment start dates, but we might *infer* them. If a patient on meds has readings [9.5, 9.2, 7.1, 6.8], the 9.2 → 7.1 drop likely marks when treatment took effect.

Possible features:
- `max_drop`: largest single-step A1c decrease across consecutive readings
- `max_drop_time`: when (relative to reference) the biggest drop occurred
- `time_since_biggest_drop`: time from biggest drop to latest reading — proxy for "how long has treatment been stabilized"
- `post_treatment_slope`: slope of A1c *after* the biggest drop (are they stable post-treatment or drifting back up?)

Only meaningful for patients who (a) have multiple readings AND (b) are on medication. For others, these would be NA (with a missingness flag).

This is speculative — the "biggest drop" might not be treatment-related (could be lab error, regression to mean, or lifestyle change). But among patients with medication orders, it's a reasonable proxy. Worth exploring.

### 2.4 Subgroup-specific thresholds
Instead of one global threshold, tune separately for:
- High vs low A1c patients
- Single-reading vs multi-reading patients
- On-meds vs off-meds
- High vs low ADI (if socioeconomic data helps)

The optimal threshold for a patient at A1c = 6.5 should probably be different from one at A1c = 9.0.

---

## Tier 3 — Moderate potential, low effort

### 3.1 Time between consecutive A1c readings (monitoring regularity)
Beyond `n_a1c` (how many readings), compute:
- `mean_time_between_readings`: average gap between consecutive A1c measurements
- `regularity`: SD of time gaps (regular vs sporadic monitoring)
- `time_since_first_reading`: total observation window length

Regular monitoring = engaged patient + engaged provider = better outcomes? Or regular monitoring = sicker patient needing more frequent checks? (Same confounding-by-indication issue as medication, but worth checking.)

### 3.2 Interaction features for borderline patients
The borderline zone (A1c 6-8) is where most errors happen. Could add:
- `a1c_latest_x_trajectory`: interaction between level and direction of change
- `borderline_flag`: indicator for a1c_latest in [6, 8]
- `borderline_x_n_readings`: borderline patients with few readings are the hardest cases

### 3.3 BMI (from height + weight)
Height and weight are mostly missing (72-83%), but when both are present, compute `BMI = weight / (height/100)^2`. Obesity is a known diabetes risk factor. The missingness flags already capture the "measured vs not" signal; BMI adds the actual value.

---

## Tier 4 — Understanding / validation

### 4.1 Calibration plot
Are predicted probabilities well-calibrated? Plot observed event rate vs predicted probability in decile bins. If the model says P=0.6, is it really ~60%? Poor calibration could mean the threshold is suboptimal.

### 4.2 Bayesian model benchmark
Subhrajyoty's JAGS model captures treatment dynamics explicitly (medication flag in Markov transitions, patient-specific volatility). It should handle the regime-switching that frequentist models miss. But it hasn't been benchmarked on the same val split. Comparing it would tell us whether the structural modeling helps.

### 4.3 Non-returned patients
26,617 patients have no 2025 A1c measurement. Key question: **does the competition score ALL patients or only returned ones?** If all patients, the non-returned are trivial TNs (predict all as controlled) and our effective competition F1 is much higher than the ~58% we see on returned patients only. Need to check competition rules.

### 4.4 Competition metric clarification
Still unclear whether the competition uses positive-class F1 or macro F1. Macro F1 gives credit for correctly predicting controlled patients (easy). Positive-class F1 focuses entirely on the hard minority. Threshold optimization differs substantially.

---

## What would break the ceiling?

The honest answer: with the features available, the ceiling is probably real. The two main error sources (future deterioration, future treatment success) require information we don't have — treatment start dates, medication adherence, lifestyle changes, patient intent to follow up.

The best bets for squeezing out improvement are:
1. **Time gap feature** (Tier 1.1) — adds real information about prediction horizon
2. **Medication timing proxy** (Tier 2.3) — imperfect but novel signal
3. **Regime-dependent slopes** (Tier 2.2) — captures dynamics the current features miss
4. **Stratified model** (Tier 2.1) — different models for fundamentally different populations
5. **Ensemble** (Tier 1.3) — free improvement from model diversity

Even with all of these, the ceiling might only move from ~62% to ~65-68%. But that could matter in a competition.

---

## Status update (Apr 3 late night)

### Implemented and tested:
- **Tier 1 features** (1.1–1.4): all implemented. GLM improved 56→60%. Ensemble: 61.6%.
- **Tier 2 features** (2.2): regime slopes, crossings, post-drop dynamics — all at both 7.0 and 8.0 thresholds.
- **Stratified model** (2.1): tested two-way (split at 7) and **three-way** (LOW <7, MID 7–8, HIGH ≥8).

### Three-way split result:
- HIGH (≥8): 75% F1 within-regime, but mostly "predict all uncontrolled" (60% base rate).
- MID (7–8): 44% F1. The hardest group — moderate signal but 25% base rate.
- LOW (<7): 24% F1. Hopeless. The 265 uncontrolled patients are "fragile treatment responders" (came from high A1c, treated down, relapsed). No features predict relapse at 4.7% base rate.
- **Combined: 60.4%** — comparable to global GLM (59.9%), doesn't beat global ensemble (61.6%).

### What we now know:
The mechanism-based split is illuminating but the global model already learns regime boundaries implicitly. The ceiling is structural: FN = deterioration from controlled state, FP = treatment success from uncontrolled state. Both require unobserved data (adherence, treatment timing).

### Still worth trying:
- **Medication timing proxy** (2.3): not yet tested. The "came from very high → treated down" pattern suggests we can infer treatment effect from the trajectory. The post-drop features partially capture this, but computing `time_since_biggest_drop` and `post_treatment_slope` might add signal especially in MID.
- **Subgroup thresholds** (2.4): different optimal thresholds for each regime. The three-way models use separate thresholds by construction.
- **Bayesian model benchmark** (4.2): Subhrajyoty's model captures medication intervention explicitly — might handle MID group better.
- **Calibration** (4.1): are predicted probabilities well-calibrated? Could improve threshold selection.









---
Subrata Ideas -- written by hand: 

i) Want to plot the trjectories of the a1c and I will see the grouped a1c, say <6, 6-6.5, 6.5-7 etc -- color coded. Check the trjectory. This might clarify the story a bit more. Fit a Cauchy/t type model instead of normal? 
ii) See this problem as a completely different problem of estimating future from past. So a time-series type train-test split -- not a subject based train test split. Then learn and see...
iii) The optimizing problem is that a1c last visit is so much that others are nothing. If it was a regression problem, I would first look at the residual from fitting the last a1c value. Then fir the remaining covariates to the residuals. Similar ideas here????


---

## Apr 5, 2026 — Fixing the multi-head NN (training-evaluation mismatch)

### Problem diagnosis

Subhrajyoty's multi-head NN (`subh_multihead_nn.R`) models state transitions (H/L at threshold 8.0) between consecutive A1c readings. Transition-level val F1 = 69.4%, but **patient-level (competition) F1 = 60.4%** — below GLM+wt (61.7%). The gap comes from a training-evaluation mismatch:

- The model trains on ALL ~36k transitions equally: a1c_1→a1c_2, ..., a1c_5→a1c_2025.
- Most transitions are easy same-state predictions (H stays H, L stays L between visits).
- Competition only scores the FINAL transition (→ a1c_2025) for returned patients.
- The model spends capacity on short-term stability (months between visits) while the hard question is long-term outcome (possibly years to 2025).

The architecture is sound (two-head design matches the two clinical dynamics we identified). The training signal is diluted.

### Fix 1 — Loss weighting on final transitions (try first)

Tag which rows are final transitions (→ a1c_2025). Upweight them ~10x in the loss. Intermediate transitions still regularize the shared layers, but the heads learn to prioritize the competition-relevant prediction. Implementation: 3 lines of code.

### Fix 2 — Two-stage training (pretrain → fine-tune)

Stage 1 (~150 epochs): train on all transitions. Shared layers learn general state dynamics.
Stage 2 (~50+ epochs): train only on final transitions (or heavily upweight them), with lower LR on shared layers. The heads specialize for the competition task. This is transfer learning: general dynamics first, task-specific fine-tuning second.

### Fix 3 — Add trajectory summary features to final transitions

Currently each transition row only knows its own delta_t and current measurements. The final transition has no access to the patient's full A1c history. But the features that drive GLM (EWMA, volatility, regime slopes, crossings) summarize exactly that trajectory. Append these to the feature vector — set to 0 for intermediate transitions, real values for final transitions. Lets the NN use the same information that drives GLM from 56% to 60%.

### On architecture size

Current: 55 inputs → 32 → 32 → two heads (2,914 parameters). With ~36k transitions this is fine — no overfitting risk (train/val F1 nearly identical at 69.8/69.4%). Once we upweight final transitions, the effective training size for the heads drops to ~18k rows, so keeping 32 hidden units is reasonable. If anything, the shared layers could be slightly wider to capture more trajectory patterns. Don't shrink yet — the model is underfitting if anything (loss still decreasing at epoch 200).

### Priority

1. Fix 1 (loss weighting) — simplest, directly addresses the mismatch
2. Fix 3 (trajectory features) — if Fix 1 closes the gap with GLM+wt, this could push past it
3. Fix 2 (two-stage) — principled version if more investment warranted

### Results (Apr 5)

Subhrajyoty found ID mismatch in patient-level evaluation — `val_returned_ids` (from raw data) didn't align with `val_states` rows (from transition pipeline). Fixed by adding `is_target` flag inside `create_state_transitions()`. Old 60.4% was slightly inflated.

| Step | Config | Patient F1 | Notes |
|---|---|---|---|
| Step 0 | Baseline (corrected eval, 200ep) | 59.7% | True baseline after ID fix |
| Step 1 | Fix 1 (loss weighting 10x, 200ep) | **60.1%** | +0.4%, modest improvement |
| Step 1b | Fix 1 (400ep) | 58.5% | Overfitting — train loss down, val F1 down |
| Step 2 | Fix 1 + Fix 2 (200+100ep fine-tune) | 58.8% | Stage 2 hurt — model already at local min |

**Fix 1 helps slightly (+0.4%). Fix 2 hurts. More epochs hurts. Best NN so far: 60.1%.**
Still below GLM+wt (61.7%). Fix 3 (trajectory features) still untested.

### BMI note

BMI is computed in the NN model (derived from height/weight in `create_state_transitions`). NOT in the GLM pipeline (`subrata_features.R`) — uses raw height/weight instead. EDA showed 72-83% missing for height/weight, so BMI has limited impact either way.

---

## Apr 5, 2026 — Time-based splitting idea (Subrata)

Expands on hand-written idea (ii) above. Currently we split by subject — train/val/test are disjoint patient groups. The NN transition model already uses all a1c_i → a1c_{i+1} pairs as training rows, but the SPLIT is still by patient.

**Alternative: time-based (within-patient) split.** For a patient with 5 visits, use transitions 1→2, 2→3, 3→4 for training, and predict transition 4→5 (or 5→2025). The same patient appears in both train and eval, but at different time points. This is the natural framing for time-series forecasting.

**What this changes:**
- Training data: ALL patients' earlier transitions (not just 50% of patients). Massive increase in training data for learning dynamics.
- Eval data: each patient's LAST transition only.
- Single-visit patients: no intermediate transitions to learn from. Must use patient-level features (demographics, meds, counts) + the single A1c value directly. Could fall back to GLM-style prediction for these.

**Why it might help:**
- The current NN learns dynamics from train-split patients and applies to val-split patients. But patient-specific patterns (individual treatment response, volatility) don't transfer across patients — they're idiosyncratic. Time-based splitting lets the model see each patient's OWN earlier dynamics and use that to predict their future.
- This is exactly what the Bayesian model does (patient-specific random effects). The NN could learn something similar if it sees the patient's history.

**Why it might not help / complications:**
- Risk of overfitting to patient-specific patterns. The model might just memorize "patient X tends to be high" without learning generalizable dynamics.
- The competition still scores on held-out patients (test set). We'd still need a subject-based held-out set for final evaluation. The time-based split is for TRAINING, not for final eval.
- Implementation: need to restructure the data pipeline. Each patient's transitions get split differently. More complex than the current approach.
- For the NN transition model specifically: it already uses all transitions. The key change would be EVALUATING on the last transition of ALL patients (not just val-split patients) during training, which is closer to what we want.

**Possible implementation:**
1. Keep the existing subject-based val/test splits for final evaluation.
2. For training: use ALL patients (train+val+test) intermediate transitions as training data.
3. For training-eval: use train patients' final transitions to tune thresholds.
4. For competition-eval: use val patients' final transitions (never seen during training for these patients).
5. Single-visit patients: separate model (GLM or just threshold on a1c value).

### Result (tested Apr 5): FAILED

Implemented as `scripts/subrata_nn_timesplit.R`. Val patient F1 = **58.0%** (vs 61.0% subject-split). The 28k additional intermediate transitions diluted the signal. Intermediate transitions (between regular visits, short gaps) are genuinely different from target transitions (last reading → 2025 eval, potentially years apart). The model learned "what happens between visits" even harder, but that doesn't help predict "what happens by 2025 after treatment."

This also confirms Fix 2's failure — intermediate and target transitions are fundamentally different tasks. The competition question isn't about short-term dynamics, it's about long-term treatment outcomes. The GLM's patient-level summary features (EWMA, volatility, regime fractions) capture trajectory information more effectively than modeling individual transitions.
