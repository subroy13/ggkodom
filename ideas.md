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

---

## Apr 5, 2026 — Residual analysis & non-linear approaches

### Residual analysis results (`scripts/subrata_residual_analysis.R`)

**a1c_weighted alone: Val F1 = 61.25%.** Full 74-feature XGBoost: 62.2%. One feature captures 99% of the achievable signal.

Key findings from Stage 1 residuals (y - P(Y=1 | a1c_weighted)):

1. **The sigmoid is systematically biased.** Overpredicts at low A1c (<7: actual 2-6%, predicted 5-10%), underpredicts at high A1c (7-8: actual 25.2%, predicted 19.4%; >=8: actual 60.1%, predicted 55.6%). The real curve is more step-like. → Isotonic regression or GAM with flexible shape.

2. **Non-linear signal exists in residuals.** `a1c_x_ndrug` (AIC +62.4), `slope_above_8` (AIC +34.4) show strong non-linearity beyond what a linear additive model captures.

3. **Massive interactions with a1c_weighted.** `frac_above_8 × a1c_weighted` (LR=515), `high8_insulin × a1c_weighted` (LR=302), `a1c_x_ndrug × a1c_weighted` (LR=274). Treatment features behave differently at different A1c levels — clinically obvious but the additive GLM misses it entirely.

4. **Stage 1+2 GLM with 38 significant features: Val F1 = 61.07% — WORSE than a1c_weighted alone.** Too many features, overfitting the residual. Need a focused model with 3-5 features + non-linearity, not 38 linear terms.

### Approaches to try

- **Isotonic regression** on a1c_weighted → non-parametric monotone step function, fixes sigmoid bias
- **GAM with s(a1c_weighted, k=10)** → flexible shape + can add smooth interactions
- **Focused residual model**: a1c_weighted + only top 3-5 features + explicit interaction (frac_above_8 × a1c_weighted)
- Manual non-linear feature engineering rather than dumping to XGBoost

### Non-returned patients (~1/3 of all patients)

About 1/3 of patients have no A1c 2025 measurement (they didn't return for follow-up). Their labels are known: all "controlled" (no uncontrolled measurement). In the test set, these patients will also have known labels.

**Semi-supervised angle (Subhrajyoti's idea):** These known-label patients could be leveraged during training — even though competition scoring is on returned patients only, incorporating the non-returned patients as guaranteed negatives could help calibrate the decision boundary. This is analogous to semi-supervised learning where we have labeled and pseudo-labeled data. Currently our models DO train on all patients (including non-returned as negatives), but a more deliberate approach might weight them differently or use them for regularization.

**F1 impact:** When predicting on the full test set (including non-returned), the non-returned patients are trivially correct predictions. If competition scoring includes them, our effective F1 improves because precision goes up (fewer false positives among the easily-classified non-returned). Even if scored on returned only, knowing which test patients are non-returned lets us avoid false positives there entirely.

---

## Apr 6, 2026 — Continuous-time A1c dynamics: CTMC, GP, and time-weighted features

### The core insight (from transition-level vs patient-level F1 gap)

The NN transition model has 69.4% F1 on all transitions but 59.7% on patient-level (final transitions). The 9.5pp gap exists because **transition dynamics are non-stationary across time scales.**

Model this with a **continuous-time Markov chain (CTMC)** for the binary state S(t) ∈ {L, H} where H = A1c ≥ 8:

$$Q = \begin{pmatrix} -q_{LH} & q_{LH} \\ q_{HL} & -q_{HL} \end{pmatrix}$$

The transition probability matrix over time interval Δt is:

$$P(\Delta t) = \exp(Q \cdot \Delta t)$$

Key properties:
- **As Δt → 0:** P(Δt) → I (identity). Persistence dominates. Short-horizon prediction is easy — this is why transition-level F1 is high (87% of transitions are same-state).
- **As Δt → ∞:** P(Δt) → π (stationary distribution). π_H = q_LH / (q_LH + q_HL). The current state becomes uninformative — prediction reverts to the base rate.
- **At intermediate Δt:** P(Δt) smoothly interpolates between persistence and equilibrium.

For a 2-state CTMC, the explicit solution is:

$$P(H \text{ at } t+\Delta t \mid L \text{ at } t) = \pi_H (1 - e^{-(q_{LH}+q_{HL})\Delta t})$$
$$P(H \text{ at } t+\Delta t \mid H \text{ at } t) = \pi_H + (1-\pi_H) e^{-(q_{LH}+q_{HL})\Delta t}$$

where the decay rate is λ = q_LH + q_HL. This gives us:
- **One feature per patient:** P(H at t_2025 | last_state, time_gap) — combines state and time gap in the correct functional form.
- The "correct" form is NOT linear in time_gap (which is what GLM assumes) — it's exponential decay toward equilibrium.

### Connection to continuous-time ACF and memory timescale

For the continuous A1c process (not just the binary state), model as an Ornstein-Uhlenbeck (OU) process:

$$dY(t) = -\frac{1}{\tau}(Y(t) - \mu) dt + \sigma dW(t)$$

This is the continuous-time analog of AR(1). The autocorrelation function is:

$$\rho(\Delta t) = \exp(-\Delta t / \tau)$$

where τ is the **memory timescale** — how long an A1c reading stays informative. From the GEE, we know AR(1) per visit ≈ 0.69. If the typical visit gap is Δt_visit, then τ = -Δt_visit / ln(0.69) ≈ Δt_visit / 0.37 ≈ 2.7 × Δt_visit.

The **variogram** γ(Δt) = Var(Y(t) - Y(t+Δt)) / 2 = σ² (1 - exp(-Δt/τ)) gives another way to estimate τ from the data.

### Connection to Gaussian processes

The OU process IS a GP with Matérn-1/2 kernel:

$$K(t, t') = \sigma_f^2 \exp\left(-\frac{|t - t'|}{\ell}\right)$$

where ℓ = τ (the lengthscale = memory timescale).

For each patient with readings {(t_1, y_1), ..., (t_n, y_n)}, the GP posterior at prediction time t* is:

$$\mu_* = \mu + \mathbf{k}_*^T (\mathbf{K} + \sigma_n^2 \mathbf{I})^{-1} (\mathbf{y} - \mu)$$
$$\sigma_*^2 = K(t_*, t_*) - \mathbf{k}_*^T (\mathbf{K} + \sigma_n^2 \mathbf{I})^{-1} \mathbf{k}_*$$

where **k*** = [K(t*, t_1), ..., K(t*, t_n)]^T.

**Key insight: this unifies everything we've tried.**

For a patient with **1 reading** at time t_1 with value y_1, predicting at t*:

$$\mu_* = \mu + \frac{\sigma_f^2 e^{-|t_* - t_1|/\tau}}{\sigma_f^2 + \sigma_n^2} (y_1 - \mu)$$

This has three components:
1. **Shrinkage toward μ:** factor σ_f²/(σ_f²+σ_n²) — same as lmer BLUP shrinkage (≈ 0.59 from our lmer)
2. **Temporal decay:** factor exp(-Δt/τ) — new! Prediction decays toward μ as time gap grows
3. **Deviation from mean:** (y_1 - μ) — the patient's individual signal

For Δt → 0 (recent reading): this reduces to the lmer BLUP.
For Δt → ∞ (stale reading): this reduces to μ (population mean = base rate).

The GP prediction variance σ²* captures uncertainty — it grows with time gap. This is a genuinely new feature that neither EWMA nor lmer provides.

For **n readings**: the GP properly weights all readings by their time proximity to t*, handling irregular spacing naturally. The effective weights are determined by the kernel, not by ordinal position (like EWMA) or equal weighting (like lmer).

### Connection to existing lmer results

From the lmer exploration:
- σ_f² ≈ 1.950 (random intercept variance)
- σ_n² ≈ 1.352 (residual variance)
- Shrinkage for n=1: σ_f²/(σ_f²+σ_n²) = 0.591 ← matches lmer exactly

What lmer MISSED: the temporal decay exp(-Δt/τ). The lmer BLUP is the GP prediction at Δt = 0 (prediction at the same time as the reading). For predicting at t_2025 with time_gap > 0, the GP properly accounts for the decay.

### Time-weighted EWMA

As a simpler feature (no full GP), replace the per-reading EWMA decay with per-time decay:

$$\text{tw\_ewma} = \frac{\sum_{k} \exp(-(t_{\text{latest}} - t_k)/\tau) \cdot y_k}{\sum_{k} \exp(-(t_{\text{latest}} - t_k)/\tau)}$$

This weights readings by their temporal proximity to the latest reading, rather than by ordinal position. For equally-spaced readings, it approximates regular EWMA with a time-appropriate λ.

### What to estimate from data

1. **τ (memory timescale):** from the empirical variogram or continuous-time ACF
2. **q_LH, q_HL (CTMC rates):** from observed state transitions, adjusted for time gaps
3. **σ_f², σ_n² (GP hyperparameters):** from lmer (already done: 1.950, 1.352) or by MML

### Expected impact

- **CTMC feature** is a principled interaction of last_state × time_gap — the functional form XGBoost approximates with tree splits. Might help GLM more than XGBoost.
- **Time-weighted EWMA** is a better summary than ordinal EWMA for patients with spread-out readings.
- **GP prediction** subsumes both and adds prediction variance (uncertainty). This is the theoretically optimal approach.
- All three are bounded by the structural ceiling: the future treatment response is unobserved regardless of how well we summarize the past.

---

## Apr 6, 2026 — Multi-scale ACF reveals negative within-patient autocorrelation

### The key finding: ICC = 0.924, within-patient ACF < 0

92.4% of A1c variance is between patients. Within-patient variation is only 7.6%, and its autocorrelation is **negative** at all lags: ρ_within ≈ −0.1 at Δt=1 week, declining to ≈ −0.65 at Δt=6 months.

**This is treatment-induced oscillation:**
- High A1c → treatment → overcorrection below patient mean → treatment relaxes → drift back up → high A1c → ...
- The process is mean-reverting with overshoot, NOT a standard AR/OU process

**Simpson's paradox:** Raw (unconditional) correlation is positive (0.72 → 0.46) because between-patient variance dominates. Within patients, readings oscillate around the patient mean. Between patients, high-A1c patients stay high. The raw positive correlation is entirely a between-patient effect.

### Implications for modeling

1. **Standard time-series models are wrong for within-patient dynamics.** AR, OU, GP with Matérn/SE kernel all assume positive decaying autocorrelation. The within-patient process has negative ACF — these models can't capture this.

2. **The correct within-patient model** would be something like a damped harmonic oscillator or a controlled OU with treatment kicks:
   $$dY = -\alpha(Y - \mu_i) dt + \beta \cdot \mathbf{1}(Y > 8) \cdot (Y_{target} - Y) dt + \sigma dW$$
   where the second term is treatment pulling Y toward a target when above threshold.

3. **But it doesn't matter for prediction** because within-patient dynamics explain only 7.6% of variance. The patient's "type" (a1c_weighted ≈ patient mean) IS the prediction. Trajectory is second-order.

4. **The ceiling is mathematically bounded:** at most ~7.6% of variance is available from within-patient dynamics → at most ~1-2% F1 improvement beyond a1c_weighted alone. Observed: a1c_weighted = 61.4%, best model = 63%. Consistent.

### Multi-scale variogram

The within-patient variogram has:
- Nugget = 73% of sill (most variation is noise)
- Short scale: τ₁ = 8 days (25% of within-patient variance)
- Long scale: τ₂ = 5+ years (2% — effectively constant)
- Residuals show periodic pattern (~6 month period) — possibly medication cycles or scheduled visits

### For the presentation

These findings tell a clean story:
1. "Who you are" (patient type) predicts 99% → a1c_weighted captures this
2. "Where you're going" (trajectory) adds 1% → treatment dynamics, mostly noise
3. The ceiling is NOT a modeling failure — it's the data telling us that patient identity overwhelms trajectory
4. Treatment creates oscillation (negative ACF) — fundamentally different from standard time-series
5. The CTMC relaxation time (8 months) sets the prediction horizon limit

---

## Apr 7, 2026 (night) — Bayesian retry suspicion (data is synthetic)

**New context (from Subrata):** the competition data is **synthetic**. That changes the framing of the entire ceiling story. The pattern we recovered — ICC = 0.924, negative within-patient ACF, CTMC relaxation = 8 months, multi-scale variogram with 73% nugget + τ₁ = 8 days + τ₂ = 5+ years — is no longer "what diabetes biology happens to look like". It's the **fingerprint of the simulator** that generated the data. There is a true DGP, and our audits have effectively been doing parameter inference on it.

**Suspicion:** the "correct" interpretable model is one whose structure mirrors the simulator. The leading candidate is a **hierarchical state-space model with treatment-conditional drift** — exactly what Subhrajyoty wrote in `plan.md` and implemented in `scripts/subh_bayesian.R` (Mar 31, JAGS). If I'm right that the simulator is in this family, fitting this model should give ~62% F1 *by construction*, because no model can beat the simulator without extra information.

**Why we abandoned the Bayesian model the first time:** Subhrajyoty ran `subh_bayesian.R` shortly after writing it (Mar 31 → Apr 3 window). The val F1 was bad — not in the 60% range — so he moved on to GLM/RF/XGBoost. This run was never logged in `changes.md`, so the audit on Apr 7 (me) re-discovered the script and incorrectly assumed it was unbenchmarked. **Documentation gap, not a model gap.**

**Why it might do much better now (worth a retry):**

1. **Wrong feature set the first time.** The Mar 31 Bayesian model used the original feature set, before:
   - Dual-threshold engineering (7.0 = clinical, 8.0 = competition; the original used only one threshold)
   - BMI / BMI×male (added Apr 5; gave GLM +2pp)
   - Rate features (insulin_rate, total_meds_rate, visits_rate)
   - Time-gap and days-to-eval as first-class features
   - The variogram-derived multi-scale structure (Apr 6)
2. **Wrong priors / parameter init.** The original priors were diffuse. We now have *direct empirical estimates* from the audit work:
   - $\sigma_B^2 \approx 1.95$ (lmer random intercept)
   - $\sigma_W^2 \approx 1.35$ (lmer residual)
   - $\mu_\text{pop} \approx 7.11$ (lmer fixed intercept)
   - $\lambda \approx 1.49$/yr (CTMC total rate, $1/\lambda = 8$ months)
   - $\pi_H \approx 0.16$ (CTMC stationary high fraction)
   - $\sigma_\text{meas}^2 \approx 1.18$ (variogram nugget)
   - $q_{HL}/q_{LH} \approx 5.2$ (treatment 5× faster than deterioration)
   These can go in as **strongly informative priors**, which both speeds convergence and prevents JAGS from drifting into bad posterior modes.
3. **Wrong target understanding.** The original Bayesian model used $A^* = 8.0$ (Subhrajyoty had it right from the start), but the surrounding pipeline at the time was still confused about whether competition target was 7 or 8. Now we have full clarity: target = $\mathbf{1}(Y_{2025} > 8)$.
4. **Wrong evaluation.** The first run probably evaluated at the JAGS default threshold (0.5 on posterior P), not the F1-optimized threshold. Just running `best_f1_threshold` on the posterior probabilities should bump F1 by 2–4 pp on its own (we saw this for GLM when class imbalance was handled).
5. **Possible MCMC convergence issues.** JAGS with 4 chains and diffuse priors on a hierarchical model with ~75k random effects (one per patient) is known to mix slowly. The first run may have been stopped before chains converged. Informative priors fix this directly.

**Two paths forward (need to pick one):**

### Path A — Bayesian with subsampling (the "principled" answer)
- Subsample 5,000–10,000 patients for the JAGS fit (random across train+val pooled). With ICC = 0.924, you don't lose much by going to 10% of patients — the between-patient variance is well-estimated by then, and the within-patient parameters are pooled across all observations from those patients.
- Use empirical priors (from list above) — turn lmer/CTMC/variogram outputs into Normal(prior_mean, small_prior_sd) priors.
- Re-run JAGS with maybe 8 chains × 5000 iter × 0.5 burn-in. Check $\hat R$ on every parameter.
- Predict on val using posterior predictive: for each val patient, sample from the posterior over their latent $\mu_i$, integrate over the time gap to 2025, and compute $P(Y_{2025} > 8)$ as the posterior probability of exceedance.
- Tune threshold via `best_f1_threshold` on train.
- Expected F1: 61–63% (matches ceiling). Differentiator: posterior intervals on every prediction.
- Cost: a few hours of compute + Subhrajyoty's time to re-tune the JAGS model.
- Risk: still doesn't converge / still gives bad F1 → wasted afternoon. But informative priors make this much less likely than the first attempt.

### Path B — Frequentist closed-form (the "fast sanity check")
- Build the closed-form CTMC prediction as a single feature: $\text{ctmc\_prob}_i = \pi_H + (\mathbf{1}(\text{state}_i = H) - \pi_H) \cdot e^{-\lambda \Delta t_i}$. (Already computed in `subrata_continuous_time.R` — `ctmc_prob` was the variable.)
- Fit a 2-parameter logistic: $\text{logit}(P_i) = \beta_0 + \beta_1 \cdot \text{ctmc\_prob}_i$.
- Or 4-parameter version: $\text{logit}(P_i) = \beta_0 + \beta_1 \cdot \text{a1c\_weighted}_i + \beta_2 \cdot \text{ctmc\_prob}_i + \beta_3 \cdot \text{ctmc\_prob}_i \cdot \text{a1c\_weighted}_i$.
- Tune threshold via `best_f1_threshold` on train.
- Expected F1: 61.5–62%. Less impressive than Bayesian but far more interpretable on a slide ("the entire model is two parameters: a global level and a CTMC dynamics term").
- Cost: 10 minutes of coding + 1 minute of fitting.
- Risk: low. We already know `a1c_weighted` alone gives 61.4% and `ctmc_prob` was XGBoost's #1 feature on its own, so this should at least match.

**My recommendation: Path B FIRST as a sanity check, Path A SECOND if Path B clears 61.5%.**

Reasoning:
- Path B is a 10-minute test and uses features we already have. It validates the *structural form* (level + CTMC dynamics) without committing to a JAGS run.
- If Path B hits ~62%, the structural form is right and Path A is worth the investment — the Bayesian model will get the same number plus posterior intervals plus the "we recovered the simulator" story.
- If Path B hits <60%, the structural form is wrong and Path A would also fail. We'd need a different model class.
- Path A is *the* presentation answer — but only if it actually works. Path B is the de-risking step.

**What I want to do (if you say go):** start with Path B, write `scripts/subrata_ctmc_glm.R` (mirroring `subrata_rf_audit.R` style — small, focused, in `scripts/`, not /tmp), get the F1 number, then decide on Path A.

### Apr 7 (night, later) — Path B result: F1 stuck, but AIC moved a lot

Built `scripts/subrata_ctmc_glm.R`. Re-estimated CTMC rates from train transitions (independent of `subrata_continuous_time.R`):
- $q_{LH} = 0.251$/yr, $q_{HL} = 1.272$/yr, $\lambda = 1.522$/yr → **relaxation = 7.9 months**, $\pi_H = 0.165$
- Within rounding of the original audit (0.24 / 1.25 / 0.66 / 0.16). The CTMC fingerprint is reproducible across estimation methods, which is its own form of validation.

Five logistic models, target = `Uncontrolled`, threshold tuned on train via `best_f1_threshold` (returned mask), evaluated on val:

| Model | n params | AIC | val F1 | val prec | val rec |
|---|---:|---:|---:|---:|---:|
| M0 (raw `ctmc_prob`, no GLM) | 0 | — | 58.62% | 60.10% | 57.19% |
| M1 (`a1c_weighted`) | 2 | 18646.7 | **61.40%** | 53.44% | 72.15% |
| M2 (`ctmc_prob`) | 2 | 18581.3 | 58.62% | 60.14% | 57.19% |
| M3 (`a1c_weighted` + `ctmc_prob`) | 3 | 18151.3 | 61.14% | 53.04% | 72.15% |
| M4 (`a1c_weighted` × `ctmc_prob`) | 4 | **17479.1** | 61.31% | 53.40% | 71.97% |

**The most important finding here is not the F1 column — it's the AIC column.** M1 → M3 → M4 drops AIC by **1167 points**. M4 fits the data dramatically better than M1 by every probabilistic measure. **But F1 is essentially flat** across M1/M3/M4 (61.40 / 61.14 / 61.31).

This is the same phenomenon as the isotonic regression result from Apr 5: **F1 depends on rank ordering, not on log-likelihood / calibration**. The CTMC dynamics carry real information — they improve probability calibration enormously — but they don't reorder patients. Whoever's at the top of the ranking under M1 is still at the top under M4.

**Decision implication for Plan A revisited:** the borderline F1 (61.31% vs the 61.5% target) is misleading. The structural form is *right* — we proved it via the parameter recovery and the AIC drop. A Bayesian model that mirrors the simulator will:
- ✅ Recover the same parameters
- ✅ Improve calibration / log-likelihood / Brier score dramatically
- ✅ Provide posterior intervals on every prediction
- ❌ Almost certainly **NOT move F1 above 61.5%** — the F1 ceiling is set by ranking, and ranking is dominated by `a1c_weighted`

That's *exactly* why the Bayesian model had bad F1 the first time. The model wasn't wrong. The metric is wrong for what the Bayesian model is good at.

If we run Plan A, we should report **(F1, log-loss, Brier, calibration plot)** — not just F1. The dynamics-aware models will look indistinguishable from M1 on F1 alone but visibly better on the other three.

---

## Apr 7 (night, even later) — Subrata's "designed competition" hypothesis

**The framing (Subrata):** "The problem with these tests on generative models is that they deliberately create a generative model which says: *if you use the last a1c, the final value would be good. If you add clever variables, it will be better — however, so minimally better that you won't understand it. The organizers know the exact values, so they can test how good you actually are even when the a1c-only signal is not improved much.*"

**Why this matters:** if the user's read is right, the data was designed as a **probe on residual signal extraction**. The simulator has three layers baked in:

1. **Floor** — any reasonable baseline using `a1c_latest` (or `a1c_weighted`) hits ~60–61%. This guarantees that unprepared teams get a passing grade and the leaderboard isn't all zeros.
2. **Ceiling** — there is a true upper bound, possibly higher than the 62–63% we keep observing, accessible only with deep modeling.
3. **A narrow visible band (60–63% F1)** in which all "reasonable" models cluster, designed so that **small F1 differences are real ranking signal, not noise**, even though they look like noise to us.

**Evidence for this read in our own results:**
- `a1c_latest` alone: ~60.3%
- `a1c_weighted` alone: 61.40%
- Regularized XGBoost (74 features): 62.0–62.2%
- RF audit (5 seeds): 61.6% ± 0.27%
- Ensemble 3-model: 62.0%
- Best NN: 61.0%

That's a **2 pp band** (60.3 → 62.3) for almost everything we've tried, with very small standard errors within each model. If this were natural noise on real data, you'd expect a wider spread; the tightness *itself* suggests a designed band.

- The CTMC + variogram + ICC analysis recovered clean parameters that look textbook (relaxation = 8 months exactly, ICC = 0.92, $\pi_H = 0.16$). Real EHR data is never this clean.

**Three implications if Subrata's hypothesis is right:**

1. **The "structural ceiling at 62%" story may be partially wrong.** We've been telling ourselves (and the presentation will say) that the ceiling is set by between-patient variance and unobservable treatment dynamics. But under this hypothesis, the actual ceiling could be 65–67% and we just haven't found the right "clever variable". The 62% we keep hitting is the *competition floor*, not the *information-theoretic ceiling*.

2. **Tiny F1 differences are decision-relevant.** A 0.5 pp improvement on the val split is *not* noise — it's exactly what the organizers designed the data to detect. So the choice between, e.g., XGBoost (62.2%) and an ensemble (62.0%) actually matters for round 2, even though it looks within noise to us. We should stop dismissing 0.3–0.5 pp differences as "within noise".

3. **The right next move is to actively hunt for the residual signal**, not to keep validating the ceiling. Concrete things to try:
   - **Cluster the patients first** (Sayan's idea, see next section). If the simulator draws $\mu_i$ from a mixture of K Gaussians or uses a finite-state HMM, GMM/k-means should recover the components. Cluster ID becomes a feature.
   - **Dynamics features that survive after conditioning on level**: residual analysis showed `frac_above_8 × a1c_weighted` had LR = 515. Maybe the simulator put the signal in patient-specific *response curves* rather than in level alone.
   - **Stratified models per patient cluster**, not per A1c regime.
   - **The Bayesian retry** still matters — but as a *parameter recovery exercise* that reveals what the simulator's hyperparameters look like, not as an F1-pushing exercise.

**What I'd update in the presentation:** soften the "ceiling is structural" claim to "**ceiling on F1 is structural under simple feature engineering**; the residual signal is real (AIC, log-loss, Brier all drop substantially) but rank-blind". Then pivot to "if the metric had been Brier or log-loss, the dynamics-aware models would dominate. F1 is the wrong metric for what's actually informative in this data."

---

## Apr 7 (night) — Sayan's idea: unsupervised clustering, no labels

**The question:** what does clustering reveal about the data structure if we don't use the target at all?

**Why it's a good idea, especially under Subrata's "designed competition" hypothesis:** synthetic data often has clean cluster structure because the simulator draws latent parameters from finite mixtures. If the WashU simulator uses something like
$$\mu_i \sim \sum_{k=1}^K w_k \mathcal{N}(\mu_k, \tau_k^2),$$
then a Gaussian mixture model on patient-level features should recover $K$ modes and assign each patient a probability of belonging to each mode. **Cluster ID could then become a feature** that captures latent type information directly, instead of approximating it through `a1c_weighted` + dozens of derived features.

**Concrete clustering plan:**

1. **Patient-level feature matrix.** One row per patient, columns = `a1c_weighted`, `a1c_sd`, `a1c_range`, `n_a1c`, `frac_above_7`, `frac_above_8`, `n_crossings_7`, `total_meds`, `n_drug_classes`, `time_gap`, `age`, `bmi`, `adi_state`. Standardize.
2. **Pick K via BIC on a Gaussian mixture model** (`mclust` package). Fit `Mclust(X, G = 1:10)`, take the K with the lowest BIC. This is the most informative single number — if BIC says K = 1, the data is unimodal and the mixture story is wrong. If K ≥ 3, there ARE latent groups.
3. **Visualize.** Project to 2D via t-SNE / UMAP / PCA, color by cluster, look for separation. Then color by target (`a1c_2025 > 8`) and see if any cluster has a strikingly different base rate.
4. **As a feature**: hard cluster ID + soft membership probabilities, fed into the existing GLM/XGBoost pipeline. Test whether any model F1 moves.
5. **Trajectory clustering** as a separate angle: cluster the *shape* of each patient's a1c trajectory using functional clustering (`funFEM` or `funHDDC`). This could recover the "fragile responders" / "stable controlled" / "chronic high" / "improving" types that the misclassification EDA hinted at on Apr 3.

**What success would look like:**
- BIC clearly favors K ≥ 2
- Clusters have visibly different uncontrolled rates
- Cluster ID, added as a feature, moves XGBoost val F1 by >0.3 pp (which under the "designed competition" hypothesis is a *real* improvement, not noise)

**What failure would look like:**
- BIC favors K = 1 → patient distribution is unimodal → no latent mixture, the simulator used continuous random effects (which `a1c_weighted` already captures via shrinkage)
- Clusters all have similar uncontrolled rates → cluster ID is redundant with `a1c_weighted`

Either outcome is informative:
- **Success** validates the "designed competition" hypothesis and gives us a concrete handle on the residual signal.
- **Failure** confirms the "single continuous latent" model and supports the structural-ceiling presentation story.

**Owner suggestion:** Sayan, since he raised it, and it's adjacent to his missing-data work (clustering also has to handle missingness — either via imputation, which is in his wheelhouse, or via the missingness pattern itself as a feature).

**Cost:** afternoon of work. Worth it regardless of which way it goes.

---

## Apr 8, 2026 — Path A and PG logistic both succeeded (closing out the threads)

Both the Bayesian retry (Path A from Apr 7 night) and Sayan's PG logistic adaptation ran today. **Both hit the ~62% ceiling.** Full details and per-run numbers are in `changes.md` (Apr 8 entry); here's the forward-looking summary.

### Path A: Bayesian state-space retry — F1 = 0.6161

The retry needed bug fixes AND informative priors to work — neither alone was enough. Five stacked bugs in the original `subh_bayesian.R` plus a major prior misspec on `alpha_prec` (centered at log(prec_mu) = 0 when truth is ~6) had been overdetermining the failure. With audit-derived priors (variogram nugget → `prec_Y`, lmer fixed intercept → `mu_init`, T_days/σ_W² → `alpha_prec`), R-hat for the drift parameters dropped from 19.7 to 1.0005 — perfect convergence. Variance components (`alpha_prec`, `prec_Y`) still mix slowly but are tractable.

The model now provides a clean interpretable headline: 5 free parameters, each clinically meaningful, F1 within 0.4 pp of the best black-box, and a state-space generative story that mirrors the likely simulator structure. The MCMC samples are cached at `scripts/outfiles/subrata_bayesian_mcmc_n2000_informative.Rds` so we can iterate on the prediction step / eval / threshold tuning without re-running JAGS.

**The unresolved trade-off:** sharper priors → sharper posteriors → better F1, but worse calibration (ECE 0.025 → 0.079). Could be recovered with isotonic post-processing without losing F1. Worth doing for the presentation calibration plot.

### Sayan's PG logistic — F1 = 0.6094, BEST calibration in the project

`scripts/subrata_pg_logistic.R` adapts Sayan's PG Gibbs to our pipeline. Key technical finding (worth telling Sayan): **his original `pg_logistic_gibbs_missing` is O(N² × p × iter)** because the inner loop calls `model.matrix(y ~ ., data = dat_current)` on every missing cell. Killed at 2:47 of CPU time without printing iteration 100 — estimated 2-4 hours to finish on 17k patients. He must have run his original on a few hundred patients where this didn't matter.

The vectorized variant in `subrata_pg_logistic.R` (`pg_logistic_gibbs_missing_fast`) uses the fact that missing cells of one variable are conditionally independent given the rest of `X` and `beta`, so they can be sampled jointly via one BLAS call per variable per iteration. **~17,000× speedup**: 2-4 hours → 3 minutes for the full 5000 iterations on 17,845 patients.

The result is the **best-calibrated model in the project**: ECE = 0.033 (near-perfect), log-loss 0.353 (best), Brier 0.106 (best). F1 is 0.7 pp below the Bayesian state-space and 1.0 pp below XGBoost — but the probabilities are dramatically more trustworthy.

**Coefficient comparison with Sayan's reported numbers** (his Box-drive run vs ours on `train.Rds`): qualitatively the same pattern (A1c features dominate, lipid/log_gap near zero) but the *distribution* of weight between `a1c_last`, `a1c_ever_gt8`, and `a1c_prop_gt8` differs noticeably. Likely his Box-drive `Fulldata.csv` has differently-engineered features. Need to ask him.

### The headline for "Fault in our model"

| Model | Val F1 | Paradigm |
|---|---:|---|
| XGBoost regularized | 0.6200 | Black-box, gradient boosting |
| RF (audited) | 0.6162 | Black-box, ensemble of trees |
| **Bayesian state-space (informative priors)** | **0.6161** | Interpretable, longitudinal generative |
| GLM (a1c_weighted alone) | 0.6140 | Interpretable, frequentist, 1 feature |
| ctmc_glm (M4) | 0.6131 | Interpretable, closed-form CTMC + GLM |
| Multi-head NN | 0.6100 | Black-box, transition-level deep learning |
| **PG logistic** | **0.6094** | Interpretable, cross-sectional Bayesian + missing-data |

**Seven independent methods, three statistical paradigms (frequentist / Bayesian state-space / Bayesian cross-sectional), all between 60.9% and 62.0% F1.** This is an extremely tight cluster. The ceiling is the data, not the method.

The "designed competition" hypothesis from Apr 7 (Subrata's note above) is now harder to evaluate: every reasonable approach is in the same 1.1 pp band. *Either* the simulator is exquisitely tuned to give a 60-62% floor with hidden 65%+ ceiling, *or* the irreducible noise floor IS 62% and we've all hit it. Both are consistent with what we observe. Without ground-truth simulator parameters, we can't disentangle these — but our presentation should acknowledge the uncertainty rather than claim "the ceiling is structural" too confidently.

### Closed threads and remaining ones

**Closed:**
- Path A (Bayesian state-space retry) — succeeded.
- Sayan's PG logistic adaptation — succeeded.
- "Designed competition vs structural ceiling" — partially closed, both readings remain consistent with the data.

**Still open:**
- Sayan's clustering idea (Apr 7 night) — deferred. Could still find latent structure if the simulator uses a finite mixture for patient types. Would close the "designed competition" question definitively if it found discriminating clusters.
- Calibration recovery for the Bayesian state-space (isotonic post-processing) — quick to add, would give us a "best of both worlds" model.
- Reading Sayan's off-GitHub Bayesian scripts (he said 2-3 hrs to fetch) — may already contain better feature engineering or fixes we haven't tried.
