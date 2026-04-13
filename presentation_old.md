# Presentation outline & storyline

Working draft of the slide structure for the WashU I2DB Datathon 2026 final presentation. Goal: tell the story of what we did, what we found, and what's actually impressive about it. Three-act structure with the "Fault in Our X" pun.

---

## Subhrajyoti's original proposal (for reference)

### Act 1: The fault in our data
- **Slide 1**: Describe the data, show missingness, emphasize that people don't visit, and sometimes things are not measured properly → so need some cleaning.
- **Slide 2**: Describe the EDA plot (violin plot) of (change rate of a1c) conditional on whether the current a1c value > 8 or < 8. Explain that < 8 patients are equally likely to improve or not.

### Act 2: The fault in our models
- **Slide 3**: We started with simple models (say GLM with median imputation) → gave F1 score of 58%. Added relevant features → improved to 60%. Tried more complex models (Random Forest, Decision Tree) → stuck at 60% → we were hunting for more precision.
- **Slide 4**: Designed 2-state transition model: each transition probability modelled by NN → maybe NN will help us select relevant features (but not so interpretable) → look at hidden state values → show some plot of hidden state values vs `a1c_latest`, over interactions (some simple plots that diagnose what kind of features NN is making). Or maybe show some Shapley-value kind of thing (we can actually calc Shapley values for XGBoost model instead of NN).
- **Slide 5**: Found a set of optimal features. Used XGBoost (because of imbalanced data, need boosting to low-prob class).

### Act 3: The fault in our health
- **Slide 6**: Show important features identified by XGBoost. Explain why they might be relevant.
- **Slide 7**: Discuss where modelling falls short. What more data may be insightful. What additional health data can be collected in EHR to improve analysis (e.g., number of different meds taken between each successive visits).

---

## What's good about this structure

- **3-act structure mirrors the natural data-science narrative.** Data → models → interpretation. Easy to follow.
- **Slide 2 (AB's conditional A1c slope EDA)** is a *great* opener for "here's what makes this hard". One plot that tells the story — the asymmetry between high and low patients is non-obvious and clinically meaningful.
- **Slide 6 (XGBoost feature importance + clinical interpretation)** is the right closer for the modeling section.
- **Slide 7 (what data we'd want)** is the strongest version of the "fault in our health" pun and the hardest for judges to dismiss.

---

## Concerns

### Concern 1 — Slide 3 underclaims our work and overclaims the difficulty

> "Started with simple GLM (58%) → added features (60%) → tried RF/Tree (stuck at 60%) → were hunting for more precision"

This **doesn't match the actual numbers**:
- Subhrajyoti's true baseline (latest A1c > 7) is ~55%, not 58%.
- After Subrata's 50 hand-engineered features, GLM hit 60%, glmnet 61.7%, GAM 62%, **XGBoost 62.3%** — already on Day 1 (Mar 27).
- After dual-thresholds + BMI + rate features + class weighting, GLM hit 61.9%, ensemble 62.0%, XGBoost regularized 62.2%.
- We were never "stuck at 60%" — we hit 62% on Day 1 and have been there since.

The honest progression is **55% → 60% → 62%**, with the second jump coming from feature engineering and the ceiling at 62% being structural, not "we ran out of ideas". If the slide says "stuck at 60%", a sharp judge will look at the leaderboard, see XGBoost at 62%, and think we're under-reporting our own work.

**Suggested rewrite for Slide 3:**

> "Baseline (latest A1c > 7) → 55%. 50 hand-engineered features → 60%. Dual-threshold + BMI + rate features + class weighting → 62%. Then **four model classes (GLM, RF, GAM, XGBoost) all converged within 0.5 pp of each other.** That convergence was the first hint the ceiling was structural."

### Concern 2 — Slide 4 (transition NN with hidden state interpretability) is a misallocation of slide budget

The multi-head NN got **61.0% F1** — *below* the GLM and Bayesian. The pitch "NN helped us select features for XGBoost" is post-hoc rationalization. XGBoost has its own feature importance, and the features the NN suggests are essentially the same ones we already had from MI/correlation analysis on Day 1.

If you're going to spend a slide on interpretability of black-box models, **Shapley values on XGBoost are strictly better than hidden-state diagnostics on a model that underperforms by 1 pp.** Subhrajyoti even hedges this in his note ("Or maybe show some Shapley-value kind of thing...").

**Two cleaner options for Slide 4:**

- **(a) Drop the NN entirely.** Replace with a slide on the **interpretable Bayesian / PG / GLM ceiling-tying triumvirate** (see "Things to add" below). Three principled interpretable models hitting the same ceiling as XGBoost — much stronger story than one underperforming NN.
- **(b) Keep the NN but reframe it.** "We tried a multi-head transition NN to see if dynamics gave us anything beyond the GLM. We got 61% — below GLM. **The NN has the same ceiling.** This was the second hint that the ceiling is structural, not modeling-limited."

**Recommendation: option (a).** The NN was useful for *us* methodologically (we learned about transition models, found an ID-mismatch bug, etc.) but it's not a presentation-strong result.

### Concern 3 — Missing the structural ceiling slide

Subhrajyoti's structure jumps from "we tried things" (Slide 5) to "feature importance" (Slide 6) without ever asking *why the ceiling exists*. This is the deepest part of our work and judges who've seen many datathon presentations will be looking for it.

We have **three independent statistical analyses** that all explain the ceiling:
- **ICC = 0.924** — variance decomposition: 92.4% between patients, 7.6% within. No within-patient method can beat ~62%.
- **CTMC relaxation time = 8 months** — after 8 months, current state is uninformative → most patients have time_gap > 8 months → predictions revert to base rate.
- **Within-patient ACF is *negative*** — treatment-induced oscillation. AR/OU/GP/lmer/GEE all structurally wrong for within-patient dynamics. But it doesn't matter because within-patient is only 7.6% of total variance.

**Three completely different methods converging on the same conclusion.** A single slide showing all three side by side would be the strongest technical-merit slide in the deck.

---

## Things to add (concrete)

### NEW Slide 3.5 — "Why we suspected the ceiling was structural"

Three panels side by side:
1. **Variance decomposition pie chart** showing 92.4% between-patient, 7.6% within-patient. Caption: "ICC = 0.924 — patient identity dominates."
2. **CTMC transition probability plot** P(H | last state, Δt) decaying exponentially. Caption: "Relaxation time = 8 months. After that, the last reading is uninformative."
3. **Within-patient ACF plot** showing negative correlation at all lags. Caption: "Treatment creates oscillation, not persistence. Standard time-series models are structurally wrong here."

One sentence below the panels: "Three independent analyses, three completely different methods, same answer: between-patient variance dominates; trajectory dynamics are bounded to ~7.6% of variance."

### NEW Slide 4 (replacing the NN slide) — "Three interpretable models, three paradigms, same ceiling"

Table:

| Method | Paradigm | Val F1 | What it explains |
|---|---|---:|---|
| Bayesian state-space | Longitudinal generative | **0.6161** | treatment dynamics, patient heterogeneity |
| PG logistic + missing-data Gibbs | Cross-sectional Bayesian | 0.6094 | proper missing-data handling, **best calibration in project** |
| GLM on a1c_weighted alone | Frequentist, 1 feature | 0.6140 | the simplest possible interpretable baseline |
| **Reference: XGBoost (74 features, black-box)** | **Gradient boosting** | **0.6200** | — |

One sentence: "We built three principled interpretable models from completely different statistical traditions. All match XGBoost within 1 pp. **The ceiling is the data, not the method.**"

If room: a **coefficient table for the Bayesian** (5 interpretable parameters with clinical names — `δ` = treatment drop in A1c units, `prec_µ` = patient-specific variance per day, `µ_pop` = population A1c mean, etc.). This is what makes "interpretable" concrete.

### Add to Slide 6 — "F1 vs calibration"

Calibration plot: PG logistic (near-perfect, ECE = 0.033) vs the simpler GLM. They have nearly the same F1 but very different probability quality.

One sentence: "F1 depends on rank ordering, not on probability calibration. Adding longitudinal dynamics dropped log-loss from 0.45 to 0.35 — the dynamics are real, but F1 is blind to them. **The metric is wrong for what's actually informative in this data.**"

This is a publication-quality observation. Most teams will not have made it.

### Add to Slide 7 — explicit synthetic-data caveat

One sentence at the end:

> "The data is synthetic. The 'ceiling' we observe at 62% may be the simulator's irreducible noise floor, OR it may be a designed competition floor with a hidden 65%+ ceiling we haven't found. Without ground truth, we can't disentangle these. We're confident the ceiling exists; we're appropriately uncertain about *where exactly* it is."

This is **disarming honesty** that judges respect. It also pre-empts the "did you really hit the ceiling?" challenge.

---

## Things I'd cut or shrink

- **The NN as a feature-selection step for XGBoost.** Post-hoc rationalization, weak.
- **Anything implying "the ceiling is set by unobserved data"** without also acknowledging the synthetic-data caveat. Real EHR data has unobservable adherence/lifestyle/timing; synthetic data has whatever the simulator decided to expose. Mixing the two is sloppy.

---

## Net suggested structure

| Slide | Subhrajyoty's | My suggested |
|---|---|---|
| 1 | Data + missingness + cleaning | same |
| 2 | AB's conditional A1c slope EDA | same |
| 3 | Simple GLM → features → "stuck at 60%" | **Honest progression: 55% → 60% → 62%, six model classes converged within 1 pp** |
| **3.5 (NEW)** | — | **"Why we suspected the ceiling was structural" — ICC + CTMC + negative ACF** |
| 4 | NN transition + hidden state interpretability | **"Three interpretable models, three paradigms, same ceiling" — Bayesian + PG + GLM table** |
| 5 | Found optimal features → XGBoost | **XGBoost as the leaderboard model + interpretable Bayesian as the alternate headline** |
| 6 | XGBoost feature importance + clinical | **Same, plus calibration plot showing F1-vs-calibration trade-off** |
| 7 | Where modeling falls short, what data we'd want | **Same, plus synthetic-data caveat** |

7 slides → 8 slides. If 8 is too many, cut Slide 5 (XGBoost can be one paragraph on Slide 4 since the table already includes it).

---

## The sharpest single line for the whole deck

> "We built six different models from three different statistical paradigms. They all hit 62% F1. **Our deepest finding isn't a model — it's that the ceiling is real and we have three independent statistical proofs of why.**"

---

## Open questions / things to decide

- **Title.** "The Fault in Our \_\_\_" — pick three nouns. "Data / Models / Health" is clean. "Data / Methods / Health" is more honest about scope.
- **Who presents which act?** Natural assignment: Subhrajyoti (cleaning + Bayesian), Subrata (features + audits + dynamics), Sayan (missing data + PG logistic), AB (NN + EDA). All four contribute.
- **Demo or static?** If demo: live calibration plot, change of threshold, posterior intervals on a single patient. If static: all of that as figures.
- **Length.** Datathon presentations are typically 8–12 minutes. 7 slides is tight, 8–9 is comfortable.
- **What to name the round-2 model?** Currently the submission is the regularized XGBoost from `final_test.R`. Should we also submit the Bayesian as an interpretable alternative if the rules allow multiple submissions?

---

## Status of supporting material (as of 2026-04-08)

- ✅ Cleaning + EDA plots (Subhrajyoti, AB) — exist in `figures/`
- ✅ Variance decomposition + ICC (Subrata, `subrata_multiscale_acf.R`)
- ✅ CTMC analysis + plots (Subrata, `subrata_continuous_time.R`)
- ✅ Within-patient ACF plots (Subrata, `subrata_multiscale_acf.R`)
- ✅ Bayesian state-space results (Subrata + Subhrajyoti, `subrata_bayesian.R` + `subrata_model.jags`)
- ✅ PG logistic results (Sayan + Subrata, `subrata_pg_logistic.R`)
- ✅ Six-model F1 table (compiled in `changes.md` Apr 8 entry)
- ⚠️ Calibration plot for Slide 6 — **needs to be made**. Eval harness writes calibration tables, but we need a side-by-side reliability diagram of GLM vs Bayesian vs PG logistic. Quick — maybe 30 lines of ggplot.
- ⚠️ Bayesian coefficient table with clinical names — **needs to be assembled**. We have the posterior summary in the JAGS output, but it needs to be relabeled with clinical interpretations (`δ` → "treatment drop in A1c units", etc.).
- ⚠️ The "honest progression" plot for Slide 3 — **may need to be made**. Could just be a bar chart of F1 across model classes ordered by feature complexity.

None of the missing material is hard to make — half a day of plotting. Worth doing before the rehearsal so the team can see the deck end-to-end.
