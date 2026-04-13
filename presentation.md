# Presentation — medical-audience version

**Status (2026-04-13):** Presentation delivered. At least 3 audience questions during Q&A, positive feedback from 2+ attendees and 1 organizer. Deck submitted 2026-04-09; presented ~Apr 10–13 window.

---

## Framing rules (what we assume about the audience)

The audience is physicians, nurses, EHR-aware clinical researchers — people who think in terms of individual patients, treatment decisions, labs, appointments, and medication burden. They are NOT statisticians.

**Terms that must NOT appear on a slide or in the talk:**
- ICC / variance decomposition / autocorrelation / ACF
- CTMC / Markov chain / Ornstein-Uhlenbeck / Gaussian process
- BLUP / mixed model / lmer / GEE
- Polya-Gamma / Bayesian vs frequentist
- F1 score / precision / recall / calibration / Brier / log-loss / ECE
- "paradigm convergence" / "six models"
- "ceiling" (unless immediately followed by a clinical explanation)
- "glycemic index" (food-based concept, not what we mean)
- "phenotype" (genetics connotation — say "baseline disease severity" instead)

**Clinical translations:**

| Statistical term | Clinical translation |
|---|---|
| ICC = 0.924 | "Each patient has their own typical A1c level. Once you know it, month-to-month fluctuations barely matter for predicting 2025." |
| CTMC relaxation = 8 months | "An A1c reading stays 'fresh' for about 8 months. After that, it's barely more informative than the population average." |
| Within-patient ACF is negative | "Treatment works but overshoots — after crossing above 7, the next reading usually comes back down below baseline, then drifts up again." |
| Six paradigms converge | "We tried several very different modeling approaches. They all land in the same place. That tells us it's the information, not the method." |
| `days_to_eval` was a confound | "One of our features looked important but was actually measuring study duration, not anything clinical. Once removed, the model stayed the same." |

---

## The story (three beats)

### Beat 1 — The data reflects clinical practice
Patient trajectories look exactly like what you'd expect: when A1c goes above 7, the next reading usually comes down (intervention); when it's below 7, it drifts either way. The data *behaves* the way diabetes care actually works.

### Beat 2 — What actually drives the prediction
Our presentation model ("SLIM NEW") uses 28 clinically interpretable features, all built around a single threshold — the competition definition of uncontrolled, A1c > 8. The three most important are:
1. **Time since the last A1c reading** (36% of the model's "attention")
2. **The weighted average of all past A1c readings** (20%)
3. **The most recent A1c reading** (8%)

That's 64% of the model's decision-making in three features. The #1 feature is not a lab value — it's *data freshness*.

### Beat 3 — Where the model fails, and why
We plateau at ~62% accuracy on the uncontrolled class. The errors split into two clinically meaningful groups:
- **"Silent deteriorators"** — patients who looked controlled at their last visit and got worse by 2025.
- **"Treatment responders"** — patients with high A1c at last visit whose doctor intervened and brought them under control by 2025.

**Both groups require information that isn't in the EHR.** If you want a better model, the lever is the data, not the algorithm.

---

## Slide structure (10 slides, ~7 min)

**Submitted 2026-04-09.** Current deck: `present_slides/I2DB ppt.pdf`.

| # | Slide title | Content | Time | Status |
|---|---|---|---:|---|
| 1 | Title | Team SASS + title (no subtitle) | 0:15 | ✅ done |
| 2 | Landscape of the Dataset: Challenges and Patterns | 6-panel kadam ful trajectories by age × sex | 0:45 | ✅ done |
| 3 | Distribution of Change Rate in A1c Measurements | Violin plot: A1c gradient by age group, faceted < 8 vs ≥ 8 (green/orange) | 0:50 | ✅ done (new faceted plot, annotation verbal only) |
| 4 | Statistical Modelling Framework | 3-column: Models / Missing Data / Imbalanced Data | 0:30 | ✅ done |
| 5 | 2-state Transition Modeling via Shared Representations | State diagram + NN architecture + logistic equations (kept) | 0:50 | ✅ done (equations kept as depth signal, not narrated) |
| 6 | Hunting for Features through Data | NN hidden-node heatmap (Spearman correlation) | 0:30 | ✅ done |
| 7 | Biological Feature Hunting | 8-row feature × description table | 1:00 | ✅ done (final wording 2026-04-09) |
| 8 | Predictive Power of XGBoost | Feature importance (top 15) + ROC (AUC 0.880) + density plot | 1:30 | ✅ done (footnote: "AUC = 0.938 based on the organizers' test data") |
| 9 | "All models are wrong" — So What Should We Do? | Left: 4 bullets. Right: "What's not in the charts" with icons | 1:00 | ✅ done |
| 10 | Questions? | Team photo + contacts | 0:10 | ✅ done |

Total ≈ 6:50. Template pages deleted. **Deck submitted.**

---

## Slide-by-slide notes

### Slide 1 — Title (0:15)

- Title: **"Predicting Future Uncontrolled Diabetes from Electronic Health Records"**
- No subtitle (decided to drop — title stands on its own, opening line of talk does the subtitle's job)
- Team name SASS — memorable, not disrespectful. Subrata Pal* = presenter.

**Talk:** *"Our task was to predict, using longitudinal patient data, whether each patient's HbA1c would still be uncontrolled — above 8 — in 2025. I'll show you what the data looks like, what actually drives our prediction, and where we hit a wall."*

### Slide 2 — Landscape of the Dataset (0:45)

- ✅ Title updated: "Landscape of the Dataset: Challenges and Patterns"
- Caption: "x-axis denotes the number of days from the baseline"

**Talk:** Point out the kadam ful shape — each patient has a "typical" A1c they hover around. Some swing across thresholds, some stay flat.

### Slide 3 — Distribution of Change Rate (0:50)

- **Updated figure (2026-04-09):** new faceted violin plot. Left panel: A1c < 8 (green, 3 age groups). Right panel: A1c ≥ 8 (orange, 3 age groups). Much cleaner than the old gender × age version.
- No on-slide annotation — the contrast speaks for itself. Annotation delivered verbally.

**Talk:** *"Left: below 8, symmetric around zero across all ages — no systematic drift. Right: above 8, wider spread, shifted negative — treatment kicks in. The response is age-invariant. Higher the A1c, more erratic the trajectory."*

### Slide 4 — Statistical Modelling Framework (0:30)

- 3-column layout: Models | Missing Data Handling | Imbalanced Data Handling
- Models list: GLM, GAM, Decision Tree, SVM, Bayesian Hierarchical, RF, 2-state NN, Boosting
- This is a "credibility slide" — shows breadth of approaches tried

### Slide 5 — 2-state Transition Modeling (0:50)

- State diagram (Controlled ↔ Uncontrolled) on the left
- NN architecture flowchart on the right
- **Logistic equations KEPT** (reversed earlier recommendation, 2026-04-09). Equations stay as a depth signal — shows mathematical rigor for quantitative judges. Don't narrate them during the talk; let them sit as background credibility. If asked: "Standard logistic link, separate heads for each transition direction, shared hidden layer."

**Talk:** *"Treatment response is a transition between two states. These transitions are driven by different dynamics. So we built a neural network that answers two questions separately — one head for each direction — while sharing the hidden representation."*

### Slide 6 — Hunting for Features (0:30)

- NN heatmap: Input Features × Hidden Nodes, Spearman correlation
- Feature clusters visible on the left (treatment intensity, A1c dynamics, demographics, data completeness)

**Talk:** *"When we looked inside the NN, we found four clean clusters. This is the model showing its work — it gave us the shortlist of features to look at biologically."*

### Slide 7 — Biological Feature Hunting (1:00) — **KEY SLIDE**

**Updated 2026-04-09.** 8-row table, 2 columns (feature name + description with bold keywords). No column headers needed. Biology-first ordering, data/measurement features second.

| Feature | Description |
|---|---|
| **A1c summary** (recency-weighted average) | Smooths out the spikes, **weighted toward the present**. |
| **First A1c reading** | Captures **baseline disease severity**, independent of current treatment. |
| **Fraction of readings above 8** | How often the patient crosses the uncontrolled threshold: **chronic** uncontrol or **transient** spikes. |
| **A1c per treatment load** | Treatment-resistant: high means **uncontrolled despite treatment**. |
| **Days since last A1c** | Long time gap between visits means reading is **outdated**. Consistent with ADA's 3–6 month monitoring recommendation. |
| **Biggest single-visit worsening** | A sudden jump might suggest a **different clinical story** than a slow drift. |
| **Drift rate while controlled** | For a patient below uncontrolled threshold, A1c **can increase, stay flat, or decrease** between visits. |
| **Number of A1c readings** | More readings suggest **unhealthy or more engaged** patient, and more **precise** estimates. |

**Design:** Left column = bold red feature names (~16pt). Right column = description with strategic bold on key words (~14pt). Alternating white/teal rows for readability.

**Caption (optional, below table):** *"Each maps to a distinct clinical mechanism. The model learned what a diabetologist already knows."*

**Omitted from this slide (already on Slide 8 or obvious to audience):**
- Days since last A1c is #1 on Slide 8 — it's here for completeness but the impact story lives on Slide 8
- HDL cholesterol — rank 9 on Slide 8, "metabolic syndrome" is obvious to every physician
- BMI — in the model but below top 15, mention verbally if asked

**Q&A prep — "Why is days since last A1c the most weighted?" (will be asked):**
*"It's not about the patient's biology — it's about how much we know. A recent A1c is almost ground truth. An A1c from a year ago tells us very little about where the patient is today. The model learned that the strongest predictor of whether it can make a good prediction is whether it has fresh data. That's why we put it in the feature importance table [Slide 8] rather than leading the biological features table [Slide 7] — it's an information quantity signal, not a clinical signal."*

If they push further: *"Our continuous-time analysis showed the decay window is about 8 months — after that, the old reading is barely more informative than the population average."*

### Slide 8 — Predictive Power of XGBoost (1:30) — **CENTERPIECE**

- Left: feature importance table (ranks 1–15, all with bold key words and clinical labels)
- Top right: ROC curve with AUC = 0.880 and decision threshold gradient
- Bottom right: predicted probability density (Controlled teal vs Uncontrolled pink)
- Footnote: "* AUC = 0.938 based on the organizers' test data"

**Verified val metrics (SLIM NEW, 28 features, val split, returned patients only, n = 9056, base rate 18.7%):**

| Metric | Value |
|---|---:|
| F1 | 0.619 |
| AUC | 0.880 |
| R² (Efron) | 0.339 |
| R² (McFadden) | 0.336 |
| R² (Tjur) | 0.337 |
| Brier | 0.100 |

**AUC (Test) = 0.938 context:** Organizers computed this on their test data. Inflated because Subhrajyoty set P=0 for non-returned patients (~1/3 of test set), which are trivially controlled — free true-negatives that push AUC up. Mention verbally near conclusion: *"The test AUC includes patients who didn't return — those are guaranteed controlled, so the AUC is somewhat generous."*

**Talk:** Focus on the top 3 features (time_gap 35.8%, a1c_weighted 20.4%, a1c_latest 8.4% = 64% of model attention), then the ROC curve ("88% ranking accuracy"), then the density plot ("controlled patients cluster low, uncontrolled spread across the range — the uncertainty lives in borderline patients").

**Q&A sidebars:**
- R²: "0.34 on val, all three definitions agree within 0.01."
- AUC-F1 gap: "AUC measures ranking, F1 measures threshold trade-off. With 19% positive class, the threshold is the bottleneck, not the ranking."
- Insulin per week: intentional scale change from per-year for clinical readability. XGBoost gain is scale-invariant.

### Slide 9 — "All models are wrong" — So What Should We Do? (1:00)

**Updated 2026-04-09.** Two elements:

**Left side:** Two bullets:
1. "Real EHR data is messy — out-of-order timestamps, some implausible values. We built our pipeline to handle these, because any deployed model would face the same."
2. "Our models learn what's in the chart, but reaches a systematic ceiling. ***What it misses is what the patient does between visits*** — and that's exactly the information that would make it a decision tool instead of a triage tool."

**Right side:** Single-column table "What's not in the charts" with template icons:
- 💊 Medication adherence
- 🍽️ Diet and nutrition
- 🏃 Physical activities
- 📅 Treatment timing (start/stop)
- 💰 Financial and life disruptions
- 🧠 Mental health and stress

Icons: pill and brain from WashU template, calendar and money from WashU template, nutrition and activity sourced externally (outline style to match).

**Talk:** *"Every error our model makes falls into one of two buckets — a patient who looked fine but deteriorated, or a patient who looked bad but responded to treatment. Both require information from this list."*

**Closing line:** *"Our model is a decent triage tool. What it can't tell you is what the patient does between visits."*

### Slide 10 — Questions? (0:10)

- Team photo (the cooking photo — memorable, humanizing)
- Names + emails
- ✅ Done, no changes needed.

---

## Post-submission grammar fixes (minor, for any revision round)

| Slide | Fix |
|---:|---|
| 9 | "but **reaches** a systematic ceiling" → "but **reach**" (models is plural) |
| 7, row 8 | "More readings **suggests**" → "**suggest**" (readings is plural) |
| 7, row 7 | "For patient below" → "For **a** patient below" (missing article) |
| 8 footnote | "organizer's" → "organizers'" (plural possessive) |

---

## Q&A preparation

**Expected questions and prepared answers:**

1. **"Why is days since last A1c the most important feature?"** — See Slide 7 Q&A prep above.

2. **"What's the R²?"** — "0.34 on the validation set, with Efron, McFadden, and Tjur definitions all agreeing within 0.01."

3. **"Did you try [specific model]?"** — "Yes, we tried 8 model families including GLM, GAM, SVM, Random Forest, Bayesian hierarchical, and neural networks. They all converge between 60.9% and 62.2% F1. The ceiling is the data, not the method."

4. **"What about the AUC-F1 gap?"** — "AUC measures ranking — the model correctly orders 88% of patient pairs. F1 measures what happens after you pick a threshold. With a 19% positive class, there's a precision-recall trade-off that caps F1 below AUC."

5. **"What about the test AUC of 0.938?"** — "The organizers computed that on their test data. It's somewhat generous because patients who didn't return for a 2025 reading are guaranteed controlled, and our pipeline assigns them probability zero — free true-negatives."

6. **"Why not include [social determinants / BMI / etc.]?"** — "They're in the model — BMI, neighborhood deprivation (ADI), demographics. They contribute, but below the top 10 in importance. A1c history dominates."

7. **"What about the logistic equations on the NN slide?"** — "Standard logistic link, separate heads for each transition direction, shared hidden layer. g₀ handles controlled→uncontrolled transitions, g₁ handles persistence of the uncontrolled state."

---

## The sharpest single line for the whole deck

> "Our model is a decent triage tool. It tells you who needs a closer look. What it can't tell you is what the patient does between visits — and that's exactly the information that would make it a decision tool."

---

## Supporting material (verified)

- ✅ SLIM NEW 28-feature XGBoost: val F1 = 0.6186, AUC = 0.8800, R² = 0.339, Brier = 0.100
- ✅ `days_to_eval` confound audit complete
- ✅ All metrics computed in `scripts/subrata_models_slim.R`
- ✅ Predictions saved to `data/processed/slim_predictions.Rds`

## Evolution of the slide outline

| Revision | Slides | Key change |
|---|---:|---|
| Original (`presentation_old.md`) | 7 | Statistics-first |
| First medical rewrite (2026-04-08) | 6 | Cut NN, CTMC on Slide 5 |
| Second revision (2026-04-08 night) | 9 | Kept NN, added biological feature hunting |
| 2026-04-09 (day) | 10 | Added Slide 4 (Statistical Modelling Framework), polished Slide 7 (8-row table with bold keywords), polished Slide 9 (single-column "not in chart" with icons), confirmed all metrics |
| **Submitted (2026-04-09 night)** | **10** | **Title filled, no subtitle. New faceted violin plot (Slide 3). Equations kept on Slide 5. Slide 7 final wording (treatment-resistant, outdated, etc.). Slide 8 footnote fixed, rank 6 aligned with Slide 7. Template pages deleted. Submitted.** |
