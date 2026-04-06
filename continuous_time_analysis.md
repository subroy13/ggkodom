# Continuous-Time Analysis of A1c Dynamics

**Date:** April 6, 2026  
**Scripts:** `scripts/subrata_continuous_time.R`, `scripts/subrata_multiscale_acf.R`, `scripts/subrata_lmer_exploration.R`

---

## 1. Variance Decomposition: ICC = 0.924

The single most important number in this project. Decompose total A1c variance:

$$\text{Var}(Y_{ij}) = \sigma^2_{\text{between}} + \sigma^2_{\text{within}}$$

| Component | Variance | % of total |
|---|---|---|
| Between-patient $\sigma^2_B$ | 3.029 | **92.4%** |
| Within-patient $\sigma^2_W$ (pooled) | 1.342 | 7.6% |
| Total | 3.279 | 100% |

$$\text{ICC} = \frac{\sigma^2_B}{\sigma^2_B + \sigma^2_W} = \frac{3.029}{3.279} = 0.924$$

**Interpretation:** Patient identity explains 92.4% of A1c variation. Once you know a patient's "typical A1c level," within-patient trajectory adds at most 7.6% of information. This is why `a1c_weighted` alone gives 61.4% F1 and the entire 74-feature XGBoost adds only ~0.8% more.

**Ceiling bound:** If within-patient dynamics are 7.6% of variance, trajectory-based approaches can improve F1 by at most ~1–2% beyond `a1c_weighted`. Observed: 61.4% (EWMA alone) → 63% (best RF) = 1.6% improvement. Consistent with the bound.

---

## 2. Within-Patient Autocorrelation: Negative at All Lags

After demeaning within each patient ($Y_{ij}^c = Y_{ij} - \bar{Y}_i$), the within-patient autocorrelation is **negative** at all observed lags:

| $\Delta t$ (years) | $\rho_{\text{within}}(\Delta t)$ |
|---|---|
| 0.01 | −0.10 |
| 0.06 | −0.18 |
| 0.14 | −0.34 |
| 0.19 | −0.40 |
| 0.26 | −0.44 |
| 0.34 | −0.51 |
| 0.46 | −0.51 |
| 0.51 | −0.66 |
| 0.69 | −0.50 |
| 0.87 | −0.49 |

**This is treatment-induced oscillation.** Within a patient, readings tend to fall on opposite sides of the patient mean:

$$Y_{i,j+1} - \bar{Y}_i \approx -\rho \cdot (Y_{ij} - \bar{Y}_i) + \epsilon$$

- High reading (above patient mean) → treatment → next reading drops **below** patient mean
- Low reading (below patient mean) → no treatment → drift **above** patient mean
- The process overshoots the mean in both directions

**Standard AR(1), OU, GP (Matérn/SE kernel) all assume $\rho(\Delta t) > 0$ and decaying.** They are fundamentally wrong for within-patient A1c dynamics.

### Why raw correlation is positive (Simpson's paradox)

The **raw** (unconditional) autocorrelation is positive:

$$\rho_{\text{raw}}(\Delta t) \approx 0.72 \text{ (short lag)} \to 0.46 \text{ (long lag)}$$

This decomposes as:

$$\rho_{\text{raw}} = \frac{\sigma^2_B \cdot 1 + \sigma^2_W \cdot \rho_{\text{within}}}{\sigma^2_B + \sigma^2_W}$$

Since $\sigma^2_B \gg \sigma^2_W$ and $\rho_{\text{within}} < 0$, the large positive between-patient correlation overwhelms the negative within-patient correlation. At $\Delta t = 0.5$ yr:

$$\rho_{\text{raw}} \approx \frac{3.03 \cdot 1 + 1.34 \cdot (-0.51)}{3.03 + 1.34} = \frac{3.03 - 0.68}{4.37} = 0.54$$

Close to the observed 0.56.

**Implication:** The GEE-estimated AR(1) = 0.69 is a raw correlation — it conflates between-patient persistence with within-patient oscillation. The lmer random intercept captures the between-patient component. The within-patient component is negative and thus cannot be modeled by any standard positive-ACF model.

---

## 3. The Non-Monotonic Variogram

The empirical within-patient variogram $\gamma(\Delta t) = \frac{1}{2} E[(Y_{ij}^c - Y_{ik}^c)^2 \mid |t_j - t_k| = \Delta t]$ is **non-monotonic**:

| $\Delta t$ (yr) | $\gamma(\Delta t)$ | Pattern |
|---|---|---|
| 0.01 | 1.36 | baseline |
| 0.07 | 1.67 | rising |
| 0.14 | **2.11** | **peak** |
| 0.16 | 2.05 | |
| 0.24 | 1.40 | dropping |
| 0.26 | 1.34 | |
| 0.34 | 1.35 | trough |
| 0.49 | 1.01 | |
| 0.51 | **0.95** | **minimum** |
| 0.64 | 2.10 | rising again |
| 0.87 | 1.65 | |

A standard OU/Matérn variogram rises monotonically to a sill. The non-monotonic pattern is a **"treatment effect ridge":**

1. **$\Delta t < 2$ months:** Treatment actively changing A1c → large differences → high $\gamma$
2. **$\Delta t \approx 3$–$6$ months:** Treatment stabilized → readings converge → $\gamma$ drops
3. **$\Delta t > 6$ months:** New perturbations (adherence decay, new conditions) → $\gamma$ rises again

The residuals from a multi-scale variogram fit show a **periodic pattern** with period $\approx 6$ months — possibly medication refill cycles or scheduled follow-up intervals.

### Multi-scale variogram decomposition

$$\gamma(\Delta t) = c_0 + c_1 (1 - e^{-\Delta t / \tau_1}) + c_2 (1 - e^{-\Delta t / \tau_2})$$

| Component | Variance | Timescale | % of within-patient |
|---|---|---|---|
| Nugget $c_0$ | 1.178 | instantaneous | **73%** |
| Short scale $c_1$ | 0.409 | $\tau_1 = 8$ days | 25% |
| Long scale $c_2$ | 0.028 | $\tau_2 > 5$ years | 2% |
| **Sill** | **1.615** | | 100% |

73% of within-patient variance is instantaneous noise (measurement error + rapid treatment response). Only 27% has temporal structure, and that structure operates on an 8-day timescale — too fast to matter for a prediction problem with months-to-years horizons.

---

## 4. Continuous-Time Markov Chain (CTMC)

Model the binary state $S(t) \in \{L, H\}$ where $H$ = A1c $\geq 8$ (uncontrolled):

$$Q = \begin{pmatrix} -q_{LH} & q_{LH} \\ q_{HL} & -q_{HL} \end{pmatrix}$$

The transition probability matrix over interval $\Delta t$:

$$P(\Delta t) = \exp(Q \cdot \Delta t)$$

### Estimated rates (from 28,163 observed transitions)

| Parameter | Value | Interpretation |
|---|---|---|
| $q_{LH}$ | 0.240 /yr | Annual deterioration rate |
| $q_{HL}$ | 1.254 /yr | Annual treatment success rate |
| $q_{HL} / q_{LH}$ | **5.2×** | Treatment works 5× faster than deterioration |
| $\pi_H = q_{LH}/\lambda$ | 0.160 | Stationary (long-run) fraction uncontrolled |
| $1/\lambda$ | 0.669 yr | **Relaxation time** (~8 months) |

where $\lambda = q_{LH} + q_{HL} = 1.494$ /yr is the total rate.

### Explicit transition probabilities

For a 2-state CTMC, the closed-form solution is:

$$P(H \text{ at } t + \Delta t \mid L \text{ at } t) = \pi_H \left(1 - e^{-\lambda \Delta t}\right)$$
$$P(H \text{ at } t + \Delta t \mid H \text{ at } t) = \pi_H + (1 - \pi_H)\, e^{-\lambda \Delta t}$$

| $\Delta t$ | $P(H \mid L)$ | $P(H \mid H)$ | Gap |
|---|---|---|---|
| 1 month | 2.2% | 88.4% | 86 pp |
| 3 months | 5.0% | 73.8% | 69 pp |
| 6 months | 8.4% | 55.8% | 47 pp |
| 1 year | 12.4% | 34.9% | 22 pp |
| 2 years | 15.2% | 20.3% | 5 pp |
| 5 years | 16.0% | 16.1% | ~0 |

**Key behavior:**
- As $\Delta t \to 0$: $P(\Delta t) \to I$ (persistence). Current state is perfectly informative.
- As $\Delta t \to \infty$: $P(\Delta t) \to \pi$ (stationary). Current state is worthless.
- At $\Delta t = 2 \times 1/\lambda \approx 1.3$ years: state carries only 5 pp of information beyond the base rate.

**This explains why `time_gap` is XGBoost's #2 feature (13–21% gain).** The tree is learning this exponential decay non-parametrically. The CTMC gives the exact functional form.

### CTMC as a feature

$$\text{ctmc\_prob}_i = P(H \text{ at } t_{2025} \mid S_i^{\text{last}}, \Delta t_i)$$

This is a single feature that optimally combines last state and time gap. In XGBoost, it captured 25% of total gain — becoming the #1 feature. But overall F1 didn't improve because the tree was already approximating this interaction through splits on `a1c_latest` × `time_gap`.

---

## 5. Connection to Gaussian Processes

The OU process is a GP with Matérn-1/2 kernel:

$$K(t, t') = \sigma_f^2 \exp\left(-\frac{|t - t'|}{\ell}\right)$$

where $\ell = \tau$ (lengthscale = memory timescale).

For a patient with readings $\{(t_1, y_1), \ldots, (t_n, y_n)\}$, the GP posterior at $t^*$ is:

$$\mu_* = \mu + \mathbf{k}_*^T (\mathbf{K} + \sigma_n^2 \mathbf{I})^{-1} (\mathbf{y} - \mu)$$
$$\sigma_*^2 = K(t_*, t_*) - \mathbf{k}_*^T (\mathbf{K} + \sigma_n^2 \mathbf{I})^{-1} \mathbf{k}_*$$

### GP with 1 reading (the majority case)

For a patient with one reading $(t_1, y_1)$, predicting at $t^*$:

$$\mu_* = \mu + \frac{\sigma_f^2 \, e^{-|t^* - t_1|/\ell}}{\sigma_f^2 + \sigma_n^2} \, (y_1 - \mu)$$

Three components:
1. **Shrinkage toward $\mu$:** factor $\sigma_f^2 / (\sigma_f^2 + \sigma_n^2)$ — same as lmer BLUP (= 0.591)
2. **Temporal decay:** factor $e^{-\Delta t / \ell}$ — prediction decays toward $\mu$ as time gap grows
3. **Individual signal:** $(y_1 - \mu)$

For $\Delta t \to 0$: reduces to lmer BLUP.  
For $\Delta t \to \infty$: reduces to population mean $\mu$.

**The GP unifies EWMA (optimal time-weighting), lmer BLUP (shrinkage), and CTMC (time-gap decay) into a single framework.**

### Why the GP failed empirically

With the single-scale OU kernel:
- $\tau = 0.010$ yr (4 days) from the variogram → kernel decays to zero for any $\Delta t > 2$ weeks → predictions collapse to $\mu$ for everyone → degenerate (F1 = 31.5%)

With the multi-scale kernel ($K = c_1 e^{-\Delta t/\tau_1} + c_2 e^{-\Delta t/\tau_2}$):
- $\tau_1 = 8$ days, $\tau_2 = 5$ years → predictions are dominated by the long-scale component ($\tau_2$) → reduces to a shrinkage estimator → F1 = 60.5%
- Better than single-scale but still below `a1c_weighted` (61.4%)

**The GP's kernel assumes positive autocorrelation.** The within-patient ACF is negative. The GP cannot capture treatment oscillation — it's structurally wrong for this data.

### A correct within-patient model (theoretical)

The treatment-oscillation dynamics would require a kernel with negative lobes, such as a damped cosine:

$$K(t, t') = \sigma^2 e^{-|t-t'|/\tau} \cos\left(\frac{2\pi |t-t'|}{T}\right)$$

where $T \approx 6$ months is the oscillation period. This is a spectral mixture kernel. However, since within-patient dynamics explain only 7.6% of variance, this exotic kernel would improve prediction by at most a fraction of a percent.

---

## 6. Time-Weighted EWMA

Replace per-reading exponential decay with per-time decay:

$$\text{tw\_ewma}_i = \frac{\sum_{k} \exp\left(-(t_{\text{latest}} - t_k)/\tau\right) \cdot y_k}{\sum_{k} \exp\left(-(t_{\text{latest}} - t_k)/\tau\right)}$$

| $\tau$ | Behavior | r with `a1c_weighted` | Val F1 |
|---|---|---|---|
| 4 days (from variogram) | Only latest reading matters | 0.966 | 60.4% |
| 5 years (long scale) | Nearly equal weight | 0.997 | 61.5% |
| ∞ | Unweighted mean | ~1.0 | ≈ 61.4% |
| 0 | Only latest reading | 1.0 with `a1c_latest` | 60.3% |

With $\tau = 5$ years, the time-weighted EWMA is essentially the same as the regular EWMA (all readings within our 1-year window get nearly equal time-decay weight). With $\tau = 4$ days, it degenerates to `a1c_latest`.

**Neither extreme beats `a1c_weighted`.** The EWMA's per-reading decay (λ = 0.5) is empirically better than any time-based decay. The per-reading weighting implicitly says "the most recent reading is most informative" without reference to calendar time — and that turns out to be right.

---

## 7. The Story for the Presentation

### Three layers of A1c variation

1. **"Who you are" (92.4%):** Patient-level trait. Chronic high vs chronic low. Captured entirely by `a1c_weighted`. This IS the prediction.

2. **"What happened to you" (5.5%):** Within-patient noise. Measurement error, day-to-day fluctuation, acute treatment effects. Instantaneous, unpredictable, irreducible.

3. **"Where you're going" (2%):** Within-patient trajectory. Treatment dynamics, adherence, lifestyle. Structured but too weak to improve prediction. Shows negative ACF (oscillation), not the positive decay that standard models assume.

### Why the ceiling exists

The prediction ceiling (~63% F1) is set by the ICC:

- `a1c_weighted` alone: **61.4%** F1 (captures the 92.4%)
- Best model (RF): **63%** F1 (captures some of the 7.6%)
- Theoretical maximum (if ALL within-patient dynamics were perfectly modeled): ~65% F1

The remaining error (37% of uncontrolled patients missed, 43% of alarms false) is driven by:
- **Future treatment response** (will THIS patient respond?) — unobserved
- **Future deterioration** (will THIS stable patient worsen?) — unobserved
- These require medication adherence data, lifestyle data, and treatment timing — none of which we have

### Why time-series models fail

Standard time-series approaches (AR, OU, GP, lmer, GEE, CTMC) all share an assumption: **recent observations are positively correlated with future observations.** For A1c:

- **Between patients:** TRUE. A patient at 9 today is likely still high next year. But this is just patient identity — `a1c_weighted` captures it.
- **Within patients:** FALSE. A reading above patient mean predicts the NEXT reading will be below patient mean (negative ACF). Treatment creates oscillation, not persistence.

The models capture the between-patient signal (trivially, through a fixed/random intercept) but are wrong for within-patient dynamics. Since within-patient dynamics are only 7.6% of variance, being wrong about them barely matters — but it does explain why no trajectory-based approach meaningfully improves over a simple average.

### The CTMC relaxation time

The CTMC gives a concrete answer to "how long does a reading stay informative?"

- Relaxation time $1/\lambda = 8$ months
- After ~2 years, current state carries only 5 pp of information beyond base rate
- Most competition patients have `time_gap` > 8 months → prediction is approaching base rate

This is why `time_gap` is a top XGBoost feature — it quantifies how much the reading has "decayed."
