# Bayesian Hierarchical Markov Model for Longitudinal Threshold Dynamics

###  Dataset & Notation
Let $i \in \{1, \dots, N\}$ index the individuals and $j \in \{1, \dots, n_i\}$ index the sequential measurements for the $i$-th individual.

* **Time:** $t_{ij}$ is the time of the $j$-th measurement for patient $i$, with interval $\Delta t_{ij} = t_{ij} - t_{i(j-1)}$.
* **Observations:** $Y_{ij}$ is the observed biomarker (e.g., A1C) at time $t_{ij}$.
* **Covariates:** $\mathbf{x}_i \in \mathbb{R}^K$ is the standardized patient-level feature vector (derived from demographic features $Z_i$ and the expected baseline of time-varying features $\mathbb{E}[X_{0i} | X_{t'_i}]$).
* **Threshold:** $A^*$ is the known clinical intervention threshold.

The dataset contains two distinct types of observations ($j$):
1.  **Fully Observed:** Standard longitudinal transitions ($j < n_i$).
2.  **Interval Censored (Train):** For the terminal node of training patients, $Y_{in_i}$ is unobserved, but the binary indicator $\mathbb{I}(Y_{in_i} \ge A^*)$ is known.


### Possible Model Formulation


Patient-specific volatility ($\sigma_i^2$) and medication efficacy ($\delta_i$) are strictly positive, modeled via a log-link to the design matrix:
$$\log(\delta_i) = \alpha_\delta + \mathbf{x}_i^\top \boldsymbol{\theta}_\delta$$
$$\log(\sigma_i^2) = \alpha_\sigma + \mathbf{x}_i^\top \boldsymbol{\theta}_\sigma$$


The true biological state $\mu_{ij}$ evolves as a continuous-time random walk. The drift depends deterministically on whether the **previously observed** test result exceeded the clinical threshold $A^*$.

**Initial State:**
$$\mu_{i1} \sim \mathcal{N}(Y_{i1}, \sigma^2_{\text{init}})$$

**Transition Dynamics ($j > 1$):**
Let $D_{ij}$ be the drift applied over the interval $\Delta t_{ij}$:
$$ D_{ij} = \begin{cases} 
0 & \text{if } Y_{i(j-1)} < A^* \quad \text{(Pre-medication)} \\
-\delta_i & \text{if } Y_{i(j-1)} \ge A^* \quad \text{(Post-medication)} 
\end{cases} $$

The latent state updates as:
$$\mu_{ij} | \mu_{i(j-1)}, Y_{i(j-1)} \sim \mathcal{N}(\mu_{i(j-1)} + D_{ij} \Delta t_{ij}, \; \sigma_i^2 \Delta t_{ij})$$

The observed data $Y_{ij}$ is a noisy realization of the latent state, governed by global measurement error $\tau^2$. The likelihood contribution of $Y_{ij}$ depends on its data topology:

* **Case 1: Fully Observed ($j < n_i$ or Initial)**
    $$Y_{ij} \sim \mathcal{N}(\mu_{ij}, \tau^2)$$

* **Case 2: Interval Censored (Train Terminal Node)**
    We integrate out the missing $Y_{in_i}$ by evaluating the Cumulative Distribution Function ($\Phi$) of the normal distribution:
    $$ \mathcal{L}(\mu_{in_i}, \tau) = \begin{cases} 
    1 - \Phi\left(\frac{A^* - \mu_{in_i}}{\tau}\right) & \text{if } Y_{in_i} \ge A^* \\
    \Phi\left(\frac{A^* - \mu_{in_i}}{\tau}\right) & \text{if } Y_{in_i} < A^* \end{cases} $$

