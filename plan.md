# Bayesian Hierarchical Markov Model for Longitudinal Threshold Dynamics

###  Dataset & Notation

* As the covariates are measured in different timepoints, it is difficult to combined. One way is to assume that on short term:
    - $X_{ti} | X_{0i} \sim N(X_{0i}, \sigma_X^2 t_i)$
    - Assume $X_{0i} \sim N(\mu_0, \sigma_0^2)$
    - Instead of modelling through $X_{ti}$, we model through $X_{0i}$. Since $X_{0i}$ is not observed, we replace by its expectation.
    - $$E(X_{0i} \mid X_{ti}) = \mu_0 + \frac{\sigma_0^2}{\sigma_0^2 + t \sigma_X^2}(X_{ti} - \mu_0)$$
    - Here are the suggested values for different covariates by GPT:
        * LDL: $\mu_0 = 115, \sigma_0 = 30, \sigma_X = 0.5$.
        * HDL: $\mu_0 = 50, \sigma_0 = 12.5, \sigma_X = 0.5$.
        * Total Cholesterol: $\mu_0 = 200, \sigma_0 = 35, \sigma_X = 0.5$.
        * Weight: $\mu_0 = 80, \sigma_0 = 15, \sigma_X^2 = 0.1$.
        * Height: $\mu_0 = 170, \sigma_0 = 10, \sigma_X^2 = 10^{-8}$. 



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

