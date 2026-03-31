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


> TODO: @Subrata_Pal to add xgboost and claude code scripts.


## March 31, 2026

- Created a data cleaning pipeline based on all EDA and sorting mechanism so far. Split into 50-25-25 train/val/test partitions.
    - Converted the non-identifiable factor levels to NA (e.g., unknown gender, race, etc.)
    - Source file: `./scripts/cleaning.R`

- Here are some related ideas:
    - Consider a mathematical model as follows:
    * There are $i = 1, 2, \dots, n$ patients. Patient $i$ has $j = 1, 2, \dots, n_i$ observations for `a1c`.
    * The `a1c` observations are denoted by $Y$ (treat them as response). The observed data is given by: $(t_{ij}, Y_{ij})$, where $t_{ij}$ is the time of measurement for `a1c` and $Y_{ij}$ is the value of the measurement. Additionally, we observe $Z_i$, the time-independent covariates (e.g., demographic information) and $(t'_i, X_{t'_i})$ which is some measured covariate at time $t'_i$, may be different from $t_{ij}$s.
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
