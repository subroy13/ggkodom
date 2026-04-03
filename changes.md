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



