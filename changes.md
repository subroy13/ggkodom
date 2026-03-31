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
    - Source file: `./scripts/cleaning.R`


