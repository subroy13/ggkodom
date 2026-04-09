############################
# Libraries
############################
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(torch)
library(fastDummies)
library(rlang)
library(purrr)

set.seed(1234)
torch_manual_seed(1234)

############################
# Helper functions
############################
safe_divide <- function(num, den, default = 0) {
  out <- ifelse(is.na(den) | den == 0, default, num / den)
  out[is.nan(out) | is.infinite(out)] <- default
  out
}

best_f1_threshold <- function(probs, y_true, idx = rep(TRUE, length(probs))) {
  probs <- probs[idx]
  y_true <- as.integer(y_true[idx])
  
  if (length(probs) == 0 || length(unique(y_true)) < 2) {
    return(tibble(
      best_th = 0.5,
      best_f1 = 0,
      precision = 0,
      recall = 0
    ))
  }
  
  th_grid <- sort(unique(c(seq(0.05, 0.95, by = 0.01), probs)))
  th_grid <- th_grid[th_grid >= 0 & th_grid <= 1]
  
  res <- lapply(th_grid, function(th) {
    pred <- as.integer(probs > th)
    tp <- sum(pred == 1 & y_true == 1)
    fp <- sum(pred == 1 & y_true == 0)
    fn <- sum(pred == 0 & y_true == 1)
    
    precision <- safe_divide(tp, tp + fp, 0)
    recall    <- safe_divide(tp, tp + fn, 0)
    f1        <- safe_divide(2 * precision * recall, precision + recall, 0)
    
    tibble(
      best_th = th,
      best_f1 = f1,
      precision = precision,
      recall = recall
    )
  }) %>% bind_rows()
  
  res %>% arrange(desc(best_f1), best_th) %>% slice(1)
}

compute_metrics <- function(pred, y_true) {
  pred   <- as.integer(pred)
  y_true <- as.integer(y_true)
  
  valid <- !(is.na(pred) | is.na(y_true))
  pred <- pred[valid]
  y_true <- y_true[valid]
  
  if (length(y_true) == 0) {
    out <- tibble(
      accuracy = NA_real_,
      precision = NA_real_,
      recall = NA_real_,
      specificity = NA_real_,
      f1 = NA_real_,
      tp = NA_integer_, tn = NA_integer_, fp = NA_integer_, fn = NA_integer_
    )
    print(out)
    return(invisible(out))
  }
  
  tp <- sum(pred == 1 & y_true == 1)
  tn <- sum(pred == 0 & y_true == 0)
  fp <- sum(pred == 1 & y_true == 0)
  fn <- sum(pred == 0 & y_true == 1)
  
  accuracy    <- safe_divide(tp + tn, length(y_true), 0)
  precision   <- safe_divide(tp, tp + fp, 0)
  recall      <- safe_divide(tp, tp + fn, 0)
  specificity <- safe_divide(tn, tn + fp, 0)
  f1          <- safe_divide(2 * precision * recall, precision + recall, 0)
  
  out <- tibble(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    specificity = specificity,
    f1 = f1,
    tp = tp, tn = tn, fp = fp, fn = fn
  )
  
  print(out)
  invisible(out)
}

fit_scaler <- function(df, cols) {
  tibble(
    feature = cols,
    mean = sapply(cols, function(x) mean(df[[x]], na.rm = TRUE)),
    sd   = sapply(cols, function(x) sd(df[[x]], na.rm = TRUE))
  ) %>%
    mutate(
      mean = ifelse(is.na(mean) | is.nan(mean) | is.infinite(mean), 0, mean),
      sd   = ifelse(is.na(sd) | is.nan(sd) | is.infinite(sd) | sd == 0, 1, sd)
    )
}

apply_scaler <- function(df, scaler_tbl) {
  for (j in seq_len(nrow(scaler_tbl))) {
    nm <- scaler_tbl$feature[j]
    mu <- scaler_tbl$mean[j]
    sg <- scaler_tbl$sd[j]
    if (nm %in% names(df)) {
      df[[nm]] <- (df[[nm]] - mu) / sg
      df[[nm]][!is.finite(df[[nm]])] <- 0
    }
  }
  df
}

to_binary_matrix <- function(x) {
  matrix(as.numeric(x), ncol = 1)
}

sanitize_df <- function(df) {
  for (nm in names(df)) {
    if (is.numeric(df[[nm]]) || is.integer(df[[nm]])) {
      df[[nm]][!is.finite(df[[nm]])] <- 0
      df[[nm]][is.na(df[[nm]])] <- 0
    }
  }
  df
}

torch_safe <- function(x, replace_val = 0) {
  bad_mask <- torch_isnan(x) | torch_isinf(x)
  torch_where(
    bad_mask,
    torch_full_like(x, replace_val),
    x
  )
}

############################
# Load data
############################
train_dat <- readRDS("./data/processed/train.Rds")
val_dat   <- readRDS("./data/processed/val.Rds")
test_dat  <- readRDS("./data/processed/test.Rds")

############################
# Optional visualization
############################
train_dat$measurements %>%
  filter(variable %in% c("a1c_1", "a1c_2025")) %>%
  select(id, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  ggplot() +
  geom_density(aes(x = a1c_1, fill = factor(a1c_2025)), alpha = 0.5) +
  geom_vline(xintercept = 8, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 6.75, color = "black", linetype = "dashed") +
  theme_bw()

############################
# Feature engineering
############################
create_state_transitions <- function(dat, scaler_tbl = NULL, fit_scaler_flag = FALSE) {
  
  measures <- dat$measurements
  
  target_rows <- measures$variable == "a1c_2025"
  measures$variable[target_rows] <- "a1c_6"
  measures$value[target_rows] <- measures$value[target_rows] * 10
  
  #####################################
  # A1c transitions
  #####################################
  a1c_measures <- measures %>%
    filter(variable %in% paste0("a1c_", 1:6)) %>%
    drop_na(time, value) %>%
    arrange(id, time, variable)
  
  transitions <- a1c_measures %>%
    group_by(id) %>%
    mutate(
      t_start = time,
      value_start = value,
      state_start = if_else(value >= 8.0, "H", "L"),
      t_end = lead(time),
      value_end = lead(value),
      state_end = lead(if_else(value >= 8.0, "H", "L")),
      end_var = lead(variable)
    ) %>%
    ungroup() %>%
    filter(!is.na(t_end), !is.na(state_end)) %>%
    mutate(
      delta_t = if_else(t_end > t_start + 0.01, t_end - t_start, 0.01)
    ) %>%
    select(id, t_start, t_end, delta_t, value_start, value_end,
           state_start, state_end, end_var) %>%
    group_by(id) %>%
    mutate(
      is_final = as.numeric(row_number() == n())
    ) %>%
    ungroup() %>%
    mutate(
      is_target = as.numeric(end_var == "a1c_6")
    )
  
  #####################################
  # Labs at transition time + recency
  #####################################
  lab_vars <- c("height", "weight", "ldl", "hdl", "chol")
  
  events <- transitions %>%
    select(id, t_start) %>%
    tidyr::crossing(variable = lab_vars) %>%
    left_join(
      measures %>%
        filter(variable %in% lab_vars) %>%
        drop_na(time, value),
      join_by(id, variable, closest(t_start >= time))
    ) %>%
    mutate(
      lab_age = if_else(!is.na(time), pmax(t_start - time, 0), NA_real_)
    ) %>%
    select(id, t_start, variable, value, lab_age) %>%
    pivot_wider(
      names_from = variable,
      values_from = c(value, lab_age),
      names_glue = "{.value}_{variable}"
    ) %>%
    rename_with(~ gsub("^value_", "measure_", .x)) %>%
    mutate(
      across(
        starts_with("measure_"),
        ~ as.numeric(is.na(.x) | is.nan(.x)),
        .names = "{.col}_is_missing"
      ),
      across(
        starts_with("lab_age_"),
        ~ as.numeric(is.na(.x) | is.nan(.x)),
        .names = "{.col}_is_missing"
      ),
      across(
        starts_with("measure_"),
        ~ ifelse(is.na(.x) | is.nan(.x) | is.infinite(.x), 0, .x)
      ),
      across(
        starts_with("lab_age_"),
        ~ ifelse(is.na(.x) | is.nan(.x) | is.infinite(.x), 999, .x)
      )
    )
  
  #####################################
  # Demographics + counts + age groups
  #####################################
  patient_vars <- dat$basic %>%
    mutate(
      age_group = case_when(
        is.na(age) ~ "Missing",
        age >= 0  & age < 20 ~ "0-20",
        age >= 20 & age < 40 ~ "20-40",
        age >= 40 & age < 60 ~ "40-60",
        age >= 60 & age < 80 ~ "60-80",
        age >= 80 ~ "80+",
        TRUE ~ "Missing"
      ),
      across(c(gender, ethnicity, race), ~ coalesce(.x, "Missing")),
      gender = factor(gender, levels = c("F", "M", "Missing")),
      ethnicity = factor(ethnicity, levels = c("HL", "Missing", "NHL")),
      race = factor(race, levels = c("Asian", "Black", "Missing", "Native", "Other", "White"))
    ) %>%
    dummy_cols(
      select_columns = c("gender", "ethnicity", "race"),
      remove_selected_columns = TRUE,
      remove_first_dummy = FALSE
    ) %>%
    mutate(
      adi_state = if_else(is.na(adi_state), 5, adi_state),
      adi_nation = if_else(is.na(adi_nation), 50, adi_nation)
    ) %>%
    select(-dob) %>%
    left_join(
      dat$counts %>%
        mutate(
          across(
            .cols = all_of(setdiff(names(dat$counts), "id")),
            .fns = ~ log1p(.x)
          )
        ),
      by = "id"
    )
  
  #####################################
  # A1c temporal features
  #####################################
  patient_a1c_features <- measures %>%
    filter(variable %in% paste0("a1c_", 1:5)) %>%
    drop_na(time, value) %>%
    arrange(id, time, variable) %>%
    group_by(id) %>%
    summarise(
      n_a1c = n(),
      a1c_first = first(value),
      a1c_latest = last(value),
      a1c_prev = ifelse(n() >= 2, dplyr::nth(value, n() - 1), NA_real_),
      a1c_min = min(value, na.rm = TRUE),
      a1c_max = max(value, na.rm = TRUE),
      a1c_range = a1c_max - a1c_min,
      a1c_sd = ifelse(n() >= 2, sd(value, na.rm = TRUE), 0),
      frac_above_8 = mean(value >= 8, na.rm = TRUE),
      a1c_ewma = {
        vals <- value
        k <- length(vals)
        w <- 0.5^rev(seq_len(k) - 1)
        sum(vals * w) / sum(w)
      },
      a1c_delta_last = ifelse(n() >= 2, last(value) - dplyr::nth(value, n() - 1), 0),
      a1c_delta_first_last = last(value) - first(value),
      a1c_slope_last = {
        if (n() >= 2) {
          dt <- last(time) - dplyr::nth(time, n() - 1)
          ifelse(is.na(dt) | dt <= 0, 0, (last(value) - dplyr::nth(value, n() - 1)) / dt)
        } else {
          0
        }
      },
      a1c_slope_global = {
        dt <- last(time) - first(time)
        ifelse(is.na(dt) | dt <= 0, 0, (last(value) - first(value)) / dt)
      },
      a1c_time_span = ifelse(n() >= 2, max(time) - min(time), 0),
      last_a1c_high = as.numeric(last(value) >= 8),
      ever_high_a1c = as.numeric(any(value >= 8, na.rm = TRUE)),
      n_a1c_crossings_8 = {
        vals <- value
        if (length(vals) >= 2) {
          sum((vals[-1] >= 8) != (vals[-length(vals)] >= 8))
        } else {
          0
        }
      },
      .groups = "drop"
    )
  
  patient_vars <- patient_vars %>%
    left_join(patient_a1c_features, by = "id")
  
  #####################################
  # Join all features
  #####################################
  out <- transitions %>%
    left_join(events, by = c("id", "t_start")) %>%
    left_join(patient_vars, by = "id") %>%
    mutate(
      state_start_num = ifelse(state_start == "H", 1, 0),
      state_end_num = ifelse(state_end == "H", 1, 0),
      
      bmi = if_else(
        measure_weight > 0 & measure_height > 0,
        measure_weight / (((measure_height + 0.01) / 100)^2),
        0
      ),
      
      non_hdl = measure_chol - measure_hdl,
      ldl_hdl_ratio = ifelse(
        measure_hdl > 0 & measure_ldl > 0,
        measure_ldl / measure_hdl,
        0
      ),
      
      months_elapsed = (delta_t + 1) / 30.42,
      ed_visit_rate = safe_divide(ed_visit, months_elapsed, 0),
      admission_rate = safe_divide(admission, months_elapsed, 0),
      pcp_visit_rate = safe_divide(pcp_visit, months_elapsed, 0),
      
      log_delta_t = log1p(delta_t),
      
      medication_burden = glp1 + insulin + metformin + sglt2 + sulfonylurea + dpp4,
      on_insulin = ifelse(insulin > 0, 1, 0),
      on_metformin = ifelse(metformin > 0, 1, 0),
      on_sglt2 = ifelse(sglt2 > 0, 1, 0),
      on_sulfonylurea = ifelse(sulfonylurea > 0, 1, 0),
      on_dpp4 = ifelse(dpp4 > 0, 1, 0),
      on_glp1 = ifelse(glp1 > 0, 1, 0),
      
      adi_discrepancy = adi_nation - adi_state,
      
      recent_ldl = as.numeric(lab_age_ldl <= 180),
      recent_hdl = as.numeric(lab_age_hdl <= 180),
      recent_chol = as.numeric(lab_age_chol <= 180),
      recent_weight = as.numeric(lab_age_weight <= 180),
      recent_height = as.numeric(lab_age_height <= 365)
    ) %>%
    select(-state_start, -state_end, -end_var, -months_elapsed)
  
  #####################################
  # Continuous features to scale
  # age removed from model
  #####################################
  continuous_features <- c(
    "delta_t", "log_delta_t",
    "value_start",
    "a1c_first", "a1c_latest", "a1c_prev", "a1c_min", "a1c_max", "a1c_range",
    "a1c_ewma", "a1c_sd", "frac_above_8",
    "a1c_delta_last", "a1c_delta_first_last", "a1c_slope_last", "a1c_slope_global",
    "a1c_time_span", "n_a1c", "n_a1c_crossings_8",
    "measure_height", "measure_weight", "bmi",
    "measure_ldl", "measure_hdl", "measure_chol", "non_hdl", "ldl_hdl_ratio",
    "lab_age_height", "lab_age_weight", "lab_age_ldl", "lab_age_hdl", "lab_age_chol",
    "adi_state", "adi_nation", "adi_discrepancy",
    "ed_visit", "pcp_visit", "admission",
    "ed_visit_rate", "pcp_visit_rate", "admission_rate",
    "medication_burden"
  )
  
  continuous_features <- intersect(continuous_features, names(out))
  
  out <- sanitize_df(out)
  
  if (fit_scaler_flag) {
    scaler_tbl <- fit_scaler(out, continuous_features)
  }
  
  out <- apply_scaler(out, scaler_tbl)
  out <- sanitize_df(out)
  
  list(
    data = out,
    scaler = scaler_tbl,
    continuous_features = continuous_features
  )
}

############################
# Build datasets
############################
train_obj <- create_state_transitions(train_dat, scaler_tbl = NULL, fit_scaler_flag = TRUE)
train_states <- train_obj$data
scaler_tbl <- train_obj$scaler

val_obj <- create_state_transitions(val_dat, scaler_tbl = scaler_tbl, fit_scaler_flag = FALSE)
val_states <- val_obj$data

test_obj <- create_state_transitions(test_dat, scaler_tbl = scaler_tbl, fit_scaler_flag = FALSE)
test_states <- test_obj$data

train_states$row_idx <- seq_len(nrow(train_states))
val_states$row_idx   <- seq_len(nrow(val_states))
test_states$row_idx  <- seq_len(nrow(test_states))

train_ids <- train_states$id
train_is_final <- train_states$is_final
train_is_target <- train_states$is_target

val_ids <- val_states$id
val_is_final <- val_states$is_final
val_is_target <- val_states$is_target

test_ids <- test_states$id
test_is_final <- test_states$is_final
test_is_target <- test_states$is_target

cat("Train transition table:\n")
print(table(train_states$state_start_num, train_states$state_end_num))
cat("Final transitions (train):", sum(train_is_final), "of", length(train_is_final), "\n")
cat("Target transitions (train):", sum(train_is_target), "of", length(train_is_target), "\n")

############################
# Feature columns
# age and age_group removed from model
############################
feature_cols <- setdiff(
  names(train_states),
  c(
    "id",
    "row_idx",
    "state_start_num",
    "state_end_num",
    "is_final",
    "is_target",
    "value_end",
    "t_end",
    "age",
    "age_group"
  )
)

############################
# Model definition
############################
transition_two_head_mlp <- nn_module(
  "TransitionTwoHeadMLP",
  initialize = function(input_dim, hidden_dim = 64, dropout = 0.20) {
    self$shared <- nn_sequential(
      nn_linear(input_dim, hidden_dim),
      nn_relu(),
      nn_dropout(p = dropout),
      nn_linear(hidden_dim, hidden_dim),
      nn_relu(),
      nn_dropout(p = dropout)
    )
    
    self$head_from_low <- nn_linear(hidden_dim, 1)
    self$head_from_high <- nn_linear(hidden_dim, 1)
  },
  
  forward = function(x, start_state) {
    z <- self$shared(x)
    logit_low <- self$head_from_low(z)
    logit_high <- self$head_from_high(z)
    (1 - start_state) * logit_low + start_state * logit_high
  }
)

############################
# Train one model per age group
############################
age_groups <- c("0-20", "20-40", "40-60", "60-80", "80+")

train_probs_all <- rep(NA_real_, nrow(train_states))
val_probs_all   <- rep(NA_real_, nrow(val_states))
test_probs_all  <- rep(NA_real_, nrow(test_states))

group_models <- list()
group_thresholds <- list()

for (grp in age_groups) {
  cat("\n=====================================\n")
  cat("Training age group:", grp, "\n")
  cat("=====================================\n")
  
  train_g <- train_states %>% filter(age_group == grp)
  val_g   <- val_states %>% filter(age_group == grp)
  test_g  <- test_states %>% filter(age_group == grp)
  
  cat("Train rows:", nrow(train_g), "\n")
  cat("Val rows:", nrow(val_g), "\n")
  cat("Test rows:", nrow(test_g), "\n")
  
  if (nrow(train_g) == 0) {
    cat("Skipping group because no training rows.\n")
    next
  }
  
  X_train <- as.matrix(train_g[, feature_cols, drop = FALSE])
  X_val   <- as.matrix(val_g[, feature_cols, drop = FALSE])
  X_test  <- as.matrix(test_g[, feature_cols, drop = FALSE])
  
  X_train[!is.finite(X_train)] <- 0
  X_val[!is.finite(X_val)] <- 0
  X_test[!is.finite(X_test)] <- 0
  
  y_train_mat <- to_binary_matrix(train_g$state_end_num)
  y_train_mat[!is.finite(y_train_mat)] <- 0
  
  start_train_mat <- to_binary_matrix(train_g$state_start_num)
  start_train_mat[!is.finite(start_train_mat)] <- 0
  
  start_val_mat <- to_binary_matrix(val_g$state_start_num)
  start_val_mat[!is.finite(start_val_mat)] <- 0
  
  start_test_mat <- to_binary_matrix(test_g$state_start_num)
  start_test_mat[!is.finite(start_test_mat)] <- 0
  
  x_tensor <- torch_tensor(X_train, dtype = torch_float())
  val_x <- torch_tensor(X_val, dtype = torch_float())
  test_x <- torch_tensor(X_test, dtype = torch_float())
  
  y_tensor <- torch_tensor(y_train_mat, dtype = torch_float())
  start_state_tensor <- torch_tensor(start_train_mat, dtype = torch_float())
  val_start_state <- torch_tensor(start_val_mat, dtype = torch_float())
  test_start_state <- torch_tensor(start_test_mat, dtype = torch_float())
  
  model <- transition_two_head_mlp(
    input_dim = ncol(x_tensor),
    hidden_dim = 64,
    dropout = 0.20
  )
  
  cat("Input dim:", ncol(x_tensor), "\n")
  cat(
    "Trainable params:",
    sum(sapply(model$parameters, function(p) if (p$requires_grad) p$numel() else 0)),
    "\n"
  )
  
  ############################
  # Head-specific class weights
  ############################
  idx_low <- which(train_g$state_start_num == 0)
  idx_high <- which(train_g$state_start_num == 1)
  
  y_low <- train_g$state_end_num[idx_low]
  y_high <- train_g$state_end_num[idx_high]
  
  n_low_pos <- sum(y_low == 1)
  n_low_neg <- sum(y_low == 0)
  n_high_pos <- sum(y_high == 1)
  n_high_neg <- sum(y_high == 0)
  
  weight_low_pos <- ifelse(n_low_pos > 0, n_low_neg / n_low_pos, 1)
  weight_high_pos <- ifelse(n_high_pos > 0, n_high_neg / n_high_pos, 1)
  
  weight_low_pos <- min(weight_low_pos, 20)
  weight_high_pos <- min(weight_high_pos, 20)
  
  weight_low_pos <- ifelse(is.finite(weight_low_pos), weight_low_pos, 1)
  weight_high_pos <- ifelse(is.finite(weight_high_pos), weight_high_pos, 1)
  
  cat("weight_low_pos  =", weight_low_pos, "\n")
  cat("weight_high_pos =", weight_high_pos, "\n")
  
  loss_fn_low <- nn_bce_with_logits_loss(
    pos_weight = torch_tensor(weight_low_pos, dtype = torch_float()),
    reduction = "none"
  )
  
  loss_fn_high <- nn_bce_with_logits_loss(
    pos_weight = torch_tensor(weight_high_pos, dtype = torch_float()),
    reduction = "none"
  )
  
  ############################
  # Masks and row weights
  ############################
  low_mask <- torch_tensor(
    matrix(as.numeric(train_g$state_start_num == 0), ncol = 1),
    dtype = torch_float()
  )
  
  high_mask <- torch_tensor(
    matrix(as.numeric(train_g$state_start_num == 1), ncol = 1),
    dtype = torch_float()
  )
  
  target_upweight <- 5.0
  row_weight <- ifelse(train_g$is_target == 1, target_upweight, 1.0)
  row_weight[!is.finite(row_weight)] <- 1
  row_weight_tensor <- torch_tensor(matrix(row_weight, ncol = 1), dtype = torch_float())
  
  ############################
  # Diagnostics
  ############################
  cat("Any NA in train group? ", any(is.na(train_g)), "\n")
  cat("Any NaN in X_train? ", any(is.nan(X_train)), "\n")
  cat("Any Inf in X_train? ", any(is.infinite(X_train)), "\n")
  
  bad_cols <- sapply(train_g[, feature_cols, drop = FALSE], function(z) {
    any(is.na(z) | is.nan(z) | is.infinite(z))
  })
  if (any(bad_cols)) {
    cat("Columns with invalid values:\n")
    print(names(bad_cols)[bad_cols])
  }
  
  ############################
  # Optimizer
  ############################
  optimizer <- optim_adam(model$parameters, lr = 0.001, weight_decay = 1e-4)
  
  ############################
  # Training loop
  ############################
  epochs <- 200
  loss_curve <- numeric(epochs)
  
  for (epoch in 1:epochs) {
    model$train()
    
    shared_z <- model$shared(x_tensor)
    logit_low_all <- model$head_from_low(shared_z)
    logit_high_all <- model$head_from_high(shared_z)
    
    raw_loss_low <- loss_fn_low(logit_low_all, y_tensor)
    raw_loss_high <- loss_fn_high(logit_high_all, y_tensor)
    
    raw_loss_low <- torch_safe(raw_loss_low, replace_val = 0)
    raw_loss_high <- torch_safe(raw_loss_high, replace_val = 0)
    
    low_weighted <- raw_loss_low * low_mask * row_weight_tensor
    high_weighted <- raw_loss_high * high_mask * row_weight_tensor
    
    low_denom <- (low_mask * row_weight_tensor)$sum()
    high_denom <- (high_mask * row_weight_tensor)$sum()
    
    low_denom <- torch_clamp(low_denom, min = 1)
    high_denom <- torch_clamp(high_denom, min = 1)
    
    loss_low <- low_weighted$sum() / low_denom
    loss_high <- high_weighted$sum() / high_denom
    
    loss <- 0.5 * loss_low + 0.5 * loss_high
    loss <- torch_safe(loss, replace_val = 0)
    
    optimizer$zero_grad()
    loss$backward()
    nn_utils_clip_grad_norm_(model$parameters, max_norm = 5)
    optimizer$step()
    
    loss_curve[epoch] <- as.numeric(loss$item())
    
    if (epoch %% 10 == 0) {
      cat(sprintf(
        "[%s] Epoch %3d | Total %.6f | Low %.6f | High %.6f\n",
        grp,
        epoch,
        as.numeric(loss$item()),
        as.numeric(loss_low$item()),
        as.numeric(loss_high$item())
      ))
    }
  }
  
  plot(loss_curve, type = "l",
       main = paste("Training loss - age group", grp),
       ylab = "Loss", xlab = "Epoch")
  
  ############################
  # Train inference
  ############################
  model$eval()
  with_no_grad({
    train_probs <- torch_sigmoid(model(x_tensor, start_state_tensor))
  })
  train_probs_num <- as.numeric(as_array(train_probs))
  
  cat("\n----- Train: state-specific thresholds -----\n")
  idx0 <- train_g$state_start_num == 0
  thresh0 <- best_f1_threshold(train_probs_num, train_g$state_end_num == 1, idx0)
  cat("LOW head threshold:\n")
  print(thresh0)
  
  idx1 <- train_g$state_start_num == 1
  thresh1 <- best_f1_threshold(train_probs_num, train_g$state_end_num == 1, idx1)
  cat("HIGH head threshold:\n")
  print(thresh1)
  
  train_thresholds <- ifelse(
    train_g$state_start_num == 0,
    thresh0$best_th,
    thresh1$best_th
  )
  
  cat("\n----- Train: transition-level metrics -----\n")
  compute_metrics(
    pred = train_probs_num > train_thresholds,
    y_true = train_g$state_end_num == 1
  )
  
  ############################
  # Validation inference
  ############################
  model$eval()
  with_no_grad({
    val_probs <- torch_sigmoid(model(val_x, val_start_state))
  })
  val_probs_num <- as.numeric(as_array(val_probs))
  
  val_thresholds <- ifelse(
    val_g$state_start_num == 0,
    thresh0$best_th,
    thresh1$best_th
  )
  
  cat("\n----- Val: transition-level metrics -----\n")
  compute_metrics(
    pred = val_probs_num > val_thresholds,
    y_true = val_g$state_end_num == 1
  )
  
  ############################
  # Patient-level validation metric
  ############################
  val_patient_level <- tibble(
    id = val_g$id,
    prob = val_probs_num,
    true_label = val_g$state_end_num == 1,
    start_state = val_g$state_start_num,
    is_target = val_g$is_target
  ) %>%
    filter(is_target == 1)
  
  cat("\n=== PATIENT-LEVEL Val Metrics ===\n")
  cat("Returned val patients:", nrow(val_patient_level), "\n")
  cat("Base rate:", mean(val_patient_level$true_label), "\n\n")
  
  cat("--- Using state-specific thresholds ---\n")
  val_patient_th <- ifelse(
    val_patient_level$start_state == 0,
    thresh0$best_th,
    thresh1$best_th
  )
  
  compute_metrics(
    pred = val_patient_level$prob > val_patient_th,
    y_true = val_patient_level$true_label
  )
  
  cat("\n--- Best single threshold tuned on val target transitions ---\n")
  patient_thresh <- best_f1_threshold(
    probs = val_patient_level$prob,
    y_true = val_patient_level$true_label
  )
  print(patient_thresh)
  
  compute_metrics(
    pred = val_patient_level$prob > patient_thresh$best_th,
    y_true = val_patient_level$true_label
  )
  
  ############################
  # Test inference
  ############################
  model$eval()
  with_no_grad({
    test_probs <- torch_sigmoid(model(test_x, test_start_state))
  })
  test_probs_num <- as.numeric(as_array(test_probs))
  
  test_thresholds <- ifelse(
    test_g$state_start_num == 0,
    thresh0$best_th,
    thresh1$best_th
  )
  
  cat("\n----- Test: transition-level metrics -----\n")
  compute_metrics(
    pred = test_probs_num > test_thresholds,
    y_true = test_g$state_end_num == 1
  )
  
  ############################
  # Store predictions in full vectors
  ############################
  train_probs_all[train_g$row_idx] <- train_probs_num
  val_probs_all[val_g$row_idx]     <- val_probs_num
  test_probs_all[test_g$row_idx]   <- test_probs_num
  
  group_models[[grp]] <- model
  group_thresholds[[grp]] <- list(
    thresh0 = thresh0,
    thresh1 = thresh1,
    patient_thresh = patient_thresh
  )
}

############################
# Overall combined metrics after age-wise training
############################
cat("\n=====================================\n")
cat("Combined metrics across all age groups\n")
cat("=====================================\n")

cat("\n----- Combined Validation: transition-level metrics -----\n")
combined_val_thresholds <- rep(NA_real_, nrow(val_states))
for (grp in names(group_thresholds)) {
  idx_grp <- which(val_states$age_group == grp)
  if (length(idx_grp) > 0) {
    combined_val_thresholds[idx_grp] <- ifelse(
      val_states$state_start_num[idx_grp] == 0,
      group_thresholds[[grp]]$thresh0$best_th,
      group_thresholds[[grp]]$thresh1$best_th
    )
  }
}
compute_metrics(
  pred = val_probs_all > combined_val_thresholds,
  y_true = val_states$state_end_num == 1
)

cat("\n----- Combined Patient-level Validation metrics -----\n")
val_patient_level_all <- tibble(
  id = val_ids,
  prob = val_probs_all,
  true_label = val_states$state_end_num == 1,
  start_state = val_states$state_start_num,
  is_target = val_is_target,
  age_group = val_states$age_group
) %>%
  filter(is_target == 1)

val_patient_th_all <- rep(NA_real_, nrow(val_patient_level_all))
for (grp in names(group_thresholds)) {
  idx_grp <- which(val_patient_level_all$age_group == grp)
  if (length(idx_grp) > 0) {
    val_patient_th_all[idx_grp] <- group_thresholds[[grp]]$patient_thresh$best_th
  }
}
compute_metrics(
  pred = val_patient_level_all$prob > val_patient_th_all,
  y_true = val_patient_level_all$true_label
)


cat("\n----- Combined Transition-level Validation metrics -----\n")

val_transition_level_all <- tibble(
  id = val_ids,
  prob = val_probs_all,
  true_label = val_states$state_end_num == 1,
  start_state = val_states$state_start_num,
  age_group = val_states$age_group
)

val_transition_th_all <- rep(NA_real_, nrow(val_transition_level_all))

for (grp in names(group_thresholds)) {
  idx_grp <- which(val_transition_level_all$age_group == grp)
  if (length(idx_grp) > 0) {
    val_transition_th_all[idx_grp] <- ifelse(
      val_transition_level_all$start_state[idx_grp] == 0,
      group_thresholds[[grp]]$thresh0$best_th,
      group_thresholds[[grp]]$thresh1$best_th
    )
  }
}

compute_metrics(
  pred = val_transition_level_all$prob > val_transition_th_all,
  y_true = val_transition_level_all$true_label
)

# cat("\n----- Combined Test: transition-level metrics -----\n")
# combined_test_thresholds <- rep(NA_real_, nrow(test_states))
# for (grp in names(group_thresholds)) {
#   idx_grp <- which(test_states$age_group == grp)
#   if (length(idx_grp) > 0) {
#     combined_test_thresholds[idx_grp] <- ifelse(
#       test_states$state_start_num[idx_grp] == 0,
#       group_thresholds[[grp]]$thresh0$best_th,
#       group_thresholds[[grp]]$thresh1$best_th
#     )
#   }
# }
# compute_metrics(
#   pred = test_probs_all > combined_test_thresholds,
#   y_true = test_states$state_end_num == 1
# )

############################
# Save probabilities
############################
tibble(
  id = c(train_ids, val_ids, test_ids),
  split = c(
    rep("train", length(train_ids)),
    rep("val", length(val_ids)),
    rep("test", length(test_ids))
  ),
  age_group = c(train_states$age_group, val_states$age_group, test_states$age_group),
  NN = c(train_probs_all, val_probs_all, test_probs_all)
) %>%
  write_csv("./data/processed/nn_probs_agewise.csv")

cat("\nSaved probabilities to ./data/processed/nn_probs_agewise.csv\n")