# =========================
# Contains useful utility functions
# =======================


# split train test validation sets from an id column
split_train_test_val <- function(dat_list, id_column = "id", split_percentage = c(0.5, 0.25, 0.25), seed = 1) {
  set.seed(seed)

  id_vals <- unique(dat_list[[1]][[id_column]])
  n <- length(id_vals)

  # split based on ids
  train_id_list <- sample(id_vals, size = round(split_percentage[1] * n))
  val_id_list <- sample(
    setdiff(id_vals, train_id_list),
    round(split_percentage[2] * n)
  )
  test_id_list <- setdiff(id_vals, c(train_id_list, val_id_list))

  # now create 3 lists, "train / test / val", each with same structure as dat
  l <- list(
    "train" = train_id_list,
    "val" = val_id_list,
    "test" = test_id_list
  ) %>% lapply(FUN = function(id_list) {
    out <- lapply(X = dat_list, FUN = function(dat) {
      id_matches <- dat[[id_column]] %in% id_list
      dat[id_matches, ]
    })
    names(out) <- names(dat_list)
    out
  })
  names(l) <- c("train", "val", "test")

  return(l)
}

merge_dataset <- function(dat_list1, dat_list2) {
  dat_items_list <- names(dat_list1)
  l <- lapply(X = dat_items_list, FUN = function(item) {
    dat1 <- dat_list1[[item]]
    dat2 <- dat_list2[[item]]
    bind_rows(dat1, dat2)
  })
  names(l) <- dat_items_list
  return(l)
}

# compute metrics from confusion matrix
compute_metrics <- function(pred_values, true_values, mask = NULL) {
  # convert them to boolean factors
  pred_values <- factor(as.logical(pred_values), levels = c(FALSE, TRUE)) # true is the 2nd level
  true_values <- factor(as.logical(true_values), levels = c(FALSE, TRUE))

  if (!is.null(mask)) {
    pred_values <- pred_values[mask]
    true_values <- true_values[mask]
  }

  cm <- table(pred_values, true_values)

  # compute metrics, predicting TRUE is more important!
  tp <- cm[2, 2]
  tn <- cm[1, 1]
  fp <- cm[2, 1]
  fn <- cm[1, 2]

  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)

  # Print nicely
  cat("=== Overall Metrics ===\n")
  cat("Confusion Matrix:\n")
  print(cm)
  results <- c(
    "Accuracy" = accuracy,
    "Precision" = precision,
    "Recall" = recall,
    "F1" = f1
  )
  cat("\n")
  cat(sprintf("Accuracy: %.4f\n", accuracy))
  cat(sprintf("Precision: %.4f\n", precision))
  cat(sprintf("Recall: %.4f\n", recall))
  cat(sprintf("F1 Score: %.4f\n", f1))

  invisible(list(
    cm = cm,
    metrics = results
  ))
}

# compute metrices from each train / test / val splits
compute_metrics_on_split <- function(
    dat_list,
    prediction_function,
    true_column = "label",
    pred_column = "pred",
    mask_column = NULL) {
  dat_names <- names(dat_list)
  for (i in seq_along(dat_names)) {
    message(paste0("Metrices for ", dat_names[i], ":"))
    dat <- dat_list[[dat_names[i]]]
    pred <- prediction_function(dat)

    if (is.null(mask_column)) {
      compute_metrics(
        pred[[pred_column]],
        pred[[true_column]]
      )
    } else {
      compute_metrics(
        pred[[pred_column]],
        pred[[true_column]],
        mask = pred[[mask_column]]
      )
    }
  }
}
