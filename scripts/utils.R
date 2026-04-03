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




# TODO: verify if correct
compute_metrics <- function(cm) {
  # Ensure matrix
  cm <- as.matrix(cm)
  
  # Basic totals
  total <- sum(cm)
  correct <- sum(diag(cm))
  
  # Accuracy
  accuracy <- correct / total
  
  # Precision, Recall, F1 (per class)
  precision <- diag(cm) / colSums(cm)
  recall    <- diag(cm) / rowSums(cm)
  f1        <- 2 * (precision * recall) / (precision + recall)
  
  # Handle NaN (e.g., division by zero)
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  f1[is.na(f1)] <- 0
  
  # Macro averages
  macro_precision <- mean(precision)
  macro_recall    <- mean(recall)
  macro_f1        <- mean(f1)
  
  # Print nicely
  cat("=== Overall Metrics ===\n")
  cat("Confusion Matrix:\n")
  print(cm)
  cat(sprintf("Accuracy: %.4f\n\n", accuracy))
  
  cat("=== Per-Class Metrics ===\n")
  results <- data.frame(
    Class = rownames(cm),
    Precision = round(precision, 4),
    Recall = round(recall, 4),
    F1 = round(f1, 4)
  )
  print(results, row.names = FALSE)
  
  cat("\n=== Macro Averages ===\n")
  cat(sprintf("Precision: %.4f\n", macro_precision))
  cat(sprintf("Recall: %.4f\n", macro_recall))
  cat(sprintf("F1 Score: %.4f\n", macro_f1))
  
  # Return invisibly if needed
  invisible(list(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1 = f1,
    macro = c(
      precision = macro_precision,
      recall = macro_recall,
      f1 = macro_f1
    )
  ))
}

