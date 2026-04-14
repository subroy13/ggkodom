## Sourceable evaluation harness for probability predictions
##
## One function: eval_predictions(probs, target, mask, train_probs, train_target, train_mask)
##
## Returns a list with:
##   - f1_train_tuned   : F1 on val using a threshold tuned on train (the honest number)
##   - f1_self_tuned    : F1 on val using a threshold tuned on val itself (data-leaking upper bound)
##   - threshold_train  : threshold from train tuning
##   - threshold_val    : threshold from val tuning (for comparison only)
##   - log_loss         : mean negative log likelihood, returned-only (lower is better)
##   - brier            : mean (prob - target)^2, returned-only (lower is better)
##   - ece              : expected calibration error (10 bins), returned-only
##   - calibration_df   : tibble with (bin, mean_prob, mean_target, n) for plotting
##   - confmat          : confusion matrix at f1_train_tuned threshold
##
## Why this exists: F1 is rank-based and blind to calibration.
## subrata_ctmc_glm.R proved that AIC can drop 1167 points without F1 moving.
## For interpretable / generative models, log-loss + Brier + ECE are the
## metrics that actually distinguish them from feature-engineered baselines.
##
## Usage:
##   source("scripts/subrata_bayesian_eval_harness.R")
##   res <- eval_predictions(probs_val, target_val_uncontrolled, mask_val,
##                           train_probs = probs_train,
##                           train_target = target_train_uncontrolled,
##                           train_mask = mask_train)
##   print_eval(res)

suppressPackageStartupMessages({
    library(tidyverse)
})


## A small wrapper around best_f1_threshold from utils.R that returns just the
## threshold value (numeric, not the named vector).
.threshold_only <- function(probs, target_logical, mask) {
    th <- best_f1_threshold(probs, target_logical, mask)
    unname(th["best_th"])
}


## Compute log-loss (mean negative log likelihood) on the masked subset.
.log_loss <- function(probs, target_logical, mask = NULL) {
    if (!is.null(mask)) {
        probs <- probs[mask]
        target_logical <- target_logical[mask]
    }
    p <- pmin(pmax(probs, 1e-15), 1 - 1e-15)  # clip to avoid log(0)
    y <- as.integer(target_logical)
    -mean(y * log(p) + (1 - y) * log(1 - p))
}


## Brier score on the masked subset.
.brier <- function(probs, target_logical, mask = NULL) {
    if (!is.null(mask)) {
        probs <- probs[mask]
        target_logical <- target_logical[mask]
    }
    mean((probs - as.integer(target_logical))^2)
}


## Expected calibration error and binned calibration table.
## n_bins = 10 by default. Bins are equal-width on [0,1], not equal-count, so
## the table can be plotted directly as a calibration diagram.
.calibration <- function(probs, target_logical, mask = NULL, n_bins = 10) {
    if (!is.null(mask)) {
        probs <- probs[mask]
        target_logical <- target_logical[mask]
    }
    breaks <- seq(0, 1, length.out = n_bins + 1)
    bin <- cut(probs, breaks = breaks, include.lowest = TRUE, labels = FALSE)
    df <- tibble(prob = probs, y = as.integer(target_logical), bin = bin) %>%
        group_by(bin) %>%
        summarise(
            mean_prob   = mean(prob),
            mean_target = mean(y),
            n           = n(),
            .groups = "drop"
        ) %>%
        arrange(bin)
    ece <- sum(df$n * abs(df$mean_prob - df$mean_target)) / sum(df$n)
    list(ece = ece, calibration_df = df)
}


## Main entry point.
##
## probs        : numeric vector, P(uncontrolled) for each row of the val set
## target       : logical (or 0/1) vector, true label = (a1c_2025 > 8)
## mask         : logical vector, TRUE for "returned" patients (with a1c_2025 measured)
## train_probs  : same as `probs` but for the training set, used to tune the threshold
## train_target : same as `target` but for training
## train_mask   : same as `mask` but for training
##
## All inputs are returned-or-not (mask handles filtering inside compute_metrics).
##
## Returns a named list (see header above).

eval_predictions <- function(probs, target, mask,
                             train_probs = NULL, train_target = NULL, train_mask = NULL) {
    stopifnot(length(probs) == length(target),
              length(probs) == length(mask))

    target_logical <- as.logical(target)

    ## Threshold tuned on train (clean, honest)
    if (!is.null(train_probs)) {
        stopifnot(length(train_probs) == length(train_target),
                  length(train_probs) == length(train_mask))
        train_target_logical <- as.logical(train_target)
        th_train <- .threshold_only(train_probs, train_target_logical, train_mask)
    } else {
        th_train <- NA_real_
    }

    ## Threshold tuned on val (data-leaking, comparison only)
    th_val <- .threshold_only(probs, target_logical, mask)

    ## F1 with each threshold
    res_train_th <- if (!is.na(th_train)) {
        compute_metrics(probs > th_train, target_logical, mask, verbose = FALSE)
    } else {
        list(metrics = c("F1" = NA_real_), cm = NULL)
    }
    res_val_th <- compute_metrics(probs > th_val, target_logical, mask, verbose = FALSE)

    ## Calibration metrics
    log_loss_val <- .log_loss(probs, target_logical, mask)
    brier_val    <- .brier(probs, target_logical, mask)
    cal          <- .calibration(probs, target_logical, mask, n_bins = 10)

    list(
        f1_train_tuned  = unname(res_train_th$metrics["F1"]),
        f1_self_tuned   = unname(res_val_th$metrics["F1"]),
        precision_train_tuned = unname(res_train_th$metrics["Precision"]),
        recall_train_tuned    = unname(res_train_th$metrics["Recall"]),
        threshold_train = th_train,
        threshold_val   = th_val,
        log_loss        = log_loss_val,
        brier           = brier_val,
        ece             = cal$ece,
        calibration_df  = cal$calibration_df,
        confmat         = res_train_th$cm,
        n_eval          = sum(mask, na.rm = TRUE),
        base_rate       = mean(target_logical[mask], na.rm = TRUE)
    )
}


## Pretty-printer for the eval result. Drop into a script with `print_eval(res)`.

print_eval <- function(res, label = NULL) {
    if (!is.null(label)) cat("\n========== ", label, " ==========\n", sep = "")
    cat(sprintf("Eval set: n = %d, base rate = %.4f\n", res$n_eval, res$base_rate))
    cat(sprintf("Train-tuned threshold = %.4f -> val F1 = %.4f (prec %.4f, rec %.4f)\n",
                res$threshold_train, res$f1_train_tuned,
                res$precision_train_tuned, res$recall_train_tuned))
    cat(sprintf("Val-tuned threshold   = %.4f -> val F1 = %.4f  (data-leaking, upper bound)\n",
                res$threshold_val, res$f1_self_tuned))
    cat(sprintf("Log-loss = %.4f | Brier = %.4f | ECE = %.4f\n",
                res$log_loss, res$brier, res$ece))
    cat("Calibration table (10 equal-width bins):\n")
    print(res$calibration_df, n = Inf)
    if (!is.null(res$confmat)) {
        cat("Confusion matrix at train-tuned threshold:\n")
        print(res$confmat)
    }
    invisible(res)
}
