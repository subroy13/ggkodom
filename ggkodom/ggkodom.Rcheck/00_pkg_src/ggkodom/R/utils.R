#### INTERNAL UTILITIES ####

#' Check that required columns exist in a data frame
#' @param data A data frame
#' @param cols Character vector of required column names
#' @return Invisible TRUE; stops with an informative error if columns are missing
#' @keywords internal
check_columns <- function(data, cols) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop("Column(s) not found in data: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Prepare longitudinal data for trajectory plotting
#'
#' Validates columns, samples subjects, orders the ID factor by a summary
#' statistic of the value, and optionally builds y-axis outcome labels.
#' All four public plot functions call this internally.
#'
#' @param data A data frame in long format (one row per measurement).
#' @param id Column name (string) for subject identifier.
#' @param time Column name (string) for time variable (coerced to numeric).
#' @param value Column name (string) for measurement value (coerced to numeric).
#' @param n_sample Max subjects to keep. \code{NULL} = all.
#' @param sort_by Either (a) one of \code{"first"}, \code{"last"},
#'   \code{"mean"}, \code{"median"} — a summary of the value column;
#'   or (b) a named numeric vector whose names match subject ids,
#'   used directly as the sort key (e.g.\ FPC scores from
#'   \code{\link{kodom_sort_scores}}, BLUPs, cluster scores, ...).
#' @param outcome Optional column name (string) for binary outcome.
#' @param outcome_label Character to mark positive-outcome subjects.
#' @param seed Random seed for sampling.
#' @return A list:
#'   \describe{
#'     \item{data}{Data frame with factor-ordered ID column.}
#'     \item{id_order}{Character vector of subject IDs in display order.}
#'     \item{y_labels}{Named character vector for y-axis labels, or NULL.}
#'     \item{cols}{List with \code{id}, \code{time}, \code{value} names.}
#'   }
#' @keywords internal
prep_trajectory_data <- function(data, id, time, value,
                                 n_sample = NULL,
                                 sort_by = c("first", "last", "mean", "median"),
                                 outcome = NULL,
                                 outcome_label = "*",
                                 seed = 123) {
  ## sort_by may be either a method string or a named numeric vector
  use_named_sort <- is.numeric(sort_by) && !is.null(names(sort_by))
  if (!use_named_sort) sort_by <- match.arg(sort_by)

  ## validate

  check_columns(data, c(id, time, value))
  if (!is.null(outcome)) check_columns(data, outcome)

  ## copy so we never mutate the caller's object
  df <- data

  ## coerce
  df[[time]]  <- as.numeric(df[[time]])
  df[[value]] <- as.numeric(df[[value]])

  ## drop incomplete rows
  keep <- !is.na(df[[time]]) & !is.na(df[[value]])
  df <- df[keep, , drop = FALSE]
  if (nrow(df) == 0L) {
    stop("No complete observations after removing NAs.", call. = FALSE)
  }

  ## sample subjects
  ids_avail <- unique(df[[id]])
  if (!is.null(n_sample) && n_sample < length(ids_avail)) {
    set.seed(seed)
    keep_ids <- sample(ids_avail, n_sample)
    df <- df[df[[id]] %in% keep_ids, , drop = FALSE]
  }

  ## sort within subject by time
  df <- df[order(df[[id]], df[[time]]), , drop = FALSE]

  ## compute sort key for y-axis ordering
  if (use_named_sort) {
    ids_now <- as.character(unique(df[[id]]))
    miss <- setdiff(ids_now, names(sort_by))
    if (length(miss) > 0L) {
      stop("sort_by has no value for ", length(miss), " subject id(s): ",
           paste(utils::head(miss, 5L), collapse = ", "),
           if (length(miss) > 5L) ", ..." else "", call. = FALSE)
    }
    sort_key <- sort_by[ids_now]
    names(sort_key) <- ids_now
  } else {
    sort_fn <- switch(sort_by,
      first  = function(x) x[1L],
      last   = function(x) x[length(x)],
      mean   = mean,
      median = stats::median
    )
    sort_key <- tapply(df[[value]], df[[id]], sort_fn)
  }
  id_order <- names(sort(sort_key))
  df[[id]] <- factor(df[[id]], levels = id_order)

  ## outcome labels for y-axis
  y_labels <- NULL
  if (!is.null(outcome)) {
    out_vals <- tapply(df[[outcome]], df[[id]], function(x) x[1L])
    y_labels <- ifelse(
      out_vals[id_order] %in% c(1, TRUE, "TRUE", "T", "1"),
      outcome_label, ""
    )
    names(y_labels) <- id_order
  }

  list(
    data     = df,
    id_order = id_order,
    y_labels = y_labels,
    cols     = list(id = id, time = time, value = value)
  )
}


#' Build a facet_grid formula from optional row/column variables
#' @keywords internal
build_facet <- function(facet_rows = NULL, facet_cols = NULL) {
  if (is.null(facet_rows) && is.null(facet_cols)) return(NULL)
  row_var <- if (!is.null(facet_rows)) facet_rows else "."
  col_var <- if (!is.null(facet_cols)) facet_cols else "."
  ggplot2::facet_grid(
    stats::as.formula(paste(row_var, "~", col_var)),
    scales = "free_y", space = "free_y"
  )
}
