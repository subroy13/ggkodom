#### STATE-RIBBON PLOT ####

#' State-ribbon trajectory plot
#'
#' Like the swimlane but segments are colored by a **discrete state**
#' rather than a continuous value.
#' States are either provided directly or derived by cutting
#' \code{value} at user-specified thresholds.
#'
#' @inheritParams kodom_swimlane
#' @param n_sample Max number of subjects to display.  \code{NULL} = all.
#'   Default 50.
#' @param sort_by Order subjects by their \code{"first"}, \code{"last"},
#'   \code{"mean"}, or \code{"median"} value, OR a named numeric vector
#'   (e.g.\ from \code{\link{kodom_sort_scores}}).  Default \code{"first"}.
#' @param point_size Point size (default 3).
#' @param alpha Segment transparency (default 0.8).
#' @param thresholds Numeric vector of cut points.
#'   E.g.\ \code{c(7, 8)} yields three states: \code{< 7},
#'   \code{7 - 8}, \code{> 8}.  Ignored when \code{state} is given.
#' @param state Column name (string) for a pre-computed state variable.
#'   Overrides \code{thresholds}.
#' @param state_labels Character labels for the states (low-to-high).
#'   Auto-generated from \code{thresholds} when \code{NULL}.
#' @param state_colors Character vector of colors per state.
#'   Auto-generated when \code{NULL}.
#' @param line_width Segment width (default 0.7).
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' set.seed(42)
#' n_subj <- 20; n_obs <- 5
#' df <- data.frame(
#'   id    = rep(seq_len(n_subj), each = n_obs),
#'   time  = as.vector(replicate(n_subj, sort(round(runif(n_obs, 0, 120))))),
#'   value = rnorm(n_subj * n_obs, 7, 1.5)
#' )
#' kodom_state(df, thresholds = c(6.5, 8))
kodom_state <- function(data, id = "id", time = "time", value = "value",
                        thresholds = NULL, state = NULL,
                        state_labels = NULL, state_colors = NULL,
                        n_sample = 50, sort_by = "first",
                        outcome = NULL, outcome_label = "*",
                        point_size = 3, line_width = 0.7, alpha = 0.8,
                        facet_rows = NULL, facet_cols = NULL,
                        seed = 123) {

  if (is.null(thresholds) && is.null(state)) {
    stop("Provide either 'thresholds' or 'state'.", call. = FALSE)
  }

  prep <- prep_trajectory_data(
    data, id, time, value,
    n_sample = n_sample, sort_by = sort_by,
    outcome = outcome, outcome_label = outcome_label,
    seed = seed
  )

  df   <- prep$data
  cols <- prep$cols

  ## assign states
  if (!is.null(state)) {
    check_columns(df, state)
    df$.state <- factor(df[[state]])
  } else {
    brks <- c(-Inf, sort(thresholds), Inf)
    n_states <- length(brks) - 1L

    if (is.null(state_labels)) {
      state_labels <- character(n_states)
      state_labels[1L] <- paste("<", thresholds[1L])
      if (n_states > 2L) {
        for (i in 2:(n_states - 1L)) {
          state_labels[i] <- paste(thresholds[i - 1L], "-", thresholds[i])
        }
      }
      state_labels[n_states] <- paste(">", thresholds[length(thresholds)])
    }

    df$.state <- cut(df[[cols$value]], breaks = brks,
                     labels = state_labels, include.lowest = TRUE)
  }

  ## default state colors
  if (is.null(state_colors)) {
    ns <- nlevels(df$.state)
    state_colors <- kodom_colors(max(ns, 3L))[seq_len(ns)]
  }

  ## build segments between consecutive observations per subject
  id_col   <- cols$id
  time_col <- cols$time
  df_split <- split(df, df[[id_col]], drop = TRUE)

  seg_list <- lapply(df_split, function(d) {
    n <- nrow(d)
    if (n < 2L) return(NULL)
    data.frame(
      .id         = rep(d[[id_col]][1L], n - 1L),
      .time_start = d[[time_col]][1:(n - 1L)],
      .time_end   = d[[time_col]][2:n],
      .state      = d$.state[1:(n - 1L)],
      stringsAsFactors = FALSE
    )
  })
  seg_df <- do.call(rbind, seg_list)
  if (is.null(seg_df) || nrow(seg_df) == 0L) {
    stop("No subjects have >= 2 observations; cannot draw segments.",
         call. = FALSE)
  }
  seg_df$.id <- factor(seg_df$.id, levels = prep$id_order)

  ## carry faceting variables onto segments
  if (!is.null(facet_rows) || !is.null(facet_cols)) {
    fv <- c(facet_rows, facet_cols)
    fv <- fv[!is.null(fv)]
    fmap <- df[!duplicated(df[[id_col]]), c(id_col, fv), drop = FALSE]
    names(fmap)[1L] <- ".id"
    seg_df <- merge(seg_df, fmap, by = ".id", all.x = TRUE)
  }

  ## plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(
        x = .data$.time_start, xend = .data$.time_end,
        y = .data$.id,         yend = .data$.id,
        color = .data$.state
      ),
      linewidth = line_width, alpha = alpha, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(
        x     = .data[[time_col]],
        y     = .data[[id_col]],
        color = .data$.state
      ),
      size = point_size
    ) +
    ggplot2::scale_color_manual(values = state_colors, name = "State")

  ## y-axis labels
  if (!is.null(prep$y_labels)) {
    p <- p + ggplot2::scale_y_discrete(labels = prep$y_labels)
  }

  ## faceting
  facet <- build_facet(facet_rows, facet_cols)
  if (!is.null(facet)) p <- p + facet

  p + theme_kodom() +
    ggplot2::labs(x = time_col, y = "")
}
