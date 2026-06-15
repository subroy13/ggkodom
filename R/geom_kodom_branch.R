#### STAT AND GEOM FOR geom_kodom_branch ####
# Shared internals (.kodom_assign_lanes, .kodom_build_segments, StatKodomBase)
# live in aaa_kodom_internals.R, which loads first alphabetically.


#' Stat for geom_kodom_branch
#'
#' Assigns integer base lanes (one per subject) then stacks prediction arms as
#' fractional sub-lanes above the observed path within each subject's lane.
#'
#' Rows where `medication` is `NA` are the observed path; rows with a non-NA
#' `medication` value are prediction arms. The stat replaces `NA` in
#' `data$medication` with `"observed"` so that the linetype scale can be
#' driven directly from the medication column:
#' ```r
#' scale_linetype_manual(
#'   values = c("observed" = "solid", "DrugA" = "dashed", "DrugB" = "dotted")
#' )
#' ```
#' Two internal columns — `kodom_lane` (integer lane rank) and `kodom_track`
#' (cleaned medication value) — are passed through to `draw_panel` so that
#' fork connectors can be placed at each subject's branch point.
#'
#' @keywords internal
#' @format A ggproto object.
StatKodomBranch <- ggplot2::ggproto(
  "StatKodomBranch", StatKodomBase,
  required_aes = c("x", "id", "colour"),
  compute_panel = function(data, scales,
                           sort_by = "none", n_max = Inf,
                           lane_width = 1,
                           branch_fraction = 0.7) {
    if (!"medication" %in% names(data)) data$medication <- NA_character_

    data <- .kodom_assign_lanes(data, sort_by = sort_by, n_max = n_max)
    # data$y is now the integer base lane rank (1, 2, ..., N)

    # Replace NA medication with "observed" so the linetype scale gets a real
    # string for every row and na.value is never triggered.
    data$medication <- ifelse(
      is.na(data$medication), "observed", as.character(data$medication)
    )

    med_levels <- setdiff(unique(data$medication), "observed")
    K <- length(med_levels)

    # Integer sub-lane index: 0 = observed, 1..K = prediction arms.
    med_as_int <- if (K > 0L) {
      ifelse(
        data$medication == "observed", 0L,
        match(data$medication, med_levels)
      )
    } else {
      rep(0L, nrow(data))
    }

    # Carry the integer lane and track label through to draw_panel so that
    # fork connectors can be computed without needing patient id.
    data$kodom_lane <- data$y # integer, before lane_width scaling
    data$kodom_track <- data$medication

    # One unique group per (subject, medication) pair.
    data$group <- as.integer(
      factor(paste(data$id, data$medication, sep = "\031"))
    )

    # Linetype handling — two modes depending on whether the user mapped it:
    #
    # 1. NOT mapped (data$linetype arrived as the GeomPath default "solid" for
    #    every row, no NAs): write valid grid strings directly so no scale is
    #    needed. Each arm gets a distinct linetype in order.
    #
    # 2. MAPPED via aes(linetype = medication) (data$linetype has NAs where
    #    medication was NA): replace NA with "observed" so the scale receives
    #    clean strings. Use scale_linetype_manual() to control exact values.
    arm_linetypes <- c("dashed", "dotted", "dotdash", "longdash")
    if (anyNA(data$linetype)) {
      data$linetype <- ifelse(
        is.na(data$linetype), "observed", as.character(data$linetype)
      )
    } else {
      data$linetype <- ifelse(
        data$medication == "observed",
        "solid",
        arm_linetypes[((med_as_int - 1L) %% length(arm_linetypes)) + 1L]
      )
    }

    # Final y: (base_lane + sub_offset) * lane_width.
    sub_step <- if (K > 0L) branch_fraction / K else 0
    data$y <- (data$y + med_as_int * sub_step) * lane_width

    data
  }
)


#' Geom for geom_kodom_branch
#'
#' Extends [ggplot2::GeomPath] to draw branching trajectory plots. In addition
#' to the interpolated colour-gradient path (via `.kodom_build_segments()`),
#' draws a short vertical **fork connector** at each subject's branch point —
#' the first x that appears in a predicted arm — connecting the observed lane
#' to the topmost prediction arm. Toggle with `show_fork`.
#'
#' @keywords internal
#' @format A ggproto object.
GeomKodomBranch <- ggplot2::ggproto(
  "GeomKodomBranch", ggplot2::GeomPath,
  default_aes = modifyList(
    ggplot2::GeomPath$default_aes,
    list(
      linewidth = 0.5,
      alpha     = 0.8,
      size      = 2.5,
      shape     = 19,
      fill      = NA,
      stroke    = 0.5
    )
  ),
  setup_data = function(data, params) {
    data$id <- NULL
    data$medication <- NULL
    # kodom_lane and kodom_track are retained for fork computation.
    data
  },
  draw_panel = function(data, panel_params, coord,
                        show_points = TRUE, show_fork = TRUE,
                        lineend = "butt", na.rm = FALSE) {
    seg_data <- .kodom_build_segments(data)
    line_grob <- if (is.null(seg_data) || nrow(seg_data) == 0L) {
      grid::nullGrob()
    } else {
      ggplot2::GeomSegment$draw_panel(
        seg_data, panel_params, coord,
        lineend = lineend, na.rm = na.rm
      )
    }

    # Fork connectors — one vertical segment per subject at the branch point.
    # Branch point = first x that appears in any prediction arm for that lane.
    fork_grob <- grid::nullGrob()
    if (show_fork && "kodom_lane" %in% names(data)) {
      fork_segs <- do.call(rbind, lapply(
        unique(data$kodom_lane), function(lane) {
          ld <- data[data$kodom_lane == lane, ]
          obs_rows <- ld[ld$kodom_track == "observed", ]
          prd_rows <- ld[ld$kodom_track != "observed", ]
          if (nrow(prd_rows) == 0L) {
            return(NULL)
          }

          fork_x <- min(prd_rows$x)

          # Observed y at the closest observed point at or before fork_x.
          obs_before <- obs_rows[obs_rows$x < fork_x, ]
          if (nrow(obs_before) == 0L) obs_before <- obs_rows
          fork_x_start <- max(obs_before$x)
          obs_ref <- obs_before[which.max(obs_before$x), ]

          # All prediction arms present at fork_x, one row each.
          arms_at_fork <- prd_rows[prd_rows$x == fork_x, ]
          if (nrow(arms_at_fork) == 0L) {
            return(NULL)
          }

          # One diagonal spoke per arm: (fork_x_start, obs_y) → (fork_x, arm_y).
          # This fans the connector out to every arm so none hangs unconnected.
          do.call(rbind, lapply(seq_len(nrow(arms_at_fork)), function(i) {
            arm_y <- arms_at_fork$y[i]
            if (is.na(arm_y) || abs(arm_y - obs_ref$y) < 1e-9) {
              return(NULL)
            }
            data.frame(
              x = fork_x_start,
              xend = fork_x,
              y = obs_ref$y,
              yend = arm_y,
              colour = obs_ref$colour,
              alpha = if (is.na(obs_ref$alpha)) 1 else obs_ref$alpha,
              linewidth = obs_ref$linewidth,
              linetype = "solid",
              PANEL = ld$PANEL[1L],
              group = -(lane * 100L + i),
              stringsAsFactors = FALSE
            )
          }))
        }
      ))
      if (!is.null(fork_segs) && nrow(fork_segs) > 0L) {
        fork_grob <- ggplot2::GeomSegment$draw_panel(
          fork_segs, panel_params, coord,
          lineend = lineend, na.rm = na.rm
        )
      }
    }

    draw_pts <- show_points &&
      !all(is.na(data$shape)) &&
      !all(is.na(data$size) | data$size <= 0)

    if (!draw_pts) {
      return(grid::grobTree(line_grob, fork_grob))
    }

    point_data <- data
    point_data$linewidth <- NULL
    point_grob <- ggplot2::GeomPoint$draw_panel(
      point_data, panel_params, coord,
      na.rm = na.rm
    )
    grid::grobTree(line_grob, fork_grob, point_grob)
  }
)


#' Branching swimlane plot for observed paths and counterfactual predictions
#'
#' Draws one horizontal path per subject for the observed trajectory, then fans
#' out into sub-lanes — one per medication or intervention arm — for predicted
#' (counterfactual) trajectories. The branching time may differ across subjects.
#'
#' **Data format.** Supply `NA` in the `medication` column for all observed rows
#' (pre- and post-branch). Supply a non-NA arm label (e.g. `"DrugA"`) for every
#' predicted row. The stat replaces `NA` with `"observed"` in its output so
#' that the linetype scale receives a clean string for every row.
#'
#' **Linetype.** Map `linetype = <arm_column>` in `aes()` **and** supply a
#' `scale_linetype_manual()` so that the legend appears and the linetypes are
#' exactly what you want. The stat converts `NA` (observed rows) to the string
#' `"observed"` before the scale is applied, so target that key explicitly:
#' ```r
#' aes(linetype = arm, medication = arm)   # both point to the same column
#' scale_linetype_manual(
#'   values = c("observed" = "solid", "DrugA" = "dashed", "DrugB" = "dotted")
#' )
#' ```
#'
#' **Fork connector.** A short vertical segment is drawn at each subject's
#' branch point (the first x that appears in a predicted arm), connecting the
#' observed lane to the topmost arm. Suppress with `show_fork = FALSE`.
#'
#' **Lane layout.** Each subject occupies a primary band of width `lane_width`.
#' Within that band the observed path is at the base; each prediction arm sits
#' at an equal sub-lane fraction above it controlled by `branch_fraction`. With
#' two arms and `branch_fraction = 0.7`, arm 1 is at `0.35 * lane_width` above
#' the base and arm 2 is at `0.70 * lane_width`, leaving 30% clearance before
#' the next subject.
#'
#' **Y-axis labels.** Subject IDs can be added by setting custom breaks:
#' ```r
#' scale_y_continuous(
#'   breaks = seq_len(n_subjects) * lane_width,
#'   labels = subject_ids
#' )
#' ```
#'
#' @section Aesthetics:
#' - **`x`** — time (numeric or Date)
#' - **`id`** — subject identifier; determines the primary lane position
#' - **`colour`** — measured value mapped to colour (interpolated along path)
#' - `medication` — `NA` for observed rows; a character/factor arm label for
#'   predicted rows. Each unique non-NA value becomes one sub-lane.
#' - `size`, `linewidth`, `alpha`, `shape`, `stroke` — standard path/point
#'   aesthetics. `linetype` is set by the stat (medication name / `"observed"`).
#'
#' @inheritParams geom_kodom_line
#' @param lane_width Positive numeric. Vertical distance between adjacent
#'   subject lanes. Default `1`.
#' @param branch_fraction Fraction of `lane_width` devoted to prediction
#'   sub-lanes. Default `0.7`. With K arms the sub-lane step is
#'   `branch_fraction / K`.
#' @param show_fork Logical. If `TRUE` (default), draws a vertical fork
#'   connector at each subject's branch point.
#' @return A ggplot2 layer object.
#' @export
#' @examples
#' \donttest{
#' library(ggplot2)
#' df <- data.frame(
#'   subject_id = rep(1:5, each = 4),
#'   time = rep(1:4, 5),
#'   visit_month = rep(1:4, 5),
#'   value = rep(1:4, 5),
#'   hba1c = rep(1:4, 5),
#'   arm = rep(c("Treatment", "Control"), c(12, 8))
#' )
#' ggplot(df, aes(
#'   x = time, id = subject_id,
#'   colour = hba1c, linetype = arm, medication = arm
#' )) +
#'   geom_kodom_branch(sort_by = "mean", lane_width = 2) +
#'   scale_linetype_manual(
#'     values = c("observed" = "solid", "DrugA" = "dashed", "DrugB" = "dotted")
#'   ) +
#'   scale_colour_kodom() +
#'   theme_kodom()
#' }
geom_kodom_branch <- function(mapping = NULL, data = NULL,
                              stat = StatKodomBranch,
                              position = "identity",
                              ...,
                              sort_by = "none",
                              n_max = Inf,
                              lane_width = 1,
                              branch_fraction = 0.7,
                              show_points = TRUE,
                              show_fork = TRUE,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomKodomBranch,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sort_by = sort_by, n_max = n_max,
      lane_width = lane_width, branch_fraction = branch_fraction,
      show_points = show_points, show_fork = show_fork,
      na.rm = na.rm, ...
    )
  )
}
