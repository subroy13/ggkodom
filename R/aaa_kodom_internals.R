#### SHARED INTERNALS ####
# This file must load before all geom_kodom_*.R files. The 'aaa_' prefix
# guarantees it is sourced first under R's default alphabetical load order.


# Interpolates colour and alpha between two consecutive observations and
# returns a data frame of n_interp sub-segments for GeomSegment.
# At draw_panel time `colour` is already a hex string (scale has been applied).
.kodom_interp_pair <- function(x0, x1, y0, y1,
                                col0, col1, a0, a1,
                                linewidth, linetype, PANEL, group,
                                n_interp) {
  t_seq <- seq(0, 1, length.out = n_interp + 1L)

  c0   <- grDevices::col2rgb(col0) / 255
  c1   <- grDevices::col2rgb(col1) / 255
  cols <- grDevices::rgb(
    c0[1L] + t_seq * (c1[1L] - c0[1L]),
    c0[2L] + t_seq * (c1[2L] - c0[2L]),
    c0[3L] + t_seq * (c1[3L] - c0[3L])
  )

  alphas <- a0 + t_seq * (a1 - a0)
  x_pts  <- x0 + t_seq * (x1 - x0)
  y_pts  <- y0 + t_seq * (y1 - y0)

  k <- length(t_seq) - 1L
  data.frame(
    x         = x_pts[seq_len(k)],
    xend      = x_pts[-1L],
    y         = y_pts[seq_len(k)],
    yend      = y_pts[-1L],
    colour    = cols[seq_len(k)],
    alpha     = alphas[seq_len(k)],
    linewidth = linewidth,
    linetype  = linetype,
    PANEL     = PANEL,
    group     = group,
    stringsAsFactors = FALSE
  )
}


# Builds the full interpolated segment data frame for all subjects in a panel.
# Called once per panel so GeomSegment draws everything in a single grob.
.kodom_build_segments <- function(data, n_interp = 20L) {
  data   <- data[order(data$group, data$x), ]
  groups <- split(data, data$group)

  seg_list <- lapply(groups, function(gd) {
    n <- nrow(gd)
    if (n < 2L) return(NULL)

    pairs <- lapply(seq_len(n - 1L), function(i) {
      a0 <- if (is.na(gd$alpha[i]))        1 else gd$alpha[i]
      a1 <- if (is.na(gd$alpha[i + 1L]))  1 else gd$alpha[i + 1L]
      
      # Dashed lines reset their dash pattern at the start of every grid segment.
      # If we break a dashed line into 20 tiny pieces, the pattern restarts 20 
      # times, effectively drawing a solid line. To preserve the dash pattern, 
      # we disable interpolation (n_interp = 1) for non-solid lines.
      lt <- gd$linetype[i]
      n_use <- if (!is.na(lt) && lt != "solid" && lt != "1" && lt != 1) 1L else n_interp

      .kodom_interp_pair(
        x0 = gd$x[i],        x1 = gd$x[i + 1L],
        y0 = gd$y[i],        y1 = gd$y[i + 1L],
        col0 = gd$colour[i], col1 = gd$colour[i + 1L],
        a0 = a0,             a1 = a1,
        linewidth = gd$linewidth[i],
        linetype  = gd$linetype[i],
        PANEL     = gd$PANEL[1L],
        group     = gd$group[i],
        n_interp  = n_use
      )
    })
    do.call(rbind, pairs)
  })

  do.call(rbind, seg_list)
}


# Assigns integer lane positions (y = 1 … N) to each subject.
#
# sort_by controls lane order. When a value aesthetic is present, it is used
# as the sort key. `fill` takes priority over `colour` so that
# geom_kodom_heatmap() (which maps fill, not colour) sorts correctly.
.kodom_assign_lanes <- function(data, sort_by = "none", n_max = Inf) {
  ids <- unique(data$id)

  if (is.finite(n_max) && length(ids) > n_max) {
    ids  <- sample(ids, n_max)
    data <- data[data$id %in% ids, ]
  }

  # Prefer fill if it is actually mapped (non-NA values exist); otherwise
  # fall back to colour. This lets both geom_kodom_line (colour) and
  # geom_kodom_heatmap (fill) share the same sorting logic.
  val_col <- if ("fill" %in% names(data) && any(!is.na(data$fill))) {
    "fill"
  } else if ("colour" %in% names(data)) {
    "colour"
  } else {
    NULL
  }

  sorted_ids <- switch(sort_by,
    none = as.character(ids),

    mean = {
      if (!is.null(val_col)) {
        m <- tapply(data[[val_col]], data$id, mean, na.rm = TRUE)
        names(sort(m, decreasing = TRUE))
      } else as.character(ids)
    },

    mean_asc = {
      if (!is.null(val_col)) {
        m <- tapply(data[[val_col]], data$id, mean, na.rm = TRUE)
        names(sort(m))
      } else as.character(ids)
    },

    first = {
      d <- data[order(data$x), ]
      d <- d[!duplicated(d$id), ]
      if (!is.null(val_col)) {
        as.character(d$id[order(d[[val_col]], decreasing = TRUE)])
      } else as.character(d$id)
    },

    last = {
      d <- data[order(data$x, decreasing = TRUE), ]
      d <- d[!duplicated(d$id), ]
      if (!is.null(val_col)) {
        as.character(d$id[order(d[[val_col]], decreasing = TRUE)])
      } else as.character(d$id)
    },

    as.character(ids)
  )

  lane_map <- stats::setNames(seq_along(sorted_ids), sorted_ids)
  data$y   <- lane_map[as.character(data$id)]
  data
}


#' Base stat shared by all kodom geoms
#'
#' Declares the `x` and `id` required aesthetics. Concrete stats inherit from
#' this and override `compute_panel()`.
#'
#' @keywords internal
#' @format A ggproto object.
StatKodomBase <- ggplot2::ggproto("StatKodomBase", ggplot2::Stat,
  required_aes = c("x", "id")
)
