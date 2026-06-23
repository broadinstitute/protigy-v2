################################################################################
# Module: QC_CV helpers
# Pure helper functions for coefficient of variation (CV) computation and
# visualization. These are free of Shiny reactivity and are fully unit-testable.
################################################################################

# Combine one or more cdesc columns row-wise into a single grouping vector.
# NAs in any selected column are replaced with the literal string "NA" so that
# NA-containing rows still form a visible group rather than being silently dropped.
#
# @param cdesc  data.frame or list with named columns
# @param cols   character vector of column names to combine
# @param sep    separator string (default "_")
# @return character vector of length nrow(cdesc)
combine_cdesc_cols <- function(cdesc, cols, sep = "_") {
  stopifnot(
    is.data.frame(cdesc) || is.list(cdesc),
    is.character(cols), length(cols) >= 1L,
    all(cols %in% names(cdesc))
  )
  vals <- lapply(cols, function(col) {
    x <- as.character(cdesc[[col]])
    x[is.na(x)] <- "NA"
    x
  })
  do.call(paste, c(vals, list(sep = sep)))
}

# Compute CV (sd / mean) per group per feature.
#
# @param mat       numeric matrix, features (rows) x samples (cols).
#                  rownames(mat) are used as feature IDs.
# @param grouping  character vector of length ncol(mat) assigning each sample
#                  to a group. Use combine_cdesc_cols() to produce this.
# @return data.frame with column `id` (feature identifier) followed by one
#         `CV_<group>` column per unique group. Features with zero/NA mean
#         produce NA CV (not Inf, not NaN).
compute_cv_table <- function(mat, grouping) {
  stopifnot(
    is.matrix(mat),
    is.numeric(mat),
    length(grouping) == ncol(mat)
  )
  groups <- unique(grouping)
  cv_cols <- vapply(groups, function(g) {
    cols <- which(grouping == g)
    sub  <- mat[, cols, drop = FALSE]
    mu   <- rowMeans(sub, na.rm = TRUE)
    sdv  <- apply(sub, 1L, function(x) sd(x, na.rm = TRUE))
    cv   <- sdv / mu
    # Guard: zero or NA mean -> NA CV (avoids Inf / NaN leaking downstream)
    cv[is.nan(cv) | is.infinite(cv)] <- NA_real_
    cv
  }, numeric(nrow(mat)))
  # vapply returns:
  #   nrow>1, groups>1 -> matrix (nrow x ngroups), colnames = group names
  #   nrow>1, groups==1 -> named numeric vector (length = nrow)
  #   nrow==1, groups>1 -> named numeric vector (length = ngroups)
  #   nrow==1, groups==1 -> single named scalar
  # Normalize to matrix with features as rows, groups as columns.
  if (!is.matrix(cv_cols)) {
    if (nrow(mat) == 1L) {
      # one feature, possibly multiple groups
      cv_cols <- matrix(cv_cols, nrow = 1L, dimnames = list(NULL, names(cv_cols)))
    } else {
      # one group, multiple features
      cv_cols <- matrix(cv_cols, ncol = 1L)
    }
  }
  colnames(cv_cols) <- paste0("CV_", groups)
  data.frame(
    id       = rownames(mat),
    cv_cols,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
}

# Filter a CV table by a cutoff value.
# Features (rows) are kept if their CV satisfies the cutoff according to the
# min_groups rule:
#   "one" -- at least one group's CV is strictly below the cutoff
#   "all" -- every group's CV is strictly below the cutoff
# NA CVs are treated as "not satisfying" the cutoff.
#
# @param cv_df      data.frame returned by compute_cv_table()
# @param cutoff     numeric cutoff value (e.g. 0.2 for 20%)
# @param min_groups "one" or "all"
# @return subset of cv_df with features that satisfy the cutoff rule
filter_cv_table <- function(cv_df, cutoff, min_groups = c("one", "all")) {
  min_groups <- match.arg(min_groups)
  cv_mat <- as.matrix(cv_df[, -1L, drop = FALSE])
  keep <- if (min_groups == "one") {
    rowSums(cv_mat < cutoff, na.rm = TRUE) >= 1L
  } else {
    n_groups <- ncol(cv_mat)
    rowSums(cv_mat < cutoff, na.rm = TRUE) == n_groups
  }
  cv_df[keep, , drop = FALSE]
}

# Create a violin + boxplot of CV distributions.
# Mirrors calculate_cvs.R:63-72.
#
# @param cv_df        data.frame from compute_cv_table()
# @param title_suffix string appended to the plot title
# @param palette      character vector of colors (named or positional)
# @param log_scale    logical; if TRUE use log10 y-axis
# @param y_range      numeric length-2 vector (ymin, ymax) for zoom, or NULL
# @return ggplot object
create_cv_violin_plot <- function(cv_df, title_suffix = "", palette,
                                  log_scale = FALSE, y_range = NULL) {
  title <- trimws(paste("CV distributions", title_suffix))
  long_df <- tidyr::gather(cv_df, key = "Group", value = "CV", -id)
  # Extract group label from column name (strip leading "CV_")
  long_df$Group <- sub("^CV_", "", long_df$Group)
  y_axis_label <- if (log_scale) "log10(CV)" else "CV"
  font.size <- scale_font_size(dimension = length(unique(long_df$Group)))

  p <- ggplot2::ggplot(long_df, ggplot2::aes(x = .data$Group, y = .data$CV, fill = .data$Group)) +
    # Keep full-opacity user-selected colors for the violin fills.
    ggplot2::geom_violin(alpha = 1, show.legend = FALSE, width = 0.9) +
    ggplot2::geom_boxplot(
      width = 0.08,
      outlier.shape = NA,
      fill = NA,
      color = "black",
      show.legend = FALSE
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = font.size)
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab("Group") +
    ggplot2::ylab(y_axis_label) +
    ggplot2::scale_fill_manual(values = palette)

  if (log_scale) {
    p <- p + ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01))
  } else {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  }

  if (!is.null(y_range)) {
    p <- p + ggplot2::coord_cartesian(ylim = y_range)
  }

  p
}
