################################################################################
# Module: Stat_Plot
#
# Allow users to see the Volcano plot of their results
################################################################################

# Magenta for all volcano feature labels (POI, top-20, and all-significant);
# contrasts with darkred significant scatter points (plotVolcano sig.col).
.volcano_label_hex <- "#FF00FF"

# Build plotly hover `text` for volcano points: always `ID: ...`; optional second
# line from the real gene-symbol metadata column when `gs_vals` is provided
# (same length as ids); optional third line from the user-selected label column
# when `lbl_vals` + `lbl_col_name` are provided and length matches ids.
# @noRd
volcano_build_hover_text <- function(ids,
                                     gs_vals = NULL, gs_col_name = NULL,
                                     lbl_vals = NULL, lbl_col_name = NULL) {
  ids <- as.character(ids)
  ht  <- paste0("ID: ", ids)

  if (!is.null(gs_vals)) {
    gs_vals <- as.character(gs_vals)
    if (length(gs_vals) == length(ids)) {
      nm <- if (!is.null(gs_col_name) && nzchar(as.character(gs_col_name)[1L])) {
        as.character(gs_col_name)[1L]
      } else {
        "geneSymbol"
      }
      ht <- paste0(ht, "<br>", nm, ": ", gs_vals)
    }
  }

  if (!is.null(lbl_vals) && !is.null(lbl_col_name) &&
      nzchar(as.character(lbl_col_name)[1L])) {
    lbl_vals <- as.character(lbl_vals)
    if (length(lbl_vals) == length(ids)) {
      ht <- paste0(ht, "<br>", as.character(lbl_col_name)[1L], ": ", lbl_vals)
    }
  }

  ht
}

# #Input parameters- 
# ome- ome that plot is run on
# volcano_groups- current group selected in the plot sidebar
# volcano_contrasts- current contrast selected in the plot sidebar
# df- stat_results of selected ome
# sig.col- color of significant points
# bg.col- color of non significant points

plotVolcano <- function(ome, volcano_groups, volcano_contrasts, df, stat_params, stat_results,
                        sig.col = 'darkred', bg.col = 'gray', gene_symbol_col = "geneSymbol",
                        label_proteins = character(0), label_mode = character(0),
                        label_column = "id", label_split_enabled = FALSE,
                        label_split_sep = ";", label_display_trim_enabled = FALSE,
                        n_top = 20L) {
  
  cat('\n-- plotVolcano --\n')

  if (is.null(stat_params())) stop("stat_params is NULL")
  if (is.null(stat_results())) stop("stat_results is NULL")
  
  ##LOG FC COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    logfc_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "logFC.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_over_", groups[2])
    logfc_pattern <- paste0("logFC.*", contrast_name)
  } 
  
  logFC_col <- grep(logfc_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  
  ##LOG P VALUE COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    logP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "Log.P.Value.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_over_", groups[2])
    logP_pattern  <- paste0("Log\\.P\\.Value.*", contrast_name)
  } 
  
  logP_col <- grep(logP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  
  ##ADJ P VALUE COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    adjP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "adj.P.Val.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_over_", groups[2])
    adjP_pattern  <- paste0("adj\\.P\\.Val.*", contrast_name)
  } 
  
  adjP_col <- grep(adjP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##P VAL COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    pval_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "P.value.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_over_", groups[2])
    pval_pattern  <- paste0("P\\.value.*", contrast_name)
  }

  pval_col <- grep(pval_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]

  ##ID COLUMN##
  id_col <- grep("id", colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##GENE SYMBOL COLUMN##
  #make a parameter so this could be user-specified
  geneSymbol_col <- tryCatch({
    grep(gene_symbol_col, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  }, error = function(e) {
    NULL
  })
  
  ## Check columns exist
  required_cols <- c(logFC_col, logP_col, adjP_col, id_col)
  if(!all(required_cols %in% colnames(df))) {
    stop("Some required columns are missing in the result data.")
  }
  
  
  # Only filter out rows where logP is NA (essential for volcano plot)
  # Keep rows with NA in other columns as they might be legitimately untested
  df <- df[!is.na(df[[logP_col]]), ]
  
  ## Add columns for plotting
  df$id <- df[[id_col]]
  df$logFC <- df[[logFC_col]]
  df$adj.P.Val <- as.numeric(df[[adjP_col]])
  df$logP <- df[[logP_col]]
  df$P.Value <- if (!is.na(pval_col)) as.numeric(df[[pval_col]]) else NA_real_
  
  # Resolve the user-selected label column into df$geneSymbol.
  # NOTE: geneSymbol is repurposed as "resolved label text"  -  it may contain
  # values from any rdesc column (not necessarily a literal gene symbol).
  lbl_col <- if (!is.null(label_column) && nzchar(label_column) &&
                   label_column %in% colnames(df)) label_column else "id"
  # Real gene symbol column for hover only (before df$geneSymbol is repurposed for labels)
  gs_for_hover <- if (!is.na(geneSymbol_col) && geneSymbol_col %in% colnames(df)) {
    as.character(df[[geneSymbol_col]])
  } else {
    NULL
  }
  df$geneSymbol <- resolve_volcano_label_text(
    df[[lbl_col]],
    split_enabled = isTRUE(label_split_enabled),
    separator     = label_split_sep
  )
  # Fall back to feature id for any row where the resolved label is NA or empty
  na_mask <- is.na(df$geneSymbol) | !nzchar(df$geneSymbol)
  df$geneSymbol[na_mask] <- df$id[na_mask]
  
  ## Define significance based on chosen stat and cutoff
  sig_cutoff <- stat_params()[[ome]]$cutoff
  sig_stat <- stat_params()[[ome]]$stat
  
  # Always use nominal p-values for Y-axis
  # stat <- df$logP
  
  # Compute threshold for dashed line
  if(sig_stat == "adj.p.val") {
    passing.id <- which(df$adj.P.Val < sig_cutoff)
    if(length(passing.id) > 0){
      # Set y-axis threshold based on maximum nominal p-value among features that pass adj.p filter
      # This corresponds to the logic used in the original ProTIGY volcano plot
      y_cutoff <- -log10(max(df$P.Value[passing.id], na.rm = TRUE))
    } else {
      y_cutoff <- Inf
    }
  } else {
    y_cutoff <- -log10(sig_cutoff)
  }
  
  df$Significant <- df$logP > y_cutoff
  df$point_color <- ifelse(df$Significant, sig.col, bg.col)

  if (stat_params()[[ome]]$test == "Two-sample Moderated T-test"){
    group_contrast<- volcano_contrasts
  } else if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    group_contrast<- volcano_groups
  }
  ## Plot
  # Hover: always ID + the real gene-symbol column when present. When the
  # user-selected label column is neither the id column nor the detected
  # geneSymbol column, append a third line with the resolved label value so
  # the tooltip matches the on-plot annotation.
  is_id_col <- identical(tolower(lbl_col), tolower(id_col))
  is_gs_col <- !is.null(geneSymbol_col) && !is.na(geneSymbol_col) &&
               identical(tolower(lbl_col), tolower(geneSymbol_col))

  extra_lbl_vals <- if (!is_id_col && !is_gs_col) df$geneSymbol else NULL
  extra_lbl_name <- if (!is_id_col && !is_gs_col) lbl_col       else NULL

  df$.hover_text <- volcano_build_hover_text(
    df$id,
    gs_vals      = gs_for_hover,
    gs_col_name  = geneSymbol_col,
    lbl_vals     = extra_lbl_vals,
    lbl_col_name = extra_lbl_name
  )
  volcano <- ggplot(df, aes(x = .data$logFC, y = .data$logP,
                       text = .data$.hover_text)) +
    geom_point(aes(color = .data$point_color), size = 1) +
    scale_color_identity() +
    geom_hline(yintercept = y_cutoff, color = "black", linetype = "solid", linewidth = 0.5) +
    labs(title = paste("Volcano plot for",ome, ": ",group_contrast, "(cutoff:", stat_params()[[ome]]$cutoff, ")"), x = "log2 Fold Change", y = "-log10 Nom. p-value") +
    theme_minimal()
  

  if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    # The log fold change is now calculated as group1 - group2 (where group1 is the first group in contrast)
    # So positive logFC means higher expression in group1, negative logFC means higher expression in group2
    group1 <- groups[1]  # First group in contrast (right side of volcano plot for positive logFC)
    group2 <- groups[2]  # Second group in contrast (left side of volcano plot for negative logFC)
    x_range <- range(df$logFC, na.rm = TRUE)
    y_range <- range(df$logP, na.rm = TRUE)
    
    volcano <- volcano +
      annotate("text", x = x_range[1], y = y_range[2], label = group2, hjust = -0.1, vjust = 3.1, size = 5, fontface = "bold", color = "red") +
      annotate("text", x = x_range[2], y = y_range[2], label = group1, hjust = 1.1, vjust = 3.1, size = 5, fontface = "bold", color = "red")
  }
  
  # Add ggrepel labels for PDF export (mirrors add_volcano_labels color logic)
  if (length(label_mode) > 0) {
    label_df_gg <- data.frame(
      id = character(0), logFC = numeric(0), logP = numeric(0),
      label_txt = character(0), label_col = character(0),
      stringsAsFactors = FALSE
    )

    if ("significant" %in% label_mode) {
      sig_rows <- df[!is.na(df$Significant) & df$Significant == TRUE, , drop = FALSE]
    } else if ("significant_top20" %in% label_mode) {
      sig_rows <- volcano_label_top_significant_subset(df, n_top)
    } else {
      sig_rows <- df[FALSE, , drop = FALSE]
    }
    if (nrow(sig_rows) > 0) {
      label_df_gg <- rbind(label_df_gg, data.frame(
        id = sig_rows$id, logFC = sig_rows$logFC, logP = sig_rows$logP,
          label_txt = sig_rows$geneSymbol, label_col = .volcano_label_hex,
        stringsAsFactors = FALSE
      ))
    }

    if ("poi" %in% label_mode && length(label_proteins) > 0) {
      poi_rows <- df[df$id %in% label_proteins, ]
      if (nrow(poi_rows) > 0) {
        label_df_gg <- label_df_gg[!label_df_gg$id %in% poi_rows$id, ]
        label_df_gg <- rbind(label_df_gg, data.frame(
          id = poi_rows$id, logFC = poi_rows$logFC, logP = poi_rows$logP,
          label_txt = poi_rows$geneSymbol, label_col = .volcano_label_hex,
          stringsAsFactors = FALSE
        ))
      }
    }

    if (nrow(label_df_gg) > 0) {
      label_df_gg$label_txt <- volcano_maybe_display_trim(
        label_df_gg$label_txt, label_display_trim_enabled
      )
      label_df_gg$.hover_text <- df$.hover_text[match(as.character(label_df_gg$id), as.character(df$id))]
      volcano <- volcano +
        geom_point(
          data        = label_df_gg,
          aes(
            x     = .data$logFC,
            y     = .data$logP,
            color = .data$label_col,
            text  = .data$.hover_text
          ),
          inherit.aes = FALSE,
          size        = 2,
          show.legend = FALSE
        ) +
        ggrepel::geom_text_repel(
          data          = label_df_gg,
          aes(x = .data$logFC, y = .data$logP, label = .data$label_txt,
              color = .data$label_col),
          inherit.aes   = FALSE,
          size          = 3,
          max.overlaps  = 20,
          box.padding   = 0.35,
          point.padding = 0.3,
          segment.color = "grey50",
          show.legend   = FALSE,
          bg.color      = "white",
          bg.r          = 0.15
        )
    }
  }

  return(volcano)

}


################################################################################
# Feature search and volcano labeling helpers
################################################################################

# Escape all PCRE metacharacters so the string can be used as a literal
# pattern inside grep/grepl/sub/gsub with perl = TRUE.
# Note: \\ matches literal backslash; \] matches literal ] without closing the class.
regex_escape <- function(s) {
  gsub(r"(([.\\|()[\]{}^$*+?#]))", "\\\\\\1", s, perl = TRUE)
}

# Resolve the stat-result column names needed for a given test/group/contrast.
# Returns a named list: logfc_col, logp_col, adjp_col, pval_col, id_col, gs_col
# df: stat_results()[[ome]]
# test: one of "One-sample Moderated T-test" / "Two-sample Moderated T-test"
# volcano_groups: group name for one-sample test (or NULL)
# volcano_contrasts: contrast string "A / B" for two-sample test (or NULL)
get_volcano_cols <- function(df, test, volcano_groups, volcano_contrasts) {
  if (test == "One-sample Moderated T-test") {
    keyword    <- regex_escape(volcano_groups)
    logfc_col  <- grep(paste0("(?i)(?=.*", keyword, ")(?=.*logFC\\.)"),       colnames(df), value = TRUE, perl = TRUE)[1]
    logp_col   <- grep(paste0("(?i)(?=.*", keyword, ")(?=.*Log\\.P\\.Value\\.)"), colnames(df), value = TRUE, perl = TRUE)[1]
    adjp_col   <- grep(paste0("(?i)(?=.*", keyword, ")(?=.*adj\\.P\\.Val\\.)"),   colnames(df), value = TRUE, perl = TRUE)[1]
    pval_col   <- grep(paste0("(?i)(?=.*", keyword, ")(?=.*(?<!Log\\.)P\\.value\\.)"), colnames(df), value = TRUE, perl = TRUE)[1]
  } else {
    groups        <- unlist(strsplit(as.character(volcano_contrasts), " / "))
    contrast_name <- regex_escape(paste0(groups[1], "_over_", groups[2]))
    logfc_col  <- grep(paste0("logFC.*", contrast_name),          colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
    logp_col   <- grep(paste0("Log\\.P\\.Value.*", contrast_name), colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
    adjp_col   <- grep(paste0("adj\\.P\\.Val.*", contrast_name),   colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
    pval_col   <- grep(paste0("(?<!Log\\.)P\\.value.*", contrast_name), colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  }

  id_col <- grep("^id$", colnames(df), value = TRUE, ignore.case = TRUE)[1]
  gs_col <- tryCatch(
    grep("geneSymbol", colnames(df), value = TRUE, ignore.case = TRUE)[1],
    error = function(e) NA_character_
  )

  list(logfc = logfc_col, logp = logp_col, adjp = adjp_col,
       pval = pval_col, id = id_col, gs = gs_col)
}


# Build the standardized plotting data frame from raw stat_results for a given
# test/group/contrast. Returns df with columns: id, logFC, logP, adj.P.Val,
# P.Value, geneSymbol, Significant.
build_volcano_df <- function(df_raw, cols, sig_cutoff, sig_stat) {
  # Validate required columns exist
  required <- c(cols$logfc, cols$logp, cols$adjp, cols$pval, cols$id)
  missing  <- required[is.na(required) | !required %in% colnames(df_raw)]
  if (length(missing) > 0) {
    stop("Missing required volcano columns: ", paste(missing, collapse = ", "))
  }

  df <- df_raw[!is.na(df_raw[[cols$logp]]), ]
  df$id        <- as.character(df[[cols$id]])
  df$logFC     <- df[[cols$logfc]]
  df$logP      <- df[[cols$logp]]
  df$adj.P.Val <- as.numeric(df[[cols$adjp]])
  df$P.Value   <- as.numeric(df[[cols$pval]])
  df$geneSymbol <- if (!is.na(cols$gs) && cols$gs %in% colnames(df_raw)) df_raw[!is.na(df_raw[[cols$logp]]), cols$gs] else df$id

  # Compute significance threshold
  if (sig_stat == "adj.p.val") {
    passing_id <- which(df$adj.P.Val < sig_cutoff)
    y_cutoff <- if (length(passing_id) > 0) -log10(max(df$P.Value[passing_id], na.rm = TRUE)) else Inf
  } else {
    y_cutoff <- -log10(sig_cutoff)
  }
  df$Significant <- df$logP > y_cutoff

  attr(df, "y_cutoff") <- y_cutoff
  df
}

# Returns the feature ID closest to a plotly click event point.
# click: list with $x (logFC) and $y (logP) from event_data("plotly_click")
# df: data frame with columns logFC, logP, id
# tol: maximum Euclidean distance threshold (in data coordinates) to match
get_clicked_feature_id <- function(click, df, tol = 0.01) {
  if (nrow(df) == 0 || is.null(click$x) || is.null(click$y)) {
    return(NA_character_)
  }

  dx   <- abs(df$logFC - click$x)
  dy   <- abs(df$logP  - click$y)
  dist <- sqrt(dx^2 + dy^2)

  idx <- which.min(dist)

  # Reject if beyond tolerance (Euclidean distance threshold = tol * sqrt(2))
  if (dist[idx] > tol * sqrt(2)) {
    return(NA_character_)
  }

  as.character(df$id[idx])
}


# Resolve on-plot label text from a raw metadata column vector.
# For each element: if split_enabled is FALSE (or separator is empty), return
# as-is (coerced to character). If split_enabled is TRUE, split on separator
# (literal), trim whitespace, drop NA / empty / whitespace-only tokens, and
# return the first surviving token (NA_character_ if none survive).
# Returns a character vector of the same length as `values`.
resolve_volcano_label_text <- function(values, split_enabled = FALSE, separator = ";") {
  values <- as.character(values)
  if (!isTRUE(split_enabled) || is.null(separator) || !nzchar(separator)) {
    return(values)
  }
  vapply(values, function(v) {
    if (is.na(v)) return(NA_character_)
    tokens <- strsplit(v, separator, fixed = TRUE)[[1]]
    tokens <- trimws(tokens)
    tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
    if (length(tokens) == 0L) NA_character_ else tokens[[1L]]
  }, character(1L), USE.NAMES = FALSE)
}

# Shorten volcano plot / hover / sidebar display strings: `ProteinID_siteID_*`
# -> `ProteinID_siteID` using `protigy_legacy_protein_site_display_id()` (same
# ID detection as gene-symbol conversion), then cap length with an ellipsis.
# @noRd
volcano_display_trim <- function(x, max_chars = 56L) {
  x <- as.character(x)
  if (length(x) == 0L) return(x)
  mc <- as.integer(max_chars)[1L]
  if (is.na(mc) || mc < 12L) mc <- 56L

  s2 <- protigy_legacy_protein_site_display_id(x)
  for (i in seq_along(x)) {
    if (is.na(s2[i])) {
      x[i] <- NA_character_
      next
    }
    s <- s2[i]
    if (!nzchar(s)) {
      x[i] <- s
      next
    }
    if (nchar(s) > mc) {
      s <- paste0(substr(s, 1L, mc - 1L), "\u2026")
    }
    x[i] <- s
  }
  x
}

# Apply `volcano_display_trim()` when `trim_enabled` is TRUE; otherwise pass-through.
# @noRd
volcano_maybe_display_trim <- function(x, trim_enabled) {
  if (isTRUE(trim_enabled)) volcano_display_trim(x) else as.character(x)
}

# Tokenize a raw search string into a character vector of feature IDs.
# Accepts space (including newlines/tabs), comma, and semicolon as delimiters.
# Drops empty tokens.
parse_protein_search_input <- function(raw) {
  if (is.null(raw) || nchar(trimws(raw)) == 0) {
    return(character(0))
  }
  tokens <- unlist(strsplit(raw, "[,;\\s]+", perl = TRUE))
  tokens <- trimws(tokens)
  tokens[nchar(tokens) > 0]
}

# Among Significant rows with non-NA logP, keep up to n: sort by decreasing logP,
# then decreasing abs(logFC). Ties on (logP, abs(logFC)): keep all matching rows
# at the cutoff (may exceed n).
# @noRd
volcano_label_top_significant_subset <- function(df, n = 20L) {
  n <- as.integer(n)[1L]
  if (is.na(n) || n < 1L || nrow(df) == 0L) {
    return(df[FALSE, , drop = FALSE])
  }
  sig <- df[!is.na(df$Significant) & df$Significant == TRUE, , drop = FALSE]
  if (nrow(sig) == 0L) {
    return(sig)
  }
  sig <- sig[!is.na(sig$logP), , drop = FALSE]
  if (nrow(sig) == 0L) {
    return(sig)
  }
  ord <- order(sig$logP, abs(sig$logFC), decreasing = c(TRUE, TRUE), na.last = TRUE)
  sig <- sig[ord, , drop = FALSE]
  abs_fc <- abs(sig$logFC)
  if (nrow(sig) <= n) {
    return(sig)
  }
  p_n <- sig$logP[n]
  abs_n <- abs_fc[n]
  lp <- sig$logP
  lf <- sig$logFC
  abs_lf <- abs(lf)
  take <- if (is.na(abs_n)) {
    lp > p_n | (lp == p_n & is.na(lf))
  } else {
    lp > p_n | (lp == p_n & !is.na(abs_lf) & abs_lf >= abs_n)
  }
  sig[take, , drop = FALSE]
}

#' Feature IDs that receive volcano labels for one group/contrast (mirrors `add_volcano_labels`).
#'
#' @param df_plot Output of `build_volcano_df()` (columns include `id`, `Significant`, ...).
#' @param label_mode Character vector; may include `"poi"`, `"significant_top20"`, `"significant"`.
#' @param poi Character vector of manually selected POI feature IDs.
#' @param n_top Integer; how many top significant features to label when `"significant_top20"` is active. Default 20.
#' @return `character()` of unique feature IDs (empty if nothing would be labeled).
#' @noRd
volcano_labeled_feature_ids <- function(df_plot, label_mode, poi, n_top = 20L) {
  if (is.null(label_mode) || length(label_mode) == 0) {
    label_mode <- character(0)
  }
  poi <- unique(as.character(poi))
  show_poi <- "poi" %in% label_mode
  show_sig <- "significant" %in% label_mode
  show_sig_top <- "significant_top20" %in% label_mode

  if (!show_poi && !show_sig && !show_sig_top) {
    return(character(0))
  }

  sig_ids <- character(0)
  if (show_sig) {
    sig_rows <- df_plot[!is.na(df_plot$Significant) & df_plot$Significant == TRUE, , drop = FALSE]
    sig_ids <- as.character(sig_rows$id)
  } else if (show_sig_top) {
    sig_rows <- volcano_label_top_significant_subset(df_plot, n_top)
    sig_ids <- as.character(sig_rows$id)
  }

  poi_ids <- character(0)
  if (show_poi && length(poi) > 0) {
    poi_ids <- as.character(df_plot$id[as.character(df_plot$id) %in% poi])
  }

  unique(c(sig_ids, poi_ids))
}


#' Compute the union of labeled feature IDs across all contrasts/groups for one ome.
#'
#' For each group (one-sample) or contrast (two-sample) defined in \code{stat_params_ome},
#' calls \code{volcano_labeled_feature_ids()} and returns the union of all results.
#' Used by the "label across contrasts" feature to build a consistent label set.
#'
#' @param stat_results_ome Data frame; \code{stat_results()[[ome]]}.
#' @param stat_params_ome  Named list; \code{stat_params()[[ome]]}.
#' @param label_mode Character vector; active label modes (e.g. \code{"poi"}, \code{"significant_top20"}).
#' @param poi Character vector of manually selected feature IDs.
#' @param n_top Integer; how many top significant features to label when `"significant_top20"` is active. Default 20.
#' @return \code{character()} of unique feature IDs (empty if nothing would be labeled).
#' @noRd
volcano_label_union_for_ome <- function(stat_results_ome, stat_params_ome, label_mode, poi, n_top = 20L) {
  if (is.null(stat_results_ome) || nrow(stat_results_ome) == 0) return(character(0))
  if (is.null(stat_params_ome)) return(character(0))

  test       <- stat_params_ome$test
  sig_cutoff <- stat_params_ome$cutoff
  sig_stat   <- stat_params_ome$stat

  if (is.null(test) || test == "None" || test == "Moderated F test") return(character(0))

  all_ids <- character(0)

  if (test == "One-sample Moderated T-test") {
    groups <- stat_params_ome$groups
    for (group in groups) {
      cols <- tryCatch(
        get_volcano_cols(stat_results_ome, test, group, NULL),
        error = function(e) NULL
      )
      if (is.null(cols)) next
      df_plot <- tryCatch(
        build_volcano_df(stat_results_ome, cols, sig_cutoff, sig_stat),
        error = function(e) NULL
      )
      if (is.null(df_plot)) next
      all_ids <- union(all_ids, volcano_labeled_feature_ids(df_plot, label_mode, poi, n_top))
    }
  } else if (test == "Two-sample Moderated T-test") {
    contrasts <- stat_params_ome$contrasts
    for (contrast in contrasts) {
      cols <- tryCatch(
        get_volcano_cols(stat_results_ome, test, NULL, contrast),
        error = function(e) NULL
      )
      if (is.null(cols)) next
      df_plot <- tryCatch(
        build_volcano_df(stat_results_ome, cols, sig_cutoff, sig_stat),
        error = function(e) NULL
      )
      if (is.null(df_plot)) next
      all_ids <- union(all_ids, volcano_labeled_feature_ids(df_plot, label_mode, poi, n_top))
    }
  }

  all_ids
}


# Add color-coded feature labels as Plotly annotations.
#
# p               - plotly object (output of ggplotly)
# df              - data frame with columns: id, logFC, logP, Significant, geneSymbol
# poi             - character vector of feature IDs to label as POI
# label_mode      - character vector; "poi", "significant_top20", and/or "significant"
#                 ("significant" includes all sig; if both sig modes are set, all wins)
# y_cutoff        - significance y threshold (used to identify Significant points)
# hidden_count_rv - reactiveVal or mock_rv; updated with count of hidden labels
# min_dist        - minimum normalized distance between labels (0 to 1 scale)
add_volcano_labels <- function(p, df, poi, label_mode, y_cutoff,
                                hidden_count_rv, min_dist = 0.04,
                                label_display_trim_enabled = FALSE,
                                n_top = 20L) {

  show_poi <- "poi" %in% label_mode
  show_sig <- "significant" %in% label_mode
  show_sig_top <- "significant_top20" %in% label_mode

  if (!show_poi && !show_sig && !show_sig_top) {
    hidden_count_rv(0L)
    return(p)
  }

  # Build label data frame
  label_df <- data.frame(
    id        = character(0),
    logFC     = numeric(0),
    logP      = numeric(0),
    label_col = character(0),
    label_txt = character(0),
    stringsAsFactors = FALSE
  )

  if (show_sig) {
    sig_rows <- df[!is.na(df$Significant) & df$Significant == TRUE, , drop = FALSE]
  } else if (show_sig_top) {
    sig_rows <- volcano_label_top_significant_subset(df, n_top)
  } else {
    sig_rows <- df[FALSE, , drop = FALSE]
  }
  if (nrow(sig_rows) > 0) {
    label_df <- rbind(label_df, data.frame(
      id        = sig_rows$id,
      logFC     = sig_rows$logFC,
      logP      = sig_rows$logP,
        label_col = .volcano_label_hex,
      label_txt = if (!is.null(sig_rows$geneSymbol)) sig_rows$geneSymbol else sig_rows$id,
      stringsAsFactors = FALSE
    ))
  }

  if (show_poi && length(poi) > 0) {
    poi_rows <- df[df$id %in% poi, ]
    if (nrow(poi_rows) > 0) {
      # POI takes priority  -  remove any existing sig entries for the same IDs
      label_df <- label_df[!label_df$id %in% poi_rows$id, ]
      label_df <- rbind(label_df, data.frame(
        id        = poi_rows$id,
        logFC     = poi_rows$logFC,
        logP      = poi_rows$logP,
        label_col = .volcano_label_hex,
        label_txt = if (!is.null(poi_rows$geneSymbol)) poi_rows$geneSymbol else poi_rows$id,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(label_df) == 0) {
    hidden_count_rv(0L)
    return(p)
  }

  label_df$label_txt <- volcano_maybe_display_trim(
    label_df$label_txt, label_display_trim_enabled
  )

  # Placement order: features of interest first (user explicitly chose them), then
  # by significance. Tie-breaker keeps rows stable. Delimiter-splitting only affects
  # label text, not (logFC, logP) positions  -  it does not change overlap among points.
  poi <- unique(as.character(poi))
  is_poi_row <- label_df$id %in% poi
  ord <- order(!is_poi_row, -label_df$logP, label_df$id)
  label_df <- label_df[ord, , drop = FALSE]

  # Normalize coordinates for overlap detection
  x_range <- range(df$logFC, na.rm = TRUE)
  y_range <- range(df$logP,  na.rm = TRUE)
  x_span  <- diff(x_range)
  y_span  <- diff(y_range)
  if (x_span == 0) x_span <- 1
  if (y_span == 0) y_span <- 1

  placed   <- list()
  hidden   <- 0L
  keep_idx <- integer(0)

  for (i in seq_len(nrow(label_df))) {
    nx <- (label_df$logFC[i] - x_range[1]) / x_span
    ny <- (label_df$logP[i]  - y_range[1]) / y_span

    too_close <- FALSE
    for (pl in placed) {
      if (sqrt((nx - pl$nx)^2 + (ny - pl$ny)^2) < min_dist) {
        too_close <- TRUE
        break
      }
    }

    if (too_close) {
      hidden <- hidden + 1L
    } else {
      placed   <- c(placed, list(list(nx = nx, ny = ny)))
      keep_idx <- c(keep_idx, i)
    }
  }

  hidden_count_rv(hidden)

  label_df_kept <- label_df[keep_idx, , drop = FALSE]
  if (nrow(label_df_kept) == 0) return(p)

  # Colored marker overlay for labeled points
  p <- plotly::add_trace(
    p,
    data       = label_df_kept,
    x          = ~logFC,
    y          = ~logP,
    type       = "scatter",
    mode       = "markers",
    marker     = list(color = label_df_kept$label_col, size = 9,
                      line = list(color = "white", width = 1.5)),
    showlegend = FALSE,
    hoverinfo  = "skip",
    inherit    = FALSE
  )

  # White-background text annotations (no arrow)
  annotations <- lapply(seq_len(nrow(label_df_kept)), function(i) {
    list(
      x           = label_df_kept$logFC[i],
      y           = label_df_kept$logP[i],
      text        = label_df_kept$label_txt[i],
      font        = list(color = label_df_kept$label_col[i], size = 11,
                         family = "Arial"),
      showarrow   = FALSE,
      xanchor     = "left",
      yanchor     = "bottom",
      xshift      = 6,
      yshift      = 4,
      bgcolor     = "rgba(255,255,255,0.85)",
      bordercolor = label_df_kept$label_col[i],
      borderwidth = 1,
      borderpad   = 2
    )
  })

  plotly::layout(p, annotations = annotations)
}
# Apply the WebGL render backend to a built Statistics-volcano plotly object,
# guarded by the client's WebGL capability. toWebGL converts the SVG scatter
# traces (base points + label markers) to scattergl so pan/zoom/hover stay
# smooth on tens of thousands of points. WebGL renders in the USER's browser,
# so when the client reports no WebGL context we MUST leave the plot as SVG
# scatter -- scattergl would otherwise paint a blank cloud (plotly does not
# auto-fall-back). The conversion is also tryCatch-guarded so a toWebGL error on
# a given object never crashes the render, and the benign `hoveron` warning
# (ggplotly sets it; scattergl drops it) is muffled while any other warning still
# surfaces.
#
# @param p         a built plotly object (typically ggplotly + add_volcano_labels).
# @param use_webgl logical; when FALSE, return p unchanged (SVG fallback).
# @return p converted to WebGL when capable, else the original p.
# @noRd
stat_volcano_apply_webgl <- function(p, use_webgl = TRUE) {
  if (!isTRUE(use_webgl)) return(p)
  tryCatch(
    withCallingHandlers(
      plotly::toWebGL(p),
      warning = function(w) {
        if (grepl("hoveron", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      message("toWebGL conversion failed, falling back to SVG: ",
              conditionMessage(e))
      p
    }
  )
}
