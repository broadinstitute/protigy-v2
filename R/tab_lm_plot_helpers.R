################################################################################
# Module: LM_Plot Helpers
#
# Volcano plot function for the Linear Model module.
# Reuses build_volcano_df() and add_volcano_labels() from tab_stat_plot_helpers.R
# for feature parity with the Statistics module (labeling, POI, top-20).
################################################################################

#' Build a standardized volcano data frame from LM results for one coefficient.
#'
#' Wraps build_volcano_df() with LM-specific column detection.
#'
#' @param ome Character, the ome name
#' @param coefficient Character, the coefficient name (may contain ":" for interactions)
#' @param df Data.frame of LM results for this ome
#' @param lm_params List of LM parameters (from reactiveVal)
#' @param gene_symbol_col Column name for gene symbols (default "geneSymbol")
#' @return build_volcano_df() result (data frame with y_cutoff attribute), or NULL on error
build_lm_volcano_df <- function(ome, coefficient, df, lm_params,
                                gene_symbol_col = "geneSymbol") {
  safe_coef <- make.names(coefficient)

  logfc_pattern <- paste0("^logFC\\.", gsub("\\.", "\\\\.", safe_coef), "$")
  adjP_pattern  <- paste0("^adj\\.P\\.Val\\.", gsub("\\.", "\\\\.", safe_coef), "$")
  pval_pattern  <- paste0("^P\\.Value\\.", gsub("\\.", "\\\\.", safe_coef), "$")

  logFC_col <- grep(logfc_pattern, colnames(df), value = TRUE, perl = TRUE)[1]
  adjP_col  <- grep(adjP_pattern,  colnames(df), value = TRUE, perl = TRUE)[1]
  pval_col  <- grep(pval_pattern,  colnames(df), value = TRUE, perl = TRUE)[1]

  if (!"id" %in% colnames(df)) df$id <- rownames(df)

  gs_col <- tryCatch({
    matches <- colnames(df)[tolower(colnames(df)) == tolower(gene_symbol_col)]
    if (length(matches) > 0) matches[1] else NA_character_
  }, error = function(e) NA_character_)

  if (is.na(logFC_col) || is.na(adjP_col) || is.na(pval_col)) {
    return(NULL)
  }

  # LM results store -log10(P.Value) as logP so compute it from P.Value directly
  df$lm_logP_tmp__ <- -log10(pmax(as.numeric(df[[pval_col]]), .Machine$double.xmin))
  logp_tmp_col <- "lm_logP_tmp__"

  sig_cutoff <- lm_params[[ome]]$cutoff %||% 0.05
  sig_stat   <- lm_params[[ome]]$stat   %||% "adj.p.val"
  sig_stat   <- if (sig_stat == "adj.p.val") "adj.p.val" else "p.val"

  cols <- list(
    logfc = logFC_col,
    logp  = logp_tmp_col,
    adjp  = adjP_col,
    pval  = pval_col,
    id    = "id",
    gs    = if (!is.na(gs_col)) gs_col else NA_character_
  )

  tryCatch(
    build_volcano_df(df, cols, sig_cutoff = sig_cutoff, sig_stat = sig_stat),
    error = function(e) NULL
  )
}


#' Create a volcano plot for a specific coefficient from LM results.
#'
#' Returns a ggplot object. For interactive plotly labeling, call
#' build_lm_volcano_df() + add_volcano_labels() in the server.
#'
#' @param ome Character, the ome name
#' @param coefficient Character, the coefficient name
#' @param df Data.frame of LM results for this ome
#' @param lm_params List of LM parameters (from reactiveVal)
#' @param sig.col Color for significant points (default "darkred")
#' @param bg.col Color for non-significant points (default "gray")
#' @param gene_symbol_col Column name for gene symbols (default "geneSymbol")
#' @param label_proteins Character vector of feature IDs to label (for PDF exports)
#' @param label_mode Character vector; "poi", "significant_top20", and/or "significant"
#' @return A ggplot object
plotLmVolcano <- function(ome,
                          coefficient,
                          df,
                          lm_params,
                          sig.col = "darkred",
                          bg.col = "gray",
                          gene_symbol_col = "geneSymbol",
                          label_proteins = character(0),
                          label_mode = character(0)) {

  df_plot <- build_lm_volcano_df(ome, coefficient, df, lm_params, gene_symbol_col)

  if (is.null(df_plot)) {
    return(
      ggplot() +
        labs(title = paste("Missing columns for coefficient:", coefficient)) +
        theme_minimal()
    )
  }

  y_cutoff <- attr(df_plot, "y_cutoff")
  sig_cutoff <- lm_params[[ome]]$cutoff %||% 0.05

  df_plot$point_color <- ifelse(df_plot$Significant, sig.col, bg.col)

  volcano <- ggplot(
    df_plot,
    aes(x = .data$logFC, y = .data$logP,
        text = paste("ID:", .data$id, "<br>Gene Symbol:", .data$geneSymbol))
  ) +
    geom_point(aes(color = .data$point_color), size = 1) +
    scale_color_identity() +
    labs(
      title = paste("Volcano plot for", ome, ":", coefficient,
                    "(cutoff:", sig_cutoff, ")"),
      x = "log2 Fold Change / Effect Size",
      y = "-log10 Nom. p-value"
    ) +
    theme_minimal()

  if (!is.null(y_cutoff) && is.finite(y_cutoff)) {
    volcano <- volcano +
      geom_hline(yintercept = y_cutoff, color = "black",
                 linetype = "solid", linewidth = 0.5)
  }

  # Add ggrepel labels for PDF export (mirrors plotVolcano label logic)
  if (length(label_mode) > 0) {
    label_df_gg <- data.frame(
      id = character(0), logFC = numeric(0), logP = numeric(0),
      label_txt = character(0), label_col = character(0),
      stringsAsFactors = FALSE
    )

    if ("significant" %in% label_mode) {
      sig_rows <- df_plot[!is.na(df_plot$Significant) & df_plot$Significant == TRUE, , drop = FALSE]
    } else if ("significant_top20" %in% label_mode) {
      sig_rows <- volcano_label_top_significant_subset(df_plot, 20L)
    } else {
      sig_rows <- df_plot[FALSE, , drop = FALSE]
    }

    if (nrow(sig_rows) > 0) {
      label_df_gg <- rbind(label_df_gg, data.frame(
        id = sig_rows$id, logFC = sig_rows$logFC, logP = sig_rows$logP,
        label_txt = sig_rows$geneSymbol, label_col = .volcano_label_hex,
        stringsAsFactors = FALSE
      ))
    }

    if ("poi" %in% label_mode && length(label_proteins) > 0) {
      poi_rows <- df_plot[df_plot$id %in% label_proteins, , drop = FALSE]
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
      volcano <- volcano +
        geom_point(
          data = label_df_gg,
          aes(x = .data$logFC, y = .data$logP, color = .data$label_col),
          inherit.aes = FALSE, size = 2, show.legend = FALSE
        ) +
        ggrepel::geom_text_repel(
          data = label_df_gg,
          aes(x = .data$logFC, y = .data$logP,
              label = .data$label_txt, color = .data$label_col),
          inherit.aes = FALSE, size = 3, max.overlaps = 20,
          box.padding = 0.35, point.padding = 0.3,
          segment.color = "grey50", show.legend = FALSE,
          bg.color = "white", bg.r = 0.15
        )
    }
  }

  volcano
}
