################################################################################
# PELSA export framework helpers (pure; no Shiny).
#
# The global exporter (tab_export.R) hands each module a directory
# (<ome>/<tab_name>/) and calls its export functions with that path. PELSA's
# three section servers are merged under a single "pelsa_exports" tab in
# app_server(), so the handed dir is <ome>/pelsa_exports/. Each PELSA export
# function then carves its own stage subfolder inside it via
# pelsa_export_stage_dir() - the generic loop in tab_export.R is never touched.
#
# Helpers:
#   pelsa_export_stage_dir(dir_name, ...)  -> create + return a nested subfolder
#   pelsa_save_figure(plot, dir, basename) -> write a ggplot as PNG (ragg); PDF
#                                             retained but gated off by default
#   pelsa_safe_name(x)                     -> sanitize a filename token
#   pelsa_export_add_any_contrast(stat_df) -> add adj.P.Val.<ANY> = min across
#                                             contrasts (drives the union set)
#   pelsa_export_gene_for(matched, acc)    -> most-common gene token for acc
#   pelsa_export_prot_len(coverage, acc, peptides) -> protein length for acc
################################################################################

# Create (recursively, idempotently) the nested subfolder dir_name/<...>/ and
# return its path. This is what produces the pelsa/<stage>/... tree without
# touching the global export loop.
# @noRd
pelsa_export_stage_dir <- function(dir_name, ...) {
  path <- do.call(file.path, c(list(dir_name), list(...)))
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

# Write ONE ggplot as PNG (via the ragg AGG device - the project's deterministic,
# high-quality raster device) into dir_name. Sizes are in inches; every figure is
# rasterized at .PELSA_EXPORT_DPI. PDF output is retained but gated OFF by default
# (`pdf = .PELSA_EXPORT_PDF`); set the flag TRUE to also emit a vector PDF. Returns
# the written path(s) invisibly.
# @noRd
pelsa_save_figure <- function(plot, dir_name, basename, width = 9, height = 5,
                              dpi = .PELSA_EXPORT_DPI, pdf = .PELSA_EXPORT_PDF) {
  png_path <- file.path(dir_name, paste0(basename, ".png"))
  ggplot2::ggsave(png_path, plot, device = ragg::agg_png,
                  width = width, height = height, units = "in", dpi = dpi)
  paths <- png_path
  # PDF export is kept for future demand; disabled unless the flag is TRUE.
  if (isTRUE(pdf)) {
    pdf_path <- file.path(dir_name, paste0(basename, ".pdf"))
    ggplot2::ggsave(pdf_path, plot, device = "pdf",
                    width = width, height = height, units = "in")
    paths <- c(pdf_path, png_path)
  }
  invisible(paths)
}

# Sanitize a gene/accession/contrast token for use in a filename: keep
# [A-Za-z0-9._-], collapse the rest to "_", and never return empty.
# @noRd
pelsa_safe_name <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "unknown"
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x[!nzchar(x)] <- "unknown"
  x
}

# Add a synthetic adj.P.Val.<.PELSA_ANY_CONTRAST> column = the row-wise MINIMUM
# of every adj.P.Val.<contrast> column (na.rm). min < cutoff iff the peptide is
# significant in ANY contrast, so the per-contrast helpers, called with this one
# key, yield the contrast-independent union set + panel split. Returns stat_df
# unchanged when it has no adj.P.Val.* columns.
# @noRd
pelsa_export_add_any_contrast <- function(stat_df) {
  if (!is.data.frame(stat_df) || nrow(stat_df) == 0L) return(stat_df)
  adjp_cols <- grep("^adj\\.P\\.Val\\.", colnames(stat_df), value = TRUE)
  if (length(adjp_cols) == 0L) return(stat_df)
  m <- as.matrix(stat_df[, adjp_cols, drop = FALSE])
  storage.mode(m) <- "double"
  any_min <- apply(m, 1L, function(r) {
    r <- r[is.finite(r)]
    if (length(r) == 0L) NA_real_ else min(r)
  })
  stat_df[[paste0("adj.P.Val.", .PELSA_ANY_CONTRAST)]] <- any_min
  stat_df
}

# Most-common non-empty gene token among the matched rows for `acc` (falls back
# to "" so the caller uses the accession). @noRd
pelsa_export_gene_for <- function(matched, acc) {
  if (!is.data.frame(matched) || !"gene" %in% colnames(matched) ||
      !"accession" %in% colnames(matched)) {
    return("")
  }
  g <- as.character(matched$gene[as.character(matched$accession) == acc])
  g <- g[!is.na(g) & nzchar(g)]
  if (length(g) == 0L) return("")
  tt <- sort(table(g), decreasing = TRUE)
  names(tt)[1L]
}

# Protein length for `acc`: the FASTA-resolved length from the coverage frame,
# else the largest mapped peptide end, else 1. @noRd
pelsa_export_prot_len <- function(coverage, acc, peptides = NULL) {
  plen <- NA_integer_
  if (is.data.frame(coverage) &&
      all(c("accession", "protein_length") %in% colnames(coverage))) {
    idx <- which(as.character(coverage$accession) == acc)
    if (length(idx) > 0L) plen <- as.integer(coverage$protein_length[idx[1L]])
  }
  if ((is.na(plen) || plen < 1L) && is.data.frame(peptides) &&
      "pep_end" %in% colnames(peptides) && nrow(peptides) > 0L) {
    plen <- max(as.integer(peptides$pep_end), na.rm = TRUE)
  }
  if (is.na(plen) || plen < 1L) 1L else plen
}
