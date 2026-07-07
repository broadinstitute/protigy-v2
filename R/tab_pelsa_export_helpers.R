################################################################################
# PELSA export framework helpers (pure; no Shiny).
#
# The global exporter (tab_export.R) hands each module a directory
# (<ome>/<tab_name>/) and calls its export functions with that path. PELSA's
# three section servers are merged under a single "PELSA_exports" tab in
# app_server(), so the handed dir is <ome>/PELSA_exports/. Each PELSA export
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

# Copy a dataset's uploaded FASTA + annotation file (verbatim) into dir_name and
# write missing_accessions.txt (dataset accessions absent from the annotation
# file = the "failed to resolve annotation" set). Files are written under their
# ORIGINAL upload names (the Shiny datapath is a mangled temp name); falls back
# to basename(path) when no name is given. Self-curated datasets pass
# annotation_path = NULL (FASTA + an empty missing list only). Returns the
# written paths invisibly.
# @noRd
pelsa_export_input_files <- function(dir_name, fasta_path, annotation_path,
                                     missing_accessions,
                                     fasta_name = NULL, annotation_name = NULL) {
  written <- character(0)
  # Reserve "missing_accessions.txt" up front and track every destination
  # basename used so far, so a colliding upload name (matching that reserved
  # name, or matching the OTHER uploaded file's name) gets a de-duplicating
  # suffix instead of silently overwriting an already-copied input file.
  used_names <- "missing_accessions.txt"
  dedupe_name <- function(safe_base) {
    if (!(safe_base %in% used_names)) return(safe_base)
    ext <- sub("^.*(\\.[^.]*)$", "\\1", safe_base)
    has_ext <- nzchar(ext) && ext != safe_base
    stem <- if (has_ext) substr(safe_base, 1, nchar(safe_base) - nchar(ext)) else safe_base
    suffix <- if (has_ext) ext else ""
    i <- 1L
    repeat {
      candidate <- paste0(stem, "_", i, suffix)
      if (!(candidate %in% used_names)) return(candidate)
      i <- i + 1L
    }
  }
  copy_one <- function(path, name) {
    if (is.null(path) || !nzchar(path %||% "") || !file.exists(path)) return(NULL)
    # basename() the upload name so a crafted filename (e.g. "../../evil") cannot
    # steer file.copy outside dir_name (path traversal). The browser normally
    # sends a bare basename, but never trust it -- strip any directory component.
    # basename() alone is not enough: basename("..") == ".." (and "." == "."),
    # which still resolves to dir_name's parent/self via file.copy. It also does
    # NOT treat "\\" as a separator on a POSIX host, so a crafted name like
    # "C:\\evil\\x.txt" passes through whole. Normalize backslashes to "/" before
    # basename(), then collapse any remaining unsafe characters via
    # pelsa_safe_name() -- it keeps "." in its allowed charset, so it will NOT
    # sanitize a pure-dot name -- reject "." / ".." explicitly instead of relying
    # on it here.
    raw_name <- name %||% basename(path)
    safe_base <- basename(gsub("\\\\", "/", raw_name))
    safe_base <- pelsa_safe_name(safe_base)
    if (safe_base %in% c("..", ".")) safe_base <- "unknown"
    safe_base <- dedupe_name(safe_base)
    used_names <<- c(used_names, safe_base)
    dest <- file.path(dir_name, safe_base)
    file.copy(path, dest, overwrite = TRUE)
    dest
  }
  written <- c(written, copy_one(fasta_path, fasta_name))
  written <- c(written, copy_one(annotation_path, annotation_name))
  miss_path <- file.path(dir_name, "missing_accessions.txt")
  writeLines(as.character(missing_accessions %||% character(0)), miss_path)
  invisible(c(written, miss_path))
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
    # Filter NAs BEFORE max() -- max(all-NA, na.rm = TRUE) warns
    # ("no non-missing arguments to max; returning -Inf"). pep_end can be all-NA
    # for older caches lacking span columns; fall through to the 1L default.
    ends <- as.integer(peptides$pep_end)
    ends <- ends[!is.na(ends)]
    if (length(ends) > 0L) plen <- max(ends)
  }
  if (is.na(plen) || plen < 1L) 1L else plen
}
