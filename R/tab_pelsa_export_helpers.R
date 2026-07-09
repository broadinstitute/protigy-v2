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

# Dynamic export width (inches) for a QC bar figure with `n_bars` bars: grow
# ~0.6in per bar, but never below the 5.6in floor (keeps small datasets looking
# exactly as they do today) and never above a 30in ceiling (bounds the PNG size
# for pathological sample counts). Non-positive / NA / non-integer input degrades
# to the floor. @noRd
pelsa_bar_export_width <- function(n_bars) {
  n <- suppressWarnings(as.integer(n_bars))
  if (length(n) != 1L || is.na(n)) n <- 1L
  n <- max(1L, n)
  min(30, max(5.6, 0.6 * n))
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

# Cap the per-export protein set to at most `cap` proteins for rendering.
# ALL marker proteins are kept; remaining slots are filled with the most-
# significant non-marker proteins (smallest per-protein ANY-contrast adj.P).
# Returns list(keep = <prot subset>, skipped = <data.frame(accession, adj.P)>);
# callers add a `gene` column before writing the manifest.
#
# Per-protein adj.P is derived from `stat_any`'s adj.P.Val.<.PELSA_ANY_CONTRAST>
# column. That frame is PEPTIDE-LEVEL and keyed by `.row_id` / PEP.StrippedSequence
# (NO `accession` column), so the peptide adj.P is joined onto accessions THROUGH
# `matched` (which carries accession + the same join keys) -- the same join
# pelsa_intensity_proteins uses. If `stat_any` itself already carries an
# `accession` column (e.g. a pre-joined test frame), that direct path is used
# instead. When nrow(prot) <= cap, keep = prot and skipped has 0 rows.
# @noRd
pelsa_export_cap_proteins <- function(prot, stat_any, matched = NULL,
                                      cap = .PELSA_EXPORT_FIGURE_CAP) {
  empty_skipped <- data.frame(accession = character(0), adj.P = numeric(0),
                              stringsAsFactors = FALSE)
  if (!is.data.frame(prot) || nrow(prot) <= cap) {
    return(list(keep = prot, skipped = empty_skipped))
  }
  any_col <- paste0("adj.P.Val.", .PELSA_ANY_CONTRAST)
  # Per-accession min ANY-contrast adj.P (finite only; missing -> Inf, sorts last).
  agg <- NULL
  if (is.data.frame(stat_any) && any_col %in% colnames(stat_any)) {
    p_s <- suppressWarnings(as.numeric(stat_any[[any_col]]))
    if ("accession" %in% colnames(stat_any)) {
      # Direct path: stat_any already carries accession (pre-joined frame).
      acc_s <- as.character(stat_any[["accession"]])
    } else if (is.data.frame(matched) && "accession" %in% colnames(matched)) {
      # Join path: map each matched row's accession to its peptide's adj.P via
      # the .row_id (else PEP.StrippedSequence) key shared by both frames.
      use_row_id <- ".row_id" %in% colnames(stat_any) &&
        ".row_id" %in% colnames(matched)
      if (use_row_id) {
        key_s <- stat_any[[".row_id"]]; key_m <- matched[[".row_id"]]
      } else if ("PEP.StrippedSequence" %in% colnames(stat_any) &&
                 "PEP.StrippedSequence" %in% colnames(matched)) {
        key_s <- as.character(stat_any[["PEP.StrippedSequence"]])
        key_m <- as.character(matched[["PEP.StrippedSequence"]])
      } else {
        key_s <- NULL
      }
      if (!is.null(key_s)) {
        acc_s <- as.character(matched[["accession"]])
        p_s   <- p_s[match(key_m, key_s)]  # peptide adj.P aligned to matched rows
      } else {
        acc_s <- NULL
      }
    } else {
      acc_s <- NULL
    }
    if (!is.null(acc_s)) {
      agg <- tapply(p_s, acc_s, FUN = function(v) {
        v <- v[is.finite(v)]
        if (length(v) == 0L) Inf else min(v)
      })
    }
  }
  padj <- rep(Inf, nrow(prot))
  if (!is.null(agg)) {
    m <- agg[as.character(prot$accession)]
    # unname(): a prot accession absent from `agg` returns an NA-NAMED element
    # (single-bracket `[` on a named vector), and that NA name would survive into
    # `data.frame(adj.P = padj[skip_idx])` below and abort with "row names contain
    # missing values". Strip names so a genuinely-missing accession degrades to
    # Inf (sorts last) as documented, instead of crashing.
    padj <- unname(ifelse(is.na(m), Inf, as.numeric(m)))
  }
  is_mk <- as.logical(prot$is_marker)
  is_mk[is.na(is_mk)] <- FALSE
  n_mk <- sum(is_mk)
  nonmk_idx <- which(!is_mk)
  nonmk_order <- nonmk_idx[order(padj[nonmk_idx])]
  slots_left <- max(0L, cap - n_mk)
  keep_nonmk <- utils::head(nonmk_order, slots_left)
  keep_idx <- sort(c(which(is_mk), keep_nonmk))
  skip_idx <- setdiff(seq_len(nrow(prot)), keep_idx)
  skipped <- data.frame(
    accession = as.character(prot$accession[skip_idx]),
    adj.P = padj[skip_idx],
    stringsAsFactors = FALSE)
  list(keep = prot[keep_idx, , drop = FALSE], skipped = skipped)
}

################################################################################
# PELSA Section 3 (volcano tab) per-ome export bodies.
#
# Extracted VERBATIM from PELSASection3_Ome_Server's export_volcano /
# export_intensity / export_woods closures (that function was ~1458 lines,
# driving the file over the 800-line cap -- see CLAUDE.md coding-style rules).
# Each function takes its former closure captures as EXPLICIT arguments so it
# is a plain, independently-testable function; the module server now just
# gathers the current reactive values and calls it. Behavior is unchanged.
################################################################################

# Body of export_volcano: one volcano PNG per contrast (all-peptide + optional
# best-peptide), sharing one x/y coord_cartesian range across every figure in
# this ome's export so any two exported figures are visually comparable.
# @noRd
pelsa_section3_export_volcano <- function(dir_name, ome, stat_results, cache_entry,
                                          feat_df, marker_accessions, color_mode,
                                          label_mode, n_top_adjp, n_top_markers,
                                          want_best, sig_cutoff, sig_stat,
                                          self_curated, contrast_choices) {
  out <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_VOLCANO,
                                .PELSA_SUB_VOLCANO)
  sr <- stat_results[[ome]]
  entry <- cache_entry
  matched <- if (is.null(entry)) NULL else entry$matched
  fdf <- feat_df
  markers <- marker_accessions
  choices <- contrast_choices
  # Precompute the contrast-invariant stat frame ONCE (its inputs sr/matched
  # are fixed for this export). Guarded so a malformed/empty sr falls back to
  # the per-call path inside pelsa_volcano_export_df (which safe_export's
  # outer tryCatch still protects) rather than throwing before the loop.
  stat_df_once <- if (is.data.frame(sr) && nrow(sr) > 0L) {
    tryCatch(pelsa_volcano_stat_df(sr, matched %||% data.frame()),
             error = function(e) NULL)
  } else {
    NULL
  }
  # ---- PASS 1: build every contrast's df_all/df_best WITHOUT rendering,
  # tracking the union logFC/logP extent (+ the significance-cutoff line,
  # which must stay visible) so every volcano PNG in this ome shares one
  # fixed x/y range -- makes any two exported figures directly comparable.
  built <- list()
  x_lo <- Inf; x_hi <- -Inf; y_lo <- Inf; y_hi <- -Inf
  track_range <- function(df) {
    if (is.null(df) || nrow(df) == 0L) return(invisible(NULL))
    # suppressWarnings: an all-NA logFC/logP column (never happens in
    # practice -- pelsa_volcano_export_df derives both from real stats)
    # would make min()/max() emit "no non-missing arguments" while still
    # correctly returning Inf/-Inf, which the is.finite() guard below
    # already degrades to per-plot autoscale.
    suppressWarnings({
      x_lo <<- min(x_lo, min(df$logFC, na.rm = TRUE))
      x_hi <<- max(x_hi, max(df$logFC, na.rm = TRUE))
      y_lo <<- min(y_lo, min(df$logP, na.rm = TRUE))
      y_hi <<- max(y_hi, max(df$logP, na.rm = TRUE))
    })
    y_cut <- attr(df, "y_cutoff")
    if (!is.null(y_cut) && is.finite(y_cut)) y_hi <<- max(y_hi, y_cut)
  }
  for (i in seq_along(choices)) {
    contrast <- unname(choices[[i]])
    df_all <- pelsa_volcano_export_df(sr, matched, fdf, markers, contrast,
                                      "all_peptide", sig_cutoff = sig_cutoff,
                                      is_self_curated = self_curated,
                                      sig_stat = sig_stat,
                                      .stat_df = stat_df_once)
    track_range(df_all)
    df_best <- if (want_best) {
      pelsa_volcano_export_df(sr, matched, fdf, markers, contrast,
                              "best_peptide", sig_cutoff = sig_cutoff,
                              is_self_curated = self_curated,
                              sig_stat = sig_stat, .stat_df = stat_df_once)
    } else {
      NULL
    }
    track_range(df_best)
    built[[i]] <- list(contrast = contrast, df_all = df_all,
                       df_best = df_best)
  }
  # No finite data across every contrast -- nothing to render; bail cleanly
  # rather than pass Inf/-Inf into coord_cartesian.
  shared_xlim <- if (is.finite(x_lo) && is.finite(x_hi)) c(x_lo, x_hi) else NULL
  shared_ylim <- if (is.finite(y_lo) && is.finite(y_hi)) c(y_lo, y_hi) else NULL

  # ---- PASS 2: render + save each contrast's figures with the shared range.
  for (i in seq_along(built)) {
    contrast <- built[[i]]$contrast
    df_all <- built[[i]]$df_all
    df_best <- built[[i]]$df_best
    if (!is.null(df_all) && nrow(df_all) > 0L) {
      p <- .pelsa_export_ggplot(df_all, df_all, color_mode, label_mode,
                                n_top_adjp = n_top_adjp,
                                n_top_markers = n_top_markers,
                                contrast = contrast,
                                volcano_label = "All-peptide volcano",
                                sig_cutoff = sig_cutoff)
      if (!is.null(shared_xlim)) {
        p <- p + ggplot2::coord_cartesian(xlim = shared_xlim,
                                          ylim = shared_ylim)
      }
      pelsa_save_figure(
        p, out, paste0("all_peptide_volcano_", pelsa_safe_name(contrast)),
        width = 6, height = 4.5)
    }
    if (want_best && !is.null(df_best) && nrow(df_best) > 0L) {
      p <- .pelsa_export_ggplot(df_best, df_best, color_mode, label_mode,
                                n_top_adjp = n_top_adjp,
                                n_top_markers = n_top_markers,
                                contrast = contrast,
                                volcano_label = "Best-peptide volcano",
                                sig_cutoff = sig_cutoff)
      if (!is.null(shared_xlim)) {
        p <- p + ggplot2::coord_cartesian(xlim = shared_xlim,
                                          ylim = shared_ylim)
      }
      pelsa_save_figure(
        p, out, paste0("best_peptide_volcano_", pelsa_safe_name(contrast)),
        width = 6, height = 4.5)
    }
  }
  invisible(out)
}

# Body of export_intensity: one intensity-line PNG per protein (marker |
# significant), contrast-independent (uses the union-across-contrasts adj.P
# via .PELSA_ANY_CONTRAST). @noRd
pelsa_section3_export_intensity <- function(dir_name, ome, stat_results, cache_entry,
                                            processed_mat, condition_map,
                                            condition_order, sig_cutoff, sig_stat,
                                            marker_accessions, log_transformation) {
  entry <- cache_entry
  if (is.null(entry)) return(invisible(NULL))
  matched <- entry$matched %||% data.frame()
  if (nrow(matched) == 0L) return(invisible(NULL))
  pm <- processed_mat; cmap <- condition_map
  corder <- condition_order
  if (is.null(pm) || is.null(cmap) || length(corder) == 0L)
    return(invisible(NULL))
  stat_df <- pelsa_export_add_any_contrast(
    pelsa_volcano_stat_df(stat_results[[ome]], matched))
  markers <- marker_accessions
  prot <- tryCatch(
    pelsa_intensity_proteins(stat_df, matched, markers, .PELSA_ANY_CONTRAST,
                             sig_cutoff, sig_stat = sig_stat),
    error = function(e) NULL)
  if (is.null(prot) || nrow(prot) == 0L) return(invisible(NULL))
  d_mk <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_VOLCANO,
                                 .PELSA_SUB_INTENSITY, .PELSA_GRP_MARKER)
  d_sg <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_VOLCANO,
                                 .PELSA_SUB_INTENSITY, .PELSA_GRP_SIGNIF)
  # Cap the protein set; record the overflow. skipped_proteins.tsv sits in the
  # intensity sub-stage folder (parent of the marker/significant split).
  capped <- pelsa_export_cap_proteins(prot, stat_df, matched = matched)
  prot <- capped$keep
  if (nrow(capped$skipped) > 0L) {
    skip <- capped$skipped
    skip$gene <- vapply(skip$accession,
                        function(a) pelsa_export_gene_for(matched, a), character(1))
    utils::write.table(
      skip[, c("accession", "gene", "adj.P"), drop = FALSE],
      file.path(dir_name, .PELSA_STAGE_VOLCANO, .PELSA_SUB_INTENSITY,
                "skipped_proteins.tsv"),
      sep = "\t", row.names = FALSE, quote = FALSE)
  }
  # y-axis label log base reflects this dataset's declared transform.
  log_xf <- log_transformation %||% NA_character_
  log_base <- if (identical(tolower(as.character(log_xf)), "log10")) 10L else 2L
  cov <- entry$coverage %||% data.frame()
  cov_lookup <- function(acc) {
    if (is.data.frame(cov) &&
        all(c("accession", "coverage") %in% colnames(cov))) {
      idx <- which(as.character(cov$accession) == acc)
      if (length(idx) > 0L) return(as.numeric(cov$coverage[idx[1L]]))
    }
    NA_real_
  }
  intensity_idx <- pelsa_intensity_build_index(matched)
  # ---- BUILD phase (sequential): assemble self-contained per-figure items ----
  items <- list()
  for (i in seq_len(nrow(prot))) {
    acc <- prot$accession[i]; is_mk <- isTRUE(prot$is_marker[i])
    ld <- tryCatch(
      pelsa_intensity_line_data(acc, stat_df, matched, pm, cmap, corder,
        .PELSA_ANY_CONTRAST, sig_cutoff, is_marker = is_mk,
        show_all = TRUE, sig_stat = sig_stat, .index = intensity_idx),
      error = function(e) NULL)
    if (is.null(ld) || nrow(ld) == 0L) next
    gene <- pelsa_export_gene_for(matched, acc)
    base <- paste0("intensityLine_", pelsa_safe_name(gene), "_",
                   pelsa_safe_name(acc))
    items[[length(items) + 1L]] <- list(
      ld = ld, gene = gene, acc = acc, log_base = log_base,
      coverage_frac = cov_lookup(acc), dir = if (is_mk) d_mk else d_sg,
      base = base)
  }
  # ---- RENDER phase (parallel): build ggplot + write PNG per item ------------
  render_one <- function(item) tryCatch({
    p <- pelsa_intensity_export_ggplot(item$ld, item$gene, item$acc,
                                       item$log_base,
                                       coverage_frac = item$coverage_frac)
    if (is.null(p)) return(invisible(NULL))
    pelsa_save_figure(p, item$dir, item$base, width = 9, height = 5)
  }, error = function(e) NULL)
  pelsa_export_render_map(items, render_one)
  invisible(NULL)
}

# Body of export_woods: one coverage/Woods PNG per (protein x contrast),
# split into marker | significant subfolders. @noRd
pelsa_section3_export_woods <- function(dir_name, ome, stat_results, cache_entry,
                                        feat_df, sig_cutoff, sig_stat,
                                        marker_accessions, contrast_choices) {
  entry <- cache_entry
  if (is.null(entry)) return(invisible(NULL))
  matched <- entry$matched %||% data.frame()
  if (nrow(matched) == 0L) return(invisible(NULL))
  stat_df <- pelsa_volcano_stat_df(stat_results[[ome]], matched)
  stat_any <- pelsa_export_add_any_contrast(stat_df)
  markers <- marker_accessions
  prot <- tryCatch(
    pelsa_intensity_proteins(stat_any, matched, markers, .PELSA_ANY_CONTRAST,
                             sig_cutoff, sig_stat = sig_stat),
    error = function(e) NULL)
  if (is.null(prot) || nrow(prot) == 0L) return(invisible(NULL))
  fdf <- feat_df %||% data.frame()
  fdf_by_acc <- if (is.data.frame(fdf) && nrow(fdf) > 0L &&
                    "accession" %in% colnames(fdf)) {
    facc <- as.character(fdf$accession)
    fvalid <- !is.na(facc) & nzchar(facc)
    if (any(fvalid)) split(fdf[fvalid, , drop = FALSE], facc[fvalid]) else list()
  } else {
    list()
  }
  cov <- entry$coverage %||% data.frame()
  choices <- contrast_choices
  woods_idx <- pelsa_woods_build_index(matched, stat_df)
  d_mk <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_VOLCANO,
                                 .PELSA_SUB_WOODS, .PELSA_GRP_MARKER)
  d_sg <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_VOLCANO,
                                 .PELSA_SUB_WOODS, .PELSA_GRP_SIGNIF)
  # Cap the protein set; record the overflow. skipped_proteins.tsv sits in the
  # woods sub-stage folder (parent of the marker/significant split).
  capped <- pelsa_export_cap_proteins(prot, stat_any, matched = matched)
  prot <- capped$keep
  if (nrow(capped$skipped) > 0L) {
    skip <- capped$skipped
    skip$gene <- vapply(skip$accession,
                        function(a) pelsa_export_gene_for(matched, a), character(1))
    utils::write.table(
      skip[, c("accession", "gene", "adj.P"), drop = FALSE],
      file.path(dir_name, .PELSA_STAGE_VOLCANO, .PELSA_SUB_WOODS,
                "skipped_proteins.tsv"),
      sep = "\t", row.names = FALSE, quote = FALSE)
  }
  # ---- BUILD phase (sequential): one item per (protein, contrast) ------------
  items <- list()
  for (i in seq_len(nrow(prot))) {
    acc <- prot$accession[i]; is_mk <- isTRUE(prot$is_marker[i])
    feats <- fdf_by_acc[[acc]] %||% fdf[0, , drop = FALSE]
    gene <- pelsa_export_gene_for(matched, acc)
    target <- if (is_mk) d_mk else d_sg
    for (cj in seq_along(choices)) {
      contrast <- unname(choices[[cj]])
      pep <- tryCatch(
        pelsa_woods_peptide_data(acc, matched, stat_df, contrast,
                                 sig_cutoff, sig_stat = sig_stat,
                                 .index = woods_idx),
        error = function(e) NULL)
      if (is.null(pep) || nrow(pep) == 0L) next
      plen <- pelsa_export_prot_len(cov, acc, pep)
      base <- paste0("woods_", pelsa_safe_name(gene), "_",
                     pelsa_safe_name(acc), "_contrast_",
                     pelsa_safe_name(contrast))
      items[[length(items) + 1L]] <- list(
        pep = pep, feats = feats, plen = plen, gene = gene, acc = acc,
        contrast = contrast, dir = target, base = base)
    }
  }
  # ---- RENDER phase (parallel) ----------------------------------------------
  render_one <- function(item) tryCatch({
    p <- pelsa_woods_export_ggplot(item$pep, item$feats, item$plen, item$gene,
                                   item$acc, item$contrast,
                                   sig_cutoff, sig_stat = sig_stat)
    if (is.null(p)) return(invisible(NULL))
    pelsa_save_figure(p, item$dir, item$base, width = 9, height = 4.2)
  }, error = function(e) NULL)
  pelsa_export_render_map(items, render_one)
  invisible(NULL)
}
