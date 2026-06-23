################################################################################
# Module: PELSA feature-class annotation (Task 2I) - the HIGHEST-parity-risk
# helper. Overlap-joins per-peptide spans against the UniProt feature table and
# resolves ONE primary feature class per peptide across all its proteins.
#
# Gold standard = the notebook's plots/volcano_annotate.py::annotate_feature_class
# + pick_primary_feature_class + FEATURE_PRIORITY / FEATURE_COLORS, ported
# VERBATIM below. The priority ladder ORDER is the NOTEBOOK's (transmembrane
# BEFORE repeat) - this differs from schema.json's level order and the planning
# doc; the NOTEBOOK wins.
#
# Public helpers:
#   pelsa_annotate_features(plot_df, feat_df)     core overlap + priority resolve
#   pelsa_unannotated_accessions(x, feat_df)      Summary-QC "failed annotation"
#
# Algorithm (vectorized, NO per-peptide R loop):
#   1. Detect shape (PG.ProteinAccessions present -> peptide panel; else protein
#      panel via accession/gene).
#   2. Build a token GRID preserving ;-order: one row per (peptide_row, token),
#      0-based _row_id + token_idx, the ;-aligned gene_token (padded to acc len).
#   3. Drop grid rows with empty accession or NA pep_start/pep_end.
#   4. foverlaps overlap join (CLOSED interval, type="any") on EXACT accession.
#   5. Priority resolution: setorder(_row_id, _rank, token_idx, feat.start),
#      keep FIRST per _row_id. Lower rank wins; tie -> leading accession (lowest
#      token_idx) -> earliest feature start.
#   6. Write winners back by _row_id; non-overlap rows keep "none" + fallbacks.
#
# Parity notes:
#   - The overlap merge is on EXACT accession (P12345-2 does NOT match a P12345
#     feature row). Mirrors the notebook's exact-string merge.
#   - The comma-in-token PEP.PeptidePosition ("2,167") case is resolved UPSTREAM
#     by 2B (one matched row per occurrence, each with its own pep_start/pep_end);
#     here each occurrence row simply annotates against its own range.
#   - pelsa_unannotated_accessions DOES apply isoform-base fallback (P12345-2 is
#     not counted unannotated if base P12345 is present) - an intentional
#     asymmetry vs the exact-accession overlap merge.
#   - INVERTED spans (pep_start > pep_end) are DROPPED from the overlap grid (so
#     they get "none" + leading-accession fallback) with a one-time warning().
#     foverlaps(type="any") mis-joins an inverted query interval silently, so
#     such rows must never reach the join; the warning surfaces upstream bugs.
################################################################################

# ---- Constants (ported VERBATIM - order/ranks/colors are parity-locked) ------

# Priority ladder, highest -> lowest. rank = 0-based index (0 = highest).
# NOTE: transmembrane_or_signal BEFORE repeat_or_coiled_coil (the NOTEBOOK's
# order; differs from schema.json feature_class_levels - notebook wins).
PELSA_FEATURE_PRIORITY <- c(
  "active_or_binding_site",
  "catalytic_domain",
  "folded_domain",
  "region_or_motif",
  "transmembrane_or_signal",
  "repeat_or_coiled_coil",
  "low_complexity_or_disorder",
  "other"
)

# Sentinel class for peptides with no overlapping feature.
NONE_FEATURE_CLASS <- "none"

# The 3 feature columns pelsa_annotate_features() appends to its input frame.
# The analysis cache stores ONLY these (row-aligned to `matched`) instead of a
# full annotated duplicate; pelsa_annotation_frame() reconstructs the full frame.
PELSA_ANNOTATION_FEATURE_COLS <- c(
  "feature_class_primary", "winning_accession", "winning_gene"
)

# Named 0-based rank vector (unknown label -> 999). Built once from the ladder.
.PELSA_FEATURE_PRIORITY_RANK <- stats::setNames(
  seq_along(PELSA_FEATURE_PRIORITY) - 1L,
  PELSA_FEATURE_PRIORITY
)

# Per-class colors (ported VERBATIM, incl. the "none" grey).
PELSA_FEATURE_COLORS <- c(
  active_or_binding_site     = "#1f77b4",
  catalytic_domain           = "#ff7f0e",
  folded_domain              = "#d62728",
  region_or_motif            = "#9467bd",
  transmembrane_or_signal    = "#2ca02c",
  repeat_or_coiled_coil      = "#8c564b",
  low_complexity_or_disorder = "#7f7f7f",
  other                      = "#bcbd22",
  none                       = "#d3d3d3"
)

# Map feature_class labels -> 0-based priority rank; unknown/NA -> 999L.
#
# @param fc character vector of feature_class labels
# @return integer vector of ranks (same length)
# @noRd
.pelsa_priority_rank <- function(fc) {
  out <- .PELSA_FEATURE_PRIORITY_RANK[fc]
  out[is.na(out)] <- 999L
  as.integer(unname(out))
}

# Strip a UniProt isoform suffix: "P12345-2" -> "P12345"; "P12345" unchanged.
#
# @param acc character vector
# @return character vector of base accessions
# @noRd
.pelsa_isoform_base <- function(acc) {
  sub("-[0-9]+$", "", acc)
}

# ---- Token grid builder ------------------------------------------------------

# Build the (peptide_row, accession_token) grid preserving ;-order.
#
# Returns a data.frame with 0-based `_row_id` (positional index into plot_df),
# 0-based `token_idx` (position within the ;-list; leading accession = 0),
# `accession` (trimmed token), `gene_token` (the ;-aligned gene, "" where
# absent), `pep_start`, `pep_end` (recycled from the row). Plus the per-row
# fallback vectors as attributes so callers needn't recompute.
#
# @param plot_df the per-peptide frame (peptide- or protein-panel shape)
# @return data.frame grid (possibly zero-row) + attr "fallback_acc"/"fallback_gene"
# @noRd
.pelsa_build_token_grid <- function(plot_df) {
  n_row <- nrow(plot_df)
  pep_start <- as.integer(plot_df[["pep_start"]])
  pep_end <- as.integer(plot_df[["pep_end"]])

  is_peptide_panel <- "PG.ProteinAccessions" %in% colnames(plot_df)

  if (is_peptide_panel) {
    acc_raw <- as.character(plot_df[["PG.ProteinAccessions"]])
    gene_raw <- if ("PG.Genes" %in% colnames(plot_df)) {
      as.character(plot_df[["PG.Genes"]])
    } else {
      rep(NA_character_, n_row)
    }
    acc_lists <- strsplit(acc_raw, ";", fixed = TRUE)
    gene_lists <- strsplit(gene_raw, ";", fixed = TRUE)
  } else {
    # Protein panel: synthesize a 1-token split from accession/gene.
    acc_lists <- as.list(as.character(plot_df[["accession"]]))
    gene_src <- if ("gene" %in% colnames(plot_df)) {
      as.character(plot_df[["gene"]])
    } else {
      rep(NA_character_, n_row)
    }
    gene_lists <- as.list(gene_src)
  }

  # Per-row fallbacks (computed on the FULL frame, independent of grid drops).
  fallback_acc <- .pelsa_fallback_acc(plot_df, acc_lists)
  fallback_gene <- .pelsa_fallback_gene(plot_df, gene_lists, fallback_acc)

  if (n_row == 0L) {
    grid <- data.frame(
      `_row_id`  = integer(0),
      token_idx  = integer(0),
      accession  = character(0),
      gene_token = character(0),
      pep_start  = integer(0),
      pep_end    = integer(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    attr(grid, "fallback_acc") <- fallback_acc
    attr(grid, "fallback_gene") <- fallback_gene
    return(grid)
  }

  n_tok <- lengths(acc_lists)
  # Flatten accession tokens (trim once on the flattened vector).
  flat_acc <- trimws(unlist(acc_lists, use.names = FALSE))
  if (is.null(flat_acc)) flat_acc <- character(0)

  # 0-based positional row id, recycled per token.
  row_id0 <- rep.int(seq_len(n_row) - 1L, n_tok)
  # 0-based token index within each ;-list.
  token_idx <- sequence(n_tok) - 1L

  # Gene tokens padded/recycled to the accession token positions: the j-th
  # accession token takes the j-th gene token; missing -> "". A single gene
  # token does NOT recycle across accessions here (alignment is positional;
  # padding keeps C's gene aligned to C even if B's slot is empty).
  gene_token <- .pelsa_pad_gene_tokens(gene_lists, n_tok)

  grid <- data.frame(
    `_row_id`  = row_id0,
    token_idx  = as.integer(token_idx),
    accession  = flat_acc,
    gene_token = gene_token,
    pep_start  = rep.int(pep_start, n_tok),
    pep_end    = rep.int(pep_end, n_tok),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Drop grid rows with empty accession, NA span, OR an INVERTED span
  # (pep_start > pep_end). foverlaps(type="any") silently mis-joins an inverted
  # query interval (no error), so inverted spans must never reach the join. We
  # DROP them (they fall through to the "none" + leading-accession fallback,
  # consistent with the no-overlap path) and emit a one-time warning so a
  # genuine upstream coordinate bug surfaces (soft-fail posture used in PELSA).
  valid_acc <- !is.na(grid$accession) & nzchar(grid$accession)
  valid_span <- !is.na(grid$pep_start) & !is.na(grid$pep_end)
  inverted <- valid_span & (grid$pep_start > grid$pep_end)
  if (any(inverted)) {
    warning("pelsa_annotate_features: dropped ", sum(inverted),
            " peptide-token span(s) with pep_start > pep_end (inverted ",
            "interval); these get feature_class_primary='none' + fallback.",
            call. = FALSE)
  }
  keep <- valid_acc & valid_span & !inverted
  grid <- grid[keep, , drop = FALSE]
  rownames(grid) <- NULL

  attr(grid, "fallback_acc") <- fallback_acc
  attr(grid, "fallback_gene") <- fallback_gene
  grid
}

# Pad each row's gene tokens to its accession-token count, trimmed, "" for NA.
#
# STRICT POSITIONAL alignment: the j-th gene token aligns to the j-th accession
# token; positions with no gene token (incl. a too-short ;-list or a single gene
# token across many accessions) get "". A single gene token does NOT recycle -
# this is what preserves alignment when a middle accession's gene slot is empty
# (so token C's gene stays with C, never shifted). FULLY VECTORIZED (no per-row
# R loop): O(total grid tokens) via sequence()/cumsum() + integer indexing.
#
# @param gene_lists list of split gene-token character vectors (per row)
# @param n_tok      integer vector of accession-token counts (per row)
# @return character vector of length sum(n_tok); "" where no aligned gene token
# @noRd
.pelsa_pad_gene_tokens <- function(gene_lists, n_tok) {
  total <- sum(n_tok)
  if (total == 0L) return(character(0))

  n_gene <- lengths(gene_lists)
  flat_gene <- trimws(unlist(gene_lists, use.names = FALSE))
  if (is.null(flat_gene)) flat_gene <- character(0)
  gene_offset <- cumsum(n_gene) - n_gene   # 0-based start of each row's genes

  # Map each grid token back to its original row and its within-row position k.
  row_idx <- rep.int(seq_along(n_tok), n_tok)
  k <- sequence(n_tok)                      # 1,2,..,n1,1,2,..,n2,...
  n_gene_row <- n_gene[row_idx]

  # j-th accession takes j-th gene token IFF that position exists; else NA -> "".
  chosen <- ifelse(k <= n_gene_row, k, NA_integer_)
  global_idx <- gene_offset[row_idx] + chosen
  vals <- flat_gene[global_idx]             # NA index -> NA value
  vals[is.na(vals)] <- ""
  vals
}

# Leading (first) token of each per-row split list, trimmed, "" for empty/NA.
# Vectorized: index the FIRST element of each row's tokens out of the flattened
# vector via cumsum offsets (no per-row R closure).
#
# @noRd
.pelsa_leading_token <- function(token_lists) {
  n <- lengths(token_lists)
  if (length(n) == 0L) return(character(0))
  flat <- unlist(token_lists, use.names = FALSE)
  if (is.null(flat)) flat <- character(0)
  offset <- cumsum(n) - n          # 0-based start of each row
  lead_idx <- offset + 1L          # first token of each row
  lead_idx[n == 0L] <- NA_integer_ # rows with no tokens -> NA -> ""
  out <- trimws(flat[lead_idx])
  out[is.na(out)] <- ""
  out
}

# Per-row fallback accession: leading_acc col if present, else leading token of
# accession list, else "".
#
# @noRd
.pelsa_fallback_acc <- function(plot_df, acc_lists) {
  n_row <- nrow(plot_df)
  if (n_row == 0L) return(character(0))
  if ("leading_acc" %in% colnames(plot_df)) {
    out <- trimws(as.character(plot_df[["leading_acc"]]))
    out[is.na(out)] <- ""
    return(out)
  }
  .pelsa_leading_token(acc_lists)
}

# Per-row fallback gene: gene_lead col if present, else leading gene token, else
# "". If the resolved gene is empty -> the fallback accession.
#
# @noRd
.pelsa_fallback_gene <- function(plot_df, gene_lists, fallback_acc) {
  n_row <- nrow(plot_df)
  if (n_row == 0L) return(character(0))
  if ("gene_lead" %in% colnames(plot_df)) {
    g <- trimws(as.character(plot_df[["gene_lead"]]))
  } else {
    g <- .pelsa_leading_token(gene_lists)
  }
  g[is.na(g)] <- ""
  empty <- !nzchar(g)
  g[empty] <- fallback_acc[empty]
  g
}

# ---- Function 2: the core overlap + priority resolution ----------------------

# Annotate each peptide row with a primary feature class + winning accession/gene.
#
# @param plot_df per-peptide frame. PEPTIDE-panel shape carries
#                `PG.ProteinAccessions` (;-delimited, + optional ;-aligned
#                `PG.Genes`), plus `pep_start`/`pep_end`. PROTEIN-panel shape
#                carries one `accession` (+ optional `gene`) per row. Optional
#                `leading_acc`/`gene_lead` override the fallbacks.
# @param feat_df per-feature table with accession/start/end/feature_class.
# @return a copy of plot_df + three columns: feature_class_primary,
#         winning_accession, winning_gene.
# @noRd
pelsa_annotate_features <- function(plot_df, feat_df) {
  stopifnot(is.data.frame(plot_df), is.data.frame(feat_df))
  # Boundary validation: plot_df shape + spans.
  if (!all(c("pep_start", "pep_end") %in% colnames(plot_df))) {
    stop("pelsa_annotate_features: plot_df must have pep_start and pep_end")
  }
  if (!("PG.ProteinAccessions" %in% colnames(plot_df) ||
        "accession" %in% colnames(plot_df))) {
    stop("pelsa_annotate_features: plot_df must have PG.ProteinAccessions ",
         "(peptide panel) or accession (protein panel)")
  }
  feat_required <- c("accession", "start", "end", "feature_class")
  if (!all(feat_required %in% colnames(feat_df))) {
    stop("pelsa_annotate_features: feat_df must have ",
         paste(feat_required, collapse = "/"))
  }

  n_row <- nrow(plot_df)
  grid <- .pelsa_build_token_grid(plot_df)
  fallback_acc <- attr(grid, "fallback_acc")
  fallback_gene <- attr(grid, "fallback_gene")

  # Initialize outputs to the no-overlap defaults.
  feature_class_primary <- rep(NONE_FEATURE_CLASS, n_row)
  winning_accession <- fallback_acc
  winning_gene <- fallback_gene

  out <- plot_df
  rownames(out) <- NULL

  # Empty grid OR empty feat_df -> everything keeps the fallback defaults.
  if (nrow(grid) == 0L || nrow(feat_df) == 0L || n_row == 0L) {
    out$feature_class_primary <- feature_class_primary
    out$winning_accession <- winning_accession
    out$winning_gene <- winning_gene
    return(out)
  }

  # ---- foverlaps overlap join (CLOSED interval, EXACT accession) ------------
  # x = query (grid): keyed on (accession, pep_start, pep_end).
  # y = lookup (features): keyed on (accession, start, end).
  # type="any" => closed-interval overlap: feat.start <= pep_end &
  #               feat.end >= pep_start.
  qry <- data.table::data.table(
    accession  = grid$accession,
    pep_start  = grid$pep_start,
    pep_end    = grid$pep_end,
    `_row_id`  = grid[["_row_id"]],
    token_idx  = grid$token_idx,
    gene_token = grid$gene_token
  )
  feat <- data.table::data.table(
    accession     = as.character(feat_df$accession),
    start         = as.integer(feat_df$start),
    end           = as.integer(feat_df$end),
    feature_class = as.character(feat_df$feature_class)
  )
  # Drop feature (y-side) rows with NA or inverted ranges before the join.
  # foverlaps() hard-errors on NA in y's range columns, and the regenerable
  # on-disk feature cache is untrusted input (a blank/unparseable coord parses
  # to NA). Mirror the grid (x) side: drop them with a one-time warning so a
  # corrupt cache surfaces in logs (soft-fail posture used in PELSA).
  # Sentinel rows (feature_class "none" with NA coords) mark resolved-but-0-
  # feature accessions; they carry no interval and are DROPPED silently. Genuine
  # corruption (NA/inverted coords on a REAL feature_class) is also dropped but
  # WARNS so a bad cache surfaces in logs.
  feat_bad <- is.na(feat$start) | is.na(feat$end) | (feat$start > feat$end)
  # NA-safe sentinel test: a blank cache row can have NA feature_class, and
  # (NA == "none") is NA -- which would make `any(feat_corrupt)` error. Guard the
  # comparison so an NA feature_class is NOT a sentinel (it is corruption: warn).
  is_sentinel <- feat_bad &
    (!is.na(feat$feature_class) & feat$feature_class == "none") &
    is.na(feat$start) & is.na(feat$end)
  feat_corrupt <- feat_bad & !is_sentinel
  if (any(feat_corrupt)) {
    warning("pelsa_annotate_features: dropped ", sum(feat_corrupt),
            " feature row(s) with NA or inverted (start > end) coordinates ",
            "from the feature cache.", call. = FALSE)
  }
  if (any(feat_bad)) feat <- feat[!feat_bad]
  data.table::setkey(qry, accession, pep_start, pep_end)
  data.table::setkey(feat, accession, start, end)

  hits <- data.table::foverlaps(
    qry, feat, type = "any", nomatch = NULL
  )

  if (nrow(hits) == 0L) {
    out$feature_class_primary <- feature_class_primary
    out$winning_accession <- winning_accession
    out$winning_gene <- winning_gene
    return(out)
  }

  # ---- Priority resolution --------------------------------------------------
  # Lower rank wins; tie -> leading accession (lowest token_idx) -> earliest
  # feature start. `start` here is the FEATURE start (foverlaps keeps y's names).
  hits[, `_rank` := .pelsa_priority_rank(feature_class)]
  data.table::setorderv(
    hits, c("_row_id", "_rank", "token_idx", "start"),
    order = 1L
  )
  winners <- unique(hits, by = "_row_id")   # keeps FIRST per _row_id (sorted)

  # Write winners back by 0-based _row_id -> 1-based positional index.
  pos <- winners[["_row_id"]] + 1L
  feature_class_primary[pos] <- winners$feature_class
  winning_accession[pos] <- winners$accession
  # winning_gene: the gene_token; if empty -> the (winning) accession.
  wg <- winners$gene_token
  wg[is.na(wg) | !nzchar(wg)] <- winners$accession[is.na(wg) | !nzchar(wg)]
  winning_gene[pos] <- wg

  out$feature_class_primary <- feature_class_primary
  out$winning_accession <- winning_accession
  out$winning_gene <- winning_gene
  out
}

# ---- Function 3: unannotated accessions (Summary QC) -------------------------

# Accessions present in the plot but ABSENT from feat_df (feeds the Summary QC
# "proteins failed annotation" metric; notebook n_unmapped_features).
#
# Set difference of the plot's ;-tokenized accessions minus the feature table's
# accessions, WITH isoform-base fallback: an accession "P12345-2" counts as
# annotated if base "P12345" is in feat_df. (This isoform-base fallback applies
# ONLY here - the overlap merge in pelsa_annotate_features is on exact accession.)
#
# @param plot_df_or_accessions either a data.frame with PG.ProteinAccessions /
#        accession, or a bare character vector of (possibly ;-delimited)
#        accession strings.
# @param feat_df per-feature table with an `accession` column.
# @return character vector of unannotated accessions (the original tokens).
# @noRd
pelsa_unannotated_accessions <- function(plot_df_or_accessions, feat_df) {
  stopifnot(is.data.frame(feat_df))
  if (!"accession" %in% colnames(feat_df)) {
    stop("pelsa_unannotated_accessions: feat_df must have an accession column")
  }

  # Collect the raw accession strings.
  if (is.data.frame(plot_df_or_accessions)) {
    if ("PG.ProteinAccessions" %in% colnames(plot_df_or_accessions)) {
      raw <- as.character(plot_df_or_accessions[["PG.ProteinAccessions"]])
    } else if ("accession" %in% colnames(plot_df_or_accessions)) {
      raw <- as.character(plot_df_or_accessions[["accession"]])
    } else {
      stop("pelsa_unannotated_accessions: data.frame needs ",
           "PG.ProteinAccessions or accession")
    }
  } else {
    raw <- as.character(plot_df_or_accessions)
  }

  tokens <- trimws(unlist(strsplit(raw, ";", fixed = TRUE), use.names = FALSE))
  if (is.null(tokens)) tokens <- character(0)
  tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
  tokens <- unique(tokens)
  if (length(tokens) == 0L) return(character(0))

  feat_acc <- unique(as.character(feat_df$accession))
  feat_acc <- feat_acc[!is.na(feat_acc) & nzchar(feat_acc)]
  # Annotated set = exact accessions + their isoform bases (so P12345-2 resolves
  # via base P12345).
  annotated_set <- unique(c(feat_acc, .pelsa_isoform_base(feat_acc)))

  # A token is annotated if it is in the set OR its isoform base is.
  token_base <- .pelsa_isoform_base(tokens)
  is_annotated <- tokens %in% annotated_set | token_base %in% annotated_set
  tokens[!is_annotated]
}

# ---- Function 4: three-way annotation status counts (Summary QC) -------------

# Bucket a dataset's accessions into three mutually-exclusive annotation states
# against the (sentinel-aware) feature cache, for the Summary QC dashboard:
#   n_with_features  accession (or its isoform base) has >= 1 REAL feature row
#                    (feature_class != "none") in feat_df.
#   n_zero_feature   accession is in feat_df but ONLY as sentinel row(s)
#                    (feature_class "none"). A user-uploaded raw annotation file
#                    usually ships no sentinels, so this is typically 0.
#   n_failed         accession (and its base) absent from the uploaded annotation
#                    file entirely -- "failed to resolve annotation". An accession
#                    not present in the file counts here, NOT as zero_feature.
# The three sum to the unique dataset-accession-token count. n_failed equals the
# legacy pelsa_unannotated_accessions() length (and the export's
# missing_accessions list).
#
# @param plot_df_or_accessions data.frame (PG.ProteinAccessions / accession) or a
#        character vector of (possibly ;-delimited) accession strings.
# @param feat_df per-feature table with accession + feature_class columns.
# @return list(n_with_features=<int>, n_zero_feature=<int>, n_failed=<int>).
# @noRd
pelsa_annotation_status_counts <- function(plot_df_or_accessions, feat_df) {
  stopifnot(is.data.frame(feat_df))
  if (!all(c("accession", "feature_class") %in% colnames(feat_df))) {
    stop("pelsa_annotation_status_counts: feat_df needs accession + ",
         "feature_class columns")
  }

  if (is.data.frame(plot_df_or_accessions)) {
    if ("PG.ProteinAccessions" %in% colnames(plot_df_or_accessions)) {
      raw <- as.character(plot_df_or_accessions[["PG.ProteinAccessions"]])
    } else if ("accession" %in% colnames(plot_df_or_accessions)) {
      raw <- as.character(plot_df_or_accessions[["accession"]])
    } else {
      stop("pelsa_annotation_status_counts: data.frame needs ",
           "PG.ProteinAccessions or accession")
    }
  } else {
    raw <- as.character(plot_df_or_accessions)
  }
  tokens <- trimws(unlist(strsplit(raw, ";", fixed = TRUE), use.names = FALSE))
  if (is.null(tokens)) tokens <- character(0)
  tokens <- unique(tokens[!is.na(tokens) & nzchar(tokens)])
  if (length(tokens) == 0L) {
    return(list(n_with_features = 0L, n_zero_feature = 0L, n_failed = 0L))
  }

  # Accessions in feat_df WITH a real feature (feature_class != "none").
  real <- feat_df[!is.na(feat_df$feature_class) &
                    feat_df$feature_class != "none", , drop = FALSE]
  real_acc <- unique(as.character(real$accession))
  real_acc <- real_acc[!is.na(real_acc) & nzchar(real_acc)]
  real_set <- unique(c(real_acc, .pelsa_isoform_base(real_acc)))

  # All accessions present in feat_df (real OR sentinel).
  all_acc <- unique(as.character(feat_df$accession))
  all_acc <- all_acc[!is.na(all_acc) & nzchar(all_acc)]
  all_set <- unique(c(all_acc, .pelsa_isoform_base(all_acc)))

  token_base <- .pelsa_isoform_base(tokens)
  has_real <- tokens %in% real_set | token_base %in% real_set
  in_cache <- tokens %in% all_set  | token_base %in% all_set
  list(n_with_features = as.integer(sum(has_real)),
       n_zero_feature  = as.integer(sum(in_cache & !has_real)),
       n_failed        = as.integer(sum(!in_cache)))
}
