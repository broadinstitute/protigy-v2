################################################################################
# Module: PELSA Start-Analysis - pure validation + compute-pipeline assembly
# (Task 5D).
#
# The Setup tab's "Start Analysis" button (tab_pelsa_section1.R) runs a one-shot
# compute pipeline that assembles the verified Phase-2 helpers into a per-dataset
# cache object the Summary (Phase 6) and Volcano (Phase 7) sections READ. The
# heavy, network-bound, and reactivity-bound wiring lives in the observer; the
# PURE logic lives here so it unit-tests with NO live network and NO Shiny.
#
# Public helpers (all @noRd):
#   pelsa_validate_setup(setup_snapshot, gcts, database_dir)
#       -> list(ok = TRUE/FALSE, errors = character()). Pre-flight checklist.
#   pelsa_setup_snapshot(setup_state)
#       -> a plain immutable list copy of the live reactiveValues, taken under
#          isolate() at click so mid-compute input edits cannot corrupt a run.
#   pelsa_dataset_peptide_frame(gct)
#       -> a peptide-level data.frame = cbind(rdesc, mat) for a cmapR GCT (or a
#          plain data.frame passed straight through; the test seam).
#   pelsa_condition_map_for(cdesc, sample_cols, condition_col)
#       -> named character vector sample -> condition, aligned to sample_cols
#          (the condition_map pelsa_within_condition_cv() consumes).
#   pelsa_run_analysis(gcts, gcts_original, setup_snapshot, fasta_map, feat_df,
#                      resolve_fasta = NULL, resolve_feat = NULL, ...)
#       -> named-by-dataset list of per-dataset cache objects (the integration
#          crux). NO network, NO Shiny: the observer reads each dataset's uploaded
#          FASTA + annotation file and passes them in; tests inject synthetic ones.
#
# ------------------------------------------------------------------------------
# DECISIONS (documented per the task spec)
# ------------------------------------------------------------------------------
# ANNOTATION-AS-UPLOADED. pelsa_run_analysis NEVER fetches from UniProt. It uses
#   the uploaded feature annotation (feat_df) as-is and records the dataset
#   accessions absent from it via pelsa_unannotated_accessions() (the "failed to
#   resolve annotation" set). This keeps the analysis path network-free, fast,
#   deterministic, and unit-testable. The cache records the unannotated set so the
#   Summary QC can flag poor annotation coverage.
#
# COMPUTE-ALL-AT-START: pelsa_run_analysis computes EVERY checked dataset's heavy
#   objects once per Start-Analysis (simpler, matches the "analyzed datasets"
#   semantics - the switcher then shows exactly the analyzed set). The planning
#   doc's switch-time freeing of INACTIVE rendered objects is a Phase 6/7 render
#   concern, not a compute concern. ALTERNATIVE (documented, not chosen): compute
#   lazily per-active-dataset to bound peak memory with many large datasets - the
#   seam is the per-dataset keying of the returned cache, so a lazy variant would
#   call pelsa_run_analysis_one() (below) on demand instead of looping here.
#
# CONDITION MAP / GCTs_original ALIGNMENT (the integration crux, documented):
#   - CV (2D) runs on RAW LINEAR intensities. We read them from
#     gcts_original[[ds]] - BUT note Protigy's `GCTs_original` is the
#     LOG-TRANSFORMED matrix (post perform_log_transformation), not raw linear.
#     CV is NOT invariant under log, so the pipeline DELINEARIZES that matrix by
#     the dataset's declared log base (params$log_transformation -> log2 => 2^x,
#     log10 => 10^x, None/NA => already-linear pass-through) via
#     pelsa_delinearize() BEFORE CV. Depth (2E) and the
#     intensity-line plot keep using the PROCESSED log2 matrix as-is. The raw
#     matrix columns are the sample
#     names; the condition map is built from THAT dataset's cdesc condition
#     column (setup$condition_col[[ds]]) keyed by sample name, so a named
#     condition_map aligns to columns regardless of column order
#     (.pelsa_resolve_condition_map reorders by name).
#   - Depth (2E) runs on the PROCESSED matrix from gcts[[ds]].
#   - The matched cache's .row_id is the 1-based ROW index into the per-dataset
#     PEPTIDE FRAME (rdesc rows == mat rows, same order), so CV's row_id (1-based
#     row index into the raw matrix) and the peptide frame rows refer to the same
#     peptide. We DO NOT join CV back onto the matched cache here (that is a
#     render-time concern for Summary); we just cache both keyed consistently.
################################################################################

# ---- inline validation message UI --------------------------------------------

# Build the inline validation feedback block (pure tag constructor).
#
# Given a pelsa_validate_setup() result, returns NULL when ok (no markup), else a
# red-bordered list of the specific errors so the user sees exactly what is
# missing. Pure (a function of its args) so it tests without a session.
#
# @param validation a list(ok=, errors=) from pelsa_validate_setup().
# @return NULL (ok) or a shiny tag (the error block).
# @noRd
pelsa_validation_msg_ui <- function(validation) {
  if (isTRUE(validation$ok) || length(validation$errors) == 0L) return(NULL)
  shiny::tags$div(
    class = "pelsa-validation-errors",
    style = paste0("border:1px solid #d9534f; border-radius:6px; ",
                   "padding:10px; margin-top:8px; background:#fdf3f2; ",
                   "color:#a94442;"),
    shiny::tags$strong(
      shiny::icon("circle-exclamation"), " Cannot start analysis:"
    ),
    shiny::tags$ul(
      style = "margin:6px 0 0 0;",
      lapply(validation$errors, function(e) shiny::tags$li(e))
    )
  )
}

# ---- snapshot ----------------------------------------------------------------

# Take a plain, immutable list snapshot of the live setup_state reactiveValues.
#
# Called under isolate() by the Start-Analysis observer so input edits made WHILE
# a run is computing cannot corrupt the in-flight run. Copies only the fields the
# pipeline + validation read. The per-dataset list fields are copied wholesale
# (they are themselves plain lists), so the snapshot shares no reference with the
# live reactiveValues.
#
# PER-DATASET FIELDS (all NAMED LISTS keyed by ome): species, compound,
# marker_rows, skip, condition_col, replicate_col, condition_order,
# replicate_order, sample_order. `datasets` is the NON-SKIPPED ome set (the Setup
# observer sets setup_state$datasets to the analyzed subset before snapshotting).
# Every per-ome field defaults to an empty list() so a fresh / partially-
# configured setup is safe to index with [[ome]].
#
# @param setup_state the live reactiveValues from PELSASection1_Tab_Server.
# @return a plain named list with the same field names.
# @noRd
pelsa_setup_snapshot <- function(setup_state) {
  list(
    datasets        = setup_state$datasets        %||% character(0),
    fasta_path      = setup_state$fasta_path      %||% list(),
    fasta_name      = setup_state$fasta_name      %||% list(),
    annotation_path = setup_state$annotation_path %||% list(),
    annotation_name = setup_state$annotation_name %||% list(),
    self_curated    = setup_state$self_curated    %||% list(),
    compound        = setup_state$compound        %||% list(),
    marker_rows     = setup_state$marker_rows     %||% list(),
    skip            = setup_state$skip            %||% list(),
    condition_col   = setup_state$condition_col   %||% list(),
    replicate_col   = setup_state$replicate_col   %||% list(),
    condition_order = setup_state$condition_order %||% list(),
    replicate_order = setup_state$replicate_order %||% list(),
    sample_order    = setup_state$sample_order    %||% list()
  )
}

# ---- pre-flight validation ---------------------------------------------------

# Pre-flight checklist for Start-Analysis (PURE, closed-form testable).
#
# `setup_snapshot$datasets` is the NON-SKIPPED (analyzed) ome set; the Setup
# observer derives it from the per-ome skip flags before snapshotting. ONLY these
# datasets are validated - a skipped dataset's (possibly incomplete) config is
# never checked.
#
# Checks, accumulating ALL failures (so the user sees every missing piece at
# once, not one-at-a-time):
#   1. >= 1 non-skipped dataset (else "all skipped").
#   2. Each non-skipped dataset has a condition column and replicate column
#      chosen. A value of "(none)" (the blank default) / "" / NA counts as NOT
#      chosen.
#   3. The chosen condition column exists in that dataset's cdesc.
#   4. Each non-skipped dataset has a confirmed (non-empty) condition order.
#   5. Each non-skipped dataset has an uploaded FASTA; and an uploaded annotation
#      file unless it is flagged self-curated.
#
# fasta_path/annotation_path/self_curated/condition_col/replicate_col are PER-OME
# named lists keyed by ome. database_dir is retained for signature stability but
# is no longer used (uploads supersede the on-disk database).
#
# An EMPTY marker table is VALID (markers are a volcano OVERLAY, not a
# prerequisite) - no marker check here.
#
# @param setup_snapshot a pelsa_setup_snapshot() list (or the live reactiveValues
#                        - both support $field access).
# @param gcts           named list of per-ome GCTs (for cdesc column existence).
# @param database_dir   the PELSA database dir (FASTA existence check).
# @return list(ok = logical scalar, errors = character()).
# @noRd
pelsa_validate_setup <- function(setup_snapshot, gcts, database_dir) {
  errors <- character(0)

  datasets <- setup_snapshot$datasets %||% character(0)
  datasets <- as.character(datasets)
  datasets <- datasets[!is.na(datasets) & nzchar(datasets)]

  # 1. >= 1 non-skipped dataset.
  if (length(datasets) == 0L) {
    errors <- c(errors,
                "Enable PELSA analysis for at least one dataset (all are skipped).")
  }

  fasta_path      <- setup_snapshot$fasta_path      %||% list()
  annotation_path <- setup_snapshot$annotation_path %||% list()
  self_curated    <- setup_snapshot$self_curated    %||% list()
  condition_col   <- setup_snapshot$condition_col   %||% list()
  replicate_col   <- setup_snapshot$replicate_col   %||% list()
  condition_order <- setup_snapshot$condition_order %||% list()

  for (ds in datasets) {
    # 2. Condition column (chosen + not the "(none)" default).
    col <- condition_col[[ds]]
    if (.pelsa_is_unset(col)) {
      errors <- c(errors, sprintf(
        "Dataset '%s': choose a condition grouping column.", ds))
    } else {
      # 3. Column must exist in that dataset's cdesc (when the GCT is available).
      gct <- if (is.list(gcts)) gcts[[ds]] else NULL
      cdesc <- .pelsa_gct_cdesc(gct)
      if (!is.null(cdesc) && !(col %in% names(cdesc))) {
        errors <- c(errors, sprintf(
          "Dataset '%s': condition column '%s' is not in its annotations.",
          ds, col))
      }
    }

    # 2. Replicate column (chosen + not the "(none)" default).
    if (.pelsa_is_unset(replicate_col[[ds]])) {
      errors <- c(errors, sprintf(
        "Dataset '%s': choose a replicate identifier column.", ds))
    }

    # 4. Confirmed condition order.
    order <- condition_order[[ds]]
    has_order <- !is.null(order) && length(order) >= 1L &&
      any(!is.na(order) & nzchar(as.character(order)))
    if (!has_order) {
      errors <- c(errors, sprintf(
        "Dataset '%s': confirm the condition order.", ds))
    }

    # 2 + 5. Per-dataset uploads: a FASTA is always required; an annotation file
    # is required unless this dataset is a self-curated database.
    fp <- fasta_path[[ds]]
    if (is.null(fp) || !nzchar(fp %||% "")) {
      errors <- c(errors, sprintf("Dataset '%s': upload a FASTA file.", ds))
    } else if (!file.exists(fp)) {
      errors <- c(errors, sprintf(
        paste0("Dataset '%s': the FASTA file is missing or was moved -- ",
               "re-upload it."), ds))
    }
    if (!isTRUE(self_curated[[ds]])) {
      ap <- annotation_path[[ds]]
      if (is.null(ap) || !nzchar(ap %||% "")) {
        errors <- c(errors, sprintf(
          paste0("Dataset '%s': upload a feature annotation file (or check ",
                 "'Self-curated database')."), ds))
      } else if (!file.exists(ap)) {
        errors <- c(errors, sprintf(
          paste0("Dataset '%s': the feature annotation file is missing or was ",
                 "moved -- re-upload it."), ds))
      }
    }
  }

  list(ok = length(errors) == 0L, errors = errors)
}

# TRUE when a per-ome scalar setting is "not chosen": NULL, non-scalar, NA,
# empty, or the blank "(none)" default the species/condition/replicate selectors
# start at. @noRd
.pelsa_is_unset <- function(v) {
  is.null(v) || length(v) != 1L || is.na(v) || !nzchar(v) ||
    identical(as.character(v), "(none)")
}

# cdesc of a cmapR GCT (or NULL when not a GCT / unavailable). @noRd
.pelsa_gct_cdesc <- function(gct) {
  if (methods::is(gct, "GCT")) {
    return(methods::slot(gct, "cdesc"))
  }
  NULL
}

# ---- delinearize (raw-intensity recovery for CV) -----------------------------

# Recover LINEAR (raw) intensities from a (possibly) log-transformed matrix.
#
# WHY THIS EXISTS: Protigy's `GCTs_original` is NOT the raw uploaded matrix in
# linear space -- it is the matrix AFTER `perform_log_transformation`
# (R/sidebar_setup_helpers_GCT-processing.R), i.e. the LOG-transformed values
# when log2/log10 was selected. The PELSA within-condition CV
# (pelsa_within_condition_cv) is defined on RAW LINEAR intensities (the notebook
# delinearizes before CV; CV is NOT invariant under log).
# So the analysis pipeline must DELINEARIZE `GCTs_original` by the dataset's
# declared log base BEFORE feeding it to the CV path.
#
# The declared base comes from that dataset's setup parameters
# `log_transformation` in {"None","log2","log10"} (perform_log_transformation:
# log2 = log(x,2), log10 = log(x,10); "None" = no transform, and the
# negative-values fallback ALSO sets the method to "None"). Therefore:
#   - "None" / NA / missing  -> the matrix is ALREADY LINEAR; pass it through
#                               UNCHANGED (do NOT exponentiate -- that would
#                               corrupt an already-linear matrix).
#   - "log2"                 -> 2 ^ mat
#   - "log10"                -> 10 ^ mat
# NA stays NA (2^NA == NA), so missingness is preserved.
#
# PURE + closed-form testable: a function of (mat, log_base) only.
#
# @param mat       numeric matrix (or data.frame intensity block, coerced).
# @param log_base  one of "None"/NA/NULL (linear pass-through), "log2", "log10".
# @return numeric matrix in LINEAR space, same shape/dimnames as mat.
# @noRd
pelsa_delinearize <- function(mat, log_base) {
  if (is.data.frame(mat)) mat <- as.matrix(mat)
  if (!is.matrix(mat) || !is.numeric(mat)) {
    stop("pelsa_delinearize: `mat` must be a numeric matrix or data.frame.",
         call. = FALSE)
  }

  base <- if (is.null(log_base) || length(log_base) == 0L) {
    NA_character_
  } else {
    as.character(log_base)[[1]]
  }

  # Already-linear: "None", NA, missing, or empty -> pass through unchanged.
  if (is.na(base) || !nzchar(base) || identical(base, "None")) {
    return(mat)
  }
  if (identical(base, "log2"))  return(2 ^ mat)
  if (identical(base, "log10")) return(10 ^ mat)

  stop(sprintf(
    "pelsa_delinearize: unknown log_base '%s' (expected None/log2/log10).",
    base), call. = FALSE)
}

# ---- GCT -> peptide frame ----------------------------------------------------

# Build a peptide-level data.frame from a cmapR GCT: cbind(@rdesc, @mat).
#
# rdesc carries the peptide annotation columns (PG.ProteinAccessions, PG.Genes,
# PEP.StrippedSequence, PEP.PeptidePosition, ...); mat carries the per-sample
# intensities (cols == sample names). They share row order (rdesc rownames ==
# mat rownames == rid), so cbind aligns peptides to intensities row-for-row. A
# plain data.frame is passed straight through (the test seam: the synthetic
# generator already yields a peptide frame).
#
# Peptide-result exports key on PEP.StrippedSequence, so a peptide GCT normally
# carries that column. Some PELSA peptide datasets, however, were uploaded with
# the stripped sequence AS the id column (the rid / rownames) and so have no
# PEP.StrippedSequence column. To keep position-mapping working for them, when
# PEP.StrippedSequence is absent we synthesize it from the dataset's id column
# (rid). This is additive: a real PEP.StrippedSequence column is always kept.
#
# @param gct a cmapR GCT, or a plain data.frame (returned unchanged).
# @return a peptide-level data.frame (guaranteed to have PEP.StrippedSequence
#         whenever a sequence-bearing id column is available).
# @noRd
pelsa_dataset_peptide_frame <- function(gct) {
  if (is.data.frame(gct)) {
    return(.pelsa_ensure_stripped_sequence(gct, id_values = rownames(gct)))
  }
  if (!methods::is(gct, "GCT")) {
    stop("pelsa_dataset_peptide_frame: expected a cmapR GCT or a data.frame.",
         call. = FALSE)
  }
  rdesc <- methods::slot(gct, "rdesc")
  mat   <- methods::slot(gct, "mat")
  rid   <- methods::slot(gct, "rid")
  mat_df <- as.data.frame(mat, check.names = FALSE, stringsAsFactors = FALSE)
  out <- cbind(rdesc, mat_df)
  out <- .pelsa_ensure_stripped_sequence(out, id_values = rid)
  rownames(out) <- NULL
  out
}

# Guarantee a PEP.StrippedSequence column on a peptide frame.
#
# Peptide results normally use PEP.StrippedSequence as their id column, so for
# PELSA datasets that column may be absent (the stripped sequence sits in the id
# column / rid instead). When PEP.StrippedSequence is missing we copy it from
# the supplied id values so downstream position-mapping has a sequence to match.
# A frame that already has PEP.StrippedSequence is returned unchanged.
#
# @param df         a peptide-level data.frame.
# @param id_values  character vector of per-row id values (the rid / rownames),
#                   used as the stripped sequence when the column is absent. May
#                   be NULL (then the frame is returned unchanged).
# @return df, with a PEP.StrippedSequence column added when it was missing and
#         id_values supplies one per row.
# @noRd
.pelsa_ensure_stripped_sequence <- function(df, id_values = NULL) {
  if ("PEP.StrippedSequence" %in% colnames(df)) return(df)
  if (is.null(id_values) || length(id_values) != nrow(df)) return(df)
  df[["PEP.StrippedSequence"]] <- as.character(id_values)
  df
}

# Numeric sample matrix from a GCT (or the intensity block of a data.frame).
#
# For a GCT: the @mat (rows = peptides, cols = samples). For a plain data.frame
# (test seam): the columns named in `sample_cols`, coerced to a numeric matrix.
#
# @param gct          a cmapR GCT or a peptide data.frame.
# @param sample_cols  sample column names (used for the data.frame seam).
# @return numeric matrix with colnames == sample names, in `sample_cols` order
#         where derivable.
# @noRd
pelsa_dataset_matrix <- function(gct, sample_cols) {
  if (methods::is(gct, "GCT")) {
    return(methods::slot(gct, "mat"))
  }
  if (is.data.frame(gct)) {
    cols <- intersect(sample_cols, colnames(gct))
    if (length(cols) == 0L) {
      stop("pelsa_dataset_matrix: none of `sample_cols` found in data.frame.",
           call. = FALSE)
    }
    m <- as.matrix(gct[, cols, drop = FALSE])
    storage.mode(m) <- "double"
    return(m)
  }
  stop("pelsa_dataset_matrix: expected a cmapR GCT or a data.frame.",
       call. = FALSE)
}

# rid (row id) vector from a GCT (@rid) or a data.frame (rownames). NULL when the
# object carries no usable id (so callers can no-op gracefully).
# @noRd
.pelsa_gct_rids <- function(g) {
  if (methods::is(g, "GCT")) return(methods::slot(g, "rid"))
  if (is.data.frame(g))      return(rownames(g))
  NULL
}

# cid (column/sample id) vector from a GCT (@cid) or a data.frame (colnames).
# NULL when the object carries no usable id (so callers can no-op gracefully).
# @noRd
.pelsa_gct_cids <- function(g) {
  if (methods::is(g, "GCT")) return(methods::slot(g, "cid"))
  if (is.data.frame(g))      return(colnames(g))
  NULL
}

# Restrict the CV-source (ORIGINAL) GCT to the PROCESSED peptide set, BY id, so
# within-condition CV describes exactly the analyzed peptides -- regardless of
# any rows the processing pipeline dropped (missing/SD filters) or reordered.
# (M8/M9)
#
# Both GCTs share one rid namespace: the processed GCT is built from the original
# by row-dropping filters that preserve rownames, so the processed rid set is a
# subset of the original's. The rid IS the identifier column chosen at setup, so
# aligning by rid keeps the CV row set in lock-step with the analysis row set.
# Both ROWS (peptides) and COLUMNS (samples) are restricted/reordered to the
# processed set: peptides by rid, samples by cid. Restricting samples means CV
# reflects exactly the analyzed samples even when setup filtered some out -- this
# matches the QC CV tab's qc_cv_align_source() behavior for consistency. The
# condition map (built from the original's cdesc, then intersected with the
# aligned matrix's columns by the caller) stays valid because it is keyed by name.
#
# We subset the @mat/@rdesc/@rid (and @cdesc/@cid) slots directly rather than via
# cmapR::subset_gct: subset_gct requires an "id" meta column that
# programmatically-built GCTs in this app don't guarantee. Direct slot replacement
# on the (copy-on-modify) local is robust to that and needs no GCT re-validation.
#
# @param gct_original   the unprocessed (CV-source) GCT, or a data.frame seam.
# @param gct_processed  the processed GCT whose rid/cid sets are the target.
# @return gct_original restricted + reordered to the processed rids and cids.
#         Inputs with no usable rid (non-GCT, non-data.frame) are returned
#         unchanged. Sample (cid) restriction is skipped when either side lacks
#         usable cids.
# @noRd
pelsa_align_original_to_processed <- function(gct_original, gct_processed) {
  proc_rids <- .pelsa_gct_rids(gct_processed)
  orig_rids <- .pelsa_gct_rids(gct_original)
  if (is.null(proc_rids) || is.null(orig_rids)) return(gct_original)
  if (anyDuplicated(orig_rids)) {
    stop("pelsa_align_original_to_processed: original GCT has duplicate ids; ",
         "cannot align the CV source by id.", call. = FALSE)
  }
  keep <- proc_rids[proc_rids %in% orig_rids]
  idx  <- match(keep, orig_rids)               # no NAs: keep is a subset of orig

  # Samples to keep, in processed order. NULL (skip) when either side lacks cids.
  proc_cids <- .pelsa_gct_cids(gct_processed)
  orig_cids <- .pelsa_gct_cids(gct_original)
  keep_cols <- if (!is.null(proc_cids) && !is.null(orig_cids)) {
    proc_cids[proc_cids %in% orig_cids]
  } else {
    NULL
  }

  if (methods::is(gct_original, "GCT")) {
    mat   <- methods::slot(gct_original, "mat")[idx, , drop = FALSE]
    rdesc <- methods::slot(gct_original, "rdesc")[idx, , drop = FALSE]
    if (!is.null(keep_cols)) {
      cidx  <- match(keep_cols, orig_cids)     # no NAs: keep_cols subset of orig
      mat   <- mat[, cidx, drop = FALSE]
      methods::slot(gct_original, "cdesc") <-
        methods::slot(gct_original, "cdesc")[cidx, , drop = FALSE]
      methods::slot(gct_original, "cid")   <- as.character(keep_cols)
    }
    methods::slot(gct_original, "mat")   <- mat
    methods::slot(gct_original, "rdesc") <- rdesc
    methods::slot(gct_original, "rid")   <- as.character(keep)
    return(gct_original)
  }
  # data.frame seam (rows = peptides, cols = samples)
  out <- gct_original[keep, , drop = FALSE]
  if (!is.null(keep_cols)) out <- out[, keep_cols, drop = FALSE]
  out
}

# ---- condition map -----------------------------------------------------------

# Build the named condition map (sample -> condition) the CV helper consumes.
#
# For each sample column, look up its condition from cdesc[[condition_col]]
# (cdesc rownames are sample names). The result is a NAMED character vector keyed
# by sample so .pelsa_resolve_condition_map() can reorder it to the matrix's
# column order regardless of how the columns are arranged.
#
# Samples absent from cdesc, or with an NA condition, are dropped (they cannot be
# assigned to a condition). The CALLER subsets the matrix to the returned names.
#
# @param cdesc         the dataset's cdesc (rownames = sample names).
# @param sample_cols   the sample (matrix) column names to map.
# @param condition_col the chosen condition grouping column.
# @return named character vector sample -> condition (a subset of sample_cols).
# @noRd
pelsa_condition_map_for <- function(cdesc, sample_cols, condition_col) {
  if (!is.data.frame(cdesc)) {
    stop("pelsa_condition_map_for: `cdesc` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(condition_col) || length(condition_col) != 1L ||
      is.na(condition_col) || !nzchar(condition_col) ||
      !(condition_col %in% names(cdesc))) {
    stop(sprintf(
      "pelsa_condition_map_for: condition column '%s' not in cdesc.",
      condition_col), call. = FALSE)
  }
  sample_cols <- as.character(sample_cols)
  cond_all <- as.character(cdesc[[condition_col]])
  names(cond_all) <- rownames(cdesc)

  cond <- cond_all[sample_cols]            # NA name -> NA value
  keep <- !is.na(cond) & nzchar(cond)
  out <- cond[keep]
  names(out) <- sample_cols[keep]
  out
}

# ---- failure discriminator ---------------------------------------------------

# Canonical predicate: did a per-dataset cache entry FAIL?
#
# The ONE place that defines the success-vs-failure rule for entries in the
# pelsa_run_analysis() return list. A successful entry is the 12-component cache
# (see the Cache contract below); a failed entry is list(error = <message>,
# stage = <stage label or NA>). Phase 6/7 MUST test with this predicate rather
# than inlining `!is.null(entry$error)`, so the rule lives in one place.
#
# @param entry one element of the pelsa_run_analysis() return list.
# @return TRUE iff the entry represents a failed dataset.
# @noRd
pelsa_analysis_failed <- function(entry) {
  is.list(entry) && !is.null(entry$error)
}

# Reconstruct a cache entry's full annotated frame: `matched` cbound with the 3
# stored feature columns (annotation_features). The cache stores only the 3
# feature columns (row-aligned to `matched`) rather than a full annotated
# duplicate of `matched` (~27MB/dataset saved); this rebuilds the frame consumers
# previously read as `entry$annotation`.
#
# @param entry a SUCCESSFUL per-dataset cache entry (list with $matched and
#              $annotation_features). Behaviour on a failed entry is undefined;
#              callers should gate with pelsa_analysis_failed() first.
# @return data.frame = matched + feature_class_primary/winning_accession/
#         winning_gene, or NULL when the entry lacks the required components.
# @noRd
pelsa_annotation_frame <- function(entry) {
  if (!is.list(entry)) return(NULL)
  matched <- entry$matched
  feats   <- entry$annotation_features
  if (!is.data.frame(matched) || !is.data.frame(feats)) return(NULL)
  if (nrow(matched) != nrow(feats)) {
    stop("pelsa_annotation_frame: matched and annotation_features row counts ",
         "disagree (", nrow(matched), " vs ", nrow(feats), ")", call. = FALSE)
  }
  out <- matched
  rownames(out) <- NULL
  for (col in PELSA_ANNOTATION_FEATURE_COLS) {
    out[[col]] <- feats[[col]]
  }
  out
}

# ---- per-condition membership + distributions (Summary toggle) ---------------

# Peptide -> condition membership over the PROCESSED matrix.
#
# A peptide BELONGS to a condition when it is quantified (the canonical
# pelsa_quantified_mask: finite AND non-zero) in AT LEAST ONE of that condition's
# samples. Many-to-many: a peptide quantified across several conditions appears
# once per condition. Pure (no Shiny, no network).
#
# @param proc_mat      peptides x samples numeric matrix (colnames = samples).
# @param condition_map NAMED vector sample -> condition (pelsa_condition_map_for).
# @return data.frame(row_id = integer 1-based peptide-frame row, condition =
#         character), one row per (peptide, condition) membership. Empty when
#         there are no samples / no mapped conditions.
# @noRd
pelsa_condition_membership <- function(proc_mat, condition_map) {
  if (is.data.frame(proc_mat)) proc_mat <- as.matrix(proc_mat)
  empty <- data.frame(row_id = integer(0), condition = character(0),
                      stringsAsFactors = FALSE)
  if (!is.matrix(proc_mat) || ncol(proc_mat) == 0L || nrow(proc_mat) == 0L) {
    return(empty)
  }
  cm <- condition_map
  if (is.null(cm) || length(cm) == 0L) return(empty)
  keep <- !is.na(cm) & nzchar(as.character(cm))
  cm <- cm[keep]
  samples <- intersect(names(cm), colnames(proc_mat))
  if (length(samples) == 0L) return(empty)

  mask <- pelsa_quantified_mask(proc_mat[, samples, drop = FALSE])
  conds <- unique(as.character(cm[samples]))
  parts <- lapply(conds, function(cond) {
    cols <- samples[as.character(cm[samples]) == cond]
    inc <- rowSums(mask[, cols, drop = FALSE]) > 0   # quantified in >= 1 sample
    rid <- which(inc)
    if (length(rid) == 0L) return(NULL)
    data.frame(row_id = as.integer(rid), condition = cond,
               stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) return(empty)
  do.call(rbind, parts)
}

# Long per-condition peptide-length frame for the Summary length toggle.
#
# @param membership      pelsa_condition_membership() output.
# @param peptide_metrics the cache peptide_metrics frame (row-aligned to the
#                        peptide frame == membership$row_id index space).
# @return data.frame(condition, peptide_length). Empty when no membership.
# @noRd
pelsa_length_by_condition <- function(membership, peptide_metrics) {
  empty <- data.frame(condition = character(0), peptide_length = numeric(0),
                      stringsAsFactors = FALSE)
  if (is.null(membership) || !is.data.frame(membership) ||
      nrow(membership) == 0L) {
    return(empty)
  }
  if (is.null(peptide_metrics) || !is.data.frame(peptide_metrics) ||
      !("peptide_length" %in% names(peptide_metrics))) {
    return(empty)
  }
  len <- suppressWarnings(as.numeric(peptide_metrics$peptide_length))
  rid <- membership$row_id
  ok <- !is.na(rid) & rid >= 1L & rid <= length(len)
  data.frame(condition = as.character(membership$condition[ok]),
             peptide_length = len[rid[ok]],
             stringsAsFactors = FALSE)
}

# Long per-condition sequence-coverage frame for the Summary coverage toggle.
#
# For each condition it subsets the matched cache to the peptides quantified in
# that condition and runs the SAME interval-union coverage as the experiment-wide
# metric, keeping the finite per-accession coverage fractions. @noRd
pelsa_coverage_by_condition <- function(membership, matched, fasta_map,
                                        acc_col = "accession",
                                        start_col = "pep_start",
                                        end_col = "pep_end",
                                        row_id_col = ".row_id") {
  empty <- data.frame(condition = character(0), coverage = numeric(0),
                      stringsAsFactors = FALSE)
  if (is.null(membership) || !is.data.frame(membership) ||
      nrow(membership) == 0L) {
    return(empty)
  }
  if (!is.data.frame(matched) || nrow(matched) == 0L ||
      !(row_id_col %in% names(matched))) {
    return(empty)
  }
  m_rid <- suppressWarnings(as.integer(matched[[row_id_col]]))
  conds <- unique(as.character(membership$condition))
  parts <- lapply(conds, function(cond) {
    rids <- membership$row_id[membership$condition == cond]
    sub <- matched[m_rid %in% rids, , drop = FALSE]
    if (nrow(sub) == 0L) return(NULL)
    cov <- suppressWarnings(
      pelsa_sequence_coverage(sub, fasta_map, acc_col = acc_col,
                              start_col = start_col, end_col = end_col))
    v <- suppressWarnings(as.numeric(cov$coverage))
    v <- v[is.finite(v)]
    if (length(v) == 0L) return(NULL)
    data.frame(condition = cond, coverage = v, stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) return(empty)
  do.call(rbind, parts)
}

# Per-sample sequence coverage: for each sample, the MEAN per-protein coverage
# fraction across all proteins with >= 1 peptide quantified (finite &
# non-zero) in that sample. Mean-of-ratios (not a pooled ratio-of-sums):
# each protein observed in the sample contributes one coverage value
# (computed from ONLY that sample's quantified peptide spans), and those
# values are averaged.
#
# @param proc_mat  peptides x samples numeric matrix (colnames = samples).
# @param matched   the cache `matched` data.frame (accession/pep_start/
#                  pep_end/.row_id), row_id 1-based into the peptide frame
#                  (== proc_mat row index space).
# @param fasta_map named list accession -> sequence.
# @return data.frame(sample, coverage, n_proteins). coverage is NA when
#         n_proteins == 0 for that sample. Empty frame when proc_mat has 0
#         columns.
# @noRd
pelsa_coverage_by_sample <- function(proc_mat, matched, fasta_map,
                                     acc_col = "accession",
                                     start_col = "pep_start",
                                     end_col = "pep_end",
                                     row_id_col = ".row_id") {
  empty <- data.frame(sample = character(0), coverage = numeric(0),
                      n_proteins = integer(0), stringsAsFactors = FALSE)
  if (is.data.frame(proc_mat)) proc_mat <- as.matrix(proc_mat)
  if (!is.matrix(proc_mat) || ncol(proc_mat) == 0L) return(empty)
  samples <- colnames(proc_mat)

  if (!is.data.frame(matched) || nrow(matched) == 0L ||
      !(row_id_col %in% names(matched))) {
    return(data.frame(sample = samples, coverage = rep(NA_real_, length(samples)),
                      n_proteins = rep(0L, length(samples)),
                      stringsAsFactors = FALSE))
  }
  m_rid <- suppressWarnings(as.integer(matched[[row_id_col]]))
  mask <- pelsa_quantified_mask(proc_mat)

  parts <- lapply(samples, function(s) {
    rid <- which(mask[, s])
    sub <- matched[m_rid %in% rid, , drop = FALSE]
    if (nrow(sub) == 0L) {
      return(data.frame(sample = s, coverage = NA_real_, n_proteins = 0L,
                        stringsAsFactors = FALSE))
    }
    cov <- suppressWarnings(
      pelsa_sequence_coverage(sub, fasta_map, acc_col = acc_col,
                              start_col = start_col, end_col = end_col))
    v <- suppressWarnings(as.numeric(cov$coverage))
    v <- v[is.finite(v)]
    if (length(v) == 0L) {
      data.frame(sample = s, coverage = NA_real_, n_proteins = 0L,
                stringsAsFactors = FALSE)
    } else {
      data.frame(sample = s, coverage = mean(v), n_proteins = length(v),
                stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, parts)
}

# ---- per-dataset assembly ----------------------------------------------------

# Assemble one dataset's per-dataset analysis cache from the verified helpers.
#
# This is the stat-INDEPENDENT object set both Summary and Volcano reuse:
# FASTA-mapped matched/unmatched cache, within-condition CV, depth, sequence
# coverage, missed-cleavage + peptide-length, feature annotation, and mapping /
# annotation QC counts. The volcano's stat-DEPENDENT rollup is Phase 7's job and
# is NOT computed here.
#
# @section Cache contract:
# The returned named list is the load-bearing contract Phases 6 (Summary) and 7
# (Volcano) READ (never recompute). On SUCCESS it has exactly these 17
# components (EXACT names + shapes as implemented):
#   (NOTE: the former full-duplicate `annotation` frame is no longer stored; the
#   cache now carries `annotation_features` - just the 3 feature columns,
#   row-aligned to `matched` - and pelsa_annotation_frame(entry) reconstructs the
#   full annotated frame on demand. ~27MB/dataset saved.)
#   matched        data.frame, one row per (peptide, accession, occurrence) that
#                  FASTA-mapped. Key cols: accession, pep_start, pep_end (1-based
#                  inclusive), pep_occurrence_idx, n_occurrences,
#                  PEP.StrippedSequence, gene, .row_id (1-based row index into the
#                  peptide frame), plus all carried-through peptide-frame cols.
#   unmatched      data.frame, one row per peptide x accession that did NOT map.
#                  Cols: peptide_sequence, accession, gene, pep_position, reason
#                  (one of accession_absent / sequence_not_found /
#                  bad_sequence_format).
#   cv             data.frame OR NULL. One row per (peptide, condition). Cols:
#                  row_id (1-based row index into the raw matrix == peptide frame
#                  row), condition, cv_pct, n_nonNA, cv_status (ok /
#                  insufficient_replicates / non_finite). NULL when there is no
#                  raw GCT, or the condition column is absent / all-NA.
#   n_quantified   NAMED integer vector, one per sample (names = sample columns),
#                  count of quantified peptides per sample.
#   depth_summary  one-row data.frame. Cols: mean_n, median_n, cv_pct,
#                  total_n_peptides (== nrow(peptide frame)).
#   coverage       data.frame, one row per DISTINCT matched accession. Cols:
#                  accession, covered_residues, protein_length, coverage ([0,1] or
#                  NA), over_length_flag.
#   coverage_by_condition data.frame(condition, coverage), the per-protein
#                  coverage fraction split by condition (a peptide belongs to a
#                  condition when quantified in >= 1 of its samples). Empty frame
#                  when no usable processed condition column. Feeds the Summary
#                  coverage panel's per-condition toggle mode.
#   n_peptides_by_condition NAMED integer vector (condition -> count) of peptides
#                  QUANTIFIED (canonical finite & non-zero) in >= 1 sample of the
#                  condition -- the same membership coverage_by_condition uses.
#                  Empty when no usable processed condition column. Feeds the
#                  Summary condition table's n_peptides_quantified column.
#   peptide_metrics data.frame, one row per peptide-frame row. Cols:
#                  PEP.StrippedSequence, missed_cleavages, peptide_length.
#   length_by_condition data.frame(condition, peptide_length), peptide lengths
#                  split by condition (same membership rule as
#                  coverage_by_condition). Empty frame when no usable processed
#                  condition column. Feeds the Summary length panel's
#                  per-condition toggle mode.
#   annotation_features data.frame, row-aligned to `matched`, with exactly 3
#                  columns: feature_class_primary, winning_accession,
#                  winning_gene. The full annotated frame (matched + these 3) is
#                  reconstructed on demand via pelsa_annotation_frame(entry); the
#                  cache does NOT store the full duplicate.
#   unannotated    character vector of accessions present in the matched cache but
#                  ABSENT from feat_df (isoform-base fallback applied).
#   qc             list: n_peptides, n_fully_quantified (peptides quantified --
#                  finite & non-zero -- in ALL samples), n_exploded,
#                  n_matched_rows, n_unmatched_rows,
#                  unmatched_by_reason (named list reason -> count),
#                  n_unannotated_accessions,
#                  n_annotated_with_features (accessions with >=1 real feature
#                  row; isoform-base fallback applied),
#                  n_annotated_zero_feature (accessions present only as sentinel
#                  rows in feat_df).
#   missed_cleavage_rate_by_sample data.frame(sample, rate, n_quantified),
#                  one row per sample column of the PROCESSED matrix. rate is
#                  the fraction of that sample's quantified (finite &
#                  non-zero) peptides with >= 1 missed cleavage; NA when
#                  n_quantified == 0 for that sample.
#   length_by_sample data.frame(sample, mean_length, n_quantified), one row
#                  per sample column. mean_length is the mean residue length
#                  of peptides quantified in that sample; NA when
#                  n_quantified == 0.
#   coverage_by_sample data.frame(sample, coverage, n_proteins), one row per
#                  sample column. coverage is the MEAN per-protein coverage
#                  fraction across proteins with >= 1 peptide quantified in
#                  that sample (mean-of-ratios); NA when n_proteins == 0.
#   condition_map  NAMED character vector, sample -> condition, over the
#                  PROCESSED matrix's sample columns (same set condition_map
#                  membership above is built from). Empty named character
#                  vector when there is no usable condition column.
#
# A FAILED dataset is instead list(error = <message>, stage = <last stage label
# reached, or NA>). Test with pelsa_analysis_failed(entry); the stage names the
# pipeline phase that threw (e.g. "Computing CV").
#
# GRACEFUL ZERO-MATCH: a dataset whose peptides do not FASTA-map at all is NOT an
# error - it returns a valid cache with qc$n_matched_rows == 0L (empty matched /
# coverage / annotation). Phase 6 should check qc$n_matched_rows > 0L before
# drawing coverage.
#
# @param gct           the PROCESSED GCT (or peptide data.frame) for this ds.
# @param gct_original  the GCT (or frame) Protigy stored as `GCTs_original` for
#                      this ds - the CV source. NOTE: this is the
#                      LOG-TRANSFORMED matrix (post perform_log_transformation),
#                      NOT raw linear, so the CV path DELINEARIZES it by
#                      `log_base` first (pelsa_delinearize) to recover the raw
#                      linear intensities CV is defined on. May be NULL (then CV
#                      is skipped, cv = NULL).
# @param fasta_map     named list accession -> sequence (read once by caller).
# @param feat_df       the species feature cache (read once by caller); used
#                      as-is (cache-as-is decision - no UniProt top-up).
# @param condition_col the chosen condition grouping column for this dataset.
# @param min_nonNA     min non-NA replicates for a finite CV (passed to 2D).
# @param log_base      this dataset's declared log transformation, one of
#                      "None"/NA (already linear), "log2", "log10". The CV input
#                      (gct_original's matrix) is delinearized by it BEFORE
#                      CV. The DEPTH metric and intensity-line
#                      use the PROCESSED log2 matrix as-is and are NOT affected.
# @param progress      NULL or a function(detail) advancing a sub-progress stage.
# @param stage_env     NULL or an environment whose $stage the assembly updates
#                      to the current stage label (so a caller's tryCatch can
#                      report WHICH stage failed). Internal seam.
# @return a named list (the per-dataset cache); see the Cache contract above.
# @noRd
pelsa_run_analysis_one <- function(gct,
                                   gct_original,
                                   fasta_map,
                                   feat_df,
                                   condition_col,
                                   min_nonNA = 3L,
                                   log_base = NA_character_,
                                   progress = NULL,
                                   stage_env = NULL) {
  .step <- function(detail) {
    if (is.environment(stage_env)) stage_env$stage <- detail
    if (is.function(progress)) progress(detail)
  }
  .step("Reading dataset")

  if (!is.list(fasta_map)) {
    stop("pelsa_run_analysis_one: `fasta_map` must be a (named) list.",
         call. = FALSE)
  }
  if (!is.data.frame(feat_df)) {
    stop("pelsa_run_analysis_one: `feat_df` must be a data.frame.",
         call. = FALSE)
  }

  peptides <- pelsa_dataset_peptide_frame(gct)

  .step("Mapping peptide positions")
  mapping <- .pelsa_map_and_annotate(peptides, fasta_map, feat_df)

  cond <- .pelsa_run_one_resolve_condition(gct, gct_original, condition_col)

  .step("Computing CV")
  cv <- .pelsa_compute_cv(gct, gct_original, peptides, cond, log_base,
                          min_nonNA)

  .step("Building coverage and depth")
  depth <- .pelsa_build_depth_and_coverage(gct, peptides, mapping$matched,
                                           fasta_map)

  per_condition <- .pelsa_build_per_condition_metrics(
    depth$proc_mat, cond, depth$peptide_metrics, mapping$matched, fasta_map)

  qc <- .pelsa_build_qc_counts(peptides, mapping, depth)

  per_sample <- .pelsa_build_per_sample_metrics(
    depth$proc_mat, depth$peptide_metrics, mapping$matched, fasta_map, cond)

  list(
    matched             = mapping$matched,
    unmatched           = mapping$unmatched,
    cv                  = cv,
    n_quantified        = depth$n_quantified,
    depth_summary       = depth$depth_summary,
    coverage            = depth$coverage,
    coverage_by_condition = per_condition$coverage_by_condition,
    n_peptides_by_condition = per_condition$n_peptides_by_condition,
    peptide_metrics     = depth$peptide_metrics,
    length_by_condition = per_condition$length_by_condition,
    annotation_features = mapping$annotation_features,
    feat_raw            = feat_df,
    unannotated         = mapping$unannotated,
    qc                  = qc,
    missed_cleavage_rate_by_sample = per_sample$missed_cleavage_rate_by_sample,
    length_by_sample    = per_sample$length_by_sample,
    coverage_by_sample  = per_sample$coverage_by_sample,
    condition_map       = per_sample$condition_map
  )
}

# --- 2A explode -> 2B FASTA-map -> 2I feature annotation (cache-as-is) ------
# Maps peptides onto FASTA positions and annotates the matched cache with
# feature classes. Returns matched/unmatched frames plus the 3-column
# annotation cache (see pelsa_run_analysis_one's Cache contract).
# @noRd
.pelsa_map_and_annotate <- function(peptides, fasta_map, feat_df) {
  exploded <- pelsa_explode_accessions(peptides)
  mapped   <- pelsa_map_peptide_positions(exploded, fasta_map)
  matched   <- mapped$matched
  unmatched <- mapped$unmatched

  # Annotate the MATCHED cache (peptide x accession w/ pep_start/pep_end). The
  # annotated frame is `matched` PLUS exactly 3 feature columns
  # (feature_class_primary, winning_accession, winning_gene), row-aligned to
  # `matched`. We store ONLY those 3 columns (not the full annotated duplicate of
  # `matched`, which wasted ~27MB/dataset) and reconstruct the full frame on
  # demand via pelsa_annotation_frame(entry).
  annotation <- pelsa_annotate_features(matched, feat_df)
  annotation_features <- annotation[, PELSA_ANNOTATION_FEATURE_COLS, drop = FALSE]
  rownames(annotation_features) <- NULL
  unannotated <- pelsa_unannotated_accessions(matched, feat_df)
  annotation_status <- pelsa_annotation_status_counts(matched, feat_df)

  list(
    exploded = exploded,
    matched = matched,
    unmatched = unmatched,
    annotation_features = annotation_features,
    unannotated = unannotated,
    annotation_status = annotation_status
  )
}

# Resolves the CANONICAL sample -> condition annotation, shared by the CV
# panel (2D) AND the per-condition membership (Summary toggle) so both
# describe the SAME sample -> condition mapping. Prefer the ORIGINAL GCT's
# cdesc (CV's source of truth); fall back to the processed GCT's cdesc for the
# data.frame seam or when the original lacks the column. Each consumer
# intersects this map with its own matrix's columns, so a sample filtered out
# of one matrix simply drops from that panel without desyncing the condition
# labels.
# @return list(cdesc_cond, condition_col, has_cond_col).
# @noRd
.pelsa_run_one_resolve_condition <- function(gct, gct_original, condition_col) {
  cdesc_cond <- if (!is.null(gct_original) && methods::is(gct_original, "GCT")) {
    methods::slot(gct_original, "cdesc")
  } else {
    NULL
  }
  # condition_col is NULL/absent for any dataset with no condition column set
  # (condition_cols[[ds]] -> NULL). `NULL %in% x` is logical(0), and `||`/`&&`
  # with a length-0 operand yields NA -> `if (NA)` crashes the whole dataset's
  # analysis. Guard the arg to a single non-empty string before the %in% test.
  cc_ok <- is.character(condition_col) && length(condition_col) == 1L &&
    !is.na(condition_col) && nzchar(condition_col)
  if (is.null(cdesc_cond) || !is.data.frame(cdesc_cond) || !cc_ok ||
      !(condition_col %in% names(cdesc_cond))) {
    cdesc_cond <- .pelsa_gct_cdesc(gct)
  }
  has_cond_col <- cc_ok && is.data.frame(cdesc_cond) &&
    condition_col %in% names(cdesc_cond)

  list(cdesc_cond = cdesc_cond, condition_col = condition_col,
       has_cond_col = has_cond_col)
}

# --- 2D within-condition CV on the DELINEARIZED (raw linear) intensities ----
# GCTs_original is LOG-transformed (Protigy stores the post-log matrix), so we
# delinearize by this dataset's declared log base BEFORE CV. CV is NOT
# invariant under log; the notebook delinearizes first. "None"/NA means the
# matrix is already linear -> pelsa_delinearize passes it through.
# @param cond the list returned by .pelsa_run_one_resolve_condition.
# @noRd
.pelsa_compute_cv <- function(gct, gct_original, peptides, cond, log_base,
                              min_nonNA) {
  if (is.null(gct_original)) {
    return(NULL)
  }
  # M8/M9: restrict the CV source to the PROCESSED set BY id, so CV describes
  # exactly the analyzed peptides AND samples (processing may drop/reorder rows
  # and filter samples). cond$cdesc_cond remains valid -- it is keyed by sample
  # name and the cmap below intersects it with the aligned matrix's columns.
  gct_original <- pelsa_align_original_to_processed(gct_original, gct)
  log_mat <- pelsa_dataset_matrix(gct_original, colnames(peptides))
  raw_mat <- pelsa_delinearize(log_mat, log_base)
  if (!cond$has_cond_col) {
    return(NULL)
  }
  cmap <- pelsa_condition_map_for(cond$cdesc_cond, colnames(raw_mat),
                                  cond$condition_col)
  if (length(cmap) == 0L) {
    return(NULL)
  }
  sub <- raw_mat[, names(cmap), drop = FALSE]
  pelsa_within_condition_cv(sub, cmap, min_nonNA = min_nonNA)
}

# --- 2E peptides-per-sample depth + 2F sequence coverage + 2C missed
# cleavage/peptide length over the peptide universe, all on the PROCESSED
# matrix. @noRd
.pelsa_build_depth_and_coverage <- function(gct, peptides, matched,
                                            fasta_map) {
  proc_mat <- pelsa_dataset_matrix(gct, colnames(peptides))
  n_quantified <- pelsa_peptides_per_sample(proc_mat)
  depth_summary <- pelsa_depth_summary(n_quantified,
                                       total_n_peptides = nrow(peptides))

  # Fully-quantified peptides: rows quantified (the canonical pelsa_quantified_
  # mask: finite & non-zero) in EVERY sample. 0 when there are no samples.
  n_fully_quantified <- if (ncol(proc_mat) == 0L) {
    0L
  } else {
    sum(rowSums(!pelsa_quantified_mask(proc_mat)) == 0L)
  }

  coverage <- pelsa_sequence_coverage(matched, fasta_map)

  seqs <- if ("PEP.StrippedSequence" %in% colnames(peptides)) {
    as.character(peptides[["PEP.StrippedSequence"]])
  } else {
    character(0)
  }
  peptide_metrics <- data.frame(
    PEP.StrippedSequence = seqs,
    missed_cleavages     = pelsa_missed_cleavages(seqs),
    peptide_length       = pelsa_peptide_length(seqs),
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )

  list(
    proc_mat = proc_mat,
    n_quantified = n_quantified,
    depth_summary = depth_summary,
    n_fully_quantified = n_fully_quantified,
    coverage = coverage,
    peptide_metrics = peptide_metrics
  )
}

# --- per-condition length / coverage (Summary toggle) -----------------------
# Membership over the PROCESSED matrix, keyed by the CANONICAL condition map
# (cond$cdesc_cond, shared with the CV panel) so the per-condition Summary
# panels and the CV panel agree on which samples belong to each condition. A
# peptide belongs to a condition when quantified in >= 1 of its samples.
# Empty frames when there is no usable condition column.
# @noRd
.pelsa_build_per_condition_metrics <- function(proc_mat, cond,
                                               peptide_metrics, matched,
                                               fasta_map) {
  length_by_condition <- data.frame(condition = character(0),
                                    peptide_length = numeric(0),
                                    stringsAsFactors = FALSE)
  coverage_by_condition <- data.frame(condition = character(0),
                                      coverage = numeric(0),
                                      stringsAsFactors = FALSE)
  # Per-condition QUANTIFIED peptide count (canonical finite & non-zero in >= 1
  # sample), counted from the same membership the coverage/length panels use, so
  # the Summary "n_peptides_quantified" column means the same "quantified" as the
  # per-sample summary. NAMED integer vector (condition -> count); empty when no
  # usable condition column.
  n_peptides_by_condition <- integer(0)
  if (cond$has_cond_col) {
    cmap_proc <- pelsa_condition_map_for(cond$cdesc_cond, colnames(proc_mat),
                                         cond$condition_col)
    if (length(cmap_proc) > 0L) {
      membership <- pelsa_condition_membership(proc_mat, cmap_proc)
      length_by_condition <- pelsa_length_by_condition(membership,
                                                       peptide_metrics)
      coverage_by_condition <- pelsa_coverage_by_condition(membership, matched,
                                                           fasta_map)
      if (is.data.frame(membership) && nrow(membership) > 0L) {
        n_peptides_by_condition <- table(as.character(membership$condition))
        n_peptides_by_condition <- stats::setNames(
          as.integer(n_peptides_by_condition), names(n_peptides_by_condition))
      }
    }
  }

  list(length_by_condition = length_by_condition,
       coverage_by_condition = coverage_by_condition,
       n_peptides_by_condition = n_peptides_by_condition)
}

# --- mapping / annotation QC counts -----------------------------------------
# @param mapping the list returned by .pelsa_map_and_annotate.
# @param depth   the list returned by .pelsa_build_depth_and_coverage.
# @noRd
.pelsa_build_qc_counts <- function(peptides, mapping, depth) {
  reasons <- if ("reason" %in% colnames(mapping$unmatched)) {
    as.character(mapping$unmatched$reason)
  } else {
    character(0)
  }
  annotation_status <- mapping$annotation_status
  list(
    n_peptides            = nrow(peptides),
    n_fully_quantified    = depth$n_fully_quantified,
    n_exploded            = nrow(mapping$exploded),
    n_matched_rows        = nrow(mapping$matched),
    n_unmatched_rows      = nrow(mapping$unmatched),
    unmatched_by_reason   = as.list(c(table(reasons))),
    n_unannotated_accessions    = length(mapping$unannotated),
    n_annotated_with_features   = annotation_status$n_with_features,
    n_annotated_zero_feature    = annotation_status$n_zero_feature,
    # Disposition buckets from a self-describing annotation (0 unless the
    # uploaded annotation carries a `disposition` column). merged/demerged/
    # deleted accessions are "excluded for a reason", NOT failures -- so
    # n_failed is the true residual (0 when every accession is accounted).
    n_annotated_merged          = annotation_status$n_merged %||% 0L,
    n_annotated_demerged        = annotation_status$n_demerged %||% 0L,
    n_annotated_deleted         = annotation_status$n_deleted %||% 0L,
    n_annotation_failed         = annotation_status$n_failed %||% 0L
  )
}

# --- per-sample QC metrics (missed-cleavage rate, length, coverage) ---------
# Built from the same proc_mat / matched / peptide_metrics already assembled
# by the caller. condition_map mirrors cmap_proc's construction (or an empty
# named character vector when there is no usable condition column), so the
# Summary dashboard reads a ready-made sample -> condition map instead of
# recomputing it at render time.
# @noRd
.pelsa_build_per_sample_metrics <- function(proc_mat, peptide_metrics,
                                            matched, fasta_map, cond) {
  missed_cleavage_rate_by_sample <- pelsa_missed_cleavage_rate_by_sample(
    proc_mat, peptide_metrics)
  length_by_sample <- pelsa_length_by_sample(proc_mat, peptide_metrics)
  coverage_by_sample <- pelsa_coverage_by_sample(proc_mat, matched, fasta_map)
  condition_map <- if (cond$has_cond_col) {
    pelsa_condition_map_for(cond$cdesc_cond, colnames(proc_mat),
                            cond$condition_col)
  } else {
    stats::setNames(character(0), character(0))
  }

  list(
    missed_cleavage_rate_by_sample = missed_cleavage_rate_by_sample,
    length_by_sample = length_by_sample,
    coverage_by_sample = coverage_by_sample,
    condition_map = condition_map
  )
}

# Run the full compute pipeline for ALL checked datasets (the public entry the
# observer calls under withProgress). PURE-ish: NO Shiny, NO network. Each
# dataset supplies its OWN uploaded FASTA + annotation file, resolved by the
# dataset name (see resolve_fasta/resolve_feat), MEMOIZED per dataset.
#
# @param gcts           named list of PROCESSED GCTs (or frames), keyed by ds.
# @param gcts_original  named list of GCTs (or frames) Protigy stored as
#                       `GCTs_original`, keyed by ds (the CV source). These are
#                       LOG-TRANSFORMED (post perform_log_transformation), so the
#                       CV path DELINEARIZES each by `log_base_by_ds[[ds]]`
#                       (pelsa_delinearize) before CV. May be
#                       NULL / missing a ds (CV skipped).
# @param setup_snapshot pelsa_setup_snapshot() list (datasets + per-ds
#                       condition_col + per-ds uploads).
# @param fasta_map      LEGACY single-map fallback: a named list accession ->
#                       sequence used for EVERY dataset when resolve_fasta is NULL
#                       (a single-map run / the existing tests). Ignored when
#                       resolve_fasta is supplied.
# @param feat_df        LEGACY single-map fallback feature cache data.frame,
#                       used when resolve_feat is NULL. Ignored when resolve_feat
#                       is supplied.
# @param resolve_fasta  NULL or function(ds) -> fasta map for that dataset.
#                       When given, the FASTA is resolved PER DATASET (memoized
#                       per ds). The observer wraps the uploaded-file read; tests
#                       inject a map lookup.
# @param resolve_feat   NULL or function(ds) -> feature-cache data.frame for that
#                       dataset (same per-dataset memoization as resolve_fasta).
# @param min_nonNA      min non-NA replicates for a finite CV.
# @param log_base_by_ds named list/character keyed by ds giving each dataset's
#                       declared log transformation ("None"/NA/"log2"/"log10").
#                       Sourced from GCTs_and_params()$parameters[[ds]]$
#                       log_transformation. A ds absent here defaults to "None"
#                       (treated as already-linear). ONLY the CV input is
#                       delinearized; depth + intensity-line stay on the
#                       processed log2 matrix.
# @param set_progress   NULL or function(value, detail) advancing an overall
#                       0..1 progress bar; each dataset occupies an equal slice.
# @return named-by-dataset list of per-dataset cache objects (see the Cache
#         contract on pelsa_run_analysis_one), one entry per REQUESTED dataset in
#         request order. Datasets that error -- OR that were requested but are
#         absent from `gcts` -- are captured as list(error = <message>, stage =
#         <last stage label or NA>) so one failure never aborts the rest; test
#         entries with pelsa_analysis_failed(). Only a genuinely empty request
#         (no datasets) stops.
# @noRd
pelsa_run_analysis <- function(gcts,
                               gcts_original,
                               setup_snapshot,
                               fasta_map = NULL,
                               feat_df = NULL,
                               resolve_fasta = NULL,
                               resolve_feat = NULL,
                               min_nonNA = 3L,
                               log_base_by_ds = NULL,
                               set_progress = NULL) {
  datasets <- setup_snapshot$datasets %||% character(0)
  datasets <- as.character(datasets)
  datasets <- datasets[!is.na(datasets) & nzchar(datasets)]
  # Keep ALL requested datasets (do NOT silently drop ones absent from `gcts`).
  # An absent dataset is surfaced as a structured failure entry below, so the
  # Summary/Volcano sections can label it as failed rather than looking up a
  # NULL cache with no explanation (the caller advertises every requested
  # dataset to the switcher). `present` marks which can actually be analyzed.
  present <- datasets %in% names(gcts)

  if (length(datasets) == 0L) {
    stop("pelsa_run_analysis: no checked datasets to analyze.", call. = FALSE)
  }

  # PER-DATASET resolution. Each dataset supplies its OWN uploaded FASTA +
  # annotation file, resolved by the dataset name via the caller's
  # resolve_fasta(ds)/resolve_feat(ds) closures (the observer reads the uploaded
  # temp paths; tests inject maps). Results are MEMOIZED per ds. When no resolvers
  # are given, fall back to a single shared fasta_map/feat_df (the legacy
  # single-map path the existing tests + a single-dataset run use).
  fasta_cache <- new.env(parent = emptyenv())
  feat_cache  <- new.env(parent = emptyenv())
  resolve_one <- function(cache, resolver, shared, ds) {
    if (is.null(resolver)) return(shared)
    key <- as.character(ds)
    if (is.null(cache[[key]])) cache[[key]] <- list(value = resolver(ds))
    cache[[key]]$value
  }

  condition_cols <- setup_snapshot$condition_col %||% list()
  n <- length(datasets)
  out <- vector("list", n)
  names(out) <- datasets

  for (k in seq_along(datasets)) {
    ds <- datasets[[k]]

    # A requested dataset with no GCT in `gcts` cannot be analyzed; record a
    # structured failure entry (same shape as a compute failure) instead of
    # dropping it, so the Summary surfaces the gap.
    if (!present[[k]]) {
      out[[ds]] <- list(
        error = sprintf("dataset '%s' not found in processed GCTs", ds),
        stage = NA_character_
      )
      if (!is.null(set_progress)) set_progress(k / n, NULL)
      next
    }

    base_frac <- (k - 1L) / n
    sub_progress <- if (is.null(set_progress)) NULL else function(detail) {
      set_progress(base_frac, sprintf("(%d/%d) %s - %s", k, n, ds, detail))
    }

    # Track the last stage reached so a failure reports WHICH phase threw
    # (e.g. "dataset X failed during Computing CV"). The env is updated by
    # .step() inside pelsa_run_analysis_one.
    stage_env <- new.env(parent = emptyenv())
    stage_env$stage <- NA_character_

    ds_log_base <- if (is.null(log_base_by_ds)) NA_character_
                   else log_base_by_ds[[ds]] %||% NA_character_

    # Per-dataset uploads: the FASTA + annotation file are resolved by the
    # dataset name itself (resolve_fasta(ds)/resolve_feat(ds)), memoized per ds.
    # When no resolvers are given, fall back to the shared fasta_map/feat_df (the
    # legacy single-map path the older tests use). These resolvers can stop()
    # on a malformed upload (bad FASTA/annotation), so they run INSIDE the
    # per-dataset tryCatch: a bad upload for one dataset must not abort the
    # whole batch (see per-dataset error entry above for the not-found case).
    out[[ds]] <- tryCatch(
      {
        ds_fasta <- resolve_one(fasta_cache, resolve_fasta, fasta_map, ds)
        ds_feat  <- resolve_one(feat_cache,  resolve_feat,  feat_df,   ds)

        pelsa_run_analysis_one(
          gct          = gcts[[ds]],
          gct_original = if (is.list(gcts_original)) gcts_original[[ds]] else NULL,
          fasta_map    = ds_fasta,
          feat_df      = ds_feat,
          condition_col = condition_cols[[ds]],
          min_nonNA    = min_nonNA,
          log_base     = ds_log_base,
          progress     = sub_progress,
          stage_env    = stage_env
        )
      },
      error = function(e) list(error = conditionMessage(e),
                               stage = stage_env$stage)
    )

    if (!is.null(set_progress)) set_progress(k / n, NULL)
  }

  out
}
