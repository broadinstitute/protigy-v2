################################################################################
# Module: PELSA per-species UniProt-annotation refresh - pure/orchestration
# helpers (Task 5C).
#
# The Setup tab carries a MAINTENANCE control (a species checklist + a "Refresh
# per-species UniProt annotation library" button) that rebuilds a species'
# feature cache by wiring the 2H UniProt fetch + the 2I/parser classifier into
# the on-disk cache that 2I reads. The derived caches
# (inst/database/<species>/uniprot_features/uniprot_features.tsv + schema.json,
# .parquet) are gitignored/regenerable - THIS control is exactly how a user
# regenerates them.
#
# Everything network- or reactivity-bound is kept OUT of this file. The three
# helpers below are pure (or pure-ish with an INJECTED fetcher), so they unit
# test with NO live network. The module observer (tab_pelsa_section1.R) is the
# only thing that calls the REAL pelsa_fetch_uniprot + withProgress.
#
# Public helpers (all @noRd):
#   pelsa_refresh_accession_universe(gcts, existing_cache, fasta_map = NULL)
#       -> character vector: the accessions to (re)fetch for a species.
#   pelsa_write_feature_cache(feature_df, species_dir)
#       -> writes uniprot_features.tsv + schema.json; returns the .tsv path.
#   pelsa_refresh_species_cache(species, universe, species_dir,
#                               fetch_fn = pelsa_fetch_uniprot, progress = NULL)
#       -> orchestration: fetch -> write -> list(features=, unresolved=, path=,
#          n_features=, n_unresolved=). fetch_fn is the TESTABILITY SEAM (a stub
#          returns a canned 8-col frame so tests never hit the network).
#
# ACCESSION UNIVERSE (decided + documented)
#   The recommended/implemented universe is the set actually NEEDED:
#     unique exploded accessions across the UPLOADED datasets'
#       PG.ProteinAccessions (the proteins that will be annotated)
#     UNION the accessions already present in the existing cache (so a refresh
#       never LOSES coverage the user already had).
#   FALLBACK: when no datasets are uploaded yet (gcts is empty/NULL), fall back
#   to the FASTA accessions (fasta_map names) so a maintenance refresh on a
#   fresh install still has a universe. The FASTA universe can be LARGE (whole
#   proteome -> a minutes-to-hours fetch); the caller WARNS + the observer caps
#   nothing automatically but surfaces the size before fetching.
#
# MEMBRANE TSV (scoped OUT - documented follow-up)
#   inst/database/<species>/uniprot_membrane/*.tsv is a SEPARATE, differently
#   sourced annotation (a *_membraneLoc.tsv export, NOT produced by
#   pelsa_fetch_uniprot's feature classifier). 2I uses it only for a TM
#   fallback. 5C rebuilds ONLY the uniprot_features cache; the membrane file is
#   left untouched. TODO(5x): a membrane-refresh path if/when its upstream build
#   is ported.
#
# INSTALLED vs DEV write location (documented)
#   species_dir is resolved by the caller via
#   file.path(system.file("database", package = "Protigy"), <species>). In
#   dev/load_all that is the repo's inst/database/<species> (the .tsv there is
#   gitignored - fine). In an INSTALLED package it is the installed library
#   location, which MAY be read-only; pelsa_write_feature_cache fails fast with
#   a CLEAR error (never a crash) when the target dir is not writable.
################################################################################

# ---- Accession-extraction internals -----------------------------------------

# Pull the raw (possibly ;-delimited) PG.ProteinAccessions strings out of ONE
# uploaded dataset. A dataset is either a cmapR GCT (accessions live in @rdesc)
# or a plain data.frame (test seam / already-melted frame); either way we read
# the PG.ProteinAccessions column. Returns character(0) when the column is
# absent (a non-PELSA ome should simply contribute nothing).
# @noRd
.pelsa_dataset_accession_strings <- function(dataset) {
  rdesc <- NULL
  if (methods::is(dataset, "GCT")) {
    rdesc <- methods::slot(dataset, "rdesc")
  } else if (is.data.frame(dataset)) {
    rdesc <- dataset
  } else {
    return(character(0))
  }
  if (is.null(rdesc) || !is.data.frame(rdesc) ||
      !("PG.ProteinAccessions" %in% colnames(rdesc))) {
    return(character(0))
  }
  as.character(rdesc[["PG.ProteinAccessions"]])
}

# Explode + clean a vector of (possibly ;-delimited) accession strings into the
# unique non-empty token set. Mirrors the explode rule used everywhere else in
# PELSA: split on ";", trim, drop NA/empty.
# @noRd
.pelsa_explode_accession_tokens <- function(raw) {
  if (length(raw) == 0L) return(character(0))
  tokens <- unlist(strsplit(as.character(raw), ";", fixed = TRUE),
                   use.names = FALSE)
  if (is.null(tokens)) return(character(0))
  tokens <- trimws(tokens)
  tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
  unique(tokens)
}

# ---- Helper 1: accession universe --------------------------------------------

# Determine the accession universe to (re)fetch for a species refresh.
#
# universe = unique exploded accessions across the uploaded datasets'
#            PG.ProteinAccessions  UNION  the existing cache's accessions;
#            FALLBACK to the FASTA accessions (fasta_map names) when no datasets
#            are uploaded (gcts NULL/empty).
#
# Pure + deterministic (no network, no reactivity). Returns a sorted character
# vector for stable downstream progress reporting.
#
# @param gcts          named list of uploaded datasets (cmapR GCTs or data.frames
#                      carrying PG.ProteinAccessions), or NULL/empty.
# @param existing_cache the species' current feature data.frame (e.g. from
#                      pelsa_read_feature_cache); needs an `accession` column.
#                      May be NULL or 0-row.
# @param fasta_map     named list accession -> sequence (from pelsa_read_fasta);
#                      ONLY used as the no-datasets fallback. May be NULL.
# @return character vector of unique accessions (sorted).
# @noRd
pelsa_refresh_accession_universe <- function(gcts, existing_cache,
                                             fasta_map = NULL) {
  # 1. Accessions from the uploaded datasets (exploded, unique).
  dataset_acc <- character(0)
  if (!is.null(gcts) && length(gcts) > 0L) {
    raw <- unlist(lapply(gcts, .pelsa_dataset_accession_strings),
                  use.names = FALSE)
    dataset_acc <- .pelsa_explode_accession_tokens(raw)
  }

  # 2. Accessions already in the existing cache.
  cache_acc <- character(0)
  if (!is.null(existing_cache) && is.data.frame(existing_cache) &&
      "accession" %in% colnames(existing_cache) &&
      nrow(existing_cache) > 0L) {
    cache_acc <- unique(as.character(existing_cache$accession))
    cache_acc <- cache_acc[!is.na(cache_acc) & nzchar(cache_acc)]
  }

  if (length(dataset_acc) > 0L) {
    # Needed set (datasets) UNION existing cache coverage.
    return(sort(unique(c(dataset_acc, cache_acc))))
  }

  # 3. No datasets uploaded -> FASTA fallback (union with cache so refresh never
  #    loses coverage even on the fallback path).
  fasta_acc <- character(0)
  if (!is.null(fasta_map) && length(fasta_map) > 0L) {
    fasta_acc <- names(fasta_map)
    if (is.null(fasta_acc)) fasta_acc <- character(0)
    fasta_acc <- fasta_acc[!is.na(fasta_acc) & nzchar(fasta_acc)]
  }
  sort(unique(c(fasta_acc, cache_acc)))
}

# ---- Helper 2: write the feature cache ---------------------------------------

# The canonical 8-column schema order (parity-locked to schema.json::columns and
# pelsa_empty_feature_frame()). Single source of truth for the write order.
# @noRd
.PELSA_FEATURE_SCHEMA_COLS <- c(
  "accession", "feature_type", "start", "end",
  "description", "feature_class", "class_score", "coord_quality"
)

# Build the schema.json list payload for the feature cache (mirrors the
# committed schema.json: columns + dtypes + classifier_version +
# feature_class_levels + feature_class_scores + generated_at).
# @noRd
.pelsa_feature_schema_payload <- function() {
  scores <- pelsa_feature_class_scores()
  list(
    columns = as.list(.PELSA_FEATURE_SCHEMA_COLS),
    dtypes = list(
      accession     = "object",
      feature_type  = "object",
      start         = "int32",
      end           = "int32",
      description   = "object",
      feature_class = "object",
      class_score   = "int8",
      coord_quality = "object"
    ),
    classifier_version  = "fixed_v1",
    feature_class_levels = as.list(names(scores)),
    feature_class_scores = as.list(scores),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  )
}

# Merge a freshly-fetched feature table OVER an existing cache (data-loss guard).
#
# A refresh must only ADD/UPDATE coverage, NEVER silently lose it: the fetcher's
# circuit breaker trips only on 5 CONSECUTIVE failures, so scattered transient
# timeouts/5xx merely demote accessions to `unresolved` and the fetch returns
# "normally" with FEWER features. Writing ONLY the fresh frame would then
# PERMANENTLY DROP the previously-cached annotations for those accessions.
#
# RULE (driven solely by `fresh` + `unresolved`; there is no `universe` arg):
#   - For every accession NOT in `unresolved`: take the FRESH rows (which may be
#     none). This is correct even when UniProt genuinely REMOVED a feature for a
#     resolved accession (the fresh rows replace the old ones; a resolved-but-
#     now-empty accession has no fresh rows and is not retained, so it correctly
#     drops to zero rows).
#   - For accessions in `unresolved` that EXIST in the old cache: RETAIN the old
#     rows (coverage preserved across a flaky fetch).
#   - Accessions in neither contribute nothing.
#
# "Resolved" here means the fetch RETURNED A UNIPROT ENTRY for the accession
# (pelsa_fetch_uniprot derives `unresolved` from ENTRY presence -- an entry that
# returned with zero usable features is RESOLVED, so it is NOT in `unresolved`).
# Therefore a resolved-but-now-feature-less accession has no fresh rows AND is
# not in `unresolved`, so it is NOT retained -- its stale rows correctly drop to
# zero. We retain old rows for an accession IFF it is in `unresolved` (the fetch
# did not return its entry: 404-equivalent, failed batch, or not-yet-fetched)
# AND it exists in the old cache.
#
# Pure + deterministic. Returns rows ordered: fresh rows first (in their order),
# then retained old rows.
#
# @param existing   the prior cache feature data.frame (or NULL / 0-row).
# @param fresh       the freshly-fetched 8-col feature data.frame.
# @param unresolved character vector of accessions the fetch did NOT resolve.
# @return merged 8-col feature data.frame.
# @noRd
pelsa_merge_feature_cache <- function(existing, fresh, unresolved = character(0)) {
  if (!is.data.frame(fresh)) {
    stop("pelsa_merge_feature_cache: `fresh` must be a data.frame", call. = FALSE)
  }
  unresolved <- unique(as.character(unresolved))
  unresolved <- unresolved[!is.na(unresolved) & nzchar(unresolved)]

  # No prior cache -> nothing to retain; the fresh frame is the whole cache.
  if (is.null(existing) || !is.data.frame(existing) || nrow(existing) == 0L) {
    return(fresh)
  }
  # Nothing unresolved -> the fresh frame fully supersedes the old cache.
  if (length(unresolved) == 0L) {
    return(fresh)
  }
  if (!"accession" %in% colnames(existing)) {
    return(fresh)
  }

  # Retain old rows ONLY for unresolved accessions that the fresh frame does NOT
  # already cover (so a fresh row always wins; a retained row fills a gap).
  fresh_acc <- unique(as.character(fresh$accession))
  retain_acc <- setdiff(unresolved, fresh_acc)
  if (length(retain_acc) == 0L) {
    return(fresh)
  }

  retained <- existing[as.character(existing$accession) %in% retain_acc, ,
                       drop = FALSE]
  if (nrow(retained) == 0L) {
    return(fresh)
  }

  # Align retained columns to the fresh frame's schema columns (existing cache
  # carries the same 8 columns; intersect defensively, fill any missing as NA).
  keep_cols <- intersect(colnames(fresh), colnames(retained))
  retained <- retained[, keep_cols, drop = FALSE]
  for (col in setdiff(colnames(fresh), keep_cols)) retained[[col]] <- NA
  retained <- retained[, colnames(fresh), drop = FALSE]

  merged <- rbind(fresh, retained)
  rownames(merged) <- NULL
  merged
}

# Write a freshly-built feature table to a species' uniprot_features cache.
#
# Writes uniprot_features.tsv (readr::write_tsv) in the canonical schema column
# order, plus a regenerated schema.json. Creates the
# <species_dir>/uniprot_features/ directory if needed. Round-trips with
# pelsa_read_feature_cache. Returns the .tsv path.
#
# ATOMICITY: both files are written to TEMPFILES in the SAME directory, then
# file.rename()'d into place (atomic on a same-filesystem POSIX rename). So an
# interrupt mid-write - or between the .tsv and schema.json - leaves the PRIOR
# good cache fully intact rather than a truncated/mismatched file. A failed
# validation or a non-writable target therefore never destroys an existing
# cache. The temp files are cleaned up on any failure.
#
# FAIL FAST (not crash) when the target location is not writable (installed
# read-only package case): a clear stop() the observer can surface.
#
# @param feature_df  8-column feature data.frame (schema cols; extra cols
#                    dropped, missing schema cols -> error).
# @param species_dir directory holding (or to hold) "uniprot_features/".
# @return the written .tsv path (invisibly via return value).
# @noRd
pelsa_write_feature_cache <- function(feature_df, species_dir) {
  if (!is.data.frame(feature_df)) {
    stop("pelsa_write_feature_cache: `feature_df` must be a data.frame",
         call. = FALSE)
  }
  if (!is.character(species_dir) || length(species_dir) != 1L ||
      is.na(species_dir) || !nzchar(species_dir)) {
    stop("pelsa_write_feature_cache: `species_dir` must be a single non-empty ",
         "path", call. = FALSE)
  }
  missing_cols <- setdiff(.PELSA_FEATURE_SCHEMA_COLS, colnames(feature_df))
  if (length(missing_cols) > 0L) {
    stop("pelsa_write_feature_cache: feature_df missing schema column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Reorder to the canonical schema order + coerce the integer columns so the
  # round-trip type matches pelsa_read_feature_cache's col_integer().
  out <- feature_df[, .PELSA_FEATURE_SCHEMA_COLS, drop = FALSE]
  out$start       <- as.integer(out$start)
  out$end         <- as.integer(out$end)
  out$class_score <- as.integer(out$class_score)

  feat_dir <- file.path(species_dir, "uniprot_features")
  # Create the directory tree; a failure here (e.g. read-only installed lib) is
  # surfaced as a clear, actionable error rather than a downstream write crash.
  if (!dir.exists(feat_dir)) {
    created <- suppressWarnings(
      dir.create(feat_dir, recursive = TRUE, showWarnings = FALSE)
    )
    if (!created && !dir.exists(feat_dir)) {
      stop("pelsa_write_feature_cache: cannot create cache directory '",
           feat_dir, "' (the package database location may be read-only; ",
           "install Protigy to a writable library or set a writable database ",
           "dir).", call. = FALSE)
    }
  }
  # Guard the writable check explicitly (so we fail fast with a clear message
  # instead of a cryptic write error from readr).
  if (file.access(feat_dir, mode = 2L) != 0L) {
    stop("pelsa_write_feature_cache: cache directory '", feat_dir,
         "' is not writable (the package database location may be read-only).",
         call. = FALSE)
  }

  tsv_path    <- file.path(feat_dir, "uniprot_features.tsv")
  schema_path <- file.path(feat_dir, "schema.json")

  # ATOMIC write: stage BOTH files in the same dir, then rename into place. Any
  # failure before the renames leaves the prior cache untouched; the temp files
  # are removed. Renaming is last so a crash between the two renames is the only
  # tiny window (and even then the .tsv - the file the app reads - is good).
  tmp_tsv    <- tempfile("uniprot_features_", tmpdir = feat_dir, fileext = ".tsv")
  tmp_schema <- tempfile("schema_", tmpdir = feat_dir, fileext = ".json")
  ok <- FALSE
  on.exit({
    if (!ok) {
      suppressWarnings(file.remove(tmp_tsv[file.exists(tmp_tsv)]))
      suppressWarnings(file.remove(tmp_schema[file.exists(tmp_schema)]))
    }
  }, add = TRUE)

  readr::write_tsv(out, tmp_tsv, na = "")
  jsonlite::write_json(
    .pelsa_feature_schema_payload(), tmp_schema,
    auto_unbox = TRUE, pretty = TRUE
  )

  if (!file.rename(tmp_tsv, tsv_path)) {
    stop("pelsa_write_feature_cache: failed to move cache into place at '",
         tsv_path, "'.", call. = FALSE)
  }
  if (!file.rename(tmp_schema, schema_path)) {
    stop("pelsa_write_feature_cache: failed to move schema.json into place at '",
         schema_path, "'.", call. = FALSE)
  }
  ok <- TRUE

  tsv_path
}

# ---- Helper 3: orchestration (fetch -> write), fetcher INJECTED --------------

# Call an injected `fetch_fn`, forwarding the optional on_batch/should_cancel
# callbacks ONLY when fetch_fn declares them (or `...`). The real
# pelsa_fetch_uniprot declares both; a minimal test stub `function(accessions)`
# does not -- this keeps both working without forcing every stub to add the args.
# @noRd
.pelsa_call_fetch_fn <- function(fetch_fn, accessions, on_batch, should_cancel) {
  fmls <- names(formals(fetch_fn))
  extra <- list()
  if ("..." %in% fmls || "on_batch" %in% fmls) extra$on_batch <- on_batch
  if ("..." %in% fmls || "should_cancel" %in% fmls) {
    extra$should_cancel <- should_cancel
  }
  do.call(fetch_fn, c(list(accessions), extra))
}

# Orchestrate one species' cache refresh: fetch the universe's UniProt features
# (via the INJECTED `fetch_fn`), then write the resulting 8-col table to the
# species cache. The injectable `fetch_fn` is the TESTABILITY SEAM - tests pass
# a stub returning a canned list(features=, unresolved=) so NO live network is
# touched; the app passes the real pelsa_fetch_uniprot.
#
# Off the reactive path (called once per button click). The optional `progress`
# is a shiny::Progress-like object with a $set(value, message, detail) method;
# when NULL no progress is reported (the test path).
#
# DATA-LOSS GUARD: the freshly-fetched features are MERGED OVER `existing` (via
# pelsa_merge_feature_cache) before writing, so a flaky fetch that demotes some
# accessions to `unresolved` RETAINS their previously-cached rows instead of
# silently dropping them. Combined with the atomic write in
# pelsa_write_feature_cache, a partial/interrupted refresh never destroys
# coverage.
#
# @param species     species name (for progress/messages).
# @param universe    character vector of accessions to fetch (from
#                    pelsa_refresh_accession_universe).
# @param species_dir directory holding (or to hold) "uniprot_features/".
# @param fetch_fn    function(accessions) -> list(features=<8-col df>,
#                    unresolved=<chr>). Defaults to the real pelsa_fetch_uniprot.
# @param existing    the prior cache feature data.frame (or NULL) to merge over.
# @param progress    optional shiny::Progress (or NULL).
# @return list(features=, unresolved=, path=, n_features=, n_unresolved=,
#              n_accessions=, n_retained_from_cache=).
# @noRd
pelsa_refresh_species_cache <- function(species, universe, species_dir,
                                        fetch_fn = pelsa_fetch_uniprot,
                                        existing = NULL,
                                        progress = NULL,
                                        should_cancel = NULL) {
  if (!is.character(species) || length(species) != 1L || !nzchar(species)) {
    stop("pelsa_refresh_species_cache: `species` must be a single non-empty ",
         "string", call. = FALSE)
  }
  if (!is.character(universe)) {
    stop("pelsa_refresh_species_cache: `universe` must be a character vector",
         call. = FALSE)
  }
  if (!is.function(fetch_fn)) {
    stop("pelsa_refresh_species_cache: `fetch_fn` must be a function",
         call. = FALSE)
  }
  universe <- unique(universe[!is.na(universe) & nzchar(universe)])

  .progress <- function(value, message, detail = NULL) {
    if (!is.null(progress) && is.function(progress$set)) {
      progress$set(value = value, message = message, detail = detail)
    }
  }
  .canceled_result <- function(reason) {
    # No write on cancel: the prior cache is left fully intact.
    list(features = existing, unresolved = universe, path = NA_character_,
         n_features = if (is.data.frame(existing)) nrow(existing) else 0L,
         n_unresolved = length(universe), n_accessions = length(universe),
         n_retained_from_cache = 0L, canceled = TRUE)
  }

  if (length(universe) == 0L) {
    stop("pelsa_refresh_species_cache: empty accession universe for species '",
         species, "' (no uploaded-dataset accessions, no existing cache, no ",
         "FASTA).", call. = FALSE)
  }

  # Bail before any network if already canceled.
  if (is.function(should_cancel) && isTRUE(should_cancel())) {
    return(.canceled_result("pre-fetch"))
  }

  .progress(0.05, sprintf("Fetching UniProt for %s", species),
            sprintf("%d accessions", length(universe)))

  # Page-level progress: map the fetcher's batch 0..1 onto the 0.05..0.80 band
  # so the bar moves smoothly across the (slow) network, not in two big jumps.
  on_batch <- function(done, total) {
    if (!is.numeric(total) || total <= 0L) return(invisible())
    frac <- 0.05 + 0.75 * (done / total)
    .progress(frac, sprintf("Fetching UniProt for %s", species),
              sprintf("page %d/%d", done, total))
  }
  # Call fetch_fn forwarding only the optional callbacks it actually declares,
  # so a simple test stub `function(accessions)` still works (the real
  # pelsa_fetch_uniprot declares on_batch + should_cancel).
  fetched <- .pelsa_call_fetch_fn(fetch_fn, universe, on_batch, should_cancel)
  if (!is.list(fetched) || is.null(fetched$features)) {
    stop("pelsa_refresh_species_cache: `fetch_fn` must return a list with a ",
         "`features` data.frame", call. = FALSE)
  }
  # Honor a mid-fetch cancel: do NOT write a partial cache.
  if (isTRUE(fetched$canceled)) {
    return(.canceled_result("mid-fetch"))
  }
  fresh      <- fetched$features
  unresolved <- fetched$unresolved %||% character(0)

  # MERGE over the prior cache so unresolved accessions keep their old rows.
  merged <- pelsa_merge_feature_cache(existing, fresh, unresolved)
  n_retained <- nrow(merged) - nrow(fresh)

  .progress(0.85, sprintf("Writing %s cache", species),
            sprintf("%d feature rows", nrow(merged)))
  path <- pelsa_write_feature_cache(merged, species_dir)
  .progress(1.0, sprintf("Done: %s", species), NULL)

  list(
    features              = merged,
    unresolved            = unresolved,
    path                  = path,
    n_features            = nrow(merged),
    n_unresolved          = length(unresolved),
    n_accessions          = length(universe),
    n_retained_from_cache = n_retained,
    canceled              = FALSE
  )
}

# ---- Helper 4: multi-species orchestration (observer-facing) -----------------

# Resolve ONE species' refresh inputs from disk: its existing cache (best-effort)
# and, when no datasets are uploaded, the species' FASTA accession map (the
# fallback universe). Pure-ish: reads files only; never fetches/writes.
#
# @param species_dir   the species directory (file.path(database_dir, species)).
# @param uploaded_gcts named list of uploaded datasets, or NULL/empty.
# @return list(existing = <feature df or NULL>, fasta_map = <list or NULL>).
# @noRd
pelsa_species_refresh_inputs <- function(species_dir, uploaded_gcts) {
  existing <- tryCatch(pelsa_read_feature_cache(species_dir),
                       error = function(e) NULL)

  fasta_map <- NULL
  if (is.null(uploaded_gcts) || length(uploaded_gcts) == 0L) {
    fdir <- file.path(species_dir, "fasta")
    fastas <- if (dir.exists(fdir)) {
      list.files(fdir, pattern = "\\.(fasta|fa)$", full.names = TRUE,
                 ignore.case = TRUE)
    } else {
      character(0)
    }
    if (length(fastas) > 0L) {
      # Refresh only ever runs for UniProt (taxon-code) species -- the refresh
      # checklist filters out self-curated species -- so the FASTA is parsed in
      # UniProt mode (pipe-aware). Explicit for intent.
      fasta_map <- tryCatch(pelsa_read_fasta(fastas[[1]], mode = "uniprot"),
                            error = function(e) NULL)
    }
  }
  list(existing = existing, fasta_map = fasta_map)
}

# Run the refresh for a SET of checked species, capturing per-species results +
# errors (so one species failing does not abort the rest, and the observer never
# crashes the app). Each species: resolve inputs -> universe -> fetch+write via
# pelsa_refresh_species_cache. fetch_fn is the INJECTED fetcher (the real
# pelsa_fetch_uniprot in-app; a stub in tests). `set_progress(value, detail)`
# (or NULL) advances an overall 0..1 progress bar; each species occupies an
# equal slice.
#
# @param species       character vector of species names to refresh.
# @param database_dir  the PELSA database dir (file.path()-joined per species).
# @param uploaded_gcts named list of uploaded datasets, or NULL.
# @param fetch_fn      function(accessions) -> list(features=, unresolved=).
# @param set_progress  function(value, detail) or NULL.
# @param should_cancel optional function() -> logical; checked before each
#                      species and forwarded into the fetcher (page-boundary
#                      cancel). A canceled species writes NO cache and is marked
#                      canceled = TRUE; remaining species are skipped.
# @return list (one element per species) of either
#         list(species=, n_features=, n_unresolved=, n_accessions=,
#              n_retained_from_cache=, had_existing=, path=, canceled=, error=NULL)
#         or list(species=, error=<message>).
# @noRd
pelsa_run_species_refresh <- function(species, database_dir, uploaded_gcts,
                                      fetch_fn = pelsa_fetch_uniprot,
                                      set_progress = NULL,
                                      should_cancel = NULL) {
  if (!is.character(species) || length(species) == 0L) {
    stop("pelsa_run_species_refresh: `species` must be a non-empty character ",
         "vector", call. = FALSE)
  }
  n <- length(species)
  results <- vector("list", n)

  for (k in seq_along(species)) {
    sp <- species[[k]]
    species_dir <- file.path(database_dir, sp)
    base_frac <- (k - 1L) / n

    # Stop launching new species once canceled (the in-flight species already
    # honored cancel at its own page boundary). Remaining species are recorded
    # as canceled-not-run so the summary is honest.
    if (is.function(should_cancel) && isTRUE(should_cancel())) {
      results[[k]] <- list(species = sp, canceled = TRUE, not_run = TRUE,
                           error = NULL)
      next
    }

    results[[k]] <- tryCatch({
      io <- pelsa_species_refresh_inputs(species_dir, uploaded_gcts)
      had_existing <- is.data.frame(io$existing) && nrow(io$existing) > 0L
      universe <- pelsa_refresh_accession_universe(
        uploaded_gcts, io$existing, fasta_map = io$fasta_map
      )

      sub_progress <- if (is.null(set_progress)) NULL else list(
        set = function(value, message, detail = NULL) {
          set_progress(
            base_frac + value / n,
            sprintf("(%d/%d) %s", k, n,
                    if (is.null(detail)) message else detail)
          )
        }
      )

      res <- pelsa_refresh_species_cache(
        species = sp, universe = universe, species_dir = species_dir,
        fetch_fn = fetch_fn, existing = io$existing, progress = sub_progress,
        should_cancel = should_cancel
      )
      list(species = sp, n_features = res$n_features,
           n_unresolved = res$n_unresolved, n_accessions = res$n_accessions,
           n_retained_from_cache = res$n_retained_from_cache,
           had_existing = had_existing, path = res$path,
           canceled = isTRUE(res$canceled), error = NULL)
    }, error = function(e) {
      list(species = sp, error = conditionMessage(e))
    })

    if (!is.null(set_progress)) set_progress(k / n, NULL)
  }
  results
}

# Format the per-species refresh results into showNotification() payloads.
#
# Pure (no Shiny) so it unit-tests without a session: returns a list of
# list(message=, type=, duration=). The observer just emits each.
#   - any error -> an "error" notification (sticky).
#   - a species that HAD a cache and finished with unresolved accessions -> a
#     "warning" (a partial/lossy-adjacent refresh must not be accepted
#     silently), reporting how many prior rows were retained.
#   - a single rolled-up "message" summary of all successes (features /
#     unresolved / retained counts).
#
# @param results the pelsa_run_species_refresh() return value.
# @return list of list(message=, type=, duration=).
# @noRd
pelsa_refresh_notifications <- function(results) {
  notes <- list()
  add <- function(message, type, duration) {
    notes[[length(notes) + 1L]] <<- list(message = message, type = type,
                                          duration = duration)
  }

  for (r in results) {
    if (!is.null(r$error)) {
      add(sprintf("Refresh failed for %s: %s", r$species, r$error),
          "error", NULL)
    }
  }
  ok <- Filter(function(r) is.null(r$error), results)
  # Canceled species (mid-fetch or not-run): no cache written, prior cache intact.
  canceled <- Filter(function(r) isTRUE(r$canceled), ok)
  if (length(canceled) > 0L) {
    add(sprintf("Refresh canceled (%s); existing cache(s) left unchanged.",
                paste(vapply(canceled, function(r) r$species, character(1)),
                      collapse = ", ")),
        "warning", 8)
  }
  done <- Filter(function(r) !isTRUE(r$canceled), ok)
  for (r in done) {
    if (isTRUE(r$had_existing) && isTRUE(r$n_unresolved > 0L)) {
      add(sprintf(paste0("%s refreshed with %d unresolved accession(s); %d ",
                         "previously-cached row(s) retained. Re-run when ",
                         "UniProt is reachable for full coverage."),
                  r$species, r$n_unresolved, r$n_retained_from_cache),
          "warning", NULL)
    }
  }
  if (length(done) > 0L) {
    summaries <- vapply(done, function(r) sprintf(
      "%s: %d features, %d unresolved, %d retained", r$species,
      r$n_features, r$n_unresolved, r$n_retained_from_cache), character(1))
    add(paste0("UniProt annotation refresh complete. ",
               paste(summaries, collapse = "; ")), "message", 10)
  }
  notes
}

# ---- Helper 5: confirm-gate universe size (pure, observer-facing) -------------

# Estimate the TOTAL accession universe a refresh of `species` would fetch, so
# the observer can WARN + confirm before a large (whole-proteome) fetch. Reads
# each species' existing cache + (no-datasets) FASTA exactly as the real run
# does, then sums the per-species universe sizes. Pure-ish (reads files only;
# never fetches/writes), so the observer can call it synchronously pre-fetch.
#
# @param species       character vector of species names.
# @param database_dir  the PELSA database dir.
# @param uploaded_gcts named list of uploaded datasets, or NULL.
# @return list(total = <int>, per_species = <named int vector>).
# @noRd
pelsa_refresh_universe_size <- function(species, database_dir, uploaded_gcts) {
  per <- vapply(species, function(sp) {
    species_dir <- file.path(database_dir, sp)
    io <- pelsa_species_refresh_inputs(species_dir, uploaded_gcts)
    universe <- pelsa_refresh_accession_universe(
      uploaded_gcts, io$existing, fasta_map = io$fasta_map
    )
    length(universe)
  }, integer(1))
  names(per) <- species
  list(total = sum(per), per_species = per)
}

# Format a one-line human estimate for the confirm dialog: count + a rough ETA
# under the batched fetcher (~batch_size accessions per ~RTT-bound page request).
#
# @param total       total accessions across the selected species.
# @param batch_size  accessions per /search page (default matches the fetcher).
# @param page_secs   modelled seconds per page request (RTT-bound).
# @return a single character string, e.g. "69,845 accessions (~2 min)".
# @noRd
pelsa_refresh_eta_text <- function(total, batch_size = 200L, page_secs = 0.9) {
  total <- as.integer(total)
  n_pages <- ceiling(max(total, 0L) / batch_size)
  secs <- n_pages * page_secs
  eta <- if (secs < 90) {
    sprintf("~%d sec", max(1L, as.integer(round(secs))))
  } else {
    sprintf("~%d min", as.integer(ceiling(secs / 60)))
  }
  sprintf("%s accession%s (%s)",
          formatC(total, big.mark = ",", format = "d"),
          if (total == 1L) "" else "s", eta)
}

# ---- Helper 6: inline progress + result UI (pure tag constructors) ------------

# Build the INLINE progress block shown under the Refresh button while a fetch is
# in flight (replaces the dismissible withProgress modal / toast). A labelled
# determinate bar + a status line (the live "(k/n) species . page X/Y" detail).
# Pure (a function of its args) so it tests without a session.
#
# @param fraction numeric 0..1 overall progress (clamped).
# @param detail   status line text (e.g. "(1/2) human . page 88/140"), or NULL.
# @return a shiny tag (the progress block).
# @noRd
pelsa_refresh_progress_ui <- function(fraction, detail = NULL) {
  pct <- max(0, min(100, round(100 * (fraction %||% 0))))
  shiny::tags$div(
    class = "pelsa-refresh-progress",
    style = paste0("margin-top:10px; padding:10px; border:1px solid #5bc0de; ",
                   "border-radius:6px; background:#eef7fb;"),
    shiny::tags$div(
      style = "font-weight:600; color:#31708f; margin-bottom:6px;",
      shiny::icon("sync"), " Refreshing UniProt annotation library..."
    ),
    # Determinate bar (styled div, no extra dependency).
    shiny::tags$div(
      style = paste0("height:14px; background:#d6eaf3; border-radius:7px; ",
                     "overflow:hidden;"),
      shiny::tags$div(
        style = sprintf(paste0("height:100%%; width:%d%%; background:#31708f; ",
                               "transition:width .2s ease;"), pct)
      )
    ),
    shiny::tags$div(
      style = "margin-top:6px; font-size:12px; color:#31708f;",
      sprintf("%d%%%s", pct,
              if (!is.null(detail) && nzchar(detail)) paste0(" . ", detail) else "")
    )
  )
}

# Build the INLINE result block shown under the Refresh button after a fetch.
# Pure: given pelsa_run_species_refresh() results, returns a colored summary
# (green success / amber partial-or-canceled / red error) that PERSISTS in place
# (it is not a toast, so it cannot be cleared off-screen). Mirrors the content of
# pelsa_refresh_notifications but as a stable inline panel.
#
# @param results the pelsa_run_species_refresh() return value (or NULL -> NULL).
# @return NULL or a shiny tag (the result block).
# @noRd
pelsa_refresh_result_ui <- function(results) {
  if (is.null(results) || length(results) == 0L) return(NULL)

  errs     <- Filter(function(r) !is.null(r$error), results)
  ok       <- Filter(function(r) is.null(r$error), results)
  canceled <- Filter(function(r) isTRUE(r$canceled), ok)
  done     <- Filter(function(r) !isTRUE(r$canceled), ok)

  # Worst status drives the panel color.
  status <- if (length(errs) > 0L) "error"
            else if (length(canceled) > 0L ||
                     any(vapply(done, function(r) isTRUE(r$n_unresolved > 0L),
                                logical(1)))) "warn"
            else "ok"
  pal <- switch(status,
    error = list(border = "#d9534f", bg = "#fdf3f2", fg = "#a94442",
                 icon = "circle-exclamation", head = "Refresh finished with errors"),
    warn  = list(border = "#f0ad4e", bg = "#fcf8e3", fg = "#8a6d3b",
                 icon = "triangle-exclamation", head = "Refresh finished"),
    list(border = "#5cb85c", bg = "#f0f9f0", fg = "#3c763d",
         icon = "circle-check", head = "Refresh complete"))

  items <- list()
  for (r in errs) {
    items <- c(items, list(shiny::tags$li(sprintf("%s: %s", r$species, r$error))))
  }
  for (r in canceled) {
    items <- c(items, list(shiny::tags$li(
      sprintf("%s: canceled - existing cache left unchanged", r$species))))
  }
  for (r in done) {
    items <- c(items, list(shiny::tags$li(sprintf(
      "%s: %d features, %d unresolved, %d retained from cache",
      r$species, r$n_features %||% 0L, r$n_unresolved %||% 0L,
      r$n_retained_from_cache %||% 0L))))
  }

  shiny::tags$div(
    class = "pelsa-refresh-result",
    style = sprintf(paste0("margin-top:10px; padding:10px; border:1px solid %s; ",
                           "border-radius:6px; background:%s; color:%s;"),
                    pal$border, pal$bg, pal$fg),
    shiny::tags$strong(shiny::icon(pal$icon), " ", pal$head),
    shiny::tags$ul(style = "margin:6px 0 0 0;", items)
  )
}
