################################################################################
# Module: PELSA species classification + resolution (taxonomy-code convention).
#
# A folder under inst/database/ is identified by its NAME, which is the sole
# signal for how the species is treated:
#   - all digits ("9606")  -> candidate UniProt species (UniProt FASTA parse +
#     annotation fetch). The digits are a UniProt/NCBI taxonomy ID, validated +
#     named via the UniProt taxonomy REST endpoint.
#   - anything else        -> self-curated species (first-token FASTA parse, no
#     annotation fetch, annotation-dependent UI disabled).
#
# The classification verdict + display name are cached in a single runtime
# registry, inst/database/species_meta.json (gitignored). Network is touched only
# on a cache miss (a new numeric folder) or when re-validating an unvalidated
# entry at app start -- never on the reactive setup-box render path.
#
# Public helpers (all @noRd):
#   pelsa_classify_folder(folder)                  "numeric" | "named"
#   pelsa_fetch_taxon(taxon_id, ...)               taxonomy name/validation fetch
#   pelsa_species_meta_path(database_dir)          registry json path
#   pelsa_read_species_meta(database_dir)          registry -> named list
#   pelsa_write_species_meta(database_dir, meta)   atomic write of the registry
#   pelsa_species_has_feature_cache(db, folder)    file.exists probe (no read)
#   pelsa_resolve_species(db, folder, validate_fn, meta) -> species struct
#   pelsa_species_display_label(struct)            the picker / export label
#   pelsa_refresh_species_meta_on_start(db, validate_fn)  promote unvalidated
#
# The taxonomy fetch (pelsa_fetch_taxon) is the injectable seam: tests pass a
# stub; production passes the real httr2 fetch. It is NEVER called against the
# live network in tests.
################################################################################

# ---- classification ----------------------------------------------------------

# Classify a database subfolder NAME by structure alone (offline, deterministic).
# @param folder character scalar folder name.
# @return "numeric" (all digits) or "named".
# @noRd
pelsa_classify_folder <- function(folder) {
  if (!is.character(folder) || length(folder) != 1L || is.na(folder)) {
    stop("pelsa_classify_folder(): `folder` must be a single string.",
         call. = FALSE)
  }
  if (grepl("^[0-9]+$", folder)) "numeric" else "named"
}

# ---- taxonomy fetch (injectable network seam) --------------------------------

.PELSA_TAXONOMY_BASE <- "https://rest.uniprot.org"
.PELSA_TAXONOMY_UA   <- "pelsa_qc/0.1 (PELSA data pipeline)"

# Fetch the scientific name for a taxonomy id from the UniProt taxonomy REST
# endpoint (GET /taxonomy/{id}). Mirrors the httr2 stack of pelsa_fetch_uniprot
# (same host): user-agent, throttle, retry on transient 429/5xx, and req_error
# raising ONLY on >= 500 so a clean 404 (fake taxon) is distinguishable from a
# server/network failure.
#
# @param taxon_id  character/integer scalar taxonomy id (digits).
# @param base      taxonomy REST base (override for testing).
# @param max_tries per-request retry attempts.
# @param rate      throttle capacity per second.
# @return list(status = "ok" | "not_found" | "network_error",
#              scientific_name = chr/NA, common_name = chr/NA, taxon_id = chr).
# @noRd
pelsa_fetch_taxon <- function(taxon_id,
                              base = .PELSA_TAXONOMY_BASE,
                              max_tries = 3L,
                              rate = 10L) {
  taxon_id <- as.character(taxon_id)[[1L]]
  fail <- function(status) {
    list(status = status, scientific_name = NA_character_,
         common_name = NA_character_, taxon_id = taxon_id)
  }

  req <- httr2::request(base)
  req <- httr2::req_url_path_append(req, "taxonomy", taxon_id)
  req <- httr2::req_user_agent(req, .PELSA_TAXONOMY_UA)
  req <- httr2::req_throttle(req, capacity = rate, fill_time_s = 1)
  req <- httr2::req_retry(
    req, max_tries = max_tries,
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
    }
  )
  # Raise only on >= 500 so a 404 returns a response we can classify as
  # "not_found" rather than throwing.
  req <- httr2::req_error(req, is_error = function(resp) {
    httr2::resp_status(resp) >= 500
  })

  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error") || inherits(resp, "condition")) {
    return(fail("network_error"))
  }

  status <- httr2::resp_status(resp)
  if (status == 404L) return(fail("not_found"))
  if (status >= 400L) return(fail("network_error"))

  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  sci <- parsed$scientificName
  if (is.null(sci) || !nzchar(as.character(sci)[[1L]])) {
    # 200 but no usable name -> treat as a (rare) network/parse failure so the
    # caller falls back rather than persisting an empty name as validated.
    return(fail("network_error"))
  }
  list(
    status          = "ok",
    scientific_name = as.character(sci)[[1L]],
    common_name     = if (!is.null(parsed$commonName))
      as.character(parsed$commonName)[[1L]] else NA_character_,
    taxon_id        = taxon_id
  )
}

# ---- species_meta registry ---------------------------------------------------

# Path to the single runtime registry (gitignored).
# @noRd
pelsa_species_meta_path <- function(database_dir) {
  file.path(database_dir, "species_meta.json")
}

# Read the registry into a named list keyed by folder name. A missing or
# unparseable file yields list() (the registry is a regenerable cache).
# @noRd
pelsa_read_species_meta <- function(database_dir) {
  if (!is.character(database_dir) || length(database_dir) != 1L ||
      is.na(database_dir) || !nzchar(database_dir)) {
    return(list())
  }
  path <- pelsa_species_meta_path(database_dir)
  if (!file.exists(path)) return(list())
  out <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(out) || !is.list(out)) list() else out
}

# Atomically write the registry (temp file + rename), mirroring the cache-write
# discipline elsewhere so two sessions cannot interleave a half-written file. A
# write failure (e.g. read-only library) is swallowed: the registry is a cache,
# and a failed persist must never break the species listing.
# @noRd
pelsa_write_species_meta <- function(database_dir, meta) {
  if (!is.character(database_dir) || length(database_dir) != 1L ||
      is.na(database_dir) || !nzchar(database_dir) || !dir.exists(database_dir)) {
    return(invisible(FALSE))
  }
  path <- pelsa_species_meta_path(database_dir)
  ok <- tryCatch({
    tmp <- tempfile(tmpdir = database_dir, fileext = ".json")
    jsonlite::write_json(meta, tmp, auto_unbox = TRUE, pretty = TRUE,
                         null = "null")
    file.rename(tmp, path)
  }, error = function(e) FALSE)
  invisible(isTRUE(ok))
}

# ---- feature-cache presence (probe, do NOT read) -----------------------------

# TRUE when a UniProt feature cache (tsv or parquet) exists for `folder`. This is
# a plain file.exists() probe -- pelsa_read_feature_cache() STOPS when the file
# is absent, so it must never be used to test presence.
# @noRd
pelsa_species_has_feature_cache <- function(database_dir, folder) {
  feat_dir <- file.path(database_dir, folder, "uniprot_features")
  file.exists(file.path(feat_dir, "uniprot_features.tsv")) ||
    file.exists(file.path(feat_dir, "uniprot_features.parquet"))
}

# ---- display label -----------------------------------------------------------

# The picker / export label for a resolved species struct. Three states:
#   uniprot + validated                  -> "<scientific name> (<folder>)"
#   uniprot + not validated (name pending, has cache) ->
#                                           "<folder> (annotations available, name pending)"
#   self_curated                          -> "<folder> (customized)"
# @noRd
pelsa_species_display_label <- function(struct) {
  folder <- struct$folder
  if (identical(struct$type, "uniprot")) {
    sci <- struct$scientific_name
    if (isTRUE(struct$validated) && !is.null(sci) && !is.na(sci) &&
        nzchar(as.character(sci))) {
      return(sprintf("%s (%s)", sci, folder))
    }
    return(sprintf("%s (annotations available, name pending)", folder))
  }
  sprintf("%s (customized)", folder)
}

# ---- resolver ----------------------------------------------------------------

# Resolve ONE folder to the typed species struct every consumer reads. Reads the
# registry first; only calls `validate_fn` on a numeric folder with no cached
# (validated) verdict. Persists the verdict on a definitive outcome.
#
# Verdict logic (see the design spec):
#   named                              -> self_curated (no network).
#   numeric + cached validated entry   -> reuse it (no network).
#   numeric + validate ok              -> uniprot/validated; persist.
#   numeric + validate not_found       -> self_curated; persist.
#   numeric + network_error + has cache-> uniprot/unvalidated (name pending); persist validated=FALSE.
#   numeric + network_error + no cache -> self_curated (transient); NOT persisted as final.
#
# @param database_dir database dir.
# @param folder       a subfolder name.
# @param validate_fn  function(taxon_id) -> pelsa_fetch_taxon-shaped list.
# @param meta         optional pre-read registry (avoids re-reading in a loop).
# @param allow_fetch  TRUE (default) may call validate_fn for an un-validated
#                     numeric folder. FALSE keeps the call CACHE-ONLY (no
#                     network) -- the resolver derives a best-effort verdict from
#                     the cached entry + the on-disk feature cache. The reactive
#                     render path passes FALSE so listing never touches the
#                     network; the cache miss / unvalidated retry is driven once
#                     by pelsa_refresh_species_meta_on_start() and by the
#                     Start-Analysis path (allow_fetch = TRUE).
# @return list(folder, type, display, taxon_id, scientific_name, validated,
#              has_feature_cache).
# @noRd
pelsa_resolve_species <- function(database_dir, folder,
                                  validate_fn = pelsa_fetch_taxon,
                                  meta = NULL,
                                  allow_fetch = TRUE) {
  if (is.null(meta)) meta <- pelsa_read_species_meta(database_dir)
  has_cache <- pelsa_species_has_feature_cache(database_dir, folder)

  .struct <- function(type, validated, scientific_name = NA_character_,
                      taxon_id = NA_integer_) {
    s <- list(
      folder            = folder,
      type              = type,
      taxon_id          = taxon_id,
      scientific_name   = scientific_name,
      validated         = validated,
      has_feature_cache = has_cache
    )
    s$display <- pelsa_species_display_label(s)
    s
  }

  # Named folder: self-curated, deterministic, never touches the network.
  if (identical(pelsa_classify_folder(folder), "named")) {
    s <- .struct("self_curated", validated = TRUE)
    meta[[folder]] <- list(type = "self_curated", display_name = folder,
                           validated = TRUE)
    pelsa_write_species_meta(database_dir, meta)
    return(s)
  }

  # Numeric folder with a cached, validated UniProt verdict: reuse it.
  cached <- meta[[folder]]
  if (!is.null(cached) && identical(cached$type, "uniprot") &&
      isTRUE(cached$validated)) {
    return(.struct("uniprot", validated = TRUE,
                   scientific_name = cached$scientific_name %||% NA_character_,
                   taxon_id = as.integer(cached$taxon_id %||% folder)))
  }

  # Numeric folder, no VALIDATED verdict. On the cache-only path (allow_fetch =
  # FALSE, the reactive render) we must NOT call the network: derive a
  # best-effort verdict from the cached entry + the on-disk feature cache.
  #   - a not_found verdict was persisted as self_curated -> honor it.
  #   - otherwise a feature cache is evidence of a real UniProt species ->
  #     uniprot/unvalidated (name pending); no cache -> self_curated (transient).
  if (!isTRUE(allow_fetch)) {
    if (!is.null(cached) && identical(cached$type, "self_curated")) {
      return(.struct("self_curated", validated = FALSE))
    }
    if (has_cache) {
      return(.struct("uniprot", validated = FALSE,
                     taxon_id = suppressWarnings(as.integer(folder))))
    }
    return(.struct("self_curated", validated = FALSE))
  }

  # Numeric folder, no validated verdict, fetch allowed: validate now.
  res <- validate_fn(folder)
  taxon_int <- suppressWarnings(as.integer(folder))

  if (identical(res$status, "ok")) {
    s <- .struct("uniprot", validated = TRUE,
                 scientific_name = res$scientific_name, taxon_id = taxon_int)
    meta[[folder]] <- list(type = "uniprot", taxon_id = taxon_int,
                           scientific_name = res$scientific_name,
                           validated = TRUE)
    pelsa_write_species_meta(database_dir, meta)
    return(s)
  }

  if (identical(res$status, "not_found")) {
    s <- .struct("self_curated", validated = FALSE)
    meta[[folder]] <- list(type = "self_curated", display_name = folder,
                           validated = FALSE)
    pelsa_write_species_meta(database_dir, meta)
    return(s)
  }

  # network_error: a feature cache is unambiguous evidence of a real UniProt
  # species -> keep UniProt parsing/coloring with a pending name; otherwise
  # degrade to self-curated for this run WITHOUT persisting a final verdict (so
  # the next app start retries).
  if (has_cache) {
    s <- .struct("uniprot", validated = FALSE, taxon_id = taxon_int)
    meta[[folder]] <- list(type = "uniprot", taxon_id = taxon_int,
                           scientific_name = NA, validated = FALSE)
    pelsa_write_species_meta(database_dir, meta)
    return(s)
  }
  # Transient self-curated: do not persist (leave the entry as-is for retry).
  .struct("self_curated", validated = FALSE)
}

# ---- refresh-on-start ---------------------------------------------------------

# Re-attempt validation for every structurally-numeric folder whose registry
# entry is missing or validated == FALSE. On a successful fetch, promote the
# entry to validated and rewrite the registry. Best-effort: a failure here must
# not break the species listing. Call ONCE per app start, OFF the reactive path.
#
# @param database_dir database dir.
# @param validate_fn  function(taxon_id) -> pelsa_fetch_taxon-shaped list.
# @return invisible(NULL).
# @noRd
pelsa_refresh_species_meta_on_start <- function(database_dir,
                                                validate_fn = pelsa_fetch_taxon) {
  if (!is.character(database_dir) || length(database_dir) != 1L ||
      is.na(database_dir) || !nzchar(database_dir) || !dir.exists(database_dir)) {
    return(invisible(NULL))
  }
  folders <- list.dirs(database_dir, full.names = FALSE, recursive = FALSE)
  folders <- folders[nzchar(folders)]
  numeric_folders <- folders[
    vapply(folders, function(f) identical(pelsa_classify_folder(f), "numeric"),
           logical(1))
  ]
  if (length(numeric_folders) == 0L) return(invisible(NULL))

  meta <- pelsa_read_species_meta(database_dir)
  changed <- FALSE
  for (f in numeric_folders) {
    entry <- meta[[f]]
    already_ok <- !is.null(entry) && identical(entry$type, "uniprot") &&
      isTRUE(entry$validated)
    if (already_ok) next

    res <- tryCatch(validate_fn(f), error = function(e) NULL)
    if (!is.null(res) && identical(res$status, "ok")) {
      meta[[f]] <- list(type = "uniprot",
                        taxon_id = suppressWarnings(as.integer(f)),
                        scientific_name = res$scientific_name, validated = TRUE)
      changed <- TRUE
    }
  }
  if (changed) pelsa_write_species_meta(database_dir, meta)
  invisible(NULL)
}
