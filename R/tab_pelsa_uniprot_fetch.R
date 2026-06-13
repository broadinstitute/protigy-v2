################################################################################
# Module: PELSA UniProt feature fetch + JSON parser/classifier (Task 2H).
#
# Builds the per-feature `uniprot_features` table used by annotation overlap
# (Task 2I). Two layers:
#
#   (A) PURE parser/classifier -- the parity-critical, fully unit-tested core:
#       pelsa_feature_to_class(ftype, desc)     vectorized class assignment
#       pelsa_feature_class_scores()            SCORES (== schema.json scores)
#       pelsa_parse_uniprot_json(entry)         one entry -> 8-col data.frame
#       pelsa_parse_uniprot_json_batch(entries) rbind of per-entry frames
#
#   (B) httr2 FETCH -- thin network wrapper, off the reactive path (runs once
#       at Start-Analysis / species refresh), network-guarded in tests:
#       pelsa_fetch_uniprot(accessions, ...)    -> list(features=, unresolved=)
#
# PARITY (classifier_version "fixed_v1"): a fresh fetch (new species or cache
# miss) MUST classify features IDENTICALLY to the notebook's
# uniprot_features.py::feature_to_class + _parse_json_features so the freshly
# built cache matches the committed one. The keyword sets, the CHECK ORDER, and
# SCORES are all parity-locked. SCORES equals
# inst/database/human/uniprot_features/schema.json::feature_class_scores.
# Annotation overlap + priority resolution is the NEXT task (2I), not here.
################################################################################

# ---- (A) Classifier ----------------------------------------------------------

# class_score lookup, parity-locked to schema.json::feature_class_scores.
# Returned as a named integer vector (class_score column is int8 in the cache).
#
# @return named integer vector feature_class -> score
# @noRd
pelsa_feature_class_scores <- function() {
  c(
    active_or_binding_site     = 5L,
    catalytic_domain           = 3L,
    folded_domain              = 2L,
    region_or_motif            = 1L,
    repeat_or_coiled_coil      = -1L,
    transmembrane_or_signal    = 0L,
    low_complexity_or_disorder = -3L,
    other                      = 0L
  )
}

# Classify one UniProt feature into a coarse functional class.
#
# Vectorized port of the notebook's feature_to_class (classifier_version
# "fixed_v1"). The CHECK ORDER is parity-critical -- reordering changes results:
#   1. compositional bias                       -> low_complexity_or_disorder
#   2. site set (active/binding/metal/.../DNA)  -> active_or_binding_site
#   3. TM/signal set                            -> transmembrane_or_signal
#   4. desc-keyword disorder check (BEFORE repeat) -> low_complexity_or_disorder
#   5. repeat / coiled-coil set                 -> repeat_or_coiled_coil
#   6. domain: catalytic-by-keyword else folded
#   7. region / motif                           -> region_or_motif
#   8. else                                     -> other
# ftype and desc are lower-cased + trimmed (NA -> "").
#
# @param ftype character vector of UniProt feature types
# @param desc  character vector of feature descriptions (recycled to ftype)
# @return character vector of feature_class labels
# @noRd
pelsa_feature_to_class <- function(ftype, desc) {
  ftype <- tolower(trimws(ifelse(is.na(ftype), "", as.character(ftype))))
  if (missing(desc) || is.null(desc)) desc <- ""
  desc <- tolower(trimws(ifelse(is.na(desc), "", as.character(desc))))

  n <- length(ftype)
  if (length(desc) == 1L && n != 1L) desc <- rep(desc, n)
  if (length(desc) != n) {
    stop("pelsa_feature_to_class: ftype and desc lengths differ")
  }

  site_set <- c("active site", "binding site", "metal binding",
                "nucleotide binding", "site", "dna binding")
  tm_set   <- c("transmembrane", "signal peptide", "topological domain",
                "intramembrane", "signal")
  repeat_set <- c("repeat", "coiled-coil", "coiled coil")
  catalytic_kw <- c("kinase", "methyltransferase", "transferase", "atpase",
                    "helicase", "protease", "dehydrogenase")

  has_kw <- function(x, kws) {
    Reduce(`|`, lapply(kws, function(k) grepl(k, x, fixed = TRUE)),
           init = rep(FALSE, length(x)))
  }

  disorder_desc <- grepl("low complexity", desc, fixed = TRUE) |
    grepl("compositionally biased", desc, fixed = TRUE) |
    grepl("disordered", desc, fixed = TRUE)

  out <- character(n)
  # default
  out[] <- "other"

  # Evaluate in REVERSE priority so earlier (higher-priority) checks overwrite
  # later ones -- preserving the notebook's first-match-wins order.
  is_region_motif <- ftype %in% c("region", "motif")
  out[is_region_motif] <- "region_or_motif"

  is_domain <- ftype == "domain"
  out[is_domain] <- ifelse(has_kw(desc[is_domain], catalytic_kw),
                           "catalytic_domain", "folded_domain")

  is_repeat <- ftype %in% repeat_set
  out[is_repeat] <- "repeat_or_coiled_coil"

  # desc-keyword disorder check BEATS repeat + region/motif + domain
  out[disorder_desc] <- "low_complexity_or_disorder"

  is_tm <- ftype %in% tm_set
  out[is_tm] <- "transmembrane_or_signal"

  is_site <- ftype %in% site_set
  out[is_site] <- "active_or_binding_site"

  # compositional bias short-circuits FIRST (highest priority)
  is_compbias <- ftype == "compositional bias"
  out[is_compbias] <- "low_complexity_or_disorder"

  out
}

# ---- (A) Parser --------------------------------------------------------------

# Empty 0-row frame with the 8 schema columns + correct types.
# @noRd
pelsa_empty_feature_frame <- function() {
  data.frame(
    accession     = character(0),
    feature_type  = character(0),
    start         = integer(0),
    end           = integer(0),
    description   = character(0),
    feature_class = character(0),
    class_score   = integer(0),
    coord_quality = character(0),
    stringsAsFactors = FALSE
  )
}

# Coerce a possibly-NULL scalar to a single character string ("" if absent).
# @noRd
.pelsa_chr1 <- function(x) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) return("")
  as.character(x)[[1]]
}

# Parse ONE parsed UniProt entry (an R list as returned by
# httr2::resp_body_json() / jsonlite::fromJSON(simplifyVector = FALSE)) into the
# 8-column per-feature data.frame. Verbatim port of _parse_json_features:
#  - accession = entry$primaryAccession
#  - per feature: start = location$start$value, end = location$end$value;
#    SKIP if either is NULL.
#  - ftype = feature$type (""); desc = feature$description ("") with fallback to
#    feature$ligand$name when desc is empty.
#  - coord_quality = "exact" iff both location modifiers == "EXACT" (default
#    "EXACT" when absent), else "fuzzy".
#  - feature_class = pelsa_feature_to_class(ftype, desc); class_score = SCORES[fc]
#
# @param uniprot_json one parsed UniProt entry (list)
# @return 8-column data.frame (0 rows if no usable features)
# @noRd
pelsa_parse_uniprot_json <- function(uniprot_json) {
  if (is.null(uniprot_json) || !is.list(uniprot_json)) {
    return(pelsa_empty_feature_frame())
  }

  accession <- .pelsa_chr1(uniprot_json$primaryAccession)
  features  <- uniprot_json$features
  if (is.null(features) || length(features) == 0L) {
    return(pelsa_empty_feature_frame())
  }

  scores <- pelsa_feature_class_scores()

  # Collect per-feature rows; loop over ONE protein's features is fine (small).
  rows <- vector("list", length(features))
  keep <- logical(length(features))

  for (i in seq_along(features)) {
    feat <- features[[i]]
    loc  <- feat$location
    start_val <- loc$start$value
    end_val   <- loc$end$value
    if (is.null(start_val) || is.null(end_val)) next # skip features missing coords

    ftype <- .pelsa_chr1(feat$type)
    desc  <- .pelsa_chr1(feat$description)
    if (!nzchar(desc)) {
      lig <- feat$ligand$name
      if (!is.null(lig)) desc <- .pelsa_chr1(lig)
    }

    smod <- .pelsa_chr1(loc$start$modifier); if (!nzchar(smod)) smod <- "EXACT"
    emod <- .pelsa_chr1(loc$end$modifier);   if (!nzchar(emod)) emod <- "EXACT"
    coord_quality <- if (smod == "EXACT" && emod == "EXACT") "exact" else "fuzzy"

    fclass <- pelsa_feature_to_class(ftype, desc)
    cscore <- scores[[fclass]]

    keep[i] <- TRUE
    rows[[i]] <- data.frame(
      accession     = accession,
      feature_type  = ftype,
      start         = as.integer(start_val),
      end           = as.integer(end_val),
      description   = desc,
      feature_class = fclass,
      class_score   = as.integer(cscore),
      coord_quality = coord_quality,
      stringsAsFactors = FALSE
    )
  }

  rows <- rows[keep]
  if (length(rows) == 0L) return(pelsa_empty_feature_frame())

  out <- data.table::rbindlist(rows)
  data.table::setDF(out)
  out
}

# Parse a BATCH (list) of parsed UniProt entries, rbinding the per-entry frames.
#
# @param list_of_entries list of parsed UniProt entries
# @return 8-column data.frame (0 rows if nothing usable)
# @noRd
pelsa_parse_uniprot_json_batch <- function(list_of_entries) {
  if (is.null(list_of_entries) || length(list_of_entries) == 0L) {
    return(pelsa_empty_feature_frame())
  }
  frames <- lapply(list_of_entries, pelsa_parse_uniprot_json)
  out <- data.table::rbindlist(frames, use.names = TRUE, fill = TRUE)
  if (nrow(out) == 0L) return(pelsa_empty_feature_frame())
  data.table::setDF(out)
  out
}

# ---- (B) httr2 fetch ---------------------------------------------------------

# UniProt REST base; per-accession `.json` GET. The notebook batches <=500 via
# the search endpoint, but per-accession is simpler and adequate for the app's
# top-up (a handful of cache misses); each request is independently retryable
# and a single 404 only drops one accession.
.PELSA_UNIPROT_BASE <- "https://rest.uniprot.org/uniprotkb"
.PELSA_UNIPROT_UA   <- "pelsa_qc/0.1 (PELSA data pipeline)"
# circuit breaker: stop after this many CONSECUTIVE server (5xx/network) errors
.PELSA_BREAKER_LIMIT <- 5L

# Transient predicate for req_retry: UniProt rate limit + gateway errors.
# @noRd
.pelsa_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
}

# Fetch + parse UniProt features for a set of accessions.
#
# Off the reactive path (runs once at Start-Analysis / species refresh). Each
# accession is fetched as `{accession}.json`, parsed by the pure parser above,
# and accumulated. Accessions that 404, error, or return no usable features go
# into `unresolved` (feeds the Summary "proteins failed annotation fetch" QC
# metric). A simple consecutive-server-error circuit breaker aborts the whole
# run with a clear error so the app can surface "UniProt unavailable".
#
# Network calls use req_retry (exponential backoff, transient 429/5xx, native
# Retry-After) + req_throttle (~10 req/s) + a User-Agent header. DO NOT call
# against the live network in tests.
#
# @param accessions character vector of UniProt accessions
# @param base       UniProt REST base URL (override for testing)
# @param max_tries  per-request retry attempts (default 5)
# @param rate       throttle capacity per second (default 10)
# @return list(features = <8-col data.frame>, unresolved = <character vector>)
# @noRd
pelsa_fetch_uniprot <- function(accessions,
                                base = .PELSA_UNIPROT_BASE,
                                max_tries = 5L,
                                rate = 10L) {
  if (!is.character(accessions)) {
    stop("pelsa_fetch_uniprot: `accessions` must be a character vector")
  }
  accessions <- unique(accessions[!is.na(accessions) & nzchar(accessions)])
  if (length(accessions) == 0L) {
    return(list(features = pelsa_empty_feature_frame(),
                unresolved = character(0)))
  }

  base_req <- httr2::request(base)
  base_req <- httr2::req_user_agent(base_req, .PELSA_UNIPROT_UA)
  base_req <- httr2::req_throttle(base_req, capacity = rate, fill_time_s = 1)
  base_req <- httr2::req_retry(
    base_req,
    max_tries    = max_tries,
    is_transient = .pelsa_is_transient
  )
  # Do not raise on 404 -- a missing accession is "unresolved", not fatal.
  base_req <- httr2::req_error(
    base_req,
    is_error = function(resp) httr2::resp_status(resp) >= 500
  )

  entries    <- list()
  unresolved <- character(0)
  consecutive_server_errors <- 0L

  for (acc in accessions) {
    req <- httr2::req_url_path_append(base_req, paste0(acc, ".json"))

    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) e
    )

    # network / 5xx error (after retries exhausted) -> trip breaker
    if (inherits(resp, "error") || inherits(resp, "condition")) {
      consecutive_server_errors <- consecutive_server_errors + 1L
      unresolved <- c(unresolved, acc)
      if (consecutive_server_errors >= .PELSA_BREAKER_LIMIT) {
        stop(sprintf(
          "pelsa_fetch_uniprot: UniProt unavailable -- %d consecutive errors (last: %s)",
          consecutive_server_errors,
          conditionMessage(resp)
        ))
      }
      next
    }

    status <- httr2::resp_status(resp)
    if (status >= 400L) {
      # 404 / other client error: unresolved, but server is healthy -> reset
      consecutive_server_errors <- 0L
      unresolved <- c(unresolved, acc)
      next
    }
    consecutive_server_errors <- 0L

    parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    df <- pelsa_parse_uniprot_json(parsed)
    if (nrow(df) == 0L) {
      unresolved <- c(unresolved, acc)
    } else {
      entries[[length(entries) + 1L]] <- parsed
    }
  }

  features <- pelsa_parse_uniprot_json_batch(entries)
  list(features = features, unresolved = unique(unresolved))
}
