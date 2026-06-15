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

# UniProt REST. We fetch in BATCHES via the /search endpoint
# (?query=accession:(P1 OR P2 OR ...)) rather than one {accession}.json GET per
# accession. A full species rebuild has tens of thousands of accessions; at the
# old serial ~10 req/s that was hours. Batching turns N requests into
# ceil(N / batch_size) page requests (each /search "page" returns up to `size`
# entries; UniProt paginates the remainder via a Link: rel="next" cursor header),
# an order-of-magnitude fewer requests for the SAME parsed output.
#
# IMPORTANT (accuracy): we do NOT pass a feature `fields=` projection. A
# projection would have to enumerate every UniProt feature type the classifier
# reads, and any omission would SILENTLY drop features. Omitting `fields` returns
# full entries (all feature types), so the batched parse is byte-identical to the
# per-accession parse. The win here is purely the request count, not payload
# trimming.
.PELSA_UNIPROT_BASE        <- "https://rest.uniprot.org/uniprotkb"
.PELSA_UNIPROT_SEARCH_PATH <- "search"
.PELSA_UNIPROT_UA          <- "pelsa_qc/0.1 (PELSA data pipeline)"
# circuit breaker: stop after this many CONSECUTIVE failed BATCHES (network/5xx
# after retries) so a UniProt outage surfaces a clear error instead of grinding.
.PELSA_BREAKER_LIMIT <- 5L
# Accessions OR'd into one /search query. Kept modest so the query string stays
# well within URL limits; the per-page `size` matches it so one page usually
# covers a whole batch (the cursor handles any spillover).
.PELSA_BATCH_SIZE <- 200L

# Transient predicate for req_retry: UniProt rate limit + gateway errors.
# @noRd
.pelsa_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
}

# Build the /search query value for ONE batch of accessions:
#   accession:(P1 OR P2 OR ... )
# req_url_query() URL-encodes it, so spaces/colons/parens are safe.
# @noRd
.pelsa_accession_query <- function(accs) {
  paste0("accession:(", paste(accs, collapse = " OR "), ")")
}

# Pull the `results` list out of one parsed /search page (the JSON shape is
# {"results": [ <entry>, ... ]}). Returns list() for an empty/odd page.
# @noRd
.pelsa_search_results <- function(parsed) {
  if (is.null(parsed) || !is.list(parsed)) return(list())
  res <- parsed$results
  if (is.null(res) || !is.list(res)) return(list())
  res
}

# Fetch ALL pages for ONE batch's query, following UniProt's Link: rel="next"
# cursor. Returns the accumulated list of entry objects (each an entry as
# resp_body_json() yields). Throws on retry-exhausted network/5xx so the caller's
# breaker can count consecutive batch failures. A 4xx (e.g. malformed query)
# yields zero entries for the batch (its accessions become unresolved).
# @noRd
.pelsa_fetch_one_batch <- function(base_req, accs, size) {
  page_req <- httr2::req_url_path_append(base_req, .PELSA_UNIPROT_SEARCH_PATH)
  page_req <- httr2::req_url_query(
    page_req,
    query  = .pelsa_accession_query(accs),
    format = "json",
    size   = size
  )

  # req_perform_iterative + iterate_with_link_url("next") walks the cursor pages
  # until no rel="next" Link remains. on_error = "return" stops paginating on a
  # failed page but keeps the good pages collected so far.
  resps <- httr2::req_perform_iterative(
    page_req,
    next_req = httr2::iterate_with_link_url(rel = "next"),
    max_reqs = Inf,
    on_error = "return"
  )

  # A retry-exhausted transient/5xx (or network) error surfaces as the LAST
  # element being an error condition; re-throw so the batch counts as failed.
  failed <- Filter(function(r) inherits(r, "error") || inherits(r, "condition"),
                   resps)
  if (length(failed) > 0L) {
    # Only treat SERVER/network failures as batch failures; a 4xx is a healthy
    # server rejecting the query -> zero entries, not a breaker trip.
    last <- failed[[length(failed)]]
    status <- tryCatch(httr2::resp_status(last$resp), error = function(e) NA_integer_)
    if (is.na(status) || status >= 500L) {
      stop(last)
    }
  }

  ok <- Filter(function(r) inherits(r, "httr2_response"), resps)
  entries <- list()
  for (resp in ok) {
    if (httr2::resp_status(resp) >= 400L) next
    parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    entries <- c(entries, .pelsa_search_results(parsed))
  }
  entries
}

# Fetch + parse UniProt features for a set of accessions (BATCHED /search).
#
# Off the reactive path (runs once at Start-Analysis / species refresh). The
# accessions are chunked (batch_size each), every chunk fetched as a single
# /search query (paginated by cursor), and all returned entries parsed by the
# pure parser above. Accessions NOT returned by any page (404-equivalent: not in
# UniProt, or returned with no usable features) go into `unresolved` (feeds the
# Summary "proteins failed annotation fetch" QC metric). A consecutive-failed-
# BATCH circuit breaker aborts with a clear error so the app can surface "UniProt
# unavailable".
#
# CONTRACT (unchanged): returns list(features = <8-col data.frame>, unresolved =
# <character vector>), identical in shape + content to the prior per-accession
# fetcher for the same accessions.
#
# Network calls use req_retry (exponential backoff, transient 429/5xx, native
# Retry-After) + req_throttle + a User-Agent header. DO NOT call against the live
# network in tests.
#
# PROGRESS + COOPERATIVE CANCEL: the fetch loops over batches (one /search query
# per batch). `on_batch(done, total)` (optional) is called after EACH batch with
# the number of batches done / the total, so a caller can drive a page-level
# progress bar. `should_cancel()` (optional) is checked BEFORE each batch; when
# it returns TRUE the fetch stops at that batch boundary (no batch is left
# half-done) and returns with `canceled = TRUE`. Neither callback fires on the
# empty-input fast path. Both default to NULL (no behavior change; the helpers
# remain pure + network-free in tests).
#
# @param accessions character vector of UniProt accessions
# @param base       UniProt REST base URL (override for testing)
# @param max_tries  per-request retry attempts (default 5)
# @param rate       throttle capacity per second (default 10)
# @param batch_size accessions per /search query (default 200)
# @param on_batch   optional function(done, total) called after each batch.
# @param should_cancel optional function() -> logical; TRUE stops at the next
#                   batch boundary.
# @return list(features = <8-col data.frame>, unresolved = <character vector>,
#              canceled = <logical scalar>).
# @noRd
pelsa_fetch_uniprot <- function(accessions,
                                base = .PELSA_UNIPROT_BASE,
                                max_tries = 5L,
                                rate = 10L,
                                batch_size = .PELSA_BATCH_SIZE,
                                on_batch = NULL,
                                should_cancel = NULL) {
  if (!is.character(accessions)) {
    stop("pelsa_fetch_uniprot: `accessions` must be a character vector")
  }
  accessions <- unique(accessions[!is.na(accessions) & nzchar(accessions)])
  if (length(accessions) == 0L) {
    return(list(features = pelsa_empty_feature_frame(),
                unresolved = character(0), canceled = FALSE))
  }
  batch_size <- max(1L, as.integer(batch_size))
  .cancel <- function() {
    is.function(should_cancel) && isTRUE(should_cancel())
  }
  .report <- function(done, total) {
    if (is.function(on_batch)) on_batch(done, total)
  }

  base_req <- httr2::request(base)
  base_req <- httr2::req_user_agent(base_req, .PELSA_UNIPROT_UA)
  base_req <- httr2::req_throttle(base_req, capacity = rate, fill_time_s = 1)
  base_req <- httr2::req_retry(
    base_req,
    max_tries    = max_tries,
    is_transient = .pelsa_is_transient
  )
  # Do not raise on 4xx -- a query that matches nothing is "unresolved", not
  # fatal; only 5xx/network are errors (counted by the breaker).
  base_req <- httr2::req_error(
    base_req,
    is_error = function(resp) httr2::resp_status(resp) >= 500
  )

  # Chunk the accessions into batches.
  n <- length(accessions)
  n_batches <- ceiling(n / batch_size)
  batches <- split(accessions, rep(seq_len(n_batches),
                                   each = batch_size, length.out = n))

  entries <- list()
  consecutive_batch_failures <- 0L
  canceled <- FALSE

  for (k in seq_along(batches)) {
    # Cooperative cancel: honor a stop request at the batch boundary (before any
    # network for this batch), so a batch is never left half-fetched.
    if (.cancel()) {
      canceled <- TRUE
      break
    }
    b <- batches[[k]]

    fetched <- tryCatch(
      .pelsa_fetch_one_batch(base_req, b, size = batch_size),
      error = function(e) e
    )

    if (inherits(fetched, "error") || inherits(fetched, "condition")) {
      consecutive_batch_failures <- consecutive_batch_failures + 1L
      # accessions in this failed batch are (for now) unresolved; the merge in
      # the caller retains any previously-cached rows for them.
      if (consecutive_batch_failures >= .PELSA_BREAKER_LIMIT) {
        stop(sprintf(
          paste0("pelsa_fetch_uniprot: UniProt unavailable -- %d consecutive ",
                 "batch failures (last: %s)"),
          consecutive_batch_failures, conditionMessage(fetched)))
      }
      .report(k, n_batches)
      next
    }
    consecutive_batch_failures <- 0L
    entries <- c(entries, fetched)
    .report(k, n_batches)
  }

  features <- pelsa_parse_uniprot_json_batch(entries)

  # unresolved = the input accessions NOT present in any returned entry. This
  # covers 404-equivalents (absent from UniProt), accessions in a failed batch,
  # and (on cancel) the not-yet-fetched accessions. The caller retains their
  # cached rows and does NOT write a partial cache on cancel.
  resolved <- if (nrow(features) > 0L) {
    unique(as.character(features$accession))
  } else {
    character(0)
  }
  unresolved <- setdiff(accessions, resolved)

  list(features = features, unresolved = unresolved, canceled = canceled)
}
