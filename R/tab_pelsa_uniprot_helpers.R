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

# Accessions that a set of returned UniProt entries "resolves" -- the union of
# every entry's primaryAccession AND its secondaryAccessions. Used so an input
# accession counts as RESOLVED when UniProt returned its entry, regardless of
# whether that entry yielded any parseable features, and including the demerged
# case where an input secondary accession comes back under a different primary.
# @param list_of_entries list of parsed UniProt entries (as resp_body_json yields)
# @return character vector of accessions (possibly empty, no NAs/empties)
# @noRd
.pelsa_entry_accessions <- function(list_of_entries) {
  if (is.null(list_of_entries) || length(list_of_entries) == 0L) {
    return(character(0))
  }
  accs <- unlist(lapply(list_of_entries, function(e) {
    if (is.null(e) || !is.list(e)) return(character(0))
    prim <- .pelsa_chr1(e$primaryAccession)
    sec  <- if (is.null(e$secondaryAccessions)) character(0) else
      vapply(e$secondaryAccessions, .pelsa_chr1, character(1))
    c(prim, sec)
  }), use.names = FALSE)
  accs <- accs[!is.na(accs) & nzchar(accs)]
  unique(accs)
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
# Accessions OR'd into one /search query. HARD-CAPPED at 100: UniProt's /search
# rejects a query with more than 100 OR conditions ("Too many OR conditions in
# query. Maximum allowed is 100." -> HTTP 400), which would silently drop every
# accession in an over-sized batch. The per-page `size` matches it so one page
# usually covers a whole batch (the cursor handles any spillover).
.PELSA_BATCH_SIZE <- 100L

# Transient predicate for req_retry: UniProt rate limit + gateway errors.
# @noRd
.pelsa_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
}

# Syntactically-valid UniProtKB accession (base or "-<n>" isoform). Non-UniProt
# FASTA keys (smORFs, contaminants like "B99901", or "smORF_G1|X") fail this and
# MUST be excluded from a query: a single malformed `accession:` filter value
# makes UniProt reject the WHOLE /search batch with HTTP 400 (dropping every valid
# accession in that batch). Two accession shapes per UniProt's spec.
# @noRd
.PELSA_ACCESSION_RE <- paste0(
  "^([OPQ][0-9][A-Z0-9]{3}[0-9]|",
  "[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})(-[0-9]+)?$"
)
.pelsa_is_valid_accession <- function(x) {
  !is.na(x) & grepl(.PELSA_ACCESSION_RE, x)
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
# cursor. Returns list(entries = <list of entry objects>, failed = <logical>).
#
# `entries` always carries the entries parsed from EVERY successfully-fetched
# page, even when a later page fails -- req_perform_iterative(on_error="return")
# returns the good pages alongside a trailing error condition, so a mid-
# pagination network/5xx must NOT discard the pages already fetched (that would
# silently mark successfully-annotated proteins as unresolved and inflate the
# "proteins failed annotation fetch" QC metric).
#
# `failed` is TRUE when a retry-exhausted network/5xx error terminated
# pagination -- the caller's breaker counts consecutive failed batches. A 4xx
# (e.g. a healthy server rejecting a malformed query) is NOT a failure: it
# yields zero entries for that page, and any un-fetched accessions simply fall
# into `unresolved` downstream.
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

  # Collect the entries from every good page FIRST, so they survive regardless of
  # whether a later page failed.
  ok <- Filter(function(r) inherits(r, "httr2_response"), resps)
  entries <- list()
  for (resp in ok) {
    if (httr2::resp_status(resp) >= 400L) next
    parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    entries <- c(entries, .pelsa_search_results(parsed))
  }

  # A retry-exhausted transient/5xx (or network) error surfaces as a trailing
  # error condition; flag the batch failed so the caller's breaker can count it.
  # NOTE: under base_req's req_error(is_error = status >= 500) policy, httr2
  # returns a 4xx as a NORMAL response (never a condition), so `failed` can only
  # ever contain 5xx (status >= 500) or network errors (no $resp -> NA status).
  # 4xx pages are handled instead by the `resp_status >= 400` skip above. The
  # `is.na(status)` arm therefore only fires for genuine network errors; the
  # `status >= 500L` arm only for server errors -- there is no reachable 4xx
  # case here (a 4xx is a healthy server rejecting the query, not a breaker trip).
  failed <- Filter(function(r) inherits(r, "error") || inherits(r, "condition"),
                   resps)
  batch_failed <- FALSE
  if (length(failed) > 0L) {
    last <- failed[[length(failed)]]
    status <- tryCatch(httr2::resp_status(last$resp), error = function(e) NA_integer_)
    if (is.na(status) || status >= 500L) batch_failed <- TRUE
  }

  list(entries = entries, failed = batch_failed)
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
# @param batch_size accessions per /search query (default .PELSA_BATCH_SIZE = 100,
#                   UniProt's max OR-conditions per query)
# @param on_batch   optional function(done, total) called after each batch.
# @param should_cancel optional function() -> logical; TRUE stops at the next
#                   batch boundary.
# @return list(features = <8-col data.frame>, unresolved = <character vector>,
#              transient_unresolved = <character vector; the failed-batch subset
#              of unresolved>, canceled = <logical scalar>).
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
                unresolved = character(0), zero_feature = character(0),
                transient_unresolved = character(0), canceled = FALSE))
  }
  # INPUT universe (`accessions`) drives resolved/unresolved accounting and may
  # contain isoform suffixes + non-UniProt FASTA keys. The QUERY universe is what
  # we actually OR into /search: isoform-base (UniProt's accession: filter indexes
  # only the base, so "P12345-3" matches nothing), valid-format only (a single
  # malformed term 400s the whole batch), and deduped. An isoform input still
  # counts resolved via the entry's base primaryAccession (see the resolved diff
  # below, which matches on pelsa_isoform_base(accessions)).
  query_accs <- unique(pelsa_isoform_base(
    accessions[.pelsa_is_valid_accession(accessions)]
  ))
  if (length(query_accs) == 0L) {
    # All inputs were non-UniProt keys -> nothing to fetch; all unresolved.
    return(list(features = pelsa_empty_feature_frame(),
                unresolved = accessions, zero_feature = character(0),
                transient_unresolved = character(0), canceled = FALSE))
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

  # Chunk the QUERY accessions into batches (base/valid/deduped, <= batch_size
  # each so the OR-count stays within UniProt's /search limit of 100).
  n <- length(query_accs)
  n_batches <- ceiling(n / batch_size)
  batches <- split(query_accs, rep(seq_len(n_batches),
                                   each = batch_size, length.out = n))

  entries <- list()
  consecutive_batch_failures <- 0L
  canceled <- FALSE
  # Accessions whose batch FAILED (5xx/network/hard error). These are TRANSIENTLY
  # unresolved -- re-running the refresh can recover them -- as opposed to
  # accessions in a SUCCEEDED batch that UniProt simply did not return (genuinely
  # absent: obsolete/404-equivalent, which re-running will never recover).
  failed_accessions <- character(0)

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

    # An unexpected hard error (not the structured partial-failure result) is
    # treated like a fully-failed batch with no salvageable entries.
    if (inherits(fetched, "error") || inherits(fetched, "condition")) {
      fetched <- list(entries = list(), failed = TRUE)
    }

    # ALWAYS keep the entries the batch managed to fetch (a mid-pagination 5xx
    # still returns the good pages); their accessions become resolved. Only the
    # un-fetched accessions of a failed batch fall into `unresolved` downstream.
    entries <- c(entries, fetched$entries)

    if (isTRUE(fetched$failed)) {
      failed_accessions <- c(failed_accessions, b)
      consecutive_batch_failures <- consecutive_batch_failures + 1L
      if (consecutive_batch_failures >= .PELSA_BREAKER_LIMIT) {
        stop(sprintf(
          paste0("pelsa_fetch_uniprot: UniProt unavailable -- %d consecutive ",
                 "batch failures"),
          consecutive_batch_failures))
      }
    } else {
      consecutive_batch_failures <- 0L
    }
    .report(k, n_batches)
  }

  features <- pelsa_parse_uniprot_json_batch(entries)

  # unresolved = the input accessions NOT present in any returned ENTRY. This
  # covers 404-equivalents (absent from UniProt), accessions in a failed batch,
  # and (on cancel) the not-yet-fetched accessions. The caller retains their
  # cached rows and does NOT write a partial cache on cancel.
  #
  # IMPORTANT: "resolved" is ENTRY presence, NOT feature presence. A valid entry
  # that returned with zero usable features is still resolved (so it does not
  # inflate the "failed annotation" QC count or retain stale cache rows). And a
  # demerged/secondary input accession returned under its primaryAccession is
  # resolved via the entry's secondaryAccessions. An ISOFORM input ("P12345-2")
  # is returned under its base primaryAccession ("P12345") and is NOT listed in
  # secondaryAccessions, so we also match on the isoform base - otherwise every
  # isoform input would be wrongly counted unresolved (inflating n_unresolved and
  # firing a spurious "re-run when UniProt is reachable" refresh warning). We keep
  # `unresolved` over the input universe.
  entry_acc <- .pelsa_entry_accessions(entries)
  resolved <- accessions[
    accessions %in% entry_acc |
      pelsa_isoform_base(accessions) %in% entry_acc
  ]
  unresolved <- setdiff(accessions, resolved)
  # The TRANSIENT subset of unresolved (failed-batch accessions). The caller only
  # prompts "re-run when UniProt is reachable" for these; genuinely-absent
  # accessions (unresolved but never in a failed batch) get a neutral note.
  transient_unresolved <- intersect(unresolved, failed_accessions)

  # zero_feature = RESOLVED accessions that produced no parsed feature row. An
  # entry can come back valid but featureless; it is NOT unresolved (UniProt
  # answered), it simply has nothing to annotate. We surface it so the caller can
  # cache a sentinel (stop re-fetching it) and report it as a distinct category.
  # Match feature presence with the same isoform-base fallback used for resolved.
  feat_acc <- if (nrow(features) > 0L) unique(as.character(features$accession))
              else character(0)
  has_feature <- resolved %in% feat_acc |
    pelsa_isoform_base(resolved) %in% feat_acc
  zero_feature <- unique(resolved[!has_feature])

  list(features = features, unresolved = unresolved,
       zero_feature = zero_feature,
       transient_unresolved = transient_unresolved, canceled = canceled)
}
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
#   pelsa_full_universe(gcts, existing_cache, fasta_map = NULL)
#       -> character vector: the FASTA proteome (full-refresh universe).
#   pelsa_incremental_universe(gcts, existing_cache, fasta_map = NULL)
#       -> character vector: (dataset U fasta) - cache (incremental universe).
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

# ---- Helper 1: accession universes (mode-specific) ---------------------------

# Internal: exploded unique dataset accessions across all uploaded GCTs.
# @noRd
.pelsa_dataset_universe <- function(gcts) {
  if (is.null(gcts) || length(gcts) == 0L) return(character(0))
  raw <- unlist(lapply(gcts, .pelsa_dataset_accession_strings), use.names = FALSE)
  .pelsa_explode_accession_tokens(raw)
}

# Internal: FASTA accessions (the proteome) from a fasta_map (names), cleaned.
# @noRd
.pelsa_fasta_universe <- function(fasta_map) {
  if (is.null(fasta_map) || length(fasta_map) == 0L) return(character(0))
  acc <- names(fasta_map)
  if (is.null(acc)) return(character(0))
  unique(acc[!is.na(acc) & nzchar(acc)])
}

# Internal: accessions already present in an existing feature cache.
# @noRd
.pelsa_cache_universe <- function(existing_cache) {
  if (is.null(existing_cache) || !is.data.frame(existing_cache) ||
      !("accession" %in% colnames(existing_cache)) ||
      nrow(existing_cache) == 0L) {
    return(character(0))
  }
  acc <- unique(as.character(existing_cache$accession))
  acc[!is.na(acc) & nzchar(acc)]
}

# FULL-mode universe: the whole FASTA proteome ONLY. A full refresh wipes the
# species' feature cache and rebuilds it from the FASTA; it deliberately ignores
# uploaded-dataset accessions (those are topped up via an incremental refresh)
# AND the existing cache (which has just been wiped). Sorted unique.
#
# @param gcts          uploaded datasets (IGNORED; kept for a uniform signature).
# @param existing_cache the species' current cache (IGNORED; wiped before fetch).
# @param fasta_map     named list accession -> sequence (from pelsa_read_fasta).
# @return character vector of FASTA accessions (sorted unique; may be empty).
# @noRd
pelsa_full_universe <- function(gcts, existing_cache, fasta_map = NULL) {
  sort(.pelsa_fasta_universe(fasta_map))
}

# INCREMENTAL-mode universe: (uploaded-dataset accessions UNION FASTA accessions)
# MINUS the accessions already in the existing cache. Drives the species toward
# full proteome + dataset coverage over repeated runs WITHOUT re-fetching what is
# already cached. Disjoint from the cache by construction. Sorted unique.
#
# @param gcts          uploaded datasets (cmapR GCTs or data.frames carrying
#                      PG.ProteinAccessions), or NULL/empty. Already filtered to
#                      the target species upstream (pelsa_gcts_for_species).
# @param existing_cache the species' current feature data.frame (needs an
#                      `accession` column); may be NULL/0-row.
# @param fasta_map     named list accession -> sequence (from pelsa_read_fasta);
#                      always included (not just a no-datasets fallback).
# @return character vector of accessions to (re)fetch (sorted unique).
# @noRd
pelsa_incremental_universe <- function(gcts, existing_cache, fasta_map = NULL) {
  needed <- unique(c(.pelsa_dataset_universe(gcts),
                     .pelsa_fasta_universe(fasta_map)))
  sort(setdiff(needed, .pelsa_cache_universe(existing_cache)))
}

# ---- Helper 1b: full-mode clean-slate wipe -----------------------------------

# Delete every top-level entry under a species directory EXCEPT the `fasta/`
# folder (and its contents) -- the clean-slate a FULL refresh performs BEFORE
# re-fetching the proteome. This removes the prior uniprot_features/ cache AND
# the uniprot_membrane/ annotation (both regenerable / re-obtainable; the feature
# cache is rebuilt by the ensuing full fetch). DESTRUCTIVE + irreversible: the
# membrane file is gitignored and not produced by this app. Called only on the
# full-refresh path, only AFTER the user confirms.
#
# No-op-safe: a missing species_dir deletes nothing and returns character(0).
#
# @param species_dir the species directory (file.path(database_dir, species)).
# @return invisibly, the character vector of deleted top-level entry names.
# @noRd
pelsa_wipe_species_cache <- function(species_dir) {
  if (!is.character(species_dir) || length(species_dir) != 1L ||
      is.na(species_dir) || !nzchar(species_dir) || !dir.exists(species_dir)) {
    return(invisible(character(0)))
  }
  entries <- list.files(species_dir, all.files = TRUE, no.. = TRUE)
  to_delete <- setdiff(entries, "fasta")
  for (e in to_delete) {
    unlink(file.path(species_dir, e), recursive = TRUE, force = TRUE)
  }
  invisible(to_delete)
}

# ---- Helper 1c: zero-feature sentinel rows -----------------------------------

# Build SENTINEL feature rows for accessions UniProt resolved with ZERO features.
# A 0-feature accession has no natural feature row, so without a sentinel it
# leaves no trace in the cache and an incremental refresh re-fetches it forever.
# The sentinel marks "resolved, genuinely no features": it puts the accession in
# cache$accession (so incremental skips it) while carrying NA coordinates +
# feature_class "none" so the annotation overlap drops it silently (see
# pelsa_annotate_features) and the Summary QC counts it as "0 annotations"
# rather than "failed annotation".
#
# @param accessions character vector of resolved-but-0-feature accessions.
# @return 8-col feature data.frame (0 rows for empty input); one row per unique
#         non-empty accession, in canonical schema column order.
# @noRd
pelsa_zero_feature_rows <- function(accessions) {
  accs <- unique(as.character(accessions))
  accs <- accs[!is.na(accs) & nzchar(accs)]
  if (length(accs) == 0L) return(pelsa_empty_feature_frame())
  data.frame(
    accession     = accs,
    feature_type  = "",
    start         = NA_integer_,
    end           = NA_integer_,
    description   = "",
    feature_class = "none",
    class_score   = 0L,
    coord_quality = "",
    stringsAsFactors = FALSE
  )
}

# Subset uploaded datasets to those whose SELECTED SPECIES matches `species`.
#
# Defect #1 guard: a species refresh must only fetch accessions belonging to that
# species. Multiple uploaded datasets of the SAME species all match (so their
# accessions UNION downstream via pelsa_incremental_universe, which already
# unions across every GCT it is handed); datasets of OTHER species are dropped
# (otherwise their accessions would be fetched into the wrong species' cache --
# the human-into-mouse spillover). A dataset whose species is unset ("(none)") or
# absent never matches a real species code.
#
# Pure + name-keyed (uploaded_gcts and species_by_ds share dataset-name keys).
#
# @param uploaded_gcts named list of uploaded datasets (or NULL/empty).
# @param species_by_ds named list ds -> selected-species chr scalar
#                      (setup_state$species). May be missing keys.
# @param species       the single refresh-target species code.
# @return the name-keyed subset of uploaded_gcts (NULL/empty passed through).
# @noRd
pelsa_gcts_for_species <- function(uploaded_gcts, species_by_ds, species) {
  if (is.null(uploaded_gcts) || length(uploaded_gcts) == 0L) {
    return(uploaded_gcts)
  }
  keep <- vapply(
    names(uploaded_gcts),
    function(ds) identical(species_by_ds[[ds]], species),
    logical(1)
  )
  uploaded_gcts[keep]
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
#                    pelsa_full_universe / pelsa_incremental_universe).
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
                                        should_cancel = NULL,
                                        mode = "incremental") {
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
  mode <- match.arg(mode, c("incremental", "full"))
  # FULL mode rebuilds from scratch: the wipe (below) clears the prior cache, so
  # there is nothing to merge against -- force existing = NULL so the fresh frame
  # fully supersedes (and n_retained_from_cache is 0).
  if (identical(mode, "full")) existing <- NULL
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
         n_retained_from_cache = 0L, n_zero_feature = 0L,
         n_with_features = 0L, mode = mode, canceled = TRUE)
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

  # FULL mode: clean slate BEFORE any network -- delete the prior feature +
  # membrane caches (sparing fasta/). Done only after the pre-fetch cancel check
  # above, so a cancel never wipes. A subsequent fetch failure leaves the species
  # fasta-only (the user re-runs Full refresh); this is the documented, accepted
  # trade-off for a true clean rebuild.
  if (identical(mode, "full")) {
    pelsa_wipe_species_cache(species_dir)
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
  # Transient (failed-batch) subset of unresolved: re-running can recover these,
  # so only they justify the "re-run when UniProt is reachable" warning. Older
  # injected stub fetchers may not return this field; default to character(0).
  transient_unresolved <- fetched$transient_unresolved %||% character(0)
  zero_feature <- fetched$zero_feature %||% character(0)
  # Distinct accessions with >= 1 REAL feature this fetch (the `fresh` frame
  # BEFORE sentinels are folded in). An ACCESSION count, mutually exclusive with
  # the zero-feature + unresolved accession counts, so the refresh summary
  # reports three non-overlapping protein tallies (not a row count that would
  # double-count sentinels). 0 rows -> 0 accessions.
  n_with_features <- if (is.data.frame(fresh) && nrow(fresh) > 0L) {
    length(unique(as.character(fresh$accession)))
  } else {
    0L
  }

  # Persist resolved-but-0-feature accessions as SENTINEL rows so they live in
  # cache$accession and an incremental refresh stops re-fetching them. Sentinels
  # are merged exactly like feature rows (their accessions are resolved, so they
  # are not in `unresolved`). Both modes write them.
  fresh <- rbind(fresh, pelsa_zero_feature_rows(zero_feature))

  # MERGE the fresh frame with the prior cache.
  #   FULL: existing was forced NULL above -> fresh fully supersedes (clean
  #     rebuild). pelsa_merge_feature_cache(NULL, fresh, ...) returns fresh.
  #   INCREMENTAL: the universe is DISJOINT from the cache by construction
  #     (pelsa_incremental_universe subtracts cached accessions), so EVERY prior
  #     cache row must be retained "atop" the fresh rows -- not just the
  #     unresolved subset. We therefore retain old rows for every existing
  #     accession NOT in `fresh` (= the whole prior cache). The supersede-only
  #     merge (which retains solely `unresolved`) would WRONGLY drop untouched
  #     cache rows, so incremental routes through retain-all-untouched instead.
  if (identical(mode, "full")) {
    merged <- pelsa_merge_feature_cache(existing, fresh, unresolved)
  } else {
    retain_acc <- if (is.data.frame(existing) && nrow(existing) > 0L &&
                      "accession" %in% colnames(existing)) {
      unique(as.character(existing$accession))
    } else {
      character(0)
    }
    merged <- pelsa_merge_feature_cache(existing, fresh,
                                        unresolved = retain_acc)
  }
  n_retained <- nrow(merged) - nrow(fresh)

  .progress(0.85, sprintf("Writing %s cache", species),
            sprintf("%d feature rows", nrow(merged)))
  path <- pelsa_write_feature_cache(merged, species_dir)
  .progress(1.0, sprintf("Done: %s", species), NULL)

  list(
    features               = merged,
    unresolved             = unresolved,
    transient_unresolved   = transient_unresolved,
    path                   = path,
    n_features             = nrow(merged),
    n_unresolved           = length(unresolved),
    n_transient_unresolved = length(transient_unresolved),
    n_zero_feature         = length(zero_feature),
    n_with_features        = n_with_features,
    n_accessions           = length(universe),
    n_retained_from_cache  = n_retained,
    mode                   = mode,
    canceled               = FALSE
  )
}

# ---- Helper 4: multi-species orchestration (observer-facing) -----------------

# Resolve ONE species' refresh inputs from disk: its existing cache (best-effort)
# and the species' FASTA accession map. Pure-ish: reads files only; never
# fetches/writes.
#
# The FASTA is ALWAYS read (not gated on whether datasets are uploaded): both
# refresh modes need it -- FULL fetches the FASTA proteome only, and INCREMENTAL
# unions the FASTA accessions with the dataset accessions before subtracting the
# cache. `uploaded_gcts` is retained in the signature for caller symmetry but no
# longer gates the read.
#
# @param species_dir   the species directory (file.path(database_dir, species)).
# @param uploaded_gcts named list of uploaded datasets, or NULL/empty (unused
#                      here; kept for signature symmetry with the callers).
# @return list(existing = <feature df or NULL>, fasta_map = <named list or NULL>).
#         fasta_map is a names-only list (names = FASTA accessions, empty values)
#         -- the refresh universe needs only the accession set, not sequences.
# @noRd
pelsa_species_refresh_inputs <- function(species_dir, uploaded_gcts) {
  existing <- tryCatch(pelsa_read_feature_cache(species_dir),
                       error = function(e) NULL)

  fdir <- file.path(species_dir, "fasta")
  fastas <- if (dir.exists(fdir)) {
    list.files(fdir, pattern = "\\.(fasta|fa)$", full.names = TRUE,
               ignore.case = TRUE)
  } else {
    character(0)
  }
  fasta_map <- if (length(fastas) > 0L) {
    # The refresh path uses ONLY the FASTA accession names (the universe
    # functions take names(fasta_map)), never the sequences -- so read keys only
    # via the lightweight pelsa_read_fasta_accessions rather than building the
    # whole proteome sequence map on the event loop. Returned as a named list
    # (names = accessions, values empty) so names(fasta_map) is the accession set.
    # Refresh only ever runs for UniProt (taxon-code) species (the checklist
    # filters out self-curated), so parse in UniProt mode (pipe-aware).
    accs <- tryCatch(pelsa_read_fasta_accessions(fastas[[1]], mode = "uniprot"),
                     error = function(e) character(0))
    if (length(accs) > 0L) stats::setNames(vector("list", length(accs)), accs)
    else NULL
  } else {
    NULL
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
                                      should_cancel = NULL,
                                      mode = "incremental") {
  if (!is.character(species) || length(species) == 0L) {
    stop("pelsa_run_species_refresh: `species` must be a non-empty character ",
         "vector", call. = FALSE)
  }
  mode <- match.arg(mode, c("incremental", "full"))
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
      universe <- if (identical(mode, "full")) {
        pelsa_full_universe(uploaded_gcts, io$existing,
                            fasta_map = io$fasta_map)
      } else {
        pelsa_incremental_universe(uploaded_gcts, io$existing,
                                   fasta_map = io$fasta_map)
      }

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
        should_cancel = should_cancel, mode = mode
      )
      list(species = sp, n_features = res$n_features,
           n_unresolved = res$n_unresolved,
           n_transient_unresolved = res$n_transient_unresolved,
           n_zero_feature = res$n_zero_feature,
           n_with_features = res$n_with_features,
           n_accessions = res$n_accessions,
           n_retained_from_cache = res$n_retained_from_cache,
           had_existing = had_existing, mode = mode, path = res$path,
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
#   - a species that HAD a cache and finished with TRANSIENT unresolved
#     accessions (failed UniProt batches) -> a "warning" prompting a re-run,
#     reporting how many prior rows were retained.
#   - a species that HAD a cache and finished with only GENUINELY-ABSENT
#     unresolved accessions (obsolete/withdrawn; re-running cannot help) ->
#     a neutral "message" note, NOT a warning.
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
    if (!isTRUE(r$had_existing) || !isTRUE(r$n_unresolved > 0L)) next

    # Split unresolved into TRANSIENT (failed-batch; re-running recovers them) vs
    # genuinely-absent (UniProt never returns them: obsolete/404-equivalent). Only
    # the transient case justifies the amber "re-run when reachable" warning. A
    # result lacking n_transient_unresolved (e.g. an older injected stub fetcher)
    # is treated as unknown -> fall back to the conservative warning.
    n_transient <- r$n_transient_unresolved
    if (is.null(n_transient)) {
      add(sprintf(paste0("%s refreshed with %d unresolved accession(s); %d ",
                         "previously-cached row(s) retained. Re-run when ",
                         "UniProt is reachable for full coverage."),
                  r$species, r$n_unresolved, r$n_retained_from_cache),
          "warning", NULL)
    } else if (n_transient > 0L) {
      add(sprintf(paste0("%s refreshed with %d unresolved accession(s) (%d from ",
                         "a transient UniProt failure); %d previously-cached ",
                         "row(s) retained. Re-run when UniProt is reachable for ",
                         "full coverage."),
                  r$species, r$n_unresolved, n_transient,
                  r$n_retained_from_cache),
          "warning", NULL)
    } else {
      # All unresolved are genuinely absent from UniProt -- re-running will not
      # help. Neutral, non-amber note; no re-run prompt.
      add(sprintf(paste0("%s: %d accession(s) are not in UniProt (obsolete or ",
                         "withdrawn); %d previously-cached row(s) retained."),
                  r$species, r$n_unresolved, r$n_retained_from_cache),
          "message", 10)
    }
  }
  if (length(done) > 0L) {
    summaries <- vapply(done, function(r) {
      # All three are mutually-exclusive ACCESSION (protein) counts.
      zf <- r$n_zero_feature %||% 0L
      wf <- r$n_with_features %||% 0L
      if (identical(r$mode, "full")) {
        sprintf(paste0("%s: rebuilt - %d proteins with features, %d with no ",
                       "features, %d unresolved (previous feature + membrane ",
                       "files cleared)"),
                r$species, wf, zf, r$n_unresolved)
      } else {
        sprintf(paste0("%s: topped up - %d proteins with features, %d with no ",
                       "features, %d unresolved, %d retained"),
                r$species, wf, zf, r$n_unresolved,
                r$n_retained_from_cache)
      }
    }, character(1))
    add(paste0("UniProt annotation refresh complete. ",
               paste(summaries, collapse = "; ")), "message", 10)
  }
  notes
}

# ---- Helper 5: confirm-gate universe size (pure, observer-facing) -------------

# Estimate the TOTAL accession universe a refresh of `species` would fetch, so
# the observer can WARN + confirm before a fetch. Reads each species' existing
# cache + FASTA exactly as the real run does, routes to the mode's universe
# function, then sums the per-species universe sizes. Pure-ish (reads files only;
# never fetches/writes), so the observer can call it synchronously pre-fetch.
# The returned count is the TRUE to-be-fetched count for the mode (full = the
# FASTA proteome; incremental = (dataset U fasta) - cache), so the confirmed
# number matches the number actually fetched.
#
# @param species       character vector of species names.
# @param database_dir  the PELSA database dir.
# @param uploaded_gcts named list of uploaded datasets, or NULL.
# @param mode          "full" (FASTA proteome) or "incremental"
#                      ((dataset U fasta) - cache).
# @return list(total = <int>, per_species = <named int vector>).
# @noRd
pelsa_refresh_universe_size <- function(species, database_dir, uploaded_gcts,
                                        mode = "incremental") {
  mode <- match.arg(mode, c("incremental", "full"))
  per <- vapply(species, function(sp) {
    species_dir <- file.path(database_dir, sp)
    io <- pelsa_species_refresh_inputs(species_dir, uploaded_gcts)
    universe <- if (identical(mode, "full")) {
      pelsa_full_universe(uploaded_gcts, io$existing, fasta_map = io$fasta_map)
    } else {
      pelsa_incremental_universe(uploaded_gcts, io$existing,
                                 fasta_map = io$fasta_map)
    }
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
pelsa_refresh_eta_text <- function(total, batch_size = 100L, page_secs = 0.9) {
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
    # Mutually-exclusive ACCESSION (protein) counts.
    zf <- r$n_zero_feature %||% 0L
    wf <- r$n_with_features %||% 0L
    line <- if (identical(r$mode, "full")) {
      sprintf(paste0("%s: rebuilt - %d proteins with features, %d with no ",
                     "features, %d unresolved (cache cleared)"),
              r$species, wf, zf, r$n_unresolved %||% 0L)
    } else {
      sprintf(paste0("%s: topped up - %d proteins with features, %d with no ",
                     "features, %d unresolved, %d retained from cache"),
              r$species, wf, zf, r$n_unresolved %||% 0L,
              r$n_retained_from_cache %||% 0L)
    }
    items <- c(items, list(shiny::tags$li(line)))
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
