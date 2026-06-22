################################################################################
# PELSA UniProt fetch + feature-cache + per-species refresh + species-resolution.
#
# Consolidated OFFLINE suite. Merged verbatim from four former files:
#   - test-pelsa-uniprot-fetch-offline.R   (pelsa_fetch_uniprot internals)
#   - test-pelsa-refresh.R                 (per-species annotation refresh)
#   - test-pelsa-refresh-benchmark.R       (runtime characterization / model)
#   - test-pelsa-species-resolve.R         (species classification + resolution)
#
# Every suite is hermetic: injected fetch_fn / mocked httr2 / stub validate_fn.
# NO real network is touched. The benchmark "network" is a calibrated arithmetic
# model, never a real request.
#
# Cross-file contracts deliberately preserved:
#   - the 8-column feature-frame schema (accession, feature_type, start, end,
#     description, feature_class, class_score, coord_quality), and
#   - the pelsa_fetch_uniprot return field-set (features / unresolved /
#     zero_feature / transient_unresolved / canceled).
#
# Helpers from each former file are kept under their original names; there were
# no name collisions across the four files (verified at merge time), so no
# helper was renamed and no source()/`%||%` line needed de-duplication. Only a
# single `library(testthat)` is kept (was duplicated across two of the files).
################################################################################

library(testthat)

# ===========================================================================
# --- from uniprot-fetch-offline ---
# ===========================================================================
# Phase 3 (P3.5) -- pelsa_fetch_uniprot internals, fully OFFLINE.
#
# The batch loop, the consecutive-failed-batch circuit breaker, the on_batch
# progress callback, and the 4xx-vs-5xx split are otherwise reachable only via the
# live network (those tests skip on CI). Here we mock the internal seam
# (.pelsa_fetch_one_batch) for the batch-loop logic, and mock httr2 itself for the
# one-batch 4xx/5xx discrimination. No network is touched.

# ===========================================================================
# Batch loop: count, on_batch callback, breaker, 4xx-vs-5xx at the loop level
# ===========================================================================

# Helper: a parsed-entry stub the real batch fn would return (one accession).
# Carries ONE feature with exact coords so pelsa_parse_uniprot_json emits a row
# (and thus the accession appears in features$accession -> "resolved").
fake_entry <- function(acc) {
  list(
    primaryAccession = acc,
    sequence = list(length = 10L),
    features = list(list(
      type = "Domain",
      description = "test domain",
      location = list(
        start = list(value = 1L, modifier = "EXACT"),
        end   = list(value = 5L, modifier = "EXACT")
      )
    ))
  )
}

# A VALID UniProt entry that was returned but carries NO usable features
# (no `features`). It produces zero feature rows, but the protein WAS resolved
# (UniProt returned it), so its accession must NOT be reported unresolved.
fake_entry_no_features <- function(acc) {
  list(primaryAccession = acc, sequence = list(length = 10L))
}

# An entry returned under `primary`, carrying `secondary` in secondaryAccessions
# (the demerged-accession case: a /search on the secondary returns this entry).
fake_entry_with_secondary <- function(primary, secondary) {
  e <- fake_entry(primary)
  e$secondaryAccessions <- list(secondary)
  e
}

test_that("on_batch fires once per batch with (done, total)", {
  seen <- list()
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      list(entries = lapply(accs, fake_entry), failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(
    c("P00001", "P00002", "P00003", "P00004", "P00005"),
    batch_size = 2L,                      # -> 3 batches of 2,2,1
    on_batch = function(done, total) seen[[length(seen) + 1L]] <<- c(done, total)
  )
  # 3 batches -> 3 callbacks, done = 1,2,3 and total = 3 each time
  expect_equal(length(seen), 3L)
  expect_equal(seen[[1]], c(1L, 3L))
  expect_equal(seen[[3]], c(3L, 3L))
  # all accessions resolved (the fake entries carry each accession)
  expect_setequal(res$features$accession,
                  c("P00001", "P00002", "P00003", "P00004", "P00005"))
  expect_length(res$unresolved, 0L)
  expect_false(res$canceled)
})

test_that("a 4xx-style empty batch yields unresolved, NOT a breaker trip", {
  # The real .pelsa_fetch_one_batch returns zero entries (failed = FALSE) for a
  # 4xx (query matched nothing). Simulate that: every batch returns no entries.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      list(entries = list(), failed = FALSE)
    },
    .package = "Protigy"
  )
  # 10 accessions / batch_size 1 -> 10 empty batches. If empties counted toward
  # the breaker (limit 5) this would error; it must NOT.
  res <- pelsa_fetch_uniprot(sprintf("P%05d", 1:10), batch_size = 1L)
  expect_setequal(res$unresolved, sprintf("P%05d", 1:10))
  expect_equal(nrow(res$features), 0L)
  expect_false(res$canceled)
})

test_that("a returned-but-feature-less entry is RESOLVED, not unresolved", {
  # Regression: `resolved` was derived from feature rows, so a valid UniProt
  # entry with zero parseable features was wrongly marked unresolved (spurious
  # 'failed annotation' QC count + stale-cache retention). Entry presence, not
  # feature presence, defines resolved.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      # P00001 has features, P00002 is returned but feature-less.
      ent <- list(fake_entry("P00001"), fake_entry_no_features("P00002"))
      list(entries = ent, failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P00001", "P00002"), batch_size = 2L)
  # P00002 was returned (resolved) even though it contributed no feature rows.
  expect_false("P00002" %in% res$unresolved)
  expect_length(res$unresolved, 0L)
  # P00001 still produced its feature row.
  expect_true("P00001" %in% res$features$accession)
  # P00002 is surfaced as a distinct zero-feature category (resolved, 0 feats).
  expect_setequal(res$zero_feature, "P00002")
  expect_false("P00001" %in% res$zero_feature)
})

test_that("fetch zero_feature is empty on the empty-input fast path", {
  res <- pelsa_fetch_uniprot(character(0))
  expect_identical(res$zero_feature, character(0))
})

test_that("an input secondary accession returned under its primary is RESOLVED", {
  # Regression: UniProt returns a demerged accession's entry under the PRIMARY
  # accession (with the secondary listed in secondaryAccessions). The input
  # secondary must be marked resolved, not unresolved.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      # input was the secondary "P0SEC1"; UniProt returns it as primary "Q99999".
      list(entries = list(fake_entry_with_secondary("Q99999", "P0SEC1")),
           failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P0SEC1"), batch_size = 2L)
  expect_false("P0SEC1" %in% res$unresolved)
  expect_length(res$unresolved, 0L)
})

test_that("multiple secondary accessions on one entry all resolve", {
  # Real UniProt entries carry secondaryAccessions as a multi-element array.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      e <- fake_entry("Q99999")
      e$secondaryAccessions <- list("Q0SEC1", "Q0SEC2", "Q0SEC3")
      list(entries = list(e), failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("Q0SEC1", "Q0SEC3"), batch_size = 2L)
  expect_length(res$unresolved, 0L)
})

test_that("an input isoform accession returned under its base is RESOLVED", {
  # Regression: an isoform input "P12345-2" is returned by UniProt under its base
  # primaryAccession "P12345" and is NOT listed in secondaryAccessions, so exact
  # intersect would always mark it unresolved (inflating n_unresolved and firing a
  # spurious "re-run when UniProt is reachable" warning on every healthy refresh).
  # It must resolve via its isoform base.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      # input was the isoform "P12345-2"; UniProt returns base "P12345".
      list(entries = list(fake_entry("P12345")), failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P12345-2"), batch_size = 2L)
  expect_false("P12345-2" %in% res$unresolved)
  expect_length(res$unresolved, 0L)
})

test_that("a genuinely-absent accession is still reported unresolved", {
  # Control: an accession UniProt never returns stays unresolved.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      # only P00001 comes back; P00098 is absent.
      list(entries = list(fake_entry("P00001")), failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P00001", "P00098"), batch_size = 2L)
  expect_true("P00098" %in% res$unresolved)
  expect_false("P00001" %in% res$unresolved)
})

test_that("a genuinely-absent isoform accession is still reported unresolved", {
  # Control for the isoform fallback: if neither the isoform NOR its base returns,
  # it stays unresolved (the base-match must not resolve an absent isoform).
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      list(entries = list(fake_entry("P00001")), failed = FALSE)  # P99999 base absent
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P00001", "P99999-3"), batch_size = 2L)
  expect_true("P99999-3" %in% res$unresolved)
})

test_that("transient_unresolved separates failed-batch accs from genuinely-absent", {
  # A batch that FAILED (5xx/network) leaves its accessions transiently unresolved
  # (re-running helps). An accession in a SUCCEEDED batch that UniProt simply did
  # not return is genuinely absent (re-running will not help). Only the former
  # should drive the "re-run when reachable" refresh warning.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      if ("P00097" %in% accs) {
        list(entries = list(), failed = TRUE)            # transient failure
      } else {
        # succeeded batch: P00096 returned, P00095 genuinely absent
        list(entries = list(fake_entry("P00096")), failed = FALSE)
      }
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P00097", "P00096", "P00095"), batch_size = 1L)
  expect_setequal(res$unresolved, c("P00097", "P00095"))
  expect_setequal(res$transient_unresolved, "P00097")    # NOT P00095
})

test_that("transient_unresolved is empty on a fully successful fetch", {
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      list(entries = lapply(accs, fake_entry), failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P00001", "P00002"), batch_size = 2L)
  expect_length(res$transient_unresolved, 0L)
})

test_that("breaker trips after .PELSA_BREAKER_LIMIT consecutive failed batches", {
  # Every batch reports failed = TRUE (a 5xx/network failure) with no entries.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      list(entries = list(), failed = TRUE)
    },
    .package = "Protigy"
  )
  limit <- get(".PELSA_BREAKER_LIMIT", envir = asNamespace("Protigy"))
  expect_true(limit >= 1L)
  # Enough batches to exceed the limit.
  expect_error(
    pelsa_fetch_uniprot(sprintf("P%05d", seq_len(limit + 2L)), batch_size = 1L),
    "UniProt unavailable"
  )
})

test_that("breaker resets after a successful batch (failures must be consecutive)", {
  limit <- get(".PELSA_BREAKER_LIMIT", envir = asNamespace("Protigy"))
  calls <- new.env(); calls$n <- 0L
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      calls$n <- calls$n + 1L
      # Alternate fail/succeed so consecutive failures never exceed 1 -- a single
      # success between failures resets the breaker counter. With this pattern the
      # breaker must NOT trip no matter how many batches run.
      if (calls$n %% 2L == 0L) {
        return(list(entries = lapply(accs, fake_entry), failed = FALSE))  # resets
      }
      list(entries = list(), failed = TRUE)
    },
    .package = "Protigy"
  )
  # 3*limit batches alternating fail/success -> never `limit` failures in a row.
  n <- 3L * limit
  res <- pelsa_fetch_uniprot(sprintf("P%05d", seq_len(n)), batch_size = 1L)
  # the successful (even-numbered) batches resolve their accessions; the rest are
  # unresolved -- but crucially no breaker error was thrown.
  expect_gt(nrow(res$features), 0L)
  expect_false(res$canceled)
})

test_that("should_cancel stops at a batch boundary and reports canceled = TRUE", {
  calls <- new.env(); calls$n <- 0L
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      calls$n <- calls$n + 1L
      list(entries = lapply(accs, fake_entry), failed = FALSE)
    },
    .package = "Protigy"
  )
  # cancel AFTER the first batch has run (so n >= 1 -> stop before batch 2)
  res <- pelsa_fetch_uniprot(
    sprintf("P%05d", 1:6), batch_size = 2L,
    should_cancel = function() calls$n >= 1L
  )
  expect_true(res$canceled)
  # only the first batch's two accessions resolved
  expect_equal(calls$n, 1L)
  expect_setequal(res$features$accession, c("P00001", "P00002"))
  # the not-yet-fetched accessions are unresolved
  expect_true(all(c("P00003", "P00004", "P00005", "P00006") %in% res$unresolved))
})

# ===========================================================================
# One-batch 4xx vs 5xx discrimination (mock httr2 directly)
# ===========================================================================
# .pelsa_fetch_one_batch treats a >=500 (or network) failure as a thrown error
# (breaker fuel) but a <500 response as a healthy server returning nothing.

make_resp <- function(status, results = list()) {
  # Must carry class "httr2_response" -- .pelsa_fetch_one_batch filters the
  # iterative output to inherits(r, "httr2_response") before reading any page.
  structure(list(.status = status, .results = results),
            class = "httr2_response")
}

test_that(".pelsa_fetch_one_batch returns entries for a 200 page", {
  fetch_one <- get(".pelsa_fetch_one_batch", envir = asNamespace("Protigy"))
  testthat::local_mocked_bindings(
    req_url_path_append = function(req, ...) req,
    req_url_query = function(req, ...) req,
    req_perform_iterative = function(req, ...) list(make_resp(200L,
      list(fake_entry("P1")))),
    resp_status = function(resp) resp$.status,
    resp_body_json = function(resp, ...) list(results = resp$.results),
    .package = "httr2"
  )
  res <- fetch_one(httr2::request("http://x"), c("P1"), size = 1L)
  expect_length(res$entries, 1L)
  expect_false(res$failed)
})

test_that(".pelsa_fetch_one_batch yields zero entries for a 4xx (no throw)", {
  fetch_one <- get(".pelsa_fetch_one_batch", envir = asNamespace("Protigy"))
  # The 400 response carries a non-empty results payload. If the >=400 skip were
  # absent the batch would parse the entry and return length 1; the skip is the
  # ONLY reason the result is empty. This ensures removing the skip breaks the test.
  testthat::local_mocked_bindings(
    req_url_path_append = function(req, ...) req,
    req_url_query = function(req, ...) req,
    req_perform_iterative = function(req, ...) list(make_resp(400L,
      list(fake_entry("P1")))),
    resp_status = function(resp) resp$.status,
    resp_body_json = function(resp, ...) list(results = resp$.results),
    .package = "httr2"
  )
  res <- fetch_one(httr2::request("http://x"), c("P1"), size = 1L)
  expect_length(res$entries, 0L)
  expect_false(res$failed)
})

test_that(".pelsa_fetch_one_batch reports failed = TRUE on a 5xx terminal error (breaker fuel)", {
  fetch_one <- get(".pelsa_fetch_one_batch", envir = asNamespace("Protigy"))
  err <- structure(
    list(message = "server error", resp = make_resp(503L)),
    class = c("httr2_http_503", "error", "condition")
  )
  testthat::local_mocked_bindings(
    req_url_path_append = function(req, ...) req,
    req_url_query = function(req, ...) req,
    # on_error = "return": iterative returns the error as the last element
    req_perform_iterative = function(req, ...) list(err),
    resp_status = function(resp) resp$.status,
    resp_body_json = function(resp, ...) list(results = list()),
    .package = "httr2"
  )
  res <- fetch_one(httr2::request("http://x"), c("P1"), size = 1L)
  expect_true(res$failed)
  expect_length(res$entries, 0L)
})

test_that("5xx on a LATE page preserves the good pages already fetched (no data loss)", {
  # Regression: req_perform_iterative(on_error='return') returns the successful
  # cursor pages PLUS a trailing error condition. The old code re-threw via
  # stop(last) BEFORE collecting the good pages, discarding P1/P2 entirely. Now
  # the good pages' entries must survive and the batch is still flagged failed.
  fetch_one <- get(".pelsa_fetch_one_batch", envir = asNamespace("Protigy"))
  err <- structure(
    list(message = "server error", resp = make_resp(503L)),
    class = c("httr2_http_503", "error", "condition")
  )
  testthat::local_mocked_bindings(
    req_url_path_append = function(req, ...) req,
    req_url_query = function(req, ...) req,
    # page 1 (P1) ok, page 2 (P2) ok, page 3 fails 5xx after retries
    req_perform_iterative = function(req, ...) list(
      make_resp(200L, list(fake_entry("P1"))),
      make_resp(200L, list(fake_entry("P2"))),
      err
    ),
    resp_status = function(resp) resp$.status,
    resp_body_json = function(resp, ...) list(results = resp$.results),
    .package = "httr2"
  )
  res <- fetch_one(httr2::request("http://x"), c("P1", "P2"), size = 1L)
  expect_true(res$failed)
  expect_length(res$entries, 2L)  # P1 and P2 survived the late-page failure
})

# ---------------------------------------------------------------------------
# Source-level guard for the failed-condition block. The 4xx-vs-5xx BEHAVIOR is
# already covered by the behavioral tests above (4xx -> no throw; 5xx -> failed;
# late-page 5xx preserves good pages). This guard pins only the two structural
# invariants the corrected comment documents -- the req_error(>=500) policy and
# the unchanged behavioral guard line -- without coupling to exact prose.
# ---------------------------------------------------------------------------
test_that("the failed-condition block keeps the req_error(>=500) policy + NA/5xx guard", {
  src_path <- testthat::test_path("..", "..", "R", "tab_pelsa_uniprot_fetch.R")
  skip_if_not(file.exists(src_path), "tab_pelsa_uniprot_fetch.R source not found")
  src <- paste(readLines(src_path, warn = FALSE), collapse = "\n")
  # base_req must keep the >= 500 error policy that makes 4xx a normal response.
  expect_true(grepl("resp_status(resp) >= 500", src, fixed = TRUE))
  # The behavioral guard (network NA OR server 5xx -> batch failed) is unchanged.
  expect_true(grepl("if (is.na(status) || status >= 500L) batch_failed <- TRUE",
                    src, fixed = TRUE))
})

# ===========================================================================
# Defect #2/#4 fix: query universe is valid-format, isoform-base, deduped.
# ===========================================================================

test_that(".pelsa_is_valid_accession accepts UniProt accessions, rejects non-UniProt keys", {
  # Valid: SwissProt (P/Q/O...) + TrEMBL forms, with optional isoform suffix.
  expect_equal(
    .pelsa_is_valid_accession(c("P12345", "Q6ZWR6", "A2ASS6", "A0A0N4SVQ2", "P12345-3")),
    rep(TRUE, 5L)
  )
  # Invalid: smORF/contaminant keys + obviously malformed.
  expect_equal(
    .pelsa_is_valid_accession(c("smORF_G035940|LINC02081.2", "B99901", "", "lowercase1", NA)),
    rep(FALSE, 5L)
  )
})

test_that("pelsa_fetch_uniprot queries base+valid+deduped terms, never isoform/invalid keys", {
  captured <- list()
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      captured[[length(captured) + 1L]] <<- accs
      list(entries = lapply(accs, fake_entry), failed = FALSE)
    },
    .package = "Protigy"
  )
  # Input mixes: a base, its isoform, a duplicate, an invalid smORF, a contaminant.
  pelsa_fetch_uniprot(
    c("P00001", "P00001-2", "P00001", "smORF_G1|X", "B99901", "Q6ZWR6-3"),
    batch_size = 100L
  )
  terms <- unlist(captured, use.names = FALSE)
  # Isoform suffix stripped to base; duplicates collapsed.
  expect_true("P00001" %in% terms)
  expect_false(any(grepl("-[0-9]+$", terms)))       # no isoform-suffixed query terms
  expect_equal(sum(terms == "P00001"), 1L)          # deduped (P00001 + P00001-2 -> one)
  # Non-UniProt keys never queried.
  expect_false("smORF_G1|X" %in% terms)
  expect_false("B99901" %in% terms)
  # Q6ZWR6-3 -> Q6ZWR6 base present.
  expect_true("Q6ZWR6" %in% terms)
})

test_that("pelsa_fetch_uniprot never sends more than batch_size (<=100) terms per batch", {
  seen_sizes <- integer(0)
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      seen_sizes[[length(seen_sizes) + 1L]] <<- length(accs)
      list(entries = lapply(accs, fake_entry), failed = FALSE)
    },
    .package = "Protigy"
  )
  # 250 distinct valid base accessions -> with batch_size 100 -> batches of <=100.
  accs <- sprintf("P%05d", 1:250)
  pelsa_fetch_uniprot(accs, batch_size = 100L)
  expect_true(all(seen_sizes <= 100L))
  expect_equal(sum(seen_sizes), 250L)
})

test_that("an isoform input resolves via its base entry (not falsely unresolved)", {
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      # UniProt returns the entry under the BASE primaryAccession only.
      list(entries = lapply(accs, fake_entry), failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P00001-2"), batch_size = 100L)
  expect_false("P00001-2" %in% res$unresolved)
})

test_that("an invalid-format input is unresolved but does not error or trip the breaker", {
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      list(entries = lapply(accs, fake_entry), failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P00001", "smORF_G1|X"), batch_size = 100L)
  expect_true("P00001" %in% res$features$accession)
  expect_true("smORF_G1|X" %in% res$unresolved)
  expect_false(res$canceled)
})

test_that(".PELSA_BATCH_SIZE stays within UniProt's 100-OR /search cap", {
  bs <- get(".PELSA_BATCH_SIZE", envir = asNamespace("Protigy"))
  expect_lte(bs, 100L)
})

# ===========================================================================
# --- from refresh ---
# ===========================================================================
# Tests for the PELSA per-species UniProt-annotation refresh (Task 5C).
#
# These cover the PURE helpers (accession universe, write/round-trip) and the
# orchestration helper with an INJECTED fake fetcher. NO LIVE NETWORK is ever
# touched: pelsa_fetch_uniprot is never called here  -  the orchestration test
# substitutes a stub returning a canned 8-col feature frame.

# Canned 8-column feature frame matching the schema (for write + orchestration).
.fake_feature_df <- function() {
  data.frame(
    accession     = c("P00001", "P00001", "P00002"),
    feature_type  = c("active site", "domain", "transmembrane"),
    start         = c(10L, 50L, 100L),
    end           = c(12L, 120L, 130L),
    description   = c("nucleophile", "kinase domain", "Helical"),
    feature_class = c("active_or_binding_site", "catalytic_domain",
                      "transmembrane_or_signal"),
    class_score   = c(5L, 3L, 0L),
    coord_quality = c("exact", "exact", "fuzzy"),
    stringsAsFactors = FALSE
  )
}

# ---- pelsa_full_universe (FASTA proteome only) -------------------------------

test_that("full_universe = FASTA accessions only; ignores datasets + cache", {
  gcts <- list(omeA = data.frame(PG.ProteinAccessions = "P10000;P20000",
                                 stringsAsFactors = FALSE))
  existing <- data.frame(accession = c("P30000", "P40000"),
                         stringsAsFactors = FALSE)
  fasta_map <- list(Q1 = "MKV", Q2 = "AAA", Q3 = "CCC")
  out <- pelsa_full_universe(gcts, existing, fasta_map = fasta_map)
  expect_identical(out, sort(c("Q1", "Q2", "Q3")))  # NO dataset/cache accessions
})

test_that("full_universe is empty when no FASTA is given", {
  gcts <- list(omeA = data.frame(PG.ProteinAccessions = "P1",
                                 stringsAsFactors = FALSE))
  expect_identical(pelsa_full_universe(gcts, NULL, fasta_map = NULL),
                   character(0))
})

# ---- pelsa_incremental_universe ((dataset U fasta) - cache) ------------------

test_that("incremental_universe = (dataset U fasta) minus cache accessions", {
  gcts <- list(omeA = data.frame(PG.ProteinAccessions = c("P1;P2", "P3"),
                                 stringsAsFactors = FALSE))
  fasta_map <- list(P3 = "AAA", P4 = "CCC")          # P3 overlaps a dataset acc
  existing <- data.frame(accession = c("P2", "P4"),  # already cached
                         stringsAsFactors = FALSE)
  out <- pelsa_incremental_universe(gcts, existing, fasta_map = fasta_map)
  # union {P1,P2,P3,P4} minus cache {P2,P4} = {P1,P3}
  expect_identical(out, sort(c("P1", "P3")))
})

test_that("incremental_universe explodes/dedups/trims dataset tokens", {
  gcts <- list(omeA = data.frame(
    PG.ProteinAccessions = c("P1; P2 ;P3", "P2;P1", NA_character_, ""),
    stringsAsFactors = FALSE))
  out <- pelsa_incremental_universe(gcts, NULL, fasta_map = NULL)
  expect_identical(out, sort(c("P1", "P2", "P3")))
})

test_that("incremental_universe result is disjoint from the cache", {
  gcts <- list(omeA = data.frame(PG.ProteinAccessions = "P1;P2;P3",
                                 stringsAsFactors = FALSE))
  existing <- data.frame(accession = c("P1", "P2", "P3"),
                         stringsAsFactors = FALSE)
  # All dataset accessions already cached, no FASTA -> empty.
  expect_identical(pelsa_incremental_universe(gcts, existing, fasta_map = NULL),
                   character(0))
})

test_that("incremental_universe ignores datasets without PG.ProteinAccessions", {
  gcts <- list(
    nonPelsa = data.frame(foo = 1:3, stringsAsFactors = FALSE),
    pelsa    = data.frame(PG.ProteinAccessions = "P5", stringsAsFactors = FALSE))
  expect_identical(pelsa_incremental_universe(gcts, NULL, fasta_map = NULL), "P5")
})

# ---- pelsa_wipe_species_cache (full-mode clean slate) ------------------------

test_that("wipe deletes uniprot_features + uniprot_membrane, spares fasta/", {
  species_dir <- withr::local_tempdir()
  dir.create(file.path(species_dir, "fasta"))
  writeLines(">x\nMKV", file.path(species_dir, "fasta", "p.fasta"))
  dir.create(file.path(species_dir, "uniprot_features"))
  writeLines("acc", file.path(species_dir, "uniprot_features", "uniprot_features.tsv"))
  dir.create(file.path(species_dir, "uniprot_membrane"))
  writeLines("m", file.path(species_dir, "uniprot_membrane", "mem.tsv"))

  pelsa_wipe_species_cache(species_dir)

  expect_true(dir.exists(file.path(species_dir, "fasta")))
  expect_true(file.exists(file.path(species_dir, "fasta", "p.fasta")))
  expect_false(dir.exists(file.path(species_dir, "uniprot_features")))
  expect_false(dir.exists(file.path(species_dir, "uniprot_membrane")))
})

test_that("wipe also removes stray top-level files (except inside fasta/)", {
  species_dir <- withr::local_tempdir()
  dir.create(file.path(species_dir, "fasta"))
  writeLines("keep", file.path(species_dir, "fasta", "keep.fasta"))
  writeLines("junk", file.path(species_dir, "stray.parquet"))

  deleted <- pelsa_wipe_species_cache(species_dir)

  expect_false(file.exists(file.path(species_dir, "stray.parquet")))
  expect_true(file.exists(file.path(species_dir, "fasta", "keep.fasta")))
  expect_true("stray.parquet" %in% deleted)
  expect_false("fasta" %in% deleted)
})

test_that("wipe is a no-op on a missing species dir", {
  missing <- file.path(withr::local_tempdir(), "does_not_exist")
  expect_silent(out <- pelsa_wipe_species_cache(missing))
  expect_identical(out, character(0))
})

# ---- pelsa_zero_feature_rows (sentinel rows for 0-feature accessions) --------

test_that("zero_feature_rows builds one schema-shaped sentinel per accession", {
  out <- pelsa_zero_feature_rows(c("P00002", "P00003", "P00002"))  # dup dropped
  expect_setequal(out$accession, c("P00002", "P00003"))
  expect_identical(nrow(out), 2L)
  expect_true(all(out$feature_type == ""))
  expect_true(all(is.na(out$start)))
  expect_true(all(is.na(out$end)))
  expect_true(all(out$feature_class == "none"))
  expect_true(all(out$class_score == 0L))
  expect_identical(colnames(out),
                   c("accession", "feature_type", "start", "end",
                     "description", "feature_class", "class_score",
                     "coord_quality"))
})

test_that("zero_feature_rows returns a 0-row schema frame for empty input", {
  out <- pelsa_zero_feature_rows(character(0))
  expect_identical(nrow(out), 0L)
  expect_identical(colnames(out), colnames(pelsa_empty_feature_frame()))
})

# ---- pelsa_write_feature_cache (round-trip) ----------------------------------

test_that("write_feature_cache round-trips via pelsa_read_feature_cache", {
  species_dir <- withr::local_tempdir()
  feat <- .fake_feature_df()

  path <- pelsa_write_feature_cache(feat, species_dir)
  expect_true(file.exists(path))
  expect_identical(basename(path), "uniprot_features.tsv")
  expect_true(file.exists(file.path(species_dir, "uniprot_features",
                                    "schema.json")))

  # Read back the four required + the present optional columns.
  back <- pelsa_read_feature_cache(species_dir)
  expect_true(all(c("accession", "start", "end", "feature_class",
                    "feature_type", "class_score", "description",
                    "coord_quality") %in% colnames(back)))
  # Round-trip identity on the schema columns (reader reorders required-first;
  # compare value-by-value).
  expect_identical(back$accession, feat$accession)
  expect_identical(back$start, feat$start)
  expect_identical(back$end, feat$end)
  expect_identical(back$feature_class, feat$feature_class)
  expect_identical(back$class_score, feat$class_score)
  expect_identical(back$coord_quality, feat$coord_quality)
})

test_that("write_feature_cache writes columns in the canonical schema order", {
  species_dir <- withr::local_tempdir()
  # Feed columns in a SHUFFLED order; the written TSV must be schema order.
  feat <- .fake_feature_df()[, c("coord_quality", "accession", "end",
                                 "class_score", "feature_type", "start",
                                 "description", "feature_class")]
  path <- pelsa_write_feature_cache(feat, species_dir)
  header <- strsplit(readLines(path, n = 1L), "\t", fixed = TRUE)[[1]]
  expect_identical(
    header,
    c("accession", "feature_type", "start", "end", "description",
      "feature_class", "class_score", "coord_quality")
  )
})

test_that("write_feature_cache schema.json matches the committed contract", {
  species_dir <- withr::local_tempdir()
  pelsa_write_feature_cache(.fake_feature_df(), species_dir)
  schema <- jsonlite::read_json(
    file.path(species_dir, "uniprot_features", "schema.json")
  )
  expect_identical(unlist(schema$columns),
                   c("accession", "feature_type", "start", "end",
                     "description", "feature_class", "class_score",
                     "coord_quality"))
  expect_identical(schema$classifier_version, "fixed_v1")
  expect_equal(schema$feature_class_scores$active_or_binding_site, 5L)
})

test_that("write_feature_cache fails fast on missing schema columns", {
  species_dir <- withr::local_tempdir()
  bad <- data.frame(accession = "P1", start = 1L, stringsAsFactors = FALSE)
  expect_error(pelsa_write_feature_cache(bad, species_dir),
               "missing schema column")
})

test_that("write_feature_cache surfaces a clear error on a non-writable dir", {
  skip_on_os("windows")  # POSIX permission semantics
  base <- withr::local_tempdir()
  ro <- file.path(base, "readonly")
  dir.create(ro)
  Sys.chmod(ro, mode = "0500")  # r-x, not writable
  on.exit(Sys.chmod(ro, mode = "0700"), add = TRUE)
  # Skip if running as root (chmod is ignored): the test can't create the
  # non-writable condition.
  skip_if(file.access(ro, mode = 2L) == 0L,
          "tempdir not enforce-able as read-only (root?)")

  expect_error(
    pelsa_write_feature_cache(.fake_feature_df(), ro),
    "read-only|not writable|cannot create"
  )
})

# ---- pelsa_merge_feature_cache (data-loss guard) -----------------------------

# A 3-accession existing cache (P1, P2, P3 each one row).
.existing_cache_df <- function() {
  data.frame(
    accession     = c("P1", "P2", "P3"),
    feature_type  = c("domain", "domain", "active site"),
    start         = c(1L, 1L, 1L),
    end           = c(10L, 10L, 10L),
    description   = c("old1", "old2", "old3"),
    feature_class = c("folded_domain", "folded_domain",
                      "active_or_binding_site"),
    class_score   = c(2L, 2L, 5L),
    coord_quality = c("exact", "exact", "exact"),
    stringsAsFactors = FALSE
  )
}

test_that("merge retains old rows for unresolved accessions, fresh rows win", {
  existing <- .existing_cache_df()
  # Fresh resolved P1, P2 (new rows); P3 ended up unresolved.
  fresh <- data.frame(
    accession     = c("P1", "P2"),
    feature_type  = c("domain", "domain"),
    start         = c(5L, 5L),
    end           = c(20L, 20L),
    description   = c("new1", "new2"),
    feature_class = c("catalytic_domain", "catalytic_domain"),
    class_score   = c(3L, 3L),
    coord_quality = c("exact", "exact"),
    stringsAsFactors = FALSE
  )
  merged <- pelsa_merge_feature_cache(existing, fresh, unresolved = "P3")

  # P3's OLD row survives (data-loss guard).
  expect_true("P3" %in% merged$accession)
  p3 <- merged[merged$accession == "P3", ]
  expect_identical(p3$description, "old3")
  # P1/P2 are the FRESH rows (not the old ones).
  p1 <- merged[merged$accession == "P1", ]
  expect_identical(p1$description, "new1")
  expect_identical(p1$class_score, 3L)
  # No duplicate P1/P2 (old rows superseded).
  expect_identical(sum(merged$accession == "P1"), 1L)
})

test_that("merge: a RESOLVED accession with zero fresh rows correctly drops", {
  existing <- .existing_cache_df()
  # P1 resolved with new rows; P2 resolved but UniProt removed its features
  # (no fresh rows, NOT in unresolved) -> P2 must NOT be retained.
  fresh <- data.frame(
    accession     = "P1", feature_type = "domain", start = 5L, end = 9L,
    description = "new1", feature_class = "folded_domain", class_score = 2L,
    coord_quality = "exact", stringsAsFactors = FALSE
  )
  merged <- pelsa_merge_feature_cache(existing, fresh, unresolved = "P3")
  expect_false("P2" %in% merged$accession)  # resolved-but-empty -> dropped
  expect_true("P3" %in% merged$accession)   # unresolved -> retained
})

test_that("merge with no existing cache returns the fresh frame unchanged", {
  fresh <- .fake_feature_df()
  expect_identical(pelsa_merge_feature_cache(NULL, fresh, "P99"), fresh)
  expect_identical(
    pelsa_merge_feature_cache(pelsa_empty_feature_frame(), fresh, "P99"),
    fresh
  )
})

test_that("merge with no unresolved fully supersedes the old cache", {
  existing <- .existing_cache_df()
  fresh <- .fake_feature_df()
  expect_identical(
    pelsa_merge_feature_cache(existing, fresh, unresolved = character(0)),
    fresh
  )
})

# ---- atomic write: a failed write leaves the PRIOR cache intact --------------

test_that("write failure leaves a pre-seeded cache uncorrupted (atomic)", {
  species_dir <- withr::local_tempdir()
  # Pre-seed a valid cache.
  good <- .existing_cache_df()
  pelsa_write_feature_cache(good, species_dir)
  tsv <- file.path(species_dir, "uniprot_features", "uniprot_features.tsv")
  before <- readLines(tsv)

  # A second write with a malformed (missing-schema) frame must FAIL without
  # touching the existing good .tsv.
  bad <- data.frame(accession = "P1", start = 1L, stringsAsFactors = FALSE)
  expect_error(pelsa_write_feature_cache(bad, species_dir),
               "missing schema column")

  # Prior cache still readable + byte-identical.
  expect_identical(readLines(tsv), before)
  back <- pelsa_read_feature_cache(species_dir)
  expect_identical(sort(back$accession), c("P1", "P2", "P3"))
})

test_that("atomic write leaves no leftover temp files on success", {
  species_dir <- withr::local_tempdir()
  pelsa_write_feature_cache(.existing_cache_df(), species_dir)
  feat_dir <- file.path(species_dir, "uniprot_features")
  files <- list.files(feat_dir)
  # Only the two canonical files; no uniprot_features_*.tsv / schema_*.json temps.
  expect_setequal(files, c("uniprot_features.tsv", "schema.json"))
})

# ---- pelsa_refresh_species_cache (INJECTED fake fetcher, NO network) ---------

test_that("refresh_species_cache fetches via injected stub + writes + counts", {
  species_dir <- withr::local_tempdir()
  universe <- c("P00001", "P00002", "P00003")

  called_with <- NULL
  fake_fetch <- function(accessions) {
    called_with <<- accessions
    list(features = .fake_feature_df(), unresolved = c("P00003"))
  }

  res <- pelsa_refresh_species_cache(
    species = "9606", universe = universe, species_dir = species_dir,
    fetch_fn = fake_fetch
  )

  # The stub (NOT the network) was called with the universe.
  expect_identical(called_with, universe)
  expect_identical(res$n_features, 3L)
  expect_identical(res$n_unresolved, 1L)
  expect_identical(res$n_accessions, 3L)
  expect_identical(res$n_retained_from_cache, 0L)  # no existing cache

  # Cache was actually written + reads back.
  expect_true(file.exists(res$path))
  back <- pelsa_read_feature_cache(species_dir)
  expect_identical(nrow(back), 3L)
})

test_that("refresh_species_cache MERGES over an existing cache (no data loss)", {
  species_dir <- withr::local_tempdir()
  existing <- .existing_cache_df()             # P1, P2, P3
  pelsa_write_feature_cache(existing, species_dir)

  # Stub resolves P1, P2; P3 demoted to unresolved (flaky network).
  fresh <- data.frame(
    accession     = c("P1", "P2"),
    feature_type  = c("domain", "domain"),
    start         = c(5L, 5L), end = c(20L, 20L),
    description   = c("new1", "new2"),
    feature_class = c("catalytic_domain", "catalytic_domain"),
    class_score   = c(3L, 3L), coord_quality = c("exact", "exact"),
    stringsAsFactors = FALSE
  )
  fake_fetch <- function(accessions) list(features = fresh, unresolved = "P3")

  res <- pelsa_refresh_species_cache(
    species = "9606", universe = c("P1", "P2", "P3"),
    species_dir = species_dir, fetch_fn = fake_fetch, existing = existing
  )
  expect_identical(res$n_retained_from_cache, 1L)

  # P3's prior annotation SURVIVES on disk; P1/P2 are the fresh rows.
  back <- pelsa_read_feature_cache(species_dir)
  expect_true("P3" %in% back$accession)
  expect_identical(back[back$accession == "P3", "description"], "old3")
  expect_identical(back[back$accession == "P1", "description"], "new1")
})

test_that("refresh_species_cache drives a Progress-like object's $set", {
  species_dir <- withr::local_tempdir()
  calls <- list()
  fake_progress <- list(set = function(value, message, detail = NULL) {
    calls[[length(calls) + 1L]] <<- list(value = value, message = message)
  })
  fake_fetch <- function(accessions) {
    list(features = .fake_feature_df(), unresolved = character(0))
  }

  pelsa_refresh_species_cache(
    species = "10090", universe = "P00001", species_dir = species_dir,
    fetch_fn = fake_fetch, progress = fake_progress
  )
  expect_gte(length(calls), 2L)               # at least fetch + write stages
  expect_equal(calls[[length(calls)]]$value, 1.0)  # ends at 100%
})

test_that("refresh_species_cache errors on an empty universe", {
  species_dir <- withr::local_tempdir()
  expect_error(
    pelsa_refresh_species_cache("9606", character(0), species_dir,
                                fetch_fn = function(a) list(features = NULL)),
    "empty accession universe"
  )
})

test_that("refresh_species_cache rejects a malformed fetch_fn return", {
  species_dir <- withr::local_tempdir()
  expect_error(
    pelsa_refresh_species_cache("9606", "P1", species_dir,
                                fetch_fn = function(a) list(nope = 1)),
    "must return a list with a `features`"
  )
})

# ---- pelsa_refresh_species_cache: mode = full (wipe + supersede) -------------

test_that("full mode WIPES the species dir before fetch + supersedes cache", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "10090")
  dir.create(species_dir)
  # Pre-seed a stale cache + a membrane file + a fasta to spare.
  pelsa_write_feature_cache(.existing_cache_df(), species_dir)  # P1,P2,P3
  dir.create(file.path(species_dir, "uniprot_membrane"))
  writeLines("stale", file.path(species_dir, "uniprot_membrane", "m.tsv"))
  dir.create(file.path(species_dir, "fasta"))
  writeLines(">x\nMKV", file.path(species_dir, "fasta", "p.fasta"))

  # Fetch returns a DIFFERENT set (P00001/P00002) -> the old P1/P2/P3 must be gone.
  fake_fetch <- function(accessions, ...) {
    list(features = .fake_feature_df(), unresolved = character(0))
  }
  res <- pelsa_refresh_species_cache(
    species = "10090", universe = c("P00001", "P00002"),
    species_dir = species_dir, fetch_fn = fake_fetch,
    existing = .existing_cache_df(),  # passed, but full mode must IGNORE it
    mode = "full"
  )

  expect_identical(res$mode, "full")
  expect_identical(res$n_retained_from_cache, 0L)  # nothing retained in full
  back <- pelsa_read_feature_cache(species_dir)
  expect_setequal(unique(back$accession), c("P00001", "P00002"))  # P1/2/3 gone
  expect_false(dir.exists(file.path(species_dir, "uniprot_membrane")))  # wiped
  expect_true(file.exists(file.path(species_dir, "fasta", "p.fasta")))   # spared
})

test_that("full mode does NOT wipe when canceled before fetch", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "10090")
  dir.create(species_dir)
  pelsa_write_feature_cache(.existing_cache_df(), species_dir)

  called <- FALSE
  fake_fetch <- function(accessions, ...) { called <<- TRUE
    list(features = .fake_feature_df(), unresolved = character(0)) }

  res <- pelsa_refresh_species_cache(
    species = "10090", universe = c("P00001"), species_dir = species_dir,
    fetch_fn = fake_fetch, existing = .existing_cache_df(),
    mode = "full", should_cancel = function() TRUE
  )
  expect_true(isTRUE(res$canceled))
  expect_false(called)
  # Prior cache STILL intact (wipe never ran).
  back <- pelsa_read_feature_cache(species_dir)
  expect_setequal(unique(back$accession), c("P1", "P2", "P3"))
})

# ---- pelsa_refresh_species_cache: mode = incremental (append atop) -----------

test_that("incremental mode merges fresh ATOP existing (no wipe)", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)
  pelsa_write_feature_cache(.existing_cache_df(), species_dir)  # P1,P2,P3

  # Incremental fetched only the cache-miss P00001/P00002 (disjoint from cache).
  fresh <- data.frame(
    accession = c("P00001", "P00002"), feature_type = c("domain", "domain"),
    start = c(1L, 1L), end = c(5L, 5L), description = c("n1", "n2"),
    feature_class = c("folded_domain", "folded_domain"), class_score = c(2L, 2L),
    coord_quality = c("exact", "exact"), stringsAsFactors = FALSE)
  fake_fetch <- function(accessions, ...) list(features = fresh,
                                               unresolved = character(0))
  res <- pelsa_refresh_species_cache(
    species = "9606", universe = c("P00001", "P00002"),
    species_dir = species_dir, fetch_fn = fake_fetch,
    existing = .existing_cache_df(), mode = "incremental"
  )
  expect_identical(res$mode, "incremental")
  back <- pelsa_read_feature_cache(species_dir)
  # Old P1/P2/P3 KEPT + new P00001/P00002 added (append atop).
  expect_setequal(unique(back$accession),
                  c("P1", "P2", "P3", "P00001", "P00002"))
})

# ---- sentinel persistence in the cache (zero-feature accessions) -------------

test_that("refresh writes sentinel rows for zero-feature accessions", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)

  # Fetch: P00001 has a feature; P00002 resolved with zero features.
  fake_fetch <- function(accessions, ...) {
    list(features = data.frame(
           accession = "P00001", feature_type = "active site", start = 10L,
           end = 12L, description = "x", feature_class = "active_or_binding_site",
           class_score = 5L, coord_quality = "exact", stringsAsFactors = FALSE),
         unresolved = character(0), zero_feature = "P00002")
  }
  res <- pelsa_refresh_species_cache(
    species = "9606", universe = c("P00001", "P00002"),
    species_dir = species_dir, fetch_fn = fake_fetch, mode = "incremental")

  expect_identical(res$n_zero_feature, 1L)
  # n_with_features is a DISTINCT-accession count from the real feature frame
  # (excludes the sentinel) -- mutually exclusive with n_zero_feature.
  expect_identical(res$n_with_features, 1L)  # only P00001 has a real feature
  back <- pelsa_read_feature_cache(species_dir)
  expect_setequal(unique(back$accession), c("P00001", "P00002"))
  sentinel <- back[back$accession == "P00002", ]
  expect_true(is.na(sentinel$start))
  expect_identical(sentinel$feature_class, "none")
})

test_that("zero-feature accession is NOT re-fetched on the next incremental run", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)
  dir.create(file.path(species_dir, "fasta"))
  writeLines(c(">sp|P00001|A t", "MKV", ">sp|P00002|B t", "AAA"),
             file.path(species_dir, "fasta", "p.fasta"))

  # First incremental: P00001 feature, P00002 zero-feature.
  fetch1 <- function(accessions, ...) list(
    features = data.frame(accession = "P00001", feature_type = "domain",
      start = 1L, end = 5L, description = "d", feature_class = "folded_domain",
      class_score = 2L, coord_quality = "exact", stringsAsFactors = FALSE),
    unresolved = character(0), zero_feature = "P00002")
  pelsa_run_species_refresh("9606", db, uploaded_gcts = NULL,
                            fetch_fn = fetch1, mode = "incremental")

  # Second incremental: both cached (P00001 feature, P00002 sentinel) -> the
  # universe is empty, so the orchestrator stops before any fetch.
  seen <- NULL
  fetch2 <- function(accessions, ...) { seen <<- accessions
    list(features = pelsa_empty_feature_frame(), unresolved = character(0),
         zero_feature = character(0)) }
  results <- pelsa_run_species_refresh("9606", db, uploaded_gcts = NULL,
                                       fetch_fn = fetch2, mode = "incremental")
  expect_null(seen)                                   # fetch never called
  expect_match(results[[1]]$error, "empty accession universe")
})

# ---- pelsa_run_species_refresh (multi-species, injected stub, NO network) ----

test_that("run_species_refresh refreshes multiple species + captures errors", {
  db <- withr::local_tempdir()
  dir.create(file.path(db, "9606"))
  dir.create(file.path(db, "10090"))

  # Stub: human succeeds; mouse "fetches" features whose schema is broken so the
  # write fails -> per-species error captured, the other species still ok.
  fake_fetch <- function(accessions) {
    list(features = .fake_feature_df(), unresolved = character(0))
  }

  # Provide an uploaded GCT so the universe is non-empty for both.
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P00001",
                              stringsAsFactors = FALSE))

  progressed <- c()
  results <- pelsa_run_species_refresh(
    species = c("9606", "10090"), database_dir = db, uploaded_gcts = gcts,
    fetch_fn = fake_fetch,
    set_progress = function(value, detail) progressed <<- c(progressed, value)
  )

  expect_length(results, 2L)
  expect_identical(results[[1]]$species, "9606")
  expect_null(results[[1]]$error)
  expect_identical(results[[1]]$n_features, 3L)
  expect_true(file.exists(file.path(db, "9606", "uniprot_features",
                                    "uniprot_features.tsv")))
  # Progress advanced and reached the end.
  expect_gt(length(progressed), 0L)
  expect_equal(max(progressed), 1.0)
})

test_that("run_species_refresh captures a per-species error without aborting", {
  db <- withr::local_tempdir()
  dir.create(file.path(db, "9606"))

  # Stub returns a malformed (missing-schema) feature frame -> write fails.
  bad_fetch <- function(accessions) {
    list(features = data.frame(accession = "P1", stringsAsFactors = FALSE),
         unresolved = character(0))
  }
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P1",
                              stringsAsFactors = FALSE))

  results <- pelsa_run_species_refresh(
    species = "9606", database_dir = db, uploaded_gcts = gcts,
    fetch_fn = bad_fetch
  )
  expect_length(results, 1L)
  expect_false(is.null(results[[1]]$error))
  expect_match(results[[1]]$error, "missing schema column")
})

# ---- pelsa_run_species_refresh: mode routing --------------------------------

test_that("run_species_refresh full mode fetches the FASTA universe + wipes", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "10090")
  dir.create(species_dir)
  dir.create(file.path(species_dir, "fasta"))
  # FASTA with two UniProt-style accessions (pipe header parsed in uniprot mode).
  writeLines(c(">sp|P00001|A_X test", "MKV",
               ">sp|P00002|B_X test", "AAA"),
             file.path(species_dir, "fasta", "p.fasta"))
  # A pre-existing dataset accession that full mode must IGNORE.
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P99999",
                              stringsAsFactors = FALSE))

  seen <- NULL
  fake_fetch <- function(accessions, ...) { seen <<- accessions
    list(features = .fake_feature_df(), unresolved = character(0)) }

  results <- pelsa_run_species_refresh(
    species = "10090", database_dir = db, uploaded_gcts = gcts,
    fetch_fn = fake_fetch, mode = "full")

  expect_identical(results[[1]]$mode, "full")
  # Full universe = FASTA accessions only; the dataset accession is NOT fetched.
  expect_setequal(seen, c("P00001", "P00002"))
  expect_false("P99999" %in% seen)
})

test_that("run_species_refresh incremental mode fetches (dataset U fasta) - cache", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)
  dir.create(file.path(species_dir, "fasta"))
  writeLines(c(">sp|P00001|A_X t", "MKV"),
             file.path(species_dir, "fasta", "p.fasta"))
  # Seed a cache that already covers P00001 -> incremental must skip it.
  pelsa_write_feature_cache(
    data.frame(accession = "P00001", feature_type = "domain", start = 1L,
               end = 5L, description = "d", feature_class = "folded_domain",
               class_score = 2L, coord_quality = "exact",
               stringsAsFactors = FALSE),
    species_dir)
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P77777",
                              stringsAsFactors = FALSE))

  seen <- NULL
  fake_fetch <- function(accessions, ...) { seen <<- accessions
    list(features = data.frame(
      accession = "P77777", feature_type = "domain", start = 1L, end = 5L,
      description = "d", feature_class = "folded_domain", class_score = 2L,
      coord_quality = "exact", stringsAsFactors = FALSE),
      unresolved = character(0)) }

  results <- pelsa_run_species_refresh(
    species = "9606", database_dir = db, uploaded_gcts = gcts,
    fetch_fn = fake_fetch, mode = "incremental")

  expect_identical(results[[1]]$mode, "incremental")
  # union {P00001 (fasta), P77777 (dataset)} minus cache {P00001} = {P77777}.
  expect_setequal(seen, "P77777")
})

# ---- pelsa_refresh_notifications (pure formatter) ----------------------------

test_that("notifications: error + lossy-warning + success summary", {
  results <- list(
    list(species = "9606", n_features = 100L, n_unresolved = 2L,
         n_retained_from_cache = 2L, had_existing = TRUE, error = NULL),
    list(species = "10090", error = "UniProt unavailable"),
    list(species = "rat", n_features = 50L, n_unresolved = 0L,
         n_retained_from_cache = 0L, had_existing = FALSE, error = NULL)
  )
  notes <- pelsa_refresh_notifications(results)
  types <- vapply(notes, function(n) n$type, character(1))

  expect_true("error" %in% types)    # mouse failed
  expect_true("warning" %in% types)  # human had cache + unresolved -> lossy warn
  expect_true("message" %in% types)  # rolled-up success summary
  # The warning mentions retained rows.
  warn <- notes[[which(types == "warning")]]
  expect_match(warn$message, "retained")
  # The success summary covers both successful species.
  msg <- notes[[which(types == "message")]]
  expect_match(msg$message, "9606")
  expect_match(msg$message, "rat")
})

test_that("notifications: a fresh-cache species with unresolved does NOT warn", {
  # No prior cache -> unresolved is expected, not lossy; no warning.
  results <- list(
    list(species = "9606", n_features = 10L, n_unresolved = 3L,
         n_retained_from_cache = 0L, had_existing = FALSE, error = NULL)
  )
  types <- vapply(pelsa_refresh_notifications(results),
                  function(n) n$type, character(1))
  expect_false("warning" %in% types)
})

test_that("notifications: genuinely-absent unresolved (no transient) does NOT prompt re-run", {
  # Cache existed + unresolved > 0, but ALL unresolved are genuinely absent from
  # UniProt (n_transient_unresolved == 0). Re-running cannot help, so we must NOT
  # fire the amber "re-run when reachable" warning. A neutral info note is fine.
  results <- list(
    list(species = "9606", n_features = 100L, n_unresolved = 2L,
         n_transient_unresolved = 0L, n_retained_from_cache = 2L,
         had_existing = TRUE, error = NULL)
  )
  notes <- pelsa_refresh_notifications(results)
  types <- vapply(notes, function(n) n$type, character(1))
  expect_false("warning" %in% types)
  # No note tells the user re-running will help.
  msgs <- vapply(notes, function(n) n$message, character(1))
  expect_false(any(grepl("[Rr]e-run when", msgs)))
})

test_that("notifications: transient unresolved DOES prompt a re-run warning", {
  # Cache existed + some unresolved came from a FAILED batch (transient): the
  # amber warning with the re-run instruction is correct here.
  results <- list(
    list(species = "9606", n_features = 100L, n_unresolved = 3L,
         n_transient_unresolved = 3L, n_retained_from_cache = 3L,
         had_existing = TRUE, error = NULL)
  )
  notes <- pelsa_refresh_notifications(results)
  types <- vapply(notes, function(n) n$type, character(1))
  expect_true("warning" %in% types)
  warn <- notes[[which(types == "warning")]]
  expect_match(warn$message, "[Rr]e-run when")
})

# ---- UI presence: refresh controls in the Setup tab --------------------------

test_that("Setup UI exposes both refresh-mode buttons + pure helpers exist", {
  ns <- shiny::NS("PELSASection1Tab")
  html <- as.character(
    pelsa_setup_box_ui(species = c("Human" = "9606"), compounds = character(0),
                       ns = ns, refresh_species = c("Human" = "9606")))
  expect_match(html, ns("pelsa_refresh_btn"), fixed = TRUE)
  expect_match(html, "Full library refresh", fixed = TRUE)
  expect_match(html, ns("pelsa_incremental_btn"), fixed = TRUE)
  expect_match(html, "Incremental refresh", fixed = TRUE)

  expect_true(exists("pelsa_full_universe"))
  expect_true(exists("pelsa_incremental_universe"))
  expect_true(exists("pelsa_wipe_species_cache"))
  expect_true(exists("pelsa_write_feature_cache"))
  expect_true(exists("pelsa_refresh_species_cache"))
})

# ---- confirm-gate: universe size + ETA text (pure) ---------------------------

test_that("universe_size full mode counts the FASTA proteome (per species)", {
  db <- withr::local_tempdir()
  sd <- file.path(db, "9606"); dir.create(sd)
  dir.create(file.path(sd, "fasta"))
  writeLines(c(">sp|P00001|A t", "MKV", ">sp|P00002|B t", "AAA",
               ">sp|P00003|C t", "CCC"),
             file.path(sd, "fasta", "p.fasta"))
  # Dataset accessions present but full mode ignores them.
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P99999",
                              stringsAsFactors = FALSE))
  sz <- pelsa_refresh_universe_size("9606", db, gcts, mode = "full")
  expect_equal(unname(sz$per_species[["9606"]]), 3L)  # 3 FASTA accessions
  expect_equal(sz$total, 3L)
})

test_that("universe_size incremental mode counts (dataset U fasta) - cache", {
  db <- withr::local_tempdir()
  sd <- file.path(db, "9606"); dir.create(sd)
  dir.create(file.path(sd, "fasta"))
  writeLines(c(">sp|P00001|A t", "MKV"), file.path(sd, "fasta", "p.fasta"))
  pelsa_write_feature_cache(
    data.frame(accession = "P00001", feature_type = "domain", start = 1L,
               end = 5L, description = "d", feature_class = "folded_domain",
               class_score = 2L, coord_quality = "exact",
               stringsAsFactors = FALSE), sd)
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P77777",
                              stringsAsFactors = FALSE))
  sz <- pelsa_refresh_universe_size("9606", db, gcts, mode = "incremental")
  # union {P00001, P77777} minus cache {P00001} = 1.
  expect_equal(unname(sz$per_species[["9606"]]), 1L)
  expect_equal(sz$total, 1L)
})

test_that("eta_text formats count + a coarse ETA (sec vs min)", {
  expect_match(pelsa_refresh_eta_text(200L), "^200 accessions \\(~\\d+ sec\\)$")
  expect_match(pelsa_refresh_eta_text(1L), "^1 accession \\(~\\d+ sec\\)$")
  big <- pelsa_refresh_eta_text(69845L)
  expect_match(big, "^69,845 accessions \\(~\\d+ min\\)$")  # thousands sep + min
})

# ---- cooperative cancel propagation (no network; stub fetch) -----------------

test_that("species_cache honors a pre-fetch cancel: no write, cache intact", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)
  existing <- .fake_feature_df()

  # should_cancel TRUE before fetch -> fetch_fn must NOT be called, no write.
  called <- FALSE
  fake_fetch <- function(accessions, ...) { called <<- TRUE
    list(features = .fake_feature_df(), unresolved = character(0)) }

  res <- pelsa_refresh_species_cache(
    species = "9606", universe = c("P1", "P2"), species_dir = species_dir,
    fetch_fn = fake_fetch, existing = existing,
    should_cancel = function() TRUE
  )
  expect_true(isTRUE(res$canceled))
  expect_false(called)                                   # never fetched
  expect_false(file.exists(file.path(species_dir, "uniprot_features",
                                     "uniprot_features.tsv")))  # no write
})

test_that("species_cache honors a mid-fetch cancel flag from the fetcher", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)

  # Fetcher reports canceled = TRUE (as the real one does when stopped at a page
  # boundary) -> orchestrator must NOT write a partial cache.
  canceling_fetch <- function(accessions, ...) {
    list(features = pelsa_empty_feature_frame(), unresolved = accessions,
         canceled = TRUE)
  }
  res <- pelsa_refresh_species_cache(
    species = "9606", universe = c("P1", "P2"), species_dir = species_dir,
    fetch_fn = canceling_fetch, existing = NULL
  )
  expect_true(isTRUE(res$canceled))
  expect_false(file.exists(file.path(species_dir, "uniprot_features",
                                     "uniprot_features.tsv")))
})

test_that("call_fetch_fn forwards callbacks only to a fetcher that declares them", {
  # Minimal stub (no extra args) must still work.
  simple <- function(accessions) list(features = .fake_feature_df(),
                                       unresolved = character(0))
  expect_silent(
    out <- .pelsa_call_fetch_fn(simple, c("P1"), on_batch = function(...) {},
                                should_cancel = function() FALSE))
  expect_equal(nrow(out$features), 3L)

  # A fetcher that DOES declare them receives them.
  seen <- new.env(); seen$ob <- FALSE; seen$sc <- FALSE
  rich <- function(accessions, on_batch = NULL, should_cancel = NULL) {
    seen$ob <- is.function(on_batch); seen$sc <- is.function(should_cancel)
    list(features = .fake_feature_df(), unresolved = character(0))
  }
  .pelsa_call_fetch_fn(rich, c("P1"), on_batch = function(...) {},
                       should_cancel = function() FALSE)
  expect_true(seen$ob); expect_true(seen$sc)
})

# ---- inline progress + result UI (pure tag constructors) ---------------------

test_that("progress_ui clamps + shows percent and detail", {
  html <- as.character(pelsa_refresh_progress_ui(0.62, "(1/2) human . page 88/140"))
  expect_match(html, "62%")
  expect_match(html, "page 88/140")
  # Clamp out-of-range fractions.
  expect_match(as.character(pelsa_refresh_progress_ui(1.5)), "100%")
  expect_match(as.character(pelsa_refresh_progress_ui(-1)), "0%")
})

test_that("result_ui colors by worst status (ok / warn / error)", {
  ok <- list(list(species = "9606", n_features = 100L, n_unresolved = 0L,
                  n_retained_from_cache = 0L, had_existing = TRUE,
                  canceled = FALSE, error = NULL))
  warn <- list(list(species = "9606", n_features = 100L, n_unresolved = 3L,
                    n_retained_from_cache = 5L, had_existing = TRUE,
                    canceled = FALSE, error = NULL))
  err <- list(list(species = "9606", error = "boom"))
  cancel <- list(list(species = "9606", canceled = TRUE, error = NULL))

  expect_match(as.character(pelsa_refresh_result_ui(ok)), "#5cb85c")     # green
  expect_match(as.character(pelsa_refresh_result_ui(warn)), "#f0ad4e")   # amber
  expect_match(as.character(pelsa_refresh_result_ui(err)), "#d9534f")    # red
  expect_match(as.character(pelsa_refresh_result_ui(cancel)), "#f0ad4e") # amber
  expect_match(as.character(pelsa_refresh_result_ui(cancel)),
               "left unchanged")
  expect_null(pelsa_refresh_result_ui(NULL))
})

# ---- notifications: canceled species surfaced honestly -----------------------

test_that("notifications report a canceled species + exclude it from 'complete'", {
  results <- list(
    list(species = "9606", n_features = 100L, n_unresolved = 0L,
         n_retained_from_cache = 0L, had_existing = TRUE, canceled = FALSE,
         error = NULL),
    list(species = "10090", canceled = TRUE, not_run = TRUE, error = NULL)
  )
  notes <- pelsa_refresh_notifications(results)
  msgs <- vapply(notes, function(n) n$message, character(1))
  expect_true(any(grepl("canceled", msgs, ignore.case = TRUE)))
  # The success summary mentions human but NOT mouse.
  complete <- msgs[grepl("refresh complete", msgs, ignore.case = TRUE)]
  expect_length(complete, 1L)
  expect_match(complete, "9606")
  expect_false(grepl("10090", complete))
})

# ---- notifications + result UI: mode-aware wording --------------------------

test_that("notifications: full mode says 'rebuilt' and notes the wipe", {
  results <- list(
    list(species = "10090", n_features = 500L, n_unresolved = 0L,
         n_retained_from_cache = 0L, had_existing = TRUE, mode = "full",
         canceled = FALSE, error = NULL))
  msgs <- vapply(pelsa_refresh_notifications(results),
                 function(n) n$message, character(1))
  summary <- msgs[grepl("rebuilt", msgs, ignore.case = TRUE)]
  expect_length(summary, 1L)
  expect_match(summary, "cleared")  # wipe note
})

test_that("notifications: incremental mode says 'topped up' + retained count", {
  results <- list(
    list(species = "9606", n_features = 120L, n_unresolved = 0L,
         n_retained_from_cache = 100L, had_existing = TRUE,
         mode = "incremental", canceled = FALSE, error = NULL))
  msgs <- vapply(pelsa_refresh_notifications(results),
                 function(n) n$message, character(1))
  summary <- msgs[grepl("topped up", msgs, ignore.case = TRUE)]
  expect_length(summary, 1L)
  expect_match(summary, "100")  # retained count surfaced
})

test_that("notifications report 3 mutually-exclusive protein counts", {
  # 70 with features + 30 with no features + 2 unresolved -- no double-counting.
  results <- list(
    list(species = "9606", n_features = 100L, n_with_features = 70L,
         n_unresolved = 2L, n_zero_feature = 30L, n_retained_from_cache = 0L,
         had_existing = FALSE, mode = "incremental", canceled = FALSE,
         error = NULL))
  msgs <- vapply(pelsa_refresh_notifications(results),
                 function(n) n$message, character(1))
  summary <- msgs[grepl("topped up", msgs, ignore.case = TRUE)]
  expect_length(summary, 1L)
  expect_match(summary, "70 proteins with features")  # NOT n_features (100)
  expect_match(summary, "30 with no features")
})

test_that("result_ui shows the with-features + zero-feature counts", {
  res <- list(list(species = "9606", n_features = 100L, n_with_features = 70L,
                   n_unresolved = 2L, n_zero_feature = 30L,
                   n_retained_from_cache = 0L, had_existing = FALSE,
                   mode = "incremental", canceled = FALSE, error = NULL))
  html <- as.character(pelsa_refresh_result_ui(res))
  expect_match(html, "70 proteins with features")
  expect_match(html, "30 with no features")
})

test_that("result_ui: full mode line says rebuilt, incremental says topped up", {
  full <- list(list(species = "10090", n_features = 500L, n_unresolved = 0L,
                    n_retained_from_cache = 0L, had_existing = TRUE,
                    mode = "full", canceled = FALSE, error = NULL))
  incr <- list(list(species = "9606", n_features = 120L, n_unresolved = 0L,
                    n_retained_from_cache = 100L, had_existing = TRUE,
                    mode = "incremental", canceled = FALSE, error = NULL))
  expect_match(as.character(pelsa_refresh_result_ui(full)),
               "rebuilt", ignore.case = TRUE)
  expect_match(as.character(pelsa_refresh_result_ui(incr)),
               "topped up", ignore.case = TRUE)
})

# ---- pelsa_gcts_for_species (Defect #1: species-dataset accession guard) -----

test_that("pelsa_gcts_for_species keeps ALL same-species datasets (union), drops others", {
  gcts <- list(
    ome_h1 = data.frame(PG.ProteinAccessions = "P10000", stringsAsFactors = FALSE),
    ome_h2 = data.frame(PG.ProteinAccessions = "P20000", stringsAsFactors = FALSE),
    ome_m1 = data.frame(PG.ProteinAccessions = "Q30000", stringsAsFactors = FALSE)
  )
  species_by_ds <- list(ome_h1 = "9606", ome_h2 = "9606", ome_m1 = "10090")

  human <- pelsa_gcts_for_species(gcts, species_by_ds, "9606")
  expect_setequal(names(human), c("ome_h1", "ome_h2"))   # both human datasets
  # The union of accessions across both same-species datasets is preserved.
  expect_setequal(pelsa_incremental_universe(human, NULL),
                  c("P10000", "P20000"))
  # Mouse dataset excluded from the human universe (no spillover).
  expect_false("Q30000" %in% pelsa_incremental_universe(human, NULL))

  mouse <- pelsa_gcts_for_species(gcts, species_by_ds, "10090")
  expect_setequal(names(mouse), "ome_m1")
})

test_that("pelsa_gcts_for_species: '(none)'/unset species never matches a real species", {
  gcts <- list(
    ome_a = data.frame(PG.ProteinAccessions = "P10000", stringsAsFactors = FALSE),
    ome_b = data.frame(PG.ProteinAccessions = "P20000", stringsAsFactors = FALSE)
  )
  species_by_ds <- list(ome_a = "9606", ome_b = "(none)")
  out <- pelsa_gcts_for_species(gcts, species_by_ds, "9606")
  expect_setequal(names(out), "ome_a")
})

test_that("pelsa_gcts_for_species returns NULL/empty input unchanged", {
  expect_null(pelsa_gcts_for_species(NULL, list(), "9606"))
  expect_length(pelsa_gcts_for_species(list(), list(), "9606"), 0L)
})

# ---- full-mode round-trip equivalence (no network) --------------------------

test_that("full refresh round-trips the fetched frame through wipe/write/read", {
  db <- withr::local_tempdir()
  sd <- file.path(db, "10090"); dir.create(sd)
  dir.create(file.path(sd, "fasta"))
  writeLines(c(">sp|P00001|A t", "MKV", ">sp|P00002|B t", "AAA"),
             file.path(sd, "fasta", "p.fasta"))
  # Pre-seed a stale cache the wipe must clear.
  pelsa_write_feature_cache(.existing_cache_df(), sd)

  canned <- .fake_feature_df()  # P00001(x2), P00002
  fake_fetch <- function(accessions, ...) list(features = canned,
                                               unresolved = character(0))

  results <- pelsa_run_species_refresh(
    species = "10090", database_dir = db, uploaded_gcts = NULL,
    fetch_fn = fake_fetch, mode = "full")
  expect_null(results[[1]]$error)

  back <- pelsa_read_feature_cache(sd)
  # Stale P1/P2/P3 gone; only the canned fetch frame remains, value-faithful.
  expect_identical(back$accession, canned$accession)
  expect_identical(back$start, canned$start)
  expect_identical(back$end, canned$end)
  expect_identical(back$feature_class, canned$feature_class)
  expect_identical(back$class_score, canned$class_score)
  expect_identical(back$coord_quality, canned$coord_quality)
})

# ===========================================================================
# --- from refresh-benchmark ---
# ===========================================================================
# Benchmark / runtime characterization for the PELSA UniProt-annotation refresh
# (the Setup tab "Maintenance: UniProt annotation library" control), focused on
# the HUMAN species.
#
# WHY THIS FILE EXISTS
#   The refresh is the single slowest user-facing PELSA action. Its cost is
#   dominated by the network fetch in pelsa_fetch_uniprot(), which today issues
#   ONE HTTP request PER ACCESSION, serially, throttled to ~10 req/s. For the
#   human FALLBACK universe (the whole FASTA proteome, ~70k accessions) that is
#   ~70k / 10 = ~7000 s ~= 2 h floor before any per-request latency or retries.
#
#   These tests measure what we CAN measure deterministically and offline:
#     1. the orchestration overhead the refresh adds ON TOP of the network
#        (merge + write + progress), to prove the pipeline itself is cheap and
#        the network is the whole story; and
#     2. a calibrated MODEL of wall-clock under the current serial design vs a
#        batched-stream design, so the speed-up is quantified, not hand-waved.
#
#   NO LIVE NETWORK: every fetch goes through the injected `fetch_fn` seam. The
#   "network" is a calibrated sleep/arithmetic model, never a real request, so
#   this file is hermetic and CI-safe (it is also tagged slow-ish but bounded:
#   the real-time sleeps are tiny and only run on a small sample).
#
# These are characterization tests, not pass/fail correctness gates: they assert
# only ROBUST inequalities (batched < serial; overhead << network) that hold by
# construction, so they are not flaky on a busy machine.

# ---- calibration constants (documented model, not magic numbers) -------------
# Per-request wall-clock the live design pays per accession: throttle slot
# (1/rate s) PLUS a round-trip latency. We model both; only their RATIO matters
# for the inequality assertions, so absolute values need only be plausible.
.BENCH_RATE_PER_S        <- 10      # req_throttle(capacity = 10, fill_time_s = 1)
.BENCH_RTT_S             <- 0.12    # typical UniProt JSON RTT under throttle
.BENCH_STREAM_PAGE       <- 500L    # accessions per batched /stream page (model)
.BENCH_STREAM_PAGE_RTT_S <- 0.9     # one big page's RTT (bigger payload)

# Wall-clock model for the CURRENT serial, one-accession-per-request fetcher.
.bench_serial_seconds <- function(n) {
  n * (1 / .BENCH_RATE_PER_S + .BENCH_RTT_S)
}

# Wall-clock model for a BATCHED /stream fetcher (ceil(n / page) page requests,
# each paying one big-page RTT; throttle is a non-binding ~1 req/s for pages).
.bench_batched_seconds <- function(n, page = .BENCH_STREAM_PAGE) {
  n_pages <- ceiling(n / page)
  n_pages * .BENCH_STREAM_PAGE_RTT_S
}

# A canned 8-col feature frame for K accessions (orchestration input; the shape
# the parser/fetcher returns). Cheap to build for large K.
.bench_feature_df <- function(accs) {
  k <- length(accs)
  data.frame(
    accession     = accs,
    feature_type  = rep("domain", k),
    start         = rep(10L, k),
    end           = rep(120L, k),
    description   = rep("kinase domain", k),
    feature_class = rep("catalytic_domain", k),
    class_score   = rep(3L, k),
    coord_quality = rep("exact", k),
    stringsAsFactors = FALSE
  )
}

# ---- 1. Orchestration overhead is negligible vs the network ------------------

test_that("refresh orchestration (merge + write) is fast; the network dominates", {
  skip_on_cran()
  # A realistic single-experiment universe size (proteins in a typical dataset).
  n <- 5000L
  accs <- sprintf("P%05d", seq_len(n))
  species_dir <- tempfile("pelsa_bench_human_")
  dir.create(species_dir, recursive = TRUE)

  # Injected "fetch" that does NO real I/O: returns the canned frame instantly.
  # This isolates the orchestration cost (universe merge + write_tsv + schema)
  # from the network so we can show the pipeline itself is not the bottleneck.
  fake_fetch <- function(accessions) {
    list(features = .bench_feature_df(accessions), unresolved = character(0))
  }

  t <- system.time(
    res <- pelsa_refresh_species_cache(
      species = "human", universe = accs, species_dir = species_dir,
      fetch_fn = fake_fetch, existing = NULL, progress = NULL
    )
  )
  orchestration_s <- unname(t["elapsed"])

  expect_equal(res$n_features, n)
  expect_true(file.exists(res$path))

  # The network the live fetcher WOULD pay for the same universe.
  network_serial_s <- .bench_serial_seconds(n)

  # Orchestration must be a tiny fraction of the network it sits on top of. We
  # assert a very loose bound (<10% of modelled network, and < 5 s absolute) so
  # this is robust on slow CI, while still proving the network is the story.
  expect_lt(orchestration_s, 0.10 * network_serial_s)
  expect_lt(orchestration_s, 5)

  message(sprintf(
    "[bench] human n=%d: orchestration=%.3fs  vs modelled serial network=%.0fs (%.1f min)",
    n, orchestration_s, network_serial_s, network_serial_s / 60))
})

# ---- 2. Serial per-accession design: human runtime is hours, not minutes -----

test_that("human FALLBACK universe (~70k accessions) blows past 'several minutes'", {
  # The no-datasets-uploaded fallback fetches the whole FASTA proteome. The
  # committed human FASTA has ~70k headers; model the serial wall-clock.
  n_human_fallback <- 70000L
  serial_s <- .bench_serial_seconds(n_human_fallback)

  # Floor is well over an hour - far beyond the "several minutes per species"
  # the UI promises. This test DOCUMENTS the gap (and guards against anyone
  # quietly assuming it's fast).
  expect_gt(serial_s, 60 * 60)   # > 1 hour
  message(sprintf(
    "[bench] human fallback n=%d: serial model=%.0fs (%.1f h)",
    n_human_fallback, serial_s, serial_s / 3600))
})

test_that("a realistic single-dataset universe already exceeds 'several minutes'", {
  # Even the dataset-driven (non-fallback) path is slow: a mid-size experiment
  # annotates several thousand proteins.
  n_dataset <- 8000L
  serial_s <- .bench_serial_seconds(n_dataset)
  expect_gt(serial_s, 5 * 60)    # > 5 minutes for a single mid-size dataset
  message(sprintf(
    "[bench] dataset n=%d: serial model=%.0fs (%.1f min)",
    n_dataset, serial_s, serial_s / 60))
})

# ---- 3. Batched /stream design: order-of-magnitude faster, same coverage -----

test_that("batched /stream model is >=20x faster than serial for human", {
  for (n in c(5000L, 8000L, 70000L)) {
    serial_s  <- .bench_serial_seconds(n)
    batched_s <- .bench_batched_seconds(n)
    speedup   <- serial_s / batched_s
    expect_gt(speedup, 20)
    message(sprintf(
      "[bench] n=%6d: serial=%8.0fs  batched=%7.1fs  speedup=%.0fx",
      n, serial_s, batched_s, speedup))
  }
})

# ---- 4. Batching does NOT change the parsed result (accuracy preserved) ------

test_that("parsing N entries in one batch == parsing them per-accession", {
  # The accuracy guarantee behind batching: pelsa_parse_uniprot_json_batch over
  # a multi-entry /stream 'results' array yields the SAME 8-col rows as parsing
  # each entry alone and rbinding. (The classifier is per-feature and pure, so a
  # batched fetch cannot change a single class/score/coord.)
  entry <- function(acc) list(
    primaryAccession = acc,
    features = list(list(
      type = "Domain", description = "Protein kinase domain",
      location = list(start = list(value = 10L, modifier = "EXACT"),
                      end   = list(value = 120L, modifier = "EXACT"))
    ))
  )
  entries <- lapply(c("P00001", "P00002", "P00003"), entry)

  per_accession <- do.call(rbind, lapply(entries, pelsa_parse_uniprot_json))
  rownames(per_accession) <- NULL
  batched <- pelsa_parse_uniprot_json_batch(entries)

  expect_identical(batched, per_accession)
  expect_identical(sort(unique(batched$accession)),
                   c("P00001", "P00002", "P00003"))
  expect_true(all(batched$feature_class == "catalytic_domain"))
})

# ===========================================================================
# --- from species-resolve ---
# ===========================================================================
# Tests for PELSA species classification + resolution (taxonomy-code convention).
#
#   pelsa_classify_folder(folder)                  "numeric" | "named"
#   pelsa_fetch_taxon(taxon_id, ...)               taxonomy name/validation fetch
#   pelsa_read_species_meta / pelsa_write_species_meta(database_dir, ...)
#   pelsa_species_has_feature_cache(database_dir, folder)
#   pelsa_resolve_species(database_dir, folder, validate_fn, meta)
#   pelsa_refresh_species_meta_on_start(database_dir, validate_fn)
#   pelsa_species_display_label(struct)
#
# The taxonomy API is NEVER hit live: every test injects a `validate_fn` stub.
# A folder named by digits is a UniProt taxon code; a named folder is self-curated.

# ---- helpers -----------------------------------------------------------------

# A stub validate_fn factory mirroring pelsa_fetch_taxon's return contract:
#   list(status = "ok"|"not_found"|"network_error",
#        scientific_name=, common_name=, taxon_id=)
.stub_ok <- function(sci = "Homo sapiens", common = "Human") {
  function(taxon_id, ...) list(status = "ok", scientific_name = sci,
                               common_name = common, taxon_id = taxon_id)
}
.stub_not_found <- function() {
  function(taxon_id, ...) list(status = "not_found", scientific_name = NA_character_,
                               common_name = NA_character_, taxon_id = taxon_id)
}
.stub_network <- function() {
  function(taxon_id, ...) list(status = "network_error",
                               scientific_name = NA_character_,
                               common_name = NA_character_, taxon_id = taxon_id)
}

# Build a database dir with the given folders; optionally drop a feature-cache
# tsv into a folder to simulate "has_feature_cache".
.make_db <- function(folders = character(0), with_cache = character(0)) {
  db <- tempfile("pelsa_db_")
  dir.create(db)
  for (f in folders) dir.create(file.path(db, f))
  for (f in with_cache) {
    fdir <- file.path(db, f, "uniprot_features")
    dir.create(fdir, recursive = TRUE)
    writeLines("accession\tstart\tend\tfeature_class",
               file.path(fdir, "uniprot_features.tsv"))
  }
  db
}

# ---- pelsa_classify_folder ---------------------------------------------------

test_that("pelsa_classify_folder: all-digits -> numeric, else named", {
  expect_identical(pelsa_classify_folder("9606"), "numeric")
  expect_identical(pelsa_classify_folder("10090"), "numeric")
  expect_identical(pelsa_classify_folder("009606"), "numeric")  # leading zeros
  expect_identical(pelsa_classify_folder("hoylesellaTimonensis"), "named")
  expect_identical(pelsa_classify_folder("strain1"), "named")   # mixed -> named
  expect_identical(pelsa_classify_folder("9606b"), "named")
})

# ---- pelsa_species_has_feature_cache -----------------------------------------

test_that("pelsa_species_has_feature_cache detects a cache tsv without reading it", {
  db <- .make_db(folders = c("9606", "10090"), with_cache = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  expect_true(pelsa_species_has_feature_cache(db, "9606"))
  expect_false(pelsa_species_has_feature_cache(db, "10090"))
})

# ---- species_meta read/write round-trip --------------------------------------

test_that("species_meta write then read round-trips; absent file -> empty list", {
  db <- .make_db()
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  expect_identical(pelsa_read_species_meta(db), list())

  meta <- list(
    "9606" = list(type = "uniprot", taxon_id = 9606L,
                  scientific_name = "Homo sapiens", validated = TRUE)
  )
  pelsa_write_species_meta(db, meta)
  rt <- pelsa_read_species_meta(db)
  expect_identical(rt[["9606"]]$type, "uniprot")
  expect_identical(rt[["9606"]]$scientific_name, "Homo sapiens")
  expect_true(isTRUE(rt[["9606"]]$validated))
})

# ---- pelsa_resolve_species: the five verdict branches ------------------------

test_that("resolve: numeric + validation ok -> uniprot, validated, display name", {
  db <- .make_db(folders = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  s <- pelsa_resolve_species(db, "9606", validate_fn = .stub_ok("Homo sapiens"))
  expect_identical(s$type, "uniprot")
  expect_true(s$validated)
  expect_identical(s$scientific_name, "Homo sapiens")
  expect_identical(s$display, "Homo sapiens (9606)")
  expect_identical(s$folder, "9606")
  # The verdict is persisted so a re-resolve needs no network.
  meta <- pelsa_read_species_meta(db)
  expect_identical(meta[["9606"]]$type, "uniprot")
  expect_true(isTRUE(meta[["9606"]]$validated))
})

test_that("resolve: numeric + 404 not_found -> self_curated (customized)", {
  db <- .make_db(folders = "9999999")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  s <- pelsa_resolve_species(db, "9999999", validate_fn = .stub_not_found())
  expect_identical(s$type, "self_curated")
  expect_identical(s$display, "9999999 (customized)")
})

test_that("resolve: numeric + network_error + has cache -> uniprot unvalidated", {
  db <- .make_db(folders = "9606", with_cache = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  s <- pelsa_resolve_species(db, "9606", validate_fn = .stub_network())
  expect_identical(s$type, "uniprot")
  expect_false(s$validated)
  expect_identical(s$display, "9606 (annotations available, name pending)")
})

test_that("resolve: numeric + network_error + no cache -> self_curated (transient)", {
  db <- .make_db(folders = "9606")  # no cache
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  s <- pelsa_resolve_species(db, "9606", validate_fn = .stub_network())
  expect_identical(s$type, "self_curated")
  expect_identical(s$display, "9606 (customized)")
})

test_that("resolve: named folder -> self_curated, no network call", {
  db <- .make_db(folders = "hoylesellaTimonensis")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  called <- FALSE
  vf <- function(...) { called <<- TRUE; .stub_ok()(...) }
  s <- pelsa_resolve_species(db, "hoylesellaTimonensis", validate_fn = vf)
  expect_identical(s$type, "self_curated")
  expect_identical(s$display, "hoylesellaTimonensis (customized)")
  expect_false(called)  # named folders never touch the network
})

test_that("resolve: a cached validated entry is reused without calling validate_fn", {
  db <- .make_db(folders = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  pelsa_write_species_meta(db, list(
    "9606" = list(type = "uniprot", taxon_id = 9606L,
                  scientific_name = "Homo sapiens", validated = TRUE)))
  called <- FALSE
  vf <- function(...) { called <<- TRUE; .stub_ok()(...) }
  s <- pelsa_resolve_species(db, "9606", validate_fn = vf)
  expect_identical(s$display, "Homo sapiens (9606)")
  expect_false(called)
})

# ---- cache-only path (allow_fetch = FALSE): never touch the network ----------

test_that("allow_fetch=FALSE never calls validate_fn (reactive render path)", {
  db <- .make_db(folders = "9606", with_cache = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  called <- FALSE
  vf <- function(...) { called <<- TRUE; .stub_ok()(...) }

  # No cached verdict yet + has feature cache -> uniprot unvalidated, no network.
  s <- pelsa_resolve_species(db, "9606", validate_fn = vf, allow_fetch = FALSE)
  expect_false(called)
  expect_identical(s$type, "uniprot")
  expect_false(s$validated)
  expect_identical(s$display, "9606 (annotations available, name pending)")
})

test_that("allow_fetch=FALSE: numeric + no cache + no verdict -> self_curated, no network", {
  db <- .make_db(folders = "9606")  # no cache
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  called <- FALSE
  vf <- function(...) { called <<- TRUE; .stub_ok()(...) }
  s <- pelsa_resolve_species(db, "9606", validate_fn = vf, allow_fetch = FALSE)
  expect_false(called)
  expect_identical(s$type, "self_curated")
})

test_that("allow_fetch=FALSE honors a cached validated verdict (display name shown)", {
  db <- .make_db(folders = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  pelsa_write_species_meta(db, list(
    "9606" = list(type = "uniprot", taxon_id = 9606L,
                  scientific_name = "Homo sapiens", validated = TRUE)))
  s <- pelsa_resolve_species(db, "9606", allow_fetch = FALSE)
  expect_identical(s$display, "Homo sapiens (9606)")
})

# ---- refresh-on-start: promote a previously-unvalidated numeric folder -------

test_that("refresh_on_start promotes unvalidated numeric folder and rewrites meta", {
  db <- .make_db(folders = "9606", with_cache = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  # Seed an unvalidated entry (e.g. earlier offline run).
  pelsa_write_species_meta(db, list(
    "9606" = list(type = "uniprot", taxon_id = 9606L,
                  scientific_name = NA, validated = FALSE)))

  pelsa_refresh_species_meta_on_start(db, validate_fn = .stub_ok("Homo sapiens"))

  meta <- pelsa_read_species_meta(db)
  expect_true(isTRUE(meta[["9606"]]$validated))
  expect_identical(meta[["9606"]]$scientific_name, "Homo sapiens")
})

# ---- display label for each state --------------------------------------------

test_that("pelsa_species_display_label formats all three states", {
  expect_identical(
    pelsa_species_display_label(list(folder = "9606", type = "uniprot",
      validated = TRUE, scientific_name = "Homo sapiens")),
    "Homo sapiens (9606)")
  expect_identical(
    pelsa_species_display_label(list(folder = "9606", type = "uniprot",
      validated = FALSE, scientific_name = NA)),
    "9606 (annotations available, name pending)")
  expect_identical(
    pelsa_species_display_label(list(folder = "hoylesellaTimonensis",
      type = "self_curated", validated = TRUE, scientific_name = NA)),
    "hoylesellaTimonensis (customized)")
})
