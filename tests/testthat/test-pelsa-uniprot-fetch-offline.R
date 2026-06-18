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
    c("P1", "P2", "P3", "P4", "P5"),
    batch_size = 2L,                      # -> 3 batches: (P1,P2)(P3,P4)(P5)
    on_batch = function(done, total) seen[[length(seen) + 1L]] <<- c(done, total)
  )
  # 3 batches -> 3 callbacks, done = 1,2,3 and total = 3 each time
  expect_equal(length(seen), 3L)
  expect_equal(seen[[1]], c(1L, 3L))
  expect_equal(seen[[3]], c(3L, 3L))
  # all accessions resolved (the fake entries carry each accession)
  expect_setequal(res$features$accession, c("P1", "P2", "P3", "P4", "P5"))
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
  res <- pelsa_fetch_uniprot(paste0("P", 1:10), batch_size = 1L)
  expect_setequal(res$unresolved, paste0("P", 1:10))
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
      # P1 has features, P2 is returned but feature-less.
      ent <- list(fake_entry("P1"), fake_entry_no_features("P2"))
      list(entries = ent, failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P1", "P2"), batch_size = 2L)
  # P2 was returned (resolved) even though it contributed no feature rows.
  expect_false("P2" %in% res$unresolved)
  expect_length(res$unresolved, 0L)
  # P1 still produced its feature row.
  expect_true("P1" %in% res$features$accession)
})

test_that("an input secondary accession returned under its primary is RESOLVED", {
  # Regression: UniProt returns a demerged accession's entry under the PRIMARY
  # accession (with the secondary listed in secondaryAccessions). The input
  # secondary must be marked resolved, not unresolved.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      # input was the secondary "P0SEC"; UniProt returns it as primary "Q99PRI".
      list(entries = list(fake_entry_with_secondary("Q99PRI", "P0SEC")),
           failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P0SEC"), batch_size = 2L)
  expect_false("P0SEC" %in% res$unresolved)
  expect_length(res$unresolved, 0L)
})

test_that("a genuinely-absent accession is still reported unresolved", {
  # Control: an accession UniProt never returns stays unresolved.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      # only P1 comes back; P_GHOST is absent.
      list(entries = list(fake_entry("P1")), failed = FALSE)
    },
    .package = "Protigy"
  )
  res <- pelsa_fetch_uniprot(c("P1", "P_GHOST"), batch_size = 2L)
  expect_true("P_GHOST" %in% res$unresolved)
  expect_false("P1" %in% res$unresolved)
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
    pelsa_fetch_uniprot(paste0("P", seq_len(limit + 2L)), batch_size = 1L),
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
  res <- pelsa_fetch_uniprot(paste0("P", seq_len(n)), batch_size = 1L)
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
    paste0("P", 1:6), batch_size = 2L,
    should_cancel = function() calls$n >= 1L
  )
  expect_true(res$canceled)
  # only the first batch's two accessions resolved
  expect_equal(calls$n, 1L)
  expect_setequal(res$features$accession, c("P1", "P2"))
  # the not-yet-fetched accessions are unresolved
  expect_true(all(c("P3", "P4", "P5", "P6") %in% res$unresolved))
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
