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

test_that("on_batch fires once per batch with (done, total)", {
  seen <- list()
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) {
      lapply(accs, fake_entry)
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
  # The real .pelsa_fetch_one_batch returns list() for a 4xx (query matched
  # nothing). Simulate that: every batch returns no entries.
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) list(),
    .package = "Protigy"
  )
  # 10 accessions / batch_size 1 -> 10 empty batches. If empties counted toward
  # the breaker (limit 5) this would error; it must NOT.
  res <- pelsa_fetch_uniprot(paste0("P", 1:10), batch_size = 1L)
  expect_setequal(res$unresolved, paste0("P", 1:10))
  expect_equal(nrow(res$features), 0L)
  expect_false(res$canceled)
})

test_that("breaker trips after .PELSA_BREAKER_LIMIT consecutive failed batches", {
  # Every batch throws (a 5xx/network failure surfaces as an error here).
  testthat::local_mocked_bindings(
    .pelsa_fetch_one_batch = function(base_req, accs, size) stop("503 boom"),
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
      if (calls$n %% 2L == 0L) return(lapply(accs, fake_entry))  # success resets
      stop("transient failure")
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
      lapply(accs, fake_entry)
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
  entries <- fetch_one(httr2::request("http://x"), c("P1"), size = 1L)
  expect_length(entries, 1L)
})

test_that(".pelsa_fetch_one_batch yields zero entries for a 4xx (no throw)", {
  fetch_one <- get(".pelsa_fetch_one_batch", envir = asNamespace("Protigy"))
  testthat::local_mocked_bindings(
    req_url_path_append = function(req, ...) req,
    req_url_query = function(req, ...) req,
    req_perform_iterative = function(req, ...) list(make_resp(400L)),
    resp_status = function(resp) resp$.status,
    resp_body_json = function(resp, ...) list(results = list()),
    .package = "httr2"
  )
  entries <- fetch_one(httr2::request("http://x"), c("P1"), size = 1L)
  expect_length(entries, 0L)
})

test_that(".pelsa_fetch_one_batch throws on a 5xx terminal error (breaker fuel)", {
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
  expect_error(fetch_one(httr2::request("http://x"), c("P1"), size = 1L))
})
