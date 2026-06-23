# PELSA pure-helper edge cases -- relocated from test-misc-helpers.R during the
# PELSA/out-of-scope PR split. Covers small deterministic helpers (no Shiny, no
# network): name sanitization, depth-summary CV guard, malformed-JSON parsing,
# headerless-FASTA error, and 0-row passthroughs.

# PELSA edge cases
# ---------------------------------------------------------------------------

test_that("pelsa_safe_name sanitizes unsafe characters and collapses runs", {
  expect_equal(pelsa_safe_name("A B/C:D"), "A_B_C_D")
  expect_equal(pelsa_safe_name("keep.dot-dash_underscore"),
               "keep.dot-dash_underscore")
  expect_equal(pelsa_safe_name("  weird  spaces  "), "weird_spaces")
  # leading/trailing separators are trimmed
  expect_equal(pelsa_safe_name("__edge__"), "edge")
})

test_that("pelsa_safe_name maps NA / empty to 'unknown'", {
  expect_equal(pelsa_safe_name(NA), "unknown")
  expect_equal(pelsa_safe_name(""), "unknown")
  expect_equal(pelsa_safe_name(c("ok", NA, "")), c("ok", "unknown", "unknown"))
})

test_that("pelsa_depth_summary(c(0,0,0)) guards a non-finite CV to NA", {
  # mean = 0 -> cv = sd/0 = NaN -> guarded to NA_real_
  res <- pelsa_depth_summary(c(0, 0, 0))
  expect_equal(res$mean_n, 0)
  expect_equal(res$median_n, 0)
  expect_true(is.na(res$cv_pct))
})

test_that("pelsa_read_fasta stops when no header line is present", {
  f <- tempfile(fileext = ".fasta")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("ACDEFGHIK", "LMNPQRST"), f)   # sequence-only, no '>' header
  expect_error(pelsa_read_fasta(f), "no FASTA header")
})

test_that("pelsa_explode_accessions returns a 0-row frame with its added columns", {
  empty <- data.frame(
    PG.ProteinAccessions = character(0),
    PG.Genes = character(0),
    PEP.PeptidePosition = character(0),
    stringsAsFactors = FALSE
  )
  out <- pelsa_explode_accessions(empty)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_true(all(c("accession", "gene", "pep_position_token") %in% names(out)))
})

test_that("pelsa_thin_background passes through a 0-row volcano frame", {
  empty <- data.frame(
    Significant = logical(0), logFC = numeric(0),
    logP = numeric(0), is_marker = logical(0),
    stringsAsFactors = FALSE
  )
  out <- pelsa_thin_background(empty)
  expect_equal(out$n_total, 0L)
  expect_equal(out$n_shown, 0L)
  expect_equal(out$n_thinnable, 0L)
  expect_equal(nrow(out$df), 0L)
})
