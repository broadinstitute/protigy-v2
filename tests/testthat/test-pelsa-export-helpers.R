################################################################################
# Tests for PELSA export helpers (pure, non-reactive).
#
#   pelsa_export_prot_len(coverage, acc, peptides) -- protein length for a Woods
#     export: prefer the coverage frame's protein_length; else fall back to the
#     max pep_end of the protein's peptides; else 1L.
################################################################################

library(testthat)

test_that("pelsa_export_prot_len uses the coverage frame's protein_length", {
  cov <- data.frame(accession = c("P1", "P2"),
                    protein_length = c(120L, 80L),
                    stringsAsFactors = FALSE)
  expect_equal(pelsa_export_prot_len(cov, "P1"), 120L)
  expect_equal(pelsa_export_prot_len(cov, "P2"), 80L)
})

test_that("pelsa_export_prot_len falls back to max(pep_end) when coverage lacks the length", {
  cov <- data.frame(accession = "P1", protein_length = NA_integer_,
                    stringsAsFactors = FALSE)
  peptides <- data.frame(pep_end = c(40L, 95L, 60L), stringsAsFactors = FALSE)
  expect_equal(pelsa_export_prot_len(cov, "P1", peptides), 95L)
})

test_that("pelsa_export_prot_len returns 1L (no warning) when all pep_end are NA", {
  # Regression: max(integer-all-NA, na.rm = TRUE) warns
  # ("no non-missing arguments to max; returning -Inf") and returns -Inf.
  # Reachable for older caches lacking span columns (pep_end all NA). The result
  # is still correct via the < 1L -> 1L fallback, but no warning should leak.
  cov <- data.frame(accession = "P1", protein_length = NA_integer_,
                    stringsAsFactors = FALSE)
  peptides <- data.frame(pep_end = c(NA_integer_, NA_integer_),
                         stringsAsFactors = FALSE)
  expect_no_warning(plen <- pelsa_export_prot_len(cov, "P1", peptides))
  expect_equal(plen, 1L)
})

test_that("pelsa_export_prot_len returns 1L when no coverage and no peptides", {
  expect_equal(pelsa_export_prot_len(NULL, "P1"), 1L)
})

test_that("pelsa_export_prot_len floors a non-positive max(pep_end) at 1L", {
  cov <- data.frame(accession = "P1", protein_length = NA_integer_,
                    stringsAsFactors = FALSE)
  peptides <- data.frame(pep_end = c(0L, -5L), stringsAsFactors = FALSE)
  expect_equal(pelsa_export_prot_len(cov, "P1", peptides), 1L)
})
