################################################################################
# Tests for pelsa_read_annotation_file() in R/tab_pelsa_annotation_io.R
################################################################################

write_tmp_tsv <- function(text) {
  p <- tempfile(fileext = ".tsv")
  writeLines(text, p)
  p
}

test_that("pelsa_read_annotation_file classifies a valid raw file", {
  p <- write_tmp_tsv(c(
    "accession\tfeature_type\tstart\tend\tdescription",
    "P12345\tactive site\t10\t10\tProton acceptor",
    "P12345\tdomain\t30\t120\tProtein kinase",
    "Q99999\trepeat\t5\t40\tWD 1"
  ))
  out <- pelsa_read_annotation_file(p)

  expect_equal(nrow(out), 3L)
  expect_equal(
    colnames(out),
    c("accession", "feature_type", "start", "end", "description",
      "feature_class", "class_score", "coord_quality")
  )
  expect_true(is.integer(out$start))
  expect_true(is.integer(out$end))
  # Classifier parity: active site -> active_or_binding_site (score 5);
  # "Protein kinase" domain -> catalytic_domain (score 3); repeat -> repeat (-1).
  expect_equal(out$feature_class,
               c("active_or_binding_site", "catalytic_domain",
                 "repeat_or_coiled_coil"))
  expect_equal(out$class_score, c(5L, 3L, -1L))
  # coord_quality defaulted to "exact" when the column is absent.
  expect_equal(unique(out$coord_quality), "exact")
})

test_that("pelsa_read_annotation_file honors a provided coord_quality column", {
  p <- write_tmp_tsv(c(
    "accession\tfeature_type\tstart\tend\tdescription\tcoord_quality",
    "P12345\tdomain\t30\t120\tProtein kinase\tfuzzy"
  ))
  out <- pelsa_read_annotation_file(p)
  expect_equal(out$coord_quality, "fuzzy")
})

test_that("pelsa_read_annotation_file maps zero-feature sentinel rows to 'none'", {
  # The external fetch workflow emits a row per RESOLVED-but-0-feature accession:
  # blank feature_type + blank start/end + blank coord_quality (readr -> NA).
  p <- write_tmp_tsv(c(
    "accession\tfeature_type\tstart\tend\tdescription\tcoord_quality",
    "P12345\tdomain\t30\t120\tProtein kinase\texact",
    "Q0SENT\t\t\t\t\t"  # zero-feature sentinel
  ))
  out <- pelsa_read_annotation_file(p)

  expect_equal(nrow(out), 2L)
  sent <- out[out$accession == "Q0SENT", ]
  expect_equal(sent$feature_class, "none")   # NOT "other"
  expect_equal(sent$class_score, 0L)
  expect_equal(sent$coord_quality, "")        # NOT "exact"
  expect_equal(sent$feature_type, "")         # NA normalized to ""
  expect_true(is.na(sent$start) && is.na(sent$end))

  # The real feature row is unaffected.
  real <- out[out$accession == "P12345", ]
  expect_equal(real$feature_class, "catalytic_domain")
  expect_equal(real$coord_quality, "exact")
})

test_that("pelsa_read_annotation_file sentinel handling feeds annotation status counts", {
  # End-to-end: a sentinel accession must count as zero-feature, NOT failed and
  # NOT feature-bearing, in pelsa_annotation_status_counts().
  p <- write_tmp_tsv(c(
    "accession\tfeature_type\tstart\tend\tdescription\tcoord_quality",
    "P12345\tdomain\t30\t120\tProtein kinase\texact",
    "Q0SENT\t\t\t\t\t"
  ))
  feat_df <- pelsa_read_annotation_file(p)
  counts <- pelsa_annotation_status_counts(c("P12345", "Q0SENT", "ABSENT9"),
                                           feat_df)
  expect_equal(counts$n_with_features, 1L)  # P12345
  expect_equal(counts$n_zero_feature, 1L)   # Q0SENT sentinel
  expect_equal(counts$n_failed, 1L)         # ABSENT9 not in feat_df
})

test_that("pelsa_read_annotation_file errors on a missing required column", {
  p <- write_tmp_tsv(c(
    "accession\tstart\tend\tdescription",   # no feature_type
    "P12345\t10\t10\tx"
  ))
  expect_error(pelsa_read_annotation_file(p), "missing required column")
})

test_that("pelsa_read_annotation_file errors when the file does not exist", {
  expect_error(pelsa_read_annotation_file(tempfile(fileext = ".tsv")),
               "not found")
})

test_that("pelsa_read_annotation_file returns an empty 8-col frame for 0 data rows", {
  p <- write_tmp_tsv("accession\tfeature_type\tstart\tend\tdescription")
  out <- pelsa_read_annotation_file(p)
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), 8L)
})
