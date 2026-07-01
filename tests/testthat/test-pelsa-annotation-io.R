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
      "feature_class", "class_score", "coord_quality",
      "disposition", "primary_accession")
  )
  # Legacy files (no disposition column) default disposition to "resolved".
  expect_equal(unique(out$disposition), "resolved")
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

test_that("pelsa_read_annotation_file drops fuzzy-coordinate feature rows", {
  # A 'fuzzy' coord_quality means UniProt itself flags at least one endpoint as
  # non-EXACT (OUTSIDE/UNKNOWN/UNSURE). Those uncertain boundaries must NOT enter
  # the exact-interval overlap join, so the reader EXCLUDES them at parse time.
  p <- write_tmp_tsv(c(
    "accession\tfeature_type\tstart\tend\tdescription\tcoord_quality",
    "P12345\tdomain\t30\t120\tProtein kinase\texact",
    "P12345\tregion\t50\t60\tFuzzy region\tfuzzy",
    "Q99999\tdomain\t5\t40\tIg-like\tFUZZY"  # case-insensitive
  ))
  out <- pelsa_read_annotation_file(p)

  # Only the exact row survives; both fuzzy rows (any case) are gone.
  expect_equal(nrow(out), 1L)
  expect_equal(out$accession, "P12345")
  expect_equal(out$feature_type, "domain")
  expect_false(any(tolower(out$coord_quality) == "fuzzy"))
})

test_that("pelsa_read_annotation_file keeps sentinels while dropping fuzzy rows", {
  # Fuzzy exclusion must not disturb sentinel handling: a disposition/zero-feature
  # sentinel carries coord_quality "" (never "fuzzy") and must be retained.
  p <- write_tmp_tsv(c(
    "accession\tfeature_type\tstart\tend\tdescription\tcoord_quality\tdisposition\tprimary_accession",
    "P12345\tdomain\t30\t120\tProtein kinase\tfuzzy\tresolved\t",
    "Q0SENT\t\t\t\t\t\tresolved\t",
    "Q0DEL\t\t\t\t\t\tdeleted\t"
  ))
  out <- pelsa_read_annotation_file(p)

  # The lone real feature was fuzzy -> dropped; both sentinels remain.
  expect_setequal(out$accession, c("Q0SENT", "Q0DEL"))
  expect_equal(unique(out$feature_class), "none")
})

test_that("pelsa_read_annotation_file keeps a disposition sentinel with a 'fuzzy' cell", {
  # Regression: a merged/deleted/demerged sentinel row can carry a literal "fuzzy"
  # coord_quality cell. The fuzzy-drop filter must NOT eat it -- dropping it would
  # mis-bucket an accounted (merged/deleted) accession as n_failed. Sentinels are
  # exempt from the fuzzy exclusion because they carry no interval to be uncertain.
  p <- write_tmp_tsv(c(
    "accession\tfeature_type\tstart\tend\tdescription\tcoord_quality\tdisposition\tprimary_accession",
    "P12345\tdomain\t30\t120\tProtein kinase\texact\tresolved\t",
    "Q9MERG\t\t\t\t\tfuzzy\tmerged\tP12345",
    "Q9DEL2\t\t\t\t\tfuzzy\tdeleted\t"
  ))
  out <- pelsa_read_annotation_file(p)

  # Real exact feature + both disposition sentinels survive; sentinels keep coords
  # NULLed and feature_class "none".
  expect_setequal(out$accession, c("P12345", "Q9MERG", "Q9DEL2"))
  merged <- out[out$accession == "Q9MERG", ]
  expect_equal(merged$feature_class, "none")
  expect_equal(merged$disposition, "merged")
  expect_equal(merged$primary_accession, "P12345")
  expect_true(is.na(merged$start) && is.na(merged$end))
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

test_that("pelsa_read_annotation_file returns an empty 10-col frame for 0 data rows", {
  p <- write_tmp_tsv("accession\tfeature_type\tstart\tend\tdescription")
  out <- pelsa_read_annotation_file(p)
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), 10L)
})

# --- disposition sentinels (self-describing annotation) ---

test_that("pelsa_read_annotation_file reads merged/deleted/demerged disposition rows", {
  p <- write_tmp_tsv(c(
    "accession\tfeature_type\tstart\tend\tdescription\tcoord_quality\tdisposition\tprimary_accession",
    "Q3I5F7\tactive site\t232\t232\tCharge relay\texact\tresolved\t",
    "A0A2R8Y7H3\t\t\t\t\t\tmerged\tQ3I5F7",
    "A0A024RAC6\t\t\t\t\t\tdeleted\t",
    "A6NE21\t\t\t\t\t\tdemerged\t"
  ))
  out <- pelsa_read_annotation_file(p)

  expect_equal(out$disposition, c("resolved", "merged", "deleted", "demerged"))
  expect_equal(out$primary_accession[[2]], "Q3I5F7")
  # Sentinel rows become feature_class "none" with NA coords (silent-drop contract).
  expect_equal(out$feature_class[2:4], rep("none", 3))
  expect_true(all(is.na(out$start[2:4])) && all(is.na(out$end[2:4])))
})

test_that("disposition sentinel coords are nulled even when the file supplies coords", {
  # HARDENING: pelsa_annotate_features drops rows by COORD validity, not by
  # feature_class. A disposition sentinel that (wrongly) carried real coords must
  # not survive the overlap join as a spurious "none" hit -> null its coords here.
  p <- write_tmp_tsv(c(
    "accession\tfeature_type\tstart\tend\tdescription\tcoord_quality\tdisposition\tprimary_accession",
    "A0A2R8Y7H3\t\t10\t20\t\texact\tmerged\tQ3I5F7"
  ))
  out <- pelsa_read_annotation_file(p)
  expect_equal(out$feature_class, "none")
  expect_true(is.na(out$start) && is.na(out$end))
})

test_that("disposition sentinels count as accounted, not failed", {
  feat <- pelsa_read_annotation_file(write_tmp_tsv(c(
    "accession\tfeature_type\tstart\tend\tdescription\tcoord_quality\tdisposition\tprimary_accession",
    "Q3I5F7\tactive site\t232\t232\tCharge relay\texact\tresolved\t",
    "A0A2R8Y7H3\t\t\t\t\t\tmerged\tQ3I5F7",
    "A0A024RAC6\t\t\t\t\t\tdeleted\t"
  )))
  ds <- "Q3I5F7;A0A2R8Y7H3;A0A024RAC6"
  expect_length(pelsa_unannotated_accessions(ds, feat), 0L)
  cnt <- pelsa_annotation_status_counts(ds, feat)
  expect_equal(cnt$n_failed, 0L)
  expect_equal(cnt$n_merged, 1L)
  expect_equal(cnt$n_deleted, 1L)
  expect_equal(cnt$n_with_features, 1L)
  # 6-way bucket sum invariant.
  expect_equal(cnt$n_with_features + cnt$n_zero_feature + cnt$n_merged +
                 cnt$n_demerged + cnt$n_deleted + cnt$n_failed, 3L)
})

# ---- D6 deviation tests: disorder-desc override gated to region/motif ----------

test_that("pelsa_feature_to_class D6: disorder desc reclassifies ONLY region/motif", {
  # Structural types honor a disorder description.
  expect_equal(pelsa_feature_to_class("Region", "Disordered"), "low_complexity_or_disorder")
  expect_equal(pelsa_feature_to_class("Motif", "Low complexity"), "low_complexity_or_disorder")
  # Non-structural types IGNORE an incidental disorder mention (D6 deviation).
  expect_equal(
    pelsa_feature_to_class("Mutagenesis",
      "Abolishes folding of the intrinsically disordered protein."),
    "other")
  expect_equal(pelsa_feature_to_class("Chain", "part of a disordered region"), "other")
  expect_equal(pelsa_feature_to_class("Natural variant", "in a disordered stretch"), "other")
  # A named Domain stays folded even if its note mentions disorder (D6 deviation).
  expect_equal(pelsa_feature_to_class("Domain", "disordered linker domain"), "folded_domain")
  # Compositional bias is unaffected (forced disorder at the top, regardless of desc).
  expect_equal(pelsa_feature_to_class("Compositional bias", "Basic residues"),
               "low_complexity_or_disorder")
})

test_that("pelsa_feature_to_class D6 is vectorized", {
  ft <- c("Region", "Mutagenesis", "Compositional bias")
  d  <- c("Disordered", "disordered protein", "Basic")
  expect_equal(pelsa_feature_to_class(ft, d),
               c("low_complexity_or_disorder", "other", "low_complexity_or_disorder"))
})

test_that("full disposition round-trip: every category counts correctly", {
  # Mirrors a real workflow annotation (verified live against UniProt 2026-07-01):
  # a with-feature primary + its merged secondary sentinel, a zero-feature
  # resolved sentinel, deleted x2, demerged x1. Then a dataset referencing every
  # case (incl. an isoform token, a ;-group, and a genuinely-absent accession)
  # must bucket exactly.
  feat <- pelsa_read_annotation_file(write_tmp_tsv(c(
    paste("accession", "feature_type", "start", "end", "description",
          "coord_quality", "disposition", "primary_accession", sep = "\t"),
    "P01308\tSignal\t1\t24\tsignal\texact\tresolved\t",
    "Q3I5F7\tActive site\t232\t232\tcharge\texact\tresolved\t",
    "A0A2R8Y7H3\t\t\t\t\t\tmerged\tQ3I5F7",
    "A0A024QZP7\t\t\t\t\t\tresolved\t",   # zero-feature resolved sentinel
    "A0A024RAC6\t\t\t\t\t\tdeleted\t",
    "A0A000ZZZ9\t\t\t\t\t\tdeleted\t",
    "A6NE21\t\t\t\t\t\tdemerged\t"
  )))
  ds <- c("P01308", "A0A024QZP7", "A0A2R8Y7H3;P01308", "A6NE21",
          "A0A024RAC6", "A0A000ZZZ9", "P01308-2", "ABSENTXX")

  expect_equal(pelsa_unannotated_accessions(ds, feat), "ABSENTXX")
  cnt <- pelsa_annotation_status_counts(ds, feat)
  expect_equal(cnt$n_with_features, 2L)  # P01308 + P01308-2 via base
  expect_equal(cnt$n_zero_feature, 1L)   # A0A024QZP7
  expect_equal(cnt$n_merged, 1L)         # A0A2R8Y7H3
  expect_equal(cnt$n_demerged, 1L)       # A6NE21
  expect_equal(cnt$n_deleted, 2L)        # A0A024RAC6 + A0A000ZZZ9
  expect_equal(cnt$n_failed, 1L)         # ABSENTXX only
  expect_equal(cnt$n_with_features + cnt$n_zero_feature + cnt$n_merged +
                 cnt$n_demerged + cnt$n_deleted + cnt$n_failed, 8L)  # unique tokens
})

# ---- D7 deviation tests: inhibitor/inactive domains excluded from catalytic ---

test_that("pelsa_feature_to_class D7: inhibitor/inactive domains are not catalytic", {
  expect_equal(pelsa_feature_to_class("Domain", "Cyclin-dependent kinase inhibitor"),
               "folded_domain")
  expect_equal(pelsa_feature_to_class("Domain", "Protein kinase; inactive"),
               "folded_domain")
  # Genuine catalytic domains unchanged.
  expect_equal(pelsa_feature_to_class("Domain", "Protein kinase"), "catalytic_domain")
  expect_equal(pelsa_feature_to_class("Domain", "Serine protease"), "catalytic_domain")
  # Non-catalytic named domain unchanged.
  expect_equal(pelsa_feature_to_class("Domain", "Ig-like"), "folded_domain")
})

test_that("pelsa_feature_to_class D7 is vectorized and case-insensitive", {
  ft <- c("Domain", "Domain", "Domain")
  d  <- c("Protein KINASE", "Kinase INHIBITOR", "Helicase")
  expect_equal(pelsa_feature_to_class(ft, d),
               c("catalytic_domain", "folded_domain", "catalytic_domain"))
})
