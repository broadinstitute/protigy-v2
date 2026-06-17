################################################################################
# Tests for the PELSA Volcano coverage + UniProt-feature + Woods panel helpers
# (tab_pelsa_woods_helpers.R). Pure helpers - no Shiny, no network.
################################################################################

library(testthat)

# ---- pelsa_woods_peptide_data ------------------------------------------------

.woods_matched <- function() data.frame(
  PEP.StrippedSequence = c("PEPA", "PEPB", "PEPC", "OTHER"),
  accession = c("A", "A", "A", "B"),
  pep_start = c(10L, 50L, 45L, 1L),
  pep_end   = c(20L, 60L, 55L, 5L),
  pep_occurrence_idx = 1L, stringsAsFactors = FALSE)

.woods_stat <- function() data.frame(
  PEP.StrippedSequence = c("PEPA", "PEPB", "PEPC"),
  logFC.AvB = c(-2.1, 0.3, 1.8),
  adj.P.Val.AvB = c(0.001, 0.40, 0.02),
  stringsAsFactors = FALSE)

test_that("woods_peptide_data joins spans to contrast stats, flags sig, sorts", {
  out <- pelsa_woods_peptide_data("A", .woods_matched(), .woods_stat(),
                                  "AvB", sig_cutoff = 0.05)
  expect_equal(nrow(out), 3L)                         # only protein A
  expect_identical(out$peptide_seq, c("PEPA", "PEPC", "PEPB"))  # sorted by start
  expect_equal(out$pep_start, c(10L, 45L, 50L))
  expect_equal(out$logFC[out$peptide_seq == "PEPA"], -2.1)
  expect_equal(out$sig, c(TRUE, TRUE, FALSE))         # 0.001,0.02 sig; 0.40 not
})

test_that("woods_peptide_data: sig flag is strict < cutoff at the boundary", {
  m <- data.frame(PEP.StrippedSequence = "P", accession = "A",
                  pep_start = 1L, pep_end = 5L, pep_occurrence_idx = 1L,
                  stringsAsFactors = FALSE)
  s <- data.frame(PEP.StrippedSequence = "P", logFC.AvB = 1,
                  adj.P.Val.AvB = 0.05, stringsAsFactors = FALSE)  # == cutoff
  expect_false(pelsa_woods_peptide_data("A", m, s, "AvB", 0.05)$sig)
})

test_that("woods_peptide_data drops NA-span peptides + empty when no match", {
  m <- data.frame(PEP.StrippedSequence = c("P", "Q"), accession = "A",
                  pep_start = c(NA_integer_, 5L), pep_end = c(10L, 9L),
                  pep_occurrence_idx = 1L, stringsAsFactors = FALSE)
  s <- data.frame(PEP.StrippedSequence = c("P", "Q"), logFC.AvB = c(1, 2),
                  adj.P.Val.AvB = c(0.01, 0.02), stringsAsFactors = FALSE)
  out <- pelsa_woods_peptide_data("A", m, s, "AvB")
  expect_equal(nrow(out), 1L)                         # NA-span P dropped
  expect_identical(out$peptide_seq, "Q")
  # missing contrast -> empty
  expect_equal(nrow(pelsa_woods_peptide_data("A", m, s, "NOPE")), 0L)
  # no accession match -> empty
  expect_equal(nrow(pelsa_woods_peptide_data("Z", m, s, "AvB")), 0L)
})

# ---- pelsa_coverage_intervals (IRanges union) --------------------------------

test_that("coverage_intervals merges overlapping + adjacent, sorts, drops bad", {
  # 10-20, 45-55, 50-60 -> 10-20, 45-60
  iv <- pelsa_coverage_intervals(c(10L, 45L, 50L), c(20L, 55L, 60L))
  expect_equal(iv$start, c(10L, 45L))
  expect_equal(iv$end, c(20L, 60L))
  # adjacency: 1-5 and 6-10 are adjacent -> merged into 1-10 (IRanges reduce)
  adj <- pelsa_coverage_intervals(c(1L, 6L), c(5L, 10L))
  expect_equal(adj, data.frame(start = 1L, end = 10L))
  # single residue
  expect_equal(pelsa_coverage_intervals(7L, 7L),
               data.frame(start = 7L, end = 7L))
  # empty + NA + inverted dropped
  expect_equal(nrow(pelsa_coverage_intervals(integer(0), integer(0))), 0L)
  expect_equal(nrow(pelsa_coverage_intervals(c(NA, 9L), c(5L, 3L))), 0L)
})

# ---- pelsa_feature_lanes (IRanges disjointBins) ------------------------------

test_that("feature_lanes packs overlapping features into distinct lanes", {
  f <- data.frame(start = c(1L, 5L, 40L), end = c(30L, 12L, 60L),
                  feature_class = c("catalytic_domain", "active_or_binding_site",
                                    "region_or_motif"),
                  stringsAsFactors = FALSE)
  out <- pelsa_feature_lanes(f)
  expect_true("lane" %in% colnames(out))
  # 1-30 and 5-12 overlap -> different lanes; 40-60 disjoint -> reuses a lane.
  expect_false(out$lane[1] == out$lane[2])
  expect_equal(out$lane[3], 1L)
})

test_that("feature_lanes: empty / all-invalid -> 0-row with lane column", {
  expect_equal(nrow(pelsa_feature_lanes(
    data.frame(start = integer(0), end = integer(0)))), 0L)
  bad <- data.frame(start = c(NA, 9L), end = c(5L, 3L),
                    feature_class = c("x", "y"), stringsAsFactors = FALSE)
  out <- pelsa_feature_lanes(bad)
  expect_equal(nrow(out), 0L)
  expect_true("lane" %in% colnames(out))
})

# ---- pelsa_woods_overlap_annotations (data.table foverlaps) ------------------

test_that("overlap_annotations lists DISTINCT feature names (no coords) per peptide", {
  f <- data.frame(start = c(1L, 5L, 40L), end = c(30L, 12L, 60L),
                  feature_class = c("catalytic_domain", "active_or_binding_site",
                                    "region_or_motif"),
                  stringsAsFactors = FALSE)
  ann <- pelsa_woods_overlap_annotations(c(10L, 50L, 100L), c(20L, 60L, 110L), f)
  # Names only (no @start-end), ";"-joined.
  expect_equal(ann[1], "catalytic_domain;active_or_binding_site")
  expect_equal(ann[2], "region_or_motif")
  expect_equal(ann[3], "")                            # peptide past all features
  expect_false(grepl("@", ann[1]))                    # no coordinates
})

test_that("overlap_annotations collapses repeated feature names to one", {
  # Two separate region_or_motif features both overlap the peptide -> listed ONCE.
  f <- data.frame(start = c(1L, 40L), end = c(30L, 60L),
                  feature_class = c("region_or_motif", "region_or_motif"),
                  stringsAsFactors = FALSE)
  ann <- pelsa_woods_overlap_annotations(10L, 55L, f)
  expect_equal(ann, "region_or_motif")               # de-duplicated
})

test_that("overlap_annotations: no features -> all empty; length preserved", {
  ann <- pelsa_woods_overlap_annotations(c(1L, 2L), c(5L, 6L), data.frame())
  expect_equal(ann, c("", ""))
})

# ---- plot builders smoke -----------------------------------------------------

test_that("track + panel builders return plots and tolerate empty inputs", {
  pep <- pelsa_woods_peptide_data("A", .woods_matched(), .woods_stat(), "AvB")
  iv  <- pelsa_coverage_intervals(pep$pep_start, pep$pep_end)
  fl  <- pelsa_feature_lanes(data.frame(
    start = 1L, end = 30L, feature_class = "catalytic_domain",
    feature_type = "Domain", stringsAsFactors = FALSE))

  expect_s3_class(pelsa_coverage_track_ggplot(iv, 70L), "ggplot")
  expect_s3_class(pelsa_feature_track_ggplot(fl, 70L), "ggplot")
  expect_s3_class(pelsa_woods_track_ggplot(pep, 70L), "ggplot")

  # empty-input variants still return a ggplot (placeholder), never error.
  expect_s3_class(pelsa_feature_track_ggplot(pelsa_feature_lanes(data.frame()), 70L),
                  "ggplot")
  expect_s3_class(pelsa_woods_track_ggplot(pep[0, ], 70L), "ggplot")

  p <- pelsa_woods_panel(pep, fl, iv, prot_len = 70L, source_id = "w")
  expect_s3_class(p, "plotly")
  expect_identical(p$x$source, "w")
})

# ---- M3 regression: coverage track for proteins shorter than 10 residues -----
# seq(10L, prot_len, by=...) errors ("wrong sign in 'by' argument") when
# prot_len < 10. The builder must guard the upper-tick sequence and still draw.

test_that("M3: coverage track builds for very short proteins (no error)", {
  iv <- data.frame(start = integer(0), end = integer(0))
  # prot_len = 5 and 1 previously errored in the tick seq(); must now build.
  expect_s3_class(pelsa_coverage_track_ggplot(iv, 5L), "ggplot")
  expect_s3_class(pelsa_coverage_track_ggplot(iv, 1L), "ggplot")

  # with an actual covered interval on a short protein it must still build.
  iv2 <- data.frame(start = 1L, end = 3L)
  expect_s3_class(pelsa_coverage_track_ggplot(iv2, 5L), "ggplot")
})

test_that("M3: coverage ticks unchanged for proteins >= 10 residues", {
  iv <- data.frame(start = integer(0), end = integer(0))
  g <- pelsa_coverage_track_ggplot(iv, 50L)
  expect_s3_class(g, "ggplot")
  # tick breaks match the original style: unique(c(1, seq(10, 50, by=10))).
  expected <- unique(c(1L, seq(10L, 50L, by = max(10L, round(50 / 10)))))
  brks <- ggplot2::ggplot_build(g)$layout$panel_params[[1]]$x$breaks
  brks <- brks[!is.na(brks)]
  expect_true(all(expected %in% brks))
})

test_that("woods builder uses the shared feature-class palette", {
  fl <- pelsa_feature_lanes(data.frame(
    start = c(1L, 40L), end = c(30L, 60L),
    feature_class = c("catalytic_domain", "region_or_motif"),
    feature_type = c("Domain", "Region"), stringsAsFactors = FALSE))
  gg <- pelsa_feature_track_ggplot(fl, 70L)
  b  <- suppressWarnings(ggplot2::ggplot_build(gg))
  # the fill scale draws from PELSA_FEATURE_COLORS
  used <- unique(b$data[[1]]$fill)
  expect_true(all(used %in% unname(PELSA_FEATURE_COLORS)))
})

test_that("feature_overlap_peptides: lists overlapping peptide aa-labels", {
  # feature [10,20]; peptides at starts 5(end 12), 30(end 40), 15(end 25)
  out <- pelsa_feature_overlap_peptides(
    feat_starts = c(10L), feat_ends = c(20L),
    pep_starts = c(5L, 30L, 15L), pep_ends = c(12L, 40L, 25L))
  expect_equal(out, "aa5;aa15")     # sorted by position, deduped; 30 excluded
})

test_that("feature_overlap_peptides: no overlap -> 'none'", {
  out <- pelsa_feature_overlap_peptides(c(100L), c(110L), c(5L), c(12L))
  expect_equal(out, "none")
})

test_that("woods track: -log10(adj.P) coloring, no gold-outline segment, builds", {
  pep <- data.frame(
    peptide_seq = c("A","B"), pep_start = c(1L,5L), pep_end = c(4L,9L),
    logFC = c(-2, 1.5), adj.P.Val = c(1e-9, 0.4), sig = c(TRUE, FALSE),
    stringsAsFactors = FALSE)
  gg <- pelsa_woods_track_ggplot(pep, prot_len = 20L)
  expect_s3_class(gg, "ggplot")
  # The -log10 column is clamped (1e-9 -> -log10 = 9 -> clamp 5); just assert build.
})

test_that("feature tooltip uses real feature_type + description, not feature_class", {
  f <- data.frame(start = 10L, end = 20L, feature_class = "region_or_motif",
                  feature_type = "Region", description = "Disordered",
                  lane = 1L, stringsAsFactors = FALSE, check.names = FALSE)
  gg <- pelsa_feature_track_ggplot(f, prot_len = 100L)
  # Assert on the .tip column the geom carries (the hover NAME line) - it must use
  # the real UniProt feature_type + description, not the 9-bucket feature_class.
  tip <- gg$data$.tip
  expect_true(any(grepl("Region: Disordered", tip, fixed = TRUE)))
  expect_false(any(grepl("region_or_motif", tip, fixed = TRUE)))
})

test_that("feature legend UI lists every PELSA_FEATURE_COLORS class", {
  html <- as.character(.pelsa_feature_legend_ui())
  # one entry per palette class, including ones absent from any given protein
  expect_true(grepl("transmembrane / signal", html, fixed = TRUE))
  expect_true(grepl("none / unannotated", html, fixed = TRUE))
  # one <li> entry per palette class (every class shown, present or not)
  n_li <- length(gregexpr("<li", html, fixed = TRUE)[[1]])
  expect_equal(n_li, length(PELSA_FEATURE_COLORS))
  # every palette HEX color appears as a swatch
  for (col in unname(PELSA_FEATURE_COLORS)) {
    expect_true(grepl(col, html, fixed = TRUE))
  }
})
