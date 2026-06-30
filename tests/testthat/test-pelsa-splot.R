################################################################################
# Tests for the PELSA Summary intensity-rank (S-plot) panel.
# PURE helpers (no Shiny) + light testServer. No network.
################################################################################

library(testthat)

test_that("S-plot constants have the expected fixed values", {
  expect_identical(.PELSA_TRYPSIN_ACCESSIONS, c("Q29463", "P00760", "P00761"))
  expect_identical(.PELSA_SPLOT_TOP_N, 3L)
  expect_match(.PELSA_SPLOT_TRYPSIN_COLOR, "^#[0-9A-Fa-f]{6}$")
  expect_identical(.PELSA_SPLOT_SUBDIR, "intensity_rank")
})

test_that("axis title reflects log base and normalization, forcing log2 for None", {
  expect_equal(
    pelsa_splot_axis_title(list(log_transformation = "log2",
                                data_normalization = "Median (non-zero)")),
    "log2(intensity), Median (non-zero) normalized")
  expect_equal(
    pelsa_splot_axis_title(list(log_transformation = "log10",
                                data_normalization = "Quantile")),
    "log10(intensity), Quantile normalized")
  # None log -> forced log2; None normalization -> clause dropped
  expect_equal(
    pelsa_splot_axis_title(list(log_transformation = "None",
                                data_normalization = "None")),
    "log2(intensity)")
  # Missing fields are tolerated
  expect_equal(pelsa_splot_axis_title(list()), "log2(intensity)")
})

test_that("display_intensity: already-log as-is; None forces log2 and drops non-positive", {
  expect_equal(pelsa_splot_display_intensity(c(10, 8), "log2"), c(10, 8))
  expect_equal(pelsa_splot_display_intensity(c(4, 2), "None"), c(2, 1))   # log2
  v <- pelsa_splot_display_intensity(c(8, 0, -3, NA), "None")
  expect_equal(v[1], 3)                       # log2(8)
  expect_true(all(is.na(v[2:4])))             # 0, negative, NA -> NA
})

test_that("rank_frame drops NA, ranks desc, keeps row_id = matrix row index", {
  mat <- matrix(c(5, NA, 9, 1), nrow = 4,
                dimnames = list(c("p1","p2","p3","p4"), "S1"))
  pf <- data.frame(
    PEP.StrippedSequence = c("AAA","BBB","CCC","DDD"),
    PG.ProteinAccessions = c("P1","P2","P3","P4"),
    PG.Genes             = c("GA","GB","GC","GD"),
    stringsAsFactors = FALSE)
  rf <- pelsa_splot_rank_frame(mat, "S1", pf, "log2")
  expect_equal(nrow(rf), 3L)                      # p2 (NA) dropped
  expect_equal(rf$rank, 1:3)
  expect_equal(rf$row_id, c(3L, 1L, 4L))          # 9, 5, 1 -> rows 3,1,4
  expect_equal(rf$display_intensity, c(9, 5, 1))
  expect_equal(rf$sequence[1], "CCC")
})

test_that("rank_frame returns an empty typed frame when no finite values", {
  mat <- matrix(c(NA_real_, NA_real_), nrow = 2, dimnames = list(NULL, "S1"))
  pf <- data.frame(PEP.StrippedSequence = c("A","B"),
                   PG.ProteinAccessions = c("P","Q"),
                   PG.Genes = c("g","h"), stringsAsFactors = FALSE)
  rf <- pelsa_splot_rank_frame(mat, "S1", pf, "log2")
  expect_equal(nrow(rf), 0L)
  expect_true(all(c("row_id","sequence","accessions","genes",
                    "display_intensity","rank") %in% names(rf)))
})

test_that("marker_topn caps at N per accession and is marker-scoped", {
  rf <- data.frame(
    row_id = 1:5, sequence = paste0("S", 1:5),
    accessions = "x", genes = "x",
    display_intensity = c(50, 40, 30, 20, 10), rank = 1:5,
    stringsAsFactors = FALSE)
  matched <- data.frame(
    .row_id   = c(1L, 2L, 3L, 4L, 5L),
    accession = c("M1","M1","M1","M1","M2"),
    gene      = c("GA","GA","GA","GA","GB"),
    pep_start = c(10L, 20L, 30L, 40L, 5L),
    PEP.StrippedSequence = paste0("S", 1:5),
    check.names = FALSE, stringsAsFactors = FALSE)
  res <- pelsa_splot_marker_topn(matched, c("M1","M2"), rf, n = 3L)
  expect_setequal(res$highlight, 1:5)             # all 5 highlighted
  expect_setequal(res$labels$row_id, c(1L, 2L, 3L, 5L))  # M1 top3 (1,2,3) + M2 (5)
  expect_equal(res$labels$label[res$labels$row_id == 1L], "GA_aa10")
  expect_equal(res$labels$label[res$labels$row_id == 5L], "GB_aa5")
})

test_that("marker_topn shares a peptide across markers with a ;-joined label", {
  rf <- data.frame(row_id = 1:2, sequence = c("S1","S2"),
                   accessions = "x", genes = "x",
                   display_intensity = c(9, 8), rank = 1:2,
                   stringsAsFactors = FALSE)
  matched <- data.frame(
    .row_id   = c(1L, 1L, 2L),
    accession = c("M1","M2","M2"),
    gene      = c("GA","GB","GB"),
    pep_start = c(11L, 22L, 33L),
    PEP.StrippedSequence = c("S1","S1","S2"),
    check.names = FALSE, stringsAsFactors = FALSE)
  res <- pelsa_splot_marker_topn(matched, c("M1","M2"), rf, n = 3L)
  expect_equal(res$labels$label[res$labels$row_id == 1L], "GA_aa11;GB_aa22")
})

test_that("marker_topn drops NA-in-sample peptides and falls back to accession label", {
  rf <- data.frame(row_id = 1L, sequence = "S1", accessions = "x", genes = "x",
                   display_intensity = 5, rank = 1L, stringsAsFactors = FALSE)
  matched <- data.frame(
    .row_id = c(1L, 2L), accession = c("M1","M1"), gene = c("", ""),
    pep_start = c(7L, 8L),
    PEP.StrippedSequence = c("S1","S2"),
    check.names = FALSE, stringsAsFactors = FALSE)
  res <- pelsa_splot_marker_topn(matched, "M1", rf, n = 3L)
  expect_equal(res$highlight, 1L)                 # row 2 absent from rf (NA)
  expect_equal(res$labels$label, "M1_aa7")        # blank gene -> accession
})

test_that("marker_topn returns empty on no markers / no matches", {
  rf <- data.frame(row_id = 1L, sequence = "S1", accessions = "x", genes = "x",
                   display_intensity = 5, rank = 1L, stringsAsFactors = FALSE)
  empty_m <- pelsa_splot_marker_topn(data.frame(), "M1", rf)
  expect_length(empty_m$highlight, 0L)
  expect_equal(nrow(empty_m$labels), 0L)
})

test_that("tooltip lists accessions, bolds matched keys, caps with always-show", {
  rf <- data.frame(
    row_id = 1L, rank = 1L, display_intensity = 12.345,
    sequence = "PEPTIDE",
    accessions = "P1;Q2;R3",
    genes = "GA;GB;",
    stringsAsFactors = FALSE)
  tip <- pelsa_splot_tooltip(rf, bold_keys = "q2", cap = 8L)
  expect_match(tip, "Rank: #1")
  expect_match(tip, "Intensity: 12.35")            # 2 decimals
  expect_match(tip, "Sequence: PEPTIDE")
  expect_match(tip, "P1 \\(GA\\)")
  expect_match(tip, "<b>Q2 \\(GB\\)</b>")          # bolded
  expect_match(tip, "R3", fixed = FALSE)           # blank gene -> bare accession
})

test_that("tooltip cap keeps bolded accession beyond the cap", {
  accs <- paste0("A", 1:10)
  rf <- data.frame(row_id = 1L, rank = 1L, display_intensity = 1,
                   sequence = "S",
                   accessions = paste(accs, collapse = ";"),
                   genes = paste(rep("g", 10), collapse = ";"),
                   stringsAsFactors = FALSE)
  tip <- pelsa_splot_tooltip(rf, bold_keys = "a10", cap = 3L)
  expect_match(tip, "<b>A10 \\(g\\)</b>")          # bold shown despite cap 3
  expect_match(tip, "\\(\\+[0-9]+ more\\)")
})

test_that("tooltip handles unmapped peptides", {
  rf <- data.frame(row_id = 1L, rank = 2L, display_intensity = 3,
                   sequence = "S", accessions = "", genes = "",
                   stringsAsFactors = FALSE)
  expect_match(pelsa_splot_tooltip(rf, character(0)), "Maps to: \\(unmapped\\)")
})

test_that("prepare bundles background, marker overlay, labels, and title", {
  mat <- matrix(c(50, 40, 30, 20, 10), nrow = 5,
                dimnames = list(paste0("p", 1:5), "S1"))
  pf <- data.frame(
    PEP.StrippedSequence = paste0("S", 1:5),
    PG.ProteinAccessions = c("M1","M1","M1","M2","P9"),
    PG.Genes             = c("GA","GA","GA","GB","G9"),
    stringsAsFactors = FALSE)
  matched <- data.frame(
    .row_id = 1:4, accession = c("M1","M1","M1","M2"),
    gene = c("GA","GA","GA","GB"), pep_start = c(10L,20L,30L,5L),
    PEP.StrippedSequence = paste0("S", 1:4),
    check.names = FALSE, stringsAsFactors = FALSE)
  prep <- pelsa_splot_prepare(
    mat, "S1", pf, matched,
    selected_markers = c("M1","M2"), trypsin_accs = .PELSA_TRYPSIN_ACCESSIONS,
    label_trypsin = FALSE,
    params = list(log_transformation = "log2",
                  data_normalization = "Median (non-zero)"))
  expect_equal(nrow(prep$background), 5L)
  expect_setequal(prep$marker_pts$rank, 1:4)            # M1+M2 peptides
  expect_equal(nrow(prep$trypsin_pts), 0L)
  expect_setequal(prep$marker_labels$label,
                  c("GA_aa10","GA_aa20","GA_aa30","GB_aa5"))
  expect_false(prep$show_trypsin)
  expect_equal(prep$y_title, "log2(intensity), Median (non-zero) normalized")
  # background hovertext bolds the selected markers
  expect_true(any(grepl("<b>M1", prep$background$hovertext)))
})

test_that("prepare yields an empty-but-typed bundle when no finite peptides", {
  mat <- matrix(NA_real_, nrow = 2, dimnames = list(NULL, "S1"))
  pf <- data.frame(PEP.StrippedSequence = c("A","B"),
                   PG.ProteinAccessions = c("M1","M2"),
                   PG.Genes = c("a","b"), stringsAsFactors = FALSE)
  prep <- pelsa_splot_prepare(mat, "S1", pf, data.frame(),
                              "M1", .PELSA_TRYPSIN_ACCESSIONS, FALSE,
                              list(log_transformation = "log2"))
  expect_equal(nrow(prep$background), 0L)
  expect_equal(nrow(prep$marker_labels), 0L)
})
