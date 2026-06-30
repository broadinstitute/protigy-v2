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
