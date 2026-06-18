library(testthat)

# Volcano-df-shaped frame. Columns the clicked-point trace reads: id, logFC,
# logP, winning_gene, winning_accession, PG.Genes, PG.ProteinAccessions,
# pep_start, pep_end (pep_end for the 6-line hover).
.mk_click_df <- function() {
  data.frame(
    id                   = c("PEPA1", "PEPA2", "PEPB1"),
    logFC                = c(1.5, -0.8, 2.1),
    logP                 = c(3.0, 1.2, 4.4),
    winning_gene         = c("GENEA", "GENEA", ""),       # B blanked (self-curated)
    winning_accession    = c("ACCA", "ACCA", "ACCB"),
    PG.Genes             = c("GENEA", "GENEA", NA_character_),
    PG.ProteinAccessions = c("ACCA", "ACCA", "ACCB"),
    pep_start            = c(101L, 222L, 55L),
    pep_end              = c(110L, 230L, 60L),
    adj.P.Val            = c(0.01, 0.20, 0.001),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
}

test_that("emphasizes the clicked point: gold fill, larger dot, thicker ring", {
  df <- .mk_click_df()
  tr <- pelsa_volcano_clicked_point_trace(
    df, list(row = 1L, peptide_seq = "PEPA1"))
  expect_equal(tr$type, "scattergl")
  expect_equal(tr$mode, "markers")
  expect_equal(tr$meta, "pelsa_gold_click")
  # SAME gold fill as the sibling highlight, but larger + thicker black ring.
  expect_equal(tr$marker$color, .PELSA_GOLD)
  expect_equal(tr$marker$size, .PELSA_CLICK_PT_SIZE)
  expect_equal(tr$marker$line$color, .PELSA_VOLCANO_MARKER_EDGE)
  expect_equal(tr$marker$line$width, .PELSA_CLICK_PT_RING_W)
  # Larger than the gold overlay (size 7) and thicker ring (0.5) so it stands out.
  expect_gt(tr$marker$size, 7)
  expect_gt(tr$marker$line$width, 0.5)
})

test_that("clicked point sits at the clicked row's (logFC, logP)", {
  df <- .mk_click_df()
  tr <- pelsa_volcano_clicked_point_trace(
    df, list(row = 1L, peptide_seq = "PEPA1"))
  expect_equal(tr$x[[1L]], 1.5)
  expect_equal(tr$y[[1L]], 3.0)
})

test_that("single-point x/y serialize to JSON ARRAYS (auto_unbox safe)", {
  # REGRESSION: plotlyProxyInvoke('addTraces', ...) serializes with
  # auto_unbox = TRUE, which collapses a length-1 numeric (5.68) to a JSON
  # scalar. A scattergl trace then reads x[0] as undefined -> NaN pixel -> the
  # point never paints. The fix wraps x/y in list() so even one point emits
  # [5.68], not 5.68. Assert the actual serialized shape.
  df <- .mk_click_df()
  tr <- pelsa_volcano_clicked_point_trace(
    df, list(row = 1L, peptide_seq = "PEPA1"))
  jx <- as.character(jsonlite::toJSON(tr$x, auto_unbox = TRUE))
  jy <- as.character(jsonlite::toJSON(tr$y, auto_unbox = TRUE))
  expect_match(jx, "^\\[")   # array, NOT a bare scalar like "1.5"
  expect_match(jy, "^\\[")
  expect_equal(jx, "[1.5]")
})

test_that("carries the standard 6-line hover for the clicked point", {
  df <- .mk_click_df()
  tr <- pelsa_volcano_clicked_point_trace(
    df, list(row = 3L, peptide_seq = "PEPB1"))
  expect_equal(tr$hoverinfo, "text")
  expect_length(tr$text, 1L)
  expect_true(grepl("Peptide: ", tr$text[[1L]], fixed = TRUE))
  expect_equal(length(gregexpr("<br>", tr$text[[1L]])[[1L]]), 5L)  # 6 lines
})

test_that("resolves the row by peptide_seq when selection$row is NA (Woods)", {
  df <- .mk_click_df()
  tr <- pelsa_volcano_clicked_point_trace(
    df, list(row = NA_integer_, peptide_seq = "PEPA2"))
  expect_equal(tr$x[[1L]], -0.8)
  expect_equal(tr$y[[1L]], 1.2)
})

test_that("returns NULL for no selection, empty df, or unresolvable peptide", {
  df <- .mk_click_df()
  expect_null(pelsa_volcano_clicked_point_trace(df, NULL))
  expect_null(pelsa_volcano_clicked_point_trace(
    df[0, , drop = FALSE], list(row = 1L, peptide_seq = "PEPA1")))
  expect_null(pelsa_volcano_clicked_point_trace(
    df, list(row = NA_integer_, peptide_seq = "NOPE")))
  expect_null(pelsa_volcano_clicked_point_trace(
    df, list(row = NA_integer_, peptide_seq = NA_character_)))
})

test_that("returns NULL when the clicked row has NA coordinates", {
  df <- .mk_click_df()
  df$logP[1L] <- NA_real_
  expect_null(pelsa_volcano_clicked_point_trace(
    df, list(row = 1L, peptide_seq = "PEPA1")))
  df2 <- .mk_click_df()
  df2$logFC[2L] <- NA_real_
  expect_null(pelsa_volcano_clicked_point_trace(
    df2, list(row = 2L, peptide_seq = "PEPA2")))
})

test_that("a single-accession Find selection (origin=find, real row) is emphasized", {
  # A single-accession Find sets origin='find' with a concrete row/peptide_seq
  # (it 'opens' one peptide, like a click), so it SHOULD be emphasized. Only a
  # multi-accession Find sets selection() to NULL (-> NULL, covered above).
  df <- .mk_click_df()
  tr <- pelsa_volcano_clicked_point_trace(
    df, list(origin = "find", row = 3L, peptide_seq = "PEPB1",
             accession = "ACCB"))
  expect_equal(tr$x[[1L]], 2.1)
  expect_equal(tr$marker$color, .PELSA_GOLD)
})
