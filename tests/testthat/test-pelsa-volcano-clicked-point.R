library(testthat)

# Volcano-df-shaped frame. Columns the label trace reads: id, logFC, logP,
# winning_gene, winning_accession, PG.Genes, PG.ProteinAccessions, pep_start.
.mk_label_df <- function() {
  data.frame(
    id                   = c("PEPA1", "PEPA2", "PEPB1"),
    logFC                = c(1.5, -0.8, 2.1),
    logP                 = c(3.0, 1.2, 4.4),
    winning_gene         = c("GENEA", "GENEA", ""),       # B blanked (self-curated)
    winning_accession    = c("ACCA", "ACCA", "ACCB"),
    PG.Genes             = c("GENEA", "GENEA", NA_character_),
    PG.ProteinAccessions = c("ACCA", "ACCA", "ACCB"),
    pep_start            = c(101L, 222L, 55L),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
}

test_that("labels the clicked peptide as <gene>_aa<pos> via selection$row", {
  df <- .mk_label_df()
  tr <- pelsa_volcano_clicked_label_trace(
    df, list(row = 1L, peptide_seq = "PEPA1"))
  expect_equal(tr$text, "GENEA_aa101")
  expect_equal(tr$x, 1.5)
  expect_equal(tr$y, 3.0)
  expect_equal(tr$meta, "pelsa_gold_label")
  expect_equal(tr$mode, "text+markers")
  expect_equal(tr$textfont$color, .PELSA_GOLD_DARK)
})

test_that("falls back to accession when gene is blank (self-curated)", {
  df <- .mk_label_df()
  tr <- pelsa_volcano_clicked_label_trace(
    df, list(row = 3L, peptide_seq = "PEPB1"))
  expect_equal(tr$text, "ACCB_aa55")
})

test_that("resolves the row by peptide_seq when selection$row is NA (Woods)", {
  df <- .mk_label_df()
  tr <- pelsa_volcano_clicked_label_trace(
    df, list(row = NA_integer_, peptide_seq = "PEPA2"))
  expect_equal(tr$text, "GENEA_aa222")
  expect_equal(tr$x, -0.8)
})

test_that("returns NULL for no selection, empty df, or unresolvable peptide", {
  df <- .mk_label_df()
  expect_null(pelsa_volcano_clicked_label_trace(df, NULL))
  expect_null(pelsa_volcano_clicked_label_trace(
    df[0, , drop = FALSE], list(row = 1L, peptide_seq = "PEPA1")))
  expect_null(pelsa_volcano_clicked_label_trace(
    df, list(row = NA_integer_, peptide_seq = "NOPE")))
  expect_null(pelsa_volcano_clicked_label_trace(
    df, list(row = NA_integer_, peptide_seq = NA_character_)))
})

test_that("returns NULL when the clicked row has NA coordinates", {
  df <- .mk_label_df()
  df$logP[1L] <- NA_real_
  expect_null(pelsa_volcano_clicked_label_trace(
    df, list(row = 1L, peptide_seq = "PEPA1")))
  df2 <- .mk_label_df()
  df2$logFC[2L] <- NA_real_
  expect_null(pelsa_volcano_clicked_label_trace(
    df2, list(row = 2L, peptide_seq = "PEPA2")))
})

test_that("labels with the stem alone (no _aaNA) when pep_start is unknown", {
  df <- .mk_label_df()
  df$pep_start[1L] <- NA_integer_
  tr <- pelsa_volcano_clicked_label_trace(
    df, list(row = 1L, peptide_seq = "PEPA1"))
  expect_equal(tr$text, "GENEA")
})

test_that("a single-accession Find selection (origin=find, real row) is labeled", {
  # A single-accession Find sets origin='find' with a concrete row/peptide_seq
  # (it 'opens' one peptide, like a click), so it SHOULD be labeled. Only a
  # multi-accession Find sets selection() to NULL (-> NULL, covered above).
  df <- .mk_label_df()
  tr <- pelsa_volcano_clicked_label_trace(
    df, list(origin = "find", row = 3L, peptide_seq = "PEPB1",
             accession = "ACCB"))
  expect_equal(tr$text, "ACCB_aa55")
})

test_that("renders a marker so the white halo shows (mode text+markers)", {
  df <- .mk_label_df()
  tr <- pelsa_volcano_clicked_label_trace(
    df, list(row = 1L, peptide_seq = "PEPA1"))
  expect_equal(tr$mode, "text+markers")
  expect_equal(tr$marker$color, "rgba(255,255,255,0.9)")
})
