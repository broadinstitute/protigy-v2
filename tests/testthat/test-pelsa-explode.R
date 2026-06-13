################################################################################
# Tests for pelsa_explode_accessions() — the ;-accession explode helper.
#
# Explodes a peptide-level frame on its ;-delimited PG.ProteinAccessions into
# one row per (peptide, accession), keeping the ;-aligned PG.Genes and
# PEP.PeptidePosition tokens aligned to each accession.
#
# Parity-gated against the Phase-1 synthetic generator
# (tests/testthat/fixtures/pelsa/generate_synthetic.R). The token-alignment
# rules verified here are the single source of truth for ;-handling in PELSA.
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# Helper: number of non-empty ;-tokens across a character vector of acc strings.
.n_acc_tokens <- function(acc_vec) {
  toks <- strsplit(acc_vec, ";", fixed = TRUE)
  sum(vapply(toks, function(x) sum(nzchar(trimws(x))), integer(1)))
}

test_that("shared peptide explodes to one row per accession with trimmed tokens", {
  syn <- pelsa_make_synthetic(seed = 1)
  df <- syn$peptides

  exploded <- pelsa_explode_accessions(df)

  shared_rows <- exploded[exploded$PEP.StrippedSequence == syn$shared_peptide, ,
                          drop = FALSE]
  expected_acc <- syn$shared_peptide_accessions
  expect_gte(length(expected_acc), 3L)
  expect_equal(nrow(shared_rows), length(expected_acc))
  expect_setequal(shared_rows$accession, expected_acc)
  # tokens trimmed (no leading/trailing whitespace)
  expect_equal(shared_rows$accession, trimws(shared_rows$accession))
})

test_that("a single gene token recycles across many accessions", {
  syn <- pelsa_make_synthetic(seed = 1)
  df <- syn$peptides
  exploded <- pelsa_explode_accessions(df)

  shared_rows <- exploded[exploded$PEP.StrippedSequence == syn$shared_peptide, ,
                          drop = FALSE]
  # SHAREDGENE is a single token for 3 accessions -> recycled to all rows
  gene_tok <- strsplit(
    df$PG.Genes[df$PEP.StrippedSequence == syn$shared_peptide][1], ";",
    fixed = TRUE
  )[[1]]
  expect_length(gene_tok, 1L)
  expect_true(all(shared_rows$gene == gene_tok))
})

test_that(";-aligned multi-gene tokens align 1:1 to accessions", {
  syn <- pelsa_make_synthetic(seed = 1)
  df <- syn$peptides
  exploded <- pelsa_explode_accessions(df)

  # multi-gene row: acc "MULTI1;MULTI2", gene "GENEA;GENEB"
  multi_seq <- syn$multi_gene_peptide
  multi_rows <- exploded[exploded$PEP.StrippedSequence == multi_seq, , drop = FALSE]
  src <- df[df$PEP.StrippedSequence == multi_seq, , drop = FALSE]
  accs <- strsplit(src$PG.ProteinAccessions[1], ";", fixed = TRUE)[[1]]
  genes <- strsplit(src$PG.Genes[1], ";", fixed = TRUE)[[1]]
  expect_length(accs, 2L)
  expect_length(genes, 2L)

  multi_rows <- multi_rows[match(accs, multi_rows$accession), , drop = FALSE]
  expect_equal(multi_rows$gene, genes)
})

test_that("empty gene token becomes NA", {
  syn <- pelsa_make_synthetic(seed = 1)
  df <- syn$peptides
  exploded <- pelsa_explode_accessions(df)

  # no-gene row (acc "NOGENEPROT") has gene ""
  nogene_rows <- exploded[
    exploded$PEP.StrippedSequence == syn$no_gene_peptide, , drop = FALSE
  ]
  expect_gte(nrow(nogene_rows), 1L)
  expect_true(all(is.na(nogene_rows$gene)))
})

test_that("pep_position_token aligns 1:1 to accessions for a multi-accession row", {
  syn <- pelsa_make_synthetic(seed = 1)
  df <- syn$peptides
  exploded <- pelsa_explode_accessions(df)

  multi_seq <- syn$multi_gene_peptide
  src <- df[df$PEP.StrippedSequence == multi_seq, , drop = FALSE]
  accs <- strsplit(src$PG.ProteinAccessions[1], ";", fixed = TRUE)[[1]]
  poss <- strsplit(src$PEP.PeptidePosition[1], ";", fixed = TRUE)[[1]]
  expect_length(poss, length(accs))

  multi_rows <- exploded[exploded$PEP.StrippedSequence == multi_seq, , drop = FALSE]
  multi_rows <- multi_rows[match(accs, multi_rows$accession), , drop = FALSE]
  expect_equal(multi_rows$pep_position_token, poss)
})

test_that("total exploded row count equals sum of non-empty accession tokens", {
  syn <- pelsa_make_synthetic(seed = 1)
  df <- syn$peptides
  exploded <- pelsa_explode_accessions(df)

  expect_equal(nrow(exploded), .n_acc_tokens(df$PG.ProteinAccessions))
})

test_that("single-accession row explodes to exactly one row preserving original columns", {
  syn <- pelsa_make_synthetic(seed = 1)
  df <- syn$peptides
  exploded <- pelsa_explode_accessions(df)

  # DUPPROT is a single-accession row (one accession, ;-separated positions)
  dup_seq <- syn$dup_peptide
  src <- df[df$PEP.StrippedSequence == dup_seq, , drop = FALSE]
  expect_equal(nrow(src), 1L)
  dup_rows <- exploded[exploded$PEP.StrippedSequence == dup_seq, , drop = FALSE]
  expect_equal(nrow(dup_rows), 1L)
  expect_equal(dup_rows$accession, "DUPPROT")

  # original columns survive: stripped sequence + one intensity column
  expect_equal(dup_rows$PEP.StrippedSequence, src$PEP.StrippedSequence)
  intensity_col <- syn$sample_cols[1]
  expect_equal(dup_rows[[intensity_col]], src[[intensity_col]])
})

test_that(".row_id is synthesized and unique-per-original-row when id_col is NULL", {
  syn <- pelsa_make_synthetic(seed = 1)
  df <- syn$peptides
  exploded <- pelsa_explode_accessions(df, id_col = NULL)

  expect_true(".row_id" %in% colnames(exploded))
  # one distinct .row_id per original row that produced >=1 token
  expect_equal(length(unique(exploded$.row_id)), nrow(df))
  # .row_id is the 1-based original row index
  expect_setequal(unique(exploded$.row_id), seq_len(nrow(df)))

  # all rows from the same original row share one .row_id
  shared_rows <- exploded[exploded$PEP.StrippedSequence == syn$shared_peptide, ,
                          drop = FALSE]
  expect_equal(length(unique(shared_rows$.row_id)), 1L)
})

test_that("hand-built alignment cases: recycle single gene, 1:1 positions", {
  df <- data.frame(
    PG.ProteinAccessions = "A;B",
    PG.Genes = "G",
    PEP.PeptidePosition = "10;20",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)

  expect_equal(nrow(exploded), 2L)
  exploded <- exploded[match(c("A", "B"), exploded$accession), , drop = FALSE]
  expect_equal(exploded$accession, c("A", "B"))
  expect_equal(exploded$gene, c("G", "G")) # single gene recycled
  expect_equal(exploded$pep_position_token, c("10", "20")) # 1:1 positions
})

test_that("hand-built: empty middle accession token is dropped (A;;B -> A,B)", {
  df <- data.frame(
    PG.ProteinAccessions = "A;;B",
    PG.Genes = "GA;GX;GB",
    PEP.PeptidePosition = "1;2;3",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 2L)
  expect_setequal(exploded$accession, c("A", "B"))
})

test_that("single position token recycles to many accessions", {
  df <- data.frame(
    PG.ProteinAccessions = "A;B;C",
    PG.Genes = "G1;G2;G3",
    PEP.PeptidePosition = "42",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 3L)
  expect_true(all(exploded$pep_position_token == "42"))
})

test_that("mismatched token counts align by index and fill missing with NA", {
  # 3 accessions, 2 genes (not 1, not 3) -> align by index, 3rd gene = NA
  # 3 accessions, 2 positions -> 3rd position = NA
  df <- data.frame(
    PG.ProteinAccessions = "A;B;C",
    PG.Genes = "GA;GB",
    PEP.PeptidePosition = "10;20",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  exploded <- exploded[match(c("A", "B", "C"), exploded$accession), , drop = FALSE]
  expect_equal(exploded$gene, c("GA", "GB", NA))
  expect_equal(exploded$pep_position_token, c("10", "20", NA))
})

test_that("explicit id_col is used as the stable identifier instead of .row_id", {
  df <- data.frame(
    pep_id = c("p1", "p2"),
    PG.ProteinAccessions = c("A;B", "C"),
    PG.Genes = c("G1;G2", "G3"),
    PEP.PeptidePosition = c("1;2", "9"),
    PEP.StrippedSequence = c("PEPK", "OTHERK"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df, id_col = "pep_id")
  expect_false(".row_id" %in% colnames(exploded))
  expect_true("pep_id" %in% colnames(exploded))
  p1_rows <- exploded[exploded$pep_id == "p1", , drop = FALSE]
  expect_equal(nrow(p1_rows), 2L)
})
