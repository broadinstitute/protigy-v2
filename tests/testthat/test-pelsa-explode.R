################################################################################
# Tests for pelsa_explode_accessions()  -  the ;-accession explode helper.
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

test_that("an NA gene/position field does not error (regression)", {
  # Regression: readr::read_tsv() reads a missing PG.Genes cell as NA (not ""),
  # and .pelsa_count_slots()'s gregexpr(";", NA) returns NA (not -1), which made
  # the slot-count if() throw "missing value where TRUE/FALSE needed". A real
  # mouse PELSA peptide report (277 NA genes) hit this. The NA field must be
  # treated as a single empty slot -> NA token, exactly like a blank string.
  df <- data.frame(
    PG.ProteinAccessions = c("A;B", "C"),
    PG.Genes = c(NA_character_, "GC"),
    PEP.PeptidePosition = c("10;20", NA_character_),
    PEP.StrippedSequence = c("PEPK", "FOOK"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- expect_no_error(pelsa_explode_accessions(df))
  expect_equal(nrow(exploded), 3L)
  # Row 1's NA gene field -> NA for both of its accessions (single empty slot,
  # NOT recycled to a real value); positions still align 1:1.
  ab <- exploded[exploded$accession %in% c("A", "B"), , drop = FALSE]
  expect_true(all(is.na(ab$gene)))
  expect_setequal(ab$pep_position_token, c("10", "20"))
  # Row 2's NA position field -> NA; its gene is present.
  c_row <- exploded[exploded$accession == "C", , drop = FALSE]
  expect_equal(c_row$gene, "GC")
  expect_true(is.na(c_row$pep_position_token))
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

# ---- M2 regression: interspersed-empty accession token alignment -------------
# An empty MIDDLE accession slot must not shift gene/position onto the wrong
# kept accession. The fix aligns gene/pos against the RAW (pre-prune) accession
# slots, then applies the same drop-empty mask -- so each kept accession keeps
# its own gene + position.

test_that("M2: interspersed empty accession keeps each accession's own gene+pos", {
  # accession="A;;B", gene="GA;GMID;GB", pos="10;99;20"
  # Correct: A->GA/10, B->GB/20 (NOT B->GMID/99, the pre-fix bug).
  df <- data.frame(
    PG.ProteinAccessions = "A;;B",
    PG.Genes = "GA;GMID;GB",
    PEP.PeptidePosition = "10;99;20",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 2L)
  exploded <- exploded[match(c("A", "B"), exploded$accession), , drop = FALSE]
  expect_equal(exploded$gene, c("GA", "GB"))
  expect_equal(exploded$pep_position_token, c("10", "20"))
})

test_that("M2: trailing empty accession is harmless (A;B; -> A,B aligned)", {
  df <- data.frame(
    PG.ProteinAccessions = "A;B;",
    PG.Genes = "GA;GB;GTRAIL",
    PEP.PeptidePosition = "10;20;30",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 2L)
  exploded <- exploded[match(c("A", "B"), exploded$accession), , drop = FALSE]
  expect_equal(exploded$gene, c("GA", "GB"))
  expect_equal(exploded$pep_position_token, c("10", "20"))
})

test_that("M2: leading empty accession keeps later accessions aligned (;A;B)", {
  df <- data.frame(
    PG.ProteinAccessions = ";A;B",
    PG.Genes = "GLEAD;GA;GB",
    PEP.PeptidePosition = "5;10;20",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 2L)
  exploded <- exploded[match(c("A", "B"), exploded$accession), , drop = FALSE]
  expect_equal(exploded$gene, c("GA", "GB"))
  expect_equal(exploded$pep_position_token, c("10", "20"))
})

test_that("M2: no-empty multi-accession row still aligns 1:1 (unchanged)", {
  df <- data.frame(
    PG.ProteinAccessions = "A;B;C",
    PG.Genes = "GA;GB;GC",
    PEP.PeptidePosition = "10;20;30",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 3L)
  exploded <- exploded[match(c("A", "B", "C"), exploded$accession), , drop = FALSE]
  expect_equal(exploded$gene, c("GA", "GB", "GC"))
  expect_equal(exploded$pep_position_token, c("10", "20", "30"))
})

test_that("M2: single accession with interspersed empties unaffected", {
  # all-empty-but-one and a single token: only the real accession survives, with
  # its own gene/position slot.
  df <- data.frame(
    PG.ProteinAccessions = ";B;",
    PG.Genes = "GX;GB;GY",
    PEP.PeptidePosition = "1;20;3",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 1L)
  expect_equal(exploded$accession, "B")
  expect_equal(exploded$gene, "GB")
  expect_equal(exploded$pep_position_token, "20")
})

test_that("M2: interspersed empty with gene/pos shorter than accession count", {
  # accession="A;;B" (3 raw slots, 2 kept), gene/pos have only 2 tokens.
  # Index-pad contract on raw slots: slots 1,2 = tokens; slot 3 (B) = NA.
  df <- data.frame(
    PG.ProteinAccessions = "A;;B",
    PG.Genes = "GA;GB",
    PEP.PeptidePosition = "10;20",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 2L)
  exploded <- exploded[match(c("A", "B"), exploded$accession), , drop = FALSE]
  # A is raw slot 1 -> first token; B is raw slot 3 -> beyond 2 tokens -> NA.
  expect_equal(exploded$gene, c("GA", NA))
  expect_equal(exploded$pep_position_token, c("10", NA))
})

test_that("M2: single gene token recycles across interspersed-empty accessions", {
  # one gene token recycles to ALL raw slots; kept accessions A,B both get it.
  df <- data.frame(
    PG.ProteinAccessions = "A;;B",
    PG.Genes = "GONLY",
    PEP.PeptidePosition = "10;99;20",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 2L)
  exploded <- exploded[match(c("A", "B"), exploded$accession), , drop = FALSE]
  expect_equal(exploded$gene, c("GONLY", "GONLY"))
  # positions still align by raw slot: A->10 (slot1), B->20 (slot3)
  expect_equal(exploded$pep_position_token, c("10", "20"))
})

test_that("hand-built: NA accessions are dropped (not emitted as NA rows)", {
  df <- data.frame(
    PG.ProteinAccessions = c("A;B", NA_character_),
    PG.Genes = c("GA;GB", "GX"),
    PEP.PeptidePosition = c("1;2", "9"),
    PEP.StrippedSequence = c("PEPK", "GHOSTK"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  # Only the A;B row contributes (2 rows); the NA-accession row drops entirely.
  expect_equal(nrow(exploded), 2L)
  expect_setequal(exploded$accession, c("A", "B"))
  expect_false(anyNA(exploded$accession))
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

test_that("trailing-empty gene/position token is NOT recycled to all accessions", {
  # Regression: strsplit() drops trailing empty fields, so "GENE1;" splits to a
  # single token c("GENE1"). The old recycle-on-single-token rule then wrongly
  # assigned GENE1 (and position 10) to BOTH accessions of "P1;P2". The trailing
  # accession P2 has no gene/position of its own and must be NA, NOT GENE1.
  df <- data.frame(
    PG.ProteinAccessions = "P1;P2",
    PG.Genes = "GENE1;",
    PEP.PeptidePosition = "10;",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 2L)
  exploded <- exploded[match(c("P1", "P2"), exploded$accession), , drop = FALSE]
  expect_equal(exploded$gene, c("GENE1", NA))
  expect_equal(exploded$pep_position_token, c("10", NA))
})

test_that("a genuinely single token (no separator) still recycles to all accessions", {
  # The legitimate recycle case must be preserved: "SHARED" has NO ";" separator,
  # so it is one shared value for every accession.
  df <- data.frame(
    PG.ProteinAccessions = "P1;P2;P3",
    PG.Genes = "SHARED",
    PEP.PeptidePosition = "42",
    PEP.StrippedSequence = "PEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_equal(nrow(exploded), 3L)
  expect_true(all(exploded$gene == "SHARED"))
  expect_true(all(exploded$pep_position_token == "42"))
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
