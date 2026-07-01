################################################################################
# Consolidated tests for the PELSA peptide analysis pipeline (pure / offline /
# synthetic-fixture-driven helper suites). Merged from nine former files:
#   test-pelsa-explode.R         -> # --- from explode ---
#   test-pelsa-fasta.R           -> # --- from fasta ---
#   test-pelsa-coverage.R        -> # --- from coverage ---
#   test-pelsa-cv.R              -> # --- from cv ---
#   test-pelsa-depth.R           -> # --- from depth ---
#   test-pelsa-rollup.R          -> # --- from rollup ---
#   test-pelsa-peptide.R         -> # --- from peptide ---
#   test-pelsa-thinning.R        -> # --- from thinning ---
#   test-pelsa-quantified-mask.R -> # --- from quantified-mask ---
#
# The peptide chain: explode -> fasta-map -> coverage/cv/depth -> rollup/peptide
# -> thinning, plus the canonical quantified-mask. All assertions and column
# contracts are preserved verbatim from the originals.
#
# COLLISION RESOLUTION: a helper named `.make_exploded` existed in BOTH the
# fasta and rollup origin files with DIFFERENT, incompatible signatures (fasta's
# took a seed and returned list(syn=, exploded=); rollup's took
# peptide_seq/accession/... and returned a data.frame). They were renamed to
# `.fasta_make_exploded` and `.rollup_make_exploded` respectively, with their
# call sites updated within their own origin blocks only.
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# ==============================================================================
# --- from explode ---
# Tests for pelsa_explode_accessions()  -  the ;-accession explode helper.
#
# Explodes a peptide-level frame on its ;-delimited PG.ProteinAccessions into
# one row per (peptide, accession), keeping the ;-aligned PG.Genes and
# PEP.PeptidePosition tokens aligned to each accession.
# Token fields: {accession, gene, pep_position_token, .row_id}
# ==============================================================================

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

test_that("a leading '>' (Spectronaut self-curated FASTA artifact) is stripped from each accession", {
  # Some Spectronaut exports searched against a self-curated FASTA carry the
  # header '>' verbatim into PG.ProteinAccessions, on EVERY ;-token
  # (">WP_001.1;>WP_002.1"). FASTA keys never have the '>', so the raw token
  # would fail to map (accession_absent). The explode boundary must clean it.
  df <- data.frame(
    PG.ProteinAccessions = c(">WP_004291454.1", ">WP_001.1;>WP_002.1"),
    PG.Genes = c("NaN", "NaN"),
    PEP.PeptidePosition = c("1", "1;1"),
    PEP.StrippedSequence = c("MELLTRNNFEGWMQK", "PEPK"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_setequal(
    exploded$accession,
    c("WP_004291454.1", "WP_001.1", "WP_002.1")
  )
  expect_false(any(startsWith(exploded$accession, ">")))
})

test_that("a bare accession (no '>') is unchanged by the strip", {
  df <- data.frame(
    PG.ProteinAccessions = c("P12345", "Q9UBM7;P00533"),
    PG.Genes = c("G1", "G2;G3"),
    PEP.PeptidePosition = c("1", "1;1"),
    PEP.StrippedSequence = c("PEPK", "OTHERK"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  exploded <- pelsa_explode_accessions(df)
  expect_setequal(exploded$accession, c("P12345", "Q9UBM7", "P00533"))
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

test_that("explode aligns PG.ProteinNames tokens to accession slots", {
  df <- data.frame(
    PG.ProteinAccessions = "P1;P2;P3",
    PG.Genes             = "GA;;GC",
    PG.ProteinNames      = "NameA;NameB;",
    PEP.PeptidePosition  = "10;20;30",
    PEP.StrippedSequence = "PEPTIDEK",
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  out <- pelsa_explode_accessions(df)
  expect_equal(out$accession, c("P1", "P2", "P3"))
  # "NameA;NameB;" has 3 slots (trailing ";") but strsplit yields only 2 tokens;
  # P3's slot is beyond available tokens -> NA (same rule as gene/pos).
  expect_equal(out$protein_name, c("NameA", "NameB", NA_character_))
})

test_that("explode yields NA protein_name when PG.ProteinNames column absent", {
  df <- data.frame(
    PG.ProteinAccessions = "P1;P2",
    PG.Genes             = "GA;GB",
    PEP.PeptidePosition  = "10;20",
    PEP.StrippedSequence = "PEPTIDEK",
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  out <- pelsa_explode_accessions(df)
  expect_true("protein_name" %in% colnames(out))
  expect_true(all(is.na(out$protein_name)))
})

test_that("explode drops the protein_name slot of a pruned empty accession", {
  # Empty middle accession "P1;;P3" -> only P1 and P3 survive; their names must
  # stay paired (NameA with P1, NameC with P3), NOT shift onto the dropped slot.
  df <- data.frame(
    PG.ProteinAccessions = "P1;;P3",
    PG.Genes             = "GA;GB;GC",
    PG.ProteinNames      = "NameA;NameB;NameC",
    PEP.PeptidePosition  = "10;20;30",
    PEP.StrippedSequence = "PEPTIDEK",
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  out <- pelsa_explode_accessions(df)
  expect_equal(out$accession, c("P1", "P3"))
  expect_equal(out$protein_name, c("NameA", "NameC"))
})

# ==============================================================================
# --- from fasta ---
# Tests for the PELSA FASTA reader + FASTA-substring peptide position mapping.
#
#   pelsa_read_fasta(path)            -> named list accession -> AA string
#   pelsa_map_peptide_positions(...)  -> list(matched=, unmatched=)
#
# This is the highest parity-risk PELSA helper: it produces the pep_start /
# pep_end coordinates used for every aa<pos> label, sequence coverage, and the
# unmatched QC table. Parity-gated against the Phase-1 synthetic generator
# whose header documents exact ground-truth coordinates for every edge case.
# ==============================================================================

# ---- pelsa_read_fasta --------------------------------------------------------

test_that("pelsa_read_fasta parses sp|...| and bare headers, concatenates seq", {
  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      ">sp|P12345|X_HUMAN Some description here",
      "MKLVste",          # lower-case to verify upper-casing
      "PTIDE",
      ">BARE this is a bare header with no pipes",
      "aaa",
      "BBB"
    ),
    tmp
  )

  fa <- pelsa_read_fasta(tmp)

  expect_true(is.list(fa))
  expect_setequal(names(fa), c("P12345", "BARE"))
  # multi-line concatenated, upper-cased
  expect_equal(fa[["P12345"]], "MKLVSTEPTIDE")
  expect_equal(fa[["BARE"]], "AAABBB")
})

# ---- pelsa_read_fasta_accessions (key-only fast path) ------------------------

test_that("read_fasta_accessions returns the same keys as names(read_fasta)", {
  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(">sp|P12345|X_HUMAN desc", "MKLV", "PTIDE",
      ">tr|A0A123|Y_HUMAN desc", "AAA",
      ">BARE bare header", "BBB"),
    tmp)
  # Parity: the fast path must yield exactly names(pelsa_read_fasta) in uniprot
  # mode (the only mode the refresh path uses).
  expect_identical(
    pelsa_read_fasta_accessions(tmp, mode = "uniprot"),
    names(pelsa_read_fasta(tmp, mode = "uniprot"))
  )
  expect_setequal(pelsa_read_fasta_accessions(tmp, mode = "uniprot"),
                  c("P12345", "A0A123", "BARE"))
})

test_that("read_fasta_accessions honors self_curated first-token keying", {
  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(">CUSTOM|withpipe rest of header", "MKL",
               ">PLAIN second", "AAA"), tmp)
  expect_identical(
    pelsa_read_fasta_accessions(tmp, mode = "self_curated"),
    names(pelsa_read_fasta(tmp, mode = "self_curated"))
  )
  expect_setequal(pelsa_read_fasta_accessions(tmp, mode = "self_curated"),
                  c("CUSTOM|withpipe", "PLAIN"))
})

test_that("read_fasta_accessions de-dups first-wins + errors on missing file", {
  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(">sp|P1|A d", "MK", ">sp|P1|A d", "AA", ">sp|P2|B d", "CC"), tmp)
  expect_identical(pelsa_read_fasta_accessions(tmp, mode = "uniprot"),
                   c("P1", "P2"))  # duplicate collapsed, order preserved
  expect_error(pelsa_read_fasta_accessions(tempfile(fileext = ".fasta")),
               "not found")
})

test_that("pelsa_read_fasta errors on missing or empty file", {
  expect_error(pelsa_read_fasta(tempfile(fileext = ".fasta")))

  empty <- tempfile(fileext = ".fasta")
  on.exit(unlink(empty), add = TRUE)
  file.create(empty)
  expect_error(pelsa_read_fasta(empty))
})

test_that("pelsa_read_fasta warns on duplicated accessions and keeps first-wins", {
  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      ">sp|P12345|X_HUMAN first record",
      "AAAA",
      ">sp|P12345|X_HUMAN duplicate accession, different sequence",
      "CCCC",
      ">sp|Q99999|Y_HUMAN unique",
      "GGGG"
    ),
    tmp
  )

  expect_warning(
    fa <- pelsa_read_fasta(tmp),
    "duplicated accession"
  )
  # first-wins: the first P12345 sequence is the one retained
  expect_equal(fa[["P12345"]], "AAAA")
  expect_equal(fa[["Q99999"]], "GGGG")
  # first-wins is structural: one entry per unique accession
  expect_setequal(names(fa), c("P12345", "Q99999"))
  expect_equal(sum(names(fa) == "P12345"), 1L)
})

test_that("pelsa_read_fasta emits no warning when accessions are unique", {
  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(">sp|P11111|A_HUMAN", "AAAA", ">sp|P22222|B_HUMAN", "CCCC"),
    tmp
  )
  expect_warning(pelsa_read_fasta(tmp), NA)
})

test_that("mode='self_curated' keys on the first whitespace token, never the pipe", {
  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      ">BalskusLab_HoyT_0001 hypothetical protein OS=Hoylesella",
      "MKLV",
      "PTIDE",
      # A self-curated header that *contains* a pipe must still key on the first
      # whitespace token, NOT the pipe field (folder type drives the mode).
      ">contig7|gene42 some annotation here",
      "AAAA"
    ),
    tmp
  )
  out <- pelsa_read_fasta(tmp, mode = "self_curated")
  expect_identical(names(out),
                   c("BalskusLab_HoyT_0001", "contig7|gene42"))
  expect_identical(out[["BalskusLab_HoyT_0001"]], "MKLVPTIDE")
  expect_identical(out[["contig7|gene42"]], "AAAA")
})

test_that("self-curated map: first-token accessions resolve peptide positions e2e", {
  # End-to-end: a self-curated FASTA parsed in self_curated mode feeds the same
  # position-mapping path. The exploded frame keys on the first-token accession.
  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(">BalskusLab_HoyT_0001 hypothetical protein", "MKLVPTIDESEQ"), tmp)
  fasta <- pelsa_read_fasta(tmp, mode = "self_curated")

  exploded <- data.frame(
    PEP.StrippedSequence = "PTIDE",
    accession            = "BalskusLab_HoyT_0001",
    gene                 = NA_character_,
    pep_position_token   = NA_character_,
    stringsAsFactors     = FALSE
  )
  res <- pelsa_map_peptide_positions(exploded, fasta)
  expect_equal(nrow(res$matched), 1L)
  expect_equal(res$matched$pep_start, 5L)   # MKLV|PTIDE -> starts at residue 5
  expect_equal(res$matched$pep_end, 9L)
  expect_equal(nrow(res$unmatched), 0L)
})

test_that("mode='uniprot' (default) is unchanged: sp|...| keys on the pipe field", {
  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(">sp|P12345|X_HUMAN desc", "MKLV"), tmp)
  # Default mode and explicit "uniprot" both take the middle pipe field.
  expect_identical(names(pelsa_read_fasta(tmp)), "P12345")
  expect_identical(names(pelsa_read_fasta(tmp, mode = "uniprot")), "P12345")
})

# ---- pelsa_map_peptide_positions --------------------------------------------

# Build the exploded long frame once per test from the synthetic generator.
# (Renamed from `.make_exploded` to resolve a cross-file collision with the
# rollup-origin helper of the same name but a different signature.)
.fasta_make_exploded <- function(seed = 1) {
  syn <- pelsa_make_synthetic(seed = seed)
  list(
    syn = syn,
    exploded = pelsa_explode_accessions(syn$peptides)
  )
}

test_that("dup peptide emits two matched rows at the known starts", {
  ctx <- .fasta_make_exploded()
  res <- pelsa_map_peptide_positions(ctx$exploded, ctx$syn$fasta)

  dup_rows <- res$matched[
    res$matched$PEP.StrippedSequence == ctx$syn$dup_peptide &
      res$matched$accession == "DUPPROT", , drop = FALSE
  ]
  expect_equal(nrow(dup_rows), 2L)
  dup_rows <- dup_rows[order(dup_rows$pep_start), , drop = FALSE]
  expect_equal(dup_rows$pep_start, ctx$syn$dup_peptide_starts)
  expect_equal(dup_rows$pep_occurrence_idx, c(1L, 2L))
  expect_true(all(dup_rows$n_occurrences == 2L))
  expect_equal(
    dup_rows$pep_end,
    ctx$syn$dup_peptide_starts + nchar(ctx$syn$dup_peptide) - 1L
  )
})

test_that("overlapping repeat captures both occurrences", {
  ctx <- .fasta_make_exploded()
  res <- pelsa_map_peptide_positions(ctx$exploded, ctx$syn$fasta)

  ov_rows <- res$matched[
    res$matched$PEP.StrippedSequence == ctx$syn$overlap_peptide &
      res$matched$accession == ctx$syn$overlap_peptide_accession, ,
    drop = FALSE
  ]
  expect_equal(nrow(ov_rows), 2L)
  expect_equal(sort(ov_rows$pep_start), ctx$syn$overlap_peptide_starts)
})

test_that("absent peptide (FASTA present, peptide missing) -> sequence_not_found", {
  ctx <- .fasta_make_exploded()
  res <- pelsa_map_peptide_positions(ctx$exploded, ctx$syn$fasta)

  # not in matched
  abs_matched <- res$matched[
    res$matched$PEP.StrippedSequence == ctx$syn$absent_peptide, , drop = FALSE
  ]
  expect_equal(nrow(abs_matched), 0L)

  # in unmatched with reason sequence_not_found, carrying the pep_position token
  abs_unmatched <- res$unmatched[
    res$unmatched$peptide_sequence == ctx$syn$absent_peptide &
      res$unmatched$accession == ctx$syn$absent_peptide_accession, ,
    drop = FALSE
  ]
  expect_equal(nrow(abs_unmatched), 1L)
  expect_equal(abs_unmatched$reason, "sequence_not_found")
  # the ;-aligned Spectronaut PEP.PeptidePosition token is carried ("999")
  expect_equal(abs_unmatched$pep_position, "999")
})

test_that("accession absent from map -> accession_absent (hand-built)", {
  # Hand-built exploded df with an accession key not present in the tiny map.
  exploded <- data.frame(
    .row_id = 1L,
    accession = "MISSINGACC",
    gene = "GX",
    pep_position_token = "12",
    PEP.StrippedSequence = "SOMEPEPK",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  fasta_map <- list(OTHER = "MKLVSOMEPEPK")

  res <- pelsa_map_peptide_positions(exploded, fasta_map)

  expect_equal(nrow(res$matched), 0L)
  expect_equal(nrow(res$unmatched), 1L)
  expect_equal(res$unmatched$reason, "accession_absent")
  expect_equal(res$unmatched$accession, "MISSINGACC")
  expect_equal(res$unmatched$pep_position, "12")
})

test_that("ragged map with a zero-length entry before target still resolves", {
  # Guards against unlist()[match()] index drift: unlist() drops character(0),
  # which would shift "C" onto the wrong (or NA) sequence. Name-indexed lookup
  # must resolve "C" to its own sequence and the correct pep_start.
  exploded <- data.frame(
    .row_id = 1L,
    accession = "C",
    gene = "GC",
    pep_position_token = "1",
    PEP.StrippedSequence = "PEPXYZ",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  fasta_map <- list(A = "MKL", B = character(0), C = "QQPEPXYZRR")

  res <- pelsa_map_peptide_positions(exploded, fasta_map)

  expect_equal(nrow(res$unmatched), 0L)
  expect_equal(nrow(res$matched), 1L)
  expect_equal(res$matched$accession, "C")
  # "PEPXYZ" sits at position 3 in "QQPEPXYZRR"
  expect_equal(res$matched$pep_start, 3L)
  expect_equal(res$matched$pep_end, 3L + nchar("PEPXYZ") - 1L)
})

test_that("I->L mismatched peptide is left unmatched (no isobaric retry)", {
  ctx <- .fasta_make_exploded()
  res <- pelsa_map_peptide_positions(ctx$exploded, ctx$syn$fasta)

  # The peptide differs from its FASTA region only by I<->L swaps. We trust the
  # sequence verbatim, so it must NOT be recovered: absent from matched...
  il_matched <- res$matched[
    res$matched$PEP.StrippedSequence == ctx$syn$il_peptide &
      res$matched$accession == ctx$syn$il_peptide_accession, , drop = FALSE
  ]
  expect_equal(nrow(il_matched), 0L)

  # ...and present in unmatched with reason sequence_not_found (it IS a
  # candidate: valid sequence + resolved FASTA, just no exact substring hit).
  il_unmatched <- res$unmatched[
    res$unmatched$peptide_sequence == ctx$syn$il_peptide &
      res$unmatched$accession == ctx$syn$il_peptide_accession, , drop = FALSE
  ]
  expect_equal(nrow(il_unmatched), 1L)
  expect_equal(il_unmatched$reason, "sequence_not_found")
})

test_that("isoform accession resolves via base-key fallback", {
  ctx <- .fasta_make_exploded()
  res <- pelsa_map_peptide_positions(ctx$exploded, ctx$syn$fasta)

  iso_rows <- res$matched[
    res$matched$accession == ctx$syn$isoform_accession, , drop = FALSE
  ]
  expect_gte(nrow(iso_rows), 1L)
  # FASTA P12345 = "MSTART" + "ISOPEPTIDEK" + "END" -> start 7
  expect_true(all(iso_rows$pep_start == 7L))
})

test_that("bad-sequence peptide -> bad_sequence_format and never searched", {
  ctx <- .fasta_make_exploded()
  res <- pelsa_map_peptide_positions(ctx$exploded, ctx$syn$fasta)

  bad_matched <- res$matched[
    res$matched$PEP.StrippedSequence == ctx$syn$bad_seq_peptide, , drop = FALSE
  ]
  expect_equal(nrow(bad_matched), 0L)

  bad_unmatched <- res$unmatched[
    res$unmatched$peptide_sequence == ctx$syn$bad_seq_peptide, , drop = FALSE
  ]
  expect_equal(nrow(bad_unmatched), 1L)
  expect_equal(bad_unmatched$reason, "bad_sequence_format")
})

test_that("matched rows carry through exploded columns", {
  ctx <- .fasta_make_exploded()
  res <- pelsa_map_peptide_positions(ctx$exploded, ctx$syn$fasta)

  expect_true(all(
    c("PEP.StrippedSequence", ".row_id", "accession",
      "pep_start", "pep_end", "pep_occurrence_idx", "n_occurrences")
      %in% colnames(res$matched)
  ))
  # a known matched peptide retains its sequence + accession
  dup_rows <- res$matched[res$matched$accession == "DUPPROT", , drop = FALSE]
  expect_true(all(dup_rows$PEP.StrippedSequence == ctx$syn$dup_peptide))
  expect_true(all(!is.na(dup_rows$.row_id)))
})

test_that("unmatched data.frame has the documented columns", {
  ctx <- .fasta_make_exploded()
  res <- pelsa_map_peptide_positions(ctx$exploded, ctx$syn$fasta)
  expect_true(all(
    c("peptide_sequence", "accession", "gene", "pep_position", "reason")
      %in% colnames(res$unmatched)
  ))
  expect_true(all(res$unmatched$reason %in%
    c("accession_absent", "sequence_not_found", "bad_sequence_format")))
})

# ==============================================================================
# --- from coverage ---
# Tests for PELSA per-protein sequence coverage (interval union).
#
#   pelsa_sequence_coverage(matched_cache, fasta_map, ...) -> data.frame
#     columns: accession, covered_residues, protein_length, coverage,
#              over_length_flag
#
# Coverage = union of a protein's mapped peptide [pep_start, pep_end] spans
# (overlaps counted ONCE, NOT summed) / FASTA length.
# ==============================================================================

# Hand-built matched_cache helper: one row per (accession, span).
.mk_matched <- function(accession, pep_start, pep_end) {
  data.frame(
    accession = accession,
    pep_start = as.integer(pep_start),
    pep_end = as.integer(pep_end),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# ---- closed-form coverage ----------------------------------------------------

test_that("overlapping spans union (counted once, not summed)", {
  # A: len 20, spans [1,10] + [5,15] -> union [1,15] = 15 residues -> 15/20.
  mc <- .mk_matched(c("A", "A"), c(1L, 5L), c(10L, 15L))
  fa <- list(A = strrep("X", 20L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "A", , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_equal(row$covered_residues, 15L)
  expect_equal(row$protein_length, 20L)
  expect_equal(row$coverage, 0.75)
})

test_that("disjoint spans sum (no overlap, no merge of adjacent)", {
  # B: len 10, spans [1,5] + [6,10] -> 5 + 5 = 10 residues (6 > 5, disjoint).
  mc <- .mk_matched(c("B", "B"), c(1L, 6L), c(5L, 10L))
  fa <- list(B = strrep("X", 10L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "B", , drop = FALSE]

  expect_equal(row$covered_residues, 10L)
  expect_equal(row$coverage, 1.0)
})

test_that("touching-overlap spans merge at the shared residue", {
  # C: len 10, spans [1,5] + [5,10] -> overlap at residue 5 -> union [1,10] = 10.
  mc <- .mk_matched(c("C", "C"), c(1L, 5L), c(5L, 10L))
  fa <- list(C = strrep("X", 10L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "C", , drop = FALSE]

  expect_equal(row$covered_residues, 10L)
  expect_equal(row$coverage, 1.0)
})

test_that("single span coverage is span_length / protein_length", {
  # D: len 100, span [10,19] -> 10 residues -> 0.1.
  mc <- .mk_matched("D", 10L, 19L)
  fa <- list(D = strrep("X", 100L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "D", , drop = FALSE]

  expect_equal(row$covered_residues, 10L)
  expect_equal(row$protein_length, 100L)
  expect_equal(row$coverage, 0.1)
})

test_that("a shared peptide contributes its span to EVERY mapped accession", {
  # One peptide span maps to both A and B (two rows, different accession).
  # A: len 20, span [1,10] -> 10/20 = 0.5
  # B: len 40, span [1,10] -> 10/40 = 0.25
  mc <- .mk_matched(c("A", "B"), c(1L, 1L), c(10L, 10L))
  fa <- list(A = strrep("X", 20L), B = strrep("X", 40L))

  res <- pelsa_sequence_coverage(mc, fa)

  expect_equal(res$covered_residues[res$accession == "A"], 10L)
  expect_equal(res$coverage[res$accession == "A"], 0.5)
  expect_equal(res$covered_residues[res$accession == "B"], 10L)
  expect_equal(res$coverage[res$accession == "B"], 0.25)
})

test_that("isoform accession resolves FASTA length via base-key fallback", {
  # P12345-2 keyed under base P12345 (len 50), span [7,16] -> 10/50 = 0.2.
  mc <- .mk_matched("P12345-2", 7L, 16L)
  fa <- list(P12345 = strrep("X", 50L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "P12345-2", , drop = FALSE]

  expect_equal(row$covered_residues, 10L)
  expect_equal(row$protein_length, 50L)
  expect_equal(row$coverage, 0.2)
})

test_that("unresolved accession (no exact or base key) -> NA length and coverage", {
  mc <- .mk_matched("GHOST", 1L, 5L)
  fa <- list(OTHER = strrep("X", 30L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "GHOST", , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_equal(row$covered_residues, 5L) # union still computable
  expect_true(is.na(row$protein_length))
  expect_true(is.na(row$coverage))
})

test_that("one row per distinct accession; multiple accessions handled together", {
  mc <- .mk_matched(
    c("A", "A", "B", "C"),
    c(1L, 5L, 1L, 10L),
    c(10L, 15L, 5L, 19L)
  )
  fa <- list(A = strrep("X", 20L), B = strrep("X", 10L), C = strrep("X", 100L))

  res <- pelsa_sequence_coverage(mc, fa)

  expect_setequal(res$accession, c("A", "B", "C"))
  expect_equal(nrow(res), 3L)
  expect_equal(res$coverage[res$accession == "A"], 0.75)
  expect_equal(res$coverage[res$accession == "B"], 0.5)
  expect_equal(res$coverage[res$accession == "C"], 0.1)
})

test_that("zero-length FASTA -> coverage NA (no divide-by-zero)", {
  mc <- .mk_matched("E", 1L, 1L)
  fa <- list(E = "")

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "E", , drop = FALSE]

  expect_equal(row$protein_length, 0L)
  expect_true(is.na(row$coverage))
  expect_false(row$over_length_flag) # zero-length is NOT an over-length anomaly
})

# ---- over-length anomaly: warn + clamp + flag (soft fail, no abort) ----------

test_that("over-length span clamps to 1.0, warns, and flags (does not error)", {
  # A: span [1,30] but FASTA only 20 long -> union 30 > 20 -> clamp to 20/20=1.0.
  # B: a normal accession alongside it -> unaffected, flag FALSE.
  mc <- .mk_matched(c("A", "B"), c(1L, 1L), c(30L, 5L))
  fa <- list(A = strrep("X", 20L), B = strrep("X", 10L))

  expect_warning(
    res <- pelsa_sequence_coverage(mc, fa),
    "exceed protein length"
  )

  row_a <- res[res$accession == "A", , drop = FALSE]
  expect_equal(row_a$covered_residues, 20L) # clamped to protein_length
  expect_equal(row_a$protein_length, 20L)
  expect_equal(row_a$coverage, 1.0) # clamped, stays in [0,1]
  expect_true(row_a$over_length_flag)

  row_b <- res[res$accession == "B", , drop = FALSE]
  expect_equal(row_b$coverage, 0.5)
  expect_false(row_b$over_length_flag)
})

# ---- boundary validation -----------------------------------------------------

test_that("a direct NA pep_start/pep_end input row triggers the coercion guard", {
  fa <- list(A = strrep("X", 20L))
  expect_error(
    pelsa_sequence_coverage(.mk_matched("A", NA_integer_, 10L), fa),
    "integer-coercible"
  )
  expect_error(
    pelsa_sequence_coverage(.mk_matched("A", 1L, NA_integer_), fa),
    "integer-coercible"
  )
})

test_that("missing required columns error", {
  fa <- list(A = "XXXX")
  expect_error(
    pelsa_sequence_coverage(data.frame(accession = "A", pep_start = 1L), fa),
    "pep_end"
  )
  expect_error(
    pelsa_sequence_coverage(data.frame(foo = 1), fa),
    "accession"
  )
})

test_that("a span with start > end is rejected (fail fast)", {
  mc <- .mk_matched("A", 10L, 5L)
  fa <- list(A = strrep("X", 20L))
  expect_error(pelsa_sequence_coverage(mc, fa), "start")
})

test_that("non-data.frame matched_cache and non-list fasta_map error", {
  expect_error(pelsa_sequence_coverage(list(), list(A = "X")))
  expect_error(
    pelsa_sequence_coverage(.mk_matched("A", 1L, 2L), 42),
    "fasta_map"
  )
})

test_that("empty matched_cache returns a zero-row frame with the right columns", {
  res <- pelsa_sequence_coverage(.mk_matched(character(0), integer(0), integer(0)),
                                 list(A = "XXXX"))
  expect_equal(nrow(res), 0L)
  expect_setequal(
    colnames(res),
    c("accession", "covered_residues", "protein_length", "coverage",
      "over_length_flag")
  )
})

# ---- integration with the REAL explode + FASTA-map helpers -------------------

test_that("integration: real matched cache -> valid coverage in [0,1]", {
  syn <- pelsa_make_synthetic(seed = 1)
  exploded <- pelsa_explode_accessions(syn$peptides)
  mapped <- pelsa_map_peptide_positions(exploded, syn$fasta)

  res <- pelsa_sequence_coverage(mapped$matched, syn$fasta)

  expect_setequal(
    colnames(res),
    c("accession", "covered_residues", "protein_length", "coverage",
      "over_length_flag")
  )
  # one row per distinct matched accession
  expect_setequal(res$accession, unique(mapped$matched$accession))
  expect_false(anyDuplicated(res$accession) > 0L)

  # resolved coverage in [0,1]; covered never exceeds protein length.
  resolved <- res[!is.na(res$coverage), , drop = FALSE]
  expect_true(all(resolved$coverage >= 0 & resolved$coverage <= 1))
  expect_true(all(resolved$covered_residues <= resolved$protein_length))
  # correctly-mapped real data never over-shoots -> no clamping.
  expect_false(any(res$over_length_flag))
})

test_that("integration: shared peptide contributes to ALL its accessions", {
  syn <- pelsa_make_synthetic(seed = 1)
  exploded <- pelsa_explode_accessions(syn$peptides)
  mapped <- pelsa_map_peptide_positions(exploded, syn$fasta)

  res <- pelsa_sequence_coverage(mapped$matched, syn$fasta)

  shared_accs <- syn$shared_peptide_accessions # c("SHARED1","SHARED2","SHARED3")
  expect_true(all(shared_accs %in% res$accession))

  pep_len <- nchar(syn$shared_peptide)
  for (acc in shared_accs) {
    row <- res[res$accession == acc, , drop = FALSE]
    # the shared peptide is the only peptide on these accessions in the synthetic
    # set, so covered_residues == its length.
    expect_equal(row$covered_residues, pep_len)
    expect_equal(
      row$protein_length, as.integer(nchar(syn$fasta[[acc]]))
    )
  }
})

# ==============================================================================
# --- from cv ---
# Tests for the PELSA within-condition CV helper
# (R/tab_pelsa_analysis_helpers.R): pelsa_within_condition_cv().
#
# CLOSED-FORM GROUND TRUTH (these comments ARE the reference):
#
# Tiny RAW (linear, un-logged) matrix, 4 peptide rows x 6 sample cols.
# Two conditions, 3 replicates each:
#   A = {A1, A2, A3}, B = {B1, B2, B3}
#
#        A1   A2   A3    B1   B2   B3
#   r1: 100  200  300    10   20   60
#   r2:  50   NA  100    30   40   20      (A2 is NA -> r2 not complete-case in A)
#   r3:   0    0    0    20   10   10      (A all-zero -> mean 0 in A)
#   r4:  60  100  200    40   30   10
#
# --- CV per (row, condition) on the RAW (NON-normalized) matrix: cv=sd/mean*100 -
#   CV is computed directly on the raw values (no sum-normalization). sd is the
#   SAMPLE sd (ddof = 1), NA ignored.
#
#   r1, condition A (raw = {100, 200, 300}):
#     mean = 200, sample sd = 100  ->  cv = 100/200*100 = 50%  (discriminates
#     non-normalized from sum-normalized; the latter would give ~5.34%)
#   r2, condition B (raw = {30, 40, 20}):
#     mean = 30, sample sd = 10  ->  cv = 10/30*100 = 33.333...%  (CLEAN closed form)
#   r1, condition B (= {10, 20, 60}): mean = 30, sd = sqrt(1400/2) = sqrt(700)
#     cv = sqrt(700)/30*100
#   r2, condition A: only A1, A3 non-NA -> n_nonNA = 2 < 3 ->
#     cv_status = "insufficient_replicates", cv_pct = NA
#   r3, condition A: raw = {0,0,0} -> mean = 0 -> cv_status = "non_finite",
#     cv_pct = NA  (n_nonNA = 3 >= min_nonNA, but result not finite)
# ==============================================================================

# Build the closed-form tiny RAW matrix + condition map used across tests.
.cv_tiny_inputs <- function() {
  mat <- matrix(
    c(
      100, 200, 300,  10, 20, 60,
      50,  NA,  100,  30, 40, 20,
      0,   0,   0,    20, 10, 10,
      60,  100, 200,  40, 30, 10
    ),
    nrow = 4, byrow = TRUE,
    dimnames = list(NULL, c("A1", "A2", "A3", "B1", "B2", "B3"))
  )
  cond <- c(A1 = "A", A2 = "A", A3 = "A", B1 = "B", B2 = "B", B3 = "B")
  list(mat = mat, cond = cond)
}

# --------------------------------------------------------------------------
# pelsa_within_condition_cv
# --------------------------------------------------------------------------

test_that("within-condition CV: clean closed-form row (r2, condition B) == 33.333...%", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)

  r2B <- res[res$row_id == 2L & res$condition == "B", , drop = FALSE]
  expect_equal(nrow(r2B), 1L)
  expect_equal(r2B$cv_status, "ok")
  expect_equal(r2B$n_nonNA, 3L)
  # normalized B == raw B == {30,40,20}: mean 30, sample sd 10 -> 33.3333...%
  expect_equal(r2B$cv_pct, 10 / 30 * 100, tolerance = 1e-8)
})

test_that("within-condition CV: r1 condition B matches closed form sqrt(700)/30*100", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  r1B <- res[res$row_id == 1L & res$condition == "B", , drop = FALSE]
  expect_equal(r1B$cv_status, "ok")
  expect_equal(r1B$cv_pct, sqrt(700) / 30 * 100, tolerance = 1e-8)
})

test_that("within-condition CV: condition A uses NON-normalized (raw-basis) values", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  r1A <- res[res$row_id == 1L & res$condition == "A", , drop = FALSE]
  # r1 condition A raw = {100, 200, 300}: mean 200, sample sd 100 -> cv 50%.
  # If sum-normalization were applied (factors 2, 16/15, 0.64) this would be
  # ~5.34%, so this row discriminates non-normalized from sum-normalized CV.
  expect_equal(r1A$cv_status, "ok")
  expect_equal(r1A$cv_pct, 100 / 200 * 100, tolerance = 1e-8)
})

test_that("within-condition CV: insufficient replicates -> status + NA cv", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  # r2 in condition A has only A1, A3 non-NA -> n_nonNA = 2 < 3
  r2A <- res[res$row_id == 2L & res$condition == "A", , drop = FALSE]
  expect_equal(nrow(r2A), 1L)
  expect_equal(r2A$n_nonNA, 2L)
  expect_equal(r2A$cv_status, "insufficient_replicates")
  expect_true(is.na(r2A$cv_pct))
})

test_that("within-condition CV: zero/non-finite mean -> non_finite + NA cv", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  # r3 in condition A: normalized {0,0,0} -> mean 0 -> non_finite (n_nonNA = 3)
  r3A <- res[res$row_id == 3L & res$condition == "A", , drop = FALSE]
  expect_equal(nrow(r3A), 1L)
  expect_equal(r3A$n_nonNA, 3L)
  expect_equal(r3A$cv_status, "non_finite")
  expect_true(is.na(r3A$cv_pct))
})

test_that("within-condition CV: n_nonNA counts non-NA replicates incl. NA holes", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  # All B cells non-NA -> n_nonNA == 3 for every row in B.
  expect_true(all(res$n_nonNA[res$condition == "B"] == 3L))
  # Condition A: r2 has one NA -> n_nonNA 2; others 3.
  nA <- res$n_nonNA[res$condition == "A"][order(res$row_id[res$condition == "A"])]
  expect_equal(nA, c(3L, 2L, 3L, 3L))
})

test_that("within-condition CV: tidy long shape is one row per (peptide, condition)", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  expect_s3_class(res, "data.frame")
  expect_setequal(colnames(res),
                  c("row_id", "condition", "cv_pct", "n_nonNA", "cv_status"))
  # 4 peptide rows x 2 conditions = 8 long rows.
  expect_equal(nrow(res), 4L * 2L)
  expect_setequal(unique(res$row_id), 1:4)
  expect_setequal(unique(res$condition), c("A", "B"))
})

test_that("within-condition CV: min_nonNA boundary and validation", {
  io <- .cv_tiny_inputs()
  # min_nonNA = 2 -> r2 condition A (n=2) now becomes ok (mean of {100,50} finite).
  res2 <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 2L)
  r2A <- res2[res2$row_id == 2L & res2$condition == "A", , drop = FALSE]
  expect_equal(r2A$cv_status, "ok")
  expect_false(is.na(r2A$cv_pct))

  expect_error(pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 0L))
})

test_that("within-condition CV: condition A uses raw-basis CVs (no normalization)", {
  # Condition A: EVERY row carries an NA hole somewhere in A1/A2/A3. CV is
  # computed directly on the raw (non-normalized) values, so the CVs for A must
  # equal the raw-basis CVs (sd/mean*100 on the un-normalized values), NOT an
  # error and NOT NA.
  mat <- matrix(
    c(
      # A1  A2  A3    B1  B2  B3
      NA,  20, 30,   10, 20, 60,
      40,  NA, 60,   30, 40, 20,
      70,  80, NA,   20, 10, 10
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(NULL, c("A1", "A2", "A3", "B1", "B2", "B3"))
  )
  cond <- c(A1 = "A", A2 = "A", A3 = "A", B1 = "B", B2 = "B", B3 = "B")

  # CVs for A equal raw-basis CVs (sd/mean*100 on un-normalized values).
  res <- pelsa_within_condition_cv(mat, cond, min_nonNA = 2L)
  resA <- res[res$condition == "A", , drop = FALSE]
  resA <- resA[order(resA$row_id), , drop = FALSE]
  raw_cv <- function(x) {
    x <- x[!is.na(x)]
    stats::sd(x) / mean(x) * 100
  }
  expected <- vapply(seq_len(nrow(mat)), function(r) {
    raw_cv(mat[r, c("A1", "A2", "A3")])
  }, numeric(1))
  expect_equal(resA$cv_status, rep("ok", 3L))
  expect_false(anyNA(resA$cv_pct))
  expect_equal(resA$cv_pct, expected, tolerance = 1e-8)
})

test_that("single-replicate condition -> sd undefined -> non_finite + NA cv", {
  # A real upload can have a singleton condition (one replicate column).
  mat <- matrix(
    c(
      100,  10, 20,
      200,  30, 40
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(NULL, c("S1", "B1", "B2"))
  )
  cond <- c(S1 = "S", B1 = "B", B2 = "B")
  res <- pelsa_within_condition_cv(mat, cond, min_nonNA = 1L)

  resS <- res[res$condition == "S", , drop = FALSE]
  expect_equal(nrow(resS), 2L)
  expect_equal(resS$n_nonNA, c(1L, 1L))      # one replicate each
  expect_equal(resS$cv_status, c("non_finite", "non_finite")) # sd undefined
  expect_true(all(is.na(resS$cv_pct)))
})

# --------------------------------------------------------------------------
# Smoke / shape test against the shared synthetic generator
# --------------------------------------------------------------------------

test_that("within-condition CV runs on synthetic frame with correct shape", {
  syn <- pelsa_make_synthetic(seed = 1)
  raw <- as.matrix(syn$peptides[, syn$sample_cols])
  res <- pelsa_within_condition_cv(raw, syn$condition_map, min_nonNA = 3L)

  n_cond <- length(unique(syn$condition_map))
  expect_equal(nrow(res), nrow(raw) * n_cond)
  expect_setequal(unique(res$condition), unique(unname(syn$condition_map)))

  # The generator forces the FIRST data row to have <3 non-NA in LowN.
  low_n_row1 <- res[res$row_id == 1L & res$condition == syn$low_n_condition, ,
                    drop = FALSE]
  expect_equal(nrow(low_n_row1), 1L)
  expect_equal(low_n_row1$cv_status, "insufficient_replicates")
  expect_true(is.na(low_n_row1$cv_pct))
})

# ==============================================================================
# --- from depth ---
# Tests for the PELSA per-sample quantified-peptide depth helpers
# (R/tab_pelsa_analysis_helpers.R): pelsa_peptides_per_sample() and
# pelsa_depth_summary().
#   depth summary columns: {mean_n, median_n, cv_pct, total_n_peptides}
#
# CLOSED-FORM GROUND TRUTH (these comments ARE the reference):
#
# A peptide is "quantified" for a sample iff its value is FINITE AND NON-ZERO --
# the canonical pelsa_quantified_mask (is.finite(x) & x != 0).
#
# Tiny PROCESSED matrix, 5 peptide rows x 3 sample cols:
#        S1     S2     S3
#   r1:   2.0    1.5    NA
#   r2:   0.0   -1.0    3.0
#   r3:   5.0    NA     0.0
#   r4:  -2.0    4.0    1.0
#   r5:   Inf    2.0   -0.5
# Per-sample quantified counts (finite & != 0): c(S1 = 3L, S2 = 4L, S3 = 3L)
#
# --- pelsa_depth_summary over a known count vector c(100, 120, 80) -----------
#   mean_n=100, median_n=100, sd=20, cv_pct=20 (PLAIN linear CV of COUNTS).
# ==============================================================================

# Build the closed-form tiny PROCESSED (log2) matrix used across tests.
.depth_tiny_mat <- function() {
  matrix(
    c(
      2.0,  1.5,  NA,
      0.0, -1.0,  3.0,
      5.0,  NA,   0.0,
      -2.0, 4.0,  1.0,
      Inf,  2.0, -0.5
    ),
    nrow = 5, byrow = TRUE,
    dimnames = list(NULL, c("S1", "S2", "S3"))
  )
}

# --------------------------------------------------------------------------
# pelsa_peptides_per_sample
# --------------------------------------------------------------------------

test_that("pelsa_peptides_per_sample counts finite & !=0 per sample (closed form)", {
  mat <- .depth_tiny_mat()
  got <- pelsa_peptides_per_sample(mat)

  exp <- c(S1 = 3L, S2 = 4L, S3 = 3L)
  expect_equal(got, exp)
  expect_type(got, "integer")
  expect_equal(names(got), c("S1", "S2", "S3"))
})

test_that("pelsa_peptides_per_sample: NA/0/Inf NOT quantified, negatives ARE", {
  # Each column isolates one rule of the finite & non-zero mask:
  #   AllNA  -> 0 (not finite)
  #   AllZero-> 0 (exact 0 = absent)
  #   AllInf -> 0 (Inf/-Inf/NaN not finite)
  #   Negs   -> nrow (negative log/normalized values are REAL measurements)
  #   Pos    -> nrow
  mat <- matrix(
    c(
      NA,  0.0,  Inf, -1.0,  1.0,
      NA,  0.0, -Inf, -3.0,  2.0,
      NA,  0.0,  NaN, -0.5,  4.0
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(NULL, c("AllNA", "AllZero", "AllInf", "Negs", "Pos"))
  )
  got <- pelsa_peptides_per_sample(mat)
  expect_equal(got,
               c(AllNA = 0L, AllZero = 0L, AllInf = 0L, Negs = 3L, Pos = 3L))
})

test_that("pelsa_quantified_mask: finite & non-zero, identical to >0 on linear", {
  # Negatives kept (real on log scale); NA/NaN/Inf/0 dropped.
  v <- c(2, -2, 0, NA, NaN, Inf, -Inf, 0.0001, -0.0001)
  expect_equal(pelsa_quantified_mask(v),
               c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))
  # On strictly-positive LINEAR data, != 0 and > 0 agree exactly (no change).
  lin <- matrix(c(120, 0, 5, 8000, 0.4, 40), nrow = 3,
                dimnames = list(NULL, c("A", "B")))
  expect_equal(pelsa_quantified_mask(lin), is.finite(lin) & lin > 0)
})

test_that("pelsa_peptides_per_sample coerces a data.frame to matrix", {
  mat <- .depth_tiny_mat()
  df <- as.data.frame(mat, check.names = FALSE)
  got <- pelsa_peptides_per_sample(df)
  expect_equal(got, pelsa_peptides_per_sample(mat))
})

test_that("pelsa_peptides_per_sample validates inputs and fails fast", {
  mat <- .depth_tiny_mat()
  # character matrix -> not numeric
  bad <- matrix(as.character(mat), nrow = 5, dimnames = dimnames(mat))
  expect_error(pelsa_peptides_per_sample(bad))
  # matrix without column names
  no_names <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_error(pelsa_peptides_per_sample(no_names))
})

test_that("pelsa_peptides_per_sample errors on duplicate column (sample) names", {
  # Duplicate sample names make the named-integer return ambiguous for
  # downstream counts["S1"] selection -> fail fast.
  dup <- matrix(
    c(1.0, 2.0, 3.0, 4.0),
    nrow = 2,
    dimnames = list(NULL, c("S1", "S1"))
  )
  expect_error(pelsa_peptides_per_sample(dup))
})

# --------------------------------------------------------------------------
# pelsa_depth_summary
# --------------------------------------------------------------------------

test_that("pelsa_depth_summary computes mean/median/cv of COUNTS (closed form)", {
  n <- c(100L, 120L, 80L)
  res <- pelsa_depth_summary(n)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_setequal(colnames(res),
                  c("mean_n", "median_n", "cv_pct", "total_n_peptides"))
  expect_equal(res$mean_n, 100, tolerance = 1e-8)
  expect_equal(res$median_n, 100, tolerance = 1e-8)
  # cv = sample sd (ddof=1) / mean * 100 = 20 / 100 * 100 = 20
  expect_equal(res$cv_pct, stats::sd(n) / mean(n) * 100, tolerance = 1e-8)
  expect_equal(res$cv_pct, 20, tolerance = 1e-8)
  # not supplied -> NA_integer_
  expect_true(is.na(res$total_n_peptides))
})

test_that("pelsa_depth_summary carries total_n_peptides through when supplied", {
  n <- c(100L, 120L, 80L)
  res <- pelsa_depth_summary(n, total_n_peptides = 500L)
  expect_equal(res$total_n_peptides, 500L)
  # other stats unchanged
  expect_equal(res$mean_n, 100, tolerance = 1e-8)
})

test_that("pelsa_depth_summary coerces a double total_n_peptides to integer", {
  # Caller may pass a double (500) instead of 500L; the output column type
  # must be stable (integer) either way.
  res_dbl <- pelsa_depth_summary(c(100L, 120L, 80L), total_n_peptides = 500)
  expect_type(res_dbl$total_n_peptides, "integer")
  expect_equal(res_dbl$total_n_peptides, 500L)
})

test_that("pelsa_depth_summary: an NA element propagates to NA stats (no na.rm)", {
  # pelsa_peptides_per_sample() can never emit NA, so an NA here signals a
  # caller bug; we pin the propagate (not na.rm) behavior.
  res <- pelsa_depth_summary(c(100, NA, 80))
  expect_true(is.na(res$mean_n))
  expect_true(is.na(res$median_n))
  expect_true(is.na(res$cv_pct))
})

test_that("pelsa_depth_summary integrates with pelsa_peptides_per_sample output", {
  mat <- .depth_tiny_mat()
  n <- pelsa_peptides_per_sample(mat) # c(S1=3, S2=4, S3=3)
  res <- pelsa_depth_summary(n, total_n_peptides = nrow(mat))

  expect_equal(res$mean_n, mean(c(3, 4, 3)), tolerance = 1e-8)
  expect_equal(res$median_n, stats::median(c(3, 4, 3)), tolerance = 1e-8)
  expect_equal(res$cv_pct, stats::sd(c(3, 4, 3)) / mean(c(3, 4, 3)) * 100,
               tolerance = 1e-8)
  expect_equal(res$total_n_peptides, 5L)
})

test_that("pelsa_depth_summary: empty vector -> NA stats", {
  res <- pelsa_depth_summary(integer(0))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$mean_n))
  expect_true(is.na(res$median_n))
  expect_true(is.na(res$cv_pct))
  expect_true(is.na(res$total_n_peptides))
})

test_that("pelsa_depth_summary: single sample -> cv_pct NA (sd of one value)", {
  res <- pelsa_depth_summary(c(only = 42L))
  expect_equal(res$mean_n, 42, tolerance = 1e-8)
  expect_equal(res$median_n, 42, tolerance = 1e-8)
  expect_true(is.na(res$cv_pct)) # sample sd of a single value is NA
})

# --------------------------------------------------------------------------
# Smoke / shape test against the shared synthetic generator
# --------------------------------------------------------------------------

test_that("depth helpers run on synthetic frame with correct shape/names/types", {
  syn <- pelsa_make_synthetic(seed = 1)
  # Treat the intensity block as the processed-like matrix (mask still applies).
  mat <- as.matrix(syn$peptides[, syn$sample_cols])

  n <- pelsa_peptides_per_sample(mat)
  expect_type(n, "integer")
  expect_equal(length(n), length(syn$sample_cols))
  expect_equal(names(n), syn$sample_cols)
  # Counts are bounded by the number of peptide rows.
  expect_true(all(n >= 0L & n <= nrow(mat)))

  res <- pelsa_depth_summary(n, total_n_peptides = nrow(mat))
  expect_s3_class(res, "data.frame")
  expect_setequal(colnames(res),
                  c("mean_n", "median_n", "cv_pct", "total_n_peptides"))
  expect_equal(res$total_n_peptides, nrow(mat))
  expect_false(is.na(res$cv_pct)) # >1 sample -> finite CV
})

# ==============================================================================
# --- from rollup ---
# Tests for pelsa_best_peptide_rollup()  -  best-peptide-per-protein rollup.
#   rollup output columns: {peptide_seq, adj_p, logFC, label, won_accessions,
#                           n_won}
#
# Two-step logic (Protigy refinement over the notebook's per-accession dots):
#   Step 1 (notebook _rollup_to_proteins): per accession, keep the FIRST row
#     after a STABLE sort on [adj.P.Val, logFC, peptide_seq, accession].
#   Step 2 (regroup by peptide): a peptide that wins multiple accessions becomes
#     ONE dot, not several overlapping ones, carrying a ;-joined multi-label.
# ==============================================================================

# Convenience constructor for a hand-built exploded stat frame. Columns match
# the rollup defaults (adj.P.Val, logFC, PEP.StrippedSequence, accession, gene,
# pep_start).
# (Renamed from `.make_exploded` to resolve a cross-file collision with the
# fasta-origin helper of the same name but a different signature.)
.rollup_make_exploded <- function(peptide_seq, accession, gene, pep_start,
                                  adj.P.Val, logFC) {
  data.frame(
    PEP.StrippedSequence = peptide_seq,
    accession = accession,
    gene = gene,
    pep_start = pep_start,
    adj.P.Val = adj.P.Val,
    logFC = logFC,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# --- Simple: one best peptide per accession, no sharing -----------------------

test_that("simple case picks smallest-adjp peptide per accession, one dot each", {
  # A: P1(.01) beats P2(.05); B: P2(.05) beats P3(.20).
  df <- .rollup_make_exploded(
    peptide_seq = c("P1", "P2", "P2", "P3"),
    accession   = c("A",  "A",  "B",  "B"),
    gene        = c("GA", "GA", "GB", "GB"),
    pep_start   = c(10L,  20L,  30L,  40L),
    adj.P.Val   = c(.01,  .05,  .05,  .20),
    logFC       = c(-1,   -2,   -3,   -4)
  )

  out <- pelsa_best_peptide_rollup(df)

  # One row per distinct best-peptide: P1 (won A), P2 (won B).
  expect_equal(nrow(out), 2L)
  expect_setequal(out$peptide_seq, c("P1", "P2"))

  p1 <- out[out$peptide_seq == "P1", ]
  expect_equal(p1$adj_p, .01)
  expect_equal(p1$logFC, -1)
  expect_equal(p1$label, "GA_aa10")
  expect_equal(p1$won_accessions, "A")
  expect_equal(p1$n_won, 1L)

  p2 <- out[out$peptide_seq == "P2", ]
  # P2 wins B (adjp .05 vs P3 .20). Its coordinate is the B row's stats.
  expect_equal(p2$adj_p, .05)
  expect_equal(p2$logFC, -3)
  expect_equal(p2$label, "GB_aa30")
  expect_equal(p2$won_accessions, "B")
  expect_equal(p2$n_won, 1L)
})

# --- Shared best peptide: ONE dot, multi-label (the key refinement) -----------

test_that("a peptide best for multiple accessions yields ONE multi-labeled dot", {
  # X is the best peptide for BOTH A (aa120, GA) and B (aa88, GB).
  df <- .rollup_make_exploded(
    peptide_seq = c("X",   "Y",   "X",   "Z"),
    accession   = c("A",   "A",   "B",   "B"),
    gene        = c("GA",  "GA",  "GB",  "GB"),
    pep_start   = c(120L,  200L,  88L,   300L),
    adj.P.Val   = c(.001,  .5,    .002,  .5),
    logFC       = c(-1,    -1,    -2,    -1)
  )

  out <- pelsa_best_peptide_rollup(df)

  # X is best for A and B -> ONE row, not two.
  x <- out[out$peptide_seq == "X", ]
  expect_equal(nrow(x), 1L)
  # LABEL entries ordered by (pep_start, accession) to match the all_peptide
  # panel: B@aa88 precedes A@aa120 (88 < 120), regardless of accession alpha order.
  expect_equal(x$label, "GB_aa88;GA_aa120")
  # won_accessions stays winner-ordered (A's adj.P .001 < B's .002 -> A first).
  expect_equal(x$won_accessions, "A;B")
  expect_equal(x$n_won, 2L)
  # The dot's single coordinate is the peptide's stats (same row everywhere).
  expect_equal(x$adj_p, .001)
})

# --- logFC tiebreak: most-negative logFC wins ---------------------------------

test_that("on equal adjp the most-negative logFC peptide wins for an accession", {
  # Both peptides have adjp .05 for A; P_neg has logFC -3 (more negative) so it
  # sorts first ascending and wins.
  df <- .rollup_make_exploded(
    peptide_seq = c("P_pos", "P_neg"),
    accession   = c("A",     "A"),
    gene        = c("GA",    "GA"),
    pep_start   = c(10L,     20L),
    adj.P.Val   = c(.05,     .05),
    logFC       = c(1.0,     -3.0)
  )

  out <- pelsa_best_peptide_rollup(df)

  expect_equal(nrow(out), 1L)
  expect_equal(out$peptide_seq, "P_neg")
  expect_equal(out$logFC, -3.0)
})

# --- Exact tie (adjp AND logFC identical): deterministic total-order pick ------

test_that("exact (adjp,logFC) tie is broken deterministically by peptide_seq", {
  # Two peptides, same accession, identical (adjp, logFC). The total-order
  # tiebreak (peptide_seq ascending) must pick the lexicographically smaller.
  df <- .rollup_make_exploded(
    peptide_seq = c("TIEPEPTWOK", "TIEPEPONEK"),  # ONE < TWO lexicographically
    accession   = c("TIEPROT",    "TIEPROT"),
    gene        = c("TIEGENE",    "TIEGENE"),
    pep_start   = c(15L,          3L),
    adj.P.Val   = c(.042,         .042),
    logFC       = c(1.2345,       1.2345)
  )

  out <- pelsa_best_peptide_rollup(df)

  expect_equal(nrow(out), 1L)
  # "TIEPEPONEK" < "TIEPEPTWOK" -> the smaller seq wins via the total order.
  expect_equal(out$peptide_seq, "TIEPEPONEK")
})

test_that("exact-tie rollup is reproducible run-to-run (identical output)", {
  df <- .rollup_make_exploded(
    peptide_seq = c("TIEPEPTWOK", "TIEPEPONEK"),
    accession   = c("TIEPROT",    "TIEPROT"),
    gene        = c("TIEGENE",    "TIEGENE"),
    pep_start   = c(15L,          3L),
    adj.P.Val   = c(.042,         .042),
    logFC       = c(1.2345,       1.2345)
  )

  out1 <- pelsa_best_peptide_rollup(df)
  out2 <- pelsa_best_peptide_rollup(df)
  expect_identical(out1, out2)
})

# --- NA stats: sort last; keep an accession's sole all-NA peptide -------------

test_that("a peptide with a real adjp beats an NA-adjp peptide for an accession", {
  df <- .rollup_make_exploded(
    peptide_seq = c("P_na", "P_real"),
    accession   = c("A",    "A"),
    gene        = c("GA",   "GA"),
    pep_start   = c(10L,    20L),
    adj.P.Val   = c(NA_real_, .30),
    logFC       = c(-5,     -1)
  )

  out <- pelsa_best_peptide_rollup(df)

  expect_equal(nrow(out), 1L)
  expect_equal(out$peptide_seq, "P_real")
  expect_equal(out$adj_p, .30)
})

test_that("an accession whose only peptide has NA stats keeps that peptide", {
  df <- .rollup_make_exploded(
    peptide_seq = "P_only",
    accession   = "A",
    gene        = "GA",
    pep_start   = 10L,
    adj.P.Val   = NA_real_,
    logFC       = NA_real_
  )

  out <- pelsa_best_peptide_rollup(df)

  expect_equal(nrow(out), 1L)
  expect_equal(out$peptide_seq, "P_only")
  expect_true(is.na(out$adj_p))
  expect_true(is.na(out$logFC))
  expect_equal(out$label, "GA_aa10")
})

# --- Multi-label collapses identical (gene, pos) ------------------------------

test_that("identical (gene,pos) across won accessions collapse to one label entry", {
  # X wins A and B, both gene GA at the SAME position 120 -> one label entry.
  df <- .rollup_make_exploded(
    peptide_seq = c("X",  "X"),
    accession   = c("A",  "B"),
    gene        = c("GA", "GA"),
    pep_start   = c(120L, 120L),
    adj.P.Val   = c(.01,  .01),
    logFC       = c(-1,   -1)
  )

  out <- pelsa_best_peptide_rollup(df)

  x <- out[out$peptide_seq == "X", ]
  expect_equal(nrow(x), 1L)
  expect_equal(x$label, "GA_aa120")           # collapsed
  expect_equal(x$won_accessions, "A;B")        # both still traced
  expect_equal(x$n_won, 2L)
})

# --- Duplicate (peptide, accession) rows must NOT inflate n_won ---------------

test_that("duplicate identical (peptide, accession) rows count the accession once", {
  # The same (peptide_seq, accession) pair appears twice (e.g. two occurrence
  # rows from the FASTA mapper). The step-1 .SD[1L]-by-accession collapse keeps
  # ONE winner for accession A, so n_won counts A exactly once -- not twice.
  df <- .rollup_make_exploded(
    peptide_seq = c("P1", "P1", "P2"),
    accession   = c("A",  "A",  "A"),   # P1 duplicated for the SAME accession
    gene        = c("GA", "GA", "GA"),
    pep_start   = c(10L,  10L,  20L),
    adj.P.Val   = c(.01,  .01,  .50),
    logFC       = c(-1,   -1,   -1)
  )

  out <- pelsa_best_peptide_rollup(df)

  # P1 is best for A; the duplicate row does not create a second win.
  expect_equal(nrow(out), 1L)
  expect_equal(out$peptide_seq, "P1")
  expect_equal(out$won_accessions, "A")
  expect_equal(out$n_won, 1L)
  expect_equal(out$label, "GA_aa10")
})

# --- Join-order determinism under shuffled input ------------------------------

test_that("won_accessions and label entry order are deterministic under shuffle", {
  # Peptide X wins three accessions A, B, C with IDENTICAL (adjp, logFC), so the
  # accession tiebreak (4th sort key) orders the winners A, B, C regardless of
  # input row order. Feed the rows shuffled (C, A, B) and assert the canonical
  # "A;B;C" join order + correspondingly ordered label entries.
  df <- .rollup_make_exploded(
    peptide_seq = c("X",   "X",   "X"),
    accession   = c("C",   "A",   "B"),   # shuffled input order
    gene        = c("GC",  "GA",  "GB"),
    pep_start   = c(30L,   10L,   20L),
    adj.P.Val   = c(.01,   .01,   .01),
    logFC       = c(-1,    -1,    -1)
  )

  out <- pelsa_best_peptide_rollup(df)

  expect_equal(nrow(out), 1L)
  expect_equal(out$won_accessions, "A;B;C")
  expect_equal(out$label, "GA_aa10;GB_aa20;GC_aa30")
  expect_equal(out$n_won, 3L)
})

# --- Boundary validation -------------------------------------------------------

test_that("missing required columns fail fast", {
  df <- .rollup_make_exploded("P1", "A", "GA", 10L, .01, -1)
  df$adj.P.Val <- NULL
  expect_error(pelsa_best_peptide_rollup(df))
})

test_that("non-data.frame input errors", {
  expect_error(pelsa_best_peptide_rollup(list(a = 1)))
})

# --- Integration: generator -> explode -> FASTA-map -> rollup -----------------

test_that("integration: shared best peptide collapses to one dot end-to-end", {
  syn <- pelsa_make_synthetic(seed = 1)
  exploded <- pelsa_explode_accessions(syn$peptides)
  mapped <- pelsa_map_peptide_positions(exploded, syn$fasta)$matched

  # Attach the contrast's stat columns under the rollup's default names.
  contrast <- syn$contrasts[1]
  mapped$adj.P.Val <- mapped[[paste0("adj.P.Val.", contrast)]]
  mapped$logFC <- mapped[[paste0("logFC.", contrast)]]

  out <- pelsa_best_peptide_rollup(mapped)

  # Output is one row per distinct best-peptide.
  expect_true(is.data.frame(out))
  expect_true(all(c("peptide_seq", "adj_p", "logFC", "label",
                    "won_accessions", "n_won") %in% colnames(out)))
  expect_equal(anyDuplicated(out$peptide_seq), 0L)  # each peptide once

  # The shared peptide maps to 3 accessions (SHARED1/2/3). If it is the best
  # peptide for more than one of them, it must appear as exactly ONE row whose
  # n_won matches how many of those accessions it won.
  shared_rows <- mapped[mapped$PEP.StrippedSequence == syn$shared_peptide, ,
                        drop = FALSE]
  shared_out <- out[out$peptide_seq == syn$shared_peptide, , drop = FALSE]
  if (nrow(shared_out) > 0L) {
    expect_equal(nrow(shared_out), 1L)             # one dot, never overlapping
    expect_gte(shared_out$n_won, 1L)
    # won_accessions are a subset of the shared peptide's mapped accessions.
    won <- strsplit(shared_out$won_accessions, ";", fixed = TRUE)[[1]]
    expect_true(all(won %in% shared_rows$accession))
  }
})

test_that("integration: tie peptides resolve deterministically per accession", {
  syn <- pelsa_make_synthetic(seed = 1)
  exploded <- pelsa_explode_accessions(syn$peptides)
  mapped <- pelsa_map_peptide_positions(exploded, syn$fasta)$matched
  contrast <- syn$contrasts[1]
  mapped$adj.P.Val <- mapped[[paste0("adj.P.Val.", contrast)]]
  mapped$logFC <- mapped[[paste0("logFC.", contrast)]]

  out1 <- pelsa_best_peptide_rollup(mapped)
  out2 <- pelsa_best_peptide_rollup(mapped)
  expect_identical(out1, out2)

  # TIEPROT's two tie peptides share identical (adjp, logFC); the winner is the
  # lexicographically smaller sequence "TIEPEPONEK".
  tie_out <- out1[out1$won_accessions == syn$tie_accession, , drop = FALSE]
  expect_equal(nrow(tie_out), 1L)
  expect_equal(tie_out$peptide_seq, "TIEPEPONEK")
})

# --- Multi-label entry order matches the all_peptide panel: (pep_start, accession)
test_that("multi-label entries are ordered by (pep_start, accession), not accession", {
  # Peptide X maps to accessions A (pep_start 200) and B (pep_start 5). The
  # all_peptide panel orders label entries by (pep_start, accession), so the
  # canonical order is B's (aa5) BEFORE A's (aa200) even though A < B
  # alphabetically. The best_peptide rollup must produce the SAME order.
  df <- .rollup_make_exploded(
    peptide_seq = c("X",   "X"),
    accession   = c("A",   "B"),
    gene        = c("GA",  "GB"),
    pep_start   = c(200L,  5L),
    adj.P.Val   = c(.001,  .001),
    logFC       = c(-1,    -1)
  )
  out <- pelsa_best_peptide_rollup(df)
  x <- out[out$peptide_seq == "X", ]
  expect_equal(nrow(x), 1L)
  # LABEL entries reorder to (pep_start, accession): B@aa5 first, then A@aa200.
  expect_equal(x$label, "GB_aa5;GA_aa200")
  # won_accessions stays in winner/stats-priority order (its first token is the
  # representative won accession); equal stats here -> accession tie-break -> A;B.
  expect_equal(x$won_accessions, "A;B")
})

# ==============================================================================
# --- from peptide ---
# Tests for the PELSA peptide helpers:
#   pelsa_missed_cleavages()  -  tryptic missed-cleavage count, notebook parity
#   pelsa_peptide_length()    -  peptide residue count
#   pelsa_build_multilabel()  -  canonical ;-joined gene_aa<pos> label builder
#
# Missed-cleavage is parity-gated against the analysis notebook's exact rule:
#   core = peptide[:-1]; len(re.findall(r'[KR](?!P)', core))
# ==============================================================================

# --- pelsa_missed_cleavages() ------------------------------------------------

test_that("peptide ending in K with no internal K/R has 0 missed cleavages", {
  # core "PEPTIDE" has no K/R -> 0
  expect_equal(pelsa_missed_cleavages("PEPTIDEK"), 0L)
})

test_that("an internal R not before P counts as one missed cleavage", {
  # core "AAARAAA" -> one internal R -> 1
  expect_equal(pelsa_missed_cleavages("AAARAAAK"), 1L)
})

test_that("internal K immediately followed by P does NOT count", {
  # core "AAKPAAA" -> K-P excluded -> 0
  expect_equal(pelsa_missed_cleavages("AAKPAAAK"), 0L)
})

test_that("internal R immediately followed by P does NOT count", {
  # core "AARPAAA" -> R-P excluded -> 0
  expect_equal(pelsa_missed_cleavages("AARPAAAK"), 0L)
})

test_that("C-terminal K is excluded from the count", {
  # core "AAAAA" -> 0
  expect_equal(pelsa_missed_cleavages("AAAAAK"), 0L)
})

test_that("two internal counted sites give 2 missed cleavages", {
  # core "AKAAARAA" -> internal K (not before P) + internal R (not before P) -> 2
  expect_equal(pelsa_missed_cleavages("AKAAARAAK"), 2L)
})

test_that("single-residue peptide has 0 missed cleavages", {
  # nchar < 2 -> 0 (no internal positions)
  expect_equal(pelsa_missed_cleavages("K"), 0L)
})

test_that("two-residue KR follows the substr(.,1,nchar-1) rule", {
  # core "K" -> str_count("K","[KR](?!P)") = 1 (no following char -> not before P)
  expect_equal(pelsa_missed_cleavages("KR"), 1L)
})

test_that("missed cleavages is vectorized over a character vector", {
  expect_equal(
    pelsa_missed_cleavages(c("PEPTIDEK", "AAARAAAK", "AAKPAAAK")),
    c(0L, 1L, 0L)
  )
})

test_that("missed cleavages returns NA for an NA sequence", {
  # documented choice: NA sequence -> NA_integer_
  expect_equal(pelsa_missed_cleavages(NA_character_), NA_integer_)
})

test_that("missed cleavages handles NA mixed within a vector", {
  expect_equal(
    pelsa_missed_cleavages(c("AAARAAAK", NA_character_, "AAKPAAAK")),
    c(1L, NA_integer_, 0L)
  )
})

test_that("missed cleavages returns an integer vector", {
  out <- pelsa_missed_cleavages(c("PEPTIDEK", "AAARAAAK"))
  expect_type(out, "integer")
})

test_that("missed cleavages returns integer(0) for empty input", {
  expect_equal(pelsa_missed_cleavages(character(0)), integer(0))
})

test_that("missed cleavages coerces factor input like character input", {
  expect_equal(
    pelsa_missed_cleavages(factor(c("PEPTIDEK", "AAARAAAK"))),
    pelsa_missed_cleavages(c("PEPTIDEK", "AAARAAAK"))
  )
})

# --- pelsa_peptide_length() --------------------------------------------------

test_that("peptide length returns nchar with NA preserved, as integer", {
  expect_equal(
    pelsa_peptide_length(c("PEPK", "AA", NA)),
    c(4L, 2L, NA_integer_)
  )
})

test_that("peptide length returns an integer vector", {
  expect_type(pelsa_peptide_length(c("PEPK", "AA")), "integer")
})

test_that("peptide length returns integer(0) for empty input", {
  expect_equal(pelsa_peptide_length(character(0)), integer(0))
})

test_that("peptide length coerces factor input like character input", {
  expect_equal(
    pelsa_peptide_length(factor(c("PEPTIDEK", "AAARAAAK"))),
    pelsa_peptide_length(c("PEPTIDEK", "AAARAAAK"))
  )
})

# --- pelsa_resolve_label_stem() ----------------------------------------------
# Fallback order gene -> protein_name -> accession. Missing = NA OR blank.

test_that("resolve_label_stem prefers the gene when present", {
  expect_equal(
    pelsa_resolve_label_stem("GENEA", "NameA_HUMAN", "P1"),
    "GENEA"
  )
})

test_that("resolve_label_stem falls back to protein name when gene is missing", {
  expect_equal(
    pelsa_resolve_label_stem(c("", NA), c("NameA_HUMAN", "NameB_HUMAN"),
                             c("P1", "P2")),
    c("NameA_HUMAN", "NameB_HUMAN")
  )
})

test_that("resolve_label_stem falls back to accession when gene AND name missing", {
  expect_equal(
    pelsa_resolve_label_stem(c("", NA), c("", NA), c("P1", "P2")),
    c("P1", "P2")
  )
})

test_that("resolve_label_stem treats a blank/whitespace protein name as missing", {
  expect_equal(
    pelsa_resolve_label_stem("", "   ", "P1"),
    "P1"
  )
})

test_that("resolve_label_stem forces accession for self-curated regardless of name", {
  expect_equal(
    pelsa_resolve_label_stem("GENEA", "NameA_HUMAN", "P1",
                             is_self_curated = TRUE),
    "P1"
  )
})

test_that("resolve_label_stem is NULL-protein-name tolerant (treated as all-missing)", {
  # A caller with no PG.ProteinNames column passes NULL; the middle tier is
  # then a no-op and the fallback is gene -> accession (legacy behavior).
  expect_equal(
    pelsa_resolve_label_stem(c("GENEA", ""), NULL, c("P1", "P2")),
    c("GENEA", "P2")
  )
})

# --- pelsa_build_multilabel() ------------------------------------------------

test_that("distinct genes/positions join in input order with ;", {
  expect_equal(
    pelsa_build_multilabel(c("GENEA", "GENEB"), c(120, 88), c("P1", "P2")),
    "GENEA_aa120;GENEB_aa88"
  )
})

test_that("fully-identical entries collapse to one", {
  expect_equal(
    pelsa_build_multilabel(c("GENEA", "GENEA"), c(120, 120), c("P1", "P2")),
    "GENEA_aa120"
  )
})

test_that("same gene at different positions are kept separate", {
  expect_equal(
    pelsa_build_multilabel(c("GENEA", "GENEA"), c(120, 130), c("P1", "P2")),
    "GENEA_aa120;GENEA_aa130"
  )
})

test_that("empty/NA gene falls back to accession", {
  expect_equal(
    pelsa_build_multilabel(c("", NA), c(50, 60), c("P1", "P2")),
    "P1_aa50;P2_aa60"
  )
})

test_that("a single mapping yields a label with no semicolon", {
  expect_equal(
    pelsa_build_multilabel("GENEA", 120, "P1"),
    "GENEA_aa120"
  )
})

test_that("character positions are coerced for the aa<pos> text", {
  expect_equal(
    pelsa_build_multilabel("GENEA", "120", "P1"),
    "GENEA_aa120"
  )
})

test_that("de-duplication preserves first-occurrence order", {
  # GENEA_aa120 appears first, then GENEB_aa88, then a duplicate GENEA_aa120
  expect_equal(
    pelsa_build_multilabel(
      c("GENEA", "GENEB", "GENEA"),
      c(120, 88, 120),
      c("P1", "P2", "P3")
    ),
    "GENEA_aa120;GENEB_aa88"
  )
})

test_that("empty input returns NA_character_", {
  expect_equal(pelsa_build_multilabel(character(0), integer(0), character(0)),
               NA_character_)
})

test_that("mismatched input lengths error rather than silently recycle", {
  # A scalar position recycled across two genes would emit a wrong label.
  expect_error(
    pelsa_build_multilabel(c("GENEA", "GENEB"), 120, c("P1", "P2"))
  )
  expect_error(
    pelsa_build_multilabel(c("GENEA", "GENEB"), c(120, 88), "P1")
  )
})

test_that("multilabel uses protein name when gene missing but name present", {
  expect_equal(
    pelsa_build_multilabel(c("", NA), c(50, 60), c("P1", "P2"),
                           protein_names = c("NameA", "NameB")),
    "NameA_aa50;NameB_aa60"
  )
})

test_that("multilabel falls to accession when gene AND protein name missing", {
  expect_equal(
    pelsa_build_multilabel(c("", NA), c(50, 60), c("P1", "P2"),
                           protein_names = c("", NA)),
    "P1_aa50;P2_aa60"
  )
})

test_that("multilabel without protein_names keeps legacy gene->accession", {
  expect_equal(
    pelsa_build_multilabel(c("", NA), c(50, 60), c("P1", "P2")),
    "P1_aa50;P2_aa60"
  )
})

test_that("multilabel self-curated still forces accession over protein name", {
  expect_equal(
    pelsa_build_multilabel(c("GA", "GB"), c(50, 60), c("P1", "P2"),
                           is_self_curated = TRUE,
                           protein_names = c("NameA", "NameB")),
    "P1_aa50;P2_aa60"
  )
})

# ==============================================================================
# --- from thinning ---
# Tests for pelsa_thin_background()  -  density-proportional volcano background
# thinning (Task 3B). Pure, no Shiny.
#
# Thin ONLY the uninformative background cloud  -  points that are ALL of:
# non-significant, |logFC| <= thresh, and NOT a marker-protein peptide. Thinning
# is DENSITY-PROPORTIONAL (a fixed fraction kept per 2-D bin).
# ==============================================================================

# ---- helpers -----------------------------------------------------------------

# Build a volcano-like data.frame carrying exactly the columns the thinner reads
# (Significant / logFC / logP / is_marker), plus an `id` so we can track which
# specific rows survive across calls.
make_volcano <- function(logFC, logP, Significant, is_marker, id = NULL) {
  n <- length(logFC)
  if (is.null(id)) id <- seq_len(n)
  data.frame(
    id          = id,
    logFC       = as.numeric(logFC),
    logP        = as.numeric(logP),
    Significant = as.logical(Significant),
    is_marker   = as.logical(is_marker),
    stringsAsFactors = FALSE
  )
}

# A mixed frame: some significant, some big-effect non-sig, some markers, and a
# pile of true background (non-sig, small |logFC|, non-marker).
mixed_volcano <- function() {
  bg <- make_volcano(
    logFC       = runif(200, -0.4, 0.4),
    logP        = runif(200, 0, 1),
    Significant = FALSE,
    is_marker   = FALSE,
    id          = paste0("bg", seq_len(200))
  )
  sig <- make_volcano(
    logFC = c(2, -2, 1.5), logP = c(5, 6, 4),
    Significant = TRUE, is_marker = FALSE,
    id = c("sig1", "sig2", "sig3")
  )
  bigeff <- make_volcano(  # non-sig but |logFC| > thresh  -  must be retained
    logFC = c(0.9, -1.2), logP = c(0.3, 0.4),
    Significant = FALSE, is_marker = FALSE,
    id = c("big1", "big2")
  )
  mk <- make_volcano(  # marker peptide, small effect, non-sig  -  must be retained
    logFC = c(0.1, -0.2), logP = c(0.2, 0.5),
    Significant = FALSE, is_marker = TRUE,
    id = c("mk1", "mk2")
  )
  rbind(bg, sig, bigeff, mk)
}

# ---- thinnable-set logic -----------------------------------------------------

test_that("significant / big-effect / marker rows are NEVER thinned", {
  df <- mixed_volcano()
  out <- pelsa_thin_background(df, keep_frac = 0.3, logfc_thresh = 0.5,
                               n_bins = 10, seed = 1)
  kept_ids <- out$df$id

  # every significant peptide retained
  expect_true(all(c("sig1", "sig2", "sig3") %in% kept_ids))
  # every |logFC| > thresh peptide retained (even non-sig)
  expect_true(all(c("big1", "big2") %in% kept_ids))
  # every marker peptide retained
  expect_true(all(c("mk1", "mk2") %in% kept_ids))
})

test_that("only non-sig & |logFC|<=thresh & non-marker rows are thinnable", {
  df <- mixed_volcano()
  out <- pelsa_thin_background(df, keep_frac = 0.3, logfc_thresh = 0.5,
                               n_bins = 10, seed = 1)
  # 200 background rows are the only thinnable set; with keep_frac 0.3 some
  # must be dropped, and the dropped rows must all come from background.
  dropped_ids <- setdiff(df$id, out$df$id)
  expect_true(length(dropped_ids) > 0)
  expect_true(all(grepl("^bg", dropped_ids)))
  expect_equal(out$n_thinnable, 200L)
})

# ---- density-proportional (THE defining test) -------------------------------

test_that("thinning is density-proportional: dense bins keep proportionally more", {
  # One DENSE cell: 1000 thinnable points at EXACTLY (0.1, 0.1).
  # One SPARSE cell: 10 thinnable points at EXACTLY (-0.4, 0.9).
  # Identical coordinates put each cluster in a SINGLE (logFC, logP) bin, so the
  # per-bin ceiling(keep_frac * n) is exact and the proportionality is crisp.
  dense <- make_volcano(
    logFC = rep(0.10, 1000), logP = rep(0.10, 1000),
    Significant = FALSE, is_marker = FALSE, id = paste0("d", seq_len(1000))
  )
  sparse <- make_volcano(
    logFC = rep(-0.40, 10), logP = rep(0.90, 10),
    Significant = FALSE, is_marker = FALSE, id = paste0("s", seq_len(10))
  )
  df <- rbind(dense, sparse)

  out <- pelsa_thin_background(df, keep_frac = 0.3, logfc_thresh = 0.5,
                               n_bins = 50, seed = 7)
  kept <- out$df$id
  n_dense_kept  <- sum(grepl("^d", kept))
  n_sparse_kept <- sum(grepl("^s", kept))

  # dense keeps ceiling(0.3 * 1000) = 300, sparse keeps ceiling(0.3 * 10) = 3
  expect_equal(n_dense_kept, 300L)
  expect_equal(n_sparse_kept, 3L)

  # PROPORTIONALITY: retained-count ratio (100:1) MIRRORS the original-count
  # ratio, NOT flattened toward 1:1 as a uniform sample would. This is the
  # defining property  -  dense stays dense, sparse stays sparse.
  ratio <- n_dense_kept / n_sparse_kept
  expect_true(ratio > 80 && ratio < 120)
})

# ---- counts ------------------------------------------------------------------

test_that("counts are correct and internally consistent", {
  df <- mixed_volcano()
  out <- pelsa_thin_background(df, keep_frac = 0.3, logfc_thresh = 0.5,
                               n_bins = 10, seed = 1)
  expect_equal(out$n_total, nrow(df))
  expect_equal(out$n_shown, nrow(out$df))
  expect_equal(out$n_thinnable, 200L)
  # consistency: shown = total - (thinnable - thinnable_kept)
  expect_equal(out$n_shown,
               out$n_total - (out$n_thinnable - out$n_thinnable_kept))
  expect_true(out$n_thinnable_kept < out$n_thinnable)  # actually thinned
})

# ---- keep_frac = 1 is a no-op ------------------------------------------------

test_that("keep_frac = 1 keeps everything (df identical to input)", {
  df <- mixed_volcano()
  out <- pelsa_thin_background(df, keep_frac = 1, logfc_thresh = 0.5,
                               n_bins = 10, seed = 1)
  expect_equal(out$df, df)
  expect_equal(out$n_shown, out$n_total)
  expect_equal(out$n_thinnable_kept, out$n_thinnable)
})

test_that("keep_frac >= 1 also keeps everything", {
  df <- mixed_volcano()
  out <- pelsa_thin_background(df, keep_frac = 2, logfc_thresh = 0.5,
                               n_bins = 10, seed = 1)
  expect_equal(out$df, df)
})

# ---- determinism -------------------------------------------------------------

test_that("a fixed seed yields identical kept rows across calls", {
  df <- mixed_volcano()
  a <- pelsa_thin_background(df, keep_frac = 0.3, n_bins = 10, seed = 42)
  b <- pelsa_thin_background(df, keep_frac = 0.3, n_bins = 10, seed = 42)
  expect_identical(a$df, b$df)
  expect_identical(a$df$id, b$df$id)
})

# ---- edge cases --------------------------------------------------------------

test_that("empty thinnable set (all significant) leaves df unchanged", {
  df <- make_volcano(
    logFC = c(2, -2, 1), logP = c(5, 6, 4),
    Significant = TRUE, is_marker = FALSE, id = c("a", "b", "c")
  )
  out <- pelsa_thin_background(df, keep_frac = 0.3, n_bins = 10, seed = 1)
  expect_equal(out$df, df)
  expect_equal(out$n_thinnable, 0L)
  expect_equal(out$n_thinnable_kept, 0L)
  expect_equal(out$n_shown, out$n_total)
})

test_that("all-thinnable frame thins down to the per-bin proportion", {
  df <- make_volcano(
    logFC = runif(100, -0.3, 0.3), logP = runif(100, 0, 1),
    Significant = FALSE, is_marker = FALSE
  )
  out <- pelsa_thin_background(df, keep_frac = 0.5, n_bins = 5, seed = 3)
  expect_equal(out$n_thinnable, 100L)
  expect_true(out$n_thinnable_kept < 100L)
  expect_true(out$n_thinnable_kept >= 50L)  # ceiling rounding never drops below frac
})

test_that("a singleton bin always survives (ceiling keeps >= 1)", {
  # One lone thinnable point far from any other -> its own bin -> survives.
  lone <- make_volcano(0.0, 0.0, FALSE, FALSE, id = "lone")
  cloud <- make_volcano(
    logFC = rnorm(500, 0.4, 0.01), logP = rnorm(500, 0.9, 0.01),
    Significant = FALSE, is_marker = FALSE, id = paste0("c", seq_len(500))
  )
  df <- rbind(lone, cloud)
  out <- pelsa_thin_background(df, keep_frac = 0.1, n_bins = 50, seed = 5)
  expect_true("lone" %in% out$df$id)
})

test_that("a thinnable row with NA coords is retained untouched", {
  df <- make_volcano(
    logFC = c(NA, 0.1, 0.2), logP = c(0.5, NA, 0.3),
    Significant = FALSE, is_marker = FALSE, id = c("na1", "na2", "ok")
  )
  out <- pelsa_thin_background(df, keep_frac = 0.0001, n_bins = 50, seed = 1)
  # NA-coord rows can't be binned -> retained; only "ok" was binnable.
  expect_true(all(c("na1", "na2") %in% out$df$id))
})

test_that("a thinnable row with non-finite coords does not crash and is retained", {
  # logP = Inf is reachable: 3A builds logP = -log10(P.Value), so a P.Value of 0
  # (permutation p-values / numeric underflow) yields logP = Inf. range() on it
  # would make seq(length.out=) throw  -  the binner must fold non-finite into the
  # "can't be binned" set and RETAIN the row untouched. logFC = -Inf likewise.
  # inf_lp: thinnable (small |logFC|) but logP = Inf -> can't be binned, retained.
  # neginf_fc: logFC = -Inf, so logP is irrelevant; with logP = 0.3 it is a true
  # background coord-wise, but a -Inf logFC's |logFC| > thresh path would mark it
  # big-effect. Use a finite small logFC here so it is genuinely thinnable and
  # the -Inf is in the COORDINATE we test (logP).
  df <- make_volcano(
    logFC = c(0.1, 0.15, -Inf, 0.2, 0.0),
    logP  = c(Inf, -Inf,  0.2, 0.4, 0.1),
    Significant = FALSE, is_marker = FALSE,
    id = c("inf_lp", "neginf_lp", "neginf_fc", "ok1", "ok2")
  )
  out <- expect_no_error(
    pelsa_thin_background(df, keep_frac = 0.0001, n_bins = 50, seed = 1)
  )
  # non-finite-coord rows are retained untouched: inf_lp / neginf_lp are
  # thinnable-but-unbinnable (retained via the coord path); neginf_fc has
  # |logFC| > thresh so it is a big-effect row retained via the non-thinnable
  # path  -  either way it must NOT crash and MUST survive.
  expect_true(all(c("inf_lp", "neginf_lp", "neginf_fc") %in% out$df$id))
  # 4 thinnable (the two finite-coord oks + the two with a non-finite COORD but
  # small |logFC|); neginf_fc is big-effect, not thinnable.
  expect_equal(out$n_total, 5L)
  expect_equal(out$n_thinnable, 4L)
  expect_equal(out$n_shown,
               out$n_total - (out$n_thinnable - out$n_thinnable_kept))
})

test_that("tiny keep_frac keeps at least one point per non-empty bin", {
  # Spread points across many distinct bins; with keep_frac well below 1/n per
  # bin, the ceiling(keep_frac*n) >= 1 floor means each non-empty bin keeps >= 1,
  # so n_thinnable_kept >= number of non-empty bins (sparse structure preserved).
  set.seed(11)
  df <- make_volcano(
    logFC = runif(800, -0.4, 0.4), logP = runif(800, 0, 5),
    Significant = FALSE, is_marker = FALSE
  )
  n_bins <- 20
  out <- pelsa_thin_background(df, keep_frac = 0.001, n_bins = n_bins, seed = 2)
  # recompute the non-empty bin count the same way the helper bins
  bin_axis <- function(x, nb) {
    rng <- range(x); if (rng[1] == rng[2]) return(rep(0L, length(x)))
    br <- seq(rng[1], rng[2], length.out = nb + 1)
    findInterval(x, br[-c(1, length(br))], rightmost.closed = TRUE)
  }
  n_nonempty <- length(unique(bin_axis(df$logFC, n_bins) * n_bins +
                                bin_axis(df$logP, n_bins)))
  expect_gte(out$n_thinnable_kept, n_nonempty)
})

test_that("a data.table volcano_df round-trips with all columns preserved", {
  skip_if_not_installed("data.table")
  df <- mixed_volcano()
  dt <- data.table::as.data.table(df)
  out <- pelsa_thin_background(dt, keep_frac = 0.3, n_bins = 10, seed = 1)
  # every input column survives the round-trip
  expect_true(all(names(df) %in% names(out$df)))
  # the returned df is usable: counts consistent and non-thinnable rows kept
  expect_equal(out$n_shown,
               out$n_total - (out$n_thinnable - out$n_thinnable_kept))
  expect_true(all(c("sig1", "sig2", "sig3", "big1", "big2", "mk1", "mk2") %in%
                    out$df$id))
})

# ---- boundary validation -----------------------------------------------------

test_that("missing required column errors", {
  df <- make_volcano(0.1, 0.2, FALSE, FALSE)
  df$is_marker <- NULL
  expect_error(pelsa_thin_background(df), "is_marker")
})

test_that("keep_frac out of (0,1] range errors", {
  df <- mixed_volcano()
  expect_error(pelsa_thin_background(df, keep_frac = 0), "keep_frac")
  expect_error(pelsa_thin_background(df, keep_frac = -0.5), "keep_frac")
  expect_error(pelsa_thin_background(df, keep_frac = NA), "keep_frac")
})

test_that("non-data.frame input errors", {
  expect_error(pelsa_thin_background(list(a = 1)), "data.frame")
})

# ==============================================================================
# --- from quantified-mask ---
# Tests for the canonical "quantified" mask (pelsa_quantified_mask) and its three
# consumers -- per-sample depth, fully-quantified count, and per-condition
# membership -- ACROSS DATA TYPES (linear / log2 / log10 / median-centered).
#
# THE CONTRACT: "quantified" = finite & non-zero. This is INVARIANT under the
# monotonic processing transforms Protigy applies. These tests couple to
# perform_log_transformation (a real package function), which is intentional.
# ==============================================================================

# A controlled raw LINEAR matrix: 4 peptides x 4 samples, two conditions
# (c1 = S1,S2 ; c2 = S3,S4). Deliberately includes:
#   pepLow : raw < 1 in every sample  -> NEGATIVE under log2/log10 (fix target)
#   pepNA  : an NA hole               -> never quantified there, any scale
#   pepHi  : large                    -> positive on every scale
#   pepMid : moderate
# No raw value is exactly 1, so no log value is exactly 0.
.qm_raw <- function() {
  matrix(
    c(
      #  S1     S2     S3     S4
      120.0, 130.0, 110.0,  90.0,   # pepHi-ish (pepMid)
        0.5,   0.6,   0.4,   0.55,  # pepLow  (raw < 1 -> log NEGATIVE)
      100.0,    NA, 100.0, 100.0,   # pepNA   (NA in S2)
      8000.0,7900.0,8100.0,7950.0   # pepHi
    ),
    nrow = 4, byrow = TRUE,
    dimnames = list(c("pepMid", "pepLow", "pepNA", "pepHi"),
                    c("S1", "S2", "S3", "S4"))
  )
}

.qm_cmap <- function() {
  c(S1 = "c1", S2 = "c1", S3 = "c2", S4 = "c2")
}

# Apply each processing scale to a raw linear matrix. Uses the real package
# transform for log; median/mean centering mirrors normalize.data's centering.
.qm_scales <- function(raw) {
  log2m  <- perform_log_transformation(raw, "log2")$data
  log10m <- perform_log_transformation(raw, "log10")$data
  med    <- sweep(log2m, 2, apply(log2m, 2, stats::median, na.rm = TRUE), "-")
  list(None = raw, log2 = log2m, log10 = log10m, `log2+median` = med)
}

# Ground truth (finite & non-zero), independent of scale, for .qm_raw():
#   depth per sample: S1 all 4 finite&!=0 = 4 ; S2 pepNA is NA -> 3 ; S3 4 ; S4 4
#   fully-quantified (finite&!=0 in ALL samples): pepMid,pepLow,pepHi = 3
#                                                 (pepNA fails on S2)
#   membership: every finite&!=0 peptide is in both conditions; pepNA is still
#               quantified in c1 via S1 (NA only in S2) and in c2 via S3,S4.
#               -> all 4 peptides in c1 AND c2.

test_that("per-sample depth is identical across None/log2/log10 (negatives kept)", {
  # Pure monotonic transforms (no centering) cannot create exact zeros here
  # (no raw value is exactly 1), so the finite & non-zero count is invariant.
  scales <- .qm_scales(.qm_raw())[c("None", "log2", "log10")]
  expected <- c(S1 = 4L, S2 = 3L, S3 = 4L, S4 = 4L)
  for (nm in names(scales)) {
    got <- pelsa_peptides_per_sample(scales[[nm]])
    expect_equal(got, expected, info = sprintf("scale = %s", nm))
  }
})

test_that("fully-quantified is identical across None/log2/log10 (negatives kept)", {
  scales <- .qm_scales(.qm_raw())[c("None", "log2", "log10")]
  fully <- function(m) sum(rowSums(!pelsa_quantified_mask(m)) == 0L)
  for (nm in names(scales)) {
    expect_equal(fully(scales[[nm]]), 3L, info = sprintf("scale = %s", nm))
  }
})

test_that("median-centering: documented exact-zero edge (median element absent)", {
  # KNOWN, NEGLIGIBLE EDGE: median-centering subtracts the column median, so a
  # column with an ODD finite count puts its MEDIAN element at exactly 0, which
  # finite & non-zero then reads as "absent". Here S2 has 3 finite values, its
  # median (pepMid) centers to 0 -> S2 drops from 3 to 2. At real scale this is
  # at most ~1 peptide per sample (immaterial), but we assert it rather than
  # hide it. All OTHER columns (even finite counts, median between elements) are
  # unaffected and still match the scale-invariant counts.
  med <- .qm_scales(.qm_raw())[["log2+median"]]
  expect_equal(pelsa_peptides_per_sample(med),
               c(S1 = 4L, S2 = 2L, S3 = 4L, S4 = 4L))
  # pepMid loses its only c1-via-S2 *additional* sample but is still present in
  # S1, so it stays fully... except S2 == 0 makes it NOT fully-quantified now.
  fully <- sum(rowSums(!pelsa_quantified_mask(med)) == 0L)
  expect_equal(fully, 2L)  # pepLow + pepHi (pepMid knocked out at S2)
})

test_that("per-condition membership is identical across scales", {
  scales <- .qm_scales(.qm_raw())
  cmap <- .qm_cmap()
  for (nm in names(scales)) {
    mem <- pelsa_condition_membership(scales[[nm]], cmap)
    # All 4 peptides appear in BOTH conditions -> 8 (row_id, condition) rows.
    expect_equal(nrow(mem), 8L, info = sprintf("scale = %s", nm))
    expect_setequal(unique(mem$condition), c("c1", "c2"))
    # pepLow is row 2: present in both conditions on every scale (negative on log)
    expect_true(all(c("c1", "c2") %in% mem$condition[mem$row_id == 2L]),
                info = sprintf("pepLow membership, scale = %s", nm))
    expect_setequal(sort(unique(mem$row_id)), 1:4)
  }
})

test_that("OLD `> 0` mask WOULD have diverged on log scales (fix is load-bearing)", {
  raw <- .qm_raw()
  log2m <- perform_log_transformation(raw, "log2")$data
  old_depth <- colSums(is.finite(log2m) & log2m > 0)       # notebook mask
  new_depth <- pelsa_peptides_per_sample(log2m)
  # pepLow is negative under log2: the old mask drops it from EVERY sample,
  # the new mask keeps it. They must differ -> proves the change matters.
  expect_false(isTRUE(all.equal(unname(old_depth), unname(new_depth))))
  expect_equal(unname(new_depth) - unname(as.integer(old_depth)),
               c(1L, 1L, 1L, 1L))  # exactly pepLow recovered per sample
})

test_that("no behavior change on LINEAR data (None): new mask == old `> 0`", {
  raw <- .qm_raw()
  expect_equal(pelsa_quantified_mask(raw), is.finite(raw) & raw > 0)
  expect_equal(unname(pelsa_peptides_per_sample(raw)),
               unname(colSums(is.finite(raw) & raw > 0)))
})

test_that("documented edges: exact 0 and exact log-zero are treated as absent", {
  # Exact 0 (linear absent) -> dropped.
  m0 <- matrix(c(0, 5, 0, 7), nrow = 2, dimnames = list(NULL, c("A", "B")))
  expect_equal(unname(pelsa_peptides_per_sample(m0)), c(1L, 1L))
  # A raw intensity of EXACTLY 1 -> log2 == 0 -> dropped (negligible edge).
  raw1 <- matrix(c(1, 4, 2, 1), nrow = 2, dimnames = list(NULL, c("A", "B")))
  l <- perform_log_transformation(raw1, "log2")$data    # col A: log2(1)=0, log2(4)=2
  expect_equal(unname(pelsa_peptides_per_sample(l)), c(1L, 1L))
})
