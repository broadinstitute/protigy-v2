################################################################################
# Tests for the PELSA FASTA reader + FASTA-substring peptide position mapping.
#
#   pelsa_read_fasta(path)            -> named list accession -> AA string
#   pelsa_map_peptide_positions(...)  -> list(matched=, unmatched=)
#
# This is the highest parity-risk PELSA helper: it produces the pep_start /
# pep_end coordinates used for every aa<pos> label, sequence coverage, and the
# unmatched QC table. Parity-gated against the Phase-1 synthetic generator
# (fixtures/pelsa/generate_synthetic.R) whose header documents exact ground-truth
# coordinates for every edge case.
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

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
.make_exploded <- function(seed = 1) {
  syn <- pelsa_make_synthetic(seed = seed)
  list(
    syn = syn,
    exploded = pelsa_explode_accessions(syn$peptides)
  )
}

test_that("dup peptide emits two matched rows at the known starts", {
  ctx <- .make_exploded()
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
  ctx <- .make_exploded()
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
  ctx <- .make_exploded()
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
  ctx <- .make_exploded()
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
  ctx <- .make_exploded()
  res <- pelsa_map_peptide_positions(ctx$exploded, ctx$syn$fasta)

  iso_rows <- res$matched[
    res$matched$accession == ctx$syn$isoform_accession, , drop = FALSE
  ]
  expect_gte(nrow(iso_rows), 1L)
  # FASTA P12345 = "MSTART" + "ISOPEPTIDEK" + "END" -> start 7
  expect_true(all(iso_rows$pep_start == 7L))
})

test_that("bad-sequence peptide -> bad_sequence_format and never searched", {
  ctx <- .make_exploded()
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
  ctx <- .make_exploded()
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
  ctx <- .make_exploded()
  res <- pelsa_map_peptide_positions(ctx$exploded, ctx$syn$fasta)
  expect_true(all(
    c("peptide_sequence", "accession", "gene", "pep_position", "reason")
      %in% colnames(res$unmatched)
  ))
  expect_true(all(res$unmatched$reason %in%
    c("accession_absent", "sequence_not_found", "bad_sequence_format")))
})
