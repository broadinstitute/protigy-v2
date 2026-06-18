################################################################################
# Tests for pelsa_best_peptide_rollup() — best-peptide-per-protein rollup.
#
# Two-step logic (Protigy refinement over the notebook's per-accession dots):
#   Step 1 (notebook _rollup_to_proteins): per accession, keep the FIRST row
#     after a STABLE sort on [adj.P.Val, logFC, peptide_seq, accession] — the
#     last two keys are a deterministic total-ordering tiebreak so the chosen
#     "best" peptide is fully reproducible even on exact (adj.P.Val, logFC) ties.
#   Step 2 (regroup by peptide): a peptide has ONE (adj.P.Val, logFC) coordinate,
#     so a peptide that wins multiple accessions becomes ONE dot, not several
#     overlapping ones. Each distinct best-peptide yields one output row carrying
#     a ;-joined multi-label (one <gene>_aa<pos> per won accession), built via
#     pelsa_build_multilabel().
#
# NA handling: rows with NA adj.P.Val sort LAST (na.last=TRUE) so a real p-value
# is preferred; an accession whose ONLY peptide has NA stats keeps that peptide.
#
# Parity-gated against the Phase-1 synthetic generator for the integration test.
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# Convenience constructor for a hand-built exploded stat frame. Columns match
# the rollup defaults (adj.P.Val, logFC, PEP.StrippedSequence, accession, gene,
# pep_start).
.make_exploded <- function(peptide_seq, accession, gene, pep_start,
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
  df <- .make_exploded(
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
  df <- .make_exploded(
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
  df <- .make_exploded(
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
  df <- .make_exploded(
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
  df <- .make_exploded(
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
  df <- .make_exploded(
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
  df <- .make_exploded(
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
  df <- .make_exploded(
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
  df <- .make_exploded(
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
  df <- .make_exploded(
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
  df <- .make_exploded("P1", "A", "GA", 10L, .01, -1)
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
  df <- .make_exploded(
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
