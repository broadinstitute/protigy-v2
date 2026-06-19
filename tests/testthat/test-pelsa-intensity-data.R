################################################################################
# Tests for the PELSA per-protein intensity-line DATA builder (Task 3C).
#
# Two pure helpers (no Shiny / no plotting):
#
#   pelsa_intensity_proteins(stat_df, matched_cache, markers, contrast, ...)
#     -> WHICH accessions get an intensity-line figure: the union of markers
#        (isoform-base matched via 2J) and accessions with >=1 SIGNIFICANT
#        peptide (adj.P.Val.<contrast> < sig_cutoff). Each tagged is_marker.
#
#   pelsa_intensity_line_data(accession, stat_df, matched_cache, processed_mat,
#                             condition_map, condition_order, contrast,
#                             sig_cutoff, is_marker)
#     -> tidy long line data for ONE protein. One line per peptide-OCCURRENCE
#        (matched_cache row for this accession). Marker -> BOTH sig + non-sig
#        peptides tagged by `panel` {"Significant","Non-significant"}; non-marker sig
#        protein -> only its significant peptide-occurrences.
#        y = MEAN processed-GCT log2 intensity AS-IS (no delinearize/z/renorm),
#        averaged over a condition's replicate columns (na.rm). aa<pos> label =
#        FASTA-derived pep_start from matched_cache. condition is a factor with
#        levels = condition_order.
#
# Row join: matched_cache carries `.row_id` (1-based into the ORIGINAL peptide
# frame); processed_mat rows align to that same order, so a peptide's y is
# processed_mat[.row_id, sample_cols]. (rownames fallback is exercised too.)
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# ---- hand-built fixtures -----------------------------------------------------

# A per-peptide stat frame with contrast-suffixed adj.P.Val.<contrast> and a
# stable .row_id (the index into processed_mat rows).
.mk_stat <- function(seq, acc, adjp, contrast = "C1", row_id = seq_along(seq)) {
  df <- data.frame(
    PEP.StrippedSequence = seq,
    PG.ProteinAccessions = acc,
    .row_id              = row_id,
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  df[[paste0("adj.P.Val.", contrast)]] <- adjp
  df
}

# A matched-cache frame: one row per (peptide, accession, occurrence), carrying
# the 2B columns the builder reads (.row_id / accession / pep_start /
# pep_occurrence_idx / PEP.StrippedSequence).
.mk_matched <- function(seq, accession, pep_start, row_id,
                        pep_occurrence_idx = rep(1L, length(seq))) {
  data.frame(
    PEP.StrippedSequence = seq,
    accession            = accession,
    pep_start            = as.integer(pep_start),
    pep_occurrence_idx   = as.integer(pep_occurrence_idx),
    .row_id              = as.integer(row_id),
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
}

# =============================================================================
# pelsa_intensity_proteins: markers UNION significant-accessions
# =============================================================================

test_that("proteins = union(markers, accessions-with-a-sig-peptide); is_marker correct", {
  # Accessions:
  #   M_ONLY   : marker, no significant peptide               -> is_marker TRUE
  #   SIG_ONLY : not a marker, has a significant peptide       -> is_marker FALSE
  #   BOTH     : marker AND has a significant peptide          -> is_marker TRUE
  #   NEITHER  : not a marker, no significant peptide          -> dropped
  stat <- .mk_stat(
    seq = c("pM", "pS", "pB", "pN"),
    acc = c("M_ONLY", "SIG_ONLY", "BOTH", "NEITHER"),
    adjp = c(0.80, 0.001, 0.001, 0.90),  # only pS, pB significant
    row_id = 1:4
  )
  matched <- .mk_matched(
    seq        = c("pM", "pS", "pB", "pN"),
    accession  = c("M_ONLY", "SIG_ONLY", "BOTH", "NEITHER"),
    pep_start  = c(10L, 20L, 30L, 40L),
    row_id     = 1:4
  )

  res <- pelsa_intensity_proteins(
    stat, matched, markers = c("M_ONLY", "BOTH"), contrast = "C1",
    sig_cutoff = 0.05
  )

  expect_s3_class(res, "data.frame")
  expect_setequal(res$accession, c("M_ONLY", "SIG_ONLY", "BOTH"))
  # is_marker flags
  flag <- setNames(res$is_marker, res$accession)
  expect_true(flag[["M_ONLY"]])
  expect_false(flag[["SIG_ONLY"]])
  expect_true(flag[["BOTH"]])   # marker-and-significant -> TRUE
  expect_false("NEITHER" %in% res$accession)
})

test_that("significance is per-accession over the exploded matched_cache (multi-mapped peptide)", {
  # ONE peptide maps to two accessions A;B; the peptide is significant. BOTH A
  # and B inherit significance (each has a matched_cache row for that peptide).
  stat <- data.frame(
    PEP.StrippedSequence = "pSHARED",
    PG.ProteinAccessions = "A;B",
    .row_id              = 1L,
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  stat[["adj.P.Val.C1"]] <- 0.001
  matched <- .mk_matched(
    seq = c("pSHARED", "pSHARED"), accession = c("A", "B"),
    pep_start = c(5L, 15L), row_id = c(1L, 1L)
  )

  res <- pelsa_intensity_proteins(stat, matched, markers = character(0),
                                  contrast = "C1")
  expect_setequal(res$accession, c("A", "B"))
  expect_false(any(res$is_marker))
})

test_that("isoform-symmetric marker matching (marker base, peptide isoform)", {
  stat <- .mk_stat(seq = "pI", acc = "P12345-2", adjp = 0.90, row_id = 1L)
  matched <- .mk_matched("pI", "P12345-2", 7L, 1L)
  res <- pelsa_intensity_proteins(stat, matched, markers = "P12345",
                                  contrast = "C1")
  expect_setequal(res$accession, "P12345-2")
  expect_true(res$is_marker[res$accession == "P12345-2"])
})

test_that("no markers and no significant peptides -> empty (zero-row) result", {
  stat <- .mk_stat(seq = c("a", "b"), acc = c("A", "B"),
                   adjp = c(0.9, 0.8), row_id = 1:2)
  matched <- .mk_matched(c("a", "b"), c("A", "B"), c(1L, 1L), c(1L, 2L))
  res <- pelsa_intensity_proteins(stat, matched, markers = character(0),
                                  contrast = "C1")
  expect_equal(nrow(res), 0L)
  expect_true(all(c("accession", "is_marker") %in% colnames(res)))
})

test_that("pelsa_intensity_proteins is vectorized & fast on ~100k peptides", {
  # P3.7 de-flake: this asserts wall-clock time (a performance smoke), not
  # correctness, so it is non-deterministic on shared/slow CI runners. Skip it
  # there; the correctness of the same call is covered by the other blocks.
  skip_on_cran()
  skip_on_ci()
  set.seed(7)
  N <- 100000L
  acc <- sprintf("P%06d", sample.int(N / 5L, N, replace = TRUE)) # ~20k accessions
  stat <- .mk_stat(
    seq = sprintf("PEP%06d", seq_len(N)), acc = acc,
    adjp = stats::runif(N), row_id = seq_len(N)
  )
  matched <- .mk_matched(
    seq = stat$PEP.StrippedSequence, accession = acc,
    pep_start = rep(10L, N), row_id = seq_len(N)
  )
  elapsed <- system.time(
    res <- pelsa_intensity_proteins(stat, matched, markers = character(0),
                                    contrast = "C1")
  )[["elapsed"]]
  expect_lt(elapsed, 2.0)  # a per-protein loop would blow past this
  expect_true(nrow(res) > 0L)
})

# =============================================================================
# pelsa_intensity_line_data: one protein's tidy line data
# =============================================================================

# A processed (log2) matrix with KNOWN values: 2 conditions x 2 reps each.
# Rows are peptides; .row_id indexes these rows directly.
#   row1 (pSIG)   : A_R1=1, A_R2=3 -> meanA=2 ; B_R1=10, B_R2=12 -> meanB=11
#   row2 (pOTHER) : A_R1=5, A_R2=5 -> meanA=5 ; B_R1=8,  B_R2=NA -> meanB=8 (n=1)
.mk_proc <- function() {
  m <- matrix(
    c(
      1, 3, 10, 12,    # row1 pSIG
      5, 5, 8, NA      # row2 pOTHER (NA in one B replicate)
    ),
    nrow = 2L, byrow = TRUE,
    dimnames = list(NULL, c("A_R1", "A_R2", "B_R1", "B_R2"))
  )
  m
}
.cond_map <- c(A_R1 = "A", A_R2 = "A", B_R1 = "B", B_R2 = "B")
.cond_order <- c("A", "B")

test_that("marker protein -> BOTH peptides present, panel tags correct, y closed-form", {
  proc <- .mk_proc()
  stat <- .mk_stat(
    seq = c("pSIG", "pOTHER"), acc = c("PROT", "PROT"),
    adjp = c(0.001, 0.40), row_id = 1:2   # pSIG significant, pOTHER not
  )
  matched <- .mk_matched(
    seq = c("pSIG", "pOTHER"), accession = c("PROT", "PROT"),
    pep_start = c(100L, 250L), row_id = 1:2
  )

  out <- pelsa_intensity_line_data(
    accession = "PROT", stat_df = stat, matched_cache = matched,
    processed_mat = proc, condition_map = .cond_map,
    condition_order = .cond_order, contrast = "C1", sig_cutoff = 0.05,
    is_marker = TRUE
  )

  # 2 occurrences x 2 conditions = 4 rows
  expect_equal(nrow(out), 4L)
  expect_setequal(unique(out$peptide_seq), c("pSIG", "pOTHER"))

  # panel tags: pSIG Significant, pOTHER Non-significant
  panel_by_pep <- unique(out[, c("peptide_seq", "panel")])
  expect_equal(panel_by_pep$panel[panel_by_pep$peptide_seq == "pSIG"], "Significant")
  expect_equal(panel_by_pep$panel[panel_by_pep$peptide_seq == "pOTHER"], "Non-significant")

  # y closed-form (mean processed log2 AS-IS, na.rm)
  ysig_A <- out$mean_log2[out$peptide_seq == "pSIG" & out$condition == "A"]
  ysig_B <- out$mean_log2[out$peptide_seq == "pSIG" & out$condition == "B"]
  yoth_A <- out$mean_log2[out$peptide_seq == "pOTHER" & out$condition == "A"]
  yoth_B <- out$mean_log2[out$peptide_seq == "pOTHER" & out$condition == "B"]
  expect_equal(ysig_A, 2, tolerance = 1e-8)
  expect_equal(ysig_B, 11, tolerance = 1e-8)
  expect_equal(yoth_A, 5, tolerance = 1e-8)
  expect_equal(yoth_B, 8, tolerance = 1e-8)   # only B_R1 non-NA -> mean = 8

  # n_rep_nonNA correct (the NA in pOTHER B leaves 1)
  n_oth_B <- out$n_rep_nonNA[out$peptide_seq == "pOTHER" & out$condition == "B"]
  expect_equal(n_oth_B, 1L)
  n_sig_A <- out$n_rep_nonNA[out$peptide_seq == "pSIG" & out$condition == "A"]
  expect_equal(n_sig_A, 2L)

  # aa labels from FASTA pep_start
  expect_equal(unique(out$aa_label[out$peptide_seq == "pSIG"]), "aa100")
  expect_equal(unique(out$aa_label[out$peptide_seq == "pOTHER"]), "aa250")
  expect_equal(unique(out$pep_start[out$peptide_seq == "pSIG"]), 100L)

  # condition factor with levels = condition_order
  expect_s3_class(out$condition, "factor")
  expect_identical(levels(out$condition), .cond_order)
})

test_that("non-marker significant protein -> ONLY the significant peptide", {
  proc <- .mk_proc()
  stat <- .mk_stat(
    seq = c("pSIG", "pOTHER"), acc = c("PROT", "PROT"),
    adjp = c(0.001, 0.40), row_id = 1:2
  )
  matched <- .mk_matched(
    seq = c("pSIG", "pOTHER"), accession = c("PROT", "PROT"),
    pep_start = c(100L, 250L), row_id = 1:2
  )

  out <- pelsa_intensity_line_data(
    accession = "PROT", stat_df = stat, matched_cache = matched,
    processed_mat = proc, condition_map = .cond_map,
    condition_order = .cond_order, contrast = "C1", is_marker = FALSE
  )

  # ONLY pSIG, both conditions -> 2 rows; all panel == "Significant"
  expect_equal(nrow(out), 2L)
  expect_setequal(unique(out$peptide_seq), "pSIG")
  expect_true(all(out$panel == "Significant"))
})

test_that("show_all = TRUE shows ALL peptides of a non-marker protein", {
  # The pinned panel wants every peptide mapping to the clicked protein, not just
  # the significant ones. With show_all=TRUE a non-marker protein returns BOTH
  # its significant and non-significant peptides, panel-tagged.
  proc <- .mk_proc()
  stat <- .mk_stat(seq = c("pSIG", "pOTHER"), acc = c("PROT", "PROT"),
                   adjp = c(0.001, 0.40), row_id = 1:2)
  matched <- .mk_matched(seq = c("pSIG", "pOTHER"),
                         accession = c("PROT", "PROT"),
                         pep_start = c(100L, 250L), row_id = 1:2)
  out <- pelsa_intensity_line_data(
    accession = "PROT", stat_df = stat, matched_cache = matched,
    processed_mat = proc, condition_map = .cond_map,
    condition_order = .cond_order, contrast = "C1", is_marker = FALSE,
    show_all = TRUE
  )
  expect_setequal(unique(out$peptide_seq), c("pSIG", "pOTHER"))   # BOTH shown
  panel_by_pep <- unique(out[, c("peptide_seq", "panel")])
  expect_equal(panel_by_pep$panel[panel_by_pep$peptide_seq == "pSIG"],
               "Significant")
  expect_equal(panel_by_pep$panel[panel_by_pep$peptide_seq == "pOTHER"],
               "Non-significant")
})

test_that("a peptide with 2 occurrences -> 2 distinct lines (distinct pep_start/aa_label)", {
  # ONE significant peptide that occurs TWICE in the protein (two matched rows,
  # same .row_id, distinct pep_start / pep_occurrence_idx). Both share the SAME
  # processed_mat row (same .row_id -> same y), but are DISTINCT lines.
  proc <- matrix(
    c(1, 3, 10, 12),  # single peptide row
    nrow = 1L, byrow = TRUE,
    dimnames = list(NULL, c("A_R1", "A_R2", "B_R1", "B_R2"))
  )
  stat <- .mk_stat(seq = "pDUP", acc = "DUPPROT", adjp = 0.001, row_id = 1L)
  matched <- .mk_matched(
    seq = c("pDUP", "pDUP"), accession = c("DUPPROT", "DUPPROT"),
    pep_start = c(4L, 19L), row_id = c(1L, 1L),
    pep_occurrence_idx = c(1L, 2L)
  )

  out <- pelsa_intensity_line_data(
    accession = "DUPPROT", stat_df = stat, matched_cache = matched,
    processed_mat = proc, condition_map = .cond_map,
    condition_order = .cond_order, contrast = "C1", is_marker = FALSE
  )

  # 2 occurrences x 2 conditions = 4 rows; 2 distinct lines
  expect_equal(nrow(out), 4L)
  lines <- unique(out[, c("pep_occurrence_idx", "pep_start", "aa_label")])
  expect_equal(nrow(lines), 2L)
  expect_setequal(lines$pep_start, c(4L, 19L))
  expect_setequal(lines$aa_label, c("aa4", "aa19"))

  # both occurrences share the same y (same .row_id row)
  occ1_A <- out$mean_log2[out$pep_occurrence_idx == 1L & out$condition == "A"]
  occ2_A <- out$mean_log2[out$pep_occurrence_idx == 2L & out$condition == "A"]
  expect_equal(occ1_A, occ2_A, tolerance = 1e-8)
  expect_equal(occ1_A, 2, tolerance = 1e-8)
})

test_that("output column contract + condition order respected", {
  proc <- .mk_proc()
  stat <- .mk_stat(c("pSIG", "pOTHER"), c("PROT", "PROT"), c(0.001, 0.4), row_id = 1:2)
  matched <- .mk_matched(c("pSIG", "pOTHER"), c("PROT", "PROT"),
                         c(100L, 250L), 1:2)
  out <- pelsa_intensity_line_data(
    "PROT", stat, matched, proc, condition_map = .cond_map,
    condition_order = .cond_order, contrast = "C1", is_marker = FALSE
  )
  expected <- c("accession", "peptide_seq", "pep_start", "pep_occurrence_idx",
                "aa_label", "panel", "condition", "mean_log2", "n_rep_nonNA")
  expect_true(all(expected %in% colnames(out)))
  # the single sig line: condition order A then B
  expect_equal(as.character(out$condition), c("A", "B"))
})

test_that("condition with no samples in the map is dropped from the x-axis", {
  # condition_order names a third condition "C" that has NO sample columns.
  proc <- .mk_proc()
  stat <- .mk_stat("pSIG", "PROT", 0.001, row_id = 1L)
  matched <- .mk_matched("pSIG", "PROT", 100L, 1L)
  out <- pelsa_intensity_line_data(
    "PROT", stat, matched, proc, condition_map = .cond_map,
    condition_order = c("A", "B", "C"), contrast = "C1", is_marker = FALSE
  )
  # only A,B have samples -> 2 rows; "C" dropped
  expect_setequal(as.character(out$condition), c("A", "B"))
  expect_equal(nrow(out), 2L)
  # but factor levels still follow the full requested order (C retained as level)
  expect_identical(levels(out$condition), c("A", "B", "C"))
})

# ---- row-alignment fallback: rownames join ----------------------------------

test_that("rownames join is used when processed_mat has peptide-id rownames", {
  # No usable .row_id alignment desired: rows are NOT in .row_id order; the join
  # keys on rownames(processed_mat) == matched_cache PEP.StrippedSequence.
  proc <- matrix(
    c(
      5, 5, 8, 8,      # pOTHER row first
      1, 3, 10, 12     # pSIG row second
    ),
    nrow = 2L, byrow = TRUE,
    dimnames = list(c("pOTHER", "pSIG"), c("A_R1", "A_R2", "B_R1", "B_R2"))
  )
  # stat_df + matched_cache carry NO .row_id -> force the rownames key.
  stat <- data.frame(
    PEP.StrippedSequence = c("pSIG", "pOTHER"),
    PG.ProteinAccessions = c("PROT", "PROT"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  stat[["adj.P.Val.C1"]] <- c(0.001, 0.40)
  matched <- data.frame(
    PEP.StrippedSequence = c("pSIG", "pOTHER"),
    accession = c("PROT", "PROT"),
    pep_start = c(100L, 250L),
    pep_occurrence_idx = c(1L, 1L),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  out <- pelsa_intensity_line_data(
    "PROT", stat, matched, proc, condition_map = .cond_map,
    condition_order = .cond_order, contrast = "C1", is_marker = FALSE
  )
  # pSIG row (rownames "pSIG") -> meanA = 2, meanB = 11 via the rownames join.
  expect_equal(out$mean_log2[out$condition == "A"], 2, tolerance = 1e-8)
  expect_equal(out$mean_log2[out$condition == "B"], 11, tolerance = 1e-8)
})

# ---- boundary validation -----------------------------------------------------

test_that("missing accession in matched_cache errors", {
  proc <- .mk_proc()
  stat <- .mk_stat("pSIG", "PROT", 0.001, row_id = 1L)
  matched <- .mk_matched("pSIG", "PROT", 100L, 1L)
  expect_error(
    pelsa_intensity_line_data("ABSENT", stat, matched, proc,
                              condition_map = .cond_map,
                              condition_order = .cond_order, contrast = "C1"),
    regexp = "accession|ABSENT|not found"
  )
})

test_that("non-numeric processed_mat errors", {
  stat <- .mk_stat("pSIG", "PROT", 0.001, row_id = 1L)
  matched <- .mk_matched("pSIG", "PROT", 100L, 1L)
  bad <- matrix("x", nrow = 1L, ncol = 4L,
                dimnames = list(NULL, c("A_R1", "A_R2", "B_R1", "B_R2")))
  expect_error(
    pelsa_intensity_line_data("PROT", stat, matched, bad,
                              condition_map = .cond_map,
                              condition_order = .cond_order, contrast = "C1"),
    regexp = "numeric|matrix"
  )
})

test_that("condition_map not covering processed_mat columns errors", {
  proc <- .mk_proc()
  stat <- .mk_stat("pSIG", "PROT", 0.001, row_id = 1L)
  matched <- .mk_matched("pSIG", "PROT", 100L, 1L)
  bad_map <- c(A_R1 = "A", A_R2 = "A", B_R1 = "B") # missing B_R2
  expect_error(
    pelsa_intensity_line_data("PROT", stat, matched, proc,
                              condition_map = bad_map,
                              condition_order = .cond_order, contrast = "C1"),
    regexp = "condition_map|cover|column"
  )
})

test_that("missing contrast adj.P.Val column errors", {
  proc <- .mk_proc()
  stat <- .mk_stat("pSIG", "PROT", 0.001, row_id = 1L)  # has adj.P.Val.C1
  matched <- .mk_matched("pSIG", "PROT", 100L, 1L)
  expect_error(
    pelsa_intensity_line_data("PROT", stat, matched, proc,
                              condition_map = .cond_map,
                              condition_order = .cond_order, contrast = "NOPE"),
    regexp = "adj.P.Val|contrast|column"
  )
})

# ---- empty/populated shape parity (.pelsa_intensity_empty) ------------------

test_that("non-marker protein whose only peptide is non-sig -> zero rows, full contract", {
  # Non-marker protein, single peptide, NOT significant -> no occurrence kept ->
  # the .pelsa_intensity_empty() path. Locks empty/populated column + level parity.
  proc <- .mk_proc()
  stat <- .mk_stat("pNS", "PROT", 0.40, row_id = 1L)   # adj.P.Val 0.40 -> not sig
  matched <- .mk_matched("pNS", "PROT", 100L, 1L)

  out <- pelsa_intensity_line_data(
    "PROT", stat, matched, proc, condition_map = .cond_map,
    condition_order = .cond_order, contrast = "C1", is_marker = FALSE
  )

  expect_equal(nrow(out), 0L)
  expect_identical(levels(out$condition), .cond_order)
  expected <- c("accession", "peptide_seq", "pep_start", "pep_end",
                "pep_occurrence_idx", "aa_label", "panel", "condition",
                "mean_log2", "n_rep_nonNA")
  expect_identical(colnames(out), expected)
  expect_s3_class(out$condition, "factor")
})

# ---- row-alignment guard: both join keys missing ----------------------------

test_that("both join keys missing -> clear error", {
  # processed_mat has NO rownames AND neither frame carries .row_id -> there is
  # no usable peptide<->row key, so the row-index resolver must error loudly.
  proc <- matrix(
    c(1, 3, 10, 12), nrow = 1L, byrow = TRUE,
    dimnames = list(NULL, c("A_R1", "A_R2", "B_R1", "B_R2"))  # no rownames
  )
  stat <- data.frame(
    PEP.StrippedSequence = "pSIG",
    PG.ProteinAccessions = "PROT",
    stringsAsFactors = FALSE, check.names = FALSE
  )
  stat[["adj.P.Val.C1"]] <- 0.001
  matched <- data.frame(
    PEP.StrippedSequence = "pSIG", accession = "PROT",
    pep_start = 100L, pep_occurrence_idx = 1L,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_error(
    pelsa_intensity_line_data("PROT", stat, matched, proc,
                              condition_map = .cond_map,
                              condition_order = .cond_order, contrast = "C1"),
    regexp = "align|\\.row_id|rownames"
  )
})

# ---- row-alignment guard: out-of-range .row_id falls back to rownames -------

test_that("out-of-range .row_id falls back to the rownames join", {
  # matched_cache carries a .row_id (5) that exceeds nrow(processed_mat) (1), so
  # the primary key is rejected and the resolver falls back to peptide-id
  # rownames. The correct y still resolves via the rownames key.
  proc <- matrix(
    c(1, 3, 10, 12), nrow = 1L, byrow = TRUE,
    dimnames = list("pSIG", c("A_R1", "A_R2", "B_R1", "B_R2"))  # rownamed
  )
  stat <- .mk_stat("pSIG", "PROT", 0.001, row_id = 5L)          # out-of-range
  matched <- .mk_matched("pSIG", "PROT", 100L, row_id = 5L)     # out-of-range

  out <- pelsa_intensity_line_data(
    "PROT", stat, matched, proc, condition_map = .cond_map,
    condition_order = .cond_order, contrast = "C1", is_marker = FALSE
  )
  # Falls back to rownames("pSIG") -> meanA = 2, meanB = 11.
  expect_equal(out$mean_log2[out$condition == "A"], 2, tolerance = 1e-8)
  expect_equal(out$mean_log2[out$condition == "B"], 11, tolerance = 1e-8)
})

test_that("out-of-range .row_id AND no rownames -> clear error (guard pinned)", {
  proc <- matrix(
    c(1, 3, 10, 12), nrow = 1L, byrow = TRUE,
    dimnames = list(NULL, c("A_R1", "A_R2", "B_R1", "B_R2"))  # no rownames
  )
  stat <- .mk_stat("pSIG", "PROT", 0.001, row_id = 5L)
  matched <- .mk_matched("pSIG", "PROT", 100L, row_id = 5L)
  expect_error(
    pelsa_intensity_line_data("PROT", stat, matched, proc,
                              condition_map = .cond_map,
                              condition_order = .cond_order, contrast = "C1"),
    regexp = "align|\\.row_id|rownames"
  )
})

# ---- STATIC export intensity line plot: left margin ------------------------
# A long, rotated leftmost condition label (e.g. "AY9944_U18666A_DMSO") clipped
# off the left panel edge. The export builder must reserve a left plot.margin
# gutter larger than ggplot's 5.5pt default so the label is fully visible.

.intensity_export_ld <- function() {
  conds <- c("AY9944_U18666A_DMSO", "AY9944_1uM", "AY9944_10uM")
  data.frame(
    condition = factor(rep(conds, times = 2L), levels = conds),
    mean_log2 = c(9.7, 3.7, 4.0, 12.3, 12.2, 12.1),
    peptide_seq = rep(c("pA", "pB"), each = 3L),
    pep_occurrence_idx = 1L,
    panel = rep(c("Significant", "Non-significant"), each = 3L),
    aa_label = rep(c("aa462", "aa14"), each = 3L),
    stringsAsFactors = FALSE)
}

test_that("intensity export reserves an enlarged left plot.margin", {
  g <- pelsa_intensity_export_ggplot(.intensity_export_ld(),
                                     gene = "DHCR7", accession = "Q9UBM7")
  m <- g$theme$plot.margin
  expect_false(is.null(m))                      # margin explicitly set
  # ggplot default is unit(5.5, "pt") on every side; left must exceed that.
  left_pt <- as.numeric(grid::convertUnit(m[4], "pt"))
  expect_gt(left_pt, 5.5)
})

# ---- in-app two-panel intensity plot: panel titles --------------------------
# The two-panel pinned intensity plot must label each panel with the full,
# unambiguous wording AT THE TOP of its own panel. plotly::subplot collapses a
# per-plot ggtitle to a single overall layout$title (the Significant title was
# silently dropped, the Non-significant title rendered as one centered top
# title), so the titles must be paper-referenced subplot annotations instead.

.intensity_inapp_ld <- function() {
  conds <- c("DMSO", "1uM", "10uM")
  data.frame(
    condition = factor(rep(conds, times = 2L), levels = conds),
    mean_log2 = c(7.5, 3.5, 7.1, 3.5, 3.7, 4.5),
    peptide_seq = rep(c("AEIITVSDGR", "pOTHER"), each = 3L),
    pep_start = rep(c(162L, 50L), each = 3L),
    pep_end   = rep(c(171L, 59L), each = 3L),
    pep_occurrence_idx = 1L,
    panel = rep(c("Significant", "Non-significant"), each = 3L),
    aa_label = rep(c("aa162", "aa50"), each = 3L),
    n_rep_nonNA = 3L,
    stringsAsFactors = FALSE)
}

test_that("two-panel intensity plot titles each panel 'Significant/Non-significant in selected contrast' at the panel top", {
  p <- pelsa_intensity_line_plot(.intensity_inapp_ld(), pinned_label = "aa162")
  b <- plotly::plotly_build(p)
  anns <- b$x$layout$annotations
  texts <- vapply(anns, function(a) a$text %||% "", character(1))

  expect_true("Significant in selected contrast" %in% texts)
  expect_true("Non-significant in selected contrast" %in% texts)

  # subplot must NOT collapse a panel title into a single overall plot title.
  ttl <- b$x$layout$title$text %||% ""
  expect_false(grepl("Non-significant", ttl))

  # Each title sits at the TOP of its own panel (Significant on top -> higher y;
  # Non-significant below -> lower y), both paper-referenced.
  get_y <- function(t) {
    a <- anns[[which(texts == t)[1]]]
    expect_identical(a$yref, "paper")
    a$y
  }
  y_sig <- get_y("Significant in selected contrast")
  y_ns  <- get_y("Non-significant in selected contrast")
  expect_gt(y_sig, y_ns)              # Significant panel title is higher
  expect_gt(y_sig, 0.5)               # in the upper (top) panel
  expect_lt(y_ns, 0.5)               # in the lower (bottom) panel
})

# =============================================================================
# Integration: generator -> explode -> FASTA-map -> build line data
# =============================================================================

test_that("integration: build line data for a synthetic protein with 2 occurrences", {
  syn <- pelsa_make_synthetic(seed = 1)
  contrast <- syn$contrasts[1]

  exploded <- pelsa_explode_accessions(syn$peptides)
  matched <- pelsa_map_peptide_positions(exploded, syn$fasta)$matched

  # stat frame = the per-peptide frame + stable .row_id aligned to matched .row_id
  stat <- syn$peptides
  stat$.row_id <- seq_len(nrow(stat))

  # processed-like matrix = the intensity block, rows aligned to stat .row_id.
  proc <- as.matrix(syn$peptides[, syn$sample_cols, drop = FALSE])

  # condition_map / order from Setup.
  condition_map <- syn$condition_map
  condition_order <- unique(unname(condition_map))

  # DUPPROT carries the dup peptide at TWO occurrences (starts 4 & 19). Force it
  # significant so it qualifies as a non-marker sig protein with 2 lines.
  dup_rows <- which(stat$PEP.StrippedSequence == syn$dup_peptide)
  stat[[paste0("adj.P.Val.", contrast)]][dup_rows] <- 0.001

  protein_set <- pelsa_intensity_proteins(
    stat, matched, markers = character(0), contrast = contrast
  )
  expect_true("DUPPROT" %in% protein_set$accession)

  out <- pelsa_intensity_line_data(
    accession = "DUPPROT", stat_df = stat, matched_cache = matched,
    processed_mat = proc, condition_map = condition_map,
    condition_order = condition_order, contrast = contrast,
    is_marker = FALSE
  )

  # Column contract.
  expected <- c("accession", "peptide_seq", "pep_start", "pep_occurrence_idx",
                "aa_label", "panel", "condition", "mean_log2", "n_rep_nonNA")
  expect_true(all(expected %in% colnames(out)))

  # The dup peptide -> two occurrences -> two distinct lines (starts 4 & 19).
  lines <- unique(out[, c("pep_occurrence_idx", "pep_start", "aa_label")])
  expect_equal(nrow(lines), 2L)
  expect_setequal(lines$pep_start, syn$dup_peptide_starts)
  expect_setequal(lines$aa_label, paste0("aa", syn$dup_peptide_starts))

  # x-axis = condition_order, factor levels respected.
  expect_s3_class(out$condition, "factor")
  expect_identical(levels(out$condition), condition_order)

  # one row per (occurrence, condition) present in the map.
  n_cond <- length(intersect(condition_order, unname(condition_map)))
  expect_equal(nrow(out), 2L * n_cond)

  # marker integration: ISOPEPTIDEK on P12345-2; marker on the BASE accession.
  pset_mk <- pelsa_intensity_proteins(
    stat, matched, markers = syn$isoform_base_accession, contrast = contrast
  )
  expect_true("P12345-2" %in% pset_mk$accession)
  expect_true(pset_mk$is_marker[pset_mk$accession == "P12345-2"])
})

# =============================================================================
# Empty condition intersection -> full-contract empty frame (not malformed list)
# =============================================================================
test_that("pelsa_intensity_line_data returns the empty frame when no condition matches", {
  # Regression: when condition_order is disjoint from the data's conditions,
  # conditions_present is empty -> parts is empty -> do.call(rbind, list()) is
  # NULL -> out$condition <- factor(...) coerced `out` into a malformed bare
  # list, dropping the contracted columns. It must return .pelsa_intensity_empty
  # (a zero-row data.frame with the full column contract) instead.
  proc <- .mk_proc()
  stat <- .mk_stat(seq = c("pSIG", "pOTHER"), acc = c("PROT", "PROT"),
                   adjp = c(0.001, 0.40), row_id = 1:2)
  matched <- .mk_matched(seq = c("pSIG", "pOTHER"),
                         accession = c("PROT", "PROT"),
                         pep_start = c(100L, 250L), row_id = 1:2)

  out <- pelsa_intensity_line_data(
    accession = "PROT", stat_df = stat, matched_cache = matched,
    processed_mat = proc, condition_map = .cond_map,
    condition_order = c("ZZZ"),          # disjoint from the data's A/B
    contrast = "C1", sig_cutoff = 0.05, is_marker = TRUE
  )

  expect_s3_class(out, "data.frame")     # NOT a bare list
  expect_equal(nrow(out), 0L)
  # full column contract preserved (matches .pelsa_intensity_empty)
  expect_setequal(
    colnames(out),
    c("accession", "peptide_seq", "pep_start", "pep_end", "pep_occurrence_idx",
      "aa_label", "panel", "condition", "mean_log2", "n_rep_nonNA")
  )
  expect_s3_class(out$condition, "factor")
})

# =============================================================================
# pelsa_plotted_intensities_df: sig_cutoff is parameterized (shared cutoff)
# =============================================================================

test_that("pelsa_plotted_intensities_df honors a sig_cutoff parameter", {
  # Two NON-marker proteins; the only thing that decides inclusion is the
  # significance cutoff applied to each peptide's adj.P.Val. P_MID's peptide sits
  # at 0.03: included at cutoff 0.05, excluded at cutoff 0.01. A hardcoded 0.05
  # would ignore the argument and always include it.
  proc <- matrix(
    c(1, 3, 10, 12,    # row1 P_MID peptide
      5, 5, 8,  9),    # row2 P_HIGH peptide
    nrow = 2L, byrow = TRUE,
    dimnames = list(NULL, c("A_R1", "A_R2", "B_R1", "B_R2"))
  )
  stat <- .mk_stat(
    seq = c("pMID", "pHIGH"), acc = c("P_MID", "P_HIGH"),
    adjp = c(0.03, 0.30), row_id = 1:2
  )
  matched <- .mk_matched(
    seq = c("pMID", "pHIGH"), accession = c("P_MID", "P_HIGH"),
    pep_start = c(100L, 250L), row_id = 1:2
  )

  loose <- pelsa_plotted_intensities_df(
    stat_raw = stat, matched = matched, markers = character(0),
    contrast = "C1", pm = proc, cmap = .cond_map, corder = .cond_order,
    sig_cutoff = 0.05
  )
  expect_s3_class(loose, "data.frame")
  expect_setequal(unique(loose$accession), "P_MID")  # 0.03 < 0.05

  strict <- pelsa_plotted_intensities_df(
    stat_raw = stat, matched = matched, markers = character(0),
    contrast = "C1", pm = proc, cmap = .cond_map, corder = .cond_order,
    sig_cutoff = 0.01
  )
  expect_null(strict)  # 0.03 is NOT < 0.01 -> no protein qualifies -> NULL
})

test_that("pelsa_plotted_intensities_df sig_cutoff defaults to the export constant", {
  default_cut <- get(".PELSA_EXPORT_SIG_CUTOFF", envir = asNamespace("Protigy"))
  expect_equal(default_cut, 0.05)
  proc <- matrix(
    c(1, 3, 10, 12), nrow = 1L, byrow = TRUE,
    dimnames = list(NULL, c("A_R1", "A_R2", "B_R1", "B_R2"))
  )
  stat <- .mk_stat(seq = "pMID", acc = "P_MID", adjp = 0.03, row_id = 1L)
  matched <- .mk_matched("pMID", "P_MID", 100L, 1L)
  # No sig_cutoff arg -> default constant (0.05) -> 0.03 qualifies.
  out <- pelsa_plotted_intensities_df(
    stat_raw = stat, matched = matched, markers = character(0),
    contrast = "C1", pm = proc, cmap = .cond_map, corder = .cond_order
  )
  expect_s3_class(out, "data.frame")
  expect_setequal(unique(out$accession), "P_MID")
})

# =============================================================================
# sig_cutoff default sources the shared constant (single source of truth)
#
# The pure helpers cannot read the reactive stat_params() cutoff themselves;
# live module callers thread isolate(sig_cutoff_r()) explicitly (matching the
# volcano). The DEFAULT must REFERENCE the shared constant .PELSA_EXPORT_SIG_CUTOFF
# rather than a stray literal 0.05, so the constant is the single source of truth
# at every layer. Asserting the default EXPRESSION is the constant's symbol makes
# reverting to `sig_cutoff = 0.05` fail (a value-only check would not, since
# 0.05 == the constant).
# =============================================================================

test_that("pelsa_intensity_proteins default sig_cutoff is the shared constant symbol", {
  expect_identical(formals(Protigy:::pelsa_intensity_proteins)$sig_cutoff,
                   as.symbol(".PELSA_EXPORT_SIG_CUTOFF"))
})

test_that("pelsa_intensity_line_data default sig_cutoff is the shared constant symbol", {
  expect_identical(formals(Protigy:::pelsa_intensity_line_data)$sig_cutoff,
                   as.symbol(".PELSA_EXPORT_SIG_CUTOFF"))
})
