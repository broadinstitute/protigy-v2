################################################################################
# Tests for pelsa_build_volcano_df() — the PELSA volcano data-frame builder (3A).
#
# ONE tidy row per SOURCE peptide for the all-peptide panel (no explode), with:
#   - id / logFC / adj.P.Val / P.Value / logP (mirrors build_volcano_df),
#   - Significant (adj.P.Val < sig_cutoff),
#   - sig_direction {up,down,ns} + sig_color (TWO-SIDED: up=darkred, down=blue,
#     ns=gray) — Decision #4,
#   - feature_class_primary / feature_color (via 2I pelsa_annotate_features),
#   - label (;-joined multilabel via 2C pelsa_build_multilabel, accession
#     fallback when the gene token is empty),
#   - is_marker (via 2J pelsa_match_markers, isoform-symmetric),
#   - winning_accession / winning_gene, PG.ProteinAccessions / PG.Genes,
#     pep_start / pep_end carried for tooltips,
#   - attr(df, "y_cutoff") = empirical raw-p at adj.P.Val == sig_cutoff.
#
# The best-peptide panel reuses the 2G rollup (one dot per distinct best
# peptide). Closed-form hand-built frames first; then a generator integration.
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# Convenience: a hand-built per-peptide stat frame carrying contrast-suffixed
# Statistics-tab columns (logFC.<c> / adj.P.Val.<c> / P.Value.<c>) plus the
# peptide-identity columns the annotation + marker helpers read. pep_start /
# pep_end are the representative (leading) span on the stat frame; the matched
# cache carries the per-occurrence detail.
.make_stat_df <- function(seq, acc, genes, logfc, adjp, pval,
                          pep_start, pep_end, contrast = "C1",
                          row_id = NULL) {
  df <- data.frame(
    PEP.StrippedSequence = seq,
    PG.ProteinAccessions = acc,
    PG.Genes             = genes,
    pep_start            = pep_start,
    pep_end              = pep_end,
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  df[[paste0("logFC.", contrast)]]     <- logfc
  df[[paste0("adj.P.Val.", contrast)]] <- adjp
  df[[paste0("P.Value.", contrast)]]   <- pval
  if (!is.null(row_id)) df$.row_id <- row_id
  df
}

# A matched-cache frame: one row per (peptide, accession, occurrence).
.make_matched <- function(seq, accession, gene, pep_start, pep_end,
                          row_id = NULL) {
  df <- data.frame(
    PEP.StrippedSequence = seq,
    accession            = accession,
    gene                 = gene,
    pep_start            = pep_start,
    pep_end              = pep_end,
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  if (!is.null(row_id)) df$.row_id <- row_id
  df
}

# A 1-feature feat_df: ACC has a folded_domain over [s, e].
.make_feat <- function(accession, start, end, feature_class) {
  data.frame(
    accession     = accession,
    start         = start,
    end           = end,
    feature_class = feature_class,
    stringsAsFactors = FALSE
  )
}

# --- sig_direction / sig_color: TWO-SIDED ------------------------------------

test_that("significant up peptide -> up / darkred", {
  stat <- .make_stat_df(
    seq = "PEPUP", acc = "ACC1", genes = "GUP",
    logfc = 2.0, adjp = 0.001, pval = 0.0005,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPUP", "ACC1", "GUP", 10L, 14L, row_id = 1L)

  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1")

  expect_equal(nrow(out), 1L)
  expect_true(out$Significant)
  expect_equal(out$sig_direction, "up")
  expect_equal(out$sig_color, "darkred")
})

test_that("significant down peptide -> down / blue (two-sided, not filtered)", {
  stat <- .make_stat_df(
    seq = "PEPDN", acc = "ACC1", genes = "GDN",
    logfc = -2.0, adjp = 0.001, pval = 0.0005,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPDN", "ACC1", "GDN", 10L, 14L, row_id = 1L)

  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1")

  expect_equal(out$sig_direction, "down")
  expect_equal(out$sig_color, "#1f4e9c")
  expect_true(out$Significant)
})

test_that("non-significant peptide -> ns / gray", {
  stat <- .make_stat_df(
    seq = "PEPNS", acc = "ACC1", genes = "GNS",
    logfc = 2.0, adjp = 0.50, pval = 0.40,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPNS", "ACC1", "GNS", 10L, 14L, row_id = 1L)

  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1")

  expect_false(out$Significant)
  expect_equal(out$sig_direction, "ns")
  expect_equal(out$sig_color, "gray")
})

test_that("logP = -log10(P.Value)", {
  stat <- .make_stat_df(
    seq = "PEPP", acc = "ACC1", genes = "GP",
    logfc = 1.0, adjp = 0.20, pval = 0.01,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPP", "ACC1", "GP", 10L, 14L, row_id = 1L)
  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1")
  expect_equal(out$logP, -log10(0.01))
})

# --- feature color via 2I -----------------------------------------------------

test_that("peptide overlapping a known feature gets that feature class + color", {
  # ACC1 has a folded_domain over [5,30]; peptide spans [10,14] -> overlap.
  feat <- .make_feat("ACC1", 5L, 30L, "folded_domain")
  stat <- .make_stat_df(
    seq = "PEPFEAT", acc = "ACC1", genes = "GF",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPFEAT", "ACC1", "GF", 10L, 14L, row_id = 1L)

  out <- pelsa_build_volcano_df(stat, matched, feat, markers = character(0),
                                contrast = "C1")

  expect_equal(out$feature_class_primary, "folded_domain")
  expect_equal(out$feature_color, unname(PELSA_FEATURE_COLORS["folded_domain"]))
})

test_that("peptide with no overlapping feature -> none + grey", {
  feat <- .make_feat("OTHER", 5L, 30L, "folded_domain")
  stat <- .make_stat_df(
    seq = "PEPNOFEAT", acc = "ACC1", genes = "GNF",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPNOFEAT", "ACC1", "GNF", 10L, 14L, row_id = 1L)

  out <- pelsa_build_volcano_df(stat, matched, feat, markers = character(0),
                                contrast = "C1")

  expect_equal(out$feature_class_primary, "none")
  expect_equal(out$feature_color, unname(PELSA_FEATURE_COLORS["none"]))
})

# --- label via 2C multilabel --------------------------------------------------

test_that("peptide mapping to 2 accessions/genes -> ;-joined multilabel", {
  # One source peptide row, 2 ;-accessions / 2 ;-genes; matched cache has 2 rows.
  stat <- .make_stat_df(
    seq = "PEPAB", acc = "ACCA;ACCB", genes = "GA;GB",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched(
    seq = c("PEPAB", "PEPAB"),
    accession = c("ACCA", "ACCB"),
    gene = c("GA", "GB"),
    pep_start = c(10L, 20L),
    pep_end = c(14L, 24L),
    row_id = c(1L, 1L)
  )

  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1")

  expect_equal(nrow(out), 1L)
  expect_equal(out$label, "GA_aa10;GB_aa20")
})

test_that("empty gene token -> label uses the accession (2C/2I fallback)", {
  stat <- .make_stat_df(
    seq = "PEPNOG", acc = "NOGENEACC", genes = "",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 5L, pep_end = 9L, row_id = 1L
  )
  matched <- .make_matched("PEPNOG", "NOGENEACC", "", 5L, 9L, row_id = 1L)

  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1")

  expect_equal(out$label, "NOGENEACC_aa5")
})

test_that("is_self_curated forces <accession>_aa<pos> label even when a gene is present", {
  # A self-curated run whose input report DID carry a gene must still label by
  # accession (genes are ignored for self-curated species) and blank winning_gene.
  stat <- .make_stat_df(
    seq = "PEPSC", acc = "BalskusLab_HoyT_0001", genes = "SOMEGENE",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 7L, pep_end = 11L, row_id = 1L
  )
  matched <- .make_matched("PEPSC", "BalskusLab_HoyT_0001", "SOMEGENE",
                           7L, 11L, row_id = 1L)

  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1",
                                is_self_curated = TRUE)

  expect_equal(out$label, "BalskusLab_HoyT_0001_aa7")
  expect_equal(out$winning_gene, "")
})

test_that(".pelsa_volcano_labels(is_self_curated=TRUE) forces accession over gene", {
  matched <- .make_matched("PEPSC", "ACC1", "GENE1", 7L, 11L, row_id = 1L)
  lab_uniprot <- .pelsa_volcano_labels(matched, ".row_id")
  lab_self    <- .pelsa_volcano_labels(matched, ".row_id", is_self_curated = TRUE)
  expect_equal(lab_uniprot$label, "GENE1_aa7")
  expect_equal(lab_self$label, "ACC1_aa7")
})

# --- is_marker via 2J ---------------------------------------------------------

test_that("peptide on a marker accession (incl. isoform) -> is_marker TRUE", {
  stat <- .make_stat_df(
    seq = "PEPMK", acc = "P99999-2", genes = "GMK",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 5L, pep_end = 9L, row_id = 1L
  )
  matched <- .make_matched("PEPMK", "P99999-2", "GMK", 5L, 9L, row_id = 1L)

  # Marker entered as the BASE accession; isoform-symmetric match must flag it.
  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = c("P99999"), contrast = "C1")

  expect_true(out$is_marker)
})

test_that("peptide not on any marker -> is_marker FALSE", {
  stat <- .make_stat_df(
    seq = "PEPNM", acc = "ACC1", genes = "GNM",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 5L, pep_end = 9L, row_id = 1L
  )
  matched <- .make_matched("PEPNM", "ACC1", "GNM", 5L, 9L, row_id = 1L)
  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = c("P99999"), contrast = "C1")
  expect_false(out$is_marker)
})

# --- one row per peptide (all_peptide, no explode) ---------------------------

test_that("shared peptide A;B;C -> ONE row in all_peptide panel", {
  stat <- .make_stat_df(
    seq = "PEPABC", acc = "A;B;C", genes = "GA;GB;GC",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 5L, pep_end = 9L, row_id = 1L
  )
  matched <- .make_matched(
    seq = rep("PEPABC", 3L),
    accession = c("A", "B", "C"),
    gene = c("GA", "GB", "GC"),
    pep_start = c(5L, 15L, 25L),
    pep_end = c(9L, 19L, 29L),
    row_id = c(1L, 1L, 1L)
  )

  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1",
                                opts = list(panel = "all_peptide"))

  expect_equal(nrow(out), 1L)
  expect_equal(out$label, "GA_aa5;GB_aa15;GC_aa25")
})

# --- contrast = NULL (already-renamed columns) -------------------------------

test_that("contrast=NULL reads already-renamed logFC/adj.P.Val/P.Value", {
  stat <- data.frame(
    PEP.StrippedSequence = "PEPR",
    PG.ProteinAccessions = "ACC1",
    PG.Genes             = "GR",
    pep_start            = 10L,
    pep_end              = 14L,
    logFC                = 2.0,
    adj.P.Val            = 0.001,
    P.Value              = 0.0005,
    .row_id              = 1L,
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  matched <- .make_matched("PEPR", "ACC1", "GR", 10L, 14L, row_id = 1L)
  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = NULL)
  expect_equal(out$sig_direction, "up")
  expect_equal(out$logFC, 2.0)
})

# --- y_cutoff attribute -------------------------------------------------------

test_that("y_cutoff attribute = empirical raw-p at adj.P.Val == sig_cutoff", {
  # Two sig (adj.P.Val < 0.05) peptides: max raw-p among them is 0.02.
  stat <- .make_stat_df(
    seq = c("S1", "S2", "NS1"),
    acc = c("A1", "A2", "A3"),
    genes = c("G1", "G2", "G3"),
    logfc = c(1, -1, 1),
    adjp = c(0.01, 0.04, 0.50),
    pval = c(0.005, 0.02, 0.40),
    pep_start = c(10L, 10L, 10L),
    pep_end = c(14L, 14L, 14L),
    row_id = c(1L, 2L, 3L)
  )
  matched <- .make_matched(
    seq = c("S1", "S2", "NS1"),
    accession = c("A1", "A2", "A3"),
    gene = c("G1", "G2", "G3"),
    pep_start = c(10L, 10L, 10L),
    pep_end = c(14L, 14L, 14L),
    row_id = c(1L, 2L, 3L)
  )

  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1",
                                opts = list(sig_cutoff = 0.05))

  expect_equal(attr(out, "y_cutoff"), -log10(0.02))
})

# --- best_peptide panel reuses 2G rollup -------------------------------------

test_that("best_peptide panel uses 2G rollup (one dot per distinct best-peptide)", {
  # Two accessions A,B; peptide BEST wins both with the smallest adj.P.Val.
  # all_peptide would emit 2 source-peptide rows; best_peptide rolls BEST to 1.
  stat <- .make_stat_df(
    seq = c("BEST", "OTHER"),
    acc = c("A;B", "A"),
    genes = c("GA;GB", "GA"),
    logfc = c(-2.0, 0.5),
    adjp = c(0.001, 0.30),
    pval = c(0.0005, 0.20),
    pep_start = c(10L, 50L),
    pep_end = c(14L, 54L),
    row_id = c(1L, 2L)
  )
  matched <- .make_matched(
    seq = c("BEST", "BEST", "OTHER"),
    accession = c("A", "B", "A"),
    gene = c("GA", "GB", "GA"),
    pep_start = c(10L, 20L, 50L),
    pep_end = c(14L, 24L, 54L),
    row_id = c(1L, 1L, 2L)
  )

  all_out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                    markers = character(0), contrast = "C1",
                                    opts = list(panel = "all_peptide"))
  best_out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                     markers = character(0), contrast = "C1",
                                     opts = list(panel = "best_peptide"))

  # all_peptide: one row per source peptide (BEST, OTHER) = 2 rows.
  expect_equal(nrow(all_out), 2L)
  # best_peptide: BEST wins A and B (one dot) + OTHER never wins (A taken by BEST)
  #   -> exactly 1 distinct best-peptide.
  expect_equal(nrow(best_out), 1L)
  expect_equal(best_out$id, "BEST")
  # Two-sided color + feature/marker still attached on the best-peptide panel.
  expect_equal(best_out$sig_direction, "down")
  expect_equal(best_out$sig_color, "#1f4e9c")
  expect_true("feature_class_primary" %in% colnames(best_out))
  expect_true("is_marker" %in% colnames(best_out))
})

# --- H2 regression: shared stripped sequence -> CONSISTENT best-peptide dot ---

test_that("H2: best-peptide dot for a non-unique stripped seq is mutually consistent", {
  # A stripped sequence "SHARED" appears in TWO stat_df rows (the same peptide
  # shared across two protein groups — common in DIA), with DIFFERENT
  # accession / logFC / P.Value / adj.P.Val per row:
  #   row1 (FIRST): A1, logFC +3, P.Value 0.5,   adj 0.5   (NOT the rollup winner)
  #   row2:         A2, logFC -1, P.Value 0.001, adj 0.001 (the rollup winner)
  # The OLD builder back-mapped "SHARED" to the FIRST stat_df row (A1), so the
  # dot's protein/gene/span/color/y-height came from A1 while its logFC came from
  # the rollup (A2) — a dot whose label, color, and HEIGHT belonged to different
  # proteins. The fix derives ALL of those from the rollup's WON accession (A2).
  stat <- data.frame(
    PEP.StrippedSequence = c("SHARED", "SHARED", "OTHER"),
    PG.ProteinAccessions = c("A1", "A2", "A3"),
    PG.Genes             = c("GA1", "GA2", "GA3"),
    pep_start            = c(10L, 200L, 50L),
    pep_end              = c(14L, 204L, 54L),
    .row_id              = c(1L, 2L, 3L),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
  stat[["logFC.C1"]]     <- c(3.0, -1.0, 0.2)
  stat[["adj.P.Val.C1"]] <- c(0.5, 0.001, 0.8)
  stat[["P.Value.C1"]]   <- c(0.5, 0.001, 0.7)

  matched <- .make_matched(
    seq       = c("SHARED", "SHARED", "OTHER"),
    accession = c("A1", "A2", "A3"),
    gene      = c("GA1", "GA2", "GA3"),
    pep_start = c(10L, 200L, 50L),
    pep_end   = c(14L, 204L, 54L),
    row_id    = c(1L, 2L, 3L)
  )

  out <- pelsa_build_volcano_df(
    stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
    markers = character(0), contrast = "C1",
    opts = list(panel = "best_peptide")
  )

  shared <- out[out$id == "SHARED", , drop = FALSE]
  expect_equal(nrow(shared), 1L)

  # The coordinate is the peptide's OWN (rollup) stats — the won (A2) row.
  expect_equal(shared$logFC, -1.0)
  expect_equal(shared$adj.P.Val, 0.001)
  # raw-p / logP come from the SAME won accession (A2), NOT A1's 0.5.
  expect_equal(shared$P.Value, 0.001)
  expect_equal(shared$logP, -log10(0.001))

  # Protein / gene / span / winner ALL come from the won accession A2 (NOT A1).
  expect_equal(shared$PG.ProteinAccessions, "A2")
  expect_equal(shared$PG.Genes, "GA2")
  expect_equal(shared$winning_accession, "A2")
  expect_equal(shared$winning_gene, "GA2")
  expect_equal(shared$pep_start, 200L)
  expect_equal(shared$pep_end, 204L)

  # Color is consistent with the won logFC (-1 -> down -> blue), NOT A1's +3.
  expect_equal(shared$sig_direction, "down")
  expect_equal(shared$sig_color, "#1f4e9c")
})

test_that("H2: shared seq won by a MARKER accession flags is_marker by the winner", {
  # "SHARED" maps to A1 (non-marker, first row) and MK (marker, the winner).
  # is_marker must reflect the WON accession (MK), not the arbitrary first row.
  stat <- data.frame(
    PEP.StrippedSequence = c("SHARED", "SHARED"),
    PG.ProteinAccessions = c("A1", "P12345"),
    PG.Genes             = c("GA1", "GMK"),
    pep_start            = c(10L, 200L),
    pep_end              = c(14L, 204L),
    .row_id              = c(1L, 2L),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
  stat[["logFC.C1"]]     <- c(3.0, -1.0)
  stat[["adj.P.Val.C1"]] <- c(0.5, 0.001)
  stat[["P.Value.C1"]]   <- c(0.5, 0.001)
  matched <- .make_matched(
    seq = c("SHARED", "SHARED"), accession = c("A1", "P12345"),
    gene = c("GA1", "GMK"), pep_start = c(10L, 200L), pep_end = c(14L, 204L),
    row_id = c(1L, 2L)
  )
  out <- pelsa_build_volcano_df(
    stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
    markers = c("P12345"), contrast = "C1", opts = list(panel = "best_peptide")
  )
  shared <- out[out$id == "SHARED", , drop = FALSE]
  expect_equal(nrow(shared), 1L)
  expect_equal(shared$winning_accession, "P12345")
  expect_true(shared$is_marker)            # won by the marker accession
})

# --- output column contract ---------------------------------------------------

test_that("output carries the full tooltip/plot column contract", {
  stat <- .make_stat_df(
    seq = "PEPC", acc = "ACC1", genes = "GC",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPC", "ACC1", "GC", 10L, 14L, row_id = 1L)
  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1")
  expected <- c("id", "logFC", "adj.P.Val", "P.Value", "logP", "Significant",
                "sig_direction", "sig_color", "feature_class_primary",
                "feature_color", "winning_accession", "winning_gene",
                "label", "is_marker", "PG.ProteinAccessions", "PG.Genes",
                "pep_start", "pep_end")
  expect_true(all(expected %in% colnames(out)))
})

# --- boundary validation ------------------------------------------------------

test_that("missing contrast stat columns errors", {
  stat <- .make_stat_df(
    seq = "PEPC", acc = "ACC1", genes = "GC",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPC", "ACC1", "GC", 10L, 14L, row_id = 1L)
  expect_error(
    pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                           markers = character(0), contrast = "NOPE"),
    regexp = "logFC|adj.P.Val|P.Value|column"
  )
})

test_that("non-data.frame stat_df errors", {
  expect_error(pelsa_build_volcano_df(list(a = 1), data.frame(), data.frame(),
                                      markers = character(0), contrast = "C1"))
})

test_that("partial contrast triplet (only some columns present) errors loudly", {
  # logFC.C1 + P.Value.C1 present but adj.P.Val.C1 missing -> loud error naming it.
  stat <- data.frame(
    PEP.StrippedSequence = "PEPT",
    PG.ProteinAccessions = "ACC1",
    PG.Genes             = "GT",
    pep_start            = 10L,
    pep_end              = 14L,
    .row_id              = 1L,
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  stat[["logFC.C1"]]   <- 1.0
  stat[["P.Value.C1"]] <- 0.10
  matched <- .make_matched("PEPT", "ACC1", "GT", 10L, 14L, row_id = 1L)
  expect_error(
    pelsa_build_volcano_df(stat, matched, .make_feat("X", 1L, 2L, "other"),
                           markers = character(0), contrast = "C1"),
    regexp = "adj.P.Val.C1"
  )
})

# --- unmapped peptide (no matched_cache row) ---------------------------------

test_that("peptide absent from matched_cache -> NA label, row retained, colored", {
  # Two source peptides; only MAPPED has a matched_cache row. UNMAPPED (e.g. it
  # failed FASTA matching upstream) must still appear as a row with NA label and
  # full sig/color attached -- never dropped, never a crash.
  stat <- .make_stat_df(
    seq = c("MAPPED", "UNMAPPED"),
    acc = c("ACC1", "ACC2"),
    genes = c("GM", "GU"),
    logfc = c(2.0, -2.0),
    adjp = c(0.001, 0.001),
    pval = c(0.0005, 0.0005),
    pep_start = c(10L, 30L),
    pep_end = c(14L, 34L),
    row_id = c(1L, 2L)
  )
  # matched cache only has the MAPPED peptide.
  matched <- .make_matched("MAPPED", "ACC1", "GM", 10L, 14L, row_id = 1L)

  out <- pelsa_build_volcano_df(stat, matched, .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1",
                                opts = list(panel = "all_peptide"))

  expect_equal(nrow(out), 2L)                 # both retained
  u <- out[out$id == "UNMAPPED", , drop = FALSE]
  expect_equal(nrow(u), 1L)
  expect_true(is.na(u$label))                 # no matched row -> NA label
  expect_equal(u$sig_direction, "down")       # sig/coloring still attached
  expect_equal(u$sig_color, "#1f4e9c")
  m <- out[out$id == "MAPPED", , drop = FALSE]
  expect_equal(m$label, "GM_aa10")            # mapped peptide labeled normally
})

# --- perf guard (catch a regression to the per-peptide loop) -----------------

test_that("builder stays fast on a few-thousand-row frame (no per-peptide loop)", {
  set.seed(11)
  N <- 5000L
  aa <- strsplit("ACDEFGHIKLMNPQRSTVWY", "")[[1]]
  seqs <- vapply(seq_len(N), function(i)
    paste0(sample(aa, sample(7:15, 1L), TRUE), collapse = ""), character(1))
  acc <- sprintf("P%05d", seq_len(N))
  stat <- .make_stat_df(
    seq = seqs, acc = acc, genes = sprintf("G%05d", seq_len(N)),
    logfc = stats::rnorm(N, 0, 1.5),
    adjp = stats::runif(N), pval = stats::runif(N),
    pep_start = rep(10L, N), pep_end = rep(14L, N), row_id = seq_len(N)
  )
  matched <- .make_matched(seqs, acc, sprintf("G%05d", seq_len(N)),
                           rep(10L, N), rep(14L, N), row_id = seq_len(N))
  feat <- .make_feat("ZZZ", 1L, 2L, "other")

  elapsed <- system.time(
    pelsa_build_volcano_df(stat, matched, feat, markers = character(0),
                           contrast = "C1", opts = list(panel = "all_peptide"))
  )[["elapsed"]]
  # Generous bound: the vectorized path is well under this; a regression to the
  # per-peptide pelsa_build_multilabel group-call would blow past it.
  expect_lt(elapsed, 1.0)
})

# --- Integration: generator -> explode -> FASTA-map -> volcano df ------------

test_that("integration: build volcano df from the synthetic generator", {
  syn <- pelsa_make_synthetic(seed = 1)
  exploded <- pelsa_explode_accessions(syn$peptides)
  mapped <- pelsa_map_peptide_positions(exploded, syn$fasta)$matched
  contrast <- syn$contrasts[1]

  # The all-peptide stat frame = the ORIGINAL per-peptide frame (one row each),
  # carrying the contrast-suffixed Statistics columns + a stable .row_id.
  stat <- syn$peptides
  stat$.row_id <- seq_len(nrow(stat))

  # Representative span per source peptide = the leading matched occurrence.
  lead <- mapped[!duplicated(mapped$.row_id), c(".row_id", "pep_start", "pep_end")]
  stat <- merge(stat, lead, by = ".row_id", all.x = TRUE, sort = FALSE)

  # A small hand-set feat_df: SHARED1 has a folded_domain spanning the shared
  # peptide's start (5..30).
  feat <- .make_feat("SHARED1", 5L, 30L, "folded_domain")

  # Marker on the isoform BASE; the synthetic isoform peptide is on "P12345-2".
  out <- pelsa_build_volcano_df(
    stat, mapped, feat,
    markers = c(syn$isoform_base_accession), contrast = contrast,
    opts = list(panel = "all_peptide")
  )

  # One row per source peptide (no explode).
  expect_equal(nrow(out), nrow(stat))
  expect_equal(anyDuplicated(out$id), 0L)

  # Column contract present.
  expect_true(all(c("logFC", "logP", "Significant", "sig_direction",
                    "sig_color", "feature_class_primary", "feature_color",
                    "label", "is_marker") %in% colnames(out)))

  # The shared peptide (3 accessions) is ONE row.
  shared <- out[out$id == syn$shared_peptide |
                  grepl(syn$shared_peptide, out$id, fixed = TRUE), , drop = FALSE]
  shared <- out[out$PG.ProteinAccessions == "SHARED1;SHARED2;SHARED3", , drop = FALSE]
  expect_equal(nrow(shared), 1L)

  # The isoform peptide flags is_marker via base-accession symmetry.
  iso_row <- out[out$PG.ProteinAccessions == syn$isoform_accession, , drop = FALSE]
  expect_equal(nrow(iso_row), 1L)
  expect_true(iso_row$is_marker)

  # y_cutoff attribute present.
  expect_true(!is.null(attr(out, "y_cutoff")))
})

################################################################################
# .pelsa_export_ggplot + .pelsa_export_color_spec : the static export figure now
# carries a legend (color-mode categories + a Marker entry) and a title (the
# contrast) / subtitle (<volcano type> | <coloring method>). These exercise the
# legend spec for both color modes and that the figure builds without error.
################################################################################

# Minimal volcano-style frame the export builder consumes.
.make_export_df <- function() {
  df <- data.frame(
    logFC = c(-3, 0.1, 2.5, -1, 0.2),
    logP  = c(6, 0.5, 5, 1.2, 0.3),
    adj.P.Val = c(1e-6, 0.5, 1e-5, 0.2, 0.6),
    sig_direction = c("down", "ns", "up", "ns", "ns"),
    feature_class_primary = c("folded_domain", "none", "catalytic_domain",
                              "other", "none"),
    winning_accession = c("A", "B", "C", "D", "E"),
    is_marker = c(FALSE, TRUE, FALSE, TRUE, FALSE),
    label = c(NA, "GENE1_aa2", NA, "GENE2_aa9", NA),
    stringsAsFactors = FALSE)
  attr(df, "y_cutoff") <- 2.0
  df
}

test_that("pelsa_volcano_export_df forces accession label + blank gene when self-curated", {
  # The export re-derive is a SEPARATE label path from the on-screen build; it
  # must apply the same self-curated forcing so the exported figure matches the
  # screen (no gene labels for a self-curated species).
  stat <- .make_stat_df(
    seq = "PEPSC", acc = "BalskusLab_HoyT_0001", genes = "SOMEGENE",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 7L, pep_end = 11L, row_id = 1L
  )
  matched <- .make_matched("PEPSC", "BalskusLab_HoyT_0001", "SOMEGENE",
                           7L, 11L, row_id = 1L)

  ex <- pelsa_volcano_export_df(
    stat_raw = stat, matched = matched, feat_df = NULL,
    markers = character(0), contrast = "C1", panel = "all_peptide",
    is_self_curated = TRUE
  )
  expect_equal(ex$label, "BalskusLab_HoyT_0001_aa7")
  expect_equal(ex$winning_gene, "")
})

test_that(".pelsa_export_color_spec: significance mode -> 3 fixed buckets", {
  bg <- .make_export_df()
  spec <- .pelsa_export_color_spec(bg, "significance")
  expect_equal(names(spec$values),
               c("Downregulated", "Non-significant", "Upregulated"))
  expect_equal(unname(spec$values["Downregulated"]), .PELSA_SIG_COLOR_DOWN)
  expect_equal(unname(spec$values["Non-significant"]), .PELSA_SIG_COLOR_NS)
  expect_equal(unname(spec$values["Upregulated"]), .PELSA_SIG_COLOR_UP)
  expect_equal(spec$method, "significance coloring")
  # category maps each row's sig_direction to its label.
  expect_equal(as.character(spec$category)[1], "Downregulated")
  expect_equal(as.character(spec$category)[3], "Upregulated")
})

test_that(".pelsa_export_color_spec: feature mode -> all 9 UniProt classes", {
  bg <- .make_export_df()
  spec <- .pelsa_export_color_spec(bg, "feature")
  expect_equal(length(spec$values), length(PELSA_FEATURE_COLORS))
  expect_equal(names(spec$values),
               unname(.PELSA_FEATURE_LABELS[names(PELSA_FEATURE_COLORS)]))
  expect_equal(unname(spec$values[.PELSA_FEATURE_LABELS[["folded_domain"]]]),
               unname(PELSA_FEATURE_COLORS[["folded_domain"]]))
  expect_equal(spec$method, "feature coloring")
})

test_that(".pelsa_export_ggplot: title from contrast, subtitle from type + mode", {
  df <- .make_export_df()
  g <- .pelsa_export_ggplot(df, df, color_mode = "significance",
                            label_mode = "all_markers",
                            contrast = "A_over_B",
                            volcano_label = "All-peptide volcano")
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "A vs B")
  expect_equal(g$labels$subtitle, "All-peptide volcano | significance coloring")
  # the whole figure builds (legend + annotation + label layer) without error.
  expect_silent(suppressWarnings(ggplot2::ggplot_build(g)))
})

test_that(".pelsa_export_ggplot: feature mode subtitle + NULL contrast -> no title", {
  df <- .make_export_df()
  g <- .pelsa_export_ggplot(df, df, color_mode = "feature",
                            volcano_label = "Best-peptide volcano")
  expect_null(g$labels$title)
  expect_equal(g$labels$subtitle, "Best-peptide volcano | feature coloring")
  expect_silent(suppressWarnings(ggplot2::ggplot_build(g)))
})

test_that(".pelsa_export_ggplot: dashed-line annotation tracks sig_cutoff", {
  df <- .make_export_df()
  # annotate("text", ...) stores its label as a layer aes_param, not in data.
  ann_text <- function(g) unlist(lapply(g$layers, function(l) {
    if (inherits(l$geom, "GeomText")) l$aes_params$label else NULL
  }))
  # default cutoff -> default constant text
  expect_true(paste0("adj.P < ", .PELSA_EXPORT_SIG_CUTOFF) %in%
                ann_text(.pelsa_export_ggplot(df, df)))
  # a user-set cutoff flows into the annotation text verbatim (no drift)
  expect_true("adj.P < 0.01" %in%
                ann_text(.pelsa_export_ggplot(df, df, sig_cutoff = 0.01)))
})
