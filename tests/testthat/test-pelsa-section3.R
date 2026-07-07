################################################################################
# PELSA Section 3 test suite (merged).
#
# Consolidates the eight PELSA Section 3 test files into one: the WebGL volcano
# (build/recolor/highlight/clicked-point/find-metadata), the Woods panel, the
# intensity line-figure data + static-export builders, and export helpers.
# Targets tab_pelsa_section3*.R + tab_pelsa_volcano_helpers.R +
# tab_pelsa_panel_helpers.R + tab_pelsa_export_helpers.R.
#
# No helper-name collisions existed across the source files, so every fixture
# builder is kept verbatim under its original name. The synthetic-generator
# source() (used by the volcano-data + intensity-data integration tests) is
# deduplicated to a single line below.
################################################################################

library(testthat)
source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

################################################################################
# --- from test-pelsa-volcano-data.R  (volcano data builder (3A) + static export figure) ---
################################################################################

################################################################################
# Tests for pelsa_build_volcano_df()  -  the PELSA volcano data-frame builder (3A).
#
# ONE tidy row per SOURCE peptide for the all-peptide panel (no explode), with:
#   - id / logFC / adj.P.Val / P.Value / logP (mirrors build_volcano_df),
#   - Significant (logP > the empirical y_cutoff derived from adj.P.Val <
#     sig_cutoff  -  mirrors the Statistics tab's own rule so both tabs agree
#     at the boundary peptide; see .pelsa_attach_significance),
#   - sig_direction {up,down,ns} + sig_color (TWO-SIDED: up=darkred, down=blue,
#     ns=gray)  -  Decision #4,
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
  # A decoy second passing peptide with a LARGER raw P.Value (still adj.P.Val
  # < cutoff, but weaker evidence) becomes the max-P.Value-among-passing
  # peptide instead of PEPUP, keeping PEPUP off the empirical y_cutoff
  # boundary (see .pelsa_attach_significance) so this test exercises ordinary
  # significance, not the boundary tie-break.
  stat <- .make_stat_df(
    seq = c("PEPUP", "DECOY"), acc = c("ACC1", "ACC2"), genes = c("GUP", "GDEC"),
    logfc = c(2.0, 1.0), adjp = c(0.001, 0.01), pval = c(0.0005, 0.03),
    pep_start = c(10L, 30L), pep_end = c(14L, 34L), row_id = c(1L, 2L)
  )
  matched <- rbind(
    .make_matched("PEPUP", "ACC1", "GUP", 10L, 14L, row_id = 1L),
    .make_matched("DECOY", "ACC2", "GDEC", 30L, 34L, row_id = 2L)
  )

  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1")
  out <- out[out$id == "PEPUP", , drop = FALSE]

  expect_equal(nrow(out), 1L)
  expect_true(out$Significant)
  expect_equal(out$sig_direction, "up")
  expect_equal(out$sig_color, "darkred")
})

test_that("significant down peptide -> down / blue (two-sided, not filtered)", {
  # Decoy peptide (see above) keeps PEPDN off the empirical boundary.
  stat <- .make_stat_df(
    seq = c("PEPDN", "DECOY"), acc = c("ACC1", "ACC2"), genes = c("GDN", "GDEC"),
    logfc = c(-2.0, 1.0), adjp = c(0.001, 0.01), pval = c(0.0005, 0.03),
    pep_start = c(10L, 30L), pep_end = c(14L, 34L), row_id = c(1L, 2L)
  )
  matched <- rbind(
    .make_matched("PEPDN", "ACC1", "GDN", 10L, 14L, row_id = 1L),
    .make_matched("DECOY", "ACC2", "GDEC", 30L, 34L, row_id = 2L)
  )

  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1")
  out <- out[out$id == "PEPDN", , drop = FALSE]

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

test_that("self-curated: winning_accession is a MATCHED accession, not an unmapped leading token", {
  # Regression: the pinned intensity-line panel looks the clicked dot's
  # winning_accession up in the matched cache. For a self-curated species (no
  # feature overlap to rewrite the winner), the all-peptide builder used to take
  # the LEADING PG.ProteinAccessions token verbatim. When that leading token does
  # NOT FASTA-map but a SECONDARY token does, winning_accession pointed at the
  # unmapped token and the intensity lookup stop()ed -> blank panel. The winner
  # must instead be a token that is actually present in the matched cache.
  stat <- .make_stat_df(
    seq = "PEPMAP", acc = "NOMAP;REALPROT", genes = "G1;G2",
    logfc = 1.5, adjp = 0.001, pval = 0.0005,
    pep_start = 5L, pep_end = 9L, row_id = 1L
  )
  # Only REALPROT FASTA-mapped, so the matched cache carries REALPROT only.
  matched <- .make_matched("PEPMAP", "REALPROT", "G2", 5L, 9L, row_id = 1L)

  out <- pelsa_build_volcano_df(
    stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
    markers = character(0), contrast = "C1", is_self_curated = TRUE)

  expect_equal(out$winning_accession, "REALPROT")
  expect_true(out$winning_accession %in% matched$accession)
})

test_that("self-curated repoint picks the representative matched acc (first by pep_start) and agrees with the label", {
  # The unmapped leading token forces a repoint; the chosen winner must be the
  # REPRESENTATIVE matched accession = first by (pep_start, accession), the same
  # order the multilabel uses -> winning_accession must equal the label's LEADING
  # entry's accession. Matched holds B@pep_start=20 and A@pep_start=5: ordered by
  # pep_start, A (start 5) is the representative even though B was listed first.
  stat <- .make_stat_df(
    seq = "PEPREP", acc = "NOMAP;A;B", genes = "G0;GA;GB",
    logfc = 1.5, adjp = 0.001, pval = 0.0005,
    pep_start = 5L, pep_end = 9L, row_id = 1L
  )
  matched <- .make_matched(
    seq = c("PEPREP", "PEPREP"),
    accession = c("B", "A"),         # input order deliberately B-first
    gene = c("GB", "GA"),
    pep_start = c(20L, 5L),          # A maps earlier (start 5) -> representative
    pep_end = c(24L, 9L),
    row_id = c(1L, 1L)
  )

  out <- pelsa_build_volcano_df(
    stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
    markers = character(0), contrast = "C1", is_self_curated = TRUE)

  expect_equal(out$winning_accession, "A")
  # Label is accession-based for self-curated, ordered by (pep_start, accession),
  # so its leading entry is the representative accession: "A_aa5".
  expect_true(startsWith(out$label, "A_aa5"))
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

test_that("all_peptide pep_start/pep_end follow the WINNING accession, not the smallest-start mapping", {
  # A shared peptide maps (in PG order) to C@162, A@120, B@50 (smallest start).
  # With no feature overlap the winning accession is the LEADING token C. The
  # tooltip span (pep_start/pep_end) and the Peptide label must describe the WON
  # accession C (162-171) -- the same coordinate the pinned intensity + Woods
  # panels use -- NOT the representative smallest-start mapping B (50-59) that
  # pelsa_volcano_stat_df carried onto stat_df. Regression for the
  # AEIITVSDGR/Q9CQ80 position mismatch (a peptide whose winning accession is not
  # the one with the smallest residue position).
  feat <- .make_feat("OTHER", 5L, 30L, "folded_domain")  # no overlap with PEPWIN
  stat <- .make_stat_df(
    seq = "PEPWIN", acc = "C;A;B", genes = "GC;GA;GB",
    logfc = 1.0, adjp = 0.20, pval = 0.10,
    pep_start = 50L, pep_end = 59L, row_id = 1L   # representative = B's span
  )
  matched <- .make_matched(
    seq = rep("PEPWIN", 3L),
    accession = c("C", "A", "B"),
    gene = c("GC", "GA", "GB"),
    pep_start = c(162L, 120L, 50L),
    pep_end   = c(171L, 129L, 59L),
    row_id = c(1L, 1L, 1L)
  )

  out <- pelsa_build_volcano_df(stat, matched, feat, markers = character(0),
                                contrast = "C1", opts = list(panel = "all_peptide"))

  expect_equal(out$winning_accession, "C")
  expect_equal(out$pep_start, 162L)
  expect_equal(out$pep_end, 171L)
})

test_that("all_peptide span falls back to the representative span when the winner has no matched occurrence", {
  # A peptide with NO matched-cache row for its key (key=2): winner stays the
  # leading token (reconciliation has nothing to repoint to), and the
  # winning-accession span lookup misses -> the representative stat_df span must
  # be RETAINED (not blanked to NA). Locks the documented fallback contract.
  stat <- .make_stat_df(
    seq = c("PEPHIT", "PEPMISS"), acc = c("A", "Z"), genes = c("GA", "GZ"),
    logfc = c(1.0, 1.0), adjp = c(0.20, 0.20), pval = c(0.10, 0.10),
    pep_start = c(10L, 77L), pep_end = c(14L, 86L), row_id = c(1L, 2L)
  )
  # matched holds ONLY PEPHIT (key 1); PEPMISS (key 2) is absent.
  matched <- .make_matched(
    seq = "PEPHIT", accession = "A", gene = "GA",
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )

  out <- pelsa_build_volcano_df(stat, matched,
                                feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = "C1",
                                opts = list(panel = "all_peptide"))

  miss <- out[out$id == "PEPMISS", , drop = FALSE]
  expect_equal(miss$pep_start, 77L)   # representative span retained
  expect_equal(miss$pep_end, 86L)
  expect_false(is.na(miss$pep_start))
})

# --- contrast = NULL (already-renamed columns) -------------------------------

test_that("contrast=NULL reads already-renamed logFC/adj.P.Val/P.Value", {
  # Decoy second row (see the significant up/down tests above) keeps PEPR off
  # the empirical y_cutoff boundary.
  stat <- data.frame(
    PEP.StrippedSequence = c("PEPR", "DECOY"),
    PG.ProteinAccessions = c("ACC1", "ACC2"),
    PG.Genes             = c("GR", "GDEC"),
    pep_start            = c(10L, 30L),
    pep_end              = c(14L, 34L),
    logFC                = c(2.0, 1.0),
    adj.P.Val            = c(0.001, 0.01),
    P.Value              = c(0.0005, 0.03),
    .row_id              = c(1L, 2L),
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  matched <- rbind(
    .make_matched("PEPR", "ACC1", "GR", 10L, 14L, row_id = 1L),
    .make_matched("DECOY", "ACC2", "GDEC", 30L, 34L, row_id = 2L)
  )
  out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                markers = character(0), contrast = NULL)
  out <- out[out$id == "PEPR", , drop = FALSE]
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

# --- sig_stat: nom.p.val honors the user-selected significance statistic ------
# The Statistics tab lets the user pick stat = "adj.p.val" (default) or
# "nom.p.val" (tab_stat_plot_helpers.R). The PELSA volcano must honor the same
# choice so the two volcanoes agree on identical data + cutoff. A peptide that
# FAILS the adj.P.Val filter but PASSES the nominal-p filter is the discriminator.

test_that("sig_stat = nom.p.val classifies on P.Value, not adj.P.Val", {
  # P.Value 0.01 < 0.05 (passes nominal) but adj.P.Val 0.30 > 0.05 (fails adj).
  stat <- .make_stat_df(
    seq = "PEPNOM", acc = "ACC1", genes = "GNOM",
    logfc = 2.0, adjp = 0.30, pval = 0.01,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPNOM", "ACC1", "GNOM", 10L, 14L, row_id = 1L)

  out <- pelsa_build_volcano_df(
    stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
    markers = character(0), contrast = "C1",
    opts = list(sig_cutoff = 0.05, sig_stat = "nom.p.val"))

  expect_true(out$Significant)
  expect_equal(out$sig_direction, "up")
})

test_that("sig_stat = nom.p.val sets y_cutoff = -log10(cutoff)", {
  stat <- .make_stat_df(
    seq = "PEPNOM", acc = "ACC1", genes = "GNOM",
    logfc = 2.0, adjp = 0.30, pval = 0.01,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPNOM", "ACC1", "GNOM", 10L, 14L, row_id = 1L)

  out <- pelsa_build_volcano_df(
    stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
    markers = character(0), contrast = "C1",
    opts = list(sig_cutoff = 0.05, sig_stat = "nom.p.val"))

  expect_equal(attr(out, "y_cutoff"), -log10(0.05))
})

test_that("sig_stat defaults to adj.p.val (unchanged) when unset", {
  # Same discriminating peptide: under the default adj path it is NOT significant.
  stat <- .make_stat_df(
    seq = "PEPNOM", acc = "ACC1", genes = "GNOM",
    logfc = 2.0, adjp = 0.30, pval = 0.01,
    pep_start = 10L, pep_end = 14L, row_id = 1L
  )
  matched <- .make_matched("PEPNOM", "ACC1", "GNOM", 10L, 14L, row_id = 1L)

  out <- pelsa_build_volcano_df(
    stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
    markers = character(0), contrast = "C1",
    opts = list(sig_cutoff = 0.05))

  expect_false(out$Significant)
})

# --- best_peptide panel reuses 2G rollup -------------------------------------

test_that("best_peptide panel uses 2G rollup (one dot per distinct best-peptide)", {
  # Two accessions A,B; peptide BEST wins both with the smallest adj.P.Val.
  # all_peptide would emit 2 source-peptide rows; best_peptide rolls BEST to 1.
  # DECOY (a third, unrelated accession) is a second passing peptide with a
  # LARGER raw P.Value than BEST (still adj.P.Val < cutoff, but weaker
  # evidence), keeping BEST off the empirical y_cutoff boundary (see
  # .pelsa_attach_significance).
  stat <- .make_stat_df(
    seq = c("BEST", "OTHER", "DECOY"),
    acc = c("A;B", "A", "C"),
    genes = c("GA;GB", "GA", "GDEC"),
    logfc = c(-2.0, 0.5, 1.0),
    adjp = c(0.001, 0.30, 0.01),
    pval = c(0.0005, 0.20, 0.03),
    pep_start = c(10L, 50L, 70L),
    pep_end = c(14L, 54L, 74L),
    row_id = c(1L, 2L, 3L)
  )
  matched <- .make_matched(
    seq = c("BEST", "BEST", "OTHER", "DECOY"),
    accession = c("A", "B", "A", "C"),
    gene = c("GA", "GB", "GA", "GDEC"),
    pep_start = c(10L, 20L, 50L, 70L),
    pep_end = c(14L, 24L, 54L, 74L),
    row_id = c(1L, 1L, 2L, 3L)
  )

  all_out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                    markers = character(0), contrast = "C1",
                                    opts = list(panel = "all_peptide"))
  best_out <- pelsa_build_volcano_df(stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
                                     markers = character(0), contrast = "C1",
                                     opts = list(panel = "best_peptide"))
  best_out <- best_out[best_out$id == "BEST", , drop = FALSE]

  # all_peptide: one row per source peptide (BEST, OTHER, DECOY) = 3 rows.
  expect_equal(nrow(all_out), 3L)
  # best_peptide: BEST wins A and B (one dot) + OTHER never wins (A taken by
  # BEST) + DECOY's own accession C -> exactly 1 distinct BEST dot (filtered
  # above from the full best-peptide rollup).
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
  # shared across two protein groups  -  common in DIA), with DIFFERENT
  # accession / logFC / P.Value / adj.P.Val per row:
  #   row1 (FIRST): A1, logFC +3, P.Value 0.5,   adj 0.5   (NOT the rollup winner)
  #   row2:         A2, logFC -1, P.Value 0.001, adj 0.001 (the rollup winner)
  # The OLD builder back-mapped "SHARED" to the FIRST stat_df row (A1), so the
  # dot's protein/gene/span/color/y-height came from A1 while its logFC came from
  # the rollup (A2)  -  a dot whose label, color, and HEIGHT belonged to different
  # proteins. The fix derives ALL of those from the rollup's WON accession (A2).
  # DECOY (a fourth, unrelated accession/peptide) is a second passing peptide
  # with a LARGER raw P.Value than SHARED's winning row (still adj.P.Val <
  # cutoff, but weaker evidence), keeping SHARED off the empirical y_cutoff
  # boundary (see .pelsa_attach_significance).
  stat <- data.frame(
    PEP.StrippedSequence = c("SHARED", "SHARED", "OTHER", "DECOY"),
    PG.ProteinAccessions = c("A1", "A2", "A3", "A4"),
    PG.Genes             = c("GA1", "GA2", "GA3", "GA4"),
    pep_start            = c(10L, 200L, 50L, 70L),
    pep_end              = c(14L, 204L, 54L, 74L),
    .row_id              = c(1L, 2L, 3L, 4L),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
  stat[["logFC.C1"]]     <- c(3.0, -1.0, 0.2, 1.0)
  stat[["adj.P.Val.C1"]] <- c(0.5, 0.001, 0.8, 0.01)
  stat[["P.Value.C1"]]   <- c(0.5, 0.001, 0.7, 0.03)

  matched <- .make_matched(
    seq       = c("SHARED", "SHARED", "OTHER", "DECOY"),
    accession = c("A1", "A2", "A3", "A4"),
    gene      = c("GA1", "GA2", "GA3", "GA4"),
    pep_start = c(10L, 200L, 50L, 70L),
    pep_end   = c(14L, 204L, 54L, 74L),
    row_id    = c(1L, 2L, 3L, 4L)
  )

  out <- pelsa_build_volcano_df(
    stat, matched, feat_df = .make_feat("X", 1L, 2L, "other"),
    markers = character(0), contrast = "C1",
    opts = list(panel = "best_peptide")
  )

  shared <- out[out$id == "SHARED", , drop = FALSE]
  expect_equal(nrow(shared), 1L)

  # The coordinate is the peptide's OWN (rollup) stats  -  the won (A2) row.
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
  # full sig/color attached -- never dropped, never a crash. UNMAPPED's raw
  # P.Value is smaller than MAPPED's (still both adj.P.Val < cutoff) so
  # UNMAPPED isn't the max-P.Value-among-passing peptide and stays off the
  # empirical y_cutoff boundary (see .pelsa_attach_significance).
  stat <- .make_stat_df(
    seq = c("MAPPED", "UNMAPPED"),
    acc = c("ACC1", "ACC2"),
    genes = c("GM", "GU"),
    logfc = c(2.0, -2.0),
    adjp = c(0.001, 0.001),
    pval = c(0.005, 0.0005),
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

test_that("builder stays fast on a large frame (no per-peptide / per-row loop)", {
  testthat::skip_on_ci()
  set.seed(11)
  # N at the scale the file header documents (~80k); large enough that an O(n^2)
  # regression in ANY builder stage (the multilabel group-call OR the
  # winning_accession reconcile) blows past the bound. At 40k the vectorized path
  # is ~0.3s; a quadratic reconcile loop was ~3s here / ~12s at 80k.
  N <- 40000L
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
  # Generous bound: the vectorized path is well under this; a regression to a
  # per-peptide pelsa_build_multilabel group-call OR a per-row reconcile loop
  # would blow past it.
  expect_lt(elapsed, 3.0)
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

test_that(".pelsa_export_color_spec: significance mode breaks exclude Non-significant", {
  # The legend must never show a "Non-significant" key: values still carries the
  # ns color (so gray points render), but breaks (which drives the legend keys
  # via scale_color_manual(breaks=)) omits it - Downregulated/Upregulated only.
  bg <- .make_export_df()
  spec <- .pelsa_export_color_spec(bg, "significance")
  expect_equal(spec$breaks, c("Downregulated", "Upregulated"))
  expect_false("Non-significant" %in% spec$breaks)
  expect_true("Non-significant" %in% names(spec$values))
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
  # feature mode keeps every class as a legend break (no exclusion rule there).
  expect_equal(spec$breaks, names(spec$values))
})

test_that(".pelsa_export_ggplot: title from contrast, subtitle from type + mode", {
  df <- .make_export_df()
  g <- .pelsa_export_ggplot(df, df, color_mode = "significance",
                            label_mode = "all_markers",
                            contrast = "A_over_B",
                            volcano_label = "All-peptide volcano")
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "A vs B")
  expect_equal(g$labels$subtitle, "All-peptide volcano | Significance coloring")
  # the whole figure builds (legend + annotation + label layer) without error.
  expect_silent(suppressWarnings(ggplot2::ggplot_build(g)))
})

test_that(".pelsa_export_ggplot: feature mode subtitle + NULL contrast -> no title", {
  df <- .make_export_df()
  g <- .pelsa_export_ggplot(df, df, color_mode = "feature",
                            volcano_label = "Best-peptide volcano")
  expect_null(g$labels$title)
  expect_equal(g$labels$subtitle, "Best-peptide volcano | Feature coloring")
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

test_that(".pelsa_export_ggplot: marker points drawn same size as background points", {
  # Marker peptides previously rendered LARGER (size 2.4) than the gray
  # background points (size 1); they must now match the background point size so
  # the only distinguishing cue is the magenta shape-21 ring, not the dot size.
  df <- .make_export_df()
  g <- .pelsa_export_ggplot(df, df, color_mode = "significance")
  # background layer = the first GeomPoint (no fill aes); marker layer = the
  # GeomPoint whose mapping carries `fill`.
  pt_layers <- Filter(function(l) inherits(l$geom, "GeomPoint"), g$layers)
  is_marker_layer <- vapply(pt_layers,
    function(l) "fill" %in% names(l$mapping), logical(1))
  bg_size  <- pt_layers[[which(!is_marker_layer)[1]]]$aes_params$size
  mk_size  <- pt_layers[[which(is_marker_layer)[1]]]$aes_params$size
  expect_equal(mk_size, bg_size)
  # the magenta ring (shape 21) is retained as the distinguishing cue.
  expect_equal(pt_layers[[which(is_marker_layer)[1]]]$aes_params$shape, 21)
})

# Mirrors the real bug report: a contrast with ZERO significantly-up, ZERO
# significantly-down, and ZERO marker peptides (every row is "ns", is_marker
# all FALSE). The legend must still show Downregulated/Upregulated/Marker keys
# with their correct colors, and must never show a Non-significant key.
.make_export_df_all_ns_no_markers <- function() {
  df <- data.frame(
    logFC = c(0.1, -0.2, 0.05),
    logP  = c(0.5, 0.3, 0.2),
    adj.P.Val = c(0.5, 0.6, 0.7),
    sig_direction = c("ns", "ns", "ns"),
    feature_class_primary = c("none", "none", "none"),
    winning_accession = c("A", "B", "C"),
    is_marker = c(FALSE, FALSE, FALSE),
    label = c(NA, NA, NA),
    stringsAsFactors = FALSE)
  attr(df, "y_cutoff") <- 2.0
  df
}

# Extracts the built color-scale legend keys (the labels that will actually
# appear in the rendered legend) via ggplot_build()'s plot$scales, matching
# this file's existing style of introspecting g$layers/g$scales rather than
# rendering to a grob.
.export_ggplot_color_breaks <- function(g) {
  built <- ggplot2::ggplot_build(g)
  color_scale <- built$plot$scales$get_scales("colour")
  color_scale$get_breaks()
}

.export_ggplot_fill_breaks <- function(g) {
  built <- ggplot2::ggplot_build(g)
  fill_scale <- built$plot$scales$get_scales("fill")
  fill_scale$get_breaks()
}

test_that(".pelsa_export_ggplot: zero significant + zero markers still show all legend keys", {
  df <- .make_export_df_all_ns_no_markers()
  g <- .pelsa_export_ggplot(df, df, color_mode = "significance")
  expect_silent(suppressWarnings(ggplot2::ggplot_build(g)))

  color_breaks <- .export_ggplot_color_breaks(g)
  expect_true("Downregulated" %in% color_breaks)
  expect_true("Upregulated" %in% color_breaks)
  expect_false("Non-significant" %in% color_breaks)

  fill_breaks <- .export_ggplot_fill_breaks(g)
  expect_true("Marker" %in% fill_breaks)
})

# Extracts the point-glyph colors ggplot2 actually drew for one legend key
# gTree (the "key-<row>-<col>-bg" grobs inside a guide's gtable). A legend TEXT
# label can be present (per get_breaks()) while its colored dot glyph is
# silently skipped by ggplot2 >= 3.5's guide_legend(), which by default only
# draws a key's glyph for a layer when that break's value is present in the
# LAYER's data (not just the scale's declared breaks/limits) -- see NEWS.md:
# "By default, guide_legend() now only draws a key glyph for a layer when the
# value is in the layer's data." This is exactly the "text present, dot
# missing" bug report, which .export_ggplot_color_breaks() (scale breaks)
# cannot detect since it never looks at the layer's data. Every OTHER layer
# sharing the guide box also contributes a (possibly invisible/NA) point to
# each key gTree, so this returns the set of non-NA colors actually drawn
# rather than a raw grob count.
.legend_key_point_colors <- function(key_gtree) {
  cols <- character(0)
  for (child in key_gtree$children) {
    if (inherits(child, "points") && !is.null(child$gp$col) &&
        !is.na(child$gp$col) && !identical(child$gp$col, "#00000000")) {
      cols <- c(cols, child$gp$col)
    }
  }
  cols
}

# Locates a named guide's key gTrees (in break order) inside the built plot's
# right-side legend gtable.
.legend_guide_keys <- function(g, aesthetic) {
  gt <- ggplot2::ggplotGrob(g)
  idx <- which(gt$layout$name == "guide-box-right")
  guide_box <- gt$grobs[[idx]]
  is_target <- vapply(guide_box$grobs, function(guide) {
    !is.null(guide$layout) && any(grepl("^key-.*-bg$", guide$layout$name))
  }, logical(1))
  candidates <- guide_box$grobs[is_target]
  target <- candidates[[if (identical(aesthetic, "colour")) 1L else length(candidates)]]
  key_rows <- grepl("^key-.*-bg$", target$layout$name)
  target$grobs[key_rows]
}

test_that(".pelsa_export_ggplot: zero significant rows still draw colored legend dots", {
  # Root-cause regression for the reported bug: Downregulated/Upregulated text
  # appeared in the legend with NO colored swatch when every row was
  # non-significant. get_breaks()-based assertions above stay green even when
  # this is broken, so this renders the gtable and inspects the actual point
  # colors drawn inside each key instead.
  df <- .make_export_df_all_ns_no_markers()
  g <- .pelsa_export_ggplot(df, df, color_mode = "significance")

  # normalize both sides through col2rgb so named colors ("darkred") and hex
  # ("#1f4e9c") compare equal to grid's rendered "#RRGGBBAA" grob color.
  as_rgb <- function(col) unname(grDevices::col2rgb(col, alpha = TRUE))

  keys <- .legend_guide_keys(g, "colour")
  expect_equal(length(keys), 2L)
  down_cols <- .legend_key_point_colors(keys[[1]])
  up_cols   <- .legend_key_point_colors(keys[[2]])
  expect_true(any(apply(as_rgb(down_cols), 2, identical,
                        as.vector(as_rgb(.PELSA_SIG_COLOR_DOWN)))))
  expect_true(any(apply(as_rgb(up_cols), 2, identical,
                        as.vector(as_rgb(.PELSA_SIG_COLOR_UP)))))
})

test_that(".pelsa_export_ggplot: marker layer always present (even with 0 marker rows)", {
  # Root-cause regression: previously the marker geom_point layer was only
  # added `if (nrow(mk) > 0L)`, so a view with zero markers had no fill
  # aesthetic mapped anywhere and the "Marker" legend key vanished entirely.
  df <- .make_export_df_all_ns_no_markers()
  g <- .pelsa_export_ggplot(df, df, color_mode = "significance")
  pt_layers <- Filter(function(l) inherits(l$geom, "GeomPoint"), g$layers)
  has_fill_layer <- any(vapply(pt_layers,
    function(l) "fill" %in% names(l$mapping), logical(1)))
  expect_true(has_fill_layer)
})

test_that(".pelsa_export_ggplot: non-significant points still render in the plot body", {
  # The legend key disappears for "ns", but the underlying points must still be
  # drawn (just uncredited in the legend) - confirm the background layer's data
  # still contains all rows, colored via the ns entry in scale_color_manual's
  # values (which is retained even though it is excluded from breaks).
  df <- .make_export_df_all_ns_no_markers()
  g <- .pelsa_export_ggplot(df, df, color_mode = "significance")
  bg_layer <- Filter(function(l) inherits(l$geom, "GeomPoint") &&
                        !("fill" %in% names(l$mapping)), g$layers)[[1]]
  expect_equal(nrow(bg_layer$data), nrow(df))
  built <- ggplot2::ggplot_build(g)
  bg_built <- built$data[[1]]
  ns_color <- unname(.PELSA_SIG_COLOR_NS)
  expect_true(all(bg_built$colour == ns_color))
})

test_that(".pelsa_export_ggplot: with significant + marker rows, legend still complete", {
  # Regression protection: the normal (non-zero) case keeps working the same
  # way as before - all three sig legend keys present, no Non-significant key,
  # Marker key present.
  df <- .make_export_df()
  g <- .pelsa_export_ggplot(df, df, color_mode = "significance")
  color_breaks <- .export_ggplot_color_breaks(g)
  expect_true(all(c("Downregulated", "Upregulated") %in% color_breaks))
  expect_false("Non-significant" %in% color_breaks)
  fill_breaks <- .export_ggplot_fill_breaks(g)
  expect_true("Marker" %in% fill_breaks)
})

# Mirror-image bug report: every row is a marker (is_marker all TRUE), so the
# non-marker/background subset is empty. The color aesthetic (Downregulated/
# Upregulated) must still be attached to a layer and appear in the legend, and
# building the plot must not emit ggplot2's "No shared levels found" warning.
.make_export_df_all_markers <- function() {
  df <- data.frame(
    logFC = c(1.5, -1.8, 0.9),
    logP  = c(3.0, 2.5, 2.1),
    adj.P.Val = c(0.001, 0.002, 0.01),
    sig_direction = c("up", "down", "up"),
    feature_class_primary = c("none", "none", "none"),
    winning_accession = c("A", "B", "C"),
    is_marker = c(TRUE, TRUE, TRUE),
    label = c(NA, NA, NA),
    stringsAsFactors = FALSE)
  attr(df, "y_cutoff") <- 2.0
  df
}

test_that(".pelsa_export_ggplot: all-marker view still shows both direction legend keys", {
  df <- .make_export_df_all_markers()
  expect_true(nrow(pelsa_volcano_marker_split(df)$background) == 0L)

  g <- .pelsa_export_ggplot(df, df, color_mode = "significance")
  expect_no_warning(suppressMessages(ggplot2::ggplot_build(g)))

  color_breaks <- .export_ggplot_color_breaks(g)
  expect_true("Downregulated" %in% color_breaks)
  expect_true("Upregulated" %in% color_breaks)
  expect_false("Non-significant" %in% color_breaks)
})

test_that(".pelsa_export_ggplot: background layer always present (even with 0 background rows)", {
  # Root-cause regression: previously the background geom_point layer was only
  # added `if (nrow(bg) > 0L)`, so a view with zero background rows had no
  # color aesthetic mapped anywhere and both direction legend keys vanished.
  df <- .make_export_df_all_markers()
  g <- .pelsa_export_ggplot(df, df, color_mode = "significance")
  pt_layers <- Filter(function(l) inherits(l$geom, "GeomPoint"), g$layers)
  has_color_layer <- any(vapply(pt_layers,
    function(l) "colour" %in% names(l$mapping), logical(1)))
  expect_true(has_color_layer)
})

################################################################################
# --- from test-pelsa-volcano-ui.R  (volcano UI helpers + testServer (Section 3)) ---
################################################################################

################################################################################
# Tests: PELSA Section 3 (Volcano)  -  7A-7C
#
# Pure helpers (closed-form): contrast-key builder, label->suffix, contrast
# choices, stat_df span attach, color-mode column pick, marker-trace split,
# label-mode row selection (all-markers / best-per-marker / top-N=3).
#
# testServer (light): stat_results NULL -> gate; good stat_results + synthetic
# cache + markers -> plot output exists, contrast selector populates, switching
# contrast frees the prior contrast's cached df, color toggle switches column,
# "showing N of M" note renders. No pixels asserted; outputs/state only.
################################################################################


# ---------------------------------------------------------------------------
# PURE HELPERS
# ---------------------------------------------------------------------------

test_that("contrast_key builds <ome>::<contrast>, NULL on empty", {
  expect_equal(pelsa_volcano_contrast_key("Proteome", "A_over_B"),
               "Proteome::A_over_B")
  expect_null(pelsa_volcano_contrast_key("Proteome", NULL))
  expect_null(pelsa_volcano_contrast_key("Proteome", ""))
  expect_null(pelsa_volcano_contrast_key(NULL, "A_over_B"))
})

test_that("label -> stat-column suffix", {
  expect_equal(pelsa_volcano_label_to_suffix("A / B"), "A_over_B")
  expect_equal(pelsa_volcano_label_to_suffix(c("A / B", "C / D")),
               c("A_over_B", "C_over_D"))
})

test_that("contrast choices: two-sample only, named label->suffix in order", {
  sp <- list(Proteome = list(test = "Two-sample Moderated T-test",
                             contrasts = c("A / B", "C / D")))
  ch <- pelsa_volcano_contrast_choices(sp, "Proteome")
  expect_equal(unname(ch), c("A_over_B", "C_over_D"))
  expect_equal(names(ch), c("A / B", "C / D"))

  # one-sample / F / None -> empty
  expect_length(pelsa_volcano_contrast_choices(
    list(Proteome = list(test = "One-sample Moderated T-test",
                         groups = c("A"))), "Proteome"), 0L)
  expect_length(pelsa_volcano_contrast_choices(NULL, "Proteome"), 0L)
})

test_that("has_contrast detects the three stat columns", {
  df <- data.frame(logFC.A_over_B = 1, adj.P.Val.A_over_B = 0.1,
                   P.Value.A_over_B = 0.05)
  expect_true(pelsa_volcano_has_contrast(df, "A_over_B"))
  expect_false(pelsa_volcano_has_contrast(df, "C_over_D"))
  expect_false(pelsa_volcano_has_contrast(NULL, "A_over_B"))
})

test_that("stat_df attaches representative (leading) pep span by sequence", {
  stat <- data.frame(
    PEP.StrippedSequence = c("PEPA", "PEPB"),
    PG.ProteinAccessions = c("ACC1", "ACC2"),
    stringsAsFactors = FALSE
  )
  matched <- data.frame(
    PEP.StrippedSequence = c("PEPA", "PEPA", "PEPB"),
    pep_start            = c(20L, 5L, 100L),   # PEPA leading = 5
    pep_end              = c(24L, 9L, 108L),
    stringsAsFactors     = FALSE
  )
  out <- pelsa_volcano_stat_df(stat, matched)
  expect_equal(out$pep_start, c(5L, 100L))
  expect_equal(out$pep_end, c(9L, 108L))
  expect_true("PG.Genes" %in% colnames(out))
})

test_that("stat_df tolerates an empty matched cache (NA span)", {
  stat <- data.frame(PEP.StrippedSequence = "PEPA",
                     PG.ProteinAccessions = "ACC1", stringsAsFactors = FALSE)
  out <- pelsa_volcano_stat_df(stat, pelsa_volcano_empty_matched())
  expect_true(is.na(out$pep_start))
  expect_true(is.na(out$pep_end))
})

test_that("stat_df derives PEP.StrippedSequence from the id column when absent", {
  # PELSA dataset that keyed on the id column (rid): stat_results carries `id`
  # (the stripped sequence) but NO PEP.StrippedSequence. The volcano must derive
  # it from id so the join key matches the cache (whose matched$PEP.StrippedSequence
  # came from the SAME rid), instead of erroring.
  stat <- data.frame(
    id                   = c("PEPA", "PEPB"),
    PG.ProteinAccessions = c("ACC1", "ACC2"),
    stringsAsFactors     = FALSE
  )
  matched <- data.frame(
    PEP.StrippedSequence = c("PEPA", "PEPB"),
    pep_start            = c(5L, 100L),
    pep_end              = c(9L, 108L),
    stringsAsFactors     = FALSE
  )
  out <- pelsa_volcano_stat_df(stat, matched)
  expect_true("PEP.StrippedSequence" %in% colnames(out))
  expect_equal(out$PEP.StrippedSequence, c("PEPA", "PEPB"))   # copied from id
  expect_equal(out$pep_start, c(5L, 100L))                    # span joined by it
  expect_equal(out$pep_end, c(9L, 108L))
})

test_that("stat_df keeps a real PEP.StrippedSequence over the id column", {
  stat <- data.frame(
    id                   = c("rid1", "rid2"),
    PEP.StrippedSequence = c("PEPA", "PEPB"),   # authoritative
    PG.ProteinAccessions = c("ACC1", "ACC2"),
    stringsAsFactors     = FALSE
  )
  out <- pelsa_volcano_stat_df(stat, pelsa_volcano_empty_matched())
  expect_equal(out$PEP.StrippedSequence, c("PEPA", "PEPB"))  # NOT the id values
})

test_that("stat_df errors only when neither PEP.StrippedSequence nor id exists", {
  stat <- data.frame(PG.ProteinAccessions = "ACC1", stringsAsFactors = FALSE)
  expect_error(
    pelsa_volcano_stat_df(stat, pelsa_volcano_empty_matched()),
    "must have PEP.StrippedSequence")
})

test_that("color-mode picks sig_color (significance) vs feature_color (feature)", {
  df <- data.frame(sig_color = c("darkred", "gray"),
                   feature_color = c("#1f77b4", "#d3d3d3"),
                   stringsAsFactors = FALSE)
  expect_equal(pelsa_volcano_color_column(df, "significance"),
               c("darkred", "gray"))
  expect_equal(pelsa_volcano_color_column(df, "feature"),
               c("#1f77b4", "#d3d3d3"))
  expect_error(pelsa_volcano_color_column(df, "nope"))
})

test_that("marker split separates is_marker rows (never thinned, on top)", {
  df <- data.frame(
    id = c("a", "b", "c"),
    is_marker = c(TRUE, FALSE, NA),
    stringsAsFactors = FALSE
  )
  sp <- pelsa_volcano_marker_split(df)
  expect_equal(sp$markers$id, "a")           # only the TRUE row
  expect_setequal(sp$background$id, c("b", "c"))  # NA treated as non-marker
})

test_that("label-mode: all_markers labels every marker row", {
  df <- data.frame(
    is_marker = c(TRUE, FALSE, TRUE),
    adj.P.Val = c(0.2, 0.001, 0.5),
    winning_accession = c("P1", "P2", "P1"),
    label = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )
  expect_equal(pelsa_volcano_label_rows(df, "all_markers"), c(1L, 3L))
})

test_that("label-mode: empty mode vector labels nothing", {
  df <- data.frame(
    is_marker         = c(TRUE, FALSE, TRUE),
    Significant       = c(TRUE, FALSE, TRUE),
    adj.P.Val         = c(0.01, 0.5, 0.02),
    winning_accession = c("P1", "P2", "P3"),
    label             = c("a", "b", "c"),
    stringsAsFactors  = FALSE
  )
  expect_equal(pelsa_volcano_label_rows(df, character(0)), integer(0))
  expect_equal(pelsa_volcano_label_rows(df, NULL), integer(0))
})

test_that("label-mode: all_significant labels sig rows only", {
  df <- data.frame(
    is_marker         = c(TRUE, FALSE, FALSE, TRUE),
    Significant       = c(TRUE, FALSE, TRUE, NA),
    adj.P.Val         = c(0.01, 0.5, 0.02, 0.2),
    winning_accession = c("P1", "P2", "P3", "P4"),
    label             = c("a", "b", "c", "d"),
    stringsAsFactors  = FALSE
  )
  # all_significant: rows where Significant == TRUE (NA -> FALSE).
  expect_equal(pelsa_volcano_label_rows(df, "all_significant"), c(1L, 3L))
})

test_that("label-mode: combining all_markers + all_significant unions the rows", {
  df <- data.frame(
    is_marker         = c(TRUE, FALSE, FALSE, FALSE),
    Significant       = c(FALSE, TRUE, FALSE, FALSE),
    adj.P.Val         = c(0.9, 0.01, 0.5, 0.5),
    winning_accession = c("P1", "P2", "P3", "P4"),
    label             = c("a", "b", "c", "d"),
    stringsAsFactors  = FALSE
  )
  # all_markers -> row 1; all_significant -> row 2; union -> both, sorted+unique.
  expect_equal(
    pelsa_volcano_label_rows(df, c("all_markers", "all_significant")),
    c(1L, 2L)
  )
  # Order of the mode vector must not matter.
  expect_equal(
    pelsa_volcano_label_rows(df, c("all_significant", "all_markers")),
    c(1L, 2L)
  )
})

test_that("label-mode: unknown mode errors; default is empty", {
  df <- data.frame(is_marker = TRUE, Significant = TRUE, adj.P.Val = 0.01,
                   winning_accession = "P1", label = "a",
                   stringsAsFactors = FALSE)
  expect_error(pelsa_volcano_label_rows(df, "bogus"), "must be one of")
  expect_identical(.PELSA_VOLCANO_DEFAULT_LABEL_MODE, character(0))
})

test_that("label-mode: top_n_adjp takes N smallest adj.P.Val per logFC-sign direction", {
  df <- data.frame(
    is_marker         = rep(FALSE, 6),
    logFC             = c(1.0, 1.0, 1.0, -1.0, -1.0, 1.0),
    adj.P.Val         = c(0.5, 0.1, 0.2, 0.05, 0.3, 0.9),
    winning_accession = c("P1", "P2", "P3", "P4", "P5", "P6"),
    label             = letters[1:6],
    stringsAsFactors  = FALSE
  )
  # down keeps the full N=2; up keeps only ceiling(2/2)=1 (asymmetric split).
  # up bucket (rows 1,2,3,6): smallest 1 by adj.P.Val -> row2(0.1).
  # down bucket (rows 4,5): smallest 2 -> both (only 2 available).
  # row 6 (non-significant is irrelevant here) still ranked in the up bucket,
  # but loses on adj.P.Val (0.9 is the largest).
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_adjp", n_top_adjp = 2L),
    c(2L, 4L, 5L)
  )
})

test_that("label-mode: top_n_adjp labels fewer than N when a bucket is small", {
  df <- data.frame(
    is_marker         = rep(FALSE, 3),
    logFC             = c(1.0, -1.0, -1.0),
    adj.P.Val         = c(0.01, 0.2, 0.3),
    winning_accession = c("P1", "P2", "P3"),
    label             = c("a", "b", "c"),
    stringsAsFactors  = FALSE
  )
  # up bucket has only 1 row -> that row is kept even though N=5.
  # down bucket has 2 rows, N=5 -> both kept.
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_adjp", n_top_adjp = 5L),
    c(1L, 2L, 3L)
  )
})

test_that("label-mode: top_n_adjp excludes NA-logFC rows from the up bucket", {
  # A peptide with NA logFC (undefined direction) but a tiny adj.P.Val must
  # NOT be picked into the "up" top-N bucket -- regression for the bug where
  # `ifelse(!is.na(logfc) & logfc < 0, "down", "up")` defaulted NA logFC to
  # "up", letting it steal an up-regulated slot.
  df <- data.frame(
    is_marker         = rep(FALSE, 3),
    logFC             = c(1.0, NA_real_, -1.0),
    adj.P.Val         = c(0.5, 0.001, 0.2),
    winning_accession = c("P1", "P2", "P3"),
    label             = c("a", "b", "c"),
    stringsAsFactors  = FALSE
  )
  # Row 2 has the smallest adj.P.Val overall but NA logFC -> excluded from
  # both buckets. Up bucket keeps row 1 (only "up" row); down bucket keeps
  # row 3 (only "down" row).
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_adjp", n_top_adjp = 2L),
    c(1L, 3L)
  )
})

test_that("label-mode: top_n_markers excludes NA-logFC rows from the up bucket", {
  df <- data.frame(
    is_marker         = rep(TRUE, 3),
    logFC             = c(1.0, NA_real_, -1.0),
    adj.P.Val         = c(0.5, 0.001, 0.2),
    winning_accession = c("P1", "P2", "P3"),
    label             = c("a", "b", "c"),
    stringsAsFactors  = FALSE
  )
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_markers", n_top_markers = 2L),
    c(1L, 3L)
  )
})

test_that("label-mode: top_n_adjp default N is 3", {
  df <- data.frame(
    is_marker         = rep(FALSE, 6),
    logFC             = rep(1.0, 6),
    adj.P.Val         = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06),
    winning_accession = paste0("P", 1:6),
    label             = letters[1:6],
    stringsAsFactors  = FALSE
  )
  # No n_top_adjp arg supplied -> defaults to 3. All rows are "up" (down
  # bucket empty), so the up bucket's limit is ceiling(3/2) = 2 -> rows 1-2
  # kept (smallest adj.P.Val), rows 3-6 dropped.
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_adjp"),
    c(1L, 2L)
  )
})

test_that("label-mode: top_n_adjp includes a non-significant winner (no gate)", {
  df <- data.frame(
    is_marker    = rep(FALSE, 2),
    Significant  = c(FALSE, TRUE),
    logFC        = c(1.0, 1.0),          # both "up"
    adj.P.Val    = c(0.01, 0.5),         # row 1 (non-sig!) has smaller adj.P.Val
    label        = c("a", "b"),
    stringsAsFactors = FALSE
  )
  # Row 1 is non-significant but wins the up-bucket on adj.P.Val alone.
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_adjp", n_top_adjp = 1L),
    1L
  )
})

test_that("label-mode: top_n_markers takes N smallest adj.P.Val per logFC-sign bucket, markers only", {
  df <- data.frame(
    is_marker         = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    logFC             = c(2.0, 1.5, -1.0, -0.5, -3.0),
    adj.P.Val         = c(0.3, 0.1, 0.05, 0.2, 0.001),
    winning_accession = c("P1", "P2", "P3", "P4", "P5"),
    label             = letters[1:5],
    stringsAsFactors  = FALSE
  )
  # Marker rows only: 1,2 (logFC>0 -> up), 3,4 (logFC<0 -> down). Row 5 is
  # NOT a marker (is_marker FALSE) and must be excluded even though it has
  # the smallest adj.P.Val of all 5 rows.
  # up bucket (rows 1,2): N=1 -> smallest adj.P.Val -> row2(0.1).
  # down bucket (rows 3,4): N=1 -> smallest adj.P.Val -> row3(0.05).
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_markers", n_top_markers = 1L),
    c(2L, 3L)
  )
})

test_that("label-mode: top_n_markers ranks ALL markers regardless of significance", {
  df <- data.frame(
    is_marker         = c(TRUE, TRUE),
    Significant       = c(FALSE, FALSE),
    sig_direction     = c("ns", "ns"),
    logFC             = c(1.0, 2.0),
    adj.P.Val         = c(0.5, 0.9),
    winning_accession = c("P1", "P2"),
    label             = c("a", "b"),
    stringsAsFactors  = FALSE
  )
  # Neither row is significant, but top_n_markers ranks ALL markers anyway.
  # Both rows are logFC > 0 ("up" bucket); N=1 keeps only the smaller adj.P.Val.
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_markers", n_top_markers = 1L),
    1L
  )
})

test_that("label-mode: top_n_markers labels fewer than N when a bucket is small", {
  df <- data.frame(
    is_marker         = c(TRUE, TRUE, TRUE),
    logFC             = c(1.0, -1.0, -2.0),
    adj.P.Val         = c(0.1, 0.2, 0.3),
    winning_accession = c("P1", "P2", "P3"),
    label             = c("a", "b", "c"),
    stringsAsFactors  = FALSE
  )
  # up bucket has only 1 row -> kept even though N=5.
  # down bucket has 2 rows, N=5 -> both kept.
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_markers", n_top_markers = 5L),
    c(1L, 2L, 3L)
  )
})

test_that("label-mode: top_n_markers default N is 3", {
  df <- data.frame(
    is_marker         = rep(TRUE, 6),
    logFC             = rep(1.0, 6),
    adj.P.Val         = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06),
    winning_accession = paste0("P", 1:6),
    label             = letters[1:6],
    stringsAsFactors  = FALSE
  )
  # No n_top_markers arg supplied -> defaults to 3. All rows are "up" (down
  # bucket empty), so the up bucket's limit is ceiling(3/2) = 2 -> rows 1-2
  # kept (smallest adj.P.Val), rows 3-6 dropped.
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_markers"),
    c(1L, 2L)
  )
})

test_that("label-mode: top_n_adjp + top_n_markers combine via union", {
  df <- data.frame(
    is_marker         = c(TRUE, FALSE, FALSE),
    logFC             = c(5.0, 1.0, -1.0),
    adj.P.Val         = c(0.9, 0.01, 0.02),
    winning_accession = c("P1", "P2", "P3"),
    label             = c("a", "b", "c"),
    stringsAsFactors  = FALSE
  )
  # top_n_adjp ranks ALL rows by logFC-sign bucket (no marker restriction):
  # up bucket (rows 1,2, both logFC > 0): N=1 -> row2 (adj.P.Val 0.01 beats
  # row 1's 0.9), so row1 does NOT come from this mode despite its huge logFC.
  # down bucket (row 3 only): kept.
  # top_n_markers (N=1): row 1 is the only marker -> kept regardless of adj.P.Val.
  # Union of both modes -> all three rows, but row 1 is contributed only by
  # top_n_markers, not top_n_adjp.
  expect_equal(
    pelsa_volcano_label_rows(df, c("top_n_markers", "top_n_adjp"),
                             n_top_adjp = 1L, n_top_markers = 1L),
    c(1L, 2L, 3L)
  )
})

test_that(".pelsa_top_n_by_direction: tiebreak_value breaks adj.P.Val ties by smallest value", {
  # 5 rows, all "up", all tied on the primary `value` (simulating a BH
  # plateau). Distinct tiebreak_value per row; n_top=2 should keep the two
  # rows with the SMALLEST tiebreak_value, not the two smallest row indices.
  idx <- 1:5
  direction <- rep("up", 5)
  value <- rep(0.05, 5)
  tiebreak <- c(0.9, 0.1, 0.5, 0.01, 0.3)
  expect_equal(
    .pelsa_top_n_by_direction(idx, direction, value,
                               n_top_down = 2L, n_top_up = 2L,
                               tiebreak_value = tiebreak),
    c(2L, 4L)
  )
})

test_that(".pelsa_top_n_by_direction: without tiebreak_value, behavior is unchanged (regression)", {
  # Same tied-value fixture as above, but tiebreak_value omitted/NULL. The old
  # behavior falls back to original row index -> smallest indices win.
  idx <- 1:5
  direction <- rep("up", 5)
  value <- rep(0.05, 5)
  old_style <- {
    bucket <- which(direction == "up")
    bucket_idx <- idx[bucket]
    bucket_val <- value[bucket]
    ord <- order(bucket_val, bucket_idx, na.last = TRUE)
    sort(unique(head(bucket_idx[ord], 2L)))
  }
  expect_equal(old_style, c(1L, 2L))

  expect_equal(
    .pelsa_top_n_by_direction(idx, direction, value,
                               n_top_down = 2L, n_top_up = 2L),
    old_style
  )
  expect_equal(
    .pelsa_top_n_by_direction(idx, direction, value,
                               n_top_down = 2L, n_top_up = 2L,
                               tiebreak_value = NULL),
    old_style
  )
})

test_that(".pelsa_top_n_by_direction: n_top_down and n_top_up are independent per-bucket limits", {
  # 5 "up" rows and 5 "down" rows, distinct values in each bucket.
  idx <- 1:10
  direction <- c(rep("up", 5), rep("down", 5))
  value <- c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.04, 0.03, 0.02, 0.01)
  # up bucket (rows 1-5): smallest 3 by value -> rows 5,4,3 (values .1,.2,.3).
  # down bucket (rows 6-10): smallest 2 by value -> rows 10,9 (values .01,.02).
  expect_equal(
    .pelsa_top_n_by_direction(idx, direction, value,
                               n_top_down = 2L, n_top_up = 3L),
    c(3L, 4L, 5L, 9L, 10L)
  )
})

test_that("label-mode: top_n_adjp N=10 keeps 10 down + 5 up (asymmetric split)", {
  set.seed(1)
  n_down <- 12L
  n_up <- 12L
  df <- data.frame(
    is_marker         = rep(FALSE, n_down + n_up),
    logFC             = c(rep(-1.0, n_down), rep(1.0, n_up)),
    adj.P.Val         = c(seq(0.01, by = 0.01, length.out = n_down),
                          seq(0.01, by = 0.01, length.out = n_up)),
    winning_accession = paste0("P", seq_len(n_down + n_up)),
    label             = paste0("l", seq_len(n_down + n_up)),
    stringsAsFactors  = FALSE
  )
  # down bucket (rows 1-12): keeps the full N=10 -> smallest 10 adj.P.Val,
  # i.e. rows 1-10.
  # up bucket (rows 13-24): keeps ceiling(10/2) = 5 -> smallest 5 adj.P.Val,
  # i.e. rows 13-17.
  idx <- pelsa_volcano_label_rows(df, "top_n_adjp", n_top_adjp = 10L)
  expect_equal(idx, c(1:10, 13:17))
  expect_equal(length(which(idx <= n_down)), 10L)
  expect_equal(length(which(idx > n_down)), 5L)
})

test_that("label-mode: top_n_adjp N=5 keeps 5 down + 3 up (ceiling rounds up)", {
  n_down <- 8L
  n_up <- 8L
  df <- data.frame(
    is_marker         = rep(FALSE, n_down + n_up),
    logFC             = c(rep(-1.0, n_down), rep(1.0, n_up)),
    adj.P.Val         = c(seq(0.01, by = 0.01, length.out = n_down),
                          seq(0.01, by = 0.01, length.out = n_up)),
    winning_accession = paste0("P", seq_len(n_down + n_up)),
    label             = paste0("l", seq_len(n_down + n_up)),
    stringsAsFactors  = FALSE
  )
  # down bucket (rows 1-8): keeps the full N=5 -> rows 1-5.
  # up bucket (rows 9-16): keeps ceiling(5/2) = 3 -> rows 9-11.
  idx <- pelsa_volcano_label_rows(df, "top_n_adjp", n_top_adjp = 5L)
  expect_equal(idx, c(1:5, 9:11))
  expect_equal(length(which(idx <= n_down)), 5L)
  expect_equal(length(which(idx > n_down)), 3L)
})

test_that("label-mode: top_n_markers N=10 keeps 10 down + 5 up (asymmetric split)", {
  n_down <- 12L
  n_up <- 12L
  df <- data.frame(
    is_marker         = rep(TRUE, n_down + n_up),
    logFC             = c(rep(-1.0, n_down), rep(1.0, n_up)),
    adj.P.Val         = c(seq(0.01, by = 0.01, length.out = n_down),
                          seq(0.01, by = 0.01, length.out = n_up)),
    winning_accession = paste0("P", seq_len(n_down + n_up)),
    label             = paste0("l", seq_len(n_down + n_up)),
    stringsAsFactors  = FALSE
  )
  idx <- pelsa_volcano_label_rows(df, "top_n_markers", n_top_markers = 10L)
  expect_equal(idx, c(1:10, 13:17))
  expect_equal(length(which(idx <= n_down)), 10L)
  expect_equal(length(which(idx > n_down)), 5L)
})

test_that("label-mode: top_n_markers N=5 keeps 5 down + 3 up (ceiling rounds up)", {
  n_down <- 8L
  n_up <- 8L
  df <- data.frame(
    is_marker         = rep(TRUE, n_down + n_up),
    logFC             = c(rep(-1.0, n_down), rep(1.0, n_up)),
    adj.P.Val         = c(seq(0.01, by = 0.01, length.out = n_down),
                          seq(0.01, by = 0.01, length.out = n_up)),
    winning_accession = paste0("P", seq_len(n_down + n_up)),
    label             = paste0("l", seq_len(n_down + n_up)),
    stringsAsFactors  = FALSE
  )
  idx <- pelsa_volcano_label_rows(df, "top_n_markers", n_top_markers = 5L)
  expect_equal(idx, c(1:5, 9:11))
  expect_equal(length(which(idx <= n_down)), 5L)
  expect_equal(length(which(idx > n_down)), 3L)
})

test_that("label-mode: top_n_adjp breaks adj.P.Val ties by smallest raw P.Value", {
  df <- data.frame(
    is_marker         = rep(FALSE, 5),
    logFC             = rep(1.0, 5),                 # all "up"
    adj.P.Val         = rep(0.05, 5),                # tied plateau
    P.Value           = c(0.04, 0.005, 0.03, 0.09, 0.06),
    winning_accession = paste0("P", 1:5),
    label             = letters[1:5],
    stringsAsFactors  = FALSE
  )
  # All rows are "up" (down bucket empty), so the up bucket's limit is
  # ceiling(2/2) = 1, not 2 (asymmetric split). All 5 rows tied on
  # adj.P.Val -> tiebreak by smallest raw P.Value: row2 (0.005) wins.
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_adjp", n_top_adjp = 2L),
    2L
  )
})

test_that("label-mode: top_n_markers breaks adj.P.Val ties by smallest raw P.Value", {
  df <- data.frame(
    is_marker         = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    logFC             = c(1.0, 1.0, 1.0, 1.0, 1.0),   # all "up"
    adj.P.Val         = c(0.05, 0.05, 0.05, 0.05, 0.05),
    P.Value           = c(0.04, 0.005, 0.03, 0.09, 0.001),
    winning_accession = paste0("P", 1:5),
    label             = letters[1:5],
    stringsAsFactors  = FALSE
  )
  # Row 5 has the smallest P.Value overall but is NOT a marker -> excluded.
  # Among markers (1-4), all tied on adj.P.Val -> tiebreak by P.Value:
  # smallest is row2 (0.005).
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_markers", n_top_markers = 1L),
    2L
  )
})

test_that("label-mode: top_n_adjp falls back to largest |logFC| when P.Value is absent", {
  df <- data.frame(
    is_marker         = rep(FALSE, 4),
    logFC             = c(0.5, -1.0, 1.0, -2.5),      # up: rows1,3; down: rows2,4
    adj.P.Val         = rep(0.05, 4),                 # all tied
    winning_accession = paste0("P", 1:4),
    label             = letters[1:4],
    stringsAsFactors  = FALSE
  )
  # No P.Value column at all -> fallback tiebreak is -abs(logFC), i.e. the
  # LARGEST |logFC| wins. up bucket: row1 |0.5| vs row3 |1.0| -> row3 wins,
  # even though row1 has the smaller original row index (proves the
  # fallback is actually being applied, not just index order coinciding).
  # down bucket: row2 |1.0| vs row4 |2.5| -> row4 wins.
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_adjp", n_top_adjp = 1L),
    c(3L, 4L)
  )
})

test_that("label-mode: top_n_adjp falls back to largest |logFC| when P.Value is all-NA", {
  df <- data.frame(
    is_marker         = rep(FALSE, 4),
    logFC             = c(1.0, -2.5, 0.5, -1.0),
    adj.P.Val         = rep(0.05, 4),
    P.Value           = rep(NA_real_, 4),
    winning_accession = paste0("P", 1:4),
    label             = letters[1:4],
    stringsAsFactors  = FALSE
  )
  # P.Value column present but all-NA: order() with na.last=TRUE pushes NA
  # entries to the end of the tiebreak sort regardless of value, so the
  # tiebreak degenerates to bucket_idx order in this all-NA case (row1, row2).
  # This documents actual behavior; a genuinely useful |logFC| fallback would
  # require detecting an all-NA column, which is out of scope here -- the
  # explicit "no P.Value column" case above is the one the design targets.
  expect_equal(
    pelsa_volcano_label_rows(df, "top_n_adjp", n_top_adjp = 1L),
    c(1L, 2L)
  )
})

test_that("build_plot bakes top_n_adjp labels using the passed N", {
  df <- data.frame(
    id = c("p1", "p2", "p3"), logFC = c(2, -2, 5), logP = c(3, 4, 0.5),
    adj.P.Val = c(0.01, 0.02, 0.9), P.Value = c(0.001, 0.002, 0.8),
    Significant = c(TRUE, TRUE, FALSE),
    sig_direction = c("up", "down", "ns"),
    sig_color = c("darkred", "#1f4e9c", "gray"),
    feature_class_primary = "none", feature_color = "#d3d3d3",
    winning_accession = c("A", "B", "C"), winning_gene = c("g1", "g2", "g3"),
    label = c("g1_aa10", "g2_aa20", "g3_aa30"), is_marker = c(FALSE, FALSE, FALSE),
    PG.ProteinAccessions = c("A", "B", "C"), PG.Genes = c("g1", "g2", "g3"),
    pep_start = c(10L, 20L, 30L), pep_end = c(18L, 28L, 38L),
    stringsAsFactors = FALSE
  )
  attr(df, "y_cutoff") <- 1.0
  p <- pelsa_volcano_build_plot(df, full_df = df, color_mode = "significance",
         label_mode = "top_n_adjp", n_top_adjp = 1L,
         source_id = "x")
  b <- suppressWarnings(plotly::plotly_build(p))
  ann <- b$x$layout$annotations
  # up bucket (p1 logFC=2, p3 logFC=5): p1 wins on smallest adj.P.Val (0.01);
  # p3 is excluded by ranking, not significance, despite being non-significant.
  # down bucket (p2 only): kept.
  expect_equal(length(ann), 2L)
  borders <- vapply(ann, function(a) a$bordercolor, "")
  expect_setequal(borders, c("darkred", "#1f4e9c"))
})

test_that(".pelsa_export_ggplot bakes top_n_markers labels using the passed N", {
  df <- data.frame(
    logFC = c(2, 1.5, -2, -1.5), logP = c(3, 2, 3, 2),
    sig_direction = c("up", "up", "down", "down"),
    feature_class_primary = "none",
    is_marker = c(TRUE, TRUE, TRUE, TRUE),
    adj.P.Val = c(0.2, 0.01, 0.3, 0.02),
    label = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  attr(df, "y_cutoff") <- 1.0
  g <- .pelsa_export_ggplot(df, full_df = df, color_mode = "significance",
                            label_mode = "top_n_markers", n_top_markers = 1L)
  # geom_label_repel layer's data should contain exactly 2 labeled rows
  # (row 2 = smallest adj.P.Val in the up/logFC>0 bucket, row 4 = smallest in
  # the down/logFC<0 bucket).
  label_layer <- Filter(function(l) inherits(l$geom, "GeomLabel") ||
                                     inherits(l$geom, "GeomText") ||
                                     inherits(l$geom, "GeomLabelRepel"), g$layers)
  expect_true(length(label_layer) >= 1L)
  lab_data <- label_layer[[length(label_layer)]]$data
  expect_equal(nrow(lab_data), 2L)
  expect_setequal(lab_data$label, c("b", "d"))
})

test_that("volcano build adds boxed annotations (white bg, point-colored border)", {
  df <- data.frame(
    id = c("p1", "p2"), logFC = c(-2, 2), logP = c(3, 4),
    adj.P.Val = c(0.001, 0.0001), P.Value = c(0.001, 0.0001),
    Significant = c(TRUE, TRUE), sig_direction = c("down", "up"),
    sig_color = c("#1f4e9c", "darkred"),
    feature_class_primary = "none", feature_color = "#d3d3d3",
    winning_accession = c("A", "B"), winning_gene = c("g1", "g2"),
    label = c("g1_aa10", "g2_aa50"), is_marker = c(TRUE, TRUE),
    PG.ProteinAccessions = c("A", "B"), PG.Genes = c("g1", "g2"),
    pep_start = c(10L, 50L), pep_end = c(18L, 58L), stringsAsFactors = FALSE
  )
  attr(df, "y_cutoff") <- 1.0
  p <- pelsa_volcano_build_plot(df, full_df = df, color_mode = "significance",
         label_mode = "all_markers",
         source_id = "x")
  b <- suppressWarnings(plotly::plotly_build(p))
  ann <- b$x$layout$annotations
  # Both points are far apart -> both labeled (overlap suppressor keeps both).
  expect_equal(length(ann), 2L)
  expect_true(all(vapply(ann, function(a) a$bgcolor, "") ==
                    "rgba(255,255,255,0.85)"))
  # Offset from the point (Statistics-tab scheme): no arrow, shifted up-and-right.
  expect_true(all(vapply(ann, function(a) isFALSE(a$showarrow), logical(1))))
  expect_true(all(vapply(ann, function(a) a$xshift, 0) == 6))
  # Each box's border = its point's own color (order-independent: both present).
  borders <- vapply(ann, function(a) a$bordercolor, "")
  expect_setequal(borders, c("#1f4e9c", "darkred"))
})

test_that("volcano label overlap-suppressor drops piled-up labels", {
  # Two labeled points sit on top of each other RELATIVE TO the plot range (the
  # spread points p3/p4 set a wide range so the two near-identical labeled points
  # normalize to ~the same spot) -> only one label survives.
  df <- data.frame(
    id = c("p1", "p2", "p3", "p4"),
    logFC = c(1.00, 1.02, -5, 5), logP = c(3.00, 3.02, 0.1, 9),
    adj.P.Val = c(0.001, 0.002, 0.5, 0.5),
    P.Value = c(0.001, 0.002, 0.5, 0.5),
    Significant = c(TRUE, TRUE, FALSE, FALSE),
    sig_direction = c("up", "up", "ns", "ns"),
    sig_color = c("darkred", "darkred", "gray", "gray"),
    feature_class_primary = "none", feature_color = "#d3d3d3",
    winning_accession = c("A", "B", "C", "D"),
    winning_gene = c("g1", "g2", "g3", "g4"),
    label = c("g1_aa10", "g2_aa11", "", ""),
    is_marker = c(TRUE, TRUE, FALSE, FALSE),
    PG.ProteinAccessions = c("A", "B", "C", "D"),
    PG.Genes = c("g1", "g2", "g3", "g4"),
    pep_start = c(10L, 11L, 1L, 1L), pep_end = c(18L, 19L, 5L, 5L),
    stringsAsFactors = FALSE
  )
  attr(df, "y_cutoff") <- 1.0
  p <- pelsa_volcano_build_plot(df, full_df = df, color_mode = "significance",
         label_mode = "all_markers",
         source_id = "x")
  b <- suppressWarnings(plotly::plotly_build(p))
  expect_equal(length(b$x$layout$annotations), 1L)   # piled-up -> 1 kept
})

test_that("thin note: NULL when nothing thinned, string otherwise", {
  expect_null(pelsa_volcano_thin_note(list(n_shown = 100, n_total = 100)))
  note <- pelsa_volcano_thin_note(list(n_shown = 30, n_total = 100))
  expect_true(grepl("30", note) && grepl("100", note))
})

# ---- annotation-LIST helpers (relayout fast-path; Stage C) -----------------

# A small two-marker volcano df with non-empty labels, well separated so the
# overlap suppressor keeps both. Mirrors the build-annotation fixture above.
.mk_label_df <- function() {
  df <- data.frame(
    id = c("p1", "p2"), logFC = c(-2, 2), logP = c(3, 4),
    adj.P.Val = c(0.001, 0.0001), P.Value = c(0.001, 0.0001),
    Significant = c(TRUE, TRUE), sig_direction = c("down", "up"),
    sig_color = c("#1f4e9c", "darkred"),
    feature_class_primary = "none", feature_color = "#d3d3d3",
    winning_accession = c("A", "B"), winning_gene = c("g1", "g2"),
    label = c("g1_aa10", "g2_aa50"), is_marker = c(TRUE, TRUE),
    PG.ProteinAccessions = c("A", "B"), PG.Genes = c("g1", "g2"),
    pep_start = c(10L, 50L), pep_end = c(18L, 58L), stringsAsFactors = FALSE
  )
  attr(df, "y_cutoff") <- 1.0
  df
}

test_that("label_annotation_list: empty/NULL -> list(); each spec well-formed", {
  expect_identical(pelsa_volcano_label_annotation_list(NULL, "significance"),
                   list())
  expect_identical(
    pelsa_volcano_label_annotation_list(.mk_label_df()[0, ], "significance"),
    list())

  df <- .mk_label_df()
  anns <- pelsa_volcano_label_annotation_list(df, "significance", full_df = df)
  expect_equal(length(anns), 2L)
  # Each spec carries the relayout-required keys.
  for (a in anns) {
    expect_true(all(c("x", "y", "text", "bordercolor") %in% names(a)))
    expect_equal(a$bgcolor, "rgba(255,255,255,0.85)")
  }
  expect_setequal(vapply(anns, function(a) a$bordercolor, ""),
                  c("#1f4e9c", "darkred"))
})

# ---------------------------------------------------------------------------
# PASS 2 (7D-7F) PURE HELPERS
# ---------------------------------------------------------------------------

.mk_volcano_df <- function() {
  data.frame(
    id                   = c("PEPA", "PEPB", "PEPC"),
    logFC                = c(2.0, -1.5, 0.1),
    adj.P.Val            = c(0.001, 0.02, 0.8),
    P.Value              = c(0.0001, 0.005, 0.7),
    logP                 = c(4.0, 2.3, 0.15),
    label                = c("G1_aa10", "G2_aa20", "G1_aa30"),
    winning_accession    = c("ACC1", "ACC2", "ACC1"),
    winning_gene         = c("G1", "G2", "G1"),
    PG.ProteinAccessions = c("ACC1", "ACC2", "ACC1"),
    PG.Genes             = c("G1", "G2", "G1"),
    feature_class_primary = c("none", "none", "none"),
    feature_color        = c("#d3d3d3", "#d3d3d3", "#d3d3d3"),
    sig_color            = c("darkred", "#1f4e9c", "gray"),
    pep_start            = c(10L, 20L, 30L),
    pep_end              = c(14L, 24L, 34L),
    is_marker            = c(TRUE, FALSE, TRUE),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
}

test_that("resolve_click maps event (x,y) -> nearest peptide + winning accession", {
  df <- .mk_volcano_df()
  # Click exactly on PEPB (logFC=-1.5, logP=2.3).
  res <- pelsa_volcano_resolve_click(data.frame(x = -1.5, y = 2.3), df)
  expect_equal(res$peptide_seq, "PEPB")
  expect_equal(res$accession, "ACC2")
  expect_equal(res$row, 2L)
  # A noisy click near PEPA snaps to PEPA.
  res2 <- pelsa_volcano_resolve_click(data.frame(x = 1.9, y = 3.95), df)
  expect_equal(res2$peptide_seq, "PEPA")
  # NULL event / empty df -> NULL.
  expect_null(pelsa_volcano_resolve_click(NULL, df))
  expect_null(pelsa_volcano_resolve_click(data.frame(x = 1, y = 1),
                                          df[0, , drop = FALSE]))
  # No-coordinate event -> NULL.
  expect_null(pelsa_volcano_resolve_click(data.frame(x = NA_real_,
                                                     y = NA_real_), df))
})

test_that("resolve_click falls back to first PG token when winning_accession NA", {
  df <- .mk_volcano_df()
  df$winning_accession <- NA_character_
  df$PG.ProteinAccessions <- c("X1;X2", "Y1", "Z1")
  res <- pelsa_volcano_resolve_click(data.frame(x = 2.0, y = 4.0), df)
  expect_equal(res$accession, "X1")
})

test_that("sibling_mask flags every row of the pinned protein", {
  df <- .mk_volcano_df()
  m <- pelsa_volcano_sibling_mask(df, "ACC1")
  expect_equal(m$siblings, c(TRUE, FALSE, TRUE))   # rows 1 & 3 are ACC1
  expect_equal(m$n_siblings, 2L)
  # NULL / NA / absent accession -> no siblings.
  expect_equal(pelsa_volcano_sibling_mask(df, NULL)$n_siblings, 0L)
  expect_equal(pelsa_volcano_sibling_mask(df, NA_character_)$n_siblings, 0L)
  expect_equal(pelsa_volcano_sibling_mask(df, "NOPE")$n_siblings, 0L)
})

test_that("labels_sidecar emits the exact 12 columns in order", {
  df <- .mk_volcano_df()
  out <- pelsa_volcano_labels_sidecar(df, "all_peptide")
  expect_equal(colnames(out),
               c("panel", "peptide_sequence", "gene", "accession", "pep_start",
                 "display_label", "feature_class_primary", "winning_accession",
                 "winning_gene", "logFC", "adj_p", "raw_p"))
  expect_equal(nrow(out), 3L)
  expect_true(all(out$panel == "all_peptide"))
  expect_equal(out$peptide_sequence, c("PEPA", "PEPB", "PEPC"))
  expect_equal(out$adj_p, df$adj.P.Val)
  expect_equal(out$raw_p, df$P.Value)
  expect_equal(out$winning_accession, df$winning_accession)
  # Empty df -> zero rows but full 12-col width.
  empty <- pelsa_volcano_labels_sidecar(df[0, , drop = FALSE], "best_peptide")
  expect_equal(ncol(empty), 12L)
  expect_equal(nrow(empty), 0L)
})

test_that("volcano tooltip is Peptide/Accession/Gene/Position/logFC/adj.P", {
  df <- data.frame(
    id = "PEPX", logFC = 1.23, logP = 3, adj.P.Val = 0.004, P.Value = 0.001,
    Significant = TRUE, sig_color = "darkred", feature_color = "#111",
    feature_class_primary = "none", winning_accession = "ACCX",
    winning_gene = "GX", PG.Genes = "GX", PG.ProteinAccessions = "ACCX",
    pep_start = 7L, pep_end = 17L, is_marker = FALSE, label = "GX_aa7",
    stringsAsFactors = FALSE, check.names = FALSE)
  p <- pelsa_volcano_build_plot(df, full_df = df, label_mode = "none",
                                source_id = "s")
  b <- plotly::plotly_build(p)
  txt <- unlist(lapply(b$x$data, function(t) t$text))
  txt <- txt[!is.na(txt) & nzchar(txt)]
  expect_true(any(grepl("Peptide: GX_aa7", txt, fixed = TRUE)))
  expect_true(any(grepl("Position: 7-17", txt, fixed = TRUE)))
  expect_true(any(grepl("logFC: 1.23", txt)))
  expect_true(any(grepl("adj.P: 0.004", txt)))
  expect_true(any(grepl("Accession: ACCX", txt, fixed = TRUE)))
  expect_true(any(grepl("Gene: GX", txt, fixed = TRUE)))
})

test_that("build_plot returns a plotly object for both source ids", {
  df <- .mk_volcano_df()
  p <- pelsa_volcano_build_plot(df, full_df = df, source_id = "s1")
  expect_s3_class(p, "plotly")
  # With a baked selection (the gold-highlight path) it still builds.
  p2 <- pelsa_volcano_build_plot(
    df, full_df = df, source_id = "s2",
    selection = list(origin = "click", accession = "ACC1",
                     peptide_seq = "PEPA"),
    register_click = TRUE)
  expect_s3_class(p2, "plotly")
})

test_that("sibling_mask: single-peptide protein -> exactly one TRUE; builds", {
  df <- .mk_volcano_df()  # ACC2 maps to exactly one row (PEPB, row 2)
  m <- pelsa_volcano_sibling_mask(df, "ACC2")
  expect_equal(m$n_siblings, 1L)
  expect_equal(which(m$siblings), 2L)
  # End-to-end: pinning a single-peptide protein builds without error.
  expect_s3_class(
    pelsa_volcano_build_plot(
      df, full_df = df, source_id = "single",
      selection = list(origin = "click", accession = "ACC2",
                       peptide_seq = "PEPB")),
    "plotly")
})

test_that("resolve_click: two near-identical points -> first df row (which.min tie)", {
  df <- .mk_volcano_df()
  # Make rows 1 and 3 share coordinates; a click there must pick the FIRST
  # (row 1) per the documented which.min tie behavior.
  df$logFC[3] <- df$logFC[1]
  df$logP[3]  <- df$logP[1]
  res <- pelsa_volcano_resolve_click(
    data.frame(x = df$logFC[1], y = df$logP[1]), df)
  expect_equal(res$row, 1L)
  expect_equal(res$peptide_seq, "PEPA")
})

test_that("intensity_line_ggplot: single vs faceted panel both build", {
  # Non-marker (single panel value) -> no facet; marker (two values) -> facet.
  one <- data.frame(
    accession = "ACC1", peptide_seq = "PEPA", pep_start = 10L, pep_end = 18L,
    pep_occurrence_idx = 1L, aa_label = "aa10", panel = "Significant",
    condition = factor(c("A", "B"), levels = c("A", "B")),
    mean_log2 = c(1, 2), n_rep_nonNA = c(2L, 2L), stringsAsFactors = FALSE)
  expect_s3_class(pelsa_intensity_line_ggplot(one), "ggplot")
  two <- rbind(one, transform(one, panel = "Non-significant", mean_log2 = c(3, 4)))
  g <- pelsa_intensity_line_ggplot(two)
  expect_s3_class(g, "ggplot")

  # Clean hover tooltip (.tip): aa_label, position start->end, sequence,
  # condition, mean intensity - surfaced via the text aesthetic.
  b <- suppressWarnings(plotly::plotly_build(
    plotly::ggplotly(pelsa_intensity_line_ggplot(one), tooltip = "text")))
  tt <- unlist(lapply(b$x$data, function(tr) tr$text))
  tt <- tt[!is.na(tt) & nzchar(tt)]
  expect_true(any(grepl("Position: 10 -> 18", tt, fixed = TRUE)))
  expect_true(any(grepl("Sequence: PEPA", tt, fixed = TRUE)))
  expect_false(any(grepl("interaction", tt)))   # no raw aesthetic leakage
})

test_that("intensity_line_plot: two panels render as a subplot (no facet strip)", {
  two <- data.frame(
    accession = "ACC1",
    peptide_seq = c("PEPA", "PEPA", "PEPB", "PEPB"),
    pep_start = c(10L, 10L, 50L, 50L), pep_end = c(18L, 18L, 58L, 58L),
    pep_occurrence_idx = 1L, aa_label = c("aa10", "aa10", "aa50", "aa50"),
    panel = c("Significant", "Significant", "Non-significant", "Non-significant"),
    condition = factor(rep(c("A", "B"), 2), levels = c("A", "B")),
    mean_log2 = c(1, 2, 3, 4), n_rep_nonNA = 2L, stringsAsFactors = FALSE)
  p <- pelsa_intensity_line_plot(two, pinned_label = "aa10")
  expect_s3_class(p, "plotly")
})

# ---------------------------------------------------------------------------
# testServer (light)
# ---------------------------------------------------------------------------

# A synthetic stat_results[[ome]] with two contrasts + peptide identity columns.
.mk_stat_results <- function() {
  list(Proteome = data.frame(
    id                   = c("PEPA", "PEPB", "PEPC"),
    PEP.StrippedSequence = c("PEPA", "PEPB", "PEPC"),
    PG.ProteinAccessions = c("ACC1", "ACC2", "ACC1"),
    PG.Genes             = c("G1", "G2", "G1"),
    logFC.A_over_B       = c(2.0, -1.5, 0.1),
    adj.P.Val.A_over_B   = c(0.001, 0.02, 0.8),
    P.Value.A_over_B     = c(0.0001, 0.005, 0.7),
    logFC.A_over_C       = c(1.0, 0.2, -0.3),
    adj.P.Val.A_over_C   = c(0.04, 0.6, 0.9),
    P.Value.A_over_C     = c(0.01, 0.5, 0.85),
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  ))
}

.mk_stat_params <- function() {
  list(Proteome = list(test = "Two-sample Moderated T-test",
                       groups = c("A", "B", "C"),
                       contrasts = c("A / B", "A / C"),
                       stat = "adj.p.val", cutoff = 0.05))
}

.mk_cache <- function() {
  matched <- data.frame(
    PEP.StrippedSequence = c("PEPA", "PEPB", "PEPC"),
    accession            = c("ACC1", "ACC2", "ACC1"),
    gene                 = c("G1", "G2", "G1"),
    pep_start            = c(10L, 20L, 30L),
    pep_end              = c(14L, 24L, 34L),
    stringsAsFactors     = FALSE
  )
  list(Proteome = list(matched = matched,
                       annotation_features = .mk_annotation_features(nrow(matched))))
}

# Minimal row-aligned annotation_features stub for the new cache shape (the
# volcano server recomputes annotation itself, so the values are placeholders).
.mk_annotation_features <- function(n) {
  data.frame(
    feature_class_primary = rep("none", n),
    winning_accession     = rep(NA_character_, n),
    winning_gene          = rep(NA_character_, n),
    stringsAsFactors      = FALSE
  )
}

.mk_setup_state <- function() {
  # species / marker_rows are PER-OME named lists (keyed by ome "Proteome").
  list(species = list(Proteome = NULL),  # NULL -> feat_df NULL path; no network
       marker_rows = list(Proteome = data.frame(
         accession = "ACC1", gene = "G1", stringsAsFactors = FALSE)))
}

# A fuller fixture for the 7D/7E/7F testServer paths: matched carries .row_id +
# pep_occurrence_idx, the cache holds a processed matrix-like GCT seam, and the
# setup_state has condition_col / condition_order so the 3C intensity path runs.
.mk_stat_results_full <- function() {
  list(Proteome = data.frame(
    id                   = c("PEPA", "PEPB", "PEPC"),
    .row_id              = c(1L, 2L, 3L),
    PEP.StrippedSequence = c("PEPA", "PEPB", "PEPC"),
    PG.ProteinAccessions = c("ACC1", "ACC2", "ACC1"),
    PG.Genes             = c("G1", "G2", "G1"),
    logFC.A_over_B       = c(2.0, -1.5, 0.1),
    adj.P.Val.A_over_B   = c(0.001, 0.02, 0.8),
    P.Value.A_over_B     = c(0.0001, 0.005, 0.7),
    stringsAsFactors     = FALSE, check.names = FALSE
  ))
}

.mk_cache_full <- function() {
  matched <- data.frame(
    .row_id              = c(1L, 2L, 3L),
    PEP.StrippedSequence = c("PEPA", "PEPB", "PEPC"),
    accession            = c("ACC1", "ACC2", "ACC1"),
    gene                 = c("G1", "G2", "G1"),
    pep_start            = c(10L, 20L, 30L),
    pep_end              = c(14L, 24L, 34L),
    pep_occurrence_idx   = c(1L, 1L, 1L),
    stringsAsFactors     = FALSE
  )
  list(Proteome = list(matched = matched,
                       annotation_features = .mk_annotation_features(nrow(matched))))
}

# A real cmapR GCT (3 peptides x 4 samples) with a cdesc `condition` column so
# the section's processed_mat_r / condition_map_r reactives resolve. Rows align
# to matched_cache .row_id (1..3); 2 conditions x 2 replicates.
.mk_gct <- function() {
  m <- matrix(c(1, 2, 5, 6,   2, 3, 6, 7,   3, 4, 7, 8),
              nrow = 3, byrow = TRUE,
              dimnames = list(c("PEPA", "PEPB", "PEPC"),
                              c("s1", "s2", "s3", "s4")))
  new("GCT",
      mat = m,
      rdesc = data.frame(id = c("PEPA", "PEPB", "PEPC")),
      cdesc = data.frame(condition = c("A", "A", "B", "B"),
                         row.names = c("s1", "s2", "s3", "s4")),
      rid = c("PEPA", "PEPB", "PEPC"),
      cid = c("s1", "s2", "s3", "s4"))
}

.mk_setup_state_full <- function() {
  list(species = list(Proteome = NULL),
       marker_rows = list(Proteome = data.frame(
         accession = "ACC1", gene = "G1", stringsAsFactors = FALSE)),
       condition_col = list(Proteome = "condition"),
       condition_order = list(Proteome = c("A", "B")))
}

test_that("gate: NULL stat_results shows the notice and renders no plot", {
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = list(
      id = "Proteome", ome = "Proteome",
      GCT_processed = reactive(NULL), parameters = reactive(NULL),
      default_annotation_column = reactive(NULL), color_map = reactive(NULL),
      stat_results = reactive(NULL), stat_params = reactive(.mk_stat_params()),
      pelsa_analysis = reactive(.mk_cache()),
      pelsa_setup_state = reactive(.mk_setup_state()),
      poi_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list()),
      n_top_adjp_registry = reactiveVal(list()),
      n_top_markers_registry = reactiveVal(list())
    ),
    {
      # stat_df_raw carries a validate(); accessing it errors with the message.
      err <- tryCatch(stat_df_raw(), error = function(e) conditionMessage(e))
      expect_match(err, "Statistics tab", fixed = FALSE)
    }
  )
})

test_that("good inputs: choices populate, df builds, switch frees prior, color toggles, note", {
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = list(
      id = "Proteome", ome = "Proteome",
      GCT_processed = reactive(NULL), parameters = reactive(NULL),
      default_annotation_column = reactive(NULL), color_map = reactive(NULL),
      stat_results = reactive(.mk_stat_results()),
      stat_params = reactive(.mk_stat_params()),
      pelsa_analysis = reactive(.mk_cache()),
      pelsa_setup_state = reactive(.mk_setup_state()),
      poi_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list()),
      n_top_adjp_registry = reactiveVal(list()),
      n_top_markers_registry = reactiveVal(list())
    ),
    {
      # Contrast choices populate (named label -> suffix).
      ch <- contrast_choices()
      expect_equal(unname(ch), c("A_over_B", "A_over_C"))

      # Default active contrast = first; df builds; cache holds ONLY it.
      session$setInputs(pelsa_color_mode = "significance",
                        pelsa_label_mode = "all_markers")
      expect_equal(active_contrast(), "A_over_B")
      df1 <- active_volcano_df()
      expect_true(is.data.frame(df1) && nrow(df1) == 3L)
      expect_equal(names(volcano_df_cache()), "A_over_B")

      # POI registry seeded with the Setup marker accession for this contrast.
      expect_true("ACC1" %in% poi_registry()[["Proteome::A_over_B"]])

      # Marker flag picked up the marker accession.
      expect_true(any(df1$is_marker))

      # Switch contrast -> prior contrast df is FREED (single-entry cache).
      session$setInputs(pelsa_volcano_contrast = "A_over_C")
      df2 <- active_volcano_df()
      expect_equal(active_contrast(), "A_over_C")
      expect_equal(names(volcano_df_cache()), "A_over_C")  # A_over_B freed
      expect_false("A_over_B" %in% names(volcano_df_cache()))

      # Color toggle switches the column source of truth.
      sig <- pelsa_volcano_color_column(df2, "significance")
      feat <- pelsa_volcano_color_column(df2, "feature")
      expect_length(sig, nrow(df2))
      expect_length(feat, nrow(df2))

      # No downsampling: the plot consumes the FULL df (every point), so plot_df
      # equals active_volcano_df row-for-row. The thin-note output is gone.
      expect_equal(nrow(plot_df()), nrow(active_volcano_df()))
      expect_identical(plot_df(), active_volcano_df())
      # The thin-note output was removed entirely  -  referencing it now errors.
      expect_error(output$pelsa_thin_note, "hasn't been defined")

      # Plot output exists (renders without error).
      expect_false(is.null(output$pelsa_volcano_plot))
    }
  )
})

# feat_df notification tests -----------------------------------------------

test_that("feat_df warns and returns NULL when annotation path exists but file is missing", {
  # local_mocked_bindings cannot intercept shiny::showNotification inside a
  # moduleServer namespace; fall back to asserting on the message() log.
  missing_path <- tempfile(fileext = ".tsv")   # never created on disk
  result_val <- NULL
  expect_message(
    shiny::testServer(
      PELSASection3_Ome_Server,
      args = list(
        id = "Proteome", ome = "Proteome",
        GCT_processed = reactive(NULL), parameters = reactive(NULL),
        default_annotation_column = reactive(NULL), color_map = reactive(NULL),
        stat_results = reactive(NULL), stat_params = reactive(.mk_stat_params()),
        pelsa_analysis = reactive(.mk_cache()),
        pelsa_setup_state = reactive(list(
          species      = list(Proteome = NULL),
          marker_rows  = list(Proteome = data.frame(
            accession = "ACC1", gene = "G1", stringsAsFactors = FALSE)),
          self_curated     = list(Proteome = FALSE),
          annotation_path  = list(Proteome = missing_path)
        )),
        poi_registry          = reactiveVal(list()),
        label_mode_registry   = reactiveVal(list()),
        n_top_adjp_registry = reactiveVal(list()),
        n_top_markers_registry = reactiveVal(list())
      ),
      {
        result_val <<- feat_df()
      }
    ),
    regexp = "PELSA feat_df"
  )
  expect_null(result_val)
})

test_that("feat_df is silent and returns NULL for a self-curated dataset", {
  result_val <- NULL
  # expect_no_message verifies zero message() calls (self-curated stays silent).
  expect_no_message(
    shiny::testServer(
      PELSASection3_Ome_Server,
      args = list(
        id = "Proteome", ome = "Proteome",
        GCT_processed = reactive(NULL), parameters = reactive(NULL),
        default_annotation_column = reactive(NULL), color_map = reactive(NULL),
        stat_results = reactive(NULL), stat_params = reactive(.mk_stat_params()),
        pelsa_analysis = reactive(.mk_cache()),
        pelsa_setup_state = reactive(list(
          species      = list(Proteome = NULL),
          marker_rows  = list(Proteome = data.frame(
            accession = "ACC1", gene = "G1", stringsAsFactors = FALSE)),
          self_curated     = list(Proteome = TRUE),
          annotation_path  = list(Proteome = "irrelevant_path.tsv")
        )),
        poi_registry          = reactiveVal(list()),
        label_mode_registry   = reactiveVal(list()),
        n_top_adjp_registry = reactiveVal(list()),
        n_top_markers_registry = reactiveVal(list())
      ),
      {
        result_val <<- feat_df()
      }
    )
  )
  expect_null(result_val)
})

# (a) cache NULL but stats present -> section surfaces the Start-Analysis notice
#     and does NOT error / half-render. The #1 reviewer gap.
test_that("cache NULL + stats present: section shows Start-Analysis notice, no df", {
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = list(
      id = "Proteome", ome = "Proteome",
      GCT_processed = reactive(NULL), parameters = reactive(NULL),
      default_annotation_column = reactive(NULL), color_map = reactive(NULL),
      stat_results = reactive(.mk_stat_results()),
      stat_params = reactive(.mk_stat_params()),
      pelsa_analysis = reactive(NULL),                 # cache MISSING
      pelsa_setup_state = reactive(.mk_setup_state()),
      poi_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list()),
      n_top_adjp_registry = reactiveVal(list()),
      n_top_markers_registry = reactiveVal(list())
    ),
    {
      # cache_entry is NULL; the section-level gate renders the Setup notice.
      expect_null(cache_entry())
      html <- as.character(output$section_contents$html %||%
                             output$section_contents)
      expect_match(html, "Start Analysis", fixed = TRUE)
      # active_volcano_df carries a validate() (the cache guard)  -  it does not
      # silently half-build a df.
      err <- tryCatch({ session$setInputs(pelsa_volcano_contrast = "A_over_B");
                        active_volcano_df(); "NO_ERROR" },
                      error = function(e) conditionMessage(e))
      expect_match(err, "Start Analysis", fixed = TRUE)
    }
  )
})

# (b) pep-span-attach partial miss: a stat_results peptide absent from matched
#     -> NA span flows to "none" coloring, no error.
test_that("pep-span partial miss: NA span -> 'none' feature color, no error", {
  stat <- data.frame(
    PEP.StrippedSequence = c("PEPA", "PEPB"),
    PG.ProteinAccessions = c("ACC1", "ACC2"),
    PG.Genes             = c("G1", "G2"),
    logFC.A_over_B       = c(2.0, -1.5),
    adj.P.Val.A_over_B   = c(0.001, 0.02),
    P.Value.A_over_B     = c(0.0001, 0.005),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
  # Only PEPA is in the matched cache; PEPB has no span.
  matched <- data.frame(
    PEP.StrippedSequence = "PEPA", accession = "ACC1", gene = "G1",
    pep_start = 10L, pep_end = 14L, stringsAsFactors = FALSE
  )
  sdf <- pelsa_volcano_stat_df(stat, matched)
  expect_equal(sdf$pep_start, c(10L, NA_integer_))

  feat_none <- data.frame(accession = character(0), start = integer(0),
                          end = integer(0), feature_class = character(0))
  out <- pelsa_build_volcano_df(sdf, matched, feat_df = feat_none,
                                markers = character(0), contrast = "A_over_B",
                                opts = list(panel = "all_peptide"))
  expect_equal(nrow(out), 2L)
  # PEPB (no overlapping feature) resolves to the "none" class/color.
  pepb <- out[out$id == "PEPB", ]
  expect_equal(pepb$feature_class_primary, "none")
  expect_equal(pepb$feature_color, unname(PELSA_FEATURE_COLORS["none"]))
})

# (c) zero-significant contrast -> y_cutoff Inf, no threshold line drawn.
test_that("zero-significant contrast: y_cutoff is Inf", {
  stat <- data.frame(
    PEP.StrippedSequence = c("PEPA", "PEPB"),
    PG.ProteinAccessions = c("ACC1", "ACC2"),
    pep_start = c(10L, 20L), pep_end = c(14L, 24L),
    logFC.A_over_B     = c(0.1, -0.2),
    adj.P.Val.A_over_B = c(0.8, 0.9),   # nothing passes 0.05
    P.Value.A_over_B   = c(0.7, 0.85),
    stringsAsFactors   = FALSE, check.names = FALSE
  )
  matched <- data.frame(
    PEP.StrippedSequence = c("PEPA", "PEPB"), accession = c("ACC1", "ACC2"),
    gene = c("G1", "G2"), pep_start = c(10L, 20L), pep_end = c(14L, 24L),
    stringsAsFactors = FALSE
  )
  feat_none <- data.frame(accession = character(0), start = integer(0),
                          end = integer(0), feature_class = character(0))
  out <- pelsa_build_volcano_df(stat, matched, feat_df = feat_none,
                                markers = character(0), contrast = "A_over_B",
                                opts = list(panel = "all_peptide",
                                            sig_cutoff = 0.05))
  expect_false(any(out$Significant))
  expect_true(is.infinite(attr(out, "y_cutoff")))
  # The module only draws geom_hline when is.finite(y_cutoff)  -  Inf -> no line.
  expect_false(is.finite(attr(out, "y_cutoff")))
})

# (d) feat_df NULL: the module's feat_df reactive returns NULL (species NULL),
#     3A colors everything "none"; feature color-mode resolves with no error.
test_that("feat_df NULL: feature color-mode resolves to the 'none' color", {
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = list(
      id = "Proteome", ome = "Proteome",
      GCT_processed = reactive(NULL), parameters = reactive(NULL),
      default_annotation_column = reactive(NULL), color_map = reactive(NULL),
      stat_results = reactive(.mk_stat_results()),
      stat_params = reactive(.mk_stat_params()),
      pelsa_analysis = reactive(.mk_cache()),
      pelsa_setup_state = reactive(.mk_setup_state()),  # species = NULL
      poi_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list()),
      n_top_adjp_registry = reactiveVal(list()),
      n_top_markers_registry = reactiveVal(list())
    ),
    {
      expect_null(feat_df())                       # species NULL -> NULL feat
      session$setInputs(pelsa_color_mode = "feature",
                        pelsa_label_mode = "all_markers",
                        pelsa_volcano_contrast = "A_over_B")
      df <- active_volcano_df()
      # No features supplied -> every peptide is class "none".
      expect_true(all(df$feature_class_primary == "none"))
      feat_cols <- pelsa_volcano_color_column(df, "feature")
      expect_true(all(feat_cols == unname(PELSA_FEATURE_COLORS["none"])))
      expect_false(is.null(output$pelsa_volcano_plot))  # renders, no error
    }
  )
})

# ---------------------------------------------------------------------------
# testServer (light): 7D best panel / 7E pin+intensity / 7F exports
# ---------------------------------------------------------------------------

.full_args <- function() {
  list(
    id = "Proteome", ome = "Proteome",
    GCT_processed = reactive(.mk_gct()),
    parameters = reactive(NULL),
    default_annotation_column = reactive(NULL), color_map = reactive(NULL),
    stat_results = reactive(.mk_stat_results_full()),
    stat_params = reactive(.mk_stat_params()),
    pelsa_analysis = reactive(.mk_cache_full()),
    pelsa_setup_state = reactive(.mk_setup_state_full()),
    poi_registry = reactiveVal(list()),
    label_mode_registry = reactiveVal(list()),
    n_top_adjp_registry = reactiveVal(list()),
    n_top_markers_registry = reactiveVal(list())
  )
}

test_that("PELSA volcano always renders SVG (scatter), never scattergl, regardless of use_webgl", {
  # Root cause (2026-07-05 investigation): scattergl's client-side WebGL
  # renderer can silently mis-paint per-point marker.color arrays on some
  # GPU/driver clients (colors wrong even though the R-built figure JSON is
  # provably correct -- verified by direct reproduction). The hasWebGL() probe
  # in R/app_ui.R only detects whether a WebGL context can be CREATED, not
  # whether it renders per-point colors correctly, so buggy clients still get
  # routed to scattergl. Force SVG for the PELSA volcano unconditionally so
  # significance/feature coloring is reliable, even when the caller passes
  # use_webgl = reactive(TRUE) (simulating a client whose probe reported
  # WebGL-capable).
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = list(
      id = "Proteome", ome = "Proteome",
      GCT_processed = reactive(NULL), parameters = reactive(NULL),
      default_annotation_column = reactive(NULL), color_map = reactive(NULL),
      stat_results = reactive(.mk_stat_results()),
      stat_params = reactive(.mk_stat_params()),
      pelsa_analysis = reactive(.mk_cache()),
      pelsa_setup_state = reactive(.mk_setup_state()),
      poi_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list()),
      n_top_adjp_registry = reactiveVal(list()),
      n_top_markers_registry = reactiveVal(list()),
      use_webgl = reactive(TRUE)
    ),
    {
      session$setInputs(pelsa_color_mode = "significance",
                        pelsa_label_mode = "all_markers")
      json <- output$pelsa_volcano_plot
      expect_true(is.character(json) && nzchar(json))
      expect_true(grepl('"type":"scatter"', json, fixed = TRUE))
      expect_false(grepl('"type":"scattergl"', json, fixed = TRUE))
    }
  )
})

test_that("changing label mode on one contrast applies to every contrast of the same ome", {
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = list(
      id = "Proteome", ome = "Proteome",
      GCT_processed = reactive(NULL), parameters = reactive(NULL),
      default_annotation_column = reactive(NULL), color_map = reactive(NULL),
      stat_results = reactive(.mk_stat_results()),
      stat_params = reactive(.mk_stat_params()),
      pelsa_analysis = reactive(.mk_cache()),
      pelsa_setup_state = reactive(.mk_setup_state()),
      poi_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list()),
      n_top_adjp_registry = reactiveVal(list()),
      n_top_markers_registry = reactiveVal(list()),
      use_webgl = reactive(FALSE)
    ),
    {
      choices <- contrast_choices()
      expect_true(length(choices) >= 2L)  # fixture must define >=2 contrasts
      contrast_a <- unname(choices[[1L]])
      contrast_b <- unname(choices[[2L]])

      # Set the label mode while viewing contrast A.
      session$setInputs(pelsa_color_mode = "significance",
                        pelsa_volcano_contrast = contrast_a,
                        pelsa_label_mode = c("all_markers", "all_significant"))
      expect_identical(sort(label_mode_for_ome()),
                       sort(c("all_markers", "all_significant")))

      # Switch to contrast B (same ome) WITHOUT touching the checkboxes -
      # the stored selection must already apply here too.
      session$setInputs(pelsa_volcano_contrast = contrast_b)
      expect_identical(sort(label_mode_for_ome()),
                       sort(c("all_markers", "all_significant")))

      # The rendered plot for contrast B reflects the union of both modes.
      df <- active_volcano_df()
      expected <- pelsa_volcano_label_rows(
        df, mode = c("all_markers", "all_significant"))
      json <- output$pelsa_volcano_plot
      expect_true(is.character(json) && nzchar(json))
      labs <- df$label[expected]
      labs <- labs[!is.na(labs) & nzchar(labs)]
      for (l in unique(labs)) {
        expect_true(grepl(l, json, fixed = TRUE),
                    info = paste("expected label", l, "in rendered JSON"))
      }
    }
  )
})

test_that("top_n_adjp and all_significant are mutually exclusive (independent of the marker pair)", {
  # NOTE on assertion target (deviation from the Task 6 brief's draft test):
  # the mutual-exclusion observer (R/tab_pelsa_section3.R ~L479) enforces
  # exclusivity ONLY via updateCheckboxGroupInput() (a client-bound message)
  # plus shinyjs::runjs() (disables the other checkbox in the DOM). In a real
  # browser, that update round-trips back to the server as a NEW
  # input$pelsa_label_mode value, which would then flow into
  # label_mode_registry via set_label_mode(). shiny::testServer has no live
  # client, so that round-trip never happens: input$pelsa_label_mode -- and
  # therefore label_mode_for_ome(), which is written directly off the raw
  # input$ value -- stays at whatever was passed to setInputs(), UNFILTERED,
  # even after session$flushReact(). This was confirmed with a diagnostic
  # script printing input$pelsa_label_mode and label_mode_for_ome() after each
  # setInputs() call (both remained the raw, unfiltered union). So
  # label_mode_for_ome() is NOT actually a reliable assertion target here --
  # asserting against it would make the test pass or fail independent of
  # whether the mutual-exclusion logic is correct. The genuinely observable,
  # deterministic contract is the `selected=` argument the code passes to
  # updateCheckboxGroupInput(); we mock that binding (shiny is a full
  # `@import`, so the mock must target the importing package's namespace)
  # and assert against the captured value instead.
  captured <- list()
  testthat::local_mocked_bindings(
    updateCheckboxGroupInput = function(session, inputId, selected = NULL, ...) {
      captured[[length(captured) + 1]] <<- selected
    },
    .package = "Protigy"
  )
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = list(
      id = "Proteome", ome = "Proteome",
      GCT_processed = reactive(NULL), parameters = reactive(NULL),
      default_annotation_column = reactive(NULL), color_map = reactive(NULL),
      stat_results = reactive(.mk_stat_results()),
      stat_params = reactive(.mk_stat_params()),
      pelsa_analysis = reactive(.mk_cache()),
      pelsa_setup_state = reactive(.mk_setup_state()),
      poi_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list()),
      n_top_adjp_registry = reactiveVal(list()),
      n_top_markers_registry = reactiveVal(list()),
      use_webgl = reactive(FALSE)
    ),
    {
      # Checking "all_significant" then "top_n_adjp": the mutual-
      # exclusion observer must issue an update dropping "all_significant".
      session$setInputs(pelsa_label_mode = c("all_significant"))
      session$setInputs(pelsa_label_mode = c("all_significant", "top_n_adjp"))
      expect_identical(captured[[length(captured)]], "top_n_adjp")

      # Marker pair is unaffected by the significant-pair state: selecting
      # "all_markers" alongside "top_n_adjp" must NOT trigger an
      # update that drops "top_n_adjp" (no significant-pair conflict).
      captured <<- list()
      session$setInputs(pelsa_label_mode = c("top_n_adjp", "all_markers"))
      for (sel in captured) {
        expect_true("top_n_adjp" %in% sel)
      }

      # Strengthen the above: with this specific combination, the observer's
      # significant-pair branch and marker-pair branch BOTH fire (each is an
      # unconditional if/else-if/else, and here "top_n_adjp" satisfies
      # the significant-pair's `if`, while "all_markers" satisfies the
      # marker-pair's `else if`) -- always in that fixed order, each emitting
      # exactly one updateCheckboxGroupInput() call. A cross-pair coupling bug
      # (e.g. the marker-pair branch wrongly dropping "top_n_adjp", or
      # the significant-pair branch wrongly dropping "all_markers") would slip
      # past a check that only looks at "top_n_adjp" membership, since
      # "all_markers" surviving is never verified. Assert on EACH call
      # individually, tied to the branch that produced it: the significant-
      # pair call (uses setdiff(modes, "all_significant"), so it must keep
      # both "top_n_adjp" AND "all_markers" intact -- "all_significant"
      # was never selected, so nothing should be dropped at all here); the
      # marker-pair call (uses setdiff(modes, "top_n_markers"), so it too must
      # keep both untouched, since "top_n_markers" was never selected).
      expect_length(captured, 2L)
      significant_pair_call <- captured[[1]]
      marker_pair_call <- captured[[2]]
      expect_setequal(significant_pair_call, c("top_n_adjp", "all_markers"))
      expect_setequal(marker_pair_call, c("top_n_adjp", "all_markers"))

      # Checking "top_n_markers" now must drop "all_markers" (marker pair
      # only) via the same updateCheckboxGroupInput mechanism.
      captured <<- list()
      session$setInputs(
        pelsa_label_mode = c("top_n_adjp", "all_markers", "top_n_markers"))
      last_sel <- captured[[length(captured)]]
      expect_true("top_n_markers" %in% last_sel)
      expect_false("all_markers" %in% last_sel)
    }
  )
})

test_that("top-N values are per-ome: set while viewing one contrast, applied when viewing another", {
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = list(
      id = "Proteome", ome = "Proteome",
      GCT_processed = reactive(NULL), parameters = reactive(NULL),
      default_annotation_column = reactive(NULL), color_map = reactive(NULL),
      stat_results = reactive(.mk_stat_results()),
      stat_params = reactive(.mk_stat_params()),
      pelsa_analysis = reactive(.mk_cache()),
      pelsa_setup_state = reactive(.mk_setup_state()),
      poi_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list()),
      n_top_adjp_registry = reactiveVal(list()),
      n_top_markers_registry = reactiveVal(list()),
      use_webgl = reactive(FALSE)
    ),
    {
      choices <- contrast_choices()
      expect_true(length(choices) >= 2L)
      contrast_a <- unname(choices[[1L]])
      contrast_b <- unname(choices[[2L]])

      # Default N is 3 before any input.
      expect_identical(n_top_adjp_for_ome(), 3L)
      expect_identical(n_top_markers_for_ome(), 3L)

      session$setInputs(pelsa_volcano_contrast = contrast_a,
                        pelsa_n_top_adjp = 8,
                        pelsa_n_top_markers = 2)
      expect_identical(n_top_adjp_for_ome(), 8L)
      expect_identical(n_top_markers_for_ome(), 2L)

      # Switch to a different contrast (same ome) - values still apply.
      session$setInputs(pelsa_volcano_contrast = contrast_b)
      expect_identical(n_top_adjp_for_ome(), 8L)
      expect_identical(n_top_markers_for_ome(), 2L)
    }
  )
})

test_that("invalid N input (blank/zero/negative) coerces to a valid integer >= 1", {
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = list(
      id = "Proteome", ome = "Proteome",
      GCT_processed = reactive(NULL), parameters = reactive(NULL),
      default_annotation_column = reactive(NULL), color_map = reactive(NULL),
      stat_results = reactive(.mk_stat_results()),
      stat_params = reactive(.mk_stat_params()),
      pelsa_analysis = reactive(.mk_cache()),
      pelsa_setup_state = reactive(.mk_setup_state()),
      poi_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list()),
      n_top_adjp_registry = reactiveVal(list()),
      n_top_markers_registry = reactiveVal(list()),
      use_webgl = reactive(FALSE)
    ),
    {
      session$setInputs(pelsa_n_top_adjp = 0)
      expect_identical(n_top_adjp_for_ome(), 1L)

      session$setInputs(pelsa_n_top_adjp = -3)
      expect_identical(n_top_adjp_for_ome(), 1L)

      # Blank/NA input goes through the is.na() fallback branch, whose default
      # is now 3 (not the max(1L, val) floor used by the 0/-3 cases above).
      session$setInputs(pelsa_n_top_adjp = NA)
      expect_identical(n_top_adjp_for_ome(), 3L)
    }
  )
})

test_that("7D: best-panel df built ONLY when the checkbox is ON", {
  shiny::testServer(PELSASection3_Ome_Server, args = .full_args(), {
    session$setInputs(pelsa_color_mode = "significance",
                      pelsa_label_mode = "all_markers",
                      pelsa_volcano_contrast = "A_over_B",
                      pelsa_show_best_panel = FALSE)
    # OFF: best cache stays empty (the reactive short-circuits on best_show()).
    expect_length(best_volcano_df_cache(), 0L)

    # ON: best-peptide df builds (panel = "best_peptide", one dot per peptide).
    session$setInputs(pelsa_show_best_panel = TRUE)
    bdf <- best_volcano_df()
    expect_true(is.data.frame(bdf) && nrow(bdf) >= 1L)
    expect_equal(names(best_volcano_df_cache()), "A_over_B")

    # Toggling OFF frees the best cache.
    session$setInputs(pelsa_show_best_panel = FALSE)
    expect_length(best_volcano_df_cache(), 0L)
  })
})

test_that("7E: a simulated pin populates metadata + computes 3C line data", {
  shiny::testServer(PELSASection3_Ome_Server, args = .full_args(), {
    session$setInputs(pelsa_color_mode = "significance",
                      pelsa_label_mode = "all_markers",
                      pelsa_volcano_contrast = "A_over_B")
    force(active_volcano_df())

    # Simulate the resolved click by setting the selection reactiveVal directly
    # (event_data() needs a live browser; the resolver itself is unit-tested).
    selection(list(origin = "click", peptide_seq = "PEPA", accession = "ACC1",
                   label = "G1_aa10", row = 1L))

    # 3C line data computes for the pinned protein (ACC1 -> marker -> both panels).
    ld <- pinned_line_data()
    expect_true(is.data.frame(ld) && nrow(ld) > 0L)
    expect_true(all(c("accession", "peptide_seq", "condition", "mean_log2",
                      "panel", "aa_label") %in% colnames(ld)))
    expect_true(all(ld$accession == "ACC1"))
    expect_setequal(as.character(unique(ld$condition)), c("A", "B"))

    # The metadata table renders (the intensity plot's validate-gated render is
    # exercised via pinned_line_data() above  -  accessing the output directly
    # would raise the no-pin validate when line data is transiently empty).
    expect_false(is.null(output$pelsa_pin_metadata))
  })
})

test_that("PERF: a pin does NOT rebuild the main volcano (build_plot not re-called)", {
  # The highlight is a client-side plotlyProxy restyle, so output$pelsa_volcano_plot
  # must NOT depend on selection() - selecting must not re-invoke the heavy
  # pelsa_volcano_build_plot (the ~1.1-1.5s / ~15MB cost). We trace build_plot
  # and assert its call count for the MAIN volcano source does not increase when
  # only selection() changes.
  build_calls <- new.env(parent = emptyenv())
  build_calls$n_main <- 0L
  trace(
    "pelsa_volcano_build_plot",
    tracer = quote({
      if (identical(source_id, "Proteome-pelsa_volcano")) {
        # bump a counter in the test env via the global option set below.
        e <- getOption(".pelsa_build_counter_env"); e$n_main <- e$n_main + 1L
      }
    }),
    print = FALSE, where = asNamespace("Protigy")
  )
  options(.pelsa_build_counter_env = build_calls)
  on.exit({ untrace("pelsa_volcano_build_plot", where = asNamespace("Protigy"))
            options(.pelsa_build_counter_env = NULL) }, add = TRUE)

  shiny::testServer(PELSASection3_Ome_Server, args = .full_args(), {
    session$setInputs(pelsa_color_mode = "significance",
                      pelsa_label_mode = "all_markers",
                      pelsa_volcano_contrast = "A_over_B")
    force(active_volcano_df())
    # Render the main volcano once (registers the reactive).
    force(output$pelsa_volcano_plot)
    n_before <- getOption(".pelsa_build_counter_env")$n_main
    expect_gte(n_before, 1L)  # built at least once

    # Select a peptide. The highlight is a proxy restyle; the render must NOT re-run.
    selection(list(origin = "click", peptide_seq = "PEPA", accession = "ACC1",
                   label = "G1_aa10", row = 1L))
    force(output$pelsa_volcano_plot)
    n_after <- getOption(".pelsa_build_counter_env")$n_main
    expect_equal(n_after, n_before)  # NO rebuild on select

    # Clear -> still no rebuild of the main volcano.
    selection(NULL)
    force(output$pelsa_volcano_plot)
    expect_equal(getOption(".pelsa_build_counter_env")$n_main, n_before)
  })
})

test_that("7E: switching contrast CLEARS a stale selection", {
  shiny::testServer(PELSASection3_Ome_Server, args = .full_args(), {
    session$setInputs(pelsa_color_mode = "significance",
                      pelsa_label_mode = "all_markers",
                      pelsa_volcano_contrast = "A_over_B")
    force(active_volcano_df())
    selection(list(origin = "click", peptide_seq = "PEPA", accession = "ACC1",
                   label = "G1_aa10", row = 1L))
    expect_false(is.null(selection()))

    # Switch to the other contrast -> the selection (made under A_over_B coords) clears.
    session$setInputs(pelsa_volcano_contrast = "A_over_C")
    expect_equal(active_contrast(), "A_over_C")
    expect_null(selection())
    # The intensity line data is gated on a selection, so it no longer computes.
    expect_error(pinned_line_data(), class = "shiny.silent.error")
  })
})

test_that("7F: exports list has volcano/intensity/woods fns; volcano writes figures", {
  shiny::testServer(PELSASection3_Ome_Server, args = .full_args(), {
    session$setInputs(pelsa_color_mode = "significance",
                      pelsa_label_mode = "all_markers",
                      pelsa_volcano_contrast = "A_over_B",
                      pelsa_show_best_panel = FALSE)
    force(active_volcano_df())

    exports <- session$returned
    expect_setequal(names(exports), c("volcano", "intensity", "woods"))
    expect_true(all(vapply(exports, is.function, logical(1))))

    dir <- tempfile("pelsa_export_"); dir.create(dir)
    for (fn in exports) fn(dir)

    # Volcano figures land in 03_volcano/01_volcano (one per contrast, PNG only;
    # PDF export was intentionally gated off in commit 71e2496), named
    # all_peptide_volcano_<contrast>.
    vdir <- file.path(dir, "03_volcano", "01_volcano")
    expect_true(dir.exists(vdir))
    vfiles <- list.files(vdir)
    expect_true(any(grepl("^all_peptide_volcano_.*\\.png$", vfiles)))
    expect_false(any(grepl("^all_peptide_volcano_.*\\.pdf$", vfiles)))
  })
})

# M5: adding a marker must clear the cached volcano df so the live view rebuilds
# and re-flags the new accession (was: cached df kept the old markers).
test_that("M5: changing markers clears the volcano cache so the active view rebuilds", {
  ss <- shiny::reactiveVal(.mk_setup_state_full())  # marker_rows = ACC1
  args <- .full_args()
  args$pelsa_setup_state <- ss
  shiny::testServer(PELSASection3_Ome_Server, args = args, {
    session$setInputs(pelsa_color_mode = "significance",
                      pelsa_label_mode = "all_markers",
                      pelsa_volcano_contrast = "A_over_B",
                      pelsa_show_best_panel = FALSE)
    df1 <- active_volcano_df()
    expect_equal(names(volcano_df_cache()), "A_over_B")
    expect_setequal(marker_accessions(), "ACC1")

    # Add ACC2 to the marker list (mimics the add-to-marker action upstream).
    # Replace marker_rows wholesale: modifyList() would recurse into the
    # data.frame (a list) and attempt a column-wise merge onto the 1-row frame.
    st_two_markers <- .mk_setup_state_full()
    st_two_markers$marker_rows <- list(Proteome = data.frame(
      accession = c("ACC1", "ACC2"), gene = c("G1", "G2"),
      stringsAsFactors = FALSE))
    ss(st_two_markers)
    session$flushReact()
    expect_setequal(marker_accessions(), c("ACC1", "ACC2"))

    # M5 fix: the marker change fires the cache-clearing observer, dropping the
    # stale (ACC1-only) df; the next read rebuilds with the current markers. We
    # assert the OBSERVABLE contract -- the live df now flags STRICTLY more
    # peptides as markers (PEPA/PEPC for ACC1, plus PEPB for ACC2: 2 -> 3) --
    # rather than the cache's transient empty state, which an output re-render
    # repopulates within the same flush. Were the stale cache kept, the rebuilt
    # df would still flag only ACC1's peptides and this would fail.
    df2 <- active_volcano_df()
    expect_equal(names(volcano_df_cache()), "A_over_B")
    expect_gt(sum(df2$is_marker, na.rm = TRUE), sum(df1$is_marker, na.rm = TRUE))
  })
})

# M5 (best-panel half): the same marker change must also clear the BEST-peptide
# cache. best_volcano_df() bakes isolate(marker_accessions()) at build time and
# caches per contrast; without clearing best_volcano_df_cache the best panel keeps
# stale is_marker flags until a contrast / color-mode switch frees it.
test_that("M5: changing markers clears the best-peptide cache so the best panel rebuilds", {
  ss <- shiny::reactiveVal(.mk_setup_state_full())  # marker_rows = ACC1
  args <- .full_args()
  args$pelsa_setup_state <- ss
  shiny::testServer(PELSASection3_Ome_Server, args = args, {
    session$setInputs(pelsa_color_mode = "significance",
                      pelsa_label_mode = "all_markers",
                      pelsa_volcano_contrast = "A_over_B",
                      pelsa_show_best_panel = TRUE)  # best panel ON
    bdf1 <- best_volcano_df()
    expect_equal(names(best_volcano_df_cache()), "A_over_B")
    expect_setequal(marker_accessions(), "ACC1")

    # Add ACC2 to the marker list while the best panel is shown.
    st_two_markers <- .mk_setup_state_full()
    st_two_markers$marker_rows <- list(Proteome = data.frame(
      accession = c("ACC1", "ACC2"), gene = c("G1", "G2"),
      stringsAsFactors = FALSE))
    ss(st_two_markers)
    session$flushReact()
    expect_setequal(marker_accessions(), c("ACC1", "ACC2"))

    # Observable contract: the rebuilt best df flags STRICTLY more peptides as
    # markers. Were the stale best cache kept, the count would not change.
    bdf2 <- best_volcano_df()
    expect_equal(names(best_volcano_df_cache()), "A_over_B")
    expect_gt(sum(bdf2$is_marker, na.rm = TRUE), sum(bdf1$is_marker, na.rm = TRUE))
  })
})

# ---------------------------------------------------------------------------
# Shared-cutoff wiring (source-level regression guard)
#
# The pinned intensity + Woods panels and the intensity/Woods exports must
# read the user-set cutoff via sig_cutoff_r() (Statistics > Summary), NOT a
# hardcoded 0.05. These are reactive observers/handlers not reachable by the
# pure-helper tests above, so we pin the wiring at the source level: reverting
# any of these call sites back to a literal 0.05 fails this test.
# ---------------------------------------------------------------------------
test_that("pinned panels + exports thread sig_cutoff_r(), never a hardcoded 0.05", {
  src_path <- testthat::test_path("..", "..", "R", "tab_pelsa_section3.R")
  skip_if_not(file.exists(src_path), "tab_pelsa_section3.R source not found")
  src <- readLines(src_path, warn = FALSE)

  # No literal `sig_cutoff = 0.05` anywhere in the module (the bug pattern).
  expect_false(any(grepl("sig_cutoff\\s*=\\s*0\\.05", src)),
               info = "tab_pelsa_section3.R must not hardcode sig_cutoff = 0.05")

  # The pinned intensity reactive passes the reactive cutoff.
  expect_true(any(grepl("sig_cutoff\\s*=\\s*sig_cutoff_r\\(\\)", src)),
              info = "pinned panels must pass sig_cutoff = sig_cutoff_r()")

  # The intensity/Woods exports must not fall back to the export constant for
  # the on-screen-mirroring significance split (volcano export already used the
  # user cutoff; these two now do too). The constant may still appear elsewhere,
  # but not as the sig_cutoff argument to the data builders in the export path.
  expect_false(any(grepl("\\.PELSA_ANY_CONTRAST,\\s*\\.PELSA_EXPORT_SIG_CUTOFF", src)),
               info = "intensity/Woods exports must use the user cutoff, not the export constant")
})

# ---------------------------------------------------------------------------
# .pelsa_woods_click_index: resolve which Woods peptide a click selected.
# Pure helper extracted from the plotly_click observer so the candidate-
# selection arithmetic is unit-testable (the observer is otherwise reactive).
# ---------------------------------------------------------------------------
test_that(".pelsa_woods_click_index picks the in-span peptide nearest the click y", {
  fn <- get(".pelsa_woods_click_index", envir = asNamespace("Protigy"))
  pep <- data.frame(
    peptide_seq = c("pA", "pB", "pC"),
    pep_start   = c(1L, 10L, 100L),
    pep_end     = c(9L, 20L, 120L),
    logFC       = c(-2, 1, 3),
    stringsAsFactors = FALSE
  )
  # x lands in pB's span; y nearest pB's logFC -> index 2.
  expect_equal(fn(pep, ev_x = 15, ev_y = 1.1), 2L)
})

test_that(".pelsa_woods_click_index handles an NA click y without error (regression)", {
  # Regression: `ev$y %||% pep$logFC[cand]` only guarded NULL. With ev_y = NA,
  # abs(logFC - NA) was all-NA, which.min returned integer(0), and pep[[integer(0)]]
  # errored. NA y must be treated like NULL (fall back to the candidate's own
  # logFC), yielding a valid index, not an error.
  fn <- get(".pelsa_woods_click_index", envir = asNamespace("Protigy"))
  pep <- data.frame(
    peptide_seq = c("pA", "pB"),
    pep_start   = c(1L, 10L),
    pep_end     = c(9L, 20L),
    logFC       = c(-2, 1),
    stringsAsFactors = FALSE
  )
  idx <- expect_no_error(fn(pep, ev_x = 15, ev_y = NA_real_))
  expect_equal(idx, 2L)            # x in pB's span -> pB
})

test_that(".pelsa_woods_click_index falls back to all peptides when x is in no span", {
  fn <- get(".pelsa_woods_click_index", envir = asNamespace("Protigy"))
  pep <- data.frame(
    peptide_seq = c("pA", "pB"),
    pep_start   = c(1L, 10L),
    pep_end     = c(9L, 20L),
    logFC       = c(-2, 1),
    stringsAsFactors = FALSE
  )
  # x=500 in no span; y nearest pA's -2 -> index 1.
  expect_equal(fn(pep, ev_x = 500, ev_y = -1.9), 1L)
  # NULL x also falls back to all peptides.
  expect_equal(fn(pep, ev_x = NULL, ev_y = 0.9), 2L)
})

test_that(".pelsa_woods_click_index returns NULL (not integer(0)) when all candidate logFC are NA", {
  # Honors the documented length-1-or-NULL contract: all-NA logFC would make
  # which.min collapse to integer(0); the caller's is.null(j) guard must catch it.
  fn <- get(".pelsa_woods_click_index", envir = asNamespace("Protigy"))
  pep <- data.frame(
    peptide_seq = c("pA", "pB"),
    pep_start   = c(1L, 10L),
    pep_end     = c(9L, 20L),
    logFC       = c(NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )
  expect_null(fn(pep, ev_x = 15, ev_y = 1.0))
})

# ---------------------------------------------------------------------------
# Source-level guard: the volcano highlight comments must describe the PRODUCTION
# addTraces/deleteTraces gold overlay, NOT the abandoned proxy-restyle path or a
# figure REBUILD. (The pelsa_volcano_recolor helper is kept for unit tests only.)
# ---------------------------------------------------------------------------
test_that("section3 highlight comments describe the gold overlay, not proxy-restyle/rebuild", {
  s3_path  <- testthat::test_path("..", "..", "R", "tab_pelsa_section3.R")
  s3h_path <- testthat::test_path("..", "..", "R", "tab_pelsa_volcano_helpers.R")
  skip_if_not(file.exists(s3_path) && file.exists(s3h_path),
              "tab_pelsa_section3 source not found")
  s3  <- paste(readLines(s3_path, warn = FALSE), collapse = "\n")
  s3h <- paste(readLines(s3h_path, warn = FALSE), collapse = "\n")
  # The stale narratives are gone.
  expect_false(grepl("ONE interactive highlight mechanism: the", s3h, fixed = TRUE))
  expect_false(grepl("REBUILDING the figure with the gold", s3, fixed = TRUE))
  # The production mechanism is documented.
  expect_true(grepl("addTraces", s3, fixed = TRUE))
  expect_true(grepl("GOLD OVERLAY", s3, fixed = TRUE) ||
              grepl("gold overlay", s3, fixed = TRUE))
})

################################################################################
# --- from test-pelsa-recolor.R  (volcano recolor / highlight / gold overlay) ---
################################################################################

# Minimal volcano-df-shaped frame: 2 proteins, one a marker. Columns the recolor
# reads: id, winning_accession, is_marker, sig_color, feature_color.
.mk_df <- function() {
  data.frame(
    id                = c("PEPA1", "PEPA2", "PEPB1", "PEPMK"),
    winning_accession = c("ACCA", "ACCA", "ACCB", "ACCMK"),
    is_marker         = c(FALSE, FALSE, FALSE, TRUE),
    sig_color         = c("#1f4e9c", "darkred", "gray70", "gray70"),
    feature_color     = c("#111111", "#222222", "#333333", "#444444"),
    stringsAsFactors  = FALSE
  )
}

test_that("highlight_mask: selected peptide + same-protein + find, uniform", {
  df <- .mk_df()
  # selection on ACCA peptide PEPA1 -> PEPA1 + sibling PEPA2 highlighted.
  sel <- list(accession = "ACCA", peptide_seq = "PEPA1")
  m <- pelsa_volcano_highlight_mask(df, selection = sel, find_mask = NULL)
  expect_equal(which(m), c(1L, 2L))
  # NULL selection + NULL find -> nothing.
  expect_false(any(pelsa_volcano_highlight_mask(df, NULL, NULL)))
  # find mask alone (ACCB) -> row 3.
  fm <- df$winning_accession == "ACCB"
  expect_equal(which(pelsa_volcano_highlight_mask(df, NULL, fm)), 3L)
  # selection with NA peptide_seq -> all same-accession rows (accession only).
  sel2 <- list(accession = "ACCA", peptide_seq = NA_character_)
  expect_equal(which(pelsa_volcano_highlight_mask(df, sel2, NULL)), c(1L, 2L))
})

test_that("recolor: NULL selection + no find -> base fills, no rings", {
  df <- .mk_df()
  out <- pelsa_volcano_recolor(df, selection = NULL, find_mask = NULL,
                               color_mode = "significance")
  split <- pelsa_volcano_marker_split(df)
  expect_length(out$background$color, nrow(split$background))
  expect_length(out$markers$color,   nrow(split$markers))
  expect_setequal(out$background$color, c("#1f4e9c", "darkred", "gray70"))
  expect_true(all(out$background$line.width == 0))
  expect_true(all(out$markers$line.width == 0))
})

test_that("recolor: click selection -> gold fill + dark ring on the clicked peptide", {
  df <- .mk_df()
  sel <- list(origin = "click", accession = "ACCA", peptide_seq = "PEPA1")
  out <- pelsa_volcano_recolor(df, sel, NULL, "significance")
  split <- pelsa_volcano_marker_split(df)
  bg_id <- as.character(split$background$id)
  expect_equal(out$background$color[bg_id == "PEPA1"], .PELSA_GOLD)
  expect_equal(out$background$line.color[bg_id == "PEPA1"], .PELSA_SEL_DARK_RING)
  expect_equal(out$background$color[bg_id == "PEPA2"], "darkred")
  expect_equal(out$background$line.color[bg_id == "PEPA2"], .PELSA_GOLD)
  expect_gt(out$background$line.width[bg_id == "PEPA2"], 0)
  expect_equal(out$background$color[bg_id == "PEPB1"], "gray70")
  expect_equal(out$background$line.width[bg_id == "PEPB1"], 0)
})

test_that("recolor: a clicked MARKER goes gold in the marker trace (gold wins)", {
  df <- .mk_df()
  sel <- list(origin = "click", accession = "ACCMK", peptide_seq = "PEPMK")
  out <- pelsa_volcano_recolor(df, sel, NULL, "significance")
  expect_equal(out$markers$color, .PELSA_GOLD)
})

test_that("recolor: multi-find mask -> uniform gold fill, no dark ring", {
  df <- .mk_df()
  mask <- df$winning_accession == "ACCA"
  out <- pelsa_volcano_recolor(df, selection = NULL, find_mask = mask,
                               color_mode = "significance")
  split <- pelsa_volcano_marker_split(df)
  bg_id <- as.character(split$background$id)
  expect_equal(out$background$color[bg_id %in% c("PEPA1", "PEPA2")],
               c(.PELSA_GOLD, .PELSA_GOLD))
  expect_true(all(out$background$line.color[bg_id %in% c("PEPA1","PEPA2")]
                  != .PELSA_SEL_DARK_RING))
})

test_that("recolor: NA peptide_seq -> all same-accession rows get gold ring, none gold fill", {
  df <- .mk_df()
  sel <- list(origin = "click", accession = "ACCA", peptide_seq = NA_character_)
  out <- pelsa_volcano_recolor(df, sel, NULL, "significance")
  split <- pelsa_volcano_marker_split(df)
  bg_id <- as.character(split$background$id)
  expect_equal(out$background$line.color[bg_id %in% c("PEPA1","PEPA2")],
               c(.PELSA_GOLD, .PELSA_GOLD))
  expect_false(.PELSA_GOLD %in% out$background$color)  # no gold FILL when no peptide id
})

test_that("recolor: feature color mode uses feature_color as the base", {
  df <- .mk_df()
  out <- pelsa_volcano_recolor(df, NULL, NULL, "feature")
  expect_true("#111111" %in% out$background$color)
})

test_that("trace_index: finds the meta-stamped bg/marker traces", {
  # build_plot needs a full volcano-df shape; extend the minimal frame.
  df <- data.frame(
    id = c("PEPA1","PEPA2","PEPB1","PEPMK"),
    winning_accession = c("ACCA","ACCA","ACCB","ACCMK"),
    PG.ProteinAccessions = c("ACCA","ACCA","ACCB","ACCMK"),
    winning_gene = c("GA","GA","GB","GM"), PG.Genes = c("GA","GA","GB","GM"),
    is_marker = c(FALSE,FALSE,FALSE,TRUE),
    sig_color = c("#1f4e9c","darkred","gray70","gray70"),
    feature_color = c("#111","#222","#333","#444"),
    logFC = c(-1,1,2,0.5), logP = c(1,2,3,1.5),
    adj.P.Val = c(0.2,0.01,0.001,0.3), P.Value = c(0.1,0.005,0.0005,0.2),
    Significant = c(FALSE,TRUE,TRUE,FALSE), feature_class_primary = "none",
    pep_start = c(1L,5L,2L,9L), pep_end = c(4L,9L,8L,15L),
    label = c("GA_aa1","GA_aa5","GB_aa2","GM_aa9"),
    stringsAsFactors = FALSE, check.names = FALSE)
  p <- pelsa_volcano_build_plot(df, full_df = df, color_mode = "significance",
                                label_mode = "none", source_id = "s")
  # Resolve on the RAW build object AND on plotly_build(p) - the production path
  # (apply_highlight) wraps in plotly_build, so the meta tag must survive it.
  idx <- .pelsa_volcano_trace_index(p)
  expect_equal(idx$background, 0L)
  expect_equal(idx$markers, 1L)
  idx_built <- .pelsa_volcano_trace_index(plotly::plotly_build(p))
  expect_equal(idx_built$background, 0L)
  expect_equal(idx_built$markers, 1L)
})

test_that("recolor find_mask: duplicate ids across protein groups stay row-aligned", {
  # Two rows share the stripped sequence "DUP" but different winning_accession.
  # A find on ACCA must gold ONLY the ACCA row, not the ACCB row that shares id.
  df <- data.frame(
    id                = c("DUP", "DUP", "PEPB1"),
    winning_accession = c("ACCA", "ACCB", "ACCB"),
    is_marker         = c(FALSE, FALSE, FALSE),
    sig_color         = c("gray70", "gray70", "gray70"),
    feature_color     = c("#111", "#222", "#333"),
    stringsAsFactors  = FALSE)
  mask <- df$winning_accession == "ACCA"          # only row 1
  out <- pelsa_volcano_recolor(df, selection = NULL, find_mask = mask,
                               color_mode = "significance")
  split <- pelsa_volcano_marker_split(df)
  # background row order == df row order here (no markers): row1 gold, row2 not.
  expect_equal(out$background$color[1], .PELSA_GOLD)
  expect_equal(out$background$color[2], "gray70")
})

# ---- gold OVERLAY trace (Stage B: proxy addTraces highlight) ----------------

# Full volcano-df shape the gold trace + hover read (logFC/logP/pep_*/gene/acc).
.mk_full_df <- function() {
  data.frame(
    id                   = c("PEPA1", "PEPA2", "PEPB1", "PEPMK"),
    winning_accession    = c("ACCA", "ACCA", "ACCB", "ACCMK"),
    PG.ProteinAccessions = c("ACCA", "ACCA", "ACCB", "ACCMK"),
    winning_gene         = c("GA", "GA", "GB", "GM"),
    PG.Genes             = c("GA", "GA", "GB", "GM"),
    is_marker            = c(FALSE, FALSE, FALSE, TRUE),
    sig_color            = c("#1f4e9c", "darkred", "gray70", "gray70"),
    feature_color        = c("#111", "#222", "#333", "#444"),
    logFC                = c(-1, 1, 2, 0.5),
    logP                 = c(1, 2, 3, 1.5),
    adj.P.Val            = c(0.2, 0.01, 0.001, 0.3),
    P.Value              = c(0.1, 0.005, 0.0005, 0.2),
    pep_start            = c(1L, 5L, 2L, 9L),
    pep_end              = c(4L, 9L, 8L, 15L),
    label                = c("GA_aa1", "GA_aa5", "GB_aa2", "GM_aa9"),
    stringsAsFactors     = FALSE, check.names = FALSE)
}

test_that("gold_trace: NULL when nothing is highlighted", {
  df <- .mk_full_df()
  expect_null(pelsa_volcano_gold_trace(df, selection = NULL, find_mask = NULL))
  expect_null(pelsa_volcano_gold_trace(df[0, , drop = FALSE],
                                       selection = list(accession = "ACCA")))
  expect_null(pelsa_volcano_gold_trace("not a df"))
})

test_that("gold_trace: selection -> gold scattergl trace over the right points", {
  df <- .mk_full_df()
  sel <- list(accession = "ACCA", peptide_seq = "PEPA1")  # PEPA1 + sibling PEPA2
  tr <- pelsa_volcano_gold_trace(df, selection = sel, find_mask = NULL)
  expect_false(is.null(tr))
  expect_equal(tr$type, "scattergl")
  expect_equal(tr$mode, "markers")
  expect_identical(tr$meta, "pelsa_gold")
  expect_identical(tr$marker$color, .PELSA_GOLD)
  expect_identical(tr$marker$line$color, .PELSA_VOLCANO_MARKER_EDGE)
  # Two highlighted points (PEPA1 + PEPA2), at their (logFC, logP). x/y/text are
  # as.list()-wrapped so even a single point serializes to a JSON array (the
  # proxy auto_unbox scalar-collapse bug); unlist before the value checks.
  expect_equal(unlist(tr$x), c(-1, 1))
  expect_equal(unlist(tr$y), c(1, 2))
  # 6-line hover, one per highlighted point.
  expect_length(tr$text, 2L)
  txt <- unlist(tr$text)
  expect_true(all(grepl("Peptide: ", txt, fixed = TRUE)))
  expect_equal(lengths(regmatches(txt, gregexpr("<br>", txt))),
               c(5L, 5L))  # 6 lines => 5 <br> separators
})

test_that("gold_trace: find_mask alone highlights the matched rows", {
  df <- .mk_full_df()
  fm <- df$winning_accession == "ACCB"   # row 3 only
  tr <- pelsa_volcano_gold_trace(df, selection = NULL, find_mask = fm)
  expect_false(is.null(tr))
  # Single matched row: as.list keeps x/y as a length-1 list (-> JSON array).
  expect_equal(unlist(tr$x), 2)     # PEPB1 logFC
  expect_equal(unlist(tr$y), 3)     # PEPB1 logP
  expect_length(tr$text, 1L)
  # Regression guard: a length-1 coord must serialize as an ARRAY, not a scalar.
  expect_equal(as.character(jsonlite::toJSON(tr$x, auto_unbox = TRUE)), "[2]")
})

test_that("gold_trace size matches the build's gold/marker px (7)", {
  df <- .mk_full_df()
  tr <- pelsa_volcano_gold_trace(df, selection = list(accession = "ACCA"))
  expect_equal(tr$marker$size, 7)
})

test_that("volcano_tip: empty in -> empty out; 6 lines per row otherwise", {
  df <- .mk_full_df()
  expect_length(pelsa_volcano_tip(df[0, , drop = FALSE]), 0L)
  tips <- pelsa_volcano_tip(df)
  expect_length(tips, nrow(df))
  expect_true(grepl("Accession: ACCA", tips[1], fixed = TRUE))
  expect_true(grepl("logFC: -1.00", tips[1], fixed = TRUE))
})

################################################################################
# --- from test-pelsa-volcano-clicked-point.R  (volcano clicked-point trace) ---
################################################################################

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

################################################################################
# --- from test-pelsa-find-metadata.R  (volcano find-mask + pin metadata rows) ---
################################################################################

# Build a small volcano-df-shaped frame directly (find_mask/metadata read only
# id / winning_accession / PG.ProteinAccessions / winning_gene / PG.Genes /
# pep_start / pep_end / logFC / adj.P.Val).
.find_df <- function() {
  data.frame(
    id                   = c("PEP1", "PEP2", "PEP3", "ISOPEPTIDEK"),
    winning_accession    = c("P12345", "P12345", "Q99999", "P12345-2"),
    PG.ProteinAccessions = c("P12345", "P12345;EXTRA", "Q99999", "P12345-2"),
    winning_gene         = c("GA", "GA", "GB", ""),
    PG.Genes             = c("GA", "GA", "GB", ""),
    pep_start            = c(7L, 40L, 5L, 7L),
    pep_end              = c(17L, 50L, 15L, 17L),
    logFC                = c(1.1, -0.5, 2.0, 0.3),
    adj.P.Val            = c(0.01, 0.20, 0.001, 0.50),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
}

test_that("find_mask: exact winning_accession match (single accession)", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "Q99999")
  expect_equal(which(out$mask), 3L)
  expect_equal(out$accessions, "Q99999")
  expect_equal(out$count, 1L)
})

test_that("find_mask: case-insensitive + trims", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "  q99999 ")
  expect_equal(out$count, 1L)
})

test_that("find_mask: isoform base P12345 also matches P12345-2", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "P12345")
  expect_setequal(which(out$mask), c(1L, 2L, 4L))
  expect_equal(out$count, 3L)
})

test_that("find_mask: PG.ProteinAccessions token match (EXTRA)", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "EXTRA")
  expect_equal(which(out$mask), 2L)
})

test_that("find_mask: no match -> empty mask, count 0", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "NOPE")
  expect_equal(out$count, 0L)
  expect_false(any(out$mask))
})

test_that("find_mask: empty/NA input -> count 0", {
  df <- .find_df()
  expect_equal(pelsa_volcano_find_mask(df, "")$count, 0L)
  expect_equal(pelsa_volcano_find_mask(df, NA)$count, 0L)
})

test_that("metadata_rows: 2-col (label,value) df with the panel fields", {
  df <- .find_df()
  rows <- pelsa_pin_metadata_rows(df, row = 1L, n_peptides = 2L)
  expect_s3_class(rows, "data.frame")
  expect_named(rows, c("label", "value"))
  lv <- setNames(rows$value, rows$label)
  expect_equal(lv[["Peptide"]], "GA_aa7")
  expect_equal(lv[["Accession"]], "P12345")
  expect_equal(lv[["Gene"]], "GA")
  expect_equal(lv[["Quantified peptides (this contrast)"]], "2")
  expect_equal(lv[["Sequence"]], "PEP1")
  expect_equal(lv[["Position"]], "7-17")
  expect_match(lv[["adj.P"]], "0.01")
  expect_match(lv[["logFC"]], "1.1")
  # Sequence coverage row sits between Accession and Gene; NA by default.
  expect_equal(lv[["Sequence coverage"]], "NA")
  expect_equal(which(rows$label == "Sequence coverage"), 3L)
  expect_equal(which(rows$label == "Gene"), 4L)
})

test_that("metadata_rows: coverage_frac renders as a 1-decimal percent", {
  df <- .find_df()
  rows <- pelsa_pin_metadata_rows(df, row = 1L, n_peptides = 2L,
                                  coverage_frac = 0.4237)
  lv <- setNames(rows$value, rows$label)
  expect_equal(lv[["Sequence coverage"]], "42.4%")
  # NA / malformed coverage falls back to "NA".
  expect_equal(setNames(
    pelsa_pin_metadata_rows(df, 1L, 2L, coverage_frac = NA_real_)$value,
    pelsa_pin_metadata_rows(df, 1L, 2L)$label)[["Sequence coverage"]], "NA")
})

test_that("metadata_rows: empty gene -> accession fallback label, Gene = NA", {
  df <- .find_df()
  rows <- pelsa_pin_metadata_rows(df, row = 4L, n_peptides = 3L)
  lv <- setNames(rows$value, rows$label)
  expect_equal(lv[["Peptide"]], "P12345-2_aa7")  # gene blank -> accession
  expect_equal(lv[["Gene"]], "NA")
})

################################################################################
# --- from test-pelsa-woods.R  (Woods panel helpers + static export) ---
################################################################################

################################################################################
# Tests for the PELSA Volcano coverage + UniProt-feature + Woods panel helpers
# (tab_pelsa_panel_helpers.R). Pure helpers - no Shiny, no network.
################################################################################


# ---- pelsa_woods_peptide_data ------------------------------------------------

.woods_matched <- function() data.frame(
  PEP.StrippedSequence = c("PEPA", "PEPB", "PEPC", "OTHER"),
  accession = c("A", "A", "A", "B"),
  pep_start = c(10L, 50L, 45L, 1L),
  pep_end   = c(20L, 60L, 55L, 5L),
  pep_occurrence_idx = 1L, stringsAsFactors = FALSE)

.woods_stat <- function() data.frame(
  PEP.StrippedSequence = c("PEPA", "PEPB", "PEPC"),
  logFC.AvB = c(-2.1, 0.3, 1.8),
  adj.P.Val.AvB = c(0.001, 0.40, 0.02),
  stringsAsFactors = FALSE)

test_that("woods_peptide_data joins spans to contrast stats, flags sig, sorts", {
  out <- pelsa_woods_peptide_data("A", .woods_matched(), .woods_stat(),
                                  "AvB", sig_cutoff = 0.05)
  expect_equal(nrow(out), 3L)                         # only protein A
  expect_identical(out$peptide_seq, c("PEPA", "PEPC", "PEPB"))  # sorted by start
  expect_equal(out$pep_start, c(10L, 45L, 50L))
  expect_equal(out$logFC[out$peptide_seq == "PEPA"], -2.1)
  expect_equal(out$sig, c(TRUE, TRUE, FALSE))         # 0.001,0.02 sig; 0.40 not
})

test_that("woods_peptide_data: sig flag is strict < cutoff at the boundary", {
  m <- data.frame(PEP.StrippedSequence = "P", accession = "A",
                  pep_start = 1L, pep_end = 5L, pep_occurrence_idx = 1L,
                  stringsAsFactors = FALSE)
  s <- data.frame(PEP.StrippedSequence = "P", logFC.AvB = 1,
                  adj.P.Val.AvB = 0.05, stringsAsFactors = FALSE)  # == cutoff
  expect_false(pelsa_woods_peptide_data("A", m, s, "AvB", 0.05)$sig)
})

test_that("woods_peptide_data drops NA-span peptides + empty when no match", {
  m <- data.frame(PEP.StrippedSequence = c("P", "Q"), accession = "A",
                  pep_start = c(NA_integer_, 5L), pep_end = c(10L, 9L),
                  pep_occurrence_idx = 1L, stringsAsFactors = FALSE)
  s <- data.frame(PEP.StrippedSequence = c("P", "Q"), logFC.AvB = c(1, 2),
                  adj.P.Val.AvB = c(0.01, 0.02), stringsAsFactors = FALSE)
  out <- pelsa_woods_peptide_data("A", m, s, "AvB")
  expect_equal(nrow(out), 1L)                         # NA-span P dropped
  expect_identical(out$peptide_seq, "Q")
  # missing contrast -> empty
  expect_equal(nrow(pelsa_woods_peptide_data("A", m, s, "NOPE")), 0L)
  # no accession match -> empty
  expect_equal(nrow(pelsa_woods_peptide_data("Z", m, s, "AvB")), 0L)
})

# ---- pelsa_woods_build_index + .index parity --------------------------------

test_that("pelsa_woods_build_index groups matched rows by non-blank accession", {
  idx <- pelsa_woods_build_index(.woods_matched(), .woods_stat())
  expect_setequal(names(idx$by_acc), c("A", "B"))
  expect_equal(nrow(idx$by_acc[["A"]]), 3L)
  expect_equal(nrow(idx$by_acc[["B"]]), 1L)
  expect_identical(idx$stat_key, c("PEPA", "PEPB", "PEPC"))
})

test_that("pelsa_woods_build_index drops NA / blank accession rows", {
  m <- data.frame(
    PEP.StrippedSequence = c("PEPA", "PEPX", "PEPY"),
    accession = c("A", NA_character_, ""),
    pep_start = c(10L, 1L, 2L), pep_end = c(20L, 5L, 6L),
    pep_occurrence_idx = 1L, stringsAsFactors = FALSE)
  idx <- pelsa_woods_build_index(m, .woods_stat())
  expect_setequal(names(idx$by_acc), "A")   # NA + "" groups dropped
})

test_that("pelsa_woods_peptide_data: .index path is identical to the scan path", {
  m <- .woods_matched(); s <- .woods_stat()
  idx <- pelsa_woods_build_index(m, s)
  plain   <- pelsa_woods_peptide_data("A", m, s, "AvB", sig_cutoff = 0.05)
  indexed <- pelsa_woods_peptide_data("A", m, s, "AvB", sig_cutoff = 0.05,
                                      .index = idx)
  expect_identical(plain, indexed)
})

test_that("pelsa_woods_peptide_data: .index absent-accession returns 0-row (no error)", {
  m <- .woods_matched(); s <- .woods_stat()
  idx <- pelsa_woods_build_index(m, s)
  out <- pelsa_woods_peptide_data("Z", m, s, "AvB", .index = idx)
  expect_equal(nrow(out), 0L)
})

# ---- pelsa_coverage_intervals (IRanges union) --------------------------------

test_that("coverage_intervals merges overlapping + adjacent, sorts, drops bad", {
  # 10-20, 45-55, 50-60 -> 10-20, 45-60
  iv <- pelsa_coverage_intervals(c(10L, 45L, 50L), c(20L, 55L, 60L))
  expect_equal(iv$start, c(10L, 45L))
  expect_equal(iv$end, c(20L, 60L))
  # adjacency: 1-5 and 6-10 are adjacent -> merged into 1-10 (IRanges reduce)
  adj <- pelsa_coverage_intervals(c(1L, 6L), c(5L, 10L))
  expect_equal(adj, data.frame(start = 1L, end = 10L))
  # single residue
  expect_equal(pelsa_coverage_intervals(7L, 7L),
               data.frame(start = 7L, end = 7L))
  # empty + NA + inverted dropped
  expect_equal(nrow(pelsa_coverage_intervals(integer(0), integer(0))), 0L)
  expect_equal(nrow(pelsa_coverage_intervals(c(NA, 9L), c(5L, 3L))), 0L)
})

# ---- pelsa_widen_point_features (single-AA region widening) -----------------

test_that("widen_point_features: single-AA feature widens by +-3, multi-AA untouched", {
  f <- data.frame(start = c(214L, 10L), end = c(214L, 30L),
                  feature_class = c("active_or_binding_site", "catalytic_domain"),
                  stringsAsFactors = FALSE)
  out <- pelsa_widen_point_features(f, prot_len = 500L)
  expect_equal(out$display_start, c(211L, 10L))
  expect_equal(out$display_end,   c(217L, 30L))
  expect_equal(out$was_widened,   c(TRUE, FALSE))
  # true coords untouched
  expect_equal(out$start, c(214L, 10L))
  expect_equal(out$end,   c(214L, 30L))
})

test_that("widen_point_features: clamps at the N-terminus and C-terminus", {
  f <- data.frame(start = c(2L, 499L), end = c(2L, 499L))
  out <- pelsa_widen_point_features(f, prot_len = 500L)
  # position 2 widens to -1..5, clamped to 1..5 (NOT shifted to keep width 7)
  expect_equal(out$display_start, c(1L, 496L))
  expect_equal(out$display_end,   c(5L, 500L))
})

test_that("widen_point_features: prot_len == 1 clamps to a single point", {
  f <- data.frame(start = 1L, end = 1L)
  out <- pelsa_widen_point_features(f, prot_len = 1L)
  expect_equal(out$display_start, 1L)
  expect_equal(out$display_end, 1L)
  expect_true(out$was_widened)
})

test_that("widen_point_features: empty / NA-coord rows pass through unchanged", {
  empty <- data.frame(start = integer(0), end = integer(0))
  out <- pelsa_widen_point_features(empty, prot_len = 100L)
  expect_equal(nrow(out), 0L)
  expect_true(all(c("display_start", "display_end", "was_widened") %in%
                  colnames(out)))

  bad <- data.frame(start = c(NA, 5L), end = c(NA, 5L))
  out2 <- pelsa_widen_point_features(bad, prot_len = 100L)
  # NA-coord row: display_* stay NA, was_widened FALSE (nothing to widen)
  expect_true(is.na(out2$display_start[1]))
  expect_true(is.na(out2$display_end[1]))
  expect_false(out2$was_widened[1])
  # valid single-AA row still widens
  expect_equal(out2$display_start[2], 2L)
  expect_equal(out2$display_end[2], 8L)
})

test_that("widen_point_features: non-data.frame input errors clearly", {
  expect_error(pelsa_widen_point_features(list(start = 1, end = 1), prot_len = 10L),
               "data.frame")
})

test_that("widen_point_features: invalid prot_len errors clearly instead of propagating NA", {
  f <- data.frame(start = 1L, end = 1L)
  expect_error(pelsa_widen_point_features(f, prot_len = NA),
               "prot_len must be a single finite number")
  expect_error(pelsa_widen_point_features(f, prot_len = Inf),
               "prot_len must be a single finite number")
  expect_error(pelsa_widen_point_features(f, prot_len = c(10L, 20L)),
               "prot_len must be a single finite number")
})

# ---- pelsa_feature_lanes (IRanges disjointBins) ------------------------------

test_that("feature_lanes packs overlapping features into distinct lanes", {
  f <- data.frame(start = c(1L, 5L, 40L), end = c(30L, 12L, 60L),
                  feature_class = c("catalytic_domain", "active_or_binding_site",
                                    "region_or_motif"),
                  stringsAsFactors = FALSE)
  out <- pelsa_feature_lanes(f)
  expect_true("lane" %in% colnames(out))
  # 1-30 and 5-12 overlap -> different lanes; 40-60 disjoint -> reuses a lane.
  expect_false(out$lane[1] == out$lane[2])
  expect_equal(out$lane[3], 1L)
})

test_that("feature_lanes: empty / all-invalid -> 0-row with lane column", {
  expect_equal(nrow(pelsa_feature_lanes(
    data.frame(start = integer(0), end = integer(0)))), 0L)
  bad <- data.frame(start = c(NA, 9L), end = c(5L, 3L),
                    feature_class = c("x", "y"), stringsAsFactors = FALSE)
  out <- pelsa_feature_lanes(bad)
  expect_equal(nrow(out), 0L)
  expect_true("lane" %in% colnames(out))
})

# ---- pelsa_feature_lanes: prot_len threading + widened lane-packing ---------

test_that("feature_lanes: prot_len threads into widened display_start/display_end", {
  f <- data.frame(start = 214L, end = 214L, feature_class = "active_or_binding_site",
                  stringsAsFactors = FALSE)
  out <- pelsa_feature_lanes(f, prot_len = 500L)
  expect_equal(out$display_start, 211L)
  expect_equal(out$display_end, 217L)
  expect_true(out$was_widened)
  expect_equal(out$lane, 1L)
})

test_that("feature_lanes: widened point features that now overlap get separate lanes", {
  # Two point features at 100 and 103: true spans don't overlap (100-100,
  # 103-103), but widened (+-3) they become 97-103 and 100-106 -- overlapping.
  f <- data.frame(start = c(100L, 103L), end = c(100L, 103L),
                  feature_class = c("active_or_binding_site", "active_or_binding_site"),
                  stringsAsFactors = FALSE)
  out <- pelsa_feature_lanes(f, prot_len = 500L)
  expect_false(out$lane[1] == out$lane[2])
})

test_that("feature_lanes: default prot_len (no clamp) preserves old no-prot_len behavior", {
  f <- data.frame(start = c(1L, 5L, 40L), end = c(30L, 12L, 60L),
                  feature_class = c("catalytic_domain", "active_or_binding_site",
                                    "region_or_motif"),
                  stringsAsFactors = FALSE)
  out <- pelsa_feature_lanes(f)   # no prot_len passed
  expect_true("lane" %in% colnames(out))
  expect_false(out$lane[1] == out$lane[2])
  expect_equal(out$lane[3], 1L)
  expect_equal(out$display_start, out$start)  # none are single-AA -> untouched
  expect_equal(out$display_end, out$end)
})

# ---- pelsa_woods_overlap_annotations (data.table foverlaps) ------------------

test_that("overlap_annotations lists DISTINCT feature names (no coords) per peptide", {
  f <- data.frame(start = c(1L, 5L, 40L), end = c(30L, 12L, 60L),
                  feature_class = c("catalytic_domain", "active_or_binding_site",
                                    "region_or_motif"),
                  stringsAsFactors = FALSE)
  ann <- pelsa_woods_overlap_annotations(c(10L, 50L, 100L), c(20L, 60L, 110L), f)
  # Names only (no @start-end), ";"-joined.
  expect_equal(ann[1], "catalytic_domain;active_or_binding_site")
  expect_equal(ann[2], "region_or_motif")
  expect_equal(ann[3], "")                            # peptide past all features
  expect_false(grepl("@", ann[1]))                    # no coordinates
})

test_that("overlap_annotations collapses repeated feature names to one", {
  # Two separate region_or_motif features both overlap the peptide -> listed ONCE.
  f <- data.frame(start = c(1L, 40L), end = c(30L, 60L),
                  feature_class = c("region_or_motif", "region_or_motif"),
                  stringsAsFactors = FALSE)
  ann <- pelsa_woods_overlap_annotations(10L, 55L, f)
  expect_equal(ann, "region_or_motif")               # de-duplicated
})

test_that("overlap_annotations: no features -> all empty; length preserved", {
  ann <- pelsa_woods_overlap_annotations(c(1L, 2L), c(5L, 6L), data.frame())
  expect_equal(ann, c("", ""))
})

# ---- plot builders smoke -----------------------------------------------------

test_that("track + panel builders return plots and tolerate empty inputs", {
  pep <- pelsa_woods_peptide_data("A", .woods_matched(), .woods_stat(), "AvB")
  iv  <- pelsa_coverage_intervals(pep$pep_start, pep$pep_end)
  fl  <- pelsa_feature_lanes(data.frame(
    start = 1L, end = 30L, feature_class = "catalytic_domain",
    feature_type = "Domain", stringsAsFactors = FALSE))

  expect_s3_class(pelsa_coverage_track_ggplot(iv, 70L), "ggplot")
  expect_s3_class(pelsa_feature_track_ggplot(fl, 70L), "ggplot")
  expect_s3_class(pelsa_woods_track_ggplot(pep, 70L), "ggplot")

  # empty-input variants still return a ggplot (placeholder), never error.
  expect_s3_class(pelsa_feature_track_ggplot(pelsa_feature_lanes(data.frame()), 70L),
                  "ggplot")
  expect_s3_class(pelsa_woods_track_ggplot(pep[0, ], 70L), "ggplot")

  p <- pelsa_woods_panel(pep, fl, iv, prot_len = 70L, source_id = "w")
  expect_s3_class(p, "plotly")
  expect_identical(p$x$source, "w")
})

# ---- M3 regression: coverage track for proteins shorter than 10 residues -----
# seq(10L, prot_len, by=...) errors ("wrong sign in 'by' argument") when
# prot_len < 10. The builder must guard the upper-tick sequence and still draw.

test_that("M3: coverage track builds for very short proteins (no error)", {
  iv <- data.frame(start = integer(0), end = integer(0))
  # prot_len = 5 and 1 previously errored in the tick seq(); must now build.
  expect_s3_class(pelsa_coverage_track_ggplot(iv, 5L), "ggplot")
  expect_s3_class(pelsa_coverage_track_ggplot(iv, 1L), "ggplot")

  # with an actual covered interval on a short protein it must still build.
  iv2 <- data.frame(start = 1L, end = 3L)
  expect_s3_class(pelsa_coverage_track_ggplot(iv2, 5L), "ggplot")
})

test_that("M3: coverage ticks unchanged for proteins >= 10 residues", {
  iv <- data.frame(start = integer(0), end = integer(0))
  g <- pelsa_coverage_track_ggplot(iv, 50L)
  expect_s3_class(g, "ggplot")
  # tick breaks match the original style: unique(c(1, seq(10, 50, by=10))).
  expected <- unique(c(1L, seq(10L, 50L, by = max(10L, round(50 / 10)))))
  brks <- ggplot2::ggplot_build(g)$layout$panel_params[[1]]$x$breaks
  brks <- brks[!is.na(brks)]
  expect_true(all(expected %in% brks))
})

test_that("woods builder uses the shared feature-class palette", {
  fl <- pelsa_feature_lanes(data.frame(
    start = c(1L, 40L), end = c(30L, 60L),
    feature_class = c("catalytic_domain", "region_or_motif"),
    feature_type = c("Domain", "Region"), stringsAsFactors = FALSE))
  gg <- pelsa_feature_track_ggplot(fl, 70L)
  b  <- suppressWarnings(ggplot2::ggplot_build(gg))
  # the fill scale draws from PELSA_FEATURE_COLORS
  used <- unique(b$data[[1]]$fill)
  expect_true(all(used %in% unname(PELSA_FEATURE_COLORS)))
})

test_that("feature_overlap_peptides: lists overlapping peptide aa-labels", {
  # feature [10,20]; peptides at starts 5(end 12), 30(end 40), 15(end 25)
  out <- pelsa_feature_overlap_peptides(
    feat_starts = c(10L), feat_ends = c(20L),
    pep_starts = c(5L, 30L, 15L), pep_ends = c(12L, 40L, 25L))
  expect_equal(out, "aa5;aa15")     # sorted by position, deduped; 30 excluded
})

test_that("feature_overlap_peptides: no overlap -> 'none'", {
  out <- pelsa_feature_overlap_peptides(c(100L), c(110L), c(5L), c(12L))
  expect_equal(out, "none")
})

test_that("woods track: -log10(adj.P) coloring, no gold-outline segment, builds", {
  pep <- data.frame(
    peptide_seq = c("A","B"), pep_start = c(1L,5L), pep_end = c(4L,9L),
    logFC = c(-2, 1.5), adj.P.Val = c(1e-9, 0.4), sig = c(TRUE, FALSE),
    stringsAsFactors = FALSE)
  gg <- pelsa_woods_track_ggplot(pep, prot_len = 20L)
  expect_s3_class(gg, "ggplot")
  # The -log10 column is clamped (1e-9 -> -log10 = 9 -> clamp 5); just assert build.
})

test_that("feature tooltip uses real feature_type + description, not feature_class", {
  f <- data.frame(start = 10L, end = 20L, feature_class = "region_or_motif",
                  feature_type = "Region", description = "Disordered",
                  lane = 1L, stringsAsFactors = FALSE, check.names = FALSE)
  gg <- pelsa_feature_track_ggplot(f, prot_len = 100L)
  # Assert on the .tip column the geom carries (the hover NAME line) - it must use
  # the real UniProt feature_type + description, not the 9-bucket feature_class.
  tip <- gg$data$.tip
  expect_true(any(grepl("Region: Disordered", tip, fixed = TRUE)))
  expect_false(any(grepl("region_or_motif", tip, fixed = TRUE)))
})

# ---- doc regression: interactive Woods track has NO gold outline ------------
# The interactive Woods track encodes significance via the neglogp color
# gradient only; it never draws a gold underlay. Guard the comments from
# resurrecting the stale "gold outline" / "thick gold segment underneath" claim.

test_that("woods_helpers source no longer documents a gold-outline Woods track", {
  src_path <- testthat::test_path("..", "..", "R", "tab_pelsa_panel_helpers.R")
  skip_if_not(file.exists(src_path), "tab_pelsa_panel_helpers.R source not found")
  src <- paste(readLines(src_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("gold outline", src, ignore.case = TRUE))
  expect_false(grepl("thick gold segment", src, ignore.case = TRUE))
})

test_that("feature legend UI lists every PELSA_FEATURE_COLORS class", {
  html <- as.character(.pelsa_feature_legend_ui())
  # one entry per palette class, including ones absent from any given protein
  expect_true(grepl("transmembrane / signal", html, fixed = TRUE))
  expect_true(grepl("none / unannotated", html, fixed = TRUE))
  # one <li> entry per palette class (every class shown, present or not)
  n_li <- length(gregexpr("<li", html, fixed = TRUE)[[1]])
  expect_equal(n_li, length(PELSA_FEATURE_COLORS))
  # every palette HEX color appears as a swatch
  for (col in unname(PELSA_FEATURE_COLORS)) {
    expect_true(grepl(col, html, fixed = TRUE))
  }
})

test_that("pelsa_woods_peptide_data default sig_cutoff is the shared constant symbol", {
  # Default REFERENCES the shared export constant, not a stray literal 0.05;
  # live callers still thread isolate(sig_cutoff_r()) for the user-set cutoff.
  expect_identical(formals(Protigy:::pelsa_woods_peptide_data)$sig_cutoff,
                   as.symbol(".PELSA_EXPORT_SIG_CUTOFF"))
})

# ---- STATIC export Woods plot: x-axis ticks every 20 + 45-degree rotation ----
# The exported (PNG) Woods plot crowds/overlaps when ticks step by 10 and sit
# horizontal. Ticks must step by 20 and the x labels rotate 45 degrees. This is
# the EXPORT builder only (pelsa_woods_export_ggplot); the interactive coverage
# track (pelsa_coverage_track_ggplot, test "M3" above) is intentionally untouched.

.woods_export_pep <- function() data.frame(
  peptide_seq = c("A", "B"), pep_start = c(10L, 80L), pep_end = c(20L, 95L),
  logFC = c(-2.0, 1.5), adj.P.Val = c(1e-6, 0.4),
  stringsAsFactors = FALSE)

test_that("woods export x ticks step by 20 (not 10)", {
  # seq(0, prot_len, by = 20) -> 0,20,...; the 0 break is below the axis lower
  # limit (1) so it renders as NA and drops out, leaving 20,40,60,80,100.
  g <- pelsa_woods_export_ggplot(.woods_export_pep(), features = NULL,
                                 prot_len = 100L, gene = "GENE", accession = "ACC",
                                 contrast = "AvB")
  brks <- ggplot2::ggplot_build(g)$layout$panel_params[[1]]$x$breaks
  brks <- sort(brks[!is.na(brks)])
  expect_true(all(c(20, 40, 60, 80, 100) %in% brks))
  expect_false(any(c(10, 30, 50, 70, 90) %in% brks))
})

test_that("woods export rotates x-axis text 45 degrees", {
  g <- pelsa_woods_export_ggplot(.woods_export_pep(), features = NULL,
                                 prot_len = 100L, gene = "GENE", accession = "ACC",
                                 contrast = "AvB")
  expect_equal(g$theme$axis.text.x$angle, 45)
  expect_equal(g$theme$axis.text.x$hjust, 1)
})

test_that("woods export empty-data placeholder also steps ticks by 20", {
  g <- pelsa_woods_export_ggplot(data.frame(), features = NULL, prot_len = 100L,
                                 gene = "GENE", accession = "ACC", contrast = "AvB")
  brks <- ggplot2::ggplot_build(g)$layout$panel_params[[1]]$x$breaks
  brks <- sort(brks[!is.na(brks)])
  expect_true(all(c(20, 40) %in% brks))
  expect_false(any(c(10, 30) %in% brks))
})

test_that("woods export still labels the protein end for short proteins (< 20 aa)", {
  # by = 20 alone yields only {0} for prot_len < 20, and 0 is below the axis
  # lower limit (1) -> a bare x-axis with no ticks. The protein end must always
  # remain a visible break so the axis is never blank.
  for (L in c(8L, 15L)) {
    g <- pelsa_woods_export_ggplot(.woods_export_pep(), features = NULL,
                                   prot_len = L, gene = "G", accession = "A",
                                   contrast = "AvB")
    brks <- ggplot2::ggplot_build(g)$layout$panel_params[[1]]$x$breaks
    brks <- brks[!is.na(brks)]
    expect_true(L %in% brks,
                info = sprintf("prot_len=%d must keep a visible end tick", L))
  }
})

################################################################################
# --- from test-pelsa-intensity-data.R  (intensity line data builders + static export) ---
################################################################################

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

# ---- pelsa_intensity_build_index + .index parity ----------------------------

test_that("pelsa_intensity_build_index groups matched_cache by non-blank accession", {
  matched <- .mk_matched(seq = c("pA", "pB", "pC"),
                         accession = c("PROT", "PROT", "OTHER"),
                         pep_start = c(10L, 20L, 30L), row_id = 1:3)
  idx <- pelsa_intensity_build_index(matched)
  expect_setequal(names(idx$by_acc), c("PROT", "OTHER"))
  expect_equal(nrow(idx$by_acc[["PROT"]]), 2L)
})

test_that("pelsa_intensity_build_index drops NA / blank accession rows", {
  matched <- .mk_matched(seq = c("pA", "pB", "pC"),
                         accession = c("PROT", NA_character_, ""),
                         pep_start = c(10L, 20L, 30L), row_id = 1:3)
  idx <- pelsa_intensity_build_index(matched)
  expect_setequal(names(idx$by_acc), "PROT")
})

test_that("pelsa_intensity_line_data: .index path is identical to the scan path", {
  proc <- .mk_proc()
  # Two peptide rows/occurrences for "PROT" (aligned to .mk_proc()'s two rows
  # via .row_id 1/2) plus a THIRD row for a different accession "OTHER" that
  # must be excluded from PROT's result -- this exercises split()'s grouping
  # across a duplicate accession, not just a single-row toy case.
  stat <- .mk_stat(c("pA", "pB", "pOther"), c("PROT", "PROT", "OTHER"),
                   c(0.001, 0.02, 0.5), row_id = 1:3)
  matched <- .mk_matched(c("pA", "pB", "pOther"), c("PROT", "PROT", "OTHER"),
                         pep_start = c(10L, 20L, 30L), row_id = 1:3,
                         pep_occurrence_idx = c(1L, 2L, 1L))
  idx <- pelsa_intensity_build_index(matched)
  plain   <- pelsa_intensity_line_data("PROT", stat, matched, proc,
                                       condition_map = .cond_map,
                                       condition_order = .cond_order,
                                       contrast = "C1")
  indexed <- pelsa_intensity_line_data("PROT", stat, matched, proc,
                                       condition_map = .cond_map,
                                       condition_order = .cond_order,
                                       contrast = "C1", .index = idx)
  expect_identical(plain, indexed)
  # Guard against a vacuous pass: both paths must actually resolve BOTH PROT
  # occurrences (not silently collapse to one row or leak the OTHER row in).
  expect_equal(nrow(plain), 4L)  # 2 occurrences x 2 conditions (A, B)
  expect_setequal(plain$peptide_seq, c("pA", "pB"))
})

test_that("pelsa_intensity_line_data: .index absent-accession still errors", {
  proc <- .mk_proc()
  stat <- .mk_stat("pSIG", "PROT", 0.001, row_id = 1L)
  matched <- .mk_matched("pSIG", "PROT", 100L, 1L)
  idx <- pelsa_intensity_build_index(matched)
  expect_error(
    pelsa_intensity_line_data("ABSENT", stat, matched, proc,
                              condition_map = .cond_map,
                              condition_order = .cond_order,
                              contrast = "C1", .index = idx),
    regexp = "accession|ABSENT|not found")
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
  # Panel titles are bolded via <b></b> HTML (plotly annotation font has no
  # `face`); strip tags so the assertions match the rendered text, not markup.
  texts <- vapply(anns, function(a) gsub("<[^>]+>", "", a$text %||% ""),
                  character(1))

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

################################################################################
# --- from test-pelsa-export-helpers.R  (export helpers (prot_len)) ---
################################################################################

################################################################################
# Tests for PELSA export helpers (pure, non-reactive).
#
#   pelsa_export_prot_len(coverage, acc, peptides) -- protein length for a Woods
#     export: prefer the coverage frame's protein_length; else fall back to the
#     max pep_end of the protein's peptides; else 1L.
################################################################################


test_that("pelsa_export_prot_len uses the coverage frame's protein_length", {
  cov <- data.frame(accession = c("P1", "P2"),
                    protein_length = c(120L, 80L),
                    stringsAsFactors = FALSE)
  expect_equal(pelsa_export_prot_len(cov, "P1"), 120L)
  expect_equal(pelsa_export_prot_len(cov, "P2"), 80L)
})

test_that("pelsa_export_prot_len falls back to max(pep_end) when coverage lacks the length", {
  cov <- data.frame(accession = "P1", protein_length = NA_integer_,
                    stringsAsFactors = FALSE)
  peptides <- data.frame(pep_end = c(40L, 95L, 60L), stringsAsFactors = FALSE)
  expect_equal(pelsa_export_prot_len(cov, "P1", peptides), 95L)
})

test_that("pelsa_export_prot_len returns 1L (no warning) when all pep_end are NA", {
  # Regression: max(integer-all-NA, na.rm = TRUE) warns
  # ("no non-missing arguments to max; returning -Inf") and returns -Inf.
  # Reachable for older caches lacking span columns (pep_end all NA). The result
  # is still correct via the < 1L -> 1L fallback, but no warning should leak.
  cov <- data.frame(accession = "P1", protein_length = NA_integer_,
                    stringsAsFactors = FALSE)
  peptides <- data.frame(pep_end = c(NA_integer_, NA_integer_),
                         stringsAsFactors = FALSE)
  expect_no_warning(plen <- pelsa_export_prot_len(cov, "P1", peptides))
  expect_equal(plen, 1L)
})

test_that("pelsa_export_prot_len returns 1L when no coverage and no peptides", {
  expect_equal(pelsa_export_prot_len(NULL, "P1"), 1L)
})

test_that("pelsa_export_prot_len floors a non-positive max(pep_end) at 1L", {
  cov <- data.frame(accession = "P1", protein_length = NA_integer_,
                    stringsAsFactors = FALSE)
  peptides <- data.frame(pep_end = c(0L, -5L), stringsAsFactors = FALSE)
  expect_equal(pelsa_export_prot_len(cov, "P1", peptides), 1L)
})

################################################################################
# --- .pelsa_volcano_labels: protein-name fallback (Task 4) ---
################################################################################

test_that(".pelsa_volcano_labels uses protein-name fallback when gene missing", {
  matched <- data.frame(
    .key         = c("k1", "k1", "k2"),
    gene         = c("", NA, "GC"),
    protein_name = c("NameA", "NameB", "NameC"),
    accession    = c("P1", "P2", "P3"),
    pep_start    = c(10L, 20L, 30L),
    stringsAsFactors = FALSE
  )
  out <- Protigy:::.pelsa_volcano_labels(matched, ".key")
  # k1: two mappings, both gene-missing -> protein names; k2: gene present.
  lab_k1 <- out$label[out$.key == "k1"]
  expect_equal(lab_k1, "NameA_aa10;NameB_aa20")
  lab_k2 <- out$label[out$.key == "k2"]
  expect_equal(lab_k2, "GC_aa30")
})

test_that(".pelsa_volcano_labels tolerates a missing protein_name column", {
  matched <- data.frame(
    .key      = "k1",
    gene      = "",
    accession = "P1",
    pep_start = 10L,
    stringsAsFactors = FALSE
  )
  out <- Protigy:::.pelsa_volcano_labels(matched, ".key")
  expect_equal(out$label, "P1_aa10")
})

test_that(".pelsa_best_back_map carries won_protein_name from the matched cache", {
  rolled <- data.frame(
    peptide_seq    = "PEPK",
    won_accessions = "P1",
    adj_p          = 0.01,
    logFC          = -2,
    stringsAsFactors = FALSE
  )
  m <- data.frame(
    PEP.StrippedSequence = "PEPK",
    accession            = "P1",
    gene                 = "",
    protein_name         = "NameA",
    pep_start            = 10L,
    pep_end              = 13L,
    P.Value              = 0.005,
    stringsAsFactors     = FALSE
  )
  back <- Protigy:::.pelsa_best_back_map(rolled, m)
  expect_equal(back$won_protein_name, "NameA")
})

test_that("volcano tip Peptide label uses winning protein name when gene missing", {
  d <- data.frame(
    winning_gene         = "",
    winning_protein_name = "NameA",
    winning_accession    = "P1",
    PG.Genes             = "",
    PG.ProteinAccessions = "P1",
    pep_start            = 10L,
    pep_end              = 13L,
    logFC                = -2,
    adj.P.Val            = 0.01,
    stringsAsFactors     = FALSE
  )
  tip <- Protigy:::pelsa_volcano_tip(d)
  expect_true(grepl("Peptide: NameA_aa10", tip, fixed = TRUE))
  # Gene display must stay gene (or its "NA" fallback), never the protein name.
  # winning_gene="" and PG.Genes="" so gene_fb="" -> Gene line renders as "NA".
  expect_false(grepl("Gene: NameA", tip, fixed = TRUE))
  expect_true(grepl("Gene: NA", tip, fixed = TRUE))
})

test_that("pin metadata Peptide row uses winning protein name when gene missing", {
  vdf <- data.frame(
    winning_gene         = "",
    winning_protein_name = "NameA",
    winning_accession    = "P1",
    PG.Genes             = "",
    PG.ProteinAccessions = "P1",
    id                   = "PEPK",
    pep_start            = 10L,
    pep_end              = 13L,
    adj.P.Val            = 0.01,
    logFC                = -2,
    stringsAsFactors     = FALSE
  )
  rows <- Protigy:::pelsa_pin_metadata_rows(vdf, 1L, n_peptides = 1L)
  pep_val <- rows$value[rows$label == "Peptide"]
  expect_equal(pep_val, "NameA_aa10")
  # Gene row must stay gene (or its "NA" fallback), never the protein name.
  # winning_gene="" and PG.Genes="" so gene_disp="NA".
  gene_val <- rows$value[rows$label == "Gene"]
  expect_false(identical(gene_val, "NameA"))
  expect_equal(gene_val, "NA")
})

# ---------------------------------------------------------------------------
# feat_df cache-preference tests (D10/D11)
#
# When a cache entry carries feat_raw, feat_df() must return it without
# consulting the live annotation file.  When no feat_raw is present, the
# existing live-file fallback (with D3 notifications) must run unchanged.
# ---------------------------------------------------------------------------

test_that("feat_df prefers the cached feat_raw over the live annotation file", {
  # Arrange: a cache entry that passes pelsa_analysis_failed() (no $error field)
  # and carries a known feat_raw data.frame.
  cached_feat <- data.frame(
    accession     = "Q_CACHE",
    start         = 1L,
    end           = 9L,
    feature_class = "folded_domain",
    stringsAsFactors = FALSE
  )
  entry_with_feat <- list(
    matched             = data.frame(
      PEP.StrippedSequence = "PEPA", accession = "Q_CACHE",
      gene = "G1", pep_start = 1L, pep_end = 9L,
      stringsAsFactors = FALSE
    ),
    annotation_features = .mk_annotation_features(1L),
    feat_raw            = cached_feat   # the new field Task 1 added
  )
  # The live annotation path points at a file that does NOT exist on disk.
  # If the cache branch is NOT in place, feat_df() hits the missing-file branch
  # and emits a message() -> the test would fail on the inner assertion AND on
  # the outer expect_no_message wrapper.
  missing_path <- tempfile(fileext = ".tsv")   # never created

  result_val <- NULL
  expect_no_message(
    shiny::testServer(
      PELSASection3_Ome_Server,
      args = list(
        id = "Proteome", ome = "Proteome",
        GCT_processed                = reactive(NULL),
        parameters                   = reactive(NULL),
        default_annotation_column    = reactive(NULL),
        color_map                    = reactive(NULL),
        stat_results                 = reactive(NULL),
        stat_params                  = reactive(.mk_stat_params()),
        pelsa_analysis               = reactive(list(Proteome = entry_with_feat)),
        pelsa_setup_state            = reactive(list(
          species          = list(Proteome = NULL),
          marker_rows      = list(Proteome = data.frame(
            accession = "Q_CACHE", gene = "G1", stringsAsFactors = FALSE)),
          self_curated     = list(Proteome = FALSE),
          annotation_path  = list(Proteome = missing_path)
        )),
        poi_registry          = reactiveVal(list()),
        label_mode_registry   = reactiveVal(list()),
        n_top_adjp_registry = reactiveVal(list()),
        n_top_markers_registry = reactiveVal(list())
      ),
      {
        result_val <<- feat_df()
      }
    )
  )
  # Cache wins: we get the cached frame, NOT NULL (which the missing-file branch
  # would have returned).
  expect_equal(result_val$accession, "Q_CACHE")
})

test_that("feat_df falls back to live file when cache entry lacks feat_raw", {
  # Arrange: a cache entry WITHOUT feat_raw (older cache / pre-Task-1 entry).
  # The live annotation path points at a REAL temporary file so the fallback
  # succeeds and returns a parsed data.frame.
  tmp <- tempfile(fileext = ".tsv")
  writeLines(
    c("accession\tfeature_type\tstart\tend\tdescription",
      "Q_LIVE\tDomain\t1\t9\tTest domain"),
    tmp
  )
  on.exit(unlink(tmp), add = TRUE)

  entry_no_feat <- list(
    matched             = data.frame(
      PEP.StrippedSequence = "PEPA", accession = "Q_LIVE",
      gene = "G1", pep_start = 1L, pep_end = 9L,
      stringsAsFactors = FALSE
    ),
    annotation_features = .mk_annotation_features(1L)
    # No feat_raw field -> fallback to live file
  )

  result_val <- NULL
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = list(
      id = "Proteome", ome = "Proteome",
      GCT_processed                = reactive(NULL),
      parameters                   = reactive(NULL),
      default_annotation_column    = reactive(NULL),
      color_map                    = reactive(NULL),
      stat_results                 = reactive(NULL),
      stat_params                  = reactive(.mk_stat_params()),
      pelsa_analysis               = reactive(list(Proteome = entry_no_feat)),
      pelsa_setup_state            = reactive(list(
        species          = list(Proteome = NULL),
        marker_rows      = list(Proteome = data.frame(
          accession = "Q_LIVE", gene = "G1", stringsAsFactors = FALSE)),
        self_curated     = list(Proteome = FALSE),
        annotation_path  = list(Proteome = tmp)
      )),
      poi_registry          = reactiveVal(list()),
      label_mode_registry   = reactiveVal(list()),
      n_top_adjp_registry = reactiveVal(list()),
      n_top_markers_registry = reactiveVal(list())
    ),
    {
      result_val <<- feat_df()
    }
  )
  # Fallback: live file was read; Q_LIVE accession is present.
  expect_true(is.data.frame(result_val))
  expect_true("Q_LIVE" %in% result_val$accession)
})

# ---- pelsa_feature_track_ggplot: widened single-AA rendering ---------------

test_that("feature_track_ggplot draws widened span for a single-AA feature", {
  fl <- pelsa_feature_lanes(
    data.frame(start = 214L, end = 214L, feature_class = "active_or_binding_site",
              feature_type = "Active site", stringsAsFactors = FALSE),
    prot_len = 500L)
  gg <- pelsa_feature_track_ggplot(fl, 500L)
  b <- suppressWarnings(ggplot2::ggplot_build(gg))
  rect_layer <- b$data[[1]]
  expect_equal(rect_layer$xmin, 211)
  expect_equal(rect_layer$xmax, 217)
})

test_that("feature_track_ggplot tooltip keeps true coords + widened note", {
  fl <- pelsa_feature_lanes(
    data.frame(start = 214L, end = 214L, feature_class = "active_or_binding_site",
              feature_type = "Active site", stringsAsFactors = FALSE),
    prot_len = 500L)
  gg <- pelsa_feature_track_ggplot(fl, 500L)
  b <- suppressWarnings(ggplot2::ggplot_build(gg))
  tip <- b$data[[1]]$text
  expect_true(grepl("214-214", tip, fixed = TRUE))
  expect_true(grepl("widened", tip, ignore.case = TRUE))
  expect_true(grepl("211-217", tip, fixed = TRUE))
})

test_that("feature_track_ggplot tooltip has no widened note for multi-AA features", {
  fl <- pelsa_feature_lanes(
    data.frame(start = 10L, end = 30L, feature_class = "catalytic_domain",
              feature_type = "Domain", stringsAsFactors = FALSE),
    prot_len = 500L)
  gg <- pelsa_feature_track_ggplot(fl, 500L)
  b <- suppressWarnings(ggplot2::ggplot_build(gg))
  tip <- b$data[[1]]$text
  expect_true(grepl("10-30", tip, fixed = TRUE))
  expect_false(grepl("widened", tip, ignore.case = TRUE))
})

test_that("feature_track_ggplot: fixture lacking display_start/display_end still renders", {
  # Guards against a caller that built a `features_lanes`-shaped frame by hand
  # (e.g. an older test fixture) without going through pelsa_feature_lanes().
  fl <- data.frame(start = 10L, end = 30L, feature_class = "catalytic_domain",
                   feature_type = "Domain", lane = 1L, stringsAsFactors = FALSE)
  gg <- pelsa_feature_track_ggplot(fl, 500L)
  expect_s3_class(gg, "ggplot")
  b <- suppressWarnings(ggplot2::ggplot_build(gg))
  expect_equal(b$data[[1]]$xmin, 10)
  expect_equal(b$data[[1]]$xmax, 30)
})

# ---- pelsa_woods_export_ggplot: widened single-AA feature band --------------

test_that("woods_export_ggplot draws the widened span for a single-AA feature", {
  pep <- data.frame(
    peptide_seq = "PEPTIDEK", pep_start = 200L, pep_end = 230L,
    logFC = 1.2, adj.P.Val = 0.01, P.Value = 0.001, sig = TRUE,
    stringsAsFactors = FALSE)
  feats <- data.frame(start = 214L, end = 214L, feature_class = "active_or_binding_site",
                      stringsAsFactors = FALSE)
  gg <- pelsa_woods_export_ggplot(pep, feats, prot_len = 500L, gene = "GENE1",
                                  accession = "P12345", contrast = "A_over_B")
  b <- suppressWarnings(ggplot2::ggplot_build(gg))
  # the feature geom_rect layer is the 3rd added rect layer (dummy legend rect,
  # frame border rect, then the real feature rect) -- identify it by having a
  # non-degenerate x-range matching the widened span.
  feat_layer <- Filter(function(d) "xmin" %in% colnames(d) && any(d$xmin != d$xmax),
                       b$data)
  expect_true(length(feat_layer) >= 1L)
  widened <- feat_layer[[length(feat_layer)]]
  expect_equal(widened$xmin, 211)
  expect_equal(widened$xmax, 217)
})

test_that("woods_export_ggplot caption notes widened features when present", {
  pep <- data.frame(
    peptide_seq = "PEPTIDEK", pep_start = 200L, pep_end = 230L,
    logFC = 1.2, adj.P.Val = 0.01, P.Value = 0.001, sig = TRUE,
    stringsAsFactors = FALSE)
  feats_point <- data.frame(start = 214L, end = 214L,
                            feature_class = "active_or_binding_site",
                            stringsAsFactors = FALSE)
  gg_point <- pelsa_woods_export_ggplot(pep, feats_point, prot_len = 500L,
                                        gene = "GENE1", accession = "P12345",
                                        contrast = "A_over_B")
  expect_true(grepl("widened", gg_point$labels$caption, ignore.case = TRUE))

  feats_wide <- data.frame(start = 10L, end = 30L, feature_class = "catalytic_domain",
                           stringsAsFactors = FALSE)
  gg_wide <- pelsa_woods_export_ggplot(pep, feats_wide, prot_len = 500L,
                                       gene = "GENE1", accession = "P12345",
                                       contrast = "A_over_B")
  expect_false(grepl("widened", gg_wide$labels$caption, ignore.case = TRUE))
})

test_that("woods_export_ggplot: no features -> builds fine, no widened note", {
  pep <- data.frame(
    peptide_seq = "PEPTIDEK", pep_start = 200L, pep_end = 230L,
    logFC = 1.2, adj.P.Val = 0.01, P.Value = 0.001, sig = TRUE,
    stringsAsFactors = FALSE)
  gg <- pelsa_woods_export_ggplot(pep, data.frame(), prot_len = 500L,
                                  gene = "GENE1", accession = "P12345",
                                  contrast = "A_over_B")
  expect_s3_class(gg, "ggplot")
  expect_false(grepl("widened", gg$labels$caption, ignore.case = TRUE))
})

# ---------------------------------------------------------------------------
# Task 8: shared volcano x/y range across every figure in one ome's export.
#
# Every volcano PNG exported for one ome (across ALL contrasts, both all-peptide
# and best-peptide types) must share the SAME coord_cartesian x/y range, so any
# two exported figures are visually comparable side-by-side. The pre-Task-8
# single-pass loop autoscaled each PNG to its own contrast's data.

# A 2-contrast stat_results in the `_full` fixture shape (carries .row_id, aligns
# to .mk_cache_full()'s matched rows). The two contrasts have deliberately
# different logFC/P.Value extents so their independently-autoscaled ranges would
# DIFFER -- the shared-range guarantee is what makes them equal.
.mk_stat_results_full_2c <- function() {
  list(Proteome = data.frame(
    id                   = c("PEPA", "PEPB", "PEPC"),
    .row_id              = c(1L, 2L, 3L),
    PEP.StrippedSequence = c("PEPA", "PEPB", "PEPC"),
    PG.ProteinAccessions = c("ACC1", "ACC2", "ACC1"),
    PG.Genes             = c("G1", "G2", "G1"),
    logFC.A_over_B       = c(2.0, -1.5, 0.1),
    adj.P.Val.A_over_B   = c(0.001, 0.02, 0.8),
    P.Value.A_over_B     = c(0.0001, 0.005, 0.7),
    logFC.A_over_C       = c(4.0, -3.2, 0.6),
    adj.P.Val.A_over_C   = c(0.03, 0.5, 0.9),
    P.Value.A_over_C     = c(0.02, 0.4, 0.88),
    stringsAsFactors     = FALSE, check.names = FALSE
  ))
}

test_that("export_volcano applies the SAME x/y coord range to every volcano PNG for one ome", {
  captured <- list()
  testthat::local_mocked_bindings(
    pelsa_save_figure = function(plot, dir_name, basename, width, height, ...) {
      built <- ggplot2::ggplot_build(plot)
      captured[[basename]] <<-
        built$layout$panel_params[[1]][c("x.range", "y.range")]
      invisible(NULL)
    },
    .package = "Protigy"
  )
  args <- .full_args()
  args$stat_results <- reactive(.mk_stat_results_full_2c())
  shiny::testServer(
    PELSASection3_Ome_Server,
    args = args,
    {
      # best panel ON -> both all-peptide AND best-peptide figures are built, so
      # the shared range must hold across BOTH types AND both contrasts.
      session$setInputs(pelsa_color_mode = "significance",
                        pelsa_label_mode = "all_markers",
                        pelsa_show_best_panel = TRUE)
      # >=2 contrasts is what makes "all ranges equal" a real assertion.
      expect_gte(length(contrast_choices()), 2L)
      session$returned$volcano(tempfile())
    }
  )
  # 2 contrasts x 2 types (all-peptide + best-peptide) = up to 4 PNGs.
  expect_true(length(captured) >= 2L)
  ranges <- lapply(captured, function(c) list(x = c$x.range, y = c$y.range))
  first <- ranges[[1]]
  for (r in ranges[-1]) {
    expect_equal(r$x, first$x, tolerance = 1e-6)
    expect_equal(r$y, first$y, tolerance = 1e-6)
  }
})
