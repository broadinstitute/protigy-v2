################################################################################
# PELSA Phase 8 — end-to-end SYNTHETIC integration check.
#
# This is the ONE test that runs the ASSEMBLED PELSA pipeline on a larger
# synthetic frame and asserts the FINAL artifacts are COHERENT — catching
# integration drift (column ordering, join semantics, sample-order handling,
# cross-helper contract mismatches) that the per-helper unit tests miss. It does
# NOT re-test each helper's internals; it asserts the SEAMS between components.
#
# Pipeline exercised exactly as Start-Analysis (5D) drives it:
#   Setup snapshot
#     -> pelsa_run_analysis(gcts, gcts_original, snapshot, fasta_map, feat_df)
#        (the per-dataset 10-component cache; NO network, cache-as-is feat_df)
#     -> pelsa_volcano_stat_df() + pelsa_build_volcano_df()   (3A volcano data)
#     -> pelsa_intensity_proteins() + pelsa_intensity_line_data()  (3C lines)
#     -> pelsa_volcano_labels_sidecar()                        (7F 12-col sidecar)
#     -> pelsa_volcano_export_df() / pelsa_plotted_intensities_df()  (exports)
#
# NO LIVE NETWORK: the FASTA + feature cache are INJECTED synthetic objects; the
# pipeline never calls pelsa_fetch_uniprot (cache-as-is). The synthetic 42MB real
# FASTA is NOT used.
################################################################################

library(testthat)
source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# ---- integration fixture builders --------------------------------------------

# Build a cmapR GCT from a synthetic peptide frame (rdesc = annotation cols,
# mat = intensity cols, cdesc = sample -> condition). Reused from the 5D tests'
# GCT-building approach so the integration path matches what the app feeds in.
.int_mk_gct <- function(syn) {
  peptides <- syn$peptides
  sc <- syn$sample_cols
  rids <- paste0("pep", seq_len(nrow(peptides)))
  mat <- as.matrix(peptides[, sc])
  rownames(mat) <- rids
  rdesc <- peptides[, setdiff(colnames(peptides), sc), drop = FALSE]
  rownames(rdesc) <- rids
  cdesc <- data.frame(
    condition = sub("_R[0-9]+$", "", sc),
    row.names = sc, stringsAsFactors = FALSE
  )
  cmapR::GCT(mat = mat, rdesc = rdesc, cdesc = cdesc)
}

# A larger synthetic frame (a few hundred peptides) wired for the integration
# assertions: the seeded edge cases (shared/absent/iso/dup/tie peptides + NA
# holes + LowN insufficient-replicate row) plus 200 filler peptides. We
# deterministically pin the TIE marker protein's two peptides to one
# SIGNIFICANT + one NON-significant so the marker intensity panel has BOTH
# significant/other panels regardless of the random stat draw (seed-stable).
.int_build <- function(seed = 7, n_extra = 200) {
  syn <- pelsa_make_synthetic(seed = seed, n_extra_peptides = n_extra)
  contrast <- syn$contrasts
  adjp_col <- paste0("adj.P.Val.", contrast)

  peptides <- syn$peptides
  ti1 <- which(peptides$PEP.StrippedSequence == syn$tie_peptides[[1]])
  ti2 <- which(peptides$PEP.StrippedSequence == syn$tie_peptides[[2]])
  peptides[[adjp_col]][ti1] <- 0.001   # TIEPEPONEK significant
  peptides[[adjp_col]][ti2] <- 0.500   # TIEPEPTWOK NOT significant
  syn$peptides <- peptides

  gct <- .int_mk_gct(syn)

  # Hand-set feature cache (NO network). Covers a couple of mapped accessions so
  # the annotation seam has a non-"none" class to resolve; the rest fall to the
  # unannotated set.
  feat_df <- data.frame(
    accession     = c("SHARED1", "TIEPROT", "DUPPROT", "P12345"),
    start         = c(1L, 1L, 1L, 1L),
    end           = c(50L, 40L, 60L, 30L),
    feature_class = c("domain", "domain", "domain", "domain"),
    stringsAsFactors = FALSE
  )

  # Setup snapshot + sample_order (drives the depth-bar order downstream). The
  # sample_order is the user-confirmed column order from Phase-5 Setup.
  sample_order <- syn$sample_cols
  snap <- list(
    datasets      = "ds",
    species       = "human",
    condition_col = list(ds = "condition"),
    sample_order  = list(ds = sample_order)
  )

  # Markers: the TIE protein (both-panels marker) + the UniProt ISOFORM accession
  # P12345-2 (exercises the isoform-base marker rule end to end).
  markers <- c(syn$tie_accession, syn$isoform_accession)

  list(
    syn = syn, gct = gct, feat_df = feat_df, snap = snap,
    contrast = contrast, adjp_col = adjp_col, markers = markers,
    sample_order = sample_order
  )
}

# Build the stat frame the Statistics tab feeds into 3A: the peptide frame with a
# stable .row_id (the 1-based peptide-frame row, the collision-proof join key the
# matched cache also carries), then the representative pep_start/pep_end span
# attached from the matched cache.
.int_stat_df <- function(syn, matched) {
  stat_raw <- syn$peptides
  stat_raw$.row_id <- seq_len(nrow(stat_raw))
  pelsa_volcano_stat_df(stat_raw, matched)
}

# =============================================================================
# THE end-to-end integration test.
# =============================================================================

test_that("PELSA pipeline is COHERENT end-to-end on a larger synthetic frame", {
  fx <- .int_build(seed = 7, n_extra = 200)
  syn <- fx$syn
  n_pep <- nrow(syn$peptides)
  expect_gt(n_pep, 200L)   # genuinely larger than the unit fixtures

  # ---- 5D: run the assembled pipeline (NO network, cache-as-is) ------------
  res <- pelsa_run_analysis(
    gcts          = list(ds = fx$gct),
    gcts_original = list(ds = fx$gct),
    setup_snapshot = fx$snap,
    fasta_map     = syn$fasta,
    feat_df       = fx$feat_df
  )
  expect_setequal(names(res), "ds")
  cache <- res$ds
  expect_false(pelsa_analysis_failed(cache))

  # ---- SEAM 1: cache has all 10 documented, well-formed components ---------
  expect_setequal(
    names(cache),
    c("matched", "unmatched", "cv", "n_quantified", "depth_summary",
      "coverage", "peptide_metrics", "annotation", "unannotated", "qc")
  )
  # matched carries the .row_id join key + the 2B span columns.
  expect_true(all(c(".row_id", "accession", "gene", "pep_start", "pep_end",
                    "pep_occurrence_idx", "n_occurrences",
                    "PEP.StrippedSequence") %in% colnames(cache$matched)))
  expect_gt(nrow(cache$matched), 0L)
  # peptide_metrics + cv are over the FULL peptide universe (row counts sane
  # relative to the input).
  expect_equal(nrow(cache$peptide_metrics), n_pep)
  expect_equal(cache$qc$n_peptides, n_pep)
  expect_equal(cache$depth_summary$total_n_peptides, n_pep)
  # n_quantified is one count per sample column.
  expect_length(cache$n_quantified, length(syn$sample_cols))
  expect_named(cache$n_quantified)

  # ---- SEAM 2: CV row_id universe + condition set + sample_order ------------
  expect_s3_class(cache$cv, "data.frame")
  # CV row_id ranges over the WHOLE peptide universe (1..n_pep): the CV row_id
  # and the peptide-frame row refer to the same peptide.
  expect_equal(range(cache$cv$row_id), c(1L, n_pep))
  # CV's condition set matches the cdesc condition column (the GCT cdesc).
  cdesc_conds <- sort(unique(cmapR::meta(fx$gct, dimension = "column")$condition))
  expect_setequal(unique(cache$cv$condition), cdesc_conds)
  # The LowN insufficient-replicate edge (first row forced to 1 non-NA) is
  # carried through the assembled pipeline.
  low1 <- cache$cv[cache$cv$row_id == 1L &
                     cache$cv$condition == syn$low_n_condition, ]
  expect_identical(low1$cv_status, "insufficient_replicates")
  # sample_order from setup drives the depth-bar order: n_quantified names are a
  # permutation of the sample columns the snapshot ordered.
  expect_setequal(names(cache$n_quantified), fx$sample_order)

  # ---- SEAM 3: volcano df coherence (all-peptide panel) --------------------
  stat_df <- .int_stat_df(syn, cache$matched)
  vdf <- pelsa_build_volcano_df(
    stat_df, cache$matched, fx$feat_df, fx$markers,
    contrast = fx$contrast, opts = list(panel = "all_peptide", sig_cutoff = 0.05)
  )
  # One row per peptide (all-peptide panel = no explode).
  expect_equal(nrow(vdf), n_pep)
  # sig_direction / sig_color consistent with the input adj.P.Val / logFC.
  adjp_in <- syn$peptides[[fx$adjp_col]]
  logfc_in <- syn$peptides[[paste0("logFC.", fx$contrast)]]
  # Align vdf rows to the input by peptide sequence (all-peptide preserves the
  # stat_df row order, but assert on the contract, not the order).
  idx <- match(vdf$id, syn$peptides$PEP.StrippedSequence)
  exp_sig <- !is.na(adjp_in[idx]) & adjp_in[idx] < 0.05
  expect_equal(vdf$Significant, exp_sig)
  expect_equal(vdf$sig_direction[exp_sig & logfc_in[idx] > 0],
               rep("up", sum(exp_sig & logfc_in[idx] > 0)))
  expect_true(all(vdf$sig_color[vdf$sig_direction == "up"] == "darkred"))
  expect_true(all(vdf$sig_color[vdf$sig_direction == "down"] == "#1f4e9c"))
  expect_true(all(vdf$sig_color[vdf$sig_direction == "ns"] == "gray"))
  # y_cutoff is finite when something passes.
  expect_true(any(exp_sig))
  expect_true(is.finite(attr(vdf, "y_cutoff")))
  # is_marker flags the seeded ISOFORM marker peptide (P12345-2 -> ISOPEPTIDEK)
  # AND the TIE-protein marker peptides — via the SAME isoform-base rule used
  # everywhere (pelsa_match_markers).
  expect_true(vdf$is_marker[vdf$id == "ISOPEPTIDEK"])
  expect_true(all(vdf$is_marker[vdf$id %in% syn$tie_peptides]))
  # A clearly-non-marker filler peptide is not flagged.
  expect_false(any(vdf$is_marker[vdf$id == syn$absent_peptide]))

  # ---- SEAM 4: coverage [0,1] + shared peptide + unmatched + missed-cleavage
  cov <- cache$coverage
  cov_vals <- cov$coverage[!is.na(cov$coverage)]
  expect_true(all(cov_vals >= 0 & cov_vals <= 1))
  # The SHARED peptide maps to 3 accessions -> it contributes coverage to EVERY
  # one of them (all three are present with positive coverage).
  shp_accs <- syn$shared_peptide_accessions
  expect_true(all(shp_accs %in% cov$accession))
  expect_true(all(cov$covered_residues[cov$accession %in% shp_accs] ==
                    nchar(syn$shared_peptide)))
  # The FASTA-unmatched peptide (absent from its FASTA) lands in unmatched.
  expect_true(syn$absent_peptide %in% cache$unmatched$peptide_sequence)
  # The seeded missed-cleavage peptide: SHAREDPEPTIDEK has one INTERNAL K
  # (…PEPTIDE-K-…) before the C-terminal K -> exactly 1 missed cleavage.
  pm <- cache$peptide_metrics
  shp_mc <- pm$missed_cleavages[pm$PEP.StrippedSequence == syn$shared_peptide]
  expect_equal(shp_mc, 1L)

  # ---- SEAM 5: intensity-line data for the marker protein -------------------
  prot <- pelsa_intensity_proteins(stat_df, cache$matched, fx$markers,
                                   contrast = fx$contrast, sig_cutoff = 0.05)
  # The TIE marker protein is in the protein set, tagged is_marker.
  expect_true("TIEPROT" %in% prot$accession)
  expect_true(prot$is_marker[prot$accession == "TIEPROT"])

  proc_mat <- cmapR::mat(fx$gct)
  cmap <- syn$condition_map
  corder <- c("AY9944_10uM", "DMSO", "LowN")
  ld <- pelsa_intensity_line_data(
    accession = "TIEPROT", stat_df = stat_df, matched_cache = cache$matched,
    processed_mat = proc_mat, condition_map = cmap, condition_order = corder,
    contrast = fx$contrast, sig_cutoff = 0.05, is_marker = TRUE
  )
  # Marker -> BOTH panels (significant + other), because we pinned one sig + one
  # non-sig TIE peptide.
  expect_setequal(unique(as.character(ld$panel)), c("significant", "other"))
  # aa<pos> labels come from the FASTA-derived pep_start (TIEPROT: TIEPEPONEK@3,
  # TIEPEPTWOK@15) — NOT PEP.PeptidePosition.
  expect_setequal(unique(ld$aa_label), c("aa3", "aa15"))
  # condition factor levels = the requested condition_order (stable x-axis).
  expect_identical(levels(ld$condition), corder)
  # One line per significant peptide-occurrence (TIEPEPONEK once -> 1 sig line).
  sig_lines <- unique(ld[ld$panel == "significant",
                         c("peptide_seq", "pep_occurrence_idx")])
  expect_equal(nrow(sig_lines), 1L)
  expect_identical(unique(sig_lines$peptide_seq), syn$tie_peptides[[1]])

  # ---- SEAM 6: 7F sidecar has EXACTLY 12 columns ----------------------------
  sidecar <- pelsa_volcano_labels_sidecar(vdf, panel = "all_peptide")
  expect_equal(ncol(sidecar), 12L)
  expect_identical(
    colnames(sidecar),
    c("panel", "peptide_sequence", "gene", "accession", "pep_start",
      "display_label", "feature_class_primary", "winning_accession",
      "winning_gene", "logFC", "adj_p", "raw_p")
  )
  expect_equal(nrow(sidecar), n_pep)

  # ---- SEAM 7: exports re-derive from scratch without error -----------------
  ex_vdf <- pelsa_volcano_export_df(
    stat_raw = syn$peptides, matched = cache$matched, feat_df = fx$feat_df,
    markers = fx$markers, contrast = fx$contrast, panel = "all_peptide"
  )
  expect_s3_class(ex_vdf, "data.frame")
  expect_equal(nrow(ex_vdf), n_pep)
  pid <- pelsa_plotted_intensities_df(
    stat_raw = syn$peptides, matched = cache$matched, markers = fx$markers,
    contrast = fx$contrast, pm = proc_mat, cmap = cmap, corder = corder
  )
  expect_s3_class(pid, "data.frame")
  expect_gt(nrow(pid), 0L)
  # The plotted-intensities body carries the full 3C line-data contract.
  expect_true(all(c("accession", "peptide_seq", "pep_start", "aa_label",
                    "panel", "condition", "mean_log2") %in% colnames(pid)))

  # ---- CROSS-HELPER CONTRACT: multilabel seam ------------------------------
  # The multilabel in the volcano df for a given peptide MUST equal what the
  # canonical pelsa_build_multilabel produces for that peptide's mappings in the
  # matched cache (the vectorized volcano labeler and the scalar builder agree).
  for (pep in c(syn$shared_peptide, syn$dup_peptide, "TIEPEPONEK")) {
    m <- cache$matched[cache$matched$PEP.StrippedSequence == pep, , drop = FALSE]
    m <- m[order(m$pep_start, m$accession), , drop = FALSE]
    ref_label <- pelsa_build_multilabel(m$gene, m$pep_start, m$accession)
    got_label <- vdf$label[vdf$id == pep]
    expect_identical(got_label, ref_label,
                     info = paste("multilabel mismatch for", pep))
  }

  # ---- CROSS-HELPER CONTRACT: matched accessions feed coverage + annotation -
  # Every accession the coverage table reports must be an accession the matched
  # cache actually produced (coverage is derived FROM matched). And the
  # annotation frame is the matched cache + feature columns (same row count).
  expect_true(all(cov$accession %in% unique(cache$matched$accession)))
  expect_equal(nrow(cache$annotation), nrow(cache$matched))
  expect_true(all(c("feature_class_primary", "winning_accession",
                    "winning_gene") %in% colnames(cache$annotation)))

  # ---- CROSS-HELPER CONTRACT: isoform-base marker rule is consistent --------
  # The isoform marker P12345-2 flags the ISOPEPTIDEK peptide whose matched
  # accession is stored verbatim as "P12345-2"; the SAME isoform-base rule
  # (pelsa_match_markers) flags it in the volcano df and the matched cache.
  iso_acc_in_matched <- unique(
    cache$matched$accession[cache$matched$PEP.StrippedSequence == "ISOPEPTIDEK"]
  )
  expect_identical(iso_acc_in_matched, syn$isoform_accession)  # "P12345-2"
  expect_true(pelsa_match_markers(iso_acc_in_matched, fx$markers))
  # And base-form marker entry would match the same peptide (symmetric rule).
  expect_true(pelsa_match_markers(iso_acc_in_matched,
                                  syn$isoform_base_accession))
})

# =============================================================================
# DETERMINISM: run the WHOLE pipeline twice -> byte-identical artifacts.
# =============================================================================

test_that("PELSA pipeline is DETERMINISTIC (run-twice identical artifacts)", {
  fx <- .int_build(seed = 11, n_extra = 150)
  syn <- fx$syn

  run_once <- function() {
    res <- pelsa_run_analysis(
      gcts = list(ds = fx$gct), gcts_original = list(ds = fx$gct),
      setup_snapshot = fx$snap, fasta_map = syn$fasta, feat_df = fx$feat_df
    )
    cache <- res$ds
    stat_df <- .int_stat_df(syn, cache$matched)
    vdf <- pelsa_build_volcano_df(
      stat_df, cache$matched, fx$feat_df, fx$markers,
      contrast = fx$contrast, opts = list(panel = "all_peptide")
    )
    proc_mat <- cmapR::mat(fx$gct)
    ld <- pelsa_intensity_line_data(
      "TIEPROT", stat_df, cache$matched, proc_mat, syn$condition_map,
      c("AY9944_10uM", "DMSO", "LowN"), contrast = fx$contrast,
      is_marker = TRUE
    )
    list(cache = cache, vdf = vdf, ld = ld)
  }

  a <- run_once()
  b <- run_once()

  # The cache (rollup tiebreak, mapping order), the volcano df (the data.table
  # group-paste label order + the significance attach), and the intensity line
  # data are all deterministic.
  expect_identical(a$cache, b$cache)
  expect_identical(a$vdf, b$vdf)
  expect_identical(attr(a$vdf, "y_cutoff"), attr(b$vdf, "y_cutoff"))
  expect_identical(a$ld, b$ld)
})
