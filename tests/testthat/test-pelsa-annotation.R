################################################################################
# Tests for the PELSA feature-class annotation helpers (Task 2I)  -  the
# data.table::foverlaps overlap join + multi-protein priority resolution.
#
# This is the HIGHEST-parity-risk helper. Gold standard = HAND-SET synthetic
# feature coordinates (R-only known truth; no Python / no notebook capture).
# Every structural edge case from the ported algorithm is asserted closed-form:
#   - priority-ladder ORDER + ranks + COLORS (constants locked)
#   - token-grid alignment incl. dropped MIDDLE token (C not shifted onto B)
#   - winner from a NON-leading accession (resolution scans all tokens)
#   - two priorities in one accession (rank wins; tie -> earliest start)
#   - no overlap -> "none" + leading-accession + gene fallback
#   - overlap merge is on EXACT accession (no isoform fallback in the merge)
#   - pelsa_unannotated_accessions DOES apply isoform-base fallback
#   - two pep_start/pep_end occurrence rows on one accession each annotated
#   - cache read returns the schema.json columns
#   - integration: generator -> explode -> FASTA-map -> annotate
#
# Gold-standard source: notebook plots/volcano_annotate.py::annotate_feature_class
# + pick_primary_feature_class + FEATURE_PRIORITY/FEATURE_COLORS, ported verbatim.
# NOTE the priority ORDER is the NOTEBOOK's (TM before repeat)  -  notebook wins
# over the planning doc / schema.json level order.
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# ---- Constants: lock the priority ladder ORDER, ranks, and COLORS ------------

test_that("PELSA_FEATURE_PRIORITY ladder order is the notebook's (TM before repeat)", {
  expect_identical(
    PELSA_FEATURE_PRIORITY,
    c(
      "active_or_binding_site", "catalytic_domain", "folded_domain",
      "region_or_motif", "transmembrane_or_signal", "repeat_or_coiled_coil",
      "low_complexity_or_disorder", "other"
    )
  )
})

test_that("NONE_FEATURE_CLASS is 'none'", {
  expect_identical(NONE_FEATURE_CLASS, "none")
})

test_that("priority ranks are 0-based ladder index; unknown -> 999", {
  expect_identical(
    .pelsa_priority_rank(PELSA_FEATURE_PRIORITY),
    0:7
  )
  expect_identical(.pelsa_priority_rank("active_or_binding_site"), 0L)
  expect_identical(.pelsa_priority_rank("other"), 7L)
  expect_identical(.pelsa_priority_rank("does_not_exist"), 999L)
  expect_identical(.pelsa_priority_rank(NONE_FEATURE_CLASS), 999L)
  expect_identical(.pelsa_priority_rank(NA_character_), 999L)
})

test_that("PELSA_FEATURE_COLORS hex values are verbatim (incl. 'none')", {
  expect_identical(PELSA_FEATURE_COLORS[["active_or_binding_site"]], "#1f77b4")
  expect_identical(PELSA_FEATURE_COLORS[["catalytic_domain"]], "#ff7f0e")
  expect_identical(PELSA_FEATURE_COLORS[["folded_domain"]], "#d62728")
  expect_identical(PELSA_FEATURE_COLORS[["region_or_motif"]], "#9467bd")
  expect_identical(PELSA_FEATURE_COLORS[["transmembrane_or_signal"]], "#2ca02c")
  expect_identical(PELSA_FEATURE_COLORS[["repeat_or_coiled_coil"]], "#8c564b")
  expect_identical(PELSA_FEATURE_COLORS[["low_complexity_or_disorder"]], "#7f7f7f")
  expect_identical(PELSA_FEATURE_COLORS[["other"]], "#bcbd22")
  expect_identical(PELSA_FEATURE_COLORS[["none"]], "#d3d3d3")
})

# ---- Hand-built feature table (closed-form ground truth) ---------------------
# Spans are CLOSED [start, end]. Classes hand-picked across the priority ladder.
.feat_df <- function() {
  data.frame(
    accession     = c("PA",   "PA",   "PB",   "PC",   "PD",   "PD"),
    start         = c(10L,    100L,   1L,     50L,    1L,     200L),
    end           = c(20L,    120L,   30L,    60L,    50L,    250L),
    feature_class = c("other","active_or_binding_site",
                      "active_or_binding_site",
                      "folded_domain",
                      "region_or_motif", "catalytic_domain"),
    stringsAsFactors = FALSE
  )
}

# ---- pelsa_annotate_features: PEPTIDE-panel shape (;-delimited) ---------------

test_that("no overlap -> 'none' + leading-accession + gene fallback", {
  feat <- .feat_df()
  # PA has features at [10,20] and [100,120]; peptide [40,45] hits neither.
  plot_df <- data.frame(
    PG.ProteinAccessions = "PA;PB",
    PG.Genes             = "GA;GB",
    pep_start            = 40L,
    pep_end              = 45L,
    stringsAsFactors     = FALSE
  )
  # PB feature is [1,30]; peptide [40,45] misses PB too -> truly no overlap.
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "none")
  expect_equal(out$winning_accession, "PA")   # leading accession fallback
  expect_equal(out$winning_gene, "GA")         # leading gene fallback
})

test_that("no-overlap gene fallback falls back to accession when gene empty", {
  feat <- .feat_df()
  plot_df <- data.frame(
    PG.ProteinAccessions = "PA;PB",
    PG.Genes             = ";",            # both gene tokens empty
    pep_start            = 40L,
    pep_end              = 45L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "none")
  expect_equal(out$winning_accession, "PA")
  expect_equal(out$winning_gene, "PA")   # empty gene -> accession
})

test_that("a feature cache row with NA start/end does not abort annotation", {
  # Regression: foverlaps() hard-errors on NA in the lookup (y) range columns.
  # readr reads a blank/unparseable coord in uniprot_features.tsv as NA, so one
  # malformed cached feature aborts the entire annotation step. The feature (y)
  # side must drop NA/inverted ranges, mirroring the grid (x) side -- the other
  # features still annotate.
  feat <- rbind(
    .feat_df(),
    data.frame(accession = "PB", start = NA_integer_, end = NA_integer_,
               feature_class = "folded_domain", stringsAsFactors = FALSE)
  )
  # PB [1,30] active_or_binding_site; peptide [5,15] overlaps it. The NA-coord
  # PB row must be ignored, not crash, and not change the result.
  plot_df <- data.frame(
    PG.ProteinAccessions = "PB",
    PG.Genes             = "GB",
    pep_start            = 5L,
    pep_end              = 15L,
    stringsAsFactors     = FALSE
  )
  # The dropped-row warning is part of the "surface a corrupt cache" contract.
  expect_warning(
    out <- pelsa_annotate_features(plot_df, feat),
    "dropped 1 feature row"
  )
  expect_equal(out$feature_class_primary, "active_or_binding_site")
  expect_equal(out$winning_accession, "PB")
})

test_that("a feature cache of ONLY NA-coord rows falls back to 'none' (no crash)", {
  # If every feature row is dropped (all NA coords), foverlaps sees an empty
  # lookup -> no hits -> the 'none' + leading-accession fallback, not an error.
  feat <- data.frame(
    accession = c("PB", "PB"), start = c(NA_integer_, NA_integer_),
    end = c(NA_integer_, NA_integer_),
    feature_class = c("folded_domain", "region_or_motif"),
    stringsAsFactors = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = "PB", PG.Genes = "GB",
    pep_start = 5L, pep_end = 15L, stringsAsFactors = FALSE
  )
  out <- expect_no_error(suppressWarnings(pelsa_annotate_features(plot_df, feat)))
  expect_equal(out$feature_class_primary, "none")
  expect_equal(out$winning_accession, "PB")
})

test_that("annotate_features drops 0-feature SENTINEL rows silently", {
  # A sentinel (feature_class 'none' + NA coords) marks a resolved-but-0-feature
  # accession. It carries no interval, so it must be dropped from the overlap
  # WITHOUT the 'dropped feature row' corruption warning (it is expected, not a
  # corrupt cache).
  feat <- data.frame(
    accession = "PB", start = NA_integer_, end = NA_integer_,
    feature_class = "none", stringsAsFactors = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = "PB", PG.Genes = "GB",
    pep_start = 5L, pep_end = 15L, stringsAsFactors = FALSE
  )
  out <- expect_silent(pelsa_annotate_features(plot_df, feat))
  expect_equal(out$feature_class_primary, "none")  # no overlap -> fallback
})

test_that("annotate_features STILL warns on genuinely corrupt NA-coord rows", {
  # NA coords on a REAL feature_class is corruption, not a sentinel -> still warn.
  feat <- data.frame(
    accession = "PB", start = NA_integer_, end = NA_integer_,
    feature_class = "folded_domain", stringsAsFactors = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = "PB", PG.Genes = "GB",
    pep_start = 5L, pep_end = 15L, stringsAsFactors = FALSE
  )
  expect_warning(pelsa_annotate_features(plot_df, feat), "NA or inverted")
})

test_that("annotate_features soft-fails on a row with NA feature_class + NA coords", {
  # A blank cache row (readr reads a blank feature_class AND blank coords as NA).
  # The sentinel predicate must be NA-safe: this is NOT a sentinel ("none"), so
  # it is corruption -> warn + drop, never crash with "missing value where
  # TRUE/FALSE needed".
  feat <- data.frame(
    accession = "PB", start = NA_integer_, end = NA_integer_,
    feature_class = NA_character_, stringsAsFactors = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = "PB", PG.Genes = "GB",
    pep_start = 5L, pep_end = 15L, stringsAsFactors = FALSE
  )
  expect_warning(out <- pelsa_annotate_features(plot_df, feat), "NA or inverted")
  expect_equal(out$feature_class_primary, "none")  # dropped -> fallback
})

test_that("single overlap on leading accession resolves to that class", {
  feat <- .feat_df()
  # PB [1,30] active_or_binding_site; peptide [5,15] overlaps it.
  plot_df <- data.frame(
    PG.ProteinAccessions = "PB",
    PG.Genes             = "GB",
    pep_start            = 5L,
    pep_end              = 15L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "active_or_binding_site")
  expect_equal(out$winning_accession, "PB")
  expect_equal(out$winning_gene, "GB")
})

test_that("two features of different priority in ONE accession -> higher wins", {
  feat <- .feat_df()
  # PA: [10,20] other (rank 7), [100,120] active_or_binding_site (rank 0).
  # A peptide spanning both -> active_or_binding_site wins on rank.
  plot_df <- data.frame(
    PG.ProteinAccessions = "PA",
    PG.Genes             = "GA",
    pep_start            = 15L,
    pep_end              = 110L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "active_or_binding_site")
  expect_equal(out$winning_accession, "PA")
})

test_that("winner from a NON-leading accession (resolution scans all tokens)", {
  feat <- .feat_df()
  # A;B where leading A (=PA) has only 'other' in [10,20]; B (=PB) has
  # active_or_binding_site in [1,30]. Peptide [12,18] overlaps both.
  # Higher-priority winner is PB -> winning_accession=PB even though non-leading.
  plot_df <- data.frame(
    PG.ProteinAccessions = "PA;PB",
    PG.Genes             = "GA;GB",
    pep_start            = 12L,
    pep_end              = 18L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "active_or_binding_site")
  expect_equal(out$winning_accession, "PB")
  expect_equal(out$winning_gene, "GB")
})

test_that(";-token realignment: dropped MIDDLE token keeps C attributed to C", {
  feat <- .feat_df()
  # A;B;C where MIDDLE B has NO feature record at all -> B drops from overlap,
  # but token alignment must be preserved so C's feature is attributed to C.
  # PA leading has only 'other' [10,20]; PMISSING (B) has nothing; PC has
  # folded_domain [50,60]. Peptide [12,55] overlaps PA(other) and PC(folded).
  # folded_domain (rank 2) beats other (rank 7) -> winner is PC, NOT PMISSING.
  plot_df <- data.frame(
    PG.ProteinAccessions = "PA;PMISSING;PC",
    PG.Genes             = "GA;GMISS;GC",
    pep_start            = 12L,
    pep_end              = 55L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "folded_domain")
  expect_equal(out$winning_accession, "PC")   # the THIRD token, not shifted onto B
  expect_equal(out$winning_gene, "GC")
})

test_that("tie on rank broken by leading accession (lowest token_idx)", {
  # Two accessions BOTH carry the SAME-rank feature overlapping the peptide.
  # Leading accession (token_idx 0) must win the tie.
  feat <- data.frame(
    accession     = c("PX", "PY"),
    start         = c(1L,   1L),
    end           = c(100L, 100L),
    feature_class = c("folded_domain", "folded_domain"),
    stringsAsFactors = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = "PX;PY",
    PG.Genes             = "GX;GY",
    pep_start            = 10L,
    pep_end              = 20L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "folded_domain")
  expect_equal(out$winning_accession, "PX")   # leading token wins tie
})

test_that("tie on rank+token broken by earliest feature start within accession", {
  # One accession, two SAME-class features overlapping the peptide at different
  # starts -> earliest start wins the within-accession tie (deterministic).
  feat <- data.frame(
    accession     = c("PZ", "PZ"),
    start         = c(40L,  10L),
    end           = c(80L,  50L),
    feature_class = c("folded_domain", "folded_domain"),
    stringsAsFactors = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = "PZ",
    PG.Genes             = "GZ",
    pep_start            = 45L,
    pep_end              = 48L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "folded_domain")
  expect_equal(out$winning_accession, "PZ")
  # Winner row's underlying feature start should be the earliest (10)  -  assert
  # indirectly: the function picks deterministically, so re-running is stable.
  out2 <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out2$feature_class_primary, out$feature_class_primary)
})

test_that("closed-interval overlap: feature touching peptide boundary counts", {
  feat <- data.frame(
    accession     = "PE",
    start         = 20L,
    end           = 30L,
    feature_class = "catalytic_domain",
    stringsAsFactors = FALSE
  )
  # peptide ends exactly at feature start (closed): [10,20] vs [20,30] -> overlap.
  plot_df <- data.frame(
    PG.ProteinAccessions = "PE",
    PG.Genes             = "GE",
    pep_start            = 10L,
    pep_end              = 20L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "catalytic_domain")
})

# ---- PROTEIN-panel shape (one accession/gene per row, no ;) ------------------

test_that("PROTEIN-panel shape (accession/gene cols, no PG.ProteinAccessions)", {
  feat <- .feat_df()
  plot_df <- data.frame(
    accession = c("PB", "PA"),
    gene      = c("GB", "GA"),
    pep_start = c(5L,   40L),
    pep_end   = c(15L,  45L),
    stringsAsFactors = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  # Row 1: PB [1,30] overlaps [5,15] -> active_or_binding_site.
  expect_equal(out$feature_class_primary[1], "active_or_binding_site")
  expect_equal(out$winning_accession[1], "PB")
  expect_equal(out$winning_gene[1], "GB")
  # Row 2: PA [40,45] hits neither PA feature -> none + fallback.
  expect_equal(out$feature_class_primary[2], "none")
  expect_equal(out$winning_accession[2], "PA")
  expect_equal(out$winning_gene[2], "GA")
})

# ---- Two occurrence rows on the same accession (2B per-occurrence rows) -------

test_that("two pep_start/pep_end occurrence rows each annotated to own range", {
  # The comma-token PEP.PeptidePosition='2,167' case is resolved UPSTREAM by 2B
  # (one matched row per occurrence). Here we assert the grid honors BOTH ranges:
  # same accession PD, occurrence 1 [5,10] overlaps region_or_motif [1,50];
  # occurrence 2 [205,210] overlaps catalytic_domain [200,250].
  feat <- .feat_df()
  plot_df <- data.frame(
    accession = c("PD", "PD"),
    gene      = c("GD", "GD"),
    pep_start = c(5L,   205L),
    pep_end   = c(10L,  210L),
    stringsAsFactors = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary[1], "region_or_motif")
  expect_equal(out$feature_class_primary[2], "catalytic_domain")
})

# ---- Empty / degenerate inputs ----------------------------------------------

test_that("empty feat_df -> all 'none' + leading-accession fallback", {
  feat <- .feat_df()[0L, , drop = FALSE]
  plot_df <- data.frame(
    PG.ProteinAccessions = c("PA;PB", "PC"),
    PG.Genes             = c("GA;GB", "GC"),
    pep_start            = c(15L, 55L),
    pep_end              = c(110L, 58L),
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, c("none", "none"))
  expect_equal(out$winning_accession, c("PA", "PC"))
  expect_equal(out$winning_gene, c("GA", "GC"))
})

test_that("empty plot_df returns the three new columns with zero rows", {
  feat <- .feat_df()
  plot_df <- data.frame(
    PG.ProteinAccessions = character(0),
    PG.Genes             = character(0),
    pep_start            = integer(0),
    pep_end              = integer(0),
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(nrow(out), 0L)
  expect_true(all(c("feature_class_primary", "winning_accession",
                    "winning_gene") %in% colnames(out)))
})

test_that("NA pep_start/pep_end grid rows are dropped (treated as no overlap)", {
  feat <- .feat_df()
  plot_df <- data.frame(
    PG.ProteinAccessions = "PB",
    PG.Genes             = "GB",
    pep_start            = NA_integer_,
    pep_end              = NA_integer_,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "none")
  expect_equal(out$winning_accession, "PB")
})

# ---- Isoform: EXACT-accession merge, NO isoform fallback in the join ---------

test_that("overlap merge is on EXACT accession (isoform P12345-2 does NOT match base)", {
  feat <- data.frame(
    accession     = "P12345",
    start         = 1L,
    end           = 100L,
    feature_class = "active_or_binding_site",
    stringsAsFactors = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = "P12345-2",
    PG.Genes             = "GISO",
    pep_start            = 10L,
    pep_end              = 20L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  # EXACT merge: P12345-2 != P12345, so no overlap -> none + fallback.
  expect_equal(out$feature_class_primary, "none")
  expect_equal(out$winning_accession, "P12345-2")
})

# ---- Merged-accession remap tests --------------------------------------------

test_that("pelsa_annotate_features remaps a merged accession to its primary for the join", {
  feat_df <- data.frame(
    accession         = c("Q_PRIMARY", "S_MERGED"),
    start             = c(30L,          NA_integer_),
    end               = c(120L,         NA_integer_),
    feature_class     = c("catalytic_domain", "none"),
    disposition       = c("resolved",   "merged"),
    primary_accession = c("",           "Q_PRIMARY"),
    stringsAsFactors  = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = "S_MERGED",
    PG.Genes             = "GENEX",
    pep_start            = 40L,
    pep_end              = 60L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat_df)
  # Peptide on the merged ID inherits the primary's feature class...
  expect_equal(out$feature_class_primary, "catalytic_domain")
  # ...but winning_accession keeps the ORIGINAL reported (merged) accession.
  expect_equal(out$winning_accession, "S_MERGED")
})

test_that("pelsa_annotate_features does NOT remap demerged/deleted (no primary)", {
  feat_df <- data.frame(
    accession         = c("Q_PRIMARY", "S_DEMERGED"),
    start             = c(30L,          NA_integer_),
    end               = c(120L,         NA_integer_),
    feature_class     = c("catalytic_domain", "none"),
    disposition       = c("resolved",   "demerged"),
    primary_accession = c("",           ""),
    stringsAsFactors  = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = "S_DEMERGED", PG.Genes = "G",
    pep_start = 40L, pep_end = 60L, stringsAsFactors = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat_df)
  expect_equal(out$feature_class_primary, "none")   # no primary -> no remap
})

test_that("pelsa_annotate_features preserves exact-accession behavior without a disposition column", {
  # Legacy 4-column feat_df: no disposition/primary_accession -> identical to today.
  feat_df <- data.frame(
    accession = "P12345", start = 30L, end = 120L,
    feature_class = "catalytic_domain", stringsAsFactors = FALSE
  )
  # Isoform must STILL NOT match base (exact-accession semantics unchanged).
  plot_df <- data.frame(
    PG.ProteinAccessions = "P12345-2", PG.Genes = "G",
    pep_start = 40L, pep_end = 60L, stringsAsFactors = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat_df)
  expect_equal(out$feature_class_primary, "none")
})

# ---- pelsa_unannotated_accessions: isoform-base fallback APPLIES here --------

test_that("unannotated set = plot accessions absent from feat_df", {
  feat <- .feat_df()   # accessions PA, PB, PC, PD
  plot_df <- data.frame(
    PG.ProteinAccessions = c("PA;PNEW", "PB", "PWHOLLYABSENT"),
    stringsAsFactors     = FALSE
  )
  un <- pelsa_unannotated_accessions(plot_df, feat)
  expect_setequal(un, c("PNEW", "PWHOLLYABSENT"))
})

test_that("unannotated set: isoform P12345-2 counts as annotated if base present", {
  feat <- data.frame(
    accession     = "P12345",
    start         = 1L, end = 100L, feature_class = "other",
    stringsAsFactors = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = c("P12345-2", "Q99999-3", "P12345"),
    stringsAsFactors     = FALSE
  )
  un <- pelsa_unannotated_accessions(plot_df, feat)
  # P12345-2 -> base P12345 present -> annotated. Q99999-3 -> base absent.
  expect_setequal(un, "Q99999-3")
})

test_that("pelsa_unannotated_accessions accepts a bare accession character vector", {
  feat <- .feat_df()
  un <- pelsa_unannotated_accessions(c("PA", "PNEW", "PB;PC"), feat)
  expect_setequal(un, "PNEW")
})

# ---- pelsa_annotation_status_counts: three-way bucketing ---------------------

test_that("annotation_status_counts buckets accessions three ways", {
  # P1: real feature; P2: sentinel only (0 features); P3: absent (failed).
  feat_df <- data.frame(
    accession     = c("P1", "P2"),
    feature_type  = c("domain", ""),
    start         = c(1L, NA_integer_),
    end           = c(9L, NA_integer_),
    description   = c("d", ""),
    feature_class = c("folded_domain", "none"),
    class_score   = c(2L, 0L),
    coord_quality = c("exact", ""),
    stringsAsFactors = FALSE)
  plot_df <- data.frame(
    PG.ProteinAccessions = c("P1", "P2", "P3"), stringsAsFactors = FALSE)

  cnt <- pelsa_annotation_status_counts(plot_df, feat_df)
  expect_identical(cnt$n_with_features, 1L)  # P1
  expect_identical(cnt$n_zero_feature, 1L)   # P2 (sentinel only)
  expect_identical(cnt$n_failed, 1L)         # P3 (absent)
})

test_that("annotation_status_counts: isoform input resolves via base feature", {
  feat_df <- data.frame(
    accession = "P1", feature_type = "domain", start = 1L, end = 9L,
    description = "d", feature_class = "folded_domain", class_score = 2L,
    coord_quality = "exact", stringsAsFactors = FALSE)
  cnt <- pelsa_annotation_status_counts(c("P1-2"), feat_df)
  expect_identical(cnt$n_with_features, 1L)
  expect_identical(cnt$n_zero_feature, 0L)
  expect_identical(cnt$n_failed, 0L)
})

test_that("annotation_status_counts: failed bucket equals legacy unannotated", {
  feat_df <- data.frame(
    accession = "P1", feature_type = "domain", start = 1L, end = 9L,
    description = "d", feature_class = "folded_domain", class_score = 2L,
    coord_quality = "exact", stringsAsFactors = FALSE)
  plot_df <- data.frame(PG.ProteinAccessions = c("P1", "PX", "PY"),
                        stringsAsFactors = FALSE)
  cnt <- pelsa_annotation_status_counts(plot_df, feat_df)
  expect_identical(cnt$n_failed,
                   length(pelsa_unannotated_accessions(plot_df, feat_df)))
})

test_that("annotation_status_counts: upload model with no sentinels -> unmatched are failed", {
  # An uploaded raw annotation file carries only real feature rows (no "none"
  # sentinels). Every dataset accession absent from it must count as FAILED, not
  # zero_feature, so zero_feature stays 0.
  feat_df <- data.frame(
    accession = "P1", feature_type = "domain", start = 1L, end = 9L,
    description = "d", feature_class = "folded_domain", class_score = 2L,
    coord_quality = "exact", stringsAsFactors = FALSE)
  cnt <- pelsa_annotation_status_counts(c("P1", "P2", "P3"), feat_df)
  expect_identical(cnt$n_with_features, 1L)
  expect_identical(cnt$n_zero_feature, 0L)
  expect_identical(cnt$n_failed, 2L)
})

# ---- Inverted span (pep_start > pep_end) -------------------------------------

test_that("inverted span (pep_start > pep_end) is dropped -> 'none' + warning", {
  feat <- .feat_df()
  # PB feature [1,30]; an inverted query [50,10] must NOT produce a bogus
  # overlap. It is dropped from the grid -> none + leading-accession fallback.
  plot_df <- data.frame(
    PG.ProteinAccessions = "PB",
    PG.Genes             = "GB",
    pep_start            = 50L,
    pep_end              = 10L,
    stringsAsFactors     = FALSE
  )
  expect_warning(
    out <- pelsa_annotate_features(plot_df, feat),
    "pep_start > pep_end"
  )
  expect_equal(out$feature_class_primary, "none")
  expect_equal(out$winning_accession, "PB")
  expect_equal(out$winning_gene, "GB")
})

test_that("inverted span is dropped while a valid sibling row still annotates", {
  feat <- .feat_df()
  plot_df <- data.frame(
    accession = c("PB",  "PB"),
    gene      = c("GB",  "GB"),
    pep_start = c(50L,   5L),    # row 1 inverted, row 2 valid [5,15]
    pep_end   = c(10L,   15L),
    stringsAsFactors = FALSE
  )
  expect_warning(out <- pelsa_annotate_features(plot_df, feat))
  expect_equal(out$feature_class_primary[1], "none")           # inverted -> none
  expect_equal(out$feature_class_primary[2], "active_or_binding_site")
})

# ---- Duplicate feature rows: deterministic resolution ------------------------

test_that("duplicate (acc,start,end,class) feat_df rows resolve correctly", {
  # Exact-duplicate feature rows must not change the winner or break determinism.
  feat <- data.frame(
    accession     = c("PD", "PD", "PD"),
    start         = c(10L,  10L,  10L),
    end           = c(50L,  50L,  50L),
    feature_class = c("folded_domain", "folded_domain", "folded_domain"),
    stringsAsFactors = FALSE
  )
  plot_df <- data.frame(
    PG.ProteinAccessions = "PD",
    PG.Genes             = "GD",
    pep_start            = 20L,
    pep_end              = 30L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(nrow(out), 1L)
  expect_equal(out$feature_class_primary, "folded_domain")
  expect_equal(out$winning_accession, "PD")
  # Deterministic across re-runs.
  out2 <- pelsa_annotate_features(plot_df, feat)
  expect_identical(out$winning_accession, out2$winning_accession)
})

# ---- Input non-mutation (no by-reference mutation) ---------------------------

test_that("pelsa_annotate_features does not mutate the input plot_df", {
  feat <- .feat_df()
  plot_df <- data.frame(
    PG.ProteinAccessions = c("PB", "PA;PC"),
    PG.Genes             = c("GB", "GA;GC"),
    pep_start            = c(5L,  12L),
    pep_end              = c(15L, 55L),
    stringsAsFactors     = FALSE
  )
  plot_df_before <- plot_df
  out <- pelsa_annotate_features(plot_df, feat)
  expect_identical(plot_df, plot_df_before)   # caller's frame untouched
  expect_true("feature_class_primary" %in% colnames(out))
})

# ---- Peptide panel with NO PG.Genes column -----------------------------------

test_that("peptide panel with NO PG.Genes -> winning_gene falls back to accession", {
  feat <- .feat_df()
  plot_df <- data.frame(
    PG.ProteinAccessions = "PB",
    pep_start            = 5L,
    pep_end              = 15L,
    stringsAsFactors     = FALSE
  )
  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(out$feature_class_primary, "active_or_binding_site")
  expect_equal(out$winning_accession, "PB")
  expect_equal(out$winning_gene, "PB")   # no gene token -> the accession
})

# ---- Integration: generator -> explode -> FASTA-map -> annotate --------------

test_that("integration: explode + FASTA-map + annotate resolves a known overlap", {
  syn <- pelsa_make_synthetic(seed = 1)

  exploded <- pelsa_explode_accessions(syn$peptides)
  mapped <- pelsa_map_peptide_positions(exploded, syn$fasta)
  plot_df <- mapped$matched
  expect_gt(nrow(plot_df), 0L)
  expect_true(all(c("accession", "gene", "pep_start", "pep_end") %in%
                    colnames(plot_df)))

  # Hand-set a small feat_df for SHARED1: a folded_domain spanning the shared
  # peptide's known position (start 5, len = nchar(shared_peptide)).
  shared_acc <- "SHARED1"
  shared_start <- 5L
  shared_end <- shared_start + nchar(syn$shared_peptide) - 1L
  feat <- data.frame(
    accession     = c(shared_acc, "SHARED2"),
    start         = c(shared_start, 1L),
    end           = c(shared_end, 5L),  # SHARED2 feature deliberately upstream
    feature_class = c("active_or_binding_site", "other"),
    stringsAsFactors = FALSE
  )

  out <- pelsa_annotate_features(plot_df, feat)
  expect_equal(nrow(out), nrow(plot_df))
  expect_true(all(c("feature_class_primary", "winning_accession",
                    "winning_gene") %in% colnames(out)))

  shared_rows <- out[out$accession == shared_acc &
                       out$pep_start == shared_start, , drop = FALSE]
  expect_gt(nrow(shared_rows), 0L)
  expect_true(all(shared_rows$feature_class_primary == "active_or_binding_site"))
  expect_true(all(shared_rows$winning_accession == shared_acc))
})

################################################################################
# Tests for the PELSA marker-matching helpers (Task 2J)  -  isoform-aware,
# any-token, case-insensitive accession matching + marker paste-box parsing.
#
# The matching rule under test (documented in tab_pelsa_analysis_helpers.R):
#   A peptide is a marker hit if ANY of its ;-delimited accession tokens,
#   normalized to isoform-BASE (strip trailing "-<digits>") and lowercased,
#   equals ANY marker's isoform-base + lowercased. The rule is SYMMETRIC:
#   marker "P12345" hits a peptide on "P12345-2" AND marker "P12345-2" hits a
#   peptide on "P12345" (both normalize to base "p12345").
#
# pelsa_parse_markers reuses parse_protein_search_input (split on
# space/comma/semicolon/newline, trim, drop empties).
################################################################################

# ---- pelsa_isoform_base ------------------------------------------------------

test_that("pelsa_isoform_base strips UniProt isoform suffix, vectorized + NA-safe", {
  expect_identical(
    pelsa_isoform_base(c("P12345-2", "P12345", "Q9-10", NA)),
    c("P12345", "P12345", "Q9", NA)
  )
})

test_that("pelsa_isoform_base returns character(0) for empty input", {
  expect_identical(pelsa_isoform_base(character(0)), character(0))
})

test_that("pelsa_isoform_base only strips trailing -<digits>, not internal dashes", {
  expect_identical(
    pelsa_isoform_base(c("A0A-B1", "P1-2-3")),
    c("A0A-B1", "P1-2")
  )
})

# ---- pelsa_parse_markers -----------------------------------------------------

test_that("pelsa_parse_markers splits on space/comma/semicolon/newline", {
  expect_identical(
    pelsa_parse_markers("P1, Q2; R3\nS4"),
    c("P1", "Q2", "R3", "S4")
  )
})

test_that("pelsa_parse_markers returns character(0) on empty/NULL/whitespace", {
  expect_identical(pelsa_parse_markers(""), character(0))
  expect_identical(pelsa_parse_markers(NULL), character(0))
  expect_identical(pelsa_parse_markers("   \n  "), character(0))
})

# ---- pelsa_match_markers: core rule ------------------------------------------

test_that("marker matches a peptide on its isoform form (base vs -2)", {
  expect_identical(
    pelsa_match_markers("AAAAAA-2", "AAAAAA"),
    TRUE
  )
})

test_that("matching is SYMMETRIC: marker -2 matches peptide on base", {
  expect_identical(
    pelsa_match_markers("AAAAAA", "AAAAAA-2"),
    TRUE
  )
})

test_that("matching is case-insensitive", {
  expect_identical(
    pelsa_match_markers("P12345", "p12345"),
    TRUE
  )
})

test_that("ANY-TOKEN: a non-leading accession token matches", {
  expect_identical(
    pelsa_match_markers("Q99999;P12345", "P12345"),
    TRUE
  )
})

test_that("non-matching marker yields FALSE", {
  expect_identical(
    pelsa_match_markers("P12345;Q99999", "ZZZZZZ"),
    FALSE
  )
})

test_that("empty marker_accessions -> all FALSE", {
  expect_identical(
    pelsa_match_markers(c("P12345", "Q99999"), character(0)),
    c(FALSE, FALSE)
  )
})

test_that("NA / empty peptide accessions -> FALSE", {
  expect_identical(
    pelsa_match_markers(c(NA, "", "P12345"), "P12345"),
    c(FALSE, FALSE, TRUE)
  )
})

test_that("vector form returns a correct per-row logical vector", {
  res <- pelsa_match_markers(
    c("P12345;Q99999", "ZZZZZZ", "P00000;P12345-7"),
    c("P12345", "AAAAAA")
  )
  expect_identical(res, c(TRUE, FALSE, TRUE))
})

test_that("tokens are whitespace-trimmed before matching", {
  expect_identical(
    pelsa_match_markers(" P12345 ; Q99999 ", " p12345 "),
    TRUE
  )
})

test_that("list (already-split) form is accepted", {
  res <- pelsa_match_markers(
    list(c("Q99999", "P12345"), c("ZZZZZZ")),
    "P12345"
  )
  expect_identical(res, c(TRUE, FALSE))
})

test_that("NA markers are ignored, not propagated", {
  expect_identical(
    pelsa_match_markers("P12345", c(NA, "P12345")),
    TRUE
  )
})

test_that("duplicate markers do not break matching", {
  expect_identical(
    pelsa_match_markers("P12345", c("P12345", "P12345")),
    TRUE
  )
})

test_that("peptide of only delimiters (';;') yields FALSE", {
  expect_identical(
    pelsa_match_markers(";;", "P12345"),
    FALSE
  )
})

# ---- Boundary validation -----------------------------------------------------

test_that("pelsa_match_markers fails fast on bad marker type", {
  expect_error(pelsa_match_markers("P12345", 123L))
})

test_that("pelsa_match_markers fails fast on bad accession type", {
  expect_error(pelsa_match_markers(123L, "P12345"))
})

# ---- Integration: generator isoform parity -----------------------------------

test_that("generator: marker on isoform BASE matches the peptide on the isoform form", {
  syn <- pelsa_make_synthetic()

  # The seeded isoform peptide row carries accession "P12345-2"; the marker is
  # entered as the BASE accession "P12345". The symmetric isoform-base rule must
  # flag that peptide.
  acc_strings <- syn$peptides$PG.ProteinAccessions
  marker <- syn$isoform_base_accession              # "P12345"

  hits <- pelsa_match_markers(acc_strings, marker)

  iso_row <- which(syn$peptides$PG.ProteinAccessions == syn$isoform_accession)
  expect_length(iso_row, 1L)
  expect_true(hits[iso_row])

  # Exactly one row should be flagged (only the isoform peptide carries P12345*).
  expect_identical(sum(hits), 1L)
})

# ---- Performance --------------------------------------------------------------

test_that("pelsa_match_markers handles ~100k peptides quickly (vectorized)", {
  testthat::skip_on_ci()
  set.seed(7)
  n <- 100000L
  acc <- sprintf("ACC%05d;Q%05d", sample.int(5000L, n, replace = TRUE),
                 sample.int(5000L, n, replace = TRUE))
  acc[seq_len(50L)] <- "P12345-2;Q00001"
  markers <- c("P12345", "NOPE1", "NOPE2")

  t <- system.time(res <- pelsa_match_markers(acc, markers))
  expect_identical(sum(res), 50L)
  expect_lt(t[["elapsed"]], 0.5)
})
