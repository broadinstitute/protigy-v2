################################################################################
# Tests: PELSA Section 3 (Volcano) — 7A-7C
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

library(testthat)

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

test_that("label-mode: best_per_marker keeps smallest adj.P.Val per marker protein", {
  df <- data.frame(
    is_marker = c(TRUE, TRUE, TRUE, FALSE),
    adj.P.Val = c(0.30, 0.01, 0.40, 0.001),  # P1: rows 1,3 -> keep row1(0.30)?no row with smaller
    winning_accession = c("P1", "P1", "P2", "P3"),
    label = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  # P1 best = row 2 (0.01); P2 best = row 3 (0.40). Non-marker row4 excluded.
  expect_equal(pelsa_volcano_label_rows(df, "best_per_marker"), c(2L, 3L))
})

test_that("label-mode: top_n keeps N smallest adj.P.Val per protein", {
  df <- data.frame(
    is_marker = rep(FALSE, 6),
    adj.P.Val = c(0.5, 0.1, 0.2, 0.9, 0.05, 0.3),
    winning_accession = c("P1", "P1", "P1", "P1", "P2", "P2"),
    label = letters[1:6],
    stringsAsFactors = FALSE
  )
  # P1 top-3 smallest: rows 2(0.1),3(0.2),1(0.5). P2: rows 5(0.05),6(0.3).
  expect_equal(pelsa_volcano_label_rows(df, "top_n", n_top = 3L),
               c(1L, 2L, 3L, 5L, 6L))
  # N=1 per protein: P1 row2, P2 row5.
  expect_equal(pelsa_volcano_label_rows(df, "top_n", n_top = 1L), c(2L, 5L))
})

test_that("label-mode: none labels nothing; all_significant labels sig rows", {
  df <- data.frame(
    is_marker         = c(TRUE, FALSE, FALSE, TRUE),
    Significant       = c(TRUE, FALSE, TRUE, NA),
    adj.P.Val         = c(0.01, 0.5, 0.02, 0.2),
    winning_accession = c("P1", "P2", "P3", "P4"),
    label             = c("a", "b", "c", "d"),
    stringsAsFactors  = FALSE
  )
  expect_equal(pelsa_volcano_label_rows(df, "none"), integer(0))
  # all_significant: rows where Significant == TRUE (NA -> FALSE).
  expect_equal(pelsa_volcano_label_rows(df, "all_significant"), c(1L, 3L))
})

test_that("label-mode: unknown mode errors; default is none", {
  df <- data.frame(is_marker = TRUE, Significant = TRUE, adj.P.Val = 0.01,
                   winning_accession = "P1", label = "a",
                   stringsAsFactors = FALSE)
  expect_error(pelsa_volcano_label_rows(df, "bogus"), "must be one of")
  expect_identical(.PELSA_VOLCANO_DEFAULT_LABEL_MODE, "none")
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
         label_mode = "all_markers", n_top = 3L,
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
         label_mode = "all_markers", n_top = 3L,
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

test_that("current_annotations: mode drives the spec count (none -> empty)", {
  df <- .mk_label_df()

  # "none" yields no labels -> an empty list (an empty relayout clears all).
  expect_identical(
    pelsa_volcano_current_annotations(df, "none", 3L, "significance"), list())

  # "all_significant" labels both significant rows.
  a_sig <- pelsa_volcano_current_annotations(df, "all_significant", 3L,
                                             "significance")
  expect_equal(length(a_sig), 2L)

  # "top_n" with n_top = 1 keeps one label per protein -> both proteins, both
  # rows kept here (one row each).
  a_top <- pelsa_volcano_current_annotations(df, "top_n", 1L, "significance")
  expect_equal(length(a_top), 2L)
  expect_true(all(vapply(a_top, function(a) "text" %in% names(a), logical(1))))

  # An empty df -> empty list (no error).
  expect_identical(
    pelsa_volcano_current_annotations(df[0, ], "top_n", 3L, "significance"),
    list())
})

test_that("current_annotations: feature color-mode drives the border color", {
  df <- .mk_label_df()
  anns <- pelsa_volcano_current_annotations(df, "all_significant", 3L, "feature")
  expect_equal(length(anns), 2L)
  # feature mode -> feature_color border (#d3d3d3 here for both rows).
  expect_true(all(vapply(anns, function(a) a$bordercolor, "") == "#d3d3d3"))
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
  list(species = NULL,  # NULL -> feat_df NULL path (colors "none"); no network
       marker_rows = data.frame(accession = "ACC1", gene = "G1",
                                stringsAsFactors = FALSE))
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
  list(species = NULL,
       marker_rows = data.frame(accession = "ACC1", gene = "G1",
                                stringsAsFactors = FALSE),
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
      top_n_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list())
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
      top_n_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list())
    ),
    {
      # Contrast choices populate (named label -> suffix).
      ch <- contrast_choices()
      expect_equal(unname(ch), c("A_over_B", "A_over_C"))

      # Default active contrast = first; df builds; cache holds ONLY it.
      session$setInputs(pelsa_color_mode = "significance",
                        pelsa_label_mode = "top_n", pelsa_top_n = 3)
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
      # The thin-note output was removed entirely — referencing it now errors.
      expect_error(output$pelsa_thin_note, "hasn't been defined")

      # Plot output exists (renders without error).
      expect_false(is.null(output$pelsa_volcano_plot))
    }
  )
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
      top_n_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list())
    ),
    {
      # cache_entry is NULL; the section-level gate renders the Setup notice.
      expect_null(cache_entry())
      html <- as.character(output$section_contents$html %||%
                             output$section_contents)
      expect_match(html, "Start Analysis", fixed = TRUE)
      # active_volcano_df carries a validate() (the cache guard) — it does not
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
  # The module only draws geom_hline when is.finite(y_cutoff) — Inf -> no line.
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
      top_n_registry = reactiveVal(list()),
      label_mode_registry = reactiveVal(list())
    ),
    {
      expect_null(feat_df())                       # species NULL -> NULL feat
      session$setInputs(pelsa_color_mode = "feature",
                        pelsa_label_mode = "top_n", pelsa_top_n = 3,
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
    top_n_registry = reactiveVal(list()),
    label_mode_registry = reactiveVal(list())
  )
}

test_that("7D: best-panel df built ONLY when the checkbox is ON", {
  shiny::testServer(PELSASection3_Ome_Server, args = .full_args(), {
    session$setInputs(pelsa_color_mode = "significance",
                      pelsa_label_mode = "top_n", pelsa_top_n = 3,
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
                      pelsa_label_mode = "top_n", pelsa_top_n = 3,
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
    # exercised via pinned_line_data() above — accessing the output directly
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
                      pelsa_label_mode = "top_n", pelsa_top_n = 3,
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
                      pelsa_label_mode = "top_n", pelsa_top_n = 3,
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
                      pelsa_label_mode = "top_n", pelsa_top_n = 3,
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
                      pelsa_label_mode = "top_n", pelsa_top_n = 3,
                      pelsa_volcano_contrast = "A_over_B",
                      pelsa_show_best_panel = FALSE)
    df1 <- active_volcano_df()
    expect_equal(names(volcano_df_cache()), "A_over_B")
    expect_setequal(marker_accessions(), "ACC1")

    # Add ACC2 to the marker list (mimics the add-to-marker action upstream).
    # Replace marker_rows wholesale: modifyList() would recurse into the
    # data.frame (a list) and attempt a column-wise merge onto the 1-row frame.
    st_two_markers <- .mk_setup_state_full()
    st_two_markers$marker_rows <- data.frame(
      accession = c("ACC1", "ACC2"), gene = c("G1", "G2"),
      stringsAsFactors = FALSE)
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
  src <- readLines(testthat::test_path("..", "..", "R", "tab_pelsa_section3.R"),
                   warn = FALSE)

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
