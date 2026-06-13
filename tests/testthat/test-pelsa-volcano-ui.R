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

test_that("thin note: NULL when nothing thinned, string otherwise", {
  expect_null(pelsa_volcano_thin_note(list(n_shown = 100, n_total = 100)))
  note <- pelsa_volcano_thin_note(list(n_shown = 30, n_total = 100))
  expect_true(grepl("30", note) && grepl("100", note))
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

test_that("build_plot returns a plotly object for both source ids", {
  df <- .mk_volcano_df()
  p <- pelsa_volcano_build_plot(df, full_df = df, source_id = "s1")
  expect_s3_class(p, "plotly")
  # With a pinned sibling accession (the fade path) it still builds.
  p2 <- pelsa_volcano_build_plot(df, full_df = df, source_id = "s2",
                                 sibling_acc = "ACC1", register_click = TRUE)
  expect_s3_class(p2, "plotly")
})

test_that("sibling_mask: single-peptide protein -> exactly one TRUE; builds", {
  df <- .mk_volcano_df()  # ACC2 maps to exactly one row (PEPB, row 2)
  m <- pelsa_volcano_sibling_mask(df, "ACC2")
  expect_equal(m$n_siblings, 1L)
  expect_equal(which(m$siblings), 2L)
  # End-to-end: pinning a single-peptide protein builds without error.
  expect_s3_class(
    pelsa_volcano_build_plot(df, full_df = df, source_id = "single",
                             sibling_acc = "ACC2"),
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
    accession = "ACC1", peptide_seq = "PEPA", pep_occurrence_idx = 1L,
    aa_label = "aa10", panel = "significant",
    condition = factor(c("A", "B"), levels = c("A", "B")),
    mean_log2 = c(1, 2), n_rep_nonNA = c(2L, 2L), stringsAsFactors = FALSE)
  expect_s3_class(pelsa_intensity_line_ggplot(one), "ggplot")
  two <- rbind(one, transform(one, panel = "other", mean_log2 = c(3, 4)))
  g <- pelsa_intensity_line_ggplot(two)
  expect_s3_class(g, "ggplot")
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
  list(Proteome = list(matched = matched, annotation = matched))
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
  list(Proteome = list(matched = matched, annotation = matched))
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

    # Simulate the resolved click by setting the pinned reactiveVal directly
    # (event_data() needs a live browser; the resolver itself is unit-tested).
    pinned(list(peptide_seq = "PEPA", accession = "ACC1",
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

test_that("7E: switching contrast CLEARS a stale pin", {
  shiny::testServer(PELSASection3_Ome_Server, args = .full_args(), {
    session$setInputs(pelsa_color_mode = "significance",
                      pelsa_label_mode = "top_n", pelsa_top_n = 3,
                      pelsa_volcano_contrast = "A_over_B")
    force(active_volcano_df())
    pinned(list(peptide_seq = "PEPA", accession = "ACC1",
                label = "G1_aa10", row = 1L))
    expect_false(is.null(pinned()))

    # Switch to the other contrast -> the pin (made under A_over_B coords) clears.
    session$setInputs(pelsa_volcano_contrast = "A_over_C")
    expect_equal(active_contrast(), "A_over_C")
    expect_null(pinned())
    # The intensity line data is gated on a pin, so it no longer computes.
    expect_error(pinned_line_data(), class = "shiny.silent.error")
  })
})

test_that("7F: exports list has the 4 fns and each writes a file", {
  shiny::testServer(PELSASection3_Ome_Server, args = .full_args(), {
    session$setInputs(pelsa_color_mode = "significance",
                      pelsa_label_mode = "top_n", pelsa_top_n = 3,
                      pelsa_volcano_contrast = "A_over_B",
                      pelsa_show_best_panel = FALSE)
    force(active_volcano_df())

    exports <- session$returned
    expect_setequal(names(exports),
                    c("volcano_plot", "proteins_of_interest",
                      "volcano_labels", "plotted_intensities"))
    expect_true(all(vapply(exports, is.function, logical(1))))

    dir <- tempfile("pelsa_export_"); dir.create(dir)
    for (fn in exports) fn(dir)
    files <- list.files(dir)
    expect_true(any(grepl("pelsa_volcano_Proteome\\.pdf$", files)))
    expect_true(any(grepl("pelsa_proteins_of_interest_Proteome\\.csv$", files)))
    expect_true(any(grepl("pelsa_volcano_labels_Proteome\\.csv$", files)))
    expect_true(any(grepl("pelsa_plotted_intensities_Proteome\\.csv$", files)))

    # The 12-col sidecar shape on disk.
    lab <- utils::read.csv(
      file.path(dir, "pelsa_volcano_labels_Proteome.csv"),
      stringsAsFactors = FALSE)
    expect_equal(ncol(lab), 12L)
    expect_true("winning_accession" %in% colnames(lab))
  })
})
