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

      # "showing N of M" note: thinned() returns counts; note NULL when nothing
      # thinned (small synthetic frame). Just assert it does not error.
      expect_silent(pelsa_volcano_thin_note(thinned()))

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
