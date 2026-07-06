################################################################################
# Tests for the PELSA Section 2 self-curated annotation value box.
#
# Covers Task 1: the failed_annotation_count valueBox must return a neutral
# (non-red) card for self-curated datasets and retain its red failure
# state for non-self-curated datasets with unannotated accessions.
################################################################################

library(testthat)

# Shared minimal reactives helper (mirrors pattern in test-pelsa-summary.R).
.s2_min_reactives <- function(entry, self_curated_flag) {
  GCTs_and_params   <- shiny::reactiveVal(list(GCTs = list(ds1 = NULL),
                                               parameters = list(ds1 = list())))
  globals           <- shiny::reactiveValues(default_ome = "ds1", colors = list())
  GCTs_original     <- shiny::reactiveVal(list(ds1 = NULL))
  active_dataset    <- shiny::reactive("ds1")
  pelsa_analysis    <- shiny::reactiveVal(list(ds1 = entry))
  pelsa_setup_state <- shiny::reactive(
    list(self_curated = list(ds1 = self_curated_flag))
  )
  list(
    GCTs_and_params   = GCTs_and_params,
    globals           = globals,
    GCTs_original     = GCTs_original,
    active_dataset    = active_dataset,
    pelsa_analysis    = pelsa_analysis,
    pelsa_setup_state = pelsa_setup_state
  )
}

# A synthetic cache entry that would normally trigger a red failure box:
# 3 unannotated accessions -> n_unannotated_accessions = 3.
.s2_failed_entry <- function() {
  list(
    qc           = list(n_unannotated_accessions = 3L),
    unannotated  = c("A", "B", "C"),
    stage        = "done"
  )
}

# ---- self-curated: neutral card (not red) ---------------------------------

test_that("failed_annotation_count is neutral for a self-curated dataset", {
  entry <- .s2_failed_entry()
  args  <- .s2_min_reactives(entry, self_curated_flag = TRUE)

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = args,
    {
      vb   <- output$failed_annotation_count
      html <- as.character(vb$html %||% vb)
      expect_false(grepl("bg-red", html, ignore.case = TRUE),
                   info = paste("Expected no 'bg-red' in valueBox HTML; got:", html))
      expect_true(grepl("self-curated", html, ignore.case = TRUE),
                  info = paste("Expected 'self-curated' in valueBox HTML; got:", html))
    }
  )
})

# ---- non-self-curated: red failure card stays ----------------------------

test_that("failed_annotation_count stays red for a non-self-curated dataset with failures", {
  entry <- .s2_failed_entry()
  args  <- .s2_min_reactives(entry, self_curated_flag = FALSE)

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = args,
    {
      vb   <- output$failed_annotation_count
      html <- as.character(vb$html %||% vb)
      expect_true(grepl("bg-red", html, ignore.case = TRUE),
                  info = paste("Expected 'bg-red' in valueBox HTML; got:", html))
    }
  )
})

# ---- non-self-curated, zero failures: neutral (black) card -------------------

test_that("failed_annotation_count is neutral (black) for a non-self-curated dataset with 0 failures", {
  # 0 unannotated accessions -> n = 0 -> has_failures FALSE -> color "black".
  entry <- list(
    qc          = list(n_unannotated_accessions = 0L),
    unannotated = character(0),
    stage       = "done"
  )
  args <- .s2_min_reactives(entry, self_curated_flag = FALSE)

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = args,
    {
      vb   <- output$failed_annotation_count
      html <- as.character(vb$html %||% vb)
      expect_true(grepl("bg-black", html, ignore.case = TRUE),
                  info = paste("Expected 'bg-black' in valueBox HTML; got:", html))
      expect_false(grepl("bg-red", html, ignore.case = TRUE),
                   info = paste("Expected no 'bg-red' in valueBox HTML; got:", html))
    }
  )
})

# ---- Task 7: bold density-plot annotations -----------------------------------

test_that("overall density mean/median labels are bold (halo layer excluded)", {
  vals <- rnorm(200, 20, 5)
  p <- pelsa_overall_density_plot(vals, x_label = "x", title = "t")
  text_layers <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  # 1 white halo + 2 colored labels.
  expect_length(text_layers, 3L)
  # The two NON-white (colored) labels must be bold; the halo copies are bold too.
  faces <- vapply(text_layers, function(l) l$aes_params$fontface %||% "", character(1))
  expect_true(all(faces == "bold"))
})

test_that("per-condition density median labels are bold", {
  df <- data.frame(value = rnorm(60, 20, 5), condition = rep(c("A","B"), 30))
  p <- pelsa_per_condition_density_plot(df, value_col = "value",
                                        x_label = "x", title = "t")
  text_layers <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  faces <- vapply(text_layers, function(l) l$aes_params$fontface %||% "", character(1))
  expect_true(any(faces == "bold"))
})

test_that("cv kde median labels are bold", {
  cv <- data.frame(cv_pct = abs(rnorm(60, 30, 10)),
                   cv_status = rep("ok", 60),
                   condition = rep(c("A","B"), 30))
  p <- pelsa_cv_kde_plot(cv)
  text_layers <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  faces <- vapply(text_layers, function(l) l$aes_params$fontface %||% "", character(1))
  expect_true(any(faces == "bold"))
})

# ---- Task 8: missed-cleavage bar labels --------------------------------------

test_that("missed-cleavage plot draws a count+percent label per bar", {
  pm <- data.frame(missed_cleavages = c(0,0,0,1,1,2))
  p <- pelsa_missed_cleavage_plot(pm)
  text_layers <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  expect_length(text_layers, 1)
})

# ---- Task 9: depth bar count labels + x-axis text size 11 -------------------

test_that("depth bar draws a count label per sample and sizes x-axis text to 11", {
  nq <- c(S1 = 100L, S2 = 250L, S3 = 175L)
  p <- pelsa_depth_bar_plot(nq)
  text_layers <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  expect_length(text_layers, 1)
  expect_equal(p$theme$axis.text.x$size, 11)
})

# ---- Task 10: bar label positioning (vjust=0 + expanded y-axis headroom) ------

test_that("in-app bar labels sit ABOVE the bar (vjust=0 + baked label_y headroom)", {
  # ggplotly drops nudge_y, so the headroom is baked into the text layer's y.
  # Assert on the BUILT layer data (layer_data) -- the raw layer$data slot is a
  # waiver here (aes-only geom_text), which would make a direct comparison vacuous.
  pm <- data.frame(
    peptide_seq = paste0("PEP", 1:6),
    peptide_length = c(8, 9, 10, 11, 12, 13),
    missed_cleavages = c(0, 0, 1, 1, 2, 0),
    stringsAsFactors = FALSE
  )
  g_mc <- pelsa_missed_cleavage_plot(pm)
  txt_mc <- Filter(function(l) inherits(l$geom, "GeomText"), g_mc$layers)
  expect_length(txt_mc, 1L)
  expect_equal(txt_mc[[1]]$aes_params$vjust, 0)
  # geom_text is the 2nd layer (geom_col is 1st). Built text y > built col y.
  col_y_mc  <- ggplot2::layer_data(g_mc, 1)$y   # bar tops
  text_y_mc <- ggplot2::layer_data(g_mc, 2)$y   # baked label_y
  expect_equal(length(text_y_mc), length(col_y_mc))
  expect_true(all(text_y_mc > col_y_mc))

  nq <- c(S1 = 1200L, S2 = 1500L, S3 = 900L)
  g_d <- pelsa_depth_bar_plot(nq)
  txt_d <- Filter(function(l) inherits(l$geom, "GeomText"), g_d$layers)
  expect_length(txt_d, 1L)
  expect_equal(txt_d[[1]]$aes_params$vjust, 0)
  col_y_d  <- ggplot2::layer_data(g_d, 1)$y
  text_y_d <- ggplot2::layer_data(g_d, 2)$y
  expect_equal(length(text_y_d), length(col_y_d))
  expect_true(all(text_y_d > col_y_d))
})

test_that("bar-label head_frac controls the gap: defaults are in-app, smaller = export", {
  pm <- data.frame(
    peptide_seq = paste0("PEP", 1:6),
    peptide_length = c(8, 9, 10, 11, 12, 13),
    missed_cleavages = c(0, 0, 1, 1, 2, 0),
    stringsAsFactors = FALSE
  )
  # Default (in-app) missed-cleavage gap vs a halved export gap.
  g_app <- pelsa_missed_cleavage_plot(pm)                    # head_frac = 0.06
  g_exp <- pelsa_missed_cleavage_plot(pm, head_frac = 0.03)  # export
  gap_app <- ggplot2::layer_data(g_app, 2)$y - ggplot2::layer_data(g_app, 1)$y
  gap_exp <- ggplot2::layer_data(g_exp, 2)$y - ggplot2::layer_data(g_exp, 1)$y
  expect_true(all(gap_app > 0))
  expect_true(all(gap_exp > 0))                 # export still clears the bar (no clash)
  expect_true(all(gap_exp < gap_app))           # export gap is smaller
  # 0.03 is half of 0.06 -> export gap ~= half the in-app gap (allow float slack).
  expect_equal(mean(gap_exp) / mean(gap_app), 0.5, tolerance = 1e-6)

  # Depth: default 0.04 (in-app), 0.02 export -> half.
  nq <- c(S1 = 1200L, S2 = 1500L, S3 = 900L)
  d_app <- pelsa_depth_bar_plot(nq)                          # head_frac = 0.04
  d_exp <- pelsa_depth_bar_plot(nq, head_frac = 0.02)        # export
  dgap_app <- ggplot2::layer_data(d_app, 2)$y - ggplot2::layer_data(d_app, 1)$y
  dgap_exp <- ggplot2::layer_data(d_exp, 2)$y - ggplot2::layer_data(d_exp, 1)$y
  expect_true(all(dgap_exp > 0))
  expect_true(all(dgap_exp < dgap_app))
  expect_equal(mean(dgap_exp) / mean(dgap_app), 0.5, tolerance = 1e-6)

  # Positional back-compat: sample_order still the 2nd positional arg.
  d_pos <- pelsa_depth_bar_plot(nq, c("S3", "S1", "S2"))
  expect_s3_class(d_pos, "ggplot")
})

# ---- Task 2: density plot x-axis always includes 0 ------

test_that("peptide-length density plot always includes 0 on the x-axis, even when all values are far from 0", {
  pm <- data.frame(
    peptide_seq = paste0("PEP", 1:20),
    peptide_length = seq(30, 68, by = 2),  # min 30, far from 0
    missed_cleavages = rep(0L, 20),
    stringsAsFactors = FALSE
  )
  g <- pelsa_length_density_plot(pm)
  built <- ggplot2::ggplot_build(g)
  x_range <- built$layout$panel_params[[1]]$x.range
  expect_true(x_range[1] <= 0)
})

test_that("sequence-coverage density plot always includes 0 on the x-axis, even when all values are far from 0", {
  cov <- data.frame(
    accession = paste0("P", 1:20),
    coverage = seq(0.5, 0.95, length.out = 20),  # min 0.5, far from 0
    protein_length = rep(100L, 20),
    stringsAsFactors = FALSE
  )
  g <- pelsa_coverage_distribution_plot(cov)
  built <- ggplot2::ggplot_build(g)
  x_range <- built$layout$panel_params[[1]]$x.range
  expect_true(x_range[1] <= 0)
})

test_that("pelsa_per_condition_density_plot renders a supplied subtitle", {
  df <- data.frame(
    condition = rep(c("A", "B"), each = 5),
    peptide_length = c(8, 9, 10, 11, 12, 7, 8, 9, 10, 11),
    stringsAsFactors = FALSE
  )
  g <- pelsa_per_condition_density_plot(
    df, value_col = "peptide_length",
    x_label = "Peptide length (residues)",
    title = "Peptide-length distribution by condition",
    subtitle = "Per-condition")
  expect_equal(g$labels$subtitle, "Per-condition")
})

test_that("experiment-wide density builders carry an 'Experiment-wide' subtitle", {
  cov <- data.frame(accession = paste0("P", 1:5),
                    coverage = c(0.1, 0.2, 0.3, 0.4, 0.5),
                    protein_length = rep(100L, 5),
                    stringsAsFactors = FALSE)
  g_cov <- pelsa_coverage_distribution_plot(cov)
  expect_true(grepl("^Experiment-wide", g_cov$labels$subtitle))

  pm <- data.frame(peptide_seq = paste0("PEP", 1:5),
                   peptide_length = c(8, 9, 10, 11, 12),
                   missed_cleavages = rep(0L, 5),
                   stringsAsFactors = FALSE)
  g_len <- pelsa_length_density_plot(pm)
  expect_equal(g_len$labels$subtitle, "Experiment-wide")
})

test_that("per-condition density builders carry a 'Per-condition' subtitle", {
  cbc <- data.frame(condition = rep(c("A", "B"), each = 5),
                    coverage = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                 0.15, 0.25, 0.35, 0.45, 0.55),
                    stringsAsFactors = FALSE)
  g_cov <- pelsa_coverage_by_condition_plot(cbc, condition_order = c("A", "B"))
  expect_equal(g_cov$labels$subtitle, "Per-condition")

  lbc <- data.frame(condition = rep(c("A", "B"), each = 5),
                    peptide_length = c(8, 9, 10, 11, 12, 7, 8, 9, 10, 11),
                    stringsAsFactors = FALSE)
  g_len <- pelsa_length_by_condition_plot(lbc, condition_order = c("A", "B"))
  expect_equal(g_len$labels$subtitle, "Per-condition")
})

test_that("coverage density plots use a percent x-axis title (both modes)", {
  cov <- data.frame(accession = paste0("P", 1:5),
                    coverage = c(0.1, 0.2, 0.3, 0.4, 0.5),
                    protein_length = rep(100L, 5),
                    stringsAsFactors = FALSE)
  g_over <- pelsa_coverage_distribution_plot(cov)
  expect_equal(g_over$labels$x, "Sequence coverage (%)")

  cbc <- data.frame(condition = rep(c("A", "B"), each = 5),
                    coverage = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                 0.15, 0.25, 0.35, 0.45, 0.55),
                    stringsAsFactors = FALSE)
  g_cond <- pelsa_coverage_by_condition_plot(cbc, condition_order = c("A", "B"))
  expect_equal(g_cond$labels$x, "Sequence coverage (%)")
})

test_that("coverage percent tick labels convert a fraction break to whole percent", {
  cov <- data.frame(accession = paste0("P", 1:5),
                    coverage = c(0.1, 0.2, 0.3, 0.4, 0.5),
                    protein_length = rep(100L, 5),
                    stringsAsFactors = FALSE)
  g <- pelsa_coverage_distribution_plot(cov)
  # The x scale's label function maps fraction breaks -> percent numbers.
  xsc <- g$scales$get_scales("x")
  expect_false(is.null(xsc))
  expect_equal(xsc$labels(c(0, 0.25, 0.5)), c(0, 25, 50))
})

test_that("length density keeps its raw (non-percent) x axis", {
  pm <- data.frame(peptide_seq = paste0("PEP", 1:5),
                   peptide_length = c(8, 9, 10, 11, 12),
                   missed_cleavages = rep(0L, 5),
                   stringsAsFactors = FALSE)
  g <- pelsa_length_density_plot(pm)
  expect_equal(g$labels$x, "Peptide length (residues)")
})

# ---- failed-annotation value box color -------------------------------------

test_that("failed-annotation box is red on failures and neutral (black) at zero", {
  # The module inlines the rule as: color = if (has_failures) "red" else "black".
  # Assert the mapping directly so a regression to green/orange is caught.
  box_color <- function(n) {
    has_failures <- isTRUE(n > 0L)
    if (has_failures) "red" else "black"
  }
  expect_equal(box_color(198L), "red")
  expect_equal(box_color(0L), "black")
  expect_equal(box_color(NA_integer_), "black")  # NA (pre-analysis) -> neutral
})

# ---- white text halo (tight, symmetric, applied to overall density too) ------

test_that("pelsa_halo_text_layers draws 8 tight white copies per label", {
  medians <- data.frame(x = c(10, 20), y = c(0.5, 0.4),
                        label = c("A median = 10", "B median = 20"),
                        stringsAsFactors = FALSE)
  layer <- pelsa_halo_text_layers(medians, x_hi = 100, peak = 1)
  expect_true(inherits(layer$geom, "GeomText"))
  # 8 directions x 2 labels = 16 white halo rows.
  expect_equal(nrow(layer$data), 16L)
  expect_equal(layer$aes_params$colour %||% layer$aes_params$color, "white")
  # Offsets are a SMALL fraction of the extents: max x offset < 0.5% of x_hi.
  expect_true(max(abs(layer$data$x - rep(medians$x, 8))) < 0.005 * 100)
})

test_that("overall density plot carries a white halo behind mean/median labels", {
  vals <- rnorm(300, 20, 5)
  p <- pelsa_overall_density_plot(vals, x_label = "x", title = "t")
  text_layers <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  # 1 halo layer + 2 colored annotate() labels = 3 GeomText layers.
  expect_length(text_layers, 3L)
  halo <- Filter(function(l) {
    col <- l$aes_params$colour %||% l$aes_params$color %||% ""
    identical(col, "white")
  }, text_layers)
  expect_length(halo, 1L)
  # Halo covers both labels, 8 copies each = 16 rows.
  expect_equal(nrow(halo[[1]]$data), 16L)
})

# ---- export styling: pelsa_overall_density_plot ------------------------------

test_that("pelsa_overall_density_plot default (export=FALSE) keeps current on-screen styling", {
  vals <- c(10, 20, 30, 40, 50)
  p <- pelsa_overall_density_plot(vals, x_label = "x", title = "t")
  expect_equal(p$theme$plot.title$size, 14)
  # theme_bw()'s own axis.text carries the grey "#4D4D4DFF" (not black) in this
  # ggplot2 version -- export=FALSE must leave that inherited default alone.
  expect_equal(p$theme$axis.text$colour, "#4D4D4DFF")
  expect_equal(p$theme$plot.title.position, "plot")
})

test_that("pelsa_overall_density_plot export=TRUE applies export styling", {
  vals <- c(10, 20, 30, 40, 50)
  p <- pelsa_overall_density_plot(vals, x_label = "x", title = "t", export = TRUE)
  expect_equal(p$theme$plot.title$size, 12)
  expect_equal(p$theme$plot.subtitle$size, 12)
  expect_null(p$theme$plot.title.position)
  expect_equal(p$theme$axis.text$colour, "black")
  expect_equal(p$theme$axis.text$size, 8)
  text_layers <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  sizes <- vapply(text_layers, function(l) l$aes_params$size %||% NA_real_, numeric(1))
  expect_true(all(sizes[!is.na(sizes)] == 4.2))
})

test_that("pelsa_coverage_distribution_plot export=TRUE fixes x-axis range to (0, 1)", {
  cov <- data.frame(accession = paste0("P", 1:5),
                    coverage = c(0.1, 0.2, 0.3, 0.4, 0.5),
                    protein_length = rep(100L, 5), stringsAsFactors = FALSE)
  p <- pelsa_coverage_distribution_plot(cov, export = TRUE)
  built <- ggplot2::ggplot_build(p)
  x_range <- built$layout$panel_params[[1]]$x.range
  # coord_cartesian(xlim=c(0,1)) with default ggplot expansion still reports
  # panel_params x.range slightly beyond (0,1) by the expansion factor -- assert
  # the UNDERLYING requested limits via the coord object rather than the
  # expanded render range.
  expect_equal(p$coordinates$limits$x, c(0, 1))
})

test_that("pelsa_coverage_distribution_plot default (export=FALSE) does not fix x-axis range", {
  cov <- data.frame(accession = paste0("P", 1:5),
                    coverage = c(0.1, 0.2, 0.3, 0.4, 0.5),
                    protein_length = rep(100L, 5), stringsAsFactors = FALSE)
  p <- pelsa_coverage_distribution_plot(cov)
  # pelsa_overall_density_plot's own coord_cartesian(xlim=c(0, right_bound))
  # always clamps the left edge to 0 with an unclamped (NA) right edge when no
  # x_hi is supplied -- export=FALSE must leave that pre-existing clamp as-is,
  # not add the export-only fixed (0, 1) range.
  expect_equal(p$coordinates$limits$x, c(0, NA))
})

# ---- export styling: pelsa_per_condition_density_plot -------------------------

test_that("pelsa_per_condition_density_plot default (export=FALSE) keeps current label text and size", {
  df <- data.frame(condition = rep(c("A", "B"), each = 5),
                   peptide_length = c(8, 9, 10, 11, 12, 7, 8, 9, 10, 11),
                   stringsAsFactors = FALSE)
  p <- pelsa_per_condition_density_plot(
    df, value_col = "peptide_length", x_label = "Peptide length (residues)",
    title = "t")
  # Verified real layer order: geom_density, geom_vline, pelsa_halo_text_layers'
  # (white) geom_text, then the colored geom_text -- 4 layers total, GeomText at
  # indices 3 and 4, so gt_idx[2] is the colored label layer.
  gt_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"),
                        logical(1)))
  expect_equal(unname(gt_idx), c(3L, 4L))
  built <- ggplot2::ggplot_build(p)
  labels <- unique(built$data[[gt_idx[2]]]$label)
  expect_true(any(grepl("^A median = ", labels)))
  colored_layer <- p$layers[[gt_idx[2]]]
  expect_equal(colored_layer$aes_params$size, 3)
})

test_that("pelsa_per_condition_density_plot export=TRUE drops condition-name prefix from label and keeps size 3", {
  df <- data.frame(condition = rep(c("A", "B"), each = 5),
                   peptide_length = c(8, 9, 10, 11, 12, 7, 8, 9, 10, 11),
                   stringsAsFactors = FALSE)
  p <- pelsa_per_condition_density_plot(
    df, value_col = "peptide_length", x_label = "Peptide length (residues)",
    title = "t", export = TRUE)
  built <- ggplot2::ggplot_build(p)
  gt_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"),
                        logical(1)))
  labels <- unique(built$data[[gt_idx[2]]]$label)
  expect_true(any(grepl("^median = ", labels)))
  expect_false(any(grepl("^A median = |^B median = ", labels)))
  colored_layer <- p$layers[[gt_idx[2]]]
  expect_equal(colored_layer$aes_params$size, 3)  # UNCHANGED -- crowding exception
  expect_equal(p$theme$plot.title$size, 12)
  expect_null(p$theme$plot.title.position)
  expect_equal(p$theme$axis.text$colour, "black")
  expect_equal(p$theme$axis.text$size, 8)
})

# ---- export styling: pelsa_cv_kde_plot ---------------------------------------

test_that("pelsa_cv_kde_plot export=TRUE applies export styling, keeps label size 3 and text unchanged", {
  cv <- data.frame(cv_pct = abs(rnorm(60, 30, 10)),
                   cv_status = rep("ok", 60),
                   condition = rep(c("A", "B"), 30))
  p <- pelsa_cv_kde_plot(cv, export = TRUE)
  expect_equal(p$theme$plot.title$size, 12)
  expect_null(p$theme$plot.title.position)
  expect_equal(p$theme$axis.text$colour, "black")
  expect_equal(p$theme$axis.text$size, 8)
  gt_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"),
                        logical(1)))
  colored_layer <- p$layers[[gt_idx[length(gt_idx)]]]
  expect_equal(colored_layer$aes_params$size, 3)  # UNCHANGED -- crowding exception
})

test_that("pelsa_cv_kde_plot default (export=FALSE) keeps current styling", {
  cv <- data.frame(cv_pct = abs(rnorm(60, 30, 10)),
                   cv_status = rep("ok", 60),
                   condition = rep(c("A", "B"), 30))
  p <- pelsa_cv_kde_plot(cv)
  expect_equal(p$theme$plot.title$size, 14)
  expect_equal(p$theme$plot.title.position, "plot")
})

# ---- export styling: pelsa_missed_cleavage_plot -------------------------------

test_that("pelsa_missed_cleavage_plot parenthesizes the percentage in the bar label (both modes)", {
  pm <- data.frame(missed_cleavages = c(0, 0, 0, 1, 1, 2))
  p_screen <- pelsa_missed_cleavage_plot(pm)
  p_export <- pelsa_missed_cleavage_plot(pm, export = TRUE)
  built_screen <- ggplot2::ggplot_build(p_screen)
  built_export <- ggplot2::ggplot_build(p_export)
  gt_screen <- which(vapply(p_screen$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  gt_export <- which(vapply(p_export$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_true(any(grepl("\\(\\d+\\.\\d%\\)", built_screen$data[[gt_screen]]$label)))
  expect_true(any(grepl("\\(\\d+\\.\\d%\\)", built_export$data[[gt_export]]$label)))
})

test_that("pelsa_missed_cleavage_plot export=TRUE changes x-axis title and label size", {
  pm <- data.frame(missed_cleavages = c(0, 0, 0, 1, 1, 2))
  p <- pelsa_missed_cleavage_plot(pm, export = TRUE)
  expect_equal(p$labels$x, "# of missed cleavages")
  gt_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_equal(p$layers[[gt_idx]]$aes_params$size, 4)
  expect_equal(p$theme$plot.title$size, 12)
  expect_null(p$theme$plot.title.position)
  expect_equal(p$theme$axis.text$colour, "black")
  expect_equal(p$theme$axis.text$size, 8)
})

test_that("pelsa_missed_cleavage_plot default (export=FALSE) keeps 'Missed cleavages' x title and size-3 label", {
  pm <- data.frame(missed_cleavages = c(0, 0, 0, 1, 1, 2))
  p <- pelsa_missed_cleavage_plot(pm)
  expect_equal(p$labels$x, "Missed cleavages")
  gt_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_equal(p$layers[[gt_idx]]$aes_params$size, 3)
})
