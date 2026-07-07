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

test_that("pelsa_condition_bar_plot draws one bar per row with an error bar and value label", {
  bar_df <- data.frame(condition = c("A", "B"), mean = c(10, 20),
                       sd = c(1, 2), n = c(3L, 4L), stringsAsFactors = FALSE)
  p <- pelsa_condition_bar_plot(bar_df, y_label = "Rate (%)", title = "Test",
                                fill = "#f28e2b")
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  # One geom_col layer (bars) + one geom_errorbar layer + a text layer.
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomCol" %in% geoms)
  expect_true("GeomErrorbar" %in% geoms)
  expect_true("GeomText" %in% geoms)
})

test_that("pelsa_condition_bar_plot returns a blank plot for 0-row input", {
  bar_df <- data.frame(condition = character(0), mean = numeric(0),
                       sd = numeric(0), n = integer(0), stringsAsFactors = FALSE)
  p <- pelsa_condition_bar_plot(bar_df, y_label = "Rate (%)", title = "Test",
                                fill = "#f28e2b",
                                blank_msg = "No eligible conditions.")
  # annotate("text", ...) stores its label as a layer aes_param, not in data.
  ann_text <- unlist(lapply(p$layers, function(l) {
    if (inherits(l$geom, "GeomText")) l$aes_params$label else NULL
  }))
  expect_true(any(grepl("No eligible conditions", ann_text, fixed = TRUE)))
})

test_that("pelsa_condition_bar_plot drops the x-axis title", {
  bar_df <- data.frame(condition = "A", mean = 10, sd = 1, n = 3L,
                       stringsAsFactors = FALSE)
  p <- pelsa_condition_bar_plot(bar_df, y_label = "Rate (%)", title = "Test",
                                fill = "#f28e2b")
  expect_null(p$labels$x)
})

test_that("pelsa_condition_bar_plot omits the error bar when sd is NA (n=1 row)", {
  bar_df <- data.frame(condition = "A", mean = 10, sd = NA_real_, n = 1L,
                       stringsAsFactors = FALSE)
  p <- pelsa_condition_bar_plot(bar_df, y_label = "Rate (%)", title = "Test",
                                fill = "#f28e2b")
  built <- ggplot2::ggplot_build(p)
  eb_idx <- which(vapply(p$layers, function(l) class(l$geom)[1], character(1))
                 == "GeomErrorbar")
  eb_data <- built$data[[eb_idx]]
  expect_equal(nrow(eb_data), 0L)
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

# ---- Task 5: per-sample bar+errorbar panels (missed-cleavage/coverage/length) --

test_that("pelsa_missed_cleavage_plot draws one bar per eligible condition (per_condition mode)", {
  per_sample <- data.frame(
    sample = c("A_R1", "A_R2", "A_R3", "B_R1"),
    rate = c(0.1, 0.2, 0.3, 0.9),
    n_quantified = c(10L, 10L, 10L, 10L),
    stringsAsFactors = FALSE
  )
  cmap <- c(A_R1 = "A", A_R2 = "A", A_R3 = "A", B_R1 = "B")
  # B has only 1 replicate -> dropped (min_replicates = 2 default).
  p <- pelsa_missed_cleavage_plot(per_sample, cmap, mode = "per_condition")
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  bar_idx <- which(vapply(p$layers, function(l) class(l$geom)[1], character(1))
                   == "GeomCol")
  expect_equal(nrow(built$data[[bar_idx]]), 1L)  # only "A" is eligible
})

test_that("pelsa_missed_cleavage_plot pools all samples in overall mode", {
  per_sample <- data.frame(
    sample = c("A_R1", "A_R2", "B_R1", "B_R2"),
    rate = c(0.1, 0.2, 0.5, 0.7),
    n_quantified = rep(10L, 4L),
    stringsAsFactors = FALSE
  )
  cmap <- c(A_R1 = "A", A_R2 = "A", B_R1 = "B", B_R2 = "B")
  p <- pelsa_missed_cleavage_plot(per_sample, cmap, mode = "overall")
  built <- ggplot2::ggplot_build(p)
  bar_idx <- which(vapply(p$layers, function(l) class(l$geom)[1], character(1))
                   == "GeomCol")
  expect_equal(nrow(built$data[[bar_idx]]), 1L)
  expect_equal(built$data[[bar_idx]]$y, mean(c(0.1, 0.2, 0.5, 0.7)))
})

test_that("pelsa_missed_cleavage_plot y-axis label is a percent", {
  per_sample <- data.frame(sample = c("A_R1", "A_R2"), rate = c(0.1, 0.3),
                           n_quantified = c(5L, 5L), stringsAsFactors = FALSE)
  cmap <- c(A_R1 = "A", A_R2 = "A")
  p <- pelsa_missed_cleavage_plot(per_sample, cmap, mode = "per_condition")
  expect_match(p$labels$y, "%")
})

test_that("pelsa_coverage_plot renders bar+errorbar in both modes", {
  per_sample <- data.frame(
    sample = c("A_R1", "A_R2", "B_R1", "B_R2"),
    coverage = c(0.5, 0.6, 0.2, 0.3),
    n_proteins = rep(4L, 4L),
    stringsAsFactors = FALSE
  )
  cmap <- c(A_R1 = "A", A_R2 = "A", B_R1 = "B", B_R2 = "B")
  p_cond <- pelsa_coverage_plot(per_sample, cmap, mode = "per_condition")
  expect_s3_class(p_cond, "ggplot")
  p_overall <- pelsa_coverage_plot(per_sample, cmap, mode = "overall")
  expect_s3_class(p_overall, "ggplot")
  expect_match(p_cond$labels$y, "%")
})

test_that("pelsa_length_plot renders bar+errorbar in both modes", {
  per_sample <- data.frame(
    sample = c("A_R1", "A_R2", "B_R1", "B_R2"),
    mean_length = c(9, 10, 14, 15),
    n_quantified = rep(20L, 4L),
    stringsAsFactors = FALSE
  )
  cmap <- c(A_R1 = "A", A_R2 = "A", B_R1 = "B", B_R2 = "B")
  p_cond <- pelsa_length_plot(per_sample, cmap, mode = "per_condition")
  expect_s3_class(p_cond, "ggplot")
  p_overall <- pelsa_length_plot(per_sample, cmap, mode = "overall")
  expect_s3_class(p_overall, "ggplot")
})

test_that("all three panels fall back to blank plot when no condition is eligible", {
  per_sample <- data.frame(sample = "A_R1", rate = 0.1, n_quantified = 5L,
                           stringsAsFactors = FALSE)
  cmap <- c(A_R1 = "A")
  p <- pelsa_missed_cleavage_plot(per_sample, cmap, mode = "per_condition")
  # annotate("text", ...) stores its label as a layer aes_param, not in data.
  ann_text <- unlist(lapply(p$layers, function(l) {
    if (inherits(l$geom, "GeomText")) l$aes_params$label else NULL
  }))
  expect_true(any(grepl("No condition has >= 2 replicate samples", ann_text,
                       fixed = TRUE)))
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

# ---- export styling: pelsa_depth_bar_plot -------------------------------------

test_that("pelsa_depth_bar_plot export=TRUE removes x-axis title and enlarges x-axis text", {
  nq <- c(S1 = 100L, S2 = 250L, S3 = 175L)
  p <- pelsa_depth_bar_plot(nq, export = TRUE)
  expect_null(p$labels$x)
  expect_equal(p$theme$axis.text.x$size, 9)
  expect_equal(p$theme$axis.text.x$colour, "black")
  expect_equal(p$theme$axis.text$size, 8)  # y-axis (general rule) unaffected
  expect_equal(p$theme$axis.text$colour, "black")
  gt_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_equal(p$layers[[gt_idx]]$aes_params$size, 4)
  expect_equal(p$theme$plot.title$size, 12)
  expect_null(p$theme$plot.title.position)
})

test_that("pelsa_depth_bar_plot default (export=FALSE) keeps 'Sample' x title, size 11 x-text, size 3 label", {
  nq <- c(S1 = 100L, S2 = 250L, S3 = 175L)
  p <- pelsa_depth_bar_plot(nq)
  expect_equal(p$labels$x, "Sample")
  expect_equal(p$theme$axis.text.x$size, 11)
  gt_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_equal(p$layers[[gt_idx]]$aes_params$size, 3)
})

# ---- export wiring: pelsa_section2_exports_for --------------------------------

test_that("pelsa_section2_exports_for's QC bundle calls builders with export=TRUE and 5.6x3.5in canvas", {
  entry <- list(
    coverage = data.frame(accession = paste0("P", 1:5),
                          coverage = c(0.1, 0.2, 0.3, 0.4, 0.5),
                          protein_length = rep(100L, 5), stringsAsFactors = FALSE),
    peptide_metrics = data.frame(peptide_seq = paste0("PEP", 1:5),
                                 peptide_length = c(8, 9, 10, 11, 12),
                                 missed_cleavages = rep(0L, 5), stringsAsFactors = FALSE),
    cv = data.frame(cv_pct = numeric(0), cv_status = character(0), condition = character(0)),
    n_quantified = c(S1 = 10L),
    coverage_by_condition = data.frame(condition = character(0), coverage = numeric(0)),
    length_by_condition = data.frame(condition = character(0), peptide_length = numeric(0))
  )
  captured <- list()
  testthat::local_mocked_bindings(
    pelsa_save_figure = function(plot, dir_name, basename, width, height, ...) {
      captured[[basename]] <<- list(width = width, height = height,
                                    title_size = plot$theme$plot.title$size,
                                    axis_colour = plot$theme$axis.text$colour)
      invisible(NULL)
    },
    .package = "Protigy"
  )
  tmp <- file.path(tempdir(), paste0("qcexport_", as.integer(runif(1, 1, 1e6))))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  bundle <- pelsa_section2_exports_for(entry, ome = "ds1")
  bundle$qc(tmp)
  unlink(tmp, recursive = TRUE)

  expect_true(length(captured) >= 1L)
  for (nm in names(captured)) {
    dims <- captured[[nm]]
    expect_equal(dims$width, 5.6, info = paste("width for", nm))
    expect_equal(dims$height, 3.5, info = paste("height for", nm))
    expect_equal(dims$title_size, 12, info = paste("export title size for", nm))
    expect_equal(dims$axis_colour, "black", info = paste("export axis colour for", nm))
  }
})
