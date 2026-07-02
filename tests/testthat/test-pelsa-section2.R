################################################################################
# Tests for the PELSA Section 2 self-curated annotation value box.
#
# Covers Task 1: the failed_annotation_count valueBox must return a neutral
# (non-orange) card for self-curated datasets and retain its orange failure
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

# A synthetic cache entry that would normally trigger an orange failure box:
# 3 unannotated accessions -> n_unannotated_accessions = 3.
.s2_failed_entry <- function() {
  list(
    qc           = list(n_unannotated_accessions = 3L),
    unannotated  = c("A", "B", "C"),
    stage        = "done"
  )
}

# ---- self-curated: neutral card (not orange) ---------------------------------

test_that("failed_annotation_count is neutral for a self-curated dataset", {
  entry <- .s2_failed_entry()
  args  <- .s2_min_reactives(entry, self_curated_flag = TRUE)

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = args,
    {
      vb   <- output$failed_annotation_count
      html <- as.character(vb$html %||% vb)
      expect_false(grepl("orange", html, ignore.case = TRUE),
                   info = paste("Expected no 'orange' in valueBox HTML; got:", html))
      expect_true(grepl("self-curated", html, ignore.case = TRUE),
                  info = paste("Expected 'self-curated' in valueBox HTML; got:", html))
    }
  )
})

# ---- non-self-curated: orange failure card stays ----------------------------

test_that("failed_annotation_count stays orange for a non-self-curated dataset with failures", {
  entry <- .s2_failed_entry()
  args  <- .s2_min_reactives(entry, self_curated_flag = FALSE)

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = args,
    {
      vb   <- output$failed_annotation_count
      html <- as.character(vb$html %||% vb)
      expect_true(grepl("orange", html, ignore.case = TRUE),
                  info = paste("Expected 'orange' in valueBox HTML; got:", html))
    }
  )
})

# ---- Task 7: bold density-plot annotations -----------------------------------

test_that("overall density mean/median labels are bold", {
  vals <- rnorm(200, 20, 5)
  p <- pelsa_overall_density_plot(vals, x_label = "x", title = "t")
  text_layers <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  expect_length(text_layers, 2L)
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

test_that("in-app bar labels are bottom-anchored (vjust=0) so they sit on top of the bar", {
  # missed-cleavage: peptide_metrics with a missed-cleavage column
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

  # depth: named integer vector of per-sample peptide counts
  nq <- c(S1 = 1200L, S2 = 1500L, S3 = 900L)
  g_d <- pelsa_depth_bar_plot(nq)
  txt_d <- Filter(function(l) inherits(l$geom, "GeomText"), g_d$layers)
  expect_length(txt_d, 1L)
  expect_equal(txt_d[[1]]$aes_params$vjust, 0)
})
