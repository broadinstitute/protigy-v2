################################################################################
# Tests for the PELSA Summary section (Phase 6).
#
#   PURE helpers (closed-form, NO Shiny):
#     pelsa_dodge_offsets        — vertically-dodged annotation y positions
#     pelsa_cv_kde_eligibility   — per-condition KDE eligibility (<20 finite skip)
#     pelsa_sample_bar_order     — per-sample bar order (sample_order, alpha fallback)
#     pelsa_depth_bar_data       — ordered per-sample bar data
#     pelsa_coverage_values / pelsa_over_length_count / pelsa_length_values /
#       pelsa_missed_cleavage_data — cache-table shaping
#     pelsa_section2_exports_for — re-derive CSVs from a cache entry
#
#   testServer (light): a synthetic pelsa_analysis cache (built via
#     pelsa_run_analysis on the generator) injected; assert the NULL-cache /
#     failed-entry gates + that a good entry's outputs render.
#
# NO NETWORK: the cache is built with an INJECTED fasta_map + hand-set feat_df.
################################################################################

library(testthat)
source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# ---- shared cache builders (mirror test-pelsa-analysis.R, no network) --------

.summary_mk_gct <- function(syn) {
  peptides <- syn$peptides
  sc <- syn$sample_cols
  rids <- paste0("pep", seq_len(nrow(peptides)))
  mat <- as.matrix(peptides[, sc]); rownames(mat) <- rids
  rdesc <- peptides[, setdiff(colnames(peptides), sc), drop = FALSE]
  rownames(rdesc) <- rids
  cdesc <- data.frame(condition = sub("_R[0-9]+$", "", sc),
                      row.names = sc, stringsAsFactors = FALSE)
  cmapR::GCT(mat = mat, rdesc = rdesc, cdesc = cdesc)
}

.summary_mk_feat_df <- function() {
  data.frame(
    accession     = c("SHARED1", "DUPPROT", "TIEPROT"),
    start         = c(1L, 1L, 1L),
    end           = c(50L, 60L, 40L),
    feature_class = c("domain", "domain", "domain"),
    stringsAsFactors = FALSE
  )
}

.summary_build_cache <- function(dataset = "ds1") {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 12)
  g <- .summary_mk_gct(syn)
  snap <- list(datasets = dataset, species = "human",
               condition_col = stats::setNames(list("condition"), dataset))
  pelsa_run_analysis(
    gcts = stats::setNames(list(g), dataset),
    gcts_original = stats::setNames(list(g), dataset),
    setup_snapshot = snap, fasta_map = syn$fasta,
    feat_df = .summary_mk_feat_df()
  )
}

# ---- pelsa_dodge_offsets -----------------------------------------------------

test_that("pelsa_dodge_offsets stacks labels downward and is closed-form", {
  expect_length(pelsa_dodge_offsets(0L, 1, 1), 0L)
  ys <- pelsa_dodge_offsets(3L, y_top = 1, y_range = 1, frac = 0.1)
  expect_equal(ys, c(1, 0.9, 0.8))
  # strictly decreasing (no two labels at the same height)
  expect_true(all(diff(ys) < 0))
})

test_that("pelsa_dodge_offsets guards degenerate y_range", {
  ys <- pelsa_dodge_offsets(2L, y_top = 5, y_range = 0)  # range coerced to 1
  expect_equal(ys, c(5, 5 - 0.08))
})

# ---- pelsa_cv_kde_eligibility ------------------------------------------------

test_that("cv KDE eligibility skips conditions with < min_n finite CVs", {
  cv <- data.frame(
    condition = c(rep("A", 25), rep("B", 5), rep("C", 25)),
    cv_pct    = runif(55, 1, 20),
    cv_status = "ok",
    stringsAsFactors = FALSE
  )
  res <- pelsa_cv_kde_eligibility(cv, condition_order = c("C", "A", "B"),
                                  min_n = 20L)
  # eligible in requested order; B skipped (only 5)
  expect_identical(res$eligible, c("C", "A"))
  expect_identical(res$skipped$condition, "B")
  expect_identical(res$skipped$n, 5L)
})

test_that("cv KDE eligibility counts only ok rows and handles NULL", {
  cv <- data.frame(
    condition = c(rep("A", 30), rep("A", 30)),
    cv_pct    = c(runif(30), rep(NA, 30)),
    cv_status = c(rep("ok", 30), rep("non_finite", 30)),
    stringsAsFactors = FALSE
  )
  res <- pelsa_cv_kde_eligibility(cv, min_n = 20L)
  expect_identical(res$eligible, "A")          # 30 ok rows, non-ok ignored
  empty <- pelsa_cv_kde_eligibility(NULL)
  expect_length(empty$eligible, 0L)
  expect_equal(nrow(empty$skipped), 0L)
})

# ---- pelsa_sample_bar_order / pelsa_depth_bar_data ---------------------------

test_that("sample bar order respects sample_order then appends extras alpha", {
  nq <- c(s3 = 10L, s1 = 5L, s2 = 7L, zz = 1L)
  ord <- pelsa_sample_bar_order(nq, sample_order = c("s1", "s2", "s3"))
  expect_identical(ord, c("s1", "s2", "s3", "zz"))  # zz absent from order -> end
})

test_that("sample bar order falls back to alphabetical when no order", {
  nq <- c(b = 1L, a = 2L, c = 3L)
  expect_identical(pelsa_sample_bar_order(nq, NULL), c("a", "b", "c"))
  expect_length(pelsa_sample_bar_order(integer(0)), 0L)
})

test_that("sample_order entries absent from n_quantified are ignored", {
  nq <- c(a = 1L, b = 2L)
  ord <- pelsa_sample_bar_order(nq, sample_order = c("ghost", "b", "a"))
  expect_identical(ord, c("b", "a"))
})

test_that("depth bar data is an ordered factor matching the bar order", {
  nq <- c(s2 = 8L, s1 = 4L)
  df <- pelsa_depth_bar_data(nq, sample_order = c("s1", "s2"))
  expect_identical(levels(df$sample), c("s1", "s2"))
  expect_identical(df$n, c(4L, 8L))
})

# ---- coverage / length / missed-cleavage shaping -----------------------------

test_that("coverage values keep finite [0,1], over-length count works", {
  cov <- data.frame(
    accession = c("a", "b", "c"),
    coverage  = c(0.5, NA, 1.0),
    over_length_flag = c(FALSE, TRUE, NA),
    stringsAsFactors = FALSE
  )
  expect_equal(pelsa_coverage_values(cov), c(0.5, 1.0))
  expect_equal(pelsa_over_length_count(cov), 1L)
  expect_equal(pelsa_over_length_count(NULL), 0L)
})

test_that("length values + missed-cleavage data shape correctly", {
  pm <- data.frame(
    PEP.StrippedSequence = c("AAA", "BBBB", "CC"),
    missed_cleavages = c(0L, 1L, 0L),
    peptide_length   = c(3L, 4L, 2L),
    stringsAsFactors = FALSE
  )
  expect_setequal(pelsa_length_values(pm), c(3, 4, 2))
  mc <- pelsa_missed_cleavage_data(pm)
  expect_identical(mc$missed, c(0L, 1L))
  expect_identical(mc$count, c(2L, 1L))
})

# ---- CV plot render paths (cv = NULL / all-skipped) --------------------------

test_that("pelsa_cv_kde_plot renders without error when cv is NULL", {
  # 5D contract allows entry$cv = NULL (no raw GCT / all-NA condition). The
  # render must short-circuit to a blank message, not error.
  p <- pelsa_cv_kde_plot(NULL, condition_order = NULL)
  expect_s3_class(p, "ggplot")
  # ggplotly must also succeed on the blank plot (the render layer wraps it).
  expect_no_error(plotly::ggplotly(p))
  # eligibility on NULL is empty (no eligible, no skipped).
  el <- pelsa_cv_kde_eligibility(NULL)
  expect_length(el$eligible, 0L)
  expect_equal(nrow(el$skipped), 0L)
})

test_that("pelsa_cv_kde_plot gives a 'not enough data' message when all conditions skipped", {
  # Every condition has < 20 finite CVs -> none eligible -> a sensible message,
  # NOT an error.
  cv <- data.frame(
    condition = c(rep("A", 5), rep("B", 8)),
    cv_pct    = runif(13, 1, 20),
    cv_status = "ok",
    stringsAsFactors = FALSE
  )
  p <- pelsa_cv_kde_plot(cv, condition_order = c("A", "B"))
  expect_s3_class(p, "ggplot")
  expect_no_error(plotly::ggplotly(p))
})

test_that("pelsa_cv_kde_plot draws curves + dodged median labels when eligible", {
  cv <- data.frame(
    condition = c(rep("A", 30), rep("B", 30)),
    cv_pct    = c(runif(30, 1, 10), runif(30, 5, 15)),
    cv_status = "ok",
    stringsAsFactors = FALSE
  )
  p <- pelsa_cv_kde_plot(cv, condition_order = c("A", "B"))
  expect_s3_class(p, "ggplot")
  # A geom_text layer (the dodged median labels) is present -> the dodge math is
  # actually consumed, not dead.
  has_text <- any(vapply(p$layers, function(l)
    inherits(l$geom, "GeomText"), logical(1)))
  expect_true(has_text)
  expect_no_error(plotly::ggplotly(p))
})

# ---- empty unmatched / unannotated shaping (happy path) ----------------------

test_that("empty unmatched / unannotated produce empty-but-valid tables", {
  # An empty unmatched df (zero failed matches) and an empty unannotated vector
  # are the HAPPY path; the DT shaping must yield a 0-row frame WITH the headers.
  um <- data.frame(peptide_sequence = character(0), accession = character(0),
                   gene = character(0), pep_position = character(0),
                   reason = character(0), stringsAsFactors = FALSE)
  display <- data.frame(
    `Peptide`          = um$peptide_sequence,
    `Accession`        = um$accession,
    `Gene`             = um$gene,
    `Peptide position` = um$pep_position,
    `Reason`           = um$reason,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  expect_equal(nrow(display), 0L)
  expect_setequal(names(display),
                  c("Peptide", "Accession", "Gene", "Peptide position", "Reason"))

  ua <- as.character(character(0) %||% character(0))
  ua_display <- data.frame(Accession = ua, stringsAsFactors = FALSE)
  expect_equal(nrow(ua_display), 0L)
  expect_identical(names(ua_display), "Accession")
})

# ---- pelsa_section2_exports_for ----------------------------------------------

test_that("exports_for re-derives CSVs from a cache entry", {
  cache <- .summary_build_cache("ds1")
  entry <- cache$ds1
  expect_false(pelsa_analysis_failed(entry))

  exp_list <- pelsa_section2_exports_for(entry, "ds1")
  expect_setequal(
    names(exp_list),
    c("cv", "coverage", "depth", "unmatched", "unannotated", "peptide_metrics")
  )

  dir <- tempfile("pelsa_exp_"); dir.create(dir)
  for (fn in exp_list) fn(dir)
  files <- list.files(dir, pattern = "\\.csv$")
  expect_true(all(c(
    "pelsa_cv_ds1.csv", "pelsa_coverage_ds1.csv", "pelsa_depth_ds1.csv",
    "pelsa_unmatched_ds1.csv", "pelsa_unannotated_ds1.csv",
    "pelsa_peptide_metrics_ds1.csv") %in% files))

  # depth CSV carries per-sample rows + the summary columns appended.
  depth <- utils::read.csv(file.path(dir, "pelsa_depth_ds1.csv"),
                           stringsAsFactors = FALSE)
  expect_true(all(c("sample", "n_quantified", "total_n_peptides") %in%
                    names(depth)))
})

# ---- testServer: gates + good-entry render -----------------------------------

test_that("Summary shows 'Run Start Analysis' when the cache is NULL", {
  GCTs_and_params <- shiny::reactiveVal(list(GCTs = list(ds1 = NULL),
                                             parameters = list(ds1 = list())))
  globals <- shiny::reactiveValues(default_ome = "ds1", colors = list())
  GCTs_original <- shiny::reactiveVal(list(ds1 = NULL))
  active_dataset <- shiny::reactive("ds1")
  pelsa_analysis <- shiny::reactiveVal(NULL)        # not run yet
  pelsa_setup_state <- shiny::reactive(NULL)

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset,
                pelsa_analysis = pelsa_analysis,
                pelsa_setup_state = pelsa_setup_state),
    {
      html <- as.character(output$summary_box$html %||% output$summary_box)
      expect_true(grepl("Run Start Analysis", html))
      expect_equal(length(session$returned()), 0L)  # no exports without a cache
    }
  )
})

test_that("Summary shows the error for a failed dataset entry", {
  cache <- list(ds1 = list(error = "boom", stage = "Computing CV"))
  GCTs_and_params <- shiny::reactiveVal(list(GCTs = list(ds1 = NULL),
                                             parameters = list(ds1 = list())))
  globals <- shiny::reactiveValues(default_ome = "ds1", colors = list())
  GCTs_original <- shiny::reactiveVal(list(ds1 = NULL))
  active_dataset <- shiny::reactive("ds1")
  pelsa_analysis <- shiny::reactiveVal(cache)
  pelsa_setup_state <- shiny::reactive(NULL)

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset,
                pelsa_analysis = pelsa_analysis,
                pelsa_setup_state = pelsa_setup_state),
    {
      html <- as.character(output$summary_box$html %||% output$summary_box)
      expect_true(grepl("failed", html))
      expect_true(grepl("Computing CV", html))
      expect_true(grepl("boom", html))
    }
  )
})

test_that("Summary renders metrics + exports for a good cache entry", {
  cache <- .summary_build_cache("ds1")
  expect_false(pelsa_analysis_failed(cache$ds1))

  GCTs_and_params <- shiny::reactiveVal(list(GCTs = list(ds1 = NULL),
                                             parameters = list(ds1 = list())))
  globals <- shiny::reactiveValues(default_ome = "ds1", colors = list())
  GCTs_original <- shiny::reactiveVal(list(ds1 = NULL))
  active_dataset <- shiny::reactive("ds1")
  pelsa_analysis <- shiny::reactiveVal(cache)
  pelsa_setup_state <- shiny::reactive(list(
    sample_order    = list(ds1 = character(0)),
    condition_order = list(ds1 = c("AY9944_10uM", "DMSO", "LowN"))
  ))

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset,
                pelsa_analysis = pelsa_analysis,
                pelsa_setup_state = pelsa_setup_state),
    {
      # The dashboard layout (not the gate message) renders.
      html <- as.character(output$summary_box$html %||% output$summary_box)
      expect_false(grepl("Run Start Analysis", html))

      # Value box: total peptide IDs == qc$n_peptides.
      vb <- output$total_peptide_ids
      vb_html <- as.character(vb$html %||% vb)
      expect_true(grepl(as.character(cache$ds1$qc$n_peptides), vb_html))

      # Plot outputs exist (do not assert pixel content).
      expect_false(is.null(output$coverage_plot))
      expect_false(is.null(output$length_plot))
      expect_false(is.null(output$missed_plot))
      expect_false(is.null(output$cv_plot))
      expect_false(is.null(output$depth_plot))

      # The CV caption is EXACTLY the required text.
      cap <- paste(as.character(output$cv_caption), collapse = "")
      expect_true(grepl(
        "CV of sum-normalized \\(un-logged\\) intensities", cap))

      # Exports: one entry per analyzed dataset, with the 6E names.
      exp_all <- session$returned()
      expect_identical(names(exp_all), "ds1")
      expect_setequal(
        names(exp_all$ds1),
        c("cv", "coverage", "depth", "unmatched", "unannotated",
          "peptide_metrics"))
    }
  )
})

test_that("Summary renders + exports for a good entry whose cv is NULL", {
  # 5D contract allows entry$cv = NULL (no raw GCT / all-NA condition). The whole
  # dashboard must still render through the render layer (CV panel short-circuits
  # to a blank), and exports still produce all six files (cv -> empty CSV).
  cache <- .summary_build_cache("ds1")
  cache$ds1$cv <- NULL
  expect_false(pelsa_analysis_failed(cache$ds1))

  GCTs_and_params <- shiny::reactiveVal(list(GCTs = list(ds1 = NULL),
                                             parameters = list(ds1 = list())))
  globals <- shiny::reactiveValues(default_ome = "ds1", colors = list())
  GCTs_original <- shiny::reactiveVal(list(ds1 = NULL))
  active_dataset <- shiny::reactive("ds1")
  pelsa_analysis <- shiny::reactiveVal(cache)
  pelsa_setup_state <- shiny::reactive(list(
    sample_order    = list(ds1 = character(0)),
    condition_order = list(ds1 = NULL)
  ))

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset,
                pelsa_analysis = pelsa_analysis,
                pelsa_setup_state = pelsa_setup_state),
    {
      html <- as.character(output$summary_box$html %||% output$summary_box)
      expect_false(grepl("Run Start Analysis", html))
      # The CV plot output renders without error despite cv = NULL.
      expect_false(is.null(output$cv_plot))
      # Exports still produce all six (cv export writes an empty data.frame).
      exp_all <- session$returned()
      expect_setequal(
        names(exp_all$ds1),
        c("cv", "coverage", "depth", "unmatched", "unannotated",
          "peptide_metrics"))
      dir <- tempfile("pelsa_cvnull_"); dir.create(dir)
      exp_all$ds1$cv(dir)
      expect_true(file.exists(file.path(dir, "pelsa_cv_ds1.csv")))
    }
  )
})
