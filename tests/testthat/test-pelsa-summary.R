################################################################################
# Tests for the PELSA Summary section (Phase 6).
#
#   PURE helpers (closed-form, NO Shiny):
#     pelsa_dodge_offsets         -  vertically-dodged annotation y positions
#     pelsa_cv_kde_eligibility    -  per-condition KDE eligibility (<20 finite skip)
#     pelsa_sample_bar_order      -  per-sample bar order (sample_order, alpha fallback)
#     pelsa_depth_bar_data        -  ordered per-sample bar data
#     pelsa_coverage_values / pelsa_over_length_count / pelsa_length_values /
#       pelsa_missed_cleavage_data  -  cache-table shaping
#     pelsa_section2_exports_for  -  re-derive CSVs from a cache entry
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

test_that("missed-cleavage data includes percent of all identified peptides", {
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:4),
    missed_cleavages = c(0L, 0L, 1L, 2L),
    peptide_length   = rep(8L, 4L),
    stringsAsFactors = FALSE
  )
  mc <- pelsa_missed_cleavage_data(pm)
  expect_identical(mc$missed, c(0L, 1L, 2L))
  expect_identical(mc$count, c(2L, 1L, 1L))
  # percent is count / total peptides identified (nrow(pm)) * 100.
  expect_equal(mc$percent, c(50, 25, 25))
  # all 4 peptides have finite missed-cleavage values, so percents sum to 100.
  expect_equal(sum(mc$percent), 100)
})

test_that("missed-cleavage percent denominator is ALL identified peptides, not just finite", {
  # 2 of 4 peptides have a non-finite missed-cleavage value. The denominator is
  # still nrow(pm) = 4 (= qc$n_peptides / total peptides identified), so the
  # bars cover only 2 peptides and percents sum to 50, NOT 100.
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:4),
    missed_cleavages = c(0L, 1L, NA_integer_, NA_integer_),
    peptide_length   = rep(8L, 4L),
    stringsAsFactors = FALSE
  )
  mc <- pelsa_missed_cleavage_data(pm)
  expect_identical(mc$missed, c(0L, 1L))
  expect_identical(mc$count, c(1L, 1L))
  # denominator is 4 (all rows), not 2 (finite rows): 1/4 = 25% each.
  expect_equal(mc$percent, c(25, 25))
  expect_equal(sum(mc$percent), 50)
})

test_that("missed-cleavage empty result carries a numeric percent column", {
  mc <- pelsa_missed_cleavage_data(NULL)
  expect_identical(nrow(mc), 0L)
  expect_true("percent" %in% names(mc))
  expect_type(mc$percent, "double")
})

test_that("missed-cleavage data fills gaps with zero-count rows for even spacing", {
  # Peptides have 0, 1, 2, 3, 5, 7 missed cleavages -> 4 and 6 are gaps.
  # The helper must emit a contiguous 0..7 sequence, with count 0 / percent 0
  # at the missing values so the bar chart can draw a visible empty slot.
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:8),
    missed_cleavages = c(0L, 0L, 1L, 2L, 3L, 5L, 7L, 7L),
    peptide_length   = rep(8L, 8L),
    stringsAsFactors = FALSE
  )
  mc <- pelsa_missed_cleavage_data(pm)
  # Contiguous 0..7 (max observed is 7), no gaps.
  expect_identical(mc$missed, 0:7)
  # Observed counts at 0,1,2,3,5,7; zeros filled at 4 and 6.
  expect_identical(mc$count, c(2L, 1L, 1L, 1L, 0L, 1L, 0L, 2L))
  # Gap rows carry percent 0; observed rows are count / nrow(pm) * 100.
  expect_equal(mc$percent[mc$missed == 4L], 0)
  expect_equal(mc$percent[mc$missed == 6L], 0)
  expect_equal(mc$percent[mc$missed == 0L], 25)   # 2 / 8 * 100
  expect_equal(mc$percent[mc$missed == 7L], 25)   # 2 / 8 * 100
  # Percentages still sum over the same numerator (8 finite peptides / 8 total).
  expect_equal(sum(mc$percent), 100)
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
# FIX C (P2.3): the old test rebuilt the data.frame shaping inline and asserted
# on its own construction (never calling output$unmatched_table or
# output$unannotated_table). Replaced with a testServer test that drives the
# real render code via a good cache entry that has zero failed rows.

test_that("output$unmatched_table and output$unannotated_table render as 0-row DT for a good entry", {
  # Use the standard synthetic cache and zero-out unmatched/unannotated to create
  # the "zero failed rows" happy path that the old tautological test described but
  # never actually exercised via the module.
  cache <- .summary_build_cache("ds1")
  expect_false(pelsa_analysis_failed(cache$ds1))

  # Inject empty unmatched / unannotated so the tables must render with 0 rows.
  cache$ds1$unmatched <- data.frame(
    peptide_sequence = character(0), accession = character(0),
    gene = character(0), pep_position = character(0), reason = character(0),
    stringsAsFactors = FALSE
  )
  cache$ds1$unannotated <- character(0)

  GCTs_and_params  <- shiny::reactiveVal(list(GCTs = list(ds1 = NULL),
                                              parameters = list(ds1 = list())))
  globals          <- shiny::reactiveValues(default_ome = "ds1", colors = list())
  GCTs_original    <- shiny::reactiveVal(list(ds1 = NULL))
  active_dataset   <- shiny::reactive("ds1")
  pelsa_analysis   <- shiny::reactiveVal(cache)
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
      # unmatched_table: the real render code (tab_pelsa_section2.R:290) builds a
      # 5-col DT from entry$unmatched.  A non-NULL JSON confirms the render ran
      # (req(entry) did not abort).  DT serializes to JSON in testServer; we parse
      # the columnDefs to assert the schema without inspecting pixel output.
      expect_false(is.null(output$unmatched_table))
      um_json <- jsonlite::fromJSON(output$unmatched_table)
      um_col_names <- um_json$x$options$columnDefs$name
      expect_setequal(um_col_names,
                      c("Peptide", "Accession", "Gene", "Peptide position", "Reason"))
      # 0 rows: the $x$data element is absent / NULL when the table is empty.
      expect_null(um_json$x$data)

      # unannotated_table: the real render code (tab_pelsa_section2.R:310) builds a
      # 1-col DT from entry$unannotated.
      expect_false(is.null(output$unannotated_table))
      ua_json <- jsonlite::fromJSON(output$unannotated_table)
      ua_col_names <- ua_json$x$options$columnDefs$name
      expect_identical(ua_col_names, "Accession")
      expect_null(ua_json$x$data)
    }
  )
})

# ---- pelsa_section2_exports_for ----------------------------------------------

test_that("exports_for writes the 02_qc summaries from a cache entry", {
  cache <- .summary_build_cache("ds1")
  entry <- cache$ds1
  expect_false(pelsa_analysis_failed(entry))

  # One `qc` bundle that writes the three summaries (+ figures) into 02_qc/.
  exp_list <- pelsa_section2_exports_for(entry, "ds1")
  expect_setequal(names(exp_list), "qc")

  dir <- tempfile("pelsa_exp_"); dir.create(dir)
  for (fn in exp_list) fn(dir)
  qc_dir <- file.path(dir, "02_qc")
  expect_true(dir.exists(qc_dir))
  files <- list.files(qc_dir, pattern = "\\.csv$")
  expect_true(all(c(
    "qc_sample_summary.csv", "qc_condition_summary.csv",
    "qc_experiment_summary.csv") %in% files))

  # Sample summary: one row per sample with the non-NA peptide count.
  sample <- utils::read.csv(file.path(qc_dir, "qc_sample_summary.csv"),
                            stringsAsFactors = FALSE)
  expect_true(all(c("sample", "n_peptides_quantified") %in% names(sample)))

  # Experiment summary: a single-row totals/failure table.
  exp_csv <- utils::read.csv(file.path(qc_dir, "qc_experiment_summary.csv"),
                             stringsAsFactors = FALSE)
  expect_equal(nrow(exp_csv), 1L)
  expect_true("n_peptides_total" %in% names(exp_csv))
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

test_that("Summary failed entry with a length-0 stage renders without error", {
  # Regression: stage_txt formerly used is.na(stage), which ERRORS on length 0.
  cache <- list(ds1 = list(error = "boom", stage = character(0)))
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
      expect_true(grepl("boom", html))
      # No "(stage: ...)" hint when the stage is length 0.
      expect_false(grepl("stage:", html))
    }
  )
})

test_that("Summary active_entry is NULL for a failed entry (defense-in-depth)", {
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
      # The dashboard's per-output reactives gate on active_entry(); a failed
      # entry must surface as NULL so they never read missing fields.
      expect_null(active_entry())
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

      # Toggling each panel's experiment-wide <-> per-condition mode renders
      # through the live module (guards the radio input-id wiring).
      session$setInputs(coverage_mode = "per_condition",
                        length_mode = "per_condition", cv_mode = "overall")
      expect_false(is.null(output$coverage_plot))
      expect_false(is.null(output$length_plot))
      expect_false(is.null(output$cv_plot))
      session$setInputs(coverage_mode = "overall", length_mode = "overall",
                        cv_mode = "per_condition")
      expect_false(is.null(output$cv_plot))

      # The CV caption is EXACTLY the required text.
      cap <- paste(as.character(output$cv_caption), collapse = "")
      expect_true(grepl(
        "CV of sum-normalized \\(un-logged\\) intensities", cap))

      # Exports: one entry per analyzed dataset, the single `qc` bundle.
      exp_all <- session$returned()
      expect_identical(names(exp_all), "ds1")
      expect_setequal(names(exp_all$ds1), "qc")
    }
  )
})

test_that("exports work when pelsa_setup_state is NULL (the legacy/default wiring)", {
  # all_exports must read setup_state through the in-scope NULL-safe wrapper
  # (setup_state_r), so the module produces exports even when pelsa_setup_state
  # is the literal NULL default (no setup-state reactive supplied). Ordering
  # falls back to NULL; no error.
  cache <- .summary_build_cache("ds1")
  expect_false(pelsa_analysis_failed(cache$ds1))

  GCTs_and_params <- shiny::reactiveVal(list(GCTs = list(ds1 = NULL),
                                             parameters = list(ds1 = list())))
  globals <- shiny::reactiveValues(default_ome = "ds1", colors = list())
  GCTs_original <- shiny::reactiveVal(list(ds1 = NULL))
  active_dataset <- shiny::reactive("ds1")
  pelsa_analysis <- shiny::reactiveVal(cache)

  shiny::testServer(
    PELSASection2_Tab_Server,
    # pelsa_setup_state intentionally OMITTED -> defaults to NULL.
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset,
                pelsa_analysis = pelsa_analysis),
    {
      exp_all <- session$returned()
      expect_identical(names(exp_all), "ds1")
      expect_setequal(names(exp_all$ds1), "qc")
    }
  )
})

test_that("all_exports does NOT swallow a thrown setup_state error", {
  # The setup_state read in all_exports must NOT be wrapped in a blanket
  # tryCatch(error -> NULL): a genuinely throwing setup_state reactive should
  # propagate, not silently degrade to "no ordering". (Guards the intent of the
  # setup_state_r() refactor against a future re-introduced tryCatch.)
  cache <- .summary_build_cache("ds1")
  GCTs_and_params <- shiny::reactiveVal(list(GCTs = list(ds1 = NULL),
                                             parameters = list(ds1 = list())))
  globals <- shiny::reactiveValues(default_ome = "ds1", colors = list())
  GCTs_original <- shiny::reactiveVal(list(ds1 = NULL))
  active_dataset <- shiny::reactive("ds1")
  pelsa_analysis <- shiny::reactiveVal(cache)
  pelsa_setup_state <- shiny::reactive(stop("setup_state boom"))

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset,
                pelsa_analysis = pelsa_analysis,
                pelsa_setup_state = pelsa_setup_state),
    {
      expect_error(session$returned(), "setup_state boom")
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
      # Exports still produce the qc bundle; the condition summary is written
      # (an empty/condition-less frame) without error when cv is NULL.
      exp_all <- session$returned()
      expect_setequal(names(exp_all$ds1), "qc")
      dir <- tempfile("pelsa_cvnull_"); dir.create(dir)
      exp_all$ds1$qc(dir)
      expect_true(file.exists(file.path(dir, "02_qc",
                                        "qc_experiment_summary.csv")))
    }
  )
})

################################################################################
# Per-condition toggle helpers (experiment-wide <-> per-condition modes).
################################################################################

test_that("pelsa_condition_membership: >=1-sample rule, many-to-many", {
  m <- matrix(c(
    # s1   s2   s3   s4
       5,   0,   7,  NA,   # pep1: A in s1 only,   B in s3 only
       0,  NA,   0,   0,   # pep2: quantified nowhere
       3,   4,   0,   9    # pep3: A in s1&s2,     B in s4
  ), nrow = 3, byrow = TRUE,
     dimnames = list(NULL, c("s1", "s2", "s3", "s4")))
  cmap <- c(s1 = "A", s2 = "A", s3 = "B", s4 = "B")

  mem <- pelsa_condition_membership(m, cmap)
  expect_setequal(colnames(mem), c("row_id", "condition"))
  # pep1: A(s1>0) + B(s3>0); pep3: A(s1,s2) + B(s4); pep2: none.
  A <- sort(mem$row_id[mem$condition == "A"])
  B <- sort(mem$row_id[mem$condition == "B"])
  expect_equal(A, c(1L, 3L))
  expect_equal(B, c(1L, 3L))
  expect_false(2L %in% mem$row_id)
})

test_that("pelsa_condition_membership: empty inputs -> empty frame", {
  empty <- pelsa_condition_membership(matrix(numeric(0), nrow = 0, ncol = 0),
                                      c(s1 = "A"))
  expect_equal(nrow(empty), 0L)
  m <- matrix(1, nrow = 1, dimnames = list(NULL, "s1"))
  expect_equal(nrow(pelsa_condition_membership(m, character(0))), 0L)
})

test_that("pelsa_length_by_condition: joins lengths by row_id", {
  mem <- data.frame(row_id = c(1L, 3L, 1L), condition = c("A", "A", "B"),
                    stringsAsFactors = FALSE)
  pm <- data.frame(peptide_length = c(7, 9, 11), stringsAsFactors = FALSE)
  out <- pelsa_length_by_condition(mem, pm)
  expect_setequal(colnames(out), c("condition", "peptide_length"))
  expect_equal(out$peptide_length[out$condition == "A"], c(7, 11))
  expect_equal(out$peptide_length[out$condition == "B"], 7)
  # Out-of-range row_ids are dropped, never error.
  bad <- pelsa_length_by_condition(data.frame(row_id = 99L, condition = "A"), pm)
  expect_equal(nrow(bad), 0L)
})

test_that("pelsa_coverage_by_condition: per-condition union coverage", {
  # 10-residue protein P. condition A covers [1,5] (50%); B covers [1,5]+[6,10]
  # (100%). matched .row_id links peptides to membership.
  matched <- data.frame(
    accession = c("P", "P"),
    pep_start = c(1L, 6L),
    pep_end   = c(5L, 10L),
    .row_id   = c(1L, 2L),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  fasta <- list(P = paste(rep("A", 10), collapse = ""))
  mem <- data.frame(
    row_id    = c(1L, 1L, 2L),
    condition = c("A", "B", "B"),
    stringsAsFactors = FALSE
  )
  out <- pelsa_coverage_by_condition(mem, matched, fasta)
  expect_setequal(colnames(out), c("condition", "coverage"))
  expect_equal(out$coverage[out$condition == "A"], 0.5)
  expect_equal(out$coverage[out$condition == "B"], 1.0)
})

test_that("pelsa_cv_ok_values: only finite ok rows pooled", {
  cv <- data.frame(
    cv_pct    = c(10, 20, NA, 30, Inf),
    cv_status = c("ok", "ok", "ok", "insufficient_replicates", "ok"),
    stringsAsFactors = FALSE
  )
  expect_equal(sort(pelsa_cv_ok_values(cv)), c(10, 20))
  expect_equal(pelsa_cv_ok_values(NULL), numeric(0))
})

test_that("density plot builders return ggplot, blank on edge cases", {
  expect_s3_class(pelsa_overall_density_plot(c(1, 2, 3, 4), "x", "t"), "ggplot")
  expect_s3_class(pelsa_overall_density_plot(numeric(0), "x", "t"), "ggplot")

  df <- data.frame(condition = rep(c("A", "B"), each = 5),
                   value = c(1:5, 11:15), stringsAsFactors = FALSE)
  expect_s3_class(
    pelsa_per_condition_density_plot(df, "value", x_label = "x", title = "t"),
    "ggplot")
  # A single-point condition is below min_n=2 -> falls back to blank (no curve).
  one <- data.frame(condition = "A", value = 3, stringsAsFactors = FALSE)
  expect_s3_class(
    pelsa_per_condition_density_plot(one, "value", x_label = "x", title = "t"),
    "ggplot")
  expect_s3_class(pelsa_cv_overall_plot(NULL), "ggplot")
})

test_that("CV experiment-wide mode discloses the pooled CV count in subtitle", {
  cv <- data.frame(
    row_id    = 1:6,
    condition = rep(c("A", "B"), each = 3),
    cv_pct    = c(10, 20, 30, 12, 18, 24),
    cv_status = "ok",
    stringsAsFactors = FALSE
  )
  p <- pelsa_cv_overall_plot(cv)
  expect_s3_class(p, "ggplot")
  # Pools EVERY "ok" CV (6 here) regardless of per-condition eligibility, and
  # says so in the subtitle so the toggle modes are not silently different.
  expect_match(p$labels$subtitle, "pooled \\(n = 6 CVs\\)")
})

test_that("CV experiment-wide density clamps x-axis to ~99th pct of ok CVs", {
  # Mostly 1-20 with a handful of extreme outliers (5000) in the far (<1%) tail.
  # The pooled density must clamp to the 99th percentile (mirroring the
  # per-condition KDE), NOT the full range that includes the 5000 outliers.
  ok_vals <- c(runif(297, 1, 20), 5000, 5000, 5000)
  cv <- data.frame(
    row_id    = seq_along(ok_vals),
    condition = "A",
    cv_pct    = ok_vals,
    cv_status = "ok",
    stringsAsFactors = FALSE
  )
  expected_hi <- stats::quantile(ok_vals, 0.99, na.rm = TRUE, names = FALSE)

  p  <- pelsa_cv_overall_plot(cv)
  pb <- ggplot2::ggplot_build(p)
  x_range <- pb$layout$panel_params[[1]]$x.range
  x_hi <- x_range[2]

  # Clamped near the 99th pct (well below the 5000 outlier). coord_cartesian
  # expands the range slightly, so allow generous slack but still far under 5000.
  expect_lt(x_hi, 500)
  expect_lt(abs(x_hi - expected_hi), 0.2 * expected_hi + 5)

  # The coverage/length path (no x_hi arg) must be UNCHANGED: its x-range still
  # spans the full data (no clamp). Feed it the same outlier-laden values.
  p2  <- pelsa_overall_density_plot(ok_vals, x_label = "x", title = "t")
  pb2 <- ggplot2::ggplot_build(p2)
  x_range2 <- pb2$layout$panel_params[[1]]$x.range
  expect_gt(x_range2[2], 1000)
})

test_that("per-condition median labels disclose n", {
  # Condition A has many values, B only a few -> each label surfaces its n.
  df <- data.frame(
    condition = c(rep("A", 12), rep("B", 3)),
    value     = c(seq_len(12), c(20, 21, 22)),
    stringsAsFactors = FALSE
  )
  p <- pelsa_per_condition_density_plot(df, "value", x_label = "x", title = "t")
  expect_s3_class(p, "ggplot")
  txt <- unlist(lapply(p$layers, function(l) {
    d <- l$data
    if (is.data.frame(d) && "label" %in% names(d)) as.character(d$label)
    else character(0)
  }))
  expect_true(any(grepl("median = .* \\(n=12\\)", txt)))
  expect_true(any(grepl("median = .* \\(n=3\\)", txt)))
})

# ---------------------------------------------------------------------------
# pelsa_qc_condition_summary: n_peptides_quantified counts peptides QUANTIFIED
# in >= 1 sample of the condition (canonical finite & non-zero membership),
# NOT every CV row. The count is sourced from the cache entry's
# n_peptides_by_condition (computed once from pelsa_condition_membership),
# matching the per-sample summary's "quantified" semantics.
# ---------------------------------------------------------------------------
test_that("condition summary n_peptides_quantified uses membership count, not CV row count", {
  # cv has 3 rows for A and 2 for B (one row per peptide x condition, including
  # non-quantified peptides). The TRUE quantified counts are fewer: 2 for A, 1
  # for B. The old code returned table(cv$condition) = c(A=3, B=2) (wrong).
  cv <- data.frame(
    condition = c("A", "A", "A", "B", "B"),
    cv_pct    = c(10, 20, NA, 5, NA),
    stringsAsFactors = FALSE
  )
  entry <- list(
    cv = cv,
    coverage_by_condition = data.frame(condition = character(0),
                                       coverage = numeric(0)),
    length_by_condition   = data.frame(condition = character(0),
                                       peptide_length = numeric(0)),
    # canonical per-condition quantified counts (finite & non-zero in >=1 sample)
    n_peptides_by_condition = c(A = 2L, B = 1L)
  )

  out <- pelsa_qc_condition_summary(entry)
  expect_true("n_peptides_quantified" %in% colnames(out))
  got <- stats::setNames(out$n_peptides_quantified, out$condition)
  expect_equal(got[["A"]], 2L)   # NOT 3 (the CV-row count)
  expect_equal(got[["B"]], 1L)   # NOT 2
})

test_that("condition with CV data but zero quantified peptides reports 0, not NA", {
  # Condition "C" has CV rows (it has samples) but is absent from
  # n_peptides_by_condition (no peptide quantified in any of its samples). Its
  # count is genuinely 0, not NA ("unknown").
  cv <- data.frame(
    condition = c("A", "A", "C", "C"),
    cv_pct    = c(10, 20, NA, NA),   # C's CVs are all NA (nothing quantified)
    stringsAsFactors = FALSE
  )
  entry <- list(
    cv = cv,
    coverage_by_condition = data.frame(condition = character(0),
                                       coverage = numeric(0)),
    length_by_condition   = data.frame(condition = character(0),
                                       peptide_length = numeric(0)),
    n_peptides_by_condition = c(A = 2L)   # C absent -> 0 quantified
  )
  out <- pelsa_qc_condition_summary(entry)
  got <- stats::setNames(out$n_peptides_quantified, out$condition)
  expect_true("C" %in% out$condition)
  expect_equal(got[["C"]], 0L)            # 0, NOT NA
  expect_false(anyNA(out$n_peptides_quantified))
  expect_equal(got[["A"]], 2L)
})

# ---------------------------------------------------------------------------
# pelsa_missed_cleavage_plot: integer peptide counts must render as plain
# integers, not scientific notation (label_scientific turned 5 -> "5e+00").
# ---------------------------------------------------------------------------
test_that("missed-cleavage y-axis uses plain integer labels, not scientific", {
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:6),
    missed_cleavages = c(0L, 0L, 0L, 1L, 1L, 2L),
    peptide_length = rep(8L, 6L),
    stringsAsFactors = FALSE
  )
  p <- pelsa_missed_cleavage_plot(pm)
  expect_s3_class(p, "ggplot")
  # Pull the y continuous scale's label formatter and apply it to typical counts.
  y_scale <- p$scales$scales[[which(vapply(p$scales$scales,
    function(s) "y" %in% s$aesthetics, logical(1)))[1]]]
  labs <- y_scale$get_labels(c(5, 20, 1000))
  # Plain integers, NOT scientific ("5e+00").
  expect_false(any(grepl("e\\+", labs)),
               info = paste("got scientific labels:", paste(labs, collapse = ", ")))
  expect_true(grepl("5", labs[1], fixed = TRUE))
})

test_that("missed-cleavage plot bakes count + percent into a tooltip text aesthetic", {
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:4),
    missed_cleavages = c(0L, 0L, 1L, 2L),
    peptide_length = rep(8L, 4L),
    stringsAsFactors = FALSE
  )
  p <- pelsa_missed_cleavage_plot(pm)
  expect_s3_class(p, "ggplot")
  # A `text` aesthetic must be mapped (used as the plotly tooltip). ggplotly
  # reads it whether it lives in the plot-level mapping or a layer mapping.
  has_text_aes <- "text" %in% names(p$mapping) ||
    any(vapply(p$layers, function(l) "text" %in% names(l$mapping), logical(1)))
  expect_true(has_text_aes)
  # The built data must contain the formatted tooltip strings with both the
  # count and the percent for the largest bar (count 2 = 50.0%).
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_true("text" %in% names(built))
  expect_true(any(grepl("50.0%", built$text, fixed = TRUE)))
  expect_true(any(grepl("Peptides: 2", built$text, fixed = TRUE)))
})

test_that("missed-cleavage plot shows contiguous x positions with an empty-slot tooltip", {
  # Gap at 6 (no peptide has 6 missed cleavages). The plot must reserve an
  # axis slot at 6 and give it a 'Peptides: 0' tooltip, not collapse 5 -> 7.
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:7),
    missed_cleavages = c(0L, 1L, 2L, 3L, 4L, 5L, 7L),
    peptide_length = rep(8L, 7L),
    stringsAsFactors = FALSE
  )
  p <- pelsa_missed_cleavage_plot(pm)
  expect_s3_class(p, "ggplot")
  # X factor levels are contiguous 0..7 (gap value 6 included as a level).
  built <- ggplot2::ggplot_build(p)
  expect_identical(levels(built$plot$data$missed),
                   as.character(0:7))
  # The gap slot (6) carries an explicit zero-count tooltip.
  txt <- built$data[[1]]$text
  expect_true(any(grepl("Missed cleavages: 6", txt, fixed = TRUE)))
  expect_true(any(grepl("Peptides: 0", txt, fixed = TRUE)))
  expect_true(any(grepl("Percent: 0.0%", txt, fixed = TRUE)))
})

# ---- 6A value boxes: three-way annotation QC --------------------------------

test_that("dashboard exposes the three annotation value boxes", {
  ns <- shiny::NS("PELSASection2Tab")
  html <- as.character(pelsa_section2_dashboard_ui(ns, ome = "proteome"))
  expect_match(html, ns("annotated_with_features_count"), fixed = TRUE)
  expect_match(html, ns("annotated_zero_feature_count"), fixed = TRUE)
  expect_match(html, ns("failed_annotation_count"), fixed = TRUE)
})
