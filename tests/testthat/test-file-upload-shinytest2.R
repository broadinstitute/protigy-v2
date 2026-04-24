################################################################################
# Integration tests: file upload workflows
#
# Tests the complete reactive chain from file upload through GCT processing
# for all supported file formats.
#
# Group A: Single-file upload (GCT, CSV, TSV, XLSX)
# Group B: Multi-omic upload with same extension (GCT, CSV, TSV, XLSX)
# Group C: Multi-omic upload with mixed extensions
################################################################################

library(testthat)

# ---------------------------------------------------------------------------
# Helpers local to this file
# ---------------------------------------------------------------------------

# Walk through the GCT setup wizard for a single uploaded dataset.
# Assumptions:
#   - File already uploaded via app$upload_file()
#   - Label input is rendered
#   - submitLabelsButton submits the label step
#   - submitGCTButton finalises setup
complete_gct_wizard <- function(app, label = "Proteome",
                                annotation_col = NULL,
                                file_name = "mb-proteome-ratio-norm-NArm.gct") {
  app$wait_for_idle(duration = 800, timeout = 20000)

  # Step 1: assign label and submit
  app$set_inputs(
    `setupSidebar-Label_mb-proteome-ratio-norm-NArm.gct` =
      if (identical(file_name, "mb-proteome-ratio-norm-NArm.gct")) label else label,
    wait_ = FALSE
  )
  app$wait_for_idle(duration = 400)
  wait_for_input_bound(app, "setupSidebar-submitLabelsButton")
  app$click(input = "setupSidebar-submitLabelsButton")
  app$wait_for_idle(duration = 1000, timeout = 20000)

  # Step 2: set annotation column (if the select input is present, pick first available)
  if (!is.null(annotation_col)) {
    tryCatch(
      app$set_inputs(
        `setupSidebar-Proteome_annotation_column` = annotation_col,
        wait_ = FALSE
      ),
      error = function(e) NULL
    )
    app$wait_for_idle(duration = 400)
  }

  # Submit GCT parameters
  wait_for_input_bound(app, "setupSidebar-submitGCTButton")
  app$click(input = "setupSidebar-submitGCTButton")
  app$wait_for_idle(duration = 2000, timeout = 30000)
}

# Complete single-label GCT wizard using dynamic label input name.
# The label input ID is: setupSidebar-Label_<filename>
complete_gct_wizard_dynamic <- function(app, file_name, label, annotation_col = NULL) {
  app$wait_for_idle(duration = 800, timeout = 20000)
  input_id <- paste0("setupSidebar-Label_", file_name)
  args <- list(wait_ = FALSE)
  args[[input_id]] <- label
  do.call(app$set_inputs, args)
  app$wait_for_idle(duration = 400)
  wait_for_input_bound(app, "setupSidebar-submitLabelsButton")
  app$click(input = "setupSidebar-submitLabelsButton")
  app$wait_for_idle(duration = 1000, timeout = 20000)
  if (!is.null(annotation_col)) {
    tryCatch({
      col_id <- paste0("setupSidebar-", label, "_annotation_column")
      col_args <- list(wait_ = FALSE)
      col_args[[col_id]] <- annotation_col
      do.call(app$set_inputs, col_args)
      app$wait_for_idle(duration = 400)
    }, error = function(e) NULL)
  }
  wait_for_input_bound(app, "setupSidebar-submitGCTButton")
  app$click(input = "setupSidebar-submitGCTButton")
  app$wait_for_idle(duration = 3000, timeout = 40000)
}

# Walk through the CSV/TSV/XLSX wizard (4 steps).
# Step 1: labels -> submitCSVExcelLabelsButton
# Step 2: identifier column -> submitCSVExcelIdentifiersButton
# Step 3: experimental design -> processCSVExcel button
# Step 4: GCT setup -> submitGCTButton
complete_csv_wizard <- function(app, file_name, label,
                                identifier_col = NULL,
                                annotation_col = NULL) {
  app$wait_for_idle(duration = 800, timeout = 20000)

  # Step 1: label — wait for the label input to be bound first (renderUI-rendered)
  label_id <- paste0("setupSidebar-CSVExcelLabel_", file_name)
  wait_for_input_bound(app, label_id, timeout = 20000)
  label_args <- list(wait_ = FALSE)
  label_args[[label_id]] <- label
  do.call(app$set_inputs, label_args)
  app$wait_for_idle(duration = 400)
  wait_for_input_bound(app, "setupSidebar-submitCSVExcelLabelsButton")
  app$click(input = "setupSidebar-submitCSVExcelLabelsButton")
  app$wait_for_idle(duration = 1500, timeout = 25000)

  # Step 2: identifier column (select first available if not specified)
  # identifierColumn_1 corresponds to the first (and only) file
  if (!is.null(identifier_col)) {
    tryCatch(
      app$set_inputs(`setupSidebar-identifierColumn_1` = identifier_col, wait_ = FALSE),
      error = function(e) NULL
    )
  }
  wait_for_input_bound(app, "setupSidebar-submitCSVExcelIdentifiersButton")
  app$wait_for_idle(duration = 400)
  app$click(input = "setupSidebar-submitCSVExcelIdentifiersButton")
  app$wait_for_idle(duration = 2000, timeout = 25000)

  # Step 3: upload experimental design file, then click "Process Files".
  # req(input$expDesignFile) in the observer requires this file before processing.
  exp_design_file <- get_test_file("experimental_design.csv")
  app$upload_file(`setupSidebar-expDesignFile` = exp_design_file)
  app$wait_for_idle(duration = 1000, timeout = 20000)
  wait_for_input_bound(app, "setupSidebar-processCSVExcel")
  app$click(input = "setupSidebar-processCSVExcel")
  app$wait_for_idle(duration = 3000, timeout = 40000)

  # Step 4: GCT setup
  if (!is.null(annotation_col)) {
    tryCatch({
      col_id <- paste0("setupSidebar-", label, "_annotation_column")
      col_args <- list(wait_ = FALSE)
      col_args[[col_id]] <- annotation_col
      do.call(app$set_inputs, col_args)
      app$wait_for_idle(duration = 400)
    }, error = function(e) NULL)
  }
  wait_for_input_bound(app, "setupSidebar-submitGCTButton")
  app$click(input = "setupSidebar-submitGCTButton")
  app$wait_for_idle(duration = 3000, timeout = 40000)
}

# Complete CSV wizard for multiple files (multi-omic).
complete_csv_wizard_multi <- function(app, file_names, labels, annotation_col = NULL) {
  app$wait_for_idle(duration = 800, timeout = 20000)

  # Step 1: labels — wait for the first label input to be bound before setting
  # any values, since these are renderUI-rendered and may not be bound immediately.
  first_label_id <- paste0("setupSidebar-CSVExcelLabel_", file_names[[1]])
  wait_for_input_bound(app, first_label_id, timeout = 20000)
  for (i in seq_along(file_names)) {
    label_id <- paste0("setupSidebar-CSVExcelLabel_", file_names[[i]])
    largs <- list(wait_ = FALSE)
    largs[[label_id]] <- labels[[i]]
    do.call(app$set_inputs, largs)
  }
  app$wait_for_idle(duration = 400)
  wait_for_input_bound(app, "setupSidebar-submitCSVExcelLabelsButton")
  app$click(input = "setupSidebar-submitCSVExcelLabelsButton")
  app$wait_for_idle(duration = 1500, timeout = 25000)

  # Step 2: identifier columns (one per file)
  wait_for_input_bound(app, "setupSidebar-submitCSVExcelIdentifiersButton")
  app$wait_for_idle(duration = 400)
  app$click(input = "setupSidebar-submitCSVExcelIdentifiersButton")
  app$wait_for_idle(duration = 2000, timeout = 25000)

  # Step 3: upload experimental design file, then click "Process Files".
  exp_design_file <- get_test_file("experimental_design.csv")
  app$upload_file(`setupSidebar-expDesignFile` = exp_design_file)
  app$wait_for_idle(duration = 1000, timeout = 20000)
  wait_for_input_bound(app, "setupSidebar-processCSVExcel")
  app$click(input = "setupSidebar-processCSVExcel")
  app$wait_for_idle(duration = 4000, timeout = 50000)

  # Step 4: GCT setup — navigate all omes
  n_omes <- length(labels)
  for (i in seq_len(n_omes)) {
    if (!is.null(annotation_col)) {
      tryCatch({
        col_id <- paste0("setupSidebar-", labels[[i]], "_annotation_column")
        col_args <- list(wait_ = FALSE)
        col_args[[col_id]] <- annotation_col
        do.call(app$set_inputs, col_args)
        app$wait_for_idle(duration = 400)
      }, error = function(e) NULL)
    }
    if (i < n_omes) {
      wait_for_input_bound(app, "setupSidebar-nextButton")
      app$click(input = "setupSidebar-nextButton")
      app$wait_for_idle(duration = 1000, timeout = 20000)
    } else {
      wait_for_input_bound(app, "setupSidebar-submitGCTButton")
      app$click(input = "setupSidebar-submitGCTButton")
      app$wait_for_idle(duration = 4000, timeout = 50000)
    }
  }
}

# Assert setup completed: GCTs_and_params export is non-NULL.
assert_setup_complete <- function(app) {
  result <- app$get_value(export = "GCTs_and_params")
  expect_false(
    is.null(result),
    info = "GCTs_and_params should be non-NULL after successful setup"
  )
  invisible(result)
}

# Assert n omes present in GCTs_and_params
assert_n_omes <- function(app, n) {
  result <- app$get_value(export = "GCTs_and_params")
  expect_false(is.null(result))
  expect_equal(length(result$GCTs), n,
               info = paste("Expected", n, "omes in GCTs_and_params"))
}

# ---------------------------------------------------------------------------
# Group A: Single-file upload
# ---------------------------------------------------------------------------

test_that("single GCT upload completes successfully", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(
    `setupSidebar-dataFiles` = get_test_file("mb-proteome-ratio-norm-NArm.gct")
  )
  complete_gct_wizard_dynamic(
    app,
    file_name = "mb-proteome-ratio-norm-NArm.gct",
    label     = "Proteome"
  )

  assert_setup_complete(app)
  assert_n_omes(app, 1)
})

test_that("single CSV upload completes successfully", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(
    `setupSidebar-dataFiles` = get_test_file("mb-proteome-ratio-norm-NArm.csv")
  )
  complete_csv_wizard(
    app,
    file_name = "mb-proteome-ratio-norm-NArm.csv",
    label     = "Proteome"
  )

  assert_setup_complete(app)
  assert_n_omes(app, 1)
})

test_that("single TSV upload completes successfully", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(
    `setupSidebar-dataFiles` = get_test_file("mb-proteome-ratio-norm-NArm.tsv")
  )
  complete_csv_wizard(
    app,
    file_name = "mb-proteome-ratio-norm-NArm.tsv",
    label     = "Proteome"
  )

  assert_setup_complete(app)
  assert_n_omes(app, 1)
})

test_that("single XLSX upload completes successfully", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(
    `setupSidebar-dataFiles` = get_test_file("mb-proteome-ratio-norm-NArm.xlsx")
  )
  complete_csv_wizard(
    app,
    file_name = "mb-proteome-ratio-norm-NArm.xlsx",
    label     = "Proteome"
  )

  assert_setup_complete(app)
  assert_n_omes(app, 1)
})

# ---------------------------------------------------------------------------
# Group B: Multi-omic upload, same extension (3 files per test)
# ---------------------------------------------------------------------------

test_that("multi-omic GCT upload (3 files) completes successfully", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(
    `setupSidebar-dataFiles` = c(
      get_test_file("mb-proteome-ratio-norm-NArm.gct"),
      get_test_file("mb-phosphoproteome-ratio-norm-NArm.gct"),
      get_test_file("mb-acetylome-ratio-norm-NArm.gct")
    )
  )
  app$wait_for_idle(duration = 1000, timeout = 25000)

  # Set labels for all 3 files
  app$set_inputs(
    `setupSidebar-Label_mb-proteome-ratio-norm-NArm.gct`       = "Proteome",
    `setupSidebar-Label_mb-phosphoproteome-ratio-norm-NArm.gct` = "Phosphoproteome",
    `setupSidebar-Label_mb-acetylome-ratio-norm-NArm.gct`       = "Acetylome",
    wait_ = FALSE
  )
  app$wait_for_idle(duration = 400)
  wait_for_input_bound(app, "setupSidebar-submitLabelsButton")
  app$click(input = "setupSidebar-submitLabelsButton")
  app$wait_for_idle(duration = 1500, timeout = 30000)

  # Navigate through all 3 GCT setup steps
  for (i in seq_len(2)) {
    wait_for_input_bound(app, "setupSidebar-nextButton")
    app$click(input = "setupSidebar-nextButton")
    app$wait_for_idle(duration = 1000, timeout = 20000)
  }
  wait_for_input_bound(app, "setupSidebar-submitGCTButton")
  app$click(input = "setupSidebar-submitGCTButton")
  app$wait_for_idle(duration = 5000, timeout = 60000)

  assert_setup_complete(app)
  assert_n_omes(app, 3)
})

test_that("multi-omic CSV upload (3 files) completes successfully", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(
    `setupSidebar-dataFiles` = c(
      get_test_file("mb-proteome-ratio-norm-NArm.csv"),
      get_test_file("mb-phosphoproteome-ratio-norm-NArm.csv"),
      get_test_file("mb-acetylome-ratio-norm-NArm.csv")
    )
  )
  complete_csv_wizard_multi(
    app,
    file_names = c(
      "mb-proteome-ratio-norm-NArm.csv",
      "mb-phosphoproteome-ratio-norm-NArm.csv",
      "mb-acetylome-ratio-norm-NArm.csv"
    ),
    labels = c("Proteome", "Phosphoproteome", "Acetylome")
  )

  assert_setup_complete(app)
  assert_n_omes(app, 3)
})

test_that("multi-omic TSV upload (3 files) completes successfully", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(
    `setupSidebar-dataFiles` = c(
      get_test_file("mb-proteome-ratio-norm-NArm.tsv"),
      get_test_file("mb-phosphoproteome-ratio-norm-NArm.tsv"),
      get_test_file("mb-acetylome-ratio-norm-NArm.tsv")
    )
  )
  complete_csv_wizard_multi(
    app,
    file_names = c(
      "mb-proteome-ratio-norm-NArm.tsv",
      "mb-phosphoproteome-ratio-norm-NArm.tsv",
      "mb-acetylome-ratio-norm-NArm.tsv"
    ),
    labels = c("Proteome", "Phosphoproteome", "Acetylome")
  )

  assert_setup_complete(app)
  assert_n_omes(app, 3)
})

test_that("multi-omic XLSX upload (3 files) completes successfully", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(
    `setupSidebar-dataFiles` = c(
      get_test_file("mb-proteome-ratio-norm-NArm.xlsx"),
      get_test_file("mb-phosphoproteome-ratio-norm-NArm.xlsx"),
      get_test_file("mb-acetylome-ratio-norm-NArm.xlsx")
    )
  )
  complete_csv_wizard_multi(
    app,
    file_names = c(
      "mb-proteome-ratio-norm-NArm.xlsx",
      "mb-phosphoproteome-ratio-norm-NArm.xlsx",
      "mb-acetylome-ratio-norm-NArm.xlsx"
    ),
    labels = c("Proteome", "Phosphoproteome", "Acetylome")
  )

  assert_setup_complete(app)
  assert_n_omes(app, 3)
})

# ---------------------------------------------------------------------------
# Group C: Multi-omic upload, mixed extensions
# ---------------------------------------------------------------------------

test_that("multi-omic mixed-extension upload (CSV + TSV + XLSX) completes successfully", {
  skip_if_no_shinytest2()

  # The app supports mixing CSV/TSV/XLSX in one batch (all non-GCT).
  # Mixing GCT with non-GCT is explicitly rejected by the upload validator.
  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(
    `setupSidebar-dataFiles` = c(
      get_test_file("mb-acetylome-ratio-norm-NArm.csv"),
      get_test_file("mb-phosphoproteome-ratio-norm-NArm.tsv"),
      get_test_file("mb-proteome-ratio-norm-NArm.xlsx")
    )
  )
  app$wait_for_idle(duration = 1500, timeout = 25000)

  complete_csv_wizard_multi(
    app,
    file_names = c(
      "mb-acetylome-ratio-norm-NArm.csv",
      "mb-phosphoproteome-ratio-norm-NArm.tsv",
      "mb-proteome-ratio-norm-NArm.xlsx"
    ),
    labels = c("Acetylome", "Phosphoproteome", "Proteome")
  )

  assert_setup_complete(app)
  assert_n_omes(app, 3)
})
