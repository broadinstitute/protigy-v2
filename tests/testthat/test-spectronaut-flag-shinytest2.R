################################################################################
# Integration tests: Spectronaut feature flag (protigy.enable_spectronaut)
#
# Tests that:
# 1. With flag OFF: the condition setup UI is absent from the label step
# 2. With flag ON:  the condition setup checkbox appears, and uploading a
#    ConditionSetup file completes the Spectronaut preprocessing workflow.
#
# Test data (from inst/extdata/spectronaut-ui-extdata/):
#   - Pivot report: spectronaut_test_pivot.tsv
#   - Condition setup: spectronaut_test_condition_setup.tsv
################################################################################

library(testthat)

# Spectronaut test data paths
spectronaut_pivot_file <- function() {
  get_test_file(file.path(
    "spectronaut-ui-extdata",
    "spectronaut_test_pivot.tsv"
  ))
}

spectronaut_condition_setup_file <- function() {
  get_test_file(file.path(
    "spectronaut-ui-extdata",
    "spectronaut_test_condition_setup.tsv"
  ))
}

# The file_id for the Spectronaut pivot TSV (gsub "[^a-zA-Z0-9_]" -> "_")
spectronaut_file_id <- function() {
  gsub(
    "[^a-zA-Z0-9_]", "_",
    "spectronaut_test_pivot.tsv"
  )
}

# Helper: check if an element is NOT hidden by a Shiny conditionalPanel.
is_visible <- function(app, selector) {
  result <- tryCatch(
    app$run_js(paste0(
      "(function() {",
      "  var el = document.querySelector('", selector, "');",
      "  if (!el) return false;",
      "  var panel = el.closest('.shiny-panel-conditional');",
      "  if (!panel) return true;",
      "  return panel.style.display !== 'none';",
      "})()"
    )),
    error = function(e) FALSE
  )
  isTRUE(result)
}

# ---------------------------------------------------------------------------
# Test 1: Flag OFF  -  condition setup UI should be absent
# ---------------------------------------------------------------------------

test_that("condition setup UI is absent when protigy.enable_spectronaut = FALSE", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  # Upload a regular CSV to reach the label step
  app$upload_file(
    `setupSidebar-dataFiles` = get_test_file("mb-proteome-ratio-norm-NArm.csv")
  )
  app$wait_for_idle(duration = 1000, timeout = 20000)

  # The condition setup checkbox ID would be:
  # setupSidebar-use_condition_setup_mb_proteome_ratio_norm_NArm_csv
  file_id <- gsub("[^a-zA-Z0-9_]", "_", "mb-proteome-ratio-norm-NArm.csv")
  cond_checkbox_id <- paste0("#setupSidebar-use_condition_setup_", file_id)

  # Element should not exist in the DOM when flag is OFF
  element_exists <- tryCatch(
    app$run_js(paste0(
      "document.querySelector('", cond_checkbox_id, "') !== null"
    )),
    error = function(e) FALSE
  )
  expect_false(
    isTRUE(element_exists),
    info = "Spectronaut condition setup checkbox should not exist when flag is OFF"
  )
})

# ---------------------------------------------------------------------------
# Test 2: Flag ON  -  condition setup UI should appear and work
# ---------------------------------------------------------------------------

test_that("condition setup checkbox appears when protigy.enable_spectronaut = TRUE", {
  skip_if_no_shinytest2()
  # use_condition_setup UI not yet implemented in the app  -  skip until feature lands
  skip("use_condition_setup_<file_id> checkbox not yet implemented in sidebar_setup")

  app <- make_app_driver(testthat::test_path("apps/full-app-spectronaut"))
  on.exit(app$stop(), add = TRUE)

  # Upload the Spectronaut pivot report TSV
  app$upload_file(
    `setupSidebar-dataFiles` = spectronaut_pivot_file()
  )
  app$wait_for_idle(duration = 1500, timeout = 25000)

  fid <- spectronaut_file_id()
  checkbox_selector  <- paste0("#setupSidebar-use_condition_setup_", fid)
  file_input_selector <- paste0("#setupSidebar-conditionSetupFile_", fid)

  # The condition setup checkbox should now exist in the DOM
  element_exists <- tryCatch(
    app$run_js(paste0(
      "document.querySelector('", checkbox_selector, "') !== null"
    )),
    error = function(e) FALSE
  )
  expect_true(
    isTRUE(element_exists),
    info = "Spectronaut condition setup checkbox should exist when flag is ON"
  )

  # Before checking the box, the file input should not be visible
  expect_false(
    is_visible(app, file_input_selector),
    info = "Condition setup file input should be hidden before checkbox is checked"
  )

  # Check the condition setup checkbox
  checkbox_input_id <- paste0("setupSidebar-use_condition_setup_", fid)
  cbox_args <- list(TRUE)
  names(cbox_args) <- checkbox_input_id
  do.call(app$set_inputs, cbox_args)
  app$wait_for_idle(duration = 600)

  # The condition setup file input should now be visible
  expect_true(
    is_visible(app, file_input_selector),
    info = "Condition setup file input should be visible after checkbox is checked"
  )
})

test_that("Spectronaut workflow completes with condition setup file", {
  skip_if_no_shinytest2()
  # conditionSetupFile UI not yet implemented in the app  -  skip until feature lands
  skip("Spectronaut condition setup workflow not yet implemented in sidebar_setup")

  app <- make_app_driver(testthat::test_path("apps/full-app-spectronaut"))
  on.exit(app$stop(), add = TRUE)

  pivot_file    <- spectronaut_pivot_file()
  cond_file     <- spectronaut_condition_setup_file()
  fid           <- spectronaut_file_id()

  # Upload the Spectronaut pivot TSV
  app$upload_file(`setupSidebar-dataFiles` = pivot_file)
  app$wait_for_idle(duration = 1500, timeout = 25000)

  # Enable condition setup
  checkbox_input_id <- paste0("setupSidebar-use_condition_setup_", fid)
  cbox_args <- list(TRUE)
  names(cbox_args) <- checkbox_input_id
  do.call(app$set_inputs, cbox_args)
  app$wait_for_idle(duration = 600)

  # Upload the condition setup file
  cond_file_input_id <- paste0("setupSidebar-conditionSetupFile_", fid)
  cond_args <- list(cond_file)
  names(cond_args) <- cond_file_input_id
  do.call(app$upload_file, cond_args)
  app$wait_for_idle(duration = 2000, timeout = 30000)

  # Proceed: set label for the file
  pivot_file_name <- basename(pivot_file)
  label_id <- paste0("setupSidebar-CSVExcelLabel_", pivot_file_name)
  largs <- list(wait_ = FALSE)
  largs[[label_id]] <- "Proteome"
  do.call(app$set_inputs, largs)
  app$wait_for_idle(duration = 400)
  app$click(input = "setupSidebar-submitCSVExcelLabelsButton")
  app$wait_for_idle(duration = 2000, timeout = 30000)

  # Step 2: identifier column
  app$click(input = "setupSidebar-submitCSVExcelIdentifiersButton")
  app$wait_for_idle(duration = 2000, timeout = 25000)

  # Step 3: process files (experimental design pre-populated from condition setup)
  app$click(input = "setupSidebar-processCSVExcel")
  app$wait_for_idle(duration = 4000, timeout = 50000)

  # Step 4: GCT setup
  app$click(input = "setupSidebar-submitGCTButton")
  app$wait_for_idle(duration = 4000, timeout = 50000)

  # Verify setup completed
  result <- app$get_value(export = "GCTs_and_params")
  expect_false(
    is.null(result),
    info = "GCTs_and_params should be non-NULL after Spectronaut workflow completes"
  )
  expect_equal(length(result$GCTs), 1)
})
