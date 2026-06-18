################################################################################
# Integration tests: conditionalPanel visibility
#
# Tests the 20 conditionalPanel instances, prioritised by risk:
#
# Tier 1 (highest)  -  sidebar_setup_helpers_shiny.R, use ns = ns pattern
#   - data_normalization != 'None' -> group_normalization checkbox visible
#   - data_normalization != 'None' && group_normalization -> column selector visible
#   - data_filter == 'StdDev' -> percentile input visible
#   - sample_filter_enabled -> column selector visible
#   - sample_filter_enabled && column != '' -> values selector visible
#   - row_filter_enabled -> column selector visible
#   - row_filter_enabled && column != '' -> values selector visible
#   - 2-component normalization absent for >20 samples
#   - intensity_data toggle updates normalization choices and max_missing bounds
#
# Tier 2  -  sidebar_setup_helpers_csv-excel-processing.R
#   - delimit_id_<file_id> toggle (CSV step 2)
#   - use_condition_setup_<file_id> toggle (requires spectronaut flag ON)
#
# All tests reach the GCT setup step by uploading a single GCT file.
################################################################################

library(testthat)

# ---------------------------------------------------------------------------
# Shared fixture: upload proteome GCT and reach the GCT setup step
# ---------------------------------------------------------------------------

# Returns an AppDriver that is at the GCT parameters step for "Proteome" label.
reach_gct_setup_step <- function() {
  app <- make_app_driver(testthat::test_path("apps/full-app"))
  app$upload_file(
    `setupSidebar-dataFiles` = get_test_file("mb-proteome-ratio-norm-NArm.gct")
  )
  app$wait_for_idle(duration = 800, timeout = 20000)
  app$set_inputs(`setupSidebar-Label_mb-proteome-ratio-norm-NArm.gct` = "Proteome",
                 wait_ = FALSE)
  app$wait_for_idle(duration = 400)
  wait_for_input_bound(app, "setupSidebar-submitLabelsButton")
  app$click(input = "setupSidebar-submitLabelsButton")
  wait_for_input_bound(app, "setupSidebar-submitGCTButton", timeout = 25000)
  app$wait_for_idle(duration = 1500, timeout = 25000)
  app
}

# is_panel_visible() is defined in helper-shinytest2.R and uses getElementById
# (safe for IDs containing dots). Kept as a local alias for readability.
is_visible <- function(app, input_id) is_panel_visible(app, input_id)

# ---------------------------------------------------------------------------
# Tier 1: sidebar_setup_helpers_shiny.R conditionalPanels
# ---------------------------------------------------------------------------

test_that("data_normalization != None reveals group normalization checkbox", {
  skip_if_no_shinytest2()
  app <- reach_gct_setup_step()
  on.exit(app$stop(), add = TRUE)

  # Default: data_normalization is "Median" (non-None), so group normalization
  # checkbox should already be visible. First set to None, verify hidden.
  # wait_for_panel_hidden/visible throw on timeout  -  that IS the assertion.
  app$set_inputs(`setupSidebar-Proteome_data_normalization` = "None", wait_ = FALSE)
  expect_no_error(
    wait_for_panel_hidden(app, "setupSidebar-Proteome_group_normalization"),
    message = "Group normalization checkbox should be hidden when data_normalization = None"
  )

  # Now set to a non-None value, verify visible
  app$set_inputs(`setupSidebar-Proteome_data_normalization` = "Median", wait_ = FALSE)
  expect_no_error(
    wait_for_panel_visible(app, "setupSidebar-Proteome_group_normalization"),
    message = "Group normalization checkbox should be visible when data_normalization != None"
  )
})

test_that("group normalization checkbox reveals column selector", {
  skip_if_no_shinytest2()
  app <- reach_gct_setup_step()
  on.exit(app$stop(), add = TRUE)

  # Ensure normalization is non-None so the group normalization checkbox is visible
  app$set_inputs(`setupSidebar-Proteome_data_normalization` = "Median", wait_ = FALSE)
  wait_for_panel_visible(app, "setupSidebar-Proteome_group_normalization")

  # Disable group normalization  -  column selector should be hidden
  app$set_inputs(`setupSidebar-Proteome_group_normalization` = FALSE, wait_ = FALSE)
  expect_no_error(
    wait_for_panel_hidden(app, "setupSidebar-Proteome_group_normalization_column"),
    message = "Group normalization column selector should be hidden when group_normalization = FALSE"
  )

  # Enable group normalization  -  column selector should appear
  app$set_inputs(`setupSidebar-Proteome_group_normalization` = TRUE, wait_ = FALSE)
  expect_no_error(
    wait_for_panel_visible(app, "setupSidebar-Proteome_group_normalization_column"),
    message = "Group normalization column selector should be visible when group_normalization = TRUE"
  )
})

test_that("data_filter StdDev reveals percentile input", {
  skip_if_no_shinytest2()
  app <- reach_gct_setup_step()
  on.exit(app$stop(), add = TRUE)

  # None selected  -  percentile input should be hidden
  app$set_inputs(`setupSidebar-Proteome_data_filter` = "None", wait_ = FALSE)
  expect_no_error(
    wait_for_panel_hidden(app, "setupSidebar-Proteome_data_filter_sd_pct"),
    message = "Percentile input should be hidden when data_filter = None"
  )

  # StdDev selected  -  percentile input should appear
  app$set_inputs(`setupSidebar-Proteome_data_filter` = "StdDev", wait_ = FALSE)
  expect_no_error(
    wait_for_panel_visible(app, "setupSidebar-Proteome_data_filter_sd_pct"),
    message = "Percentile input should be visible when data_filter = StdDev"
  )
})

test_that("sample_filter_enabled checkbox reveals column selector", {
  skip_if_no_shinytest2()
  app <- reach_gct_setup_step()
  on.exit(app$stop(), add = TRUE)

  # Disable sample filter
  app$set_inputs(`setupSidebar-Proteome_sample_filter_enabled` = FALSE, wait_ = FALSE)
  expect_no_error(
    wait_for_panel_hidden(app, "setupSidebar-Proteome_sample_filter_column"),
    message = "Sample filter column selector should be hidden when filter disabled"
  )

  # Enable sample filter
  app$set_inputs(`setupSidebar-Proteome_sample_filter_enabled` = TRUE, wait_ = FALSE)
  expect_no_error(
    wait_for_panel_visible(app, "setupSidebar-Proteome_sample_filter_column"),
    message = "Sample filter column selector should be visible when filter enabled"
  )
})

test_that("selecting a sample_filter_column reveals values selector", {
  skip_if_no_shinytest2()
  app <- reach_gct_setup_step()
  on.exit(app$stop(), add = TRUE)

  # Enable filter and select a column
  app$set_inputs(`setupSidebar-Proteome_sample_filter_enabled` = TRUE, wait_ = FALSE)
  app$wait_for_idle(duration = 600)
  # The dataset has a "Type" column  -  use first available column
  # The values selector should appear once a non-empty column is selected
  current_col <- app$get_value(input = "setupSidebar-Proteome_sample_filter_column")
  if (is.null(current_col) || identical(current_col, "")) {
    # Set to first available column (Type is present in the mb-proteome GCT cdesc)
    tryCatch(
      app$set_inputs(`setupSidebar-Proteome_sample_filter_column` = "Type", wait_ = FALSE),
      error = function(e) NULL
    )
    app$wait_for_idle(duration = 600)
  }
  # If column is now set, values selector should be visible
  col_val <- app$get_value(input = "setupSidebar-Proteome_sample_filter_column")
  if (!is.null(col_val) && nzchar(col_val)) {
    expect_true(
      is_visible(app, "setupSidebar-Proteome_sample_filter_values"),
      info = "Sample filter values selector should be visible when column is selected"
    )
  }

  # Clear the column  -  values selector should hide
  app$set_inputs(`setupSidebar-Proteome_sample_filter_column` = "", wait_ = FALSE)
  app$wait_for_idle(duration = 600)
  expect_false(
    is_visible(app, "setupSidebar-Proteome_sample_filter_values"),
    info = "Sample filter values selector should be hidden when no column selected"
  )
})

test_that("row_filter_enabled checkbox reveals column selector", {
  skip_if_no_shinytest2()
  app <- reach_gct_setup_step()
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(`setupSidebar-Proteome_row_filter_enabled` = FALSE, wait_ = FALSE)
  expect_no_error(
    wait_for_panel_hidden(app, "setupSidebar-Proteome_row_filter_column"),
    message = "Row filter column selector should be hidden when filter disabled"
  )

  app$set_inputs(`setupSidebar-Proteome_row_filter_enabled` = TRUE, wait_ = FALSE)
  expect_no_error(
    wait_for_panel_visible(app, "setupSidebar-Proteome_row_filter_column"),
    message = "Row filter column selector should be visible when filter enabled"
  )
})

test_that("selecting a row_filter_column reveals values selector", {
  skip_if_no_shinytest2()
  app <- reach_gct_setup_step()
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(`setupSidebar-Proteome_row_filter_enabled` = TRUE, wait_ = FALSE)
  app$wait_for_idle(duration = 600)

  # Try to select a row metadata column (e.g., "GeneSymbol" or similar)
  tryCatch(
    app$set_inputs(`setupSidebar-Proteome_row_filter_column` = "Species", wait_ = FALSE),
    error = function(e) NULL
  )
  app$wait_for_idle(duration = 600)

  col_val <- app$get_value(input = "setupSidebar-Proteome_row_filter_column")
  if (!is.null(col_val) && nzchar(col_val)) {
    expect_true(
      is_visible(app, "setupSidebar-Proteome_row_filter_values"),
      info = "Row filter values selector should be visible when column is selected"
    )
  }

  app$set_inputs(`setupSidebar-Proteome_row_filter_column` = "", wait_ = FALSE)
  app$wait_for_idle(duration = 600)
  expect_false(
    is_visible(app, "setupSidebar-Proteome_row_filter_values"),
    info = "Row filter values selector should be hidden when no column selected"
  )
})

test_that("2-component normalization absent for GCT with >20 samples", {
  skip_if_no_shinytest2()
  # The mb-proteome GCT has >20 samples, so "2-component" should not appear
  # in the data_normalization select input choices.
  app <- reach_gct_setup_step()
  on.exit(app$stop(), add = TRUE)

  # Read available options for data_normalization via JavaScript.
  # app$run_js() returns a list when the JS expression evaluates to an array,
  # so coerce to character vector before using %in%.
  options_js <- tryCatch(
    as.character(unlist(app$run_js(paste0(
      "Array.from(document.querySelectorAll(",
      "  '#setupSidebar-Proteome_data_normalization option'",
      ")).map(o => o.value)"
    )))),
    error = function(e) character(0)
  )

  if (length(options_js) > 0) {
    expect_false(
      "2-component" %in% options_js,
      info = "2-component normalization should not appear for datasets with >20 samples"
    )
    expect_true("None" %in% options_js)
    expect_true("Median" %in% options_js)
  }
})

test_that("intensity data toggle updates normalization choices and max missing bounds", {
  skip_if_no_shinytest2()
  app <- reach_gct_setup_step()
  on.exit(app$stop(), add = TRUE)

  get_norm_choices <- function() {
    tryCatch(
      as.character(unlist(app$run_js(paste0(
        "Array.from(document.querySelectorAll(",
        "  '#setupSidebar-Proteome_data_normalization option'",
        ")).map(o => o.value)"
      )))),
      error = function(e) character(0)
    )
  }

  get_max_missing_max <- function() {
    tryCatch(
      suppressWarnings(as.numeric(app$run_js(
        "document.getElementById('setupSidebar-Proteome_max_missing').max"
      ))),
      error = function(e) NA_real_
    )
  }

  # Intensity OFF -> "No" branch: should include plain "Median", max missing up to 100.
  app$set_inputs(`setupSidebar-Proteome_intensity_data` = FALSE, wait_ = FALSE)
  app$wait_for_idle(duration = 800, timeout = 20000)
  choices_no <- get_norm_choices()
  max_no <- get_max_missing_max()

  if (length(choices_no) > 0) {
    expect_true(
      "Median" %in% choices_no,
      info = "When intensity_data is FALSE, normalization should include plain 'Median'"
    )
  }
  if (!is.na(max_no)) {
    expect_equal(
      max_no, 100,
      info = "When intensity_data is FALSE, max_missing max should be 100"
    )
  }

  # Intensity ON -> "Yes" branch: plain "Median" should disappear, max missing max = 99.
  app$set_inputs(`setupSidebar-Proteome_intensity_data` = TRUE, wait_ = FALSE)
  app$wait_for_idle(duration = 800, timeout = 20000)
  choices_yes <- get_norm_choices()
  max_yes <- get_max_missing_max()

  if (length(choices_yes) > 0) {
    expect_false(
      "Median" %in% choices_yes,
      info = "When intensity_data is TRUE, normalization should not include plain 'Median'"
    )
    expect_true(
      "Median (non-zero)" %in% choices_yes,
      info = "When intensity_data is TRUE, normalization should include 'Median (non-zero)'"
    )
  }
  if (!is.na(max_yes)) {
    expect_equal(
      max_yes, 99,
      info = "When intensity_data is TRUE, max_missing max should be 99"
    )
  }
})

# ---------------------------------------------------------------------------
# INT-1: toggling intensity must NOT lose in-progress edits.
#
# The intensity toggle observer no longer calls collectInputs() (which used to
# write parameters_internal_reactive and force a full setup-panel rebuild / grey-out).
# This test guards the safety property the removal must preserve: an in-progress
# edit the user made BEFORE toggling must still be present AFTER toggling (the
# widget must not be reset/reverted), AND the toggle's own job (recomputing the
# normalization choices + max_missing bounds from the live checkbox) must still work.
# ---------------------------------------------------------------------------
test_that("INT-1: intensity toggle preserves in-progress edits and still recomputes choices", {
  skip_if_no_shinytest2()
  app <- reach_gct_setup_step()
  on.exit(app$stop(), add = TRUE)

  get_norm_choices <- function() {
    tryCatch(
      as.character(unlist(app$run_js(paste0(
        "Array.from(document.querySelectorAll(",
        "  '#setupSidebar-Proteome_data_normalization option'",
        ")).map(o => o.value)"
      )))),
      error = function(e) character(0)
    )
  }

  # 1. Make an in-progress edit that the toggle's collectInputs() USED to persist:
  #    enable the sample filter (a boolean that controls a conditionalPanel).
  app$set_inputs(`setupSidebar-Proteome_sample_filter_enabled` = TRUE, wait_ = FALSE)
  app$wait_for_idle(duration = 600, timeout = 20000)
  expect_true(
    is_visible(app, "setupSidebar-Proteome_sample_filter_column"),
    info = "Precondition: enabling sample filter reveals its column selector"
  )

  # 2. Toggle intensity data. Previously this forced a full panel rebuild; the
  #    sample_filter_enabled edit must survive the toggle (no reset to default).
  app$set_inputs(`setupSidebar-Proteome_intensity_data` = TRUE, wait_ = FALSE)
  app$wait_for_idle(duration = 800, timeout = 20000)

  # 3a. The edit is preserved: the sample-filter column selector is STILL visible
  #     (i.e. sample_filter_enabled was NOT reverted to its default of FALSE).
  expect_true(
    is_visible(app, "setupSidebar-Proteome_sample_filter_column"),
    info = "INT-1: sample_filter_enabled edit must survive an intensity toggle (no rebuild reset)"
  )

  # 3b. The toggle still did its job: intensity ON ('Yes' branch) drops plain
  #     'Median' from the normalization choices.
  choices_yes <- get_norm_choices()
  if (length(choices_yes) > 0) {
    expect_false(
      "Median" %in% choices_yes,
      info = "INT-1: toggle still recomputes normalization choices (plain 'Median' gone when intensity ON)"
    )
  }

  # 3c. CRITICAL regression guard (the edit-then-toggle case): a normalization
  #     choice the user picked, that is VALID in both intensity branches, must
  #     survive a toggle. The bug (bare collectInputs removal reading STALE stored
  #     params) reset such a pick back to the stored pre-edit value. "Quantile" is
  #     present in both the "Yes" and "No" branches, so toggling must NOT change it.
  get_norm_selected <- function() {
    tryCatch(
      as.character(app$get_value(input = "setupSidebar-Proteome_data_normalization")),
      error = function(e) NA_character_
    )
  }
  # Pick "Quantile" while intensity is currently ON, then toggle OFF.
  if ("Quantile" %in% choices_yes) {
    app$set_inputs(`setupSidebar-Proteome_data_normalization` = "Quantile", wait_ = FALSE)
    app$wait_for_idle(duration = 500, timeout = 20000)
    app$set_inputs(`setupSidebar-Proteome_intensity_data` = FALSE, wait_ = FALSE)
    app$wait_for_idle(duration = 800, timeout = 20000)
    expect_equal(
      get_norm_selected(), "Quantile",
      info = "INT-1: an in-progress normalization pick valid in both branches must survive a toggle (not reset to stale stored value)"
    )
    # restore intensity ON for the steps below
    app$set_inputs(`setupSidebar-Proteome_intensity_data` = TRUE, wait_ = FALSE)
    app$wait_for_idle(duration = 800, timeout = 20000)
  }

  # 4. Toggle back OFF and confirm the edit STILL survives and choices recompute.
  app$set_inputs(`setupSidebar-Proteome_intensity_data` = FALSE, wait_ = FALSE)
  app$wait_for_idle(duration = 800, timeout = 20000)
  expect_true(
    is_visible(app, "setupSidebar-Proteome_sample_filter_column"),
    info = "INT-1: edit survives a second toggle as well"
  )
  choices_no <- get_norm_choices()
  if (length(choices_no) > 0) {
    expect_true(
      "Median" %in% choices_no,
      info = "INT-1: toggling intensity OFF restores plain 'Median' in normalization choices"
    )
  }
})

# ---------------------------------------------------------------------------
# Tier 2: sidebar_setup_helpers_csv-excel-processing.R
# ---------------------------------------------------------------------------

test_that("CSV identifier delimiter toggle reveals source column input", {
  skip_if_no_shinytest2()
  # delimit_id UI is not yet implemented in the app  -  skip until feature lands
  skip("delimit_id_<file_id> checkbox not yet implemented in sidebar_setup")
  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  # Upload a CSV file and proceed to the identifier column step
  app$upload_file(
    `setupSidebar-dataFiles` = get_test_file("mb-proteome-ratio-norm-NArm.csv")
  )
  app$wait_for_idle(duration = 800, timeout = 20000)

  # Step 1: label
  app$set_inputs(
    `setupSidebar-CSVExcelLabel_mb-proteome-ratio-norm-NArm.csv` = "Proteome",
    wait_ = FALSE
  )
  app$wait_for_idle(duration = 400)
  app$click(input = "setupSidebar-submitCSVExcelLabelsButton")
  app$wait_for_idle(duration = 1500, timeout = 25000)

  # Now on Step 2 (identifier column). The "Split identifier column" checkbox
  # has ID: setupSidebar-delimit_id_<file_id>
  # file_id is the sanitized file name: mb_proteome_ratio_norm_NArm_csv
  file_id <- "mb_proteome_ratio_norm_NArm_csv"
  checkbox_id  <- paste0("setupSidebar-delimit_id_", file_id)
  source_col_id <- paste0("setupSidebar-id_source_column_", file_id)

  # Check that source column is hidden when checkbox is unchecked
  unchecked_args <- list(wait_ = FALSE)
  unchecked_args[[checkbox_id]] <- FALSE
  do.call(app$set_inputs, unchecked_args)
  app$wait_for_idle(duration = 600)
  expect_false(
    is_visible(app, source_col_id),
    info = "Source column input should be hidden when delimiter checkbox is unchecked"
  )

  # Check it  -  source column input should appear
  checked_args <- list(wait_ = FALSE)
  checked_args[[checkbox_id]] <- TRUE
  do.call(app$set_inputs, checked_args)
  app$wait_for_idle(duration = 600)
  expect_true(
    is_visible(app, source_col_id),
    info = "Source column input should be visible when delimiter checkbox is checked"
  )
})
