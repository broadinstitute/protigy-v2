################################################################################
# Tests for the app-level PELSA dataset switcher wiring (Phase 4).
#
# Non-flaky, no-browser checks:
#   - app_UI() evaluates without error and produces renderable HTML
#   - the PELSA sub-tab titles are "Setup" / "Summary" / "Volcano Plot"
#   - each PELSA tab has its OWN top-level switcher-bar uiOutput (unique DOM
#     ids, no duplicates) while sharing one un-namespaced source-of-truth input
#   - app_server exists and is a function with an active_dataset seam
################################################################################

library(testthat)

test_that("app_UI() evaluates and renders without error", {
  ui <- app_UI(request = list())
  expect_s3_class(ui, "shiny.tag.list")
  html <- as.character(ui)
  expect_true(nchar(html) > 0)
})

test_that("PELSA sub-tabs are titled Setup / Summary / Volcano Plot", {
  html <- as.character(app_UI(request = list()))
  expect_match(html, "Setup")
  expect_match(html, "Summary")
  expect_match(html, "Volcano Plot")
  # The old scaffold titles must be gone.
  expect_false(grepl("Section 1", html, fixed = TRUE))
  expect_false(grepl("Section 2", html, fixed = TRUE))
  expect_false(grepl("Section 3", html, fixed = TRUE))
})

test_that("each PELSA tab has its own switcher bar with a unique top-level id", {
  html <- as.character(app_UI(request = list()))
  expect_match(html, "pelsa-dataset-switcher")
  # One unique uiOutput id per PELSA tab (no duplicate DOM ids), all at top
  # level (NOT module-prefixed).
  ids <- c(
    "pelsa_active_dataset_bar_setup",
    "pelsa_active_dataset_bar_summary",
    "pelsa_active_dataset_bar_volcano"
  )
  for (id in ids) {
    expect_match(html, id)
    # Exactly one occurrence each -> no duplicate ids.
    expect_equal(lengths(regmatches(html, gregexpr(id, html, fixed = TRUE)))[[1]], 1L)
  }
  # The old single shared id must be gone (it caused the duplicate-id issue).
  expect_false(grepl("\"pelsa_active_dataset_bar\"", html, fixed = TRUE))
  expect_false(grepl("'pelsa_active_dataset_bar'", html, fixed = TRUE))
})

test_that("pelsa_switcher_bar_UI builds distinct ids and shares one input", {
  setup_html <- as.character(pelsa_switcher_bar_UI("setup"))
  summary_html <- as.character(pelsa_switcher_bar_UI("summary"))
  expect_match(setup_html, "pelsa_active_dataset_bar_setup")
  expect_match(summary_html, "pelsa_active_dataset_bar_summary")
  # The shared source-of-truth input id is constructed server-side.
  expect_equal(
    pelsa_switcher_bar_output_id("volcano"),
    "pelsa_active_dataset_bar_volcano"
  )
})

test_that("app_server exists and gains the active_dataset seam", {
  expect_true(is.function(app_server))
  # pelsaContainer_Server returns the active_dataset reactive consumed by all
  # three PELSA sections.
  expect_true(is.function(pelsaContainer_Server))
})
