################################################################################
# Smoke tests: verify the app launches and renders its initial state correctly.
# These tests do not upload any data  -  they only confirm app startup health.
################################################################################

library(testthat)

test_that("app launches and renders initial state without errors", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle(duration = 500, timeout = 20000)

  # Title contains "ProTIGY"  -  check the dashboard header logo, not the <title> tag
  # (the DOM contains multiple <title> elements from shinydashboard modules)
  title <- app$get_text(".main-header .logo")
  expect_match(title, "ProTIGY", ignore.case = TRUE)

  # Sidebar file input is present
  file_input_label <- app$get_text("#setupSidebar-dataFiles-label")
  expect_false(is.null(file_input_label))
  expect_gt(nchar(trimws(file_input_label)), 0)

  # No data is loaded yet  -  GCTs_and_params export should be NULL
  all_exports <- app$get_values(export = TRUE)
  if (!is.null(all_exports$export$GCTs_and_params)) {
    # If exported, should be NULL at startup
    expect_null(all_exports$export$GCTs_and_params)
  }
})

test_that("spectronaut fixture also launches cleanly", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app-spectronaut"))
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle(duration = 500, timeout = 20000)

  title <- app$get_text(".main-header .logo")
  expect_match(title, "ProTIGY", ignore.case = TRUE)
})
