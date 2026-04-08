# Helper utilities for shinytest2 integration tests.
# This file is auto-sourced by testthat before any test file runs.

# ---------------------------------------------------------------------------
# Skip guards
# ---------------------------------------------------------------------------

skip_if_no_shinytest2 <- function() {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  # Skip if no Chrome/Chromium binary is found
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (identical(chrome, "") || is.null(chrome)) {
    testthat::skip("No Chrome/Chromium found for shinytest2")
  }
  # Allow opt-out via environment variable for fast local development
  if (identical(Sys.getenv("PROTIGY_SKIP_SHINYTEST2"), "true")) {
    testthat::skip("PROTIGY_SKIP_SHINYTEST2=true")
  }
}

# ---------------------------------------------------------------------------
# Test data helpers (all data from inst/extdata/ exclusively)
# ---------------------------------------------------------------------------

#' Return the absolute path to a file in inst/extdata/
#'
#' @param basename Filename relative to inst/extdata/, e.g.
#'   "mb-proteome-ratio-norm-NArm.gct" or
#'   "spectronaut-ui-extdata/foo.tsv"
get_test_file <- function(basename) {
  path <- system.file(file.path("extdata", basename), package = "Protigy")
  if (identical(path, "")) {
    # Fallback for dev mode (package not installed)
    path <- testthat::test_path("..", "..", "inst", "extdata", basename)
  }
  path
}

# ---------------------------------------------------------------------------
# AppDriver factory
# ---------------------------------------------------------------------------

#' Create a shinytest2 AppDriver with consistent defaults.
#'
#' @param app_dir Path to the app directory (relative to tests/testthat/ or
#'   absolute).  Pass \code{test_path("apps/full-app")} from within test files.
#' @param ... Additional arguments forwarded to \code{AppDriver$new()}.
make_app_driver <- function(app_dir, ...) {
  shinytest2::AppDriver$new(
    app_dir      = app_dir,
    seed         = 42,
    load_timeout = 30000,
    timeout      = 15000,
    height       = 900,
    width        = 1400,
    ...
  )
}

# ---------------------------------------------------------------------------
# DOM / input helpers
# ---------------------------------------------------------------------------

#' Wait until a Shiny input element is bound in the browser (has the
#' .shiny-bound-input class).  This is needed for renderUI-rendered buttons
#' which may appear in the DOM before Shiny's JS binding runs.
#'
#' @param app   AppDriver instance
#' @param input_id  Full namespaced input id, e.g. "setupSidebar-submitGCTButton"
#' @param timeout   Maximum wait in ms (default 15000)
wait_for_input_bound <- function(app, input_id, timeout = 15000) {
  selector <- paste0("#", input_id, ".shiny-bound-input")
  js <- paste0(
    "document.querySelector('", selector, "') !== null"
  )
  app$wait_for_js(js, timeout = timeout)
}

#' Wait until a Shiny conditionalPanel containing a given element becomes
#' visible (display != 'none').  Uses wait_for_js so the poll loop runs in
#' the browser rather than a fixed sleep on the R side.
#'
#' @param app       AppDriver instance
#' @param selector  CSS selector for an element *inside* the conditionalPanel,
#'                  e.g. "#setupSidebar-Proteome_group_normalization"
#' @param timeout   Maximum wait in ms (default 5000)
wait_for_panel_visible <- function(app, selector, timeout = 5000) {
  js <- paste0(
    "(function() {",
    "  var el = document.querySelector('", selector, "');",
    "  if (!el) return false;",
    "  var panel = el.closest('.shiny-panel-conditional');",
    "  if (!panel) return true;",
    "  return panel.style.display !== 'none';",
    "})()"
  )
  app$wait_for_js(js, timeout = timeout)
}

#' Wait until a Shiny conditionalPanel containing a given element becomes
#' hidden (display == 'none').
#'
#' @param app       AppDriver instance
#' @param selector  CSS selector for an element *inside* the conditionalPanel
#' @param timeout   Maximum wait in ms (default 5000)
wait_for_panel_hidden <- function(app, selector, timeout = 5000) {
  js <- paste0(
    "(function() {",
    "  var el = document.querySelector('", selector, "');",
    "  if (!el) return true;",
    "  var panel = el.closest('.shiny-panel-conditional');",
    "  if (!panel) return false;",
    "  return panel.style.display === 'none';",
    "})()"
  )
  app$wait_for_js(js, timeout = timeout)
}
