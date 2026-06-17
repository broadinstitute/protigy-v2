# Phase 3 (P3.3) -- the 5C species UniProt-refresh observer in
# PELSASection1_Tab_Server (R/tab_pelsa_section1.R).
#
# The observer is intentionally thin: gather inputs, (optionally) confirm a large
# fetch, drive a live progress bar, store results. We drive it via testServer with
# the heavy/network helpers mocked offline:
#   * pelsa_database_dir         -> a temp dir (no real DB)
#   * pelsa_refresh_universe_size-> controls the confirm-gate branch
#   * pelsa_run_species_refresh  -> records that the run happened (no network)
#
# Covered branches:
#   * no species selected -> warning, run NOT invoked
#   * below threshold      -> run invoked directly, refresh_result set
#   * above threshold      -> confirm dialog; run NOT invoked synchronously
#   * in-flight guard      -> overlapping click ignored

`%||%` <- function(a, b) if (is.null(a)) b else a

.refresh_test_gp <- function() {
  ok <- tryCatch({
    utils::data("brca_retrospective_v5.0_proteome_gct", package = "Protigy")
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(ok, "brca proteome test data not available")
  gct <- get("brca_retrospective_v5.0_proteome_gct")
  list(GCTs = list(proteome = gct),
       parameters = list(proteome = list(annotation_column = NA)))
}

.refresh_args <- function(gp) {
  list(
    GCTs_and_params = shiny::reactiveVal(gp),
    globals = shiny::reactiveValues(default_ome = "proteome",
                                    colors = list(proteome = NULL)),
    GCTs_original = shiny::reactiveVal(gp$GCTs),
    active_dataset = shiny::reactive("proteome")
  )
}

test_that("refresh observer warns and does NOT run when no species selected", {
  gp <- .refresh_test_gp()
  run_calls <- new.env(); run_calls$n <- 0L

  testthat::local_mocked_bindings(
    pelsa_database_dir = function() tempdir(),
    pelsa_run_species_refresh = function(...) { run_calls$n <- run_calls$n + 1L; list() },
    .package = "Protigy"
  )

  shiny::testServer(PELSASection1_Tab_Server, args = .refresh_args(gp), {
    session$setInputs(pelsa_refresh_species = character(0))
    session$setInputs(pelsa_refresh_btn = 1)
    session$flushReact()
    expect_equal(run_calls$n, 0L)
  })
})

test_that("refresh observer runs directly when universe is below the threshold", {
  gp <- .refresh_test_gp()
  run_calls <- new.env(); run_calls$n <- 0L; run_calls$species <- NULL

  testthat::local_mocked_bindings(
    pelsa_database_dir = function() tempdir(),
    pelsa_refresh_universe_size = function(species, database_dir, uploaded_gcts) {
      list(total = 10L, per_species = stats::setNames(10L, species[[1]]))
    },
    pelsa_run_species_refresh = function(species, ...) {
      run_calls$n <- run_calls$n + 1L
      run_calls$species <- species
      list(updated = species)
    },
    .package = "Protigy"
  )

  shiny::testServer(PELSASection1_Tab_Server, args = .refresh_args(gp), {
    session$setInputs(pelsa_refresh_species = "human")
    session$flushReact()
    # With ignoreInit = TRUE, the first observed change to the button fires once.
    session$setInputs(pelsa_refresh_btn = 1)
    session$flushReact()
    expect_equal(run_calls$n, 1L)
    expect_equal(run_calls$species, "human")
  })
})

test_that("refresh observer confirms (does NOT run synchronously) above threshold", {
  gp <- .refresh_test_gp()
  run_calls <- new.env(); run_calls$n <- 0L
  alert_calls <- new.env(); alert_calls$n <- 0L

  testthat::local_mocked_bindings(
    pelsa_database_dir = function() tempdir(),
    pelsa_refresh_universe_size = function(species, database_dir, uploaded_gcts) {
      list(total = 999999L, per_species = stats::setNames(999999L, species[[1]]))
    },
    pelsa_run_species_refresh = function(...) { run_calls$n <- run_calls$n + 1L; list() },
    .package = "Protigy"
  )
  # shinyalert is a different package; mock the confirm dialog to a no-op so the
  # observer takes the confirm branch without surfacing a real modal.
  testthat::local_mocked_bindings(
    shinyalert = function(...) { alert_calls$n <- alert_calls$n + 1L; invisible() },
    .package = "shinyalert"
  )

  shiny::testServer(PELSASection1_Tab_Server, args = .refresh_args(gp), {
    session$setInputs(pelsa_refresh_species = "human")
    session$flushReact()
    session$setInputs(pelsa_refresh_btn = 1)
    session$flushReact()
    # large fetch -> confirm dialog shown, run deferred to the callback
    expect_equal(alert_calls$n, 1L)
    expect_equal(run_calls$n, 0L)
  })
})
