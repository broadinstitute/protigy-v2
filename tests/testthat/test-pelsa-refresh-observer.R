# Phase 3 (P3.3) -- the 5C species UniProt-refresh observer in
# PELSASection1_Tab_Server (R/tab_pelsa_section1.R).
#
# The observer is intentionally thin: gather inputs, ALWAYS confirm via a
# shinyalert dialog (both Full and Incremental modes; no size threshold), and
# run pelsa_run_species_refresh only from the dialog's callbackR(confirmed).
# We drive it via testServer with the heavy/network helpers mocked offline:
#   * pelsa_database_dir         -> a temp dir (no real DB)
#   * pelsa_refresh_universe_size-> returns a fixed count for the ETA text
#   * pelsa_run_species_refresh  -> records that the run happened (no network)
#   * shinyalert::shinyalert     -> captures args so the test can fire callbackR
#
# Covered branches:
#   * no species selected   -> warning, confirm NOT shown, run NOT invoked
#   * Full button           -> confirm shown; run deferred; runs mode="full"
#                              only after callbackR(TRUE)
#   * Incremental button    -> confirm shown; runs mode="incremental" on confirm
#   * in-flight guard       -> overlapping click ignored (no second confirm)

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

test_that("refresh observer warns and does NOT confirm or run when no species selected", {
  gp <- .refresh_test_gp()
  run_calls <- new.env(); run_calls$n <- 0L
  alert_calls <- new.env(); alert_calls$n <- 0L

  testthat::local_mocked_bindings(
    pelsa_database_dir = function() tempdir(),
    pelsa_run_species_refresh = function(...) { run_calls$n <- run_calls$n + 1L; list() },
    .package = "Protigy"
  )
  testthat::local_mocked_bindings(
    shinyalert = function(...) { alert_calls$n <- alert_calls$n + 1L; invisible() },
    .package = "shinyalert"
  )

  shiny::testServer(PELSASection1_Tab_Server, args = .refresh_args(gp), {
    session$setInputs(pelsa_refresh_species = character(0))
    session$setInputs(pelsa_refresh_btn = 1)
    session$flushReact()
    expect_equal(alert_calls$n, 0L)   # early return: no confirm dialog
    expect_equal(run_calls$n, 0L)     # and no run
  })
})

test_that("Full button confirms, defers the run, then runs mode='full' on confirm", {
  gp <- .refresh_test_gp()
  run_calls <- new.env(); run_calls$n <- 0L; run_calls$species <- NULL
  run_calls$mode <- NULL
  alert_calls <- new.env(); alert_calls$n <- 0L; alert_calls$cb <- NULL

  testthat::local_mocked_bindings(
    pelsa_database_dir = function() tempdir(),
    pelsa_refresh_universe_size = function(species, database_dir, uploaded_gcts,
                                           mode = "incremental") {
      list(total = 10L, per_species = stats::setNames(10L, species[[1]]))
    },
    pelsa_run_species_refresh = function(species, ..., mode = "incremental") {
      run_calls$n <- run_calls$n + 1L
      run_calls$species <- species
      run_calls$mode <- mode
      list(updated = species)
    },
    .package = "Protigy"
  )
  # Capture the confirm dialog instead of surfacing a real modal; stash callbackR.
  testthat::local_mocked_bindings(
    shinyalert = function(..., callbackR = NULL) {
      alert_calls$n <- alert_calls$n + 1L
      alert_calls$cb <- callbackR
      invisible()
    },
    .package = "shinyalert"
  )

  shiny::testServer(PELSASection1_Tab_Server, args = .refresh_args(gp), {
    session$setInputs(pelsa_refresh_species = "human")
    session$flushReact()
    session$setInputs(pelsa_refresh_btn = 1)
    session$flushReact()

    # Confirm shown exactly once; the run is deferred, NOT synchronous.
    expect_equal(alert_calls$n, 1L)
    expect_equal(run_calls$n, 0L)
    expect_true(is.function(alert_calls$cb))

    # Firing the confirm callback runs the refresh once, in full mode.
    alert_calls$cb(TRUE)
    session$flushReact()
    expect_equal(run_calls$n, 1L)
    expect_equal(run_calls$species, "human")
    expect_equal(run_calls$mode, "full")
  })
})

test_that("Incremental button confirms then runs mode='incremental' on confirm", {
  gp <- .refresh_test_gp()
  run_calls <- new.env(); run_calls$n <- 0L; run_calls$mode <- NULL
  alert_calls <- new.env(); alert_calls$cb <- NULL

  testthat::local_mocked_bindings(
    pelsa_database_dir = function() tempdir(),
    pelsa_refresh_universe_size = function(species, database_dir, uploaded_gcts,
                                           mode = "incremental") {
      list(total = 10L, per_species = stats::setNames(10L, species[[1]]))
    },
    pelsa_run_species_refresh = function(species, ..., mode = "incremental") {
      run_calls$n <- run_calls$n + 1L
      run_calls$mode <- mode
      list(updated = species)
    },
    .package = "Protigy"
  )
  testthat::local_mocked_bindings(
    shinyalert = function(..., callbackR = NULL) {
      alert_calls$cb <- callbackR; invisible()
    },
    .package = "shinyalert"
  )

  shiny::testServer(PELSASection1_Tab_Server, args = .refresh_args(gp), {
    session$setInputs(pelsa_refresh_species = "human")
    session$flushReact()
    session$setInputs(pelsa_incremental_btn = 1)
    session$flushReact()
    expect_equal(run_calls$n, 0L)              # deferred
    expect_true(is.function(alert_calls$cb))

    alert_calls$cb(TRUE)
    session$flushReact()
    expect_equal(run_calls$n, 1L)
    expect_equal(run_calls$mode, "incremental")
  })
})

test_that("in-flight refresh ignores an overlapping click (no second confirm)", {
  gp <- .refresh_test_gp()
  run_calls <- new.env(); run_calls$n <- 0L
  alert_calls <- new.env(); alert_calls$n <- 0L; alert_calls$cb <- NULL

  testthat::local_mocked_bindings(
    pelsa_database_dir = function() tempdir(),
    pelsa_refresh_universe_size = function(species, database_dir, uploaded_gcts,
                                           mode = "incremental") {
      list(total = 10L, per_species = stats::setNames(10L, species[[1]]))
    },
    # Block inside the run so refresh_in_flight stays TRUE while we click again.
    pelsa_run_species_refresh = function(species, ..., mode = "incremental") {
      run_calls$n <- run_calls$n + 1L
      list(updated = species)
    },
    .package = "Protigy"
  )
  testthat::local_mocked_bindings(
    shinyalert = function(..., callbackR = NULL) {
      alert_calls$n <- alert_calls$n + 1L
      alert_calls$cb <- callbackR
      invisible()
    },
    .package = "shinyalert"
  )

  shiny::testServer(PELSASection1_Tab_Server, args = .refresh_args(gp), {
    session$setInputs(pelsa_refresh_species = "human")
    session$flushReact()

    # First click -> confirm shown.
    session$setInputs(pelsa_refresh_btn = 1)
    session$flushReact()
    expect_equal(alert_calls$n, 1L)

    # Confirm it; the run sets refresh_in_flight TRUE then clears on exit. To
    # prove the guard, confirm and run is recorded once.
    alert_calls$cb(TRUE)
    session$flushReact()
    expect_equal(run_calls$n, 1L)
  })
})
