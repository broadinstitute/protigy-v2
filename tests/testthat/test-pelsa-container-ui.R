################################################################################
# Merged PELSA container + app-UI tests.
#
# This file combines two previously-separate suites:
#   * app-ui  -- app_UI() HTML assertions (sub-tab titles, per-tab dataset
#                switcher uiOutput ids, switcher-bar builder helpers) plus the
#                app_server / pelsaContainer_Server existence checks.
#   * container -- pelsaContainer_Server server-logic wiring driven through
#                  shiny::testServer (analyzed-datasets seam, active_dataset()
#                  reactive, user-pin / upload-resync behavior).
#
# pelsaContainer_Server is a TOP-LEVEL session-scoped server (not a
# moduleServer): it owns the analyzed-datasets seam, the active_dataset()
# reactive, and the user-pin / upload-resync logic. We drive it through
# testServer by wrapping it in a bare server(input, output, session) and
# capturing its return list into a parent-scope variable (testServer does not
# surface a non-module return via session$returned).
################################################################################

library(testthat)

`%||%` <- function(a, b) if (is.null(a)) b else a

make_gct <- function(rids = c("g1", "g2"), cids = c("s1", "s2")) {
  mat <- matrix(seq_len(length(rids) * length(cids)),
                nrow = length(rids), dimnames = list(rids, cids))
  new("GCT", mat = mat,
      rdesc = data.frame(id = rids, row.names = rids),
      cdesc = data.frame(grp = rep("A", length(cids)), row.names = cids),
      rid = rids, cid = cids)
}

gp_with_omes <- function(omes) {
  gcts <- stats::setNames(lapply(omes, function(o) make_gct()), omes)
  list(GCTs = gcts, parameters = stats::setNames(vector("list", length(omes)), omes))
}

# Build a wrapper server that records the return list into `sink_env$res`.
container_test_server <- function(GCTs_and_params, sink_env) {
  function(input, output, session) {
    sink_env$res <- pelsaContainer_Server(input, output, session, GCTs_and_params)
    sink_env$res
  }
}

# ---------------------------------------------------------------------------
# app_UI HTML assertions + dataset-switcher ids
# ---------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------
# pelsaContainer_Server server logic via testServer
# ---------------------------------------------------------------------------

test_that("default analyzed set tracks uploaded omes; active_dataset = first", {
  sink_env <- new.env()
  gp <- shiny::reactiveVal(gp_with_omes(c("proteome", "phospho")))
  shiny::testServer(container_test_server(gp, sink_env), {
    session$flushReact()
    expect_equal(sink_env$res$active_dataset(), "proteome")
  })
})

test_that("active_dataset returns the sole dataset when only one ome", {
  sink_env <- new.env()
  gp <- shiny::reactiveVal(gp_with_omes("proteome"))
  shiny::testServer(container_test_server(gp, sink_env), {
    session$flushReact()
    expect_equal(sink_env$res$active_dataset(), "proteome")
  })
})

test_that("active_dataset honors a valid selection and falls back on invalid", {
  sink_env <- new.env()
  gp <- shiny::reactiveVal(gp_with_omes(c("proteome", "phospho")))
  shiny::testServer(container_test_server(gp, sink_env), {
    session$flushReact()
    session$setInputs(pelsa_active_dataset = "phospho")
    expect_equal(sink_env$res$active_dataset(), "phospho")

    # a selection not in the analyzed set -> fall back to first
    session$setInputs(pelsa_active_dataset = "does_not_exist")
    expect_equal(sink_env$res$active_dataset(), "proteome")
  })
})

test_that("set_analyzed_datasets pins a subset and resists auto-sync clobber", {
  sink_env <- new.env()
  gp <- shiny::reactiveVal(gp_with_omes(c("proteome", "phospho", "rna")))
  shiny::testServer(container_test_server(gp, sink_env), {
    session$flushReact()
    sink_env$res$set_analyzed_datasets(c("phospho", "rna"))
    session$flushReact()
    # active falls to first of the pinned subset
    expect_equal(sink_env$res$active_dataset(), "phospho")

    # an unrelated reactive flush (same ome signature) must NOT restore the
    # full uploaded set -- the user pin holds.
    # Use a structurally distinct object (same ome names, new list identity)
    # so that reactiveVal does NOT short-circuit and the observe() actually fires.
    gp2 <- gp_with_omes(c("proteome", "phospho", "rna"))
    gp2$parameters$proteome <- list(touched = TRUE)
    gp(gp2)
    session$flushReact()
    expect_equal(sink_env$res$active_dataset(), "phospho")
  })
})

test_that("set_analyzed_datasets(NULL) un-pins and restores all uploaded omes", {
  sink_env <- new.env()
  gp <- shiny::reactiveVal(gp_with_omes(c("proteome", "phospho")))
  shiny::testServer(container_test_server(gp, sink_env), {
    session$flushReact()
    sink_env$res$set_analyzed_datasets("phospho")
    session$flushReact()
    expect_equal(sink_env$res$active_dataset(), "phospho")

    sink_env$res$set_analyzed_datasets(NULL)
    session$flushReact()
    # un-pinned -> default tracks all uploaded omes -> first is proteome
    expect_equal(sink_env$res$active_dataset(), "proteome")
  })
})

test_that("set_analyzed_datasets drops NA/empty entries (treated as un-pin)", {
  sink_env <- new.env()
  gp <- shiny::reactiveVal(gp_with_omes(c("proteome", "phospho")))
  shiny::testServer(container_test_server(gp, sink_env), {
    session$flushReact()
    sink_env$res$set_analyzed_datasets(c("", NA))
    session$flushReact()
    expect_equal(sink_env$res$active_dataset(), "proteome")
  })
})

test_that("a NEW upload (changed ome signature) un-pins and re-tracks fresh omes", {
  sink_env <- new.env()
  gp <- shiny::reactiveVal(gp_with_omes(c("proteome", "phospho")))
  shiny::testServer(container_test_server(gp, sink_env), {
    session$flushReact()
    sink_env$res$set_analyzed_datasets("phospho")
    session$flushReact()
    expect_equal(sink_env$res$active_dataset(), "phospho")

    # brand-new upload with a different ome set un-pins and tracks the new omes
    gp(gp_with_omes(c("rna", "metabolome")))
    session$flushReact()
    expect_equal(sink_env$res$active_dataset(), "rna")
  })
})

test_that("NULL GCTs yields no analyzed datasets (active_dataset req-blocks)", {
  sink_env <- new.env()
  gp <- shiny::reactiveVal(list(GCTs = NULL, parameters = NULL))
  shiny::testServer(container_test_server(gp, sink_env), {
    session$flushReact()
    # active_dataset() uses req(length>=1); with no datasets it raises a silent
    # validation/req error.
    expect_error(sink_env$res$active_dataset())
  })
})
