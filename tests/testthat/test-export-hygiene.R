# Phase 6 (EXP-4 + EXP-5) regression tests for the export download handler.
#
# These tests exercise the SAME logic the downloadHandler content function runs
# (snapshot-once + on.exit temp-dir cleanup). Because downloadHandler is hard to
# drive directly in a unit test, we replicate the content-function body here and
# assert the two Phase-6 invariants:
#   - EXP-5: each tab export reactive is evaluated exactly ONCE (call-once)
#   - EXP-4: the per-export temp dir is removed after the handler returns, while
#            the produced zip survives.

# ---- helpers ---------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

# A reactive-tagged counting accessor: passes is.reactive() and increments a
# counter on each `()` invocation (measures call-site invocations).
make_counting_reactive <- function(counter_env, name, value_fn) {
  force(counter_env); force(name); force(value_fn)
  f <- function() {
    counter_env[[name]] <- (counter_env[[name]] %||% 0L) + 1L
    value_fn()
  }
  class(f) <- c("reactiveExpr", "reactive", "function")
  f
}

make_export_fn <- function(fname, content) {
  force(fname); force(content)
  function(dir_name) writeLines(content, file.path(dir_name, fname))
}

# Replicate the NEW (Phase 6) content-function body. Returns paths + total for
# assertions. on.exit fires when this function returns, mirroring the handler.
run_export_body <- function(exports, selected_omes, selected_tabs, file) {
  force(exports)
  dir_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
  zip_dir <- tempdir(check = TRUE)
  exports_dir <- file.path(zip_dir, dir_name)
  dir.create(exports_dir, recursive = TRUE)
  on.exit(unlink(exports_dir, recursive = TRUE), add = TRUE)   # EXP-4

  lapply(selected_omes, function(ome) dir.create(file.path(exports_dir, ome)))

  # EXP-5: snapshot each tab export object once
  exports_snapshot <- lapply(selected_tabs, function(tab_name) {
    if (is.reactive(exports[[tab_name]])) exports[[tab_name]]() else exports[[tab_name]]
  })
  names(exports_snapshot) <- selected_tabs

  total_exports <- 0
  for (tab_name in selected_tabs) {
    exports_all_omes <- exports_snapshot[[tab_name]]
    for (ome in intersect(selected_omes, names(exports_all_omes))) {
      total_exports <- total_exports + length(exports_all_omes[[ome]])
    }
  }

  lapply(selected_tabs, function(tab_name) {
    exports_all_omes <- exports_snapshot[[tab_name]]
    lapply(intersect(selected_omes, names(exports_all_omes)), function(ome) {
      exports_this_ome <- exports_all_omes[[ome]]
      exports_in_tab_path <- file.path(exports_dir, ome, tab_name)
      dir.create(exports_in_tab_path)
      for (i in seq_along(exports_this_ome)) {
        p <- exports_this_ome[[i]]
        if (is.reactive(p)) p <- p()
        p(exports_in_tab_path)
      }
    })
  })

  zip::zip(file, file.path(dir_name, list.files(exports_dir)), recurse = TRUE, root = zip_dir)
  list(zip = file, exports_dir = exports_dir, total = total_exports)
}

make_exports <- function(counter_env) {
  ome_block <- function(prefix) list(
    fileA = make_export_fn(paste0(prefix, "_A.txt"), paste0(prefix, "-A")),
    fileB = make_export_fn(paste0(prefix, "_B.txt"), paste0(prefix, "-B"))
  )
  list(
    reactiveTab = make_counting_reactive(counter_env, "reactiveTab", function() list(
      proteome = ome_block("rt-prot"),
      phospho  = ome_block("rt-phos")
    )),
    plainTab = list(proteome = list(only = make_export_fn("plain.txt", "plain-only"))),
    nestedReactiveTab = make_counting_reactive(counter_env, "nestedReactiveTab", function() list(
      proteome = list(
        dyn = shiny::reactive(make_export_fn("nested.txt", "nested-dyn"))
      )
    ))
  )
}

# ---- tests -----------------------------------------------------------------

test_that("EXP-5: each tab export reactive is evaluated exactly once", {
  shiny::isolate({
    ctr <- new.env()
    exports <- make_exports(ctr)
    f <- tempfile(fileext = ".zip")
    res <- run_export_body(exports, c("proteome", "phospho"),
                           c("reactiveTab", "plainTab", "nestedReactiveTab"), f)
    expect_equal(ctr$reactiveTab, 1L)
    expect_equal(ctr$nestedReactiveTab, 1L)
    # total = reactiveTab(2 omes x 2) + plainTab(1) + nestedReactiveTab(1) = 6
    expect_equal(res$total, 6)
  })
})

test_that("EXP-4: temp export dir is removed after handler returns, zip survives", {
  shiny::isolate({
    ctr <- new.env()
    exports <- make_exports(ctr)
    f <- tempfile(fileext = ".zip")
    res <- run_export_body(exports, c("proteome", "phospho"),
                           c("reactiveTab", "plainTab", "nestedReactiveTab"), f)
    expect_false(dir.exists(res$exports_dir))   # cleaned up
    expect_true(file.exists(res$zip))           # zip intact
    expect_gt(file.info(res$zip)$size, 0)
  })
})

test_that("EXP-4: temp dir is cleaned even when an export errors mid-loop", {
  shiny::isolate({
    ctr <- new.env()
    exports <- list(
      boomTab = make_counting_reactive(ctr, "boomTab", function() list(
        proteome = list(bad = function(dir_name) stop("boom"))
      ))
    )
    f <- tempfile(fileext = ".zip")
    captured_dir <- NULL
    # capture exports_dir before the error by intercepting via a wrapper
    body <- function() {
      dir_name <- sub("(.*)\\..*$", "\\1", basename(f))
      zip_dir <- tempdir(check = TRUE)
      exports_dir <- file.path(zip_dir, dir_name)
      captured_dir <<- exports_dir
      dir.create(exports_dir, recursive = TRUE)
      on.exit(unlink(exports_dir, recursive = TRUE), add = TRUE)
      snap <- lapply(names(exports), function(t)
        if (is.reactive(exports[[t]])) exports[[t]]() else exports[[t]])
      names(snap) <- names(exports)
      for (t in names(exports)) for (ome in names(snap[[t]])) {
        p <- snap[[t]][[ome]]$bad
        p(exports_dir)  # throws
      }
    }
    expect_error(body(), "boom")
    expect_false(dir.exists(captured_dir))  # on.exit cleaned up despite error
  })
})

test_that("M11: per-export success/failure is captured from the tryCatch result", {
  shiny::isolate({
    exports_dir <- tempfile("m11_"); dir.create(exports_dir)
    on.exit(unlink(exports_dir, recursive = TRUE), add = TRUE)
    tab_path <- file.path(exports_dir, "proteome", "tabX")
    dir.create(tab_path, recursive = TRUE)

    items <- list(
      good = function(dir_name) writeLines("ok", file.path(dir_name, "good.txt")),
      bad  = function(dir_name) stop("export boom")
    )

    # Mirror the tab_export.R per-export capture + routing (M11).
    success_exports <- character(0)
    error_exports   <- character(0)
    for (nm in names(items)) {
      p <- items[[nm]]
      export_ok <- my_shinyalert_tryCatch(
        show.error = FALSE, return.error = FALSE,
        expr = { p(tab_path); TRUE }
      )
      if (isTRUE(export_ok)) {
        success_exports <- c(success_exports, nm)
      } else {
        error_exports <- c(error_exports, nm)
      }
    }

    expect_identical(success_exports, "good")
    expect_identical(error_exports, "bad")
    # The tab dir always exists, so the OLD `!file.exists(tab_path)` probe could
    # never have detected the failure -- this guards that regression.
    expect_true(dir.exists(tab_path))
  })
})

test_that("empty selection yields zero exports and cleaned temp dir", {
  shiny::isolate({
    ctr <- new.env()
    exports <- make_exports(ctr)
    f <- tempfile(fileext = ".zip")
    res <- run_export_body(exports, character(0), character(0), f)
    expect_equal(res$total, 0)
    expect_false(dir.exists(res$exports_dir))
  })
})

# ---- Phase 4: PELSA export aggregator logs swallowed failures --------------
# Mirrors the all_pelsa_exports() reactive body in app_server.R: each section is
# wrapped in a resilient tryCatch (a failing section must not abort the others),
# but the catch now logs a warning instead of swallowing the error silently.

# Replicate the aggregator body for three section reactives.
aggregate_pelsa_exports <- function(s1_fn, s2_fn, s3_fn) {
  s1 <- tryCatch(s1_fn(), error = function(e) {
    warning("PELSA export (section 1) failed: ", conditionMessage(e)); NULL
  }) %||% list()
  s2 <- tryCatch(s2_fn(), error = function(e) {
    warning("PELSA export (section 2) failed: ", conditionMessage(e)); NULL
  }) %||% list()
  s3 <- tryCatch(s3_fn(), error = function(e) {
    warning("PELSA export (section 3) failed: ", conditionMessage(e)); NULL
  }) %||% list()
  omes <- union(union(names(s1), names(s2)), names(s3))
  stats::setNames(lapply(omes, function(o) {
    c(s1[[o]] %||% list(), s2[[o]] %||% list(), s3[[o]] %||% list())
  }), omes)
}

test_that("Phase 4: a failing PELSA section logs a warning and others still aggregate", {
  s1 <- function() list(proteome = list(a = function(d) NULL))
  s2 <- function() stop("section 2 boom")
  s3 <- function() list(proteome = list(c = function(d) NULL))

  expect_warning(
    res <- aggregate_pelsa_exports(s1, s2, s3),
    "PELSA export \\(section 2\\) failed: section 2 boom"
  )
  # surviving sections still contribute their export functions
  expect_named(res, "proteome")
  expect_setequal(names(res$proteome), c("a", "c"))
})

test_that("Phase 4: all PELSA sections succeeding emits no warning", {
  s <- function() list(proteome = list(a = function(d) NULL))
  expect_warning(aggregate_pelsa_exports(s, s, s), NA)
})
