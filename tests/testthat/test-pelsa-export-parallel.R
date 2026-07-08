test_that("pelsa_export_max_workers tiers on 16-core boundary", {
  expect_identical(pelsa_export_max_workers(2L), 4L)
  expect_identical(pelsa_export_max_workers(4L), 4L)
  expect_identical(pelsa_export_max_workers(16L), 4L)
  expect_identical(pelsa_export_max_workers(17L), 8L)
  expect_identical(pelsa_export_max_workers(64L), 8L)
})

test_that("pelsa_export_workers leaves one core free and honors the tier", {
  # Mock availableCores via a local shim: the function reads future::availableCores(),
  # so stub it with `local_mocked_bindings` on the future namespace.
  testthat::local_mocked_bindings(
    availableCores = function(...) 8L, .package = "future"
  )
  # avail=8 -> headroom=7, ceiling=4, so min(7,4,n_items)
  expect_identical(pelsa_export_workers(100L), 4L)  # clamped by tier ceiling
  expect_identical(pelsa_export_workers(3L), 3L)    # clamped by n_items
  expect_identical(pelsa_export_workers(1L), 1L)    # single item -> sequential
})

test_that("pelsa_export_workers on a 2-core machine yields 1 (sequential)", {
  testthat::local_mocked_bindings(
    availableCores = function(...) 2L, .package = "future"
  )
  # headroom = 1, so workers = 1 regardless of n_items
  expect_identical(pelsa_export_workers(100L), 1L)
})

test_that("pelsa_export_workers on a 64-core machine caps at 8", {
  testthat::local_mocked_bindings(
    availableCores = function(...) 64L, .package = "future"
  )
  expect_identical(pelsa_export_workers(1000L), 8L)
})

test_that("pelsa_export_render_map applies render_one to every item (sequential plan)", {
  # Force sequential so the side effects land in THIS process (writes to a file).
  old <- future::plan(future::sequential)
  on.exit(future::plan(old), add = TRUE)
  tmp <- tempfile(fileext = ".txt")
  items <- as.list(1:5)
  render_one <- function(item) cat(item, "\n", file = tmp, append = TRUE)
  pelsa_export_render_map(items, render_one)
  got <- sort(as.integer(readLines(tmp)))
  expect_identical(got, 1:5)
})

test_that("pelsa_export_render_map returns invisibly NULL on empty items", {
  expect_null(pelsa_export_render_map(list(), function(x) x))
})

test_that("pelsa_export_render_map: render_one owning tryCatch skips a bad item", {
  old <- future::plan(future::sequential)
  on.exit(future::plan(old), add = TRUE)
  tmp <- tempfile(fileext = ".txt")
  items <- as.list(1:4)
  render_one <- function(item) tryCatch({
    if (item == 3L) stop("boom")
    cat(item, "\n", file = tmp, append = TRUE)
  }, error = function(e) NULL)
  # Must NOT propagate the item==3 error.
  expect_silent(pelsa_export_render_map(items, render_one))
  got <- sort(as.integer(readLines(tmp)))
  expect_identical(got, c(1L, 2L, 4L))
})

test_that("pelsa_export_render_map is a no-op for progress with no handler", {
  old <- future::plan(future::sequential)
  on.exit(future::plan(old), add = TRUE)
  # No progressr handler registered -> progressor() calls are silent no-ops.
  expect_silent(pelsa_export_render_map(as.list(1:3), function(x) invisible(NULL)))
})
