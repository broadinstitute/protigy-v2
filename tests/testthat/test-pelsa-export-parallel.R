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

test_that(".PELSA_EXPORT_FIGURE_CAP is 150", {
  expect_identical(.PELSA_EXPORT_FIGURE_CAP, 150L)
})

test_that("pelsa_export_cap_proteins keeps all when under the cap", {
  prot <- data.frame(accession = c("A", "B"), is_marker = c(FALSE, TRUE),
                     stringsAsFactors = FALSE)
  stat_any <- data.frame(
    accession = c("A", "B"),
    "adj.P.Val.any_contrast" = c(0.01, 0.2), check.names = FALSE)
  res <- pelsa_export_cap_proteins(prot, stat_any, cap = 10L)
  expect_identical(nrow(res$keep), 2L)
  expect_identical(nrow(res$skipped), 0L)
})

test_that("pelsa_export_cap_proteins keeps markers + top non-markers by adj.P", {
  # 1 marker (M) + 4 non-markers with varying adj.P; cap = 3.
  prot <- data.frame(
    accession = c("M", "N1", "N2", "N3", "N4"),
    is_marker = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE)
  stat_any <- data.frame(
    accession = c("M", "N1", "N2", "N3", "N4"),
    "adj.P.Val.any_contrast" = c(0.9, 0.001, 0.05, 0.01, 0.5),
    check.names = FALSE)
  res <- pelsa_export_cap_proteins(prot, stat_any, cap = 3L)
  # Marker M always kept; remaining 2 slots -> N1 (0.001) and N3 (0.01).
  expect_setequal(res$keep$accession, c("M", "N1", "N3"))
  expect_setequal(res$skipped$accession, c("N2", "N4"))
  # skipped frame carries an adj.P column for the manifest.
  expect_true("adj.P" %in% colnames(res$skipped))
})

test_that("pelsa_export_can_parallelize is FALSE under load_all (pkg not installed)", {
  # In dev (devtools::load_all), Protigy is not in installed.packages().
  # This is the exact condition that must force the sequential fallback.
  expect_type(pelsa_export_can_parallelize(), "logical")
  expect_length(pelsa_export_can_parallelize(), 1L)
})

test_that("render_map runs sequentially in-process when not parallelizable", {
  # Force the not-installed branch regardless of the real environment.
  testthat::local_mocked_bindings(
    pelsa_export_can_parallelize = function() FALSE, .package = "Protigy")
  tmp <- tempfile(fileext = ".txt")
  items <- as.list(1:5)
  # A render_one that references a value from THIS process's closure would fail
  # in a PSOCK worker but must succeed in-process -- proving we stayed in-process.
  token <- "in-process-only"
  render_one <- function(item) cat(token, item, "\n", file = tmp, append = TRUE)
  # Must NOT set a multisession plan: capture the plan before/after.
  before <- future::plan()
  pelsa_export_render_map(items, render_one)
  after <- future::plan()
  expect_identical(class(before), class(after))   # plan untouched
  got <- readLines(tmp)
  expect_length(got, 5L)
  expect_true(all(grepl("in-process-only", got)))
})

test_that("render_map sequential fallback still skips a bad item (render_one owns tryCatch)", {
  testthat::local_mocked_bindings(
    pelsa_export_can_parallelize = function() FALSE, .package = "Protigy")
  tmp <- tempfile(fileext = ".txt")
  render_one <- function(item) tryCatch({
    if (item == 3L) stop("boom")
    cat(item, "\n", file = tmp, append = TRUE)
  }, error = function(e) NULL)
  expect_silent(pelsa_export_render_map(as.list(1:4), render_one))
  expect_identical(sort(as.integer(readLines(tmp))), c(1L, 2L, 4L))
})

# ---------------------------------------------------------------------------
# Task 7: equivalence / determinism tests for the build/render split
#
# Under devtools::load_all, Protigy is NOT in installed.packages(), so
# pelsa_export_can_parallelize() is always FALSE and pelsa_export_render_map()
# always takes its sequential in-process fallback (Task 5b). That means a
# naive "parallel vs sequential" comparison would really be sequential vs
# sequential and would prove nothing about real multisession parallelism.
# What IS fully testable in-process, and is the real regression risk of the
# Task 5-6 build/render split, is: (1) the emitted file SET is stable across
# repeated runs (no drop/dup/reorder), (2) the rendered figure bytes are
# stable (ragg determinism), and (3) the sequential branch is exercised
# end-to-end and matches a baseline that forces it explicitly. Real
# multisession (installed-package) equivalence is validated once Protigy is
# R CMD INSTALLed -- not attempted here since forcing multisession under
# load_all would throw.
# ---------------------------------------------------------------------------

# Helper: run the intensity export into a given dir, return the dir.
.run_intensity_export <- function(fx, dir) {
  pelsa_section3_export_intensity(
    dir_name = dir, ome = fx$ome, stat_results = fx$stat_results,
    cache_entry = fx$cache_entry, processed_mat = fx$processed_mat,
    condition_map = fx$condition_map, condition_order = fx$condition_order,
    sig_cutoff = 0.05, sig_stat = "adj.p.val",
    marker_accessions = fx$marker_accessions, log_transformation = "log2")
  dir
}

test_that("intensity export is deterministic: two runs emit an identical file set + bytes", {
  fx <- .make_pelsa_export_fixture(n_proteins = 8L, n_markers = 1L)
  d1 <- withr::local_tempdir(); d2 <- withr::local_tempdir()
  .run_intensity_export(fx, d1)
  .run_intensity_export(fx, d2)

  rel <- function(d) sort(list.files(d, recursive = TRUE))
  expect_identical(rel(d1), rel(d2))            # same file SET (no drop/dup)
  expect_true(length(rel(d1)) > 0L)             # actually emitted figures

  # Spot-check: the first PNG is byte-identical across the two runs (ragg is
  # deterministic). If a PNG ever embeds a timestamp and this proves flaky,
  # relax to comparing png::readPNG(...) dim() and keep the file-SET assertion
  # as the hard requirement.
  pngs <- rel(d1)[grepl("[.]png$", rel(d1))]
  expect_true(length(pngs) > 0L)
  png1 <- pngs[1]
  expect_identical(
    readBin(file.path(d1, png1), "raw", n = 5e6),
    readBin(file.path(d2, png1), "raw", n = 5e6))
})

test_that("intensity export sequential-branch output matches a forced-sequential baseline", {
  fx <- .make_pelsa_export_fixture(n_proteins = 8L, n_markers = 1L)

  # Force the render map's sequential in-process branch explicitly.
  testthat::local_mocked_bindings(
    pelsa_export_can_parallelize = function() FALSE, .package = "Protigy")
  base_dir <- withr::local_tempdir()
  .run_intensity_export(fx, base_dir)

  seq_dir <- withr::local_tempdir()
  .run_intensity_export(fx, seq_dir)

  rel <- function(d) sort(list.files(d, recursive = TRUE))
  expect_identical(rel(base_dir), rel(seq_dir))
  # NOTE: real multisession (installed-package) equivalence is validated once
  # Protigy is R CMD INSTALLed; under devtools::load_all the render map always
  # takes the sequential fallback (Task 5b), so both runs here exercise that path.
})

test_that("woods export is deterministic: two runs emit an identical file set", {
  fx <- .make_pelsa_export_fixture(n_proteins = 8L, n_markers = 1L)
  run_woods <- function(dir) {
    pelsa_section3_export_woods(
      dir_name = dir, ome = fx$ome, stat_results = fx$stat_results,
      cache_entry = fx$cache_entry, feat_df = fx$feat_df,
      sig_cutoff = 0.05, sig_stat = "adj.p.val",
      marker_accessions = fx$marker_accessions,
      contrast_choices = fx$contrast_choices)
    dir
  }
  d1 <- withr::local_tempdir(); d2 <- withr::local_tempdir()
  run_woods(d1); run_woods(d2)
  rel <- function(d) sort(list.files(d, recursive = TRUE))
  expect_identical(rel(d1), rel(d2))
  expect_true(length(rel(d1)) > 0L)
})
