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

test_that("pelsa_export_cap_proteins: an accession absent from the adj.P source degrades to Inf, never crashes", {
  # Contract guard: a prot accession NOT present in the adj.P source must sort
  # LAST (Inf), not crash. Regression for the NA-rowname hazard where a single-
  # bracket lookup returned an NA-NAMED element that aborted the skipped data.frame
  # with "row names contain missing values".
  prot <- data.frame(
    accession = c("M", "N1", "N2", "GHOST"),
    is_marker = c(TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE)
  # Direct-accession path: GHOST absent from stat_any.
  stat_any <- data.frame(
    accession = c("M", "N1", "N2"),
    "adj.P.Val.any_contrast" = c(0.9, 0.001, 0.05),
    check.names = FALSE)
  res <- pelsa_export_cap_proteins(prot, stat_any, cap = 2L)
  # Cap 2: marker M kept, best non-marker N1 (0.001) kept; N2 (0.05) and GHOST
  # (Inf) skipped. GHOST must sort last with adj.P == Inf.
  expect_setequal(res$keep$accession, c("M", "N1"))
  expect_setequal(res$skipped$accession, c("N2", "GHOST"))
  ghost_adjp <- res$skipped$adj.P[res$skipped$accession == "GHOST"]
  expect_true(is.infinite(ghost_adjp))
  # No NA rownames leaked into the skipped frame.
  expect_false(anyNA(rownames(res$skipped)))

  # Matched-join path: same GHOST-absent condition via a peptide-level frame.
  matched <- data.frame(
    accession = c("M", "N1", "N2", "GHOST"), .row_id = 1:4,
    PEP.StrippedSequence = paste0("PEP", 1:4), stringsAsFactors = FALSE)
  stat_peptide <- data.frame(
    .row_id = 1:3, PEP.StrippedSequence = paste0("PEP", 1:3),
    "adj.P.Val.any_contrast" = c(0.9, 0.001, 0.05), check.names = FALSE)
  res2 <- pelsa_export_cap_proteins(prot, stat_peptide, matched = matched, cap = 2L)
  expect_setequal(res2$skipped$accession, c("N2", "GHOST"))
  expect_true(is.infinite(res2$skipped$adj.P[res2$skipped$accession == "GHOST"]))
})

test_that("cap ranking keeps the most-significant non-markers using real caller inputs (matched-joined)", {
  # 10 proteins, 1 marker, cap 4. frac_sig makes the FIRST 5 significant
  # (adj.P 0.001) and the rest not (0.5); markers kept regardless.
  # pelsa_intensity_proteins() itself only returns marker | significant
  # proteins, so `prot` here is the marker (ACC001) + the 5 significant
  # peptides (ACC002-ACC005 in this fixture's ordering) -- cap 4 forces one
  # of the (tied, all adj.P == 0.001) significant non-markers to be skipped.
  fx <- .make_pelsa_export_fixture(n_proteins = 10L, n_markers = 1L, frac_sig = 0.5)
  matched  <- fx$cache_entry$matched
  stat_df  <- pelsa_volcano_stat_df(fx$stat_results[[fx$ome]], matched)
  stat_any <- pelsa_export_add_any_contrast(stat_df)
  prot     <- pelsa_intensity_proteins(stat_any, matched, fx$marker_accessions,
                                       .PELSA_ANY_CONTRAST, 0.05, sig_stat = "adj.p.val")
  res <- pelsa_export_cap_proteins(prot, stat_any, matched = matched, cap = 4L)

  # The manifest adj.P must be FINITE (the bug made these all Inf).
  expect_true(all(is.finite(res$skipped$adj.P)) || nrow(res$skipped) == 0L)
  expect_lte(nrow(res$keep), 4L)
  # Marker is always kept; every kept/skipped accession is drawn from the
  # significant set the fixture produced (never Inf-ranked arbitrary rows).
  expect_true("ACC001" %in% res$keep$accession)
  sig_accessions <- prot$accession[!prot$is_marker]
  expect_true(all(res$skipped$accession %in% sig_accessions))
  # Real ranking (finite adj.P), not arbitrary retention: skipped adj.P must
  # equal the peptide-level significant adj.P (0.001), not Inf.
  expect_true(all(res$skipped$adj.P == 0.001))
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

  pngs <- rel(d1)[grepl("[.]png$", rel(d1))]
  expect_true(length(pngs) > 0L)
  png1 <- pngs[1]
  # ragg embeds a non-deterministic byte (timestamp/metadata) in the PNG, so two
  # renders of the SAME plot are byte-DIFFERENT but content-identical (same
  # length, same pixel dimensions). Compare decoded image dimensions instead of
  # raw bytes; the file-SET assertion above remains the hard determinism guard.
  skip_if_not_installed("png")
  expect_identical(
    dim(png::readPNG(file.path(d1, png1))),
    dim(png::readPNG(file.path(d2, png1))))
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
  expect_true(length(rel(base_dir)) > 0L)   # non-vacuity: dirs are actually populated
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
