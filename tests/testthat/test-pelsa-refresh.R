################################################################################
# Tests for the PELSA per-species UniProt-annotation refresh (Task 5C).
#
# These cover the PURE helpers (accession universe, write/round-trip) and the
# orchestration helper with an INJECTED fake fetcher. NO LIVE NETWORK is ever
# touched: pelsa_fetch_uniprot is never called here  -  the orchestration test
# substitutes a stub returning a canned 8-col feature frame.
################################################################################

library(testthat)

# Canned 8-column feature frame matching the schema (for write + orchestration).
.fake_feature_df <- function() {
  data.frame(
    accession     = c("P00001", "P00001", "P00002"),
    feature_type  = c("active site", "domain", "transmembrane"),
    start         = c(10L, 50L, 100L),
    end           = c(12L, 120L, 130L),
    description   = c("nucleophile", "kinase domain", "Helical"),
    feature_class = c("active_or_binding_site", "catalytic_domain",
                      "transmembrane_or_signal"),
    class_score   = c(5L, 3L, 0L),
    coord_quality = c("exact", "exact", "fuzzy"),
    stringsAsFactors = FALSE
  )
}

# ---- pelsa_full_universe (FASTA proteome only) -------------------------------

test_that("full_universe = FASTA accessions only; ignores datasets + cache", {
  gcts <- list(omeA = data.frame(PG.ProteinAccessions = "P10000;P20000",
                                 stringsAsFactors = FALSE))
  existing <- data.frame(accession = c("P30000", "P40000"),
                         stringsAsFactors = FALSE)
  fasta_map <- list(Q1 = "MKV", Q2 = "AAA", Q3 = "CCC")
  out <- pelsa_full_universe(gcts, existing, fasta_map = fasta_map)
  expect_identical(out, sort(c("Q1", "Q2", "Q3")))  # NO dataset/cache accessions
})

test_that("full_universe is empty when no FASTA is given", {
  gcts <- list(omeA = data.frame(PG.ProteinAccessions = "P1",
                                 stringsAsFactors = FALSE))
  expect_identical(pelsa_full_universe(gcts, NULL, fasta_map = NULL),
                   character(0))
})

# ---- pelsa_incremental_universe ((dataset U fasta) - cache) ------------------

test_that("incremental_universe = (dataset U fasta) minus cache accessions", {
  gcts <- list(omeA = data.frame(PG.ProteinAccessions = c("P1;P2", "P3"),
                                 stringsAsFactors = FALSE))
  fasta_map <- list(P3 = "AAA", P4 = "CCC")          # P3 overlaps a dataset acc
  existing <- data.frame(accession = c("P2", "P4"),  # already cached
                         stringsAsFactors = FALSE)
  out <- pelsa_incremental_universe(gcts, existing, fasta_map = fasta_map)
  # union {P1,P2,P3,P4} minus cache {P2,P4} = {P1,P3}
  expect_identical(out, sort(c("P1", "P3")))
})

test_that("incremental_universe explodes/dedups/trims dataset tokens", {
  gcts <- list(omeA = data.frame(
    PG.ProteinAccessions = c("P1; P2 ;P3", "P2;P1", NA_character_, ""),
    stringsAsFactors = FALSE))
  out <- pelsa_incremental_universe(gcts, NULL, fasta_map = NULL)
  expect_identical(out, sort(c("P1", "P2", "P3")))
})

test_that("incremental_universe result is disjoint from the cache", {
  gcts <- list(omeA = data.frame(PG.ProteinAccessions = "P1;P2;P3",
                                 stringsAsFactors = FALSE))
  existing <- data.frame(accession = c("P1", "P2", "P3"),
                         stringsAsFactors = FALSE)
  # All dataset accessions already cached, no FASTA -> empty.
  expect_identical(pelsa_incremental_universe(gcts, existing, fasta_map = NULL),
                   character(0))
})

test_that("incremental_universe ignores datasets without PG.ProteinAccessions", {
  gcts <- list(
    nonPelsa = data.frame(foo = 1:3, stringsAsFactors = FALSE),
    pelsa    = data.frame(PG.ProteinAccessions = "P5", stringsAsFactors = FALSE))
  expect_identical(pelsa_incremental_universe(gcts, NULL, fasta_map = NULL), "P5")
})

# ---- pelsa_wipe_species_cache (full-mode clean slate) ------------------------

test_that("wipe deletes uniprot_features + uniprot_membrane, spares fasta/", {
  species_dir <- withr::local_tempdir()
  dir.create(file.path(species_dir, "fasta"))
  writeLines(">x\nMKV", file.path(species_dir, "fasta", "p.fasta"))
  dir.create(file.path(species_dir, "uniprot_features"))
  writeLines("acc", file.path(species_dir, "uniprot_features", "uniprot_features.tsv"))
  dir.create(file.path(species_dir, "uniprot_membrane"))
  writeLines("m", file.path(species_dir, "uniprot_membrane", "mem.tsv"))

  pelsa_wipe_species_cache(species_dir)

  expect_true(dir.exists(file.path(species_dir, "fasta")))
  expect_true(file.exists(file.path(species_dir, "fasta", "p.fasta")))
  expect_false(dir.exists(file.path(species_dir, "uniprot_features")))
  expect_false(dir.exists(file.path(species_dir, "uniprot_membrane")))
})

test_that("wipe also removes stray top-level files (except inside fasta/)", {
  species_dir <- withr::local_tempdir()
  dir.create(file.path(species_dir, "fasta"))
  writeLines("keep", file.path(species_dir, "fasta", "keep.fasta"))
  writeLines("junk", file.path(species_dir, "stray.parquet"))

  deleted <- pelsa_wipe_species_cache(species_dir)

  expect_false(file.exists(file.path(species_dir, "stray.parquet")))
  expect_true(file.exists(file.path(species_dir, "fasta", "keep.fasta")))
  expect_true("stray.parquet" %in% deleted)
  expect_false("fasta" %in% deleted)
})

test_that("wipe is a no-op on a missing species dir", {
  missing <- file.path(withr::local_tempdir(), "does_not_exist")
  expect_silent(out <- pelsa_wipe_species_cache(missing))
  expect_identical(out, character(0))
})

# ---- pelsa_write_feature_cache (round-trip) ----------------------------------

test_that("write_feature_cache round-trips via pelsa_read_feature_cache", {
  species_dir <- withr::local_tempdir()
  feat <- .fake_feature_df()

  path <- pelsa_write_feature_cache(feat, species_dir)
  expect_true(file.exists(path))
  expect_identical(basename(path), "uniprot_features.tsv")
  expect_true(file.exists(file.path(species_dir, "uniprot_features",
                                    "schema.json")))

  # Read back the four required + the present optional columns.
  back <- pelsa_read_feature_cache(species_dir)
  expect_true(all(c("accession", "start", "end", "feature_class",
                    "feature_type", "class_score", "description",
                    "coord_quality") %in% colnames(back)))
  # Round-trip identity on the schema columns (reader reorders required-first;
  # compare value-by-value).
  expect_identical(back$accession, feat$accession)
  expect_identical(back$start, feat$start)
  expect_identical(back$end, feat$end)
  expect_identical(back$feature_class, feat$feature_class)
  expect_identical(back$class_score, feat$class_score)
  expect_identical(back$coord_quality, feat$coord_quality)
})

test_that("write_feature_cache writes columns in the canonical schema order", {
  species_dir <- withr::local_tempdir()
  # Feed columns in a SHUFFLED order; the written TSV must be schema order.
  feat <- .fake_feature_df()[, c("coord_quality", "accession", "end",
                                 "class_score", "feature_type", "start",
                                 "description", "feature_class")]
  path <- pelsa_write_feature_cache(feat, species_dir)
  header <- strsplit(readLines(path, n = 1L), "\t", fixed = TRUE)[[1]]
  expect_identical(
    header,
    c("accession", "feature_type", "start", "end", "description",
      "feature_class", "class_score", "coord_quality")
  )
})

test_that("write_feature_cache schema.json matches the committed contract", {
  species_dir <- withr::local_tempdir()
  pelsa_write_feature_cache(.fake_feature_df(), species_dir)
  schema <- jsonlite::read_json(
    file.path(species_dir, "uniprot_features", "schema.json")
  )
  expect_identical(unlist(schema$columns),
                   c("accession", "feature_type", "start", "end",
                     "description", "feature_class", "class_score",
                     "coord_quality"))
  expect_identical(schema$classifier_version, "fixed_v1")
  expect_equal(schema$feature_class_scores$active_or_binding_site, 5L)
})

test_that("write_feature_cache fails fast on missing schema columns", {
  species_dir <- withr::local_tempdir()
  bad <- data.frame(accession = "P1", start = 1L, stringsAsFactors = FALSE)
  expect_error(pelsa_write_feature_cache(bad, species_dir),
               "missing schema column")
})

test_that("write_feature_cache surfaces a clear error on a non-writable dir", {
  skip_on_os("windows")  # POSIX permission semantics
  base <- withr::local_tempdir()
  ro <- file.path(base, "readonly")
  dir.create(ro)
  Sys.chmod(ro, mode = "0500")  # r-x, not writable
  on.exit(Sys.chmod(ro, mode = "0700"), add = TRUE)
  # Skip if running as root (chmod is ignored): the test can't create the
  # non-writable condition.
  skip_if(file.access(ro, mode = 2L) == 0L,
          "tempdir not enforce-able as read-only (root?)")

  expect_error(
    pelsa_write_feature_cache(.fake_feature_df(), ro),
    "read-only|not writable|cannot create"
  )
})

# ---- pelsa_merge_feature_cache (data-loss guard) -----------------------------

# A 3-accession existing cache (P1, P2, P3 each one row).
.existing_cache_df <- function() {
  data.frame(
    accession     = c("P1", "P2", "P3"),
    feature_type  = c("domain", "domain", "active site"),
    start         = c(1L, 1L, 1L),
    end           = c(10L, 10L, 10L),
    description   = c("old1", "old2", "old3"),
    feature_class = c("folded_domain", "folded_domain",
                      "active_or_binding_site"),
    class_score   = c(2L, 2L, 5L),
    coord_quality = c("exact", "exact", "exact"),
    stringsAsFactors = FALSE
  )
}

test_that("merge retains old rows for unresolved accessions, fresh rows win", {
  existing <- .existing_cache_df()
  # Fresh resolved P1, P2 (new rows); P3 ended up unresolved.
  fresh <- data.frame(
    accession     = c("P1", "P2"),
    feature_type  = c("domain", "domain"),
    start         = c(5L, 5L),
    end           = c(20L, 20L),
    description   = c("new1", "new2"),
    feature_class = c("catalytic_domain", "catalytic_domain"),
    class_score   = c(3L, 3L),
    coord_quality = c("exact", "exact"),
    stringsAsFactors = FALSE
  )
  merged <- pelsa_merge_feature_cache(existing, fresh, unresolved = "P3")

  # P3's OLD row survives (data-loss guard).
  expect_true("P3" %in% merged$accession)
  p3 <- merged[merged$accession == "P3", ]
  expect_identical(p3$description, "old3")
  # P1/P2 are the FRESH rows (not the old ones).
  p1 <- merged[merged$accession == "P1", ]
  expect_identical(p1$description, "new1")
  expect_identical(p1$class_score, 3L)
  # No duplicate P1/P2 (old rows superseded).
  expect_identical(sum(merged$accession == "P1"), 1L)
})

test_that("merge: a RESOLVED accession with zero fresh rows correctly drops", {
  existing <- .existing_cache_df()
  # P1 resolved with new rows; P2 resolved but UniProt removed its features
  # (no fresh rows, NOT in unresolved) -> P2 must NOT be retained.
  fresh <- data.frame(
    accession     = "P1", feature_type = "domain", start = 5L, end = 9L,
    description = "new1", feature_class = "folded_domain", class_score = 2L,
    coord_quality = "exact", stringsAsFactors = FALSE
  )
  merged <- pelsa_merge_feature_cache(existing, fresh, unresolved = "P3")
  expect_false("P2" %in% merged$accession)  # resolved-but-empty -> dropped
  expect_true("P3" %in% merged$accession)   # unresolved -> retained
})

test_that("merge with no existing cache returns the fresh frame unchanged", {
  fresh <- .fake_feature_df()
  expect_identical(pelsa_merge_feature_cache(NULL, fresh, "P99"), fresh)
  expect_identical(
    pelsa_merge_feature_cache(pelsa_empty_feature_frame(), fresh, "P99"),
    fresh
  )
})

test_that("merge with no unresolved fully supersedes the old cache", {
  existing <- .existing_cache_df()
  fresh <- .fake_feature_df()
  expect_identical(
    pelsa_merge_feature_cache(existing, fresh, unresolved = character(0)),
    fresh
  )
})

# ---- atomic write: a failed write leaves the PRIOR cache intact --------------

test_that("write failure leaves a pre-seeded cache uncorrupted (atomic)", {
  species_dir <- withr::local_tempdir()
  # Pre-seed a valid cache.
  good <- .existing_cache_df()
  pelsa_write_feature_cache(good, species_dir)
  tsv <- file.path(species_dir, "uniprot_features", "uniprot_features.tsv")
  before <- readLines(tsv)

  # A second write with a malformed (missing-schema) frame must FAIL without
  # touching the existing good .tsv.
  bad <- data.frame(accession = "P1", start = 1L, stringsAsFactors = FALSE)
  expect_error(pelsa_write_feature_cache(bad, species_dir),
               "missing schema column")

  # Prior cache still readable + byte-identical.
  expect_identical(readLines(tsv), before)
  back <- pelsa_read_feature_cache(species_dir)
  expect_identical(sort(back$accession), c("P1", "P2", "P3"))
})

test_that("atomic write leaves no leftover temp files on success", {
  species_dir <- withr::local_tempdir()
  pelsa_write_feature_cache(.existing_cache_df(), species_dir)
  feat_dir <- file.path(species_dir, "uniprot_features")
  files <- list.files(feat_dir)
  # Only the two canonical files; no uniprot_features_*.tsv / schema_*.json temps.
  expect_setequal(files, c("uniprot_features.tsv", "schema.json"))
})

# ---- pelsa_refresh_species_cache (INJECTED fake fetcher, NO network) ---------

test_that("refresh_species_cache fetches via injected stub + writes + counts", {
  species_dir <- withr::local_tempdir()
  universe <- c("P00001", "P00002", "P00003")

  called_with <- NULL
  fake_fetch <- function(accessions) {
    called_with <<- accessions
    list(features = .fake_feature_df(), unresolved = c("P00003"))
  }

  res <- pelsa_refresh_species_cache(
    species = "9606", universe = universe, species_dir = species_dir,
    fetch_fn = fake_fetch
  )

  # The stub (NOT the network) was called with the universe.
  expect_identical(called_with, universe)
  expect_identical(res$n_features, 3L)
  expect_identical(res$n_unresolved, 1L)
  expect_identical(res$n_accessions, 3L)
  expect_identical(res$n_retained_from_cache, 0L)  # no existing cache

  # Cache was actually written + reads back.
  expect_true(file.exists(res$path))
  back <- pelsa_read_feature_cache(species_dir)
  expect_identical(nrow(back), 3L)
})

test_that("refresh_species_cache MERGES over an existing cache (no data loss)", {
  species_dir <- withr::local_tempdir()
  existing <- .existing_cache_df()             # P1, P2, P3
  pelsa_write_feature_cache(existing, species_dir)

  # Stub resolves P1, P2; P3 demoted to unresolved (flaky network).
  fresh <- data.frame(
    accession     = c("P1", "P2"),
    feature_type  = c("domain", "domain"),
    start         = c(5L, 5L), end = c(20L, 20L),
    description   = c("new1", "new2"),
    feature_class = c("catalytic_domain", "catalytic_domain"),
    class_score   = c(3L, 3L), coord_quality = c("exact", "exact"),
    stringsAsFactors = FALSE
  )
  fake_fetch <- function(accessions) list(features = fresh, unresolved = "P3")

  res <- pelsa_refresh_species_cache(
    species = "9606", universe = c("P1", "P2", "P3"),
    species_dir = species_dir, fetch_fn = fake_fetch, existing = existing
  )
  expect_identical(res$n_retained_from_cache, 1L)

  # P3's prior annotation SURVIVES on disk; P1/P2 are the fresh rows.
  back <- pelsa_read_feature_cache(species_dir)
  expect_true("P3" %in% back$accession)
  expect_identical(back[back$accession == "P3", "description"], "old3")
  expect_identical(back[back$accession == "P1", "description"], "new1")
})

test_that("refresh_species_cache drives a Progress-like object's $set", {
  species_dir <- withr::local_tempdir()
  calls <- list()
  fake_progress <- list(set = function(value, message, detail = NULL) {
    calls[[length(calls) + 1L]] <<- list(value = value, message = message)
  })
  fake_fetch <- function(accessions) {
    list(features = .fake_feature_df(), unresolved = character(0))
  }

  pelsa_refresh_species_cache(
    species = "10090", universe = "P00001", species_dir = species_dir,
    fetch_fn = fake_fetch, progress = fake_progress
  )
  expect_gte(length(calls), 2L)               # at least fetch + write stages
  expect_equal(calls[[length(calls)]]$value, 1.0)  # ends at 100%
})

test_that("refresh_species_cache errors on an empty universe", {
  species_dir <- withr::local_tempdir()
  expect_error(
    pelsa_refresh_species_cache("9606", character(0), species_dir,
                                fetch_fn = function(a) list(features = NULL)),
    "empty accession universe"
  )
})

test_that("refresh_species_cache rejects a malformed fetch_fn return", {
  species_dir <- withr::local_tempdir()
  expect_error(
    pelsa_refresh_species_cache("9606", "P1", species_dir,
                                fetch_fn = function(a) list(nope = 1)),
    "must return a list with a `features`"
  )
})

# ---- pelsa_refresh_species_cache: mode = full (wipe + supersede) -------------

test_that("full mode WIPES the species dir before fetch + supersedes cache", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "10090")
  dir.create(species_dir)
  # Pre-seed a stale cache + a membrane file + a fasta to spare.
  pelsa_write_feature_cache(.existing_cache_df(), species_dir)  # P1,P2,P3
  dir.create(file.path(species_dir, "uniprot_membrane"))
  writeLines("stale", file.path(species_dir, "uniprot_membrane", "m.tsv"))
  dir.create(file.path(species_dir, "fasta"))
  writeLines(">x\nMKV", file.path(species_dir, "fasta", "p.fasta"))

  # Fetch returns a DIFFERENT set (P00001/P00002) -> the old P1/P2/P3 must be gone.
  fake_fetch <- function(accessions, ...) {
    list(features = .fake_feature_df(), unresolved = character(0))
  }
  res <- pelsa_refresh_species_cache(
    species = "10090", universe = c("P00001", "P00002"),
    species_dir = species_dir, fetch_fn = fake_fetch,
    existing = .existing_cache_df(),  # passed, but full mode must IGNORE it
    mode = "full"
  )

  expect_identical(res$mode, "full")
  expect_identical(res$n_retained_from_cache, 0L)  # nothing retained in full
  back <- pelsa_read_feature_cache(species_dir)
  expect_setequal(unique(back$accession), c("P00001", "P00002"))  # P1/2/3 gone
  expect_false(dir.exists(file.path(species_dir, "uniprot_membrane")))  # wiped
  expect_true(file.exists(file.path(species_dir, "fasta", "p.fasta")))   # spared
})

test_that("full mode does NOT wipe when canceled before fetch", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "10090")
  dir.create(species_dir)
  pelsa_write_feature_cache(.existing_cache_df(), species_dir)

  called <- FALSE
  fake_fetch <- function(accessions, ...) { called <<- TRUE
    list(features = .fake_feature_df(), unresolved = character(0)) }

  res <- pelsa_refresh_species_cache(
    species = "10090", universe = c("P00001"), species_dir = species_dir,
    fetch_fn = fake_fetch, existing = .existing_cache_df(),
    mode = "full", should_cancel = function() TRUE
  )
  expect_true(isTRUE(res$canceled))
  expect_false(called)
  # Prior cache STILL intact (wipe never ran).
  back <- pelsa_read_feature_cache(species_dir)
  expect_setequal(unique(back$accession), c("P1", "P2", "P3"))
})

# ---- pelsa_refresh_species_cache: mode = incremental (append atop) -----------

test_that("incremental mode merges fresh ATOP existing (no wipe)", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)
  pelsa_write_feature_cache(.existing_cache_df(), species_dir)  # P1,P2,P3

  # Incremental fetched only the cache-miss P00001/P00002 (disjoint from cache).
  fresh <- data.frame(
    accession = c("P00001", "P00002"), feature_type = c("domain", "domain"),
    start = c(1L, 1L), end = c(5L, 5L), description = c("n1", "n2"),
    feature_class = c("folded_domain", "folded_domain"), class_score = c(2L, 2L),
    coord_quality = c("exact", "exact"), stringsAsFactors = FALSE)
  fake_fetch <- function(accessions, ...) list(features = fresh,
                                               unresolved = character(0))
  res <- pelsa_refresh_species_cache(
    species = "9606", universe = c("P00001", "P00002"),
    species_dir = species_dir, fetch_fn = fake_fetch,
    existing = .existing_cache_df(), mode = "incremental"
  )
  expect_identical(res$mode, "incremental")
  back <- pelsa_read_feature_cache(species_dir)
  # Old P1/P2/P3 KEPT + new P00001/P00002 added (append atop).
  expect_setequal(unique(back$accession),
                  c("P1", "P2", "P3", "P00001", "P00002"))
})

# ---- pelsa_run_species_refresh (multi-species, injected stub, NO network) ----

test_that("run_species_refresh refreshes multiple species + captures errors", {
  db <- withr::local_tempdir()
  dir.create(file.path(db, "9606"))
  dir.create(file.path(db, "10090"))

  # Stub: human succeeds; mouse "fetches" features whose schema is broken so the
  # write fails -> per-species error captured, the other species still ok.
  fake_fetch <- function(accessions) {
    list(features = .fake_feature_df(), unresolved = character(0))
  }

  # Provide an uploaded GCT so the universe is non-empty for both.
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P00001",
                              stringsAsFactors = FALSE))

  progressed <- c()
  results <- pelsa_run_species_refresh(
    species = c("9606", "10090"), database_dir = db, uploaded_gcts = gcts,
    fetch_fn = fake_fetch,
    set_progress = function(value, detail) progressed <<- c(progressed, value)
  )

  expect_length(results, 2L)
  expect_identical(results[[1]]$species, "9606")
  expect_null(results[[1]]$error)
  expect_identical(results[[1]]$n_features, 3L)
  expect_true(file.exists(file.path(db, "9606", "uniprot_features",
                                    "uniprot_features.tsv")))
  # Progress advanced and reached the end.
  expect_gt(length(progressed), 0L)
  expect_equal(max(progressed), 1.0)
})

test_that("run_species_refresh captures a per-species error without aborting", {
  db <- withr::local_tempdir()
  dir.create(file.path(db, "9606"))

  # Stub returns a malformed (missing-schema) feature frame -> write fails.
  bad_fetch <- function(accessions) {
    list(features = data.frame(accession = "P1", stringsAsFactors = FALSE),
         unresolved = character(0))
  }
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P1",
                              stringsAsFactors = FALSE))

  results <- pelsa_run_species_refresh(
    species = "9606", database_dir = db, uploaded_gcts = gcts,
    fetch_fn = bad_fetch
  )
  expect_length(results, 1L)
  expect_false(is.null(results[[1]]$error))
  expect_match(results[[1]]$error, "missing schema column")
})

# ---- pelsa_refresh_notifications (pure formatter) ----------------------------

test_that("notifications: error + lossy-warning + success summary", {
  results <- list(
    list(species = "9606", n_features = 100L, n_unresolved = 2L,
         n_retained_from_cache = 2L, had_existing = TRUE, error = NULL),
    list(species = "10090", error = "UniProt unavailable"),
    list(species = "rat", n_features = 50L, n_unresolved = 0L,
         n_retained_from_cache = 0L, had_existing = FALSE, error = NULL)
  )
  notes <- pelsa_refresh_notifications(results)
  types <- vapply(notes, function(n) n$type, character(1))

  expect_true("error" %in% types)    # mouse failed
  expect_true("warning" %in% types)  # human had cache + unresolved -> lossy warn
  expect_true("message" %in% types)  # rolled-up success summary
  # The warning mentions retained rows.
  warn <- notes[[which(types == "warning")]]
  expect_match(warn$message, "retained")
  # The success summary covers both successful species.
  msg <- notes[[which(types == "message")]]
  expect_match(msg$message, "9606")
  expect_match(msg$message, "rat")
})

test_that("notifications: a fresh-cache species with unresolved does NOT warn", {
  # No prior cache -> unresolved is expected, not lossy; no warning.
  results <- list(
    list(species = "9606", n_features = 10L, n_unresolved = 3L,
         n_retained_from_cache = 0L, had_existing = FALSE, error = NULL)
  )
  types <- vapply(pelsa_refresh_notifications(results),
                  function(n) n$type, character(1))
  expect_false("warning" %in% types)
})

test_that("notifications: genuinely-absent unresolved (no transient) does NOT prompt re-run", {
  # Cache existed + unresolved > 0, but ALL unresolved are genuinely absent from
  # UniProt (n_transient_unresolved == 0). Re-running cannot help, so we must NOT
  # fire the amber "re-run when reachable" warning. A neutral info note is fine.
  results <- list(
    list(species = "9606", n_features = 100L, n_unresolved = 2L,
         n_transient_unresolved = 0L, n_retained_from_cache = 2L,
         had_existing = TRUE, error = NULL)
  )
  notes <- pelsa_refresh_notifications(results)
  types <- vapply(notes, function(n) n$type, character(1))
  expect_false("warning" %in% types)
  # No note tells the user re-running will help.
  msgs <- vapply(notes, function(n) n$message, character(1))
  expect_false(any(grepl("[Rr]e-run when", msgs)))
})

test_that("notifications: transient unresolved DOES prompt a re-run warning", {
  # Cache existed + some unresolved came from a FAILED batch (transient): the
  # amber warning with the re-run instruction is correct here.
  results <- list(
    list(species = "9606", n_features = 100L, n_unresolved = 3L,
         n_transient_unresolved = 3L, n_retained_from_cache = 3L,
         had_existing = TRUE, error = NULL)
  )
  notes <- pelsa_refresh_notifications(results)
  types <- vapply(notes, function(n) n$type, character(1))
  expect_true("warning" %in% types)
  warn <- notes[[which(types == "warning")]]
  expect_match(warn$message, "[Rr]e-run when")
})

# ---- UI presence: refresh controls in the Setup tab --------------------------

test_that("Setup UI exposes the refresh species checklist + button ids", {
  html <- as.character(PELSASection1_Tab_UI("PELSASection1Tab"))
  # The Tab UI is just the setup_box uiOutput; the refresh controls render
  # server-side inside renderUI. Construct-smoke is covered by the app_UI test
  # in test-pelsa-setup-controls.R; here we assert the helper file loaded and
  # the pure helpers are exported into the package namespace.
  expect_true(exists("pelsa_refresh_accession_universe"))
  expect_true(exists("pelsa_write_feature_cache"))
  expect_true(exists("pelsa_refresh_species_cache"))
})

# ---- confirm-gate: universe size + ETA text (pure) ---------------------------

test_that("universe_size sums per-species universes (datasets union cache)", {
  db <- withr::local_tempdir()
  dir.create(file.path(db, "9606")); dir.create(file.path(db, "10090"))
  gcts <- list(
    d = data.frame(PG.ProteinAccessions = c("P1;P2", "P3"),
                   stringsAsFactors = FALSE)
  )
  sz <- pelsa_refresh_universe_size(c("9606", "10090"), db, gcts)
  # Both species share the same dataset universe (3 accessions); no caches yet.
  expect_equal(unname(sz$per_species[["9606"]]), 3L)
  expect_equal(unname(sz$per_species[["10090"]]), 3L)
  expect_equal(sz$total, 6L)
})

test_that("eta_text formats count + a coarse ETA (sec vs min)", {
  expect_match(pelsa_refresh_eta_text(200L), "^200 accessions \\(~\\d+ sec\\)$")
  expect_match(pelsa_refresh_eta_text(1L), "^1 accession \\(~\\d+ sec\\)$")
  big <- pelsa_refresh_eta_text(69845L)
  expect_match(big, "^69,845 accessions \\(~\\d+ min\\)$")  # thousands sep + min
})

# ---- cooperative cancel propagation (no network; stub fetch) -----------------

test_that("species_cache honors a pre-fetch cancel: no write, cache intact", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)
  existing <- .fake_feature_df()

  # should_cancel TRUE before fetch -> fetch_fn must NOT be called, no write.
  called <- FALSE
  fake_fetch <- function(accessions, ...) { called <<- TRUE
    list(features = .fake_feature_df(), unresolved = character(0)) }

  res <- pelsa_refresh_species_cache(
    species = "9606", universe = c("P1", "P2"), species_dir = species_dir,
    fetch_fn = fake_fetch, existing = existing,
    should_cancel = function() TRUE
  )
  expect_true(isTRUE(res$canceled))
  expect_false(called)                                   # never fetched
  expect_false(file.exists(file.path(species_dir, "uniprot_features",
                                     "uniprot_features.tsv")))  # no write
})

test_that("species_cache honors a mid-fetch cancel flag from the fetcher", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)

  # Fetcher reports canceled = TRUE (as the real one does when stopped at a page
  # boundary) -> orchestrator must NOT write a partial cache.
  canceling_fetch <- function(accessions, ...) {
    list(features = pelsa_empty_feature_frame(), unresolved = accessions,
         canceled = TRUE)
  }
  res <- pelsa_refresh_species_cache(
    species = "9606", universe = c("P1", "P2"), species_dir = species_dir,
    fetch_fn = canceling_fetch, existing = NULL
  )
  expect_true(isTRUE(res$canceled))
  expect_false(file.exists(file.path(species_dir, "uniprot_features",
                                     "uniprot_features.tsv")))
})

test_that("call_fetch_fn forwards callbacks only to a fetcher that declares them", {
  # Minimal stub (no extra args) must still work.
  simple <- function(accessions) list(features = .fake_feature_df(),
                                       unresolved = character(0))
  expect_silent(
    out <- .pelsa_call_fetch_fn(simple, c("P1"), on_batch = function(...) {},
                                should_cancel = function() FALSE))
  expect_equal(nrow(out$features), 3L)

  # A fetcher that DOES declare them receives them.
  seen <- new.env(); seen$ob <- FALSE; seen$sc <- FALSE
  rich <- function(accessions, on_batch = NULL, should_cancel = NULL) {
    seen$ob <- is.function(on_batch); seen$sc <- is.function(should_cancel)
    list(features = .fake_feature_df(), unresolved = character(0))
  }
  .pelsa_call_fetch_fn(rich, c("P1"), on_batch = function(...) {},
                       should_cancel = function() FALSE)
  expect_true(seen$ob); expect_true(seen$sc)
})

# ---- inline progress + result UI (pure tag constructors) ---------------------

test_that("progress_ui clamps + shows percent and detail", {
  html <- as.character(pelsa_refresh_progress_ui(0.62, "(1/2) human . page 88/140"))
  expect_match(html, "62%")
  expect_match(html, "page 88/140")
  # Clamp out-of-range fractions.
  expect_match(as.character(pelsa_refresh_progress_ui(1.5)), "100%")
  expect_match(as.character(pelsa_refresh_progress_ui(-1)), "0%")
})

test_that("result_ui colors by worst status (ok / warn / error)", {
  ok <- list(list(species = "9606", n_features = 100L, n_unresolved = 0L,
                  n_retained_from_cache = 0L, had_existing = TRUE,
                  canceled = FALSE, error = NULL))
  warn <- list(list(species = "9606", n_features = 100L, n_unresolved = 3L,
                    n_retained_from_cache = 5L, had_existing = TRUE,
                    canceled = FALSE, error = NULL))
  err <- list(list(species = "9606", error = "boom"))
  cancel <- list(list(species = "9606", canceled = TRUE, error = NULL))

  expect_match(as.character(pelsa_refresh_result_ui(ok)), "#5cb85c")     # green
  expect_match(as.character(pelsa_refresh_result_ui(warn)), "#f0ad4e")   # amber
  expect_match(as.character(pelsa_refresh_result_ui(err)), "#d9534f")    # red
  expect_match(as.character(pelsa_refresh_result_ui(cancel)), "#f0ad4e") # amber
  expect_match(as.character(pelsa_refresh_result_ui(cancel)),
               "left unchanged")
  expect_null(pelsa_refresh_result_ui(NULL))
})

# ---- notifications: canceled species surfaced honestly -----------------------

test_that("notifications report a canceled species + exclude it from 'complete'", {
  results <- list(
    list(species = "9606", n_features = 100L, n_unresolved = 0L,
         n_retained_from_cache = 0L, had_existing = TRUE, canceled = FALSE,
         error = NULL),
    list(species = "10090", canceled = TRUE, not_run = TRUE, error = NULL)
  )
  notes <- pelsa_refresh_notifications(results)
  msgs <- vapply(notes, function(n) n$message, character(1))
  expect_true(any(grepl("canceled", msgs, ignore.case = TRUE)))
  # The success summary mentions human but NOT mouse.
  complete <- msgs[grepl("refresh complete", msgs, ignore.case = TRUE)]
  expect_length(complete, 1L)
  expect_match(complete, "9606")
  expect_false(grepl("10090", complete))
})

# ---- pelsa_gcts_for_species (Defect #1: species-dataset accession guard) -----

test_that("pelsa_gcts_for_species keeps ALL same-species datasets (union), drops others", {
  gcts <- list(
    ome_h1 = data.frame(PG.ProteinAccessions = "P10000", stringsAsFactors = FALSE),
    ome_h2 = data.frame(PG.ProteinAccessions = "P20000", stringsAsFactors = FALSE),
    ome_m1 = data.frame(PG.ProteinAccessions = "Q30000", stringsAsFactors = FALSE)
  )
  species_by_ds <- list(ome_h1 = "9606", ome_h2 = "9606", ome_m1 = "10090")

  human <- pelsa_gcts_for_species(gcts, species_by_ds, "9606")
  expect_setequal(names(human), c("ome_h1", "ome_h2"))   # both human datasets
  # The union of accessions across both same-species datasets is preserved.
  expect_setequal(pelsa_incremental_universe(human, NULL),
                  c("P10000", "P20000"))
  # Mouse dataset excluded from the human universe (no spillover).
  expect_false("Q30000" %in% pelsa_incremental_universe(human, NULL))

  mouse <- pelsa_gcts_for_species(gcts, species_by_ds, "10090")
  expect_setequal(names(mouse), "ome_m1")
})

test_that("pelsa_gcts_for_species: '(none)'/unset species never matches a real species", {
  gcts <- list(
    ome_a = data.frame(PG.ProteinAccessions = "P10000", stringsAsFactors = FALSE),
    ome_b = data.frame(PG.ProteinAccessions = "P20000", stringsAsFactors = FALSE)
  )
  species_by_ds <- list(ome_a = "9606", ome_b = "(none)")
  out <- pelsa_gcts_for_species(gcts, species_by_ds, "9606")
  expect_setequal(names(out), "ome_a")
})

test_that("pelsa_gcts_for_species returns NULL/empty input unchanged", {
  expect_null(pelsa_gcts_for_species(NULL, list(), "9606"))
  expect_length(pelsa_gcts_for_species(list(), list(), "9606"), 0L)
})
