################################################################################
# Tests for PELSA species classification + resolution (taxonomy-code convention).
#
#   pelsa_classify_folder(folder)                  "numeric" | "named"
#   pelsa_fetch_taxon(taxon_id, ...)               taxonomy name/validation fetch
#   pelsa_read_species_meta / pelsa_write_species_meta(database_dir, ...)
#   pelsa_species_has_feature_cache(database_dir, folder)
#   pelsa_resolve_species(database_dir, folder, validate_fn, meta)
#   pelsa_refresh_species_meta_on_start(database_dir, validate_fn)
#   pelsa_species_display_label(struct)
#
# The taxonomy API is NEVER hit live: every test injects a `validate_fn` stub.
# A folder named by digits is a UniProt taxon code; a named folder is self-curated.
################################################################################

# ---- helpers -----------------------------------------------------------------

# A stub validate_fn factory mirroring pelsa_fetch_taxon's return contract:
#   list(status = "ok"|"not_found"|"network_error",
#        scientific_name=, common_name=, taxon_id=)
.stub_ok <- function(sci = "Homo sapiens", common = "Human") {
  function(taxon_id, ...) list(status = "ok", scientific_name = sci,
                               common_name = common, taxon_id = taxon_id)
}
.stub_not_found <- function() {
  function(taxon_id, ...) list(status = "not_found", scientific_name = NA_character_,
                               common_name = NA_character_, taxon_id = taxon_id)
}
.stub_network <- function() {
  function(taxon_id, ...) list(status = "network_error",
                               scientific_name = NA_character_,
                               common_name = NA_character_, taxon_id = taxon_id)
}

# Build a database dir with the given folders; optionally drop a feature-cache
# tsv into a folder to simulate "has_feature_cache".
.make_db <- function(folders = character(0), with_cache = character(0)) {
  db <- tempfile("pelsa_db_")
  dir.create(db)
  for (f in folders) dir.create(file.path(db, f))
  for (f in with_cache) {
    fdir <- file.path(db, f, "uniprot_features")
    dir.create(fdir, recursive = TRUE)
    writeLines("accession\tstart\tend\tfeature_class",
               file.path(fdir, "uniprot_features.tsv"))
  }
  db
}

# ---- pelsa_classify_folder ---------------------------------------------------

test_that("pelsa_classify_folder: all-digits -> numeric, else named", {
  expect_identical(pelsa_classify_folder("9606"), "numeric")
  expect_identical(pelsa_classify_folder("10090"), "numeric")
  expect_identical(pelsa_classify_folder("009606"), "numeric")  # leading zeros
  expect_identical(pelsa_classify_folder("hoylesellaTimonensis"), "named")
  expect_identical(pelsa_classify_folder("strain1"), "named")   # mixed -> named
  expect_identical(pelsa_classify_folder("9606b"), "named")
})

# ---- pelsa_species_has_feature_cache -----------------------------------------

test_that("pelsa_species_has_feature_cache detects a cache tsv without reading it", {
  db <- .make_db(folders = c("9606", "10090"), with_cache = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  expect_true(pelsa_species_has_feature_cache(db, "9606"))
  expect_false(pelsa_species_has_feature_cache(db, "10090"))
})

# ---- species_meta read/write round-trip --------------------------------------

test_that("species_meta write then read round-trips; absent file -> empty list", {
  db <- .make_db()
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  expect_identical(pelsa_read_species_meta(db), list())

  meta <- list(
    "9606" = list(type = "uniprot", taxon_id = 9606L,
                  scientific_name = "Homo sapiens", validated = TRUE)
  )
  pelsa_write_species_meta(db, meta)
  rt <- pelsa_read_species_meta(db)
  expect_identical(rt[["9606"]]$type, "uniprot")
  expect_identical(rt[["9606"]]$scientific_name, "Homo sapiens")
  expect_true(isTRUE(rt[["9606"]]$validated))
})

# ---- pelsa_resolve_species: the five verdict branches ------------------------

test_that("resolve: numeric + validation ok -> uniprot, validated, display name", {
  db <- .make_db(folders = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  s <- pelsa_resolve_species(db, "9606", validate_fn = .stub_ok("Homo sapiens"))
  expect_identical(s$type, "uniprot")
  expect_true(s$validated)
  expect_identical(s$scientific_name, "Homo sapiens")
  expect_identical(s$display, "Homo sapiens (9606)")
  expect_identical(s$folder, "9606")
  # The verdict is persisted so a re-resolve needs no network.
  meta <- pelsa_read_species_meta(db)
  expect_identical(meta[["9606"]]$type, "uniprot")
  expect_true(isTRUE(meta[["9606"]]$validated))
})

test_that("resolve: numeric + 404 not_found -> self_curated (customized)", {
  db <- .make_db(folders = "9999999")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  s <- pelsa_resolve_species(db, "9999999", validate_fn = .stub_not_found())
  expect_identical(s$type, "self_curated")
  expect_identical(s$display, "9999999 (customized)")
})

test_that("resolve: numeric + network_error + has cache -> uniprot unvalidated", {
  db <- .make_db(folders = "9606", with_cache = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  s <- pelsa_resolve_species(db, "9606", validate_fn = .stub_network())
  expect_identical(s$type, "uniprot")
  expect_false(s$validated)
  expect_identical(s$display, "9606 (annotations available, name pending)")
})

test_that("resolve: numeric + network_error + no cache -> self_curated (transient)", {
  db <- .make_db(folders = "9606")  # no cache
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  s <- pelsa_resolve_species(db, "9606", validate_fn = .stub_network())
  expect_identical(s$type, "self_curated")
  expect_identical(s$display, "9606 (customized)")
})

test_that("resolve: named folder -> self_curated, no network call", {
  db <- .make_db(folders = "hoylesellaTimonensis")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  called <- FALSE
  vf <- function(...) { called <<- TRUE; .stub_ok()(...) }
  s <- pelsa_resolve_species(db, "hoylesellaTimonensis", validate_fn = vf)
  expect_identical(s$type, "self_curated")
  expect_identical(s$display, "hoylesellaTimonensis (customized)")
  expect_false(called)  # named folders never touch the network
})

test_that("resolve: a cached validated entry is reused without calling validate_fn", {
  db <- .make_db(folders = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  pelsa_write_species_meta(db, list(
    "9606" = list(type = "uniprot", taxon_id = 9606L,
                  scientific_name = "Homo sapiens", validated = TRUE)))
  called <- FALSE
  vf <- function(...) { called <<- TRUE; .stub_ok()(...) }
  s <- pelsa_resolve_species(db, "9606", validate_fn = vf)
  expect_identical(s$display, "Homo sapiens (9606)")
  expect_false(called)
})

# ---- cache-only path (allow_fetch = FALSE): never touch the network ----------

test_that("allow_fetch=FALSE never calls validate_fn (reactive render path)", {
  db <- .make_db(folders = "9606", with_cache = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  called <- FALSE
  vf <- function(...) { called <<- TRUE; .stub_ok()(...) }

  # No cached verdict yet + has feature cache -> uniprot unvalidated, no network.
  s <- pelsa_resolve_species(db, "9606", validate_fn = vf, allow_fetch = FALSE)
  expect_false(called)
  expect_identical(s$type, "uniprot")
  expect_false(s$validated)
  expect_identical(s$display, "9606 (annotations available, name pending)")
})

test_that("allow_fetch=FALSE: numeric + no cache + no verdict -> self_curated, no network", {
  db <- .make_db(folders = "9606")  # no cache
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  called <- FALSE
  vf <- function(...) { called <<- TRUE; .stub_ok()(...) }
  s <- pelsa_resolve_species(db, "9606", validate_fn = vf, allow_fetch = FALSE)
  expect_false(called)
  expect_identical(s$type, "self_curated")
})

test_that("allow_fetch=FALSE honors a cached validated verdict (display name shown)", {
  db <- .make_db(folders = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  pelsa_write_species_meta(db, list(
    "9606" = list(type = "uniprot", taxon_id = 9606L,
                  scientific_name = "Homo sapiens", validated = TRUE)))
  s <- pelsa_resolve_species(db, "9606", allow_fetch = FALSE)
  expect_identical(s$display, "Homo sapiens (9606)")
})

# ---- refresh-on-start: promote a previously-unvalidated numeric folder -------

test_that("refresh_on_start promotes unvalidated numeric folder and rewrites meta", {
  db <- .make_db(folders = "9606", with_cache = "9606")
  on.exit(unlink(db, recursive = TRUE), add = TRUE)
  # Seed an unvalidated entry (e.g. earlier offline run).
  pelsa_write_species_meta(db, list(
    "9606" = list(type = "uniprot", taxon_id = 9606L,
                  scientific_name = NA, validated = FALSE)))

  pelsa_refresh_species_meta_on_start(db, validate_fn = .stub_ok("Homo sapiens"))

  meta <- pelsa_read_species_meta(db)
  expect_true(isTRUE(meta[["9606"]]$validated))
  expect_identical(meta[["9606"]]$scientific_name, "Homo sapiens")
})

# ---- display label for each state --------------------------------------------

test_that("pelsa_species_display_label formats all three states", {
  expect_identical(
    pelsa_species_display_label(list(folder = "9606", type = "uniprot",
      validated = TRUE, scientific_name = "Homo sapiens")),
    "Homo sapiens (9606)")
  expect_identical(
    pelsa_species_display_label(list(folder = "9606", type = "uniprot",
      validated = FALSE, scientific_name = NA)),
    "9606 (annotations available, name pending)")
  expect_identical(
    pelsa_species_display_label(list(folder = "hoylesellaTimonensis",
      type = "self_curated", validated = TRUE, scientific_name = NA)),
    "hoylesellaTimonensis (customized)")
})
