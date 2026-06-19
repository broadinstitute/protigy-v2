# PELSA Full vs. Incremental UniProt Refresh Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split the single PELSA "Refresh annotation library" control into two modes — a destructive **Full library refresh** (wipe the species folder except `fasta/`, then fetch the whole FASTA proteome) and a non-destructive **Incremental refresh** (fetch only `(dataset ∪ fasta) − cache` and append atop the existing cache) — with the incremental button disabled until a populated feature cache exists.

**Architecture:** The existing pipeline is *observer (`tab_pelsa_section1.R`) → `pelsa_run_species_refresh` → `pelsa_refresh_species_cache` → fetch + `pelsa_merge_feature_cache` + `pelsa_write_feature_cache`*. We thread a single `mode` argument (`"full"` | `"incremental"`) through the orchestrators, branching at exactly three points: (1) the accession **universe** (two new pure functions replace `pelsa_refresh_accession_universe`), (2) a pre-fetch **wipe** (new `pelsa_wipe_species_cache`, full only), and (3) the **merge base** (`existing = NULL` for full, real cache for incremental). Everything else — fetch, progress, atomic write, cancel, notifications, inline result UI — is shared. The UI gains a second button and a reactive disable-guard; both confirm unconditionally before running.

**Tech Stack:** R / Shiny module, `testthat` (offline, injected `fetch_fn` stub — NO live network), `httr2` (existing fetcher, untouched), `shinyjs` (button enable/disable), `shinyalert` (confirm dialog), `withr` (tempdirs in tests).

## Global Constraints

- **ASCII-only R source.** No literal Unicode in `R/`; use `\uXXXX` escapes. (CLAUDE.md — enforced; non-ASCII breaks `R CMD check`.)
- **Reload before testing.** After any `R/` edit run `devtools::load_all(".")`; tests exercise the *loaded* package, not source files. After roxygen `@export`/`@import` changes run `devtools::document()`. (No new exports are added here — all helpers are `@noRd`.)
- **No live network in tests.** `pelsa_fetch_uniprot` is never called in tests; inject a stub `fetch_fn`. (tests/testthat/test-pelsa-refresh.R header.)
- **`%||%`** is from rlang (already imported in `R/protigy-package.R`); do not assume base.
- **Single-select species.** The refresh control is `radioButtons("pelsa_refresh_species")` with `selected = character(0)` default — `selected` is length-0 (nothing) or length-1 (one species).
- **`ns()` rule.** Required for every `inputId`/`outputId` in UI / `renderUI()`; NOT used when referencing `input$`/`output$` or in `update*Input()`.
- **Mode values are the exact strings `"full"` and `"incremental"`** everywhere they appear.
- **Decision: "in cache" = accession appears in `cache$accession`** (≥1 feature row). Zero-feature resolutions are re-queried each incremental run; accepted.
- **Decision: full-fetch wipe deletes everything under `inst/database/<species>/` EXCEPT `fasta/`** — including `uniprot_membrane/`. Delete happens **before** fetch (pre-delete accepted, even though a failed fetch then leaves the species fasta-only).
- **Decision: both modes confirm unconditionally** (no size threshold) — full with destructive-wipe wording, incremental with append wording.

---

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `R/tab_pelsa_refresh_helpers.R` | Pure universe / wipe / orchestration helpers | MODIFY — replace `pelsa_refresh_accession_universe` with `pelsa_full_universe` + `pelsa_incremental_universe`; add `pelsa_wipe_species_cache`; thread `mode` through `pelsa_refresh_species_cache`, `pelsa_run_species_refresh`, `pelsa_refresh_universe_size`; mode-aware wording in `pelsa_refresh_notifications` + `pelsa_refresh_result_ui` |
| `R/tab_pelsa_section1_helpers.R` | Setup-box UI (maintenance section) | MODIFY — relabel `pelsa_refresh_btn` to "Full library refresh", add `pelsa_incremental_btn`, update helpText |
| `R/tab_pelsa_section1.R` | Module observer wiring | MODIFY — two `observeEvent`s → shared `run_refresh(selected, gcts, mode)`; in-flight lock on both buttons; reactive incremental disable-guard; unconditional confirm |
| `tests/testthat/test-pelsa-refresh.R` | Offline unit tests | MODIFY — migrate `pelsa_refresh_accession_universe` tests to the two new fns; add wipe, mode-aware orchestration, size, notification/UI, round-trip tests |

No new files. All helpers stay `@noRd` (no `NAMESPACE`/`document()` churn).

---

## Interfaces (signatures other tasks rely on)

```r
# Universe (pure; replace pelsa_refresh_accession_universe)
pelsa_full_universe(gcts, existing_cache, fasta_map = NULL) -> character  # FASTA acc only
pelsa_incremental_universe(gcts, existing_cache, fasta_map = NULL) -> character  # (dataset ∪ fasta) − cache

# Wipe (filesystem; full only)
pelsa_wipe_species_cache(species_dir) -> invisible(character)  # deleted entries; spares fasta/; no-op-safe

# Orchestration (mode threaded)
pelsa_refresh_species_cache(species, universe, species_dir, fetch_fn = pelsa_fetch_uniprot,
                            existing = NULL, progress = NULL, should_cancel = NULL,
                            mode = "incremental") -> list(...)  # full: wipes + existing forced NULL
pelsa_run_species_refresh(species, database_dir, uploaded_gcts, fetch_fn = pelsa_fetch_uniprot,
                          set_progress = NULL, should_cancel = NULL,
                          mode = "incremental") -> list(per-species; each carries $mode)
pelsa_refresh_universe_size(species, database_dir, uploaded_gcts, mode = "incremental")
                          -> list(total = int, per_species = named int)

# UI helper (unchanged signature; new button id rendered)
pelsa_setup_box_ui(species, compounds, ns, selected_species, selected_compound,
                   selected_skip, refresh_species)  # renders pelsa_refresh_btn + pelsa_incremental_btn
```

`pelsa_merge_feature_cache`, `pelsa_write_feature_cache`, `pelsa_read_feature_cache`, `pelsa_read_fasta`, `pelsa_species_refresh_inputs`, `pelsa_gcts_for_species`, `pelsa_refresh_eta_text`, `pelsa_refresh_progress_ui` are **unchanged** and consumed as-is.

---

### Task 1: Replace the accession-universe function with full + incremental variants

**Files:**
- Modify: `R/tab_pelsa_refresh_helpers.R` (replace `pelsa_refresh_accession_universe`, lines ~96-150; keep the `.pelsa_dataset_accession_strings` / `.pelsa_explode_accession_tokens` internals)
- Test: `tests/testthat/test-pelsa-refresh.R` (replace the `pelsa_refresh_accession_universe` block, lines ~28-81)

**Interfaces:**
- Consumes: `.pelsa_dataset_accession_strings`, `.pelsa_explode_accession_tokens` (existing internals, unchanged).
- Produces: `pelsa_full_universe(gcts, existing_cache, fasta_map = NULL)`, `pelsa_incremental_universe(gcts, existing_cache, fasta_map = NULL)`.

**Semantics:**
- `pelsa_full_universe`: FASTA accessions only (`names(fasta_map)`), ignoring datasets and cache. Empty FASTA → `character(0)`. Sorted unique.
- `pelsa_incremental_universe`: `sort(unique((dataset_acc ∪ fasta_acc)))` minus `cache_acc`. `dataset_acc` exploded from `gcts` `PG.ProteinAccessions`; `fasta_acc` from `names(fasta_map)`; `cache_acc` from `existing_cache$accession`. Result is disjoint from the cache by construction.

- [ ] **Step 1: Write the failing tests**

Replace the `# ---- pelsa_refresh_accession_universe ----` block (lines ~28-81) in `tests/testthat/test-pelsa-refresh.R` with:

```r
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
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: FAIL — `could not find function "pelsa_full_universe"` / `"pelsa_incremental_universe"`.

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_refresh_helpers.R`, DELETE the whole `pelsa_refresh_accession_universe` function (the `# ---- Helper 1: accession universe ----` block, lines ~96-150) and replace it with:

```r
# ---- Helper 1: accession universes (mode-specific) ---------------------------

# Internal: exploded unique dataset accessions across all uploaded GCTs.
# @noRd
.pelsa_dataset_universe <- function(gcts) {
  if (is.null(gcts) || length(gcts) == 0L) return(character(0))
  raw <- unlist(lapply(gcts, .pelsa_dataset_accession_strings), use.names = FALSE)
  .pelsa_explode_accession_tokens(raw)
}

# Internal: FASTA accessions (the proteome) from a fasta_map (names), cleaned.
# @noRd
.pelsa_fasta_universe <- function(fasta_map) {
  if (is.null(fasta_map) || length(fasta_map) == 0L) return(character(0))
  acc <- names(fasta_map)
  if (is.null(acc)) return(character(0))
  unique(acc[!is.na(acc) & nzchar(acc)])
}

# Internal: accessions already present in an existing feature cache.
# @noRd
.pelsa_cache_universe <- function(existing_cache) {
  if (is.null(existing_cache) || !is.data.frame(existing_cache) ||
      !("accession" %in% colnames(existing_cache)) ||
      nrow(existing_cache) == 0L) {
    return(character(0))
  }
  acc <- unique(as.character(existing_cache$accession))
  acc[!is.na(acc) & nzchar(acc)]
}

# FULL-mode universe: the whole FASTA proteome ONLY. A full refresh wipes the
# species' feature cache and rebuilds it from the FASTA; it deliberately ignores
# uploaded-dataset accessions (those are topped up via an incremental refresh)
# AND the existing cache (which has just been wiped). Sorted unique.
#
# @param gcts          uploaded datasets (IGNORED; kept for a uniform signature).
# @param existing_cache the species' current cache (IGNORED; wiped before fetch).
# @param fasta_map     named list accession -> sequence (from pelsa_read_fasta).
# @return character vector of FASTA accessions (sorted unique; may be empty).
# @noRd
pelsa_full_universe <- function(gcts, existing_cache, fasta_map = NULL) {
  sort(.pelsa_fasta_universe(fasta_map))
}

# INCREMENTAL-mode universe: (uploaded-dataset accessions UNION FASTA accessions)
# MINUS the accessions already in the existing cache. Drives the species toward
# full proteome + dataset coverage over repeated runs WITHOUT re-fetching what is
# already cached. Disjoint from the cache by construction. Sorted unique.
#
# @param gcts          uploaded datasets (cmapR GCTs or data.frames carrying
#                      PG.ProteinAccessions), or NULL/empty. Already filtered to
#                      the target species upstream (pelsa_gcts_for_species).
# @param existing_cache the species' current feature data.frame (needs an
#                      `accession` column); may be NULL/0-row.
# @param fasta_map     named list accession -> sequence (from pelsa_read_fasta);
#                      always included (not just a no-datasets fallback).
# @return character vector of accessions to (re)fetch (sorted unique).
# @noRd
pelsa_incremental_universe <- function(gcts, existing_cache, fasta_map = NULL) {
  needed <- unique(c(.pelsa_dataset_universe(gcts),
                     .pelsa_fasta_universe(fasta_map)))
  sort(setdiff(needed, .pelsa_cache_universe(existing_cache)))
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: the 7 universe tests PASS. Other tests in the file may still fail (they reference downstream changes / the removed function) — that is expected until later tasks.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_refresh_helpers.R tests/testthat/test-pelsa-refresh.R
git commit -m "feat(pelsa): split refresh universe into full (fasta-only) + incremental ((dataset U fasta) - cache)"
```

---

### Task 2: Add `pelsa_wipe_species_cache` (full-mode pre-fetch wipe)

**Files:**
- Modify: `R/tab_pelsa_refresh_helpers.R` (add after the universe helpers, before `# ---- Helper 2: write the feature cache ----`)
- Test: `tests/testthat/test-pelsa-refresh.R` (add a new block after the universe tests)

**Interfaces:**
- Produces: `pelsa_wipe_species_cache(species_dir) -> invisible(character)` (the deleted top-level entry names).

**Semantics:** Delete every top-level entry under `species_dir` EXCEPT a `fasta` directory (case-sensitive folder name `fasta`). No-op-safe when `species_dir` doesn't exist. Uses `unlink(recursive = TRUE)`.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-pelsa-refresh.R` after the incremental-universe tests:

```r
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
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: FAIL — `could not find function "pelsa_wipe_species_cache"`.

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_refresh_helpers.R`, add immediately after `pelsa_incremental_universe` (before `# ---- Helper 2: write the feature cache ----`):

```r
# ---- Helper 1b: full-mode clean-slate wipe -----------------------------------

# Delete every top-level entry under a species directory EXCEPT the `fasta/`
# folder (and its contents) -- the clean-slate a FULL refresh performs BEFORE
# re-fetching the proteome. This removes the prior uniprot_features/ cache AND
# the uniprot_membrane/ annotation (both regenerable / re-obtainable; the feature
# cache is rebuilt by the ensuing full fetch). DESTRUCTIVE + irreversible: the
# membrane file is gitignored and not produced by this app. Called only on the
# full-refresh path, only AFTER the user confirms.
#
# No-op-safe: a missing species_dir deletes nothing and returns character(0).
#
# @param species_dir the species directory (file.path(database_dir, species)).
# @return invisibly, the character vector of deleted top-level entry names.
# @noRd
pelsa_wipe_species_cache <- function(species_dir) {
  if (!is.character(species_dir) || length(species_dir) != 1L ||
      is.na(species_dir) || !nzchar(species_dir) || !dir.exists(species_dir)) {
    return(invisible(character(0)))
  }
  entries <- list.files(species_dir, all.files = TRUE, no.. = TRUE)
  to_delete <- setdiff(entries, "fasta")
  for (e in to_delete) {
    unlink(file.path(species_dir, e), recursive = TRUE, force = TRUE)
  }
  invisible(to_delete)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: the 3 wipe tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_refresh_helpers.R tests/testthat/test-pelsa-refresh.R
git commit -m "feat(pelsa): add pelsa_wipe_species_cache (full-refresh clean slate, spares fasta/)"
```

---

### Task 3: Thread `mode` through `pelsa_refresh_species_cache` (wipe + universe selection)

**Files:**
- Modify: `R/tab_pelsa_refresh_helpers.R` (`pelsa_refresh_species_cache`, lines ~445-539)
- Test: `tests/testthat/test-pelsa-refresh.R` (add mode tests after the existing `pelsa_refresh_species_cache` block, ~line 367)

**Interfaces:**
- Consumes: `pelsa_wipe_species_cache` (Task 2), `pelsa_merge_feature_cache`, `pelsa_write_feature_cache` (existing).
- Produces: `pelsa_refresh_species_cache(..., mode = "incremental")`; on `mode == "full"` it wipes `species_dir` before fetch and forces `existing = NULL`; the returned list gains `$mode`.

**Semantics:** `mode == "full"` → call `pelsa_wipe_species_cache(species_dir)` right before fetch (after a pre-fetch cancel check, so a cancel never wipes), and merge against `existing = NULL` (fresh fully supersedes). `mode == "incremental"` → no wipe, merge against the passed `existing`. The result list carries `mode = mode`.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-pelsa-refresh.R` after the `pelsa_refresh_species_cache` block (after line ~367):

```r
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
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: FAIL — `res$mode` is NULL (unused arg `mode`), and the full-mode test finds P1/P2/P3 still present (no wipe).

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_refresh_helpers.R`, edit `pelsa_refresh_species_cache`. Change the signature to add `mode`:

```r
pelsa_refresh_species_cache <- function(species, universe, species_dir,
                                        fetch_fn = pelsa_fetch_uniprot,
                                        existing = NULL,
                                        progress = NULL,
                                        should_cancel = NULL,
                                        mode = "incremental") {
```

Add a `mode` validation right after the existing `fetch_fn` check (after the `if (!is.function(fetch_fn))` stop, ~line 461):

```r
  mode <- match.arg(mode, c("incremental", "full"))
  # FULL mode rebuilds from scratch: the wipe (below) clears the prior cache, so
  # there is nothing to merge against -- force existing = NULL so the fresh frame
  # fully supersedes (and n_retained_from_cache is 0).
  if (identical(mode, "full")) existing <- NULL
```

Update `.canceled_result` so the canceled list also carries the mode (find the `.canceled_result <- function(reason)` block, ~line 469, and add `mode = mode,` to its returned list, e.g. after `n_retained_from_cache = 0L,`):

```r
  .canceled_result <- function(reason) {
    # No write on cancel: the prior cache is left fully intact.
    list(features = existing, unresolved = universe, path = NA_character_,
         n_features = if (is.data.frame(existing)) nrow(existing) else 0L,
         n_unresolved = length(universe), n_accessions = length(universe),
         n_retained_from_cache = 0L, mode = mode, canceled = TRUE)
  }
```

Add the wipe AFTER the pre-fetch cancel check and BEFORE the `.progress(0.05, ...)` fetch-start line (the cancel check is `if (is.function(should_cancel) && isTRUE(should_cancel())) return(.canceled_result("pre-fetch"))`, ~line 484). Insert directly after that `if` block:

```r
  # FULL mode: clean slate BEFORE any network -- delete the prior feature +
  # membrane caches (sparing fasta/). Done only after the pre-fetch cancel check
  # above, so a cancel never wipes. A subsequent fetch failure leaves the species
  # fasta-only (the user re-runs Full refresh); this is the documented, accepted
  # trade-off for a true clean rebuild.
  if (identical(mode, "full")) {
    pelsa_wipe_species_cache(species_dir)
  }
```

Finally, add `mode = mode` to the SUCCESS return list (the final `list(features = merged, ...)` at the end of the function, ~line 527) — insert it alongside the other fields, e.g. after `canceled = FALSE`:

```r
  list(
    features               = merged,
    unresolved             = unresolved,
    transient_unresolved   = transient_unresolved,
    path                   = path,
    n_features             = nrow(merged),
    n_unresolved           = length(unresolved),
    n_transient_unresolved = length(transient_unresolved),
    n_accessions           = length(universe),
    n_retained_from_cache  = n_retained,
    mode                   = mode,
    canceled               = FALSE
  )
```

Also update the mid-fetch cancel return (the `if (isTRUE(fetched$canceled)) return(.canceled_result("mid-fetch"))` — already routed through `.canceled_result`, which now carries `mode`; no extra edit needed there).

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: the 3 new mode tests PASS; the existing `pelsa_refresh_species_cache` tests (which omit `mode`, defaulting to `"incremental"`) still PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_refresh_helpers.R tests/testthat/test-pelsa-refresh.R
git commit -m "feat(pelsa): thread mode through pelsa_refresh_species_cache (full wipes + supersedes, incremental appends)"
```

---

### Task 4: Thread `mode` through `pelsa_run_species_refresh` (universe selection + per-species mode)

**Files:**
- Modify: `R/tab_pelsa_refresh_helpers.R` (`pelsa_run_species_refresh`, lines ~597-658)
- Test: `tests/testthat/test-pelsa-refresh.R` (add after the existing `pelsa_run_species_refresh` block, ~line 423)

**Interfaces:**
- Consumes: `pelsa_full_universe` / `pelsa_incremental_universe` (Task 1), `pelsa_species_refresh_inputs` (existing), `pelsa_refresh_species_cache(..., mode)` (Task 3).
- Produces: `pelsa_run_species_refresh(..., mode = "incremental")`; each per-species result carries `$mode`.

**Semantics:** Pick the universe function by `mode`: full → `pelsa_full_universe`, incremental → `pelsa_incremental_universe`. Pass `mode` into `pelsa_refresh_species_cache`. Each result record gains `mode = mode`.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-pelsa-refresh.R` after the `run_species_refresh captures a per-species error` test (~line 423):

```r
# ---- pelsa_run_species_refresh: mode routing --------------------------------

test_that("run_species_refresh full mode fetches the FASTA universe + wipes", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "10090")
  dir.create(species_dir)
  dir.create(file.path(species_dir, "fasta"))
  # FASTA with two UniProt-style accessions (pipe header parsed in uniprot mode).
  writeLines(c(">sp|P00001|A_X test", "MKV",
               ">sp|P00002|B_X test", "AAA"),
             file.path(species_dir, "fasta", "p.fasta"))
  # A pre-existing dataset accession that full mode must IGNORE.
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P99999",
                              stringsAsFactors = FALSE))

  seen <- NULL
  fake_fetch <- function(accessions, ...) { seen <<- accessions
    list(features = .fake_feature_df(), unresolved = character(0)) }

  results <- pelsa_run_species_refresh(
    species = "10090", database_dir = db, uploaded_gcts = gcts,
    fetch_fn = fake_fetch, mode = "full")

  expect_identical(results[[1]]$mode, "full")
  # Full universe = FASTA accessions only; the dataset accession is NOT fetched.
  expect_setequal(seen, c("P00001", "P00002"))
  expect_false("P99999" %in% seen)
})

test_that("run_species_refresh incremental mode fetches (dataset U fasta) - cache", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)
  dir.create(file.path(species_dir, "fasta"))
  writeLines(c(">sp|P00001|A_X t", "MKV"),
             file.path(species_dir, "fasta", "p.fasta"))
  # Seed a cache that already covers P00001 -> incremental must skip it.
  pelsa_write_feature_cache(
    data.frame(accession = "P00001", feature_type = "domain", start = 1L,
               end = 5L, description = "d", feature_class = "folded_domain",
               class_score = 2L, coord_quality = "exact",
               stringsAsFactors = FALSE),
    species_dir)
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P77777",
                              stringsAsFactors = FALSE))

  seen <- NULL
  fake_fetch <- function(accessions, ...) { seen <<- accessions
    list(features = data.frame(
      accession = "P77777", feature_type = "domain", start = 1L, end = 5L,
      description = "d", feature_class = "folded_domain", class_score = 2L,
      coord_quality = "exact", stringsAsFactors = FALSE),
      unresolved = character(0)) }

  results <- pelsa_run_species_refresh(
    species = "9606", database_dir = db, uploaded_gcts = gcts,
    fetch_fn = fake_fetch, mode = "incremental")

  expect_identical(results[[1]]$mode, "incremental")
  # union {P00001 (fasta), P77777 (dataset)} minus cache {P00001} = {P77777}.
  expect_setequal(seen, "P77777")
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: FAIL — `results[[1]]$mode` is NULL and `seen` includes the wrong accessions (the old `pelsa_refresh_accession_universe` no longer exists, so the un-migrated call site errors).

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_refresh_helpers.R`, edit `pelsa_run_species_refresh`. Add `mode` to the signature:

```r
pelsa_run_species_refresh <- function(species, database_dir, uploaded_gcts,
                                      fetch_fn = pelsa_fetch_uniprot,
                                      set_progress = NULL,
                                      should_cancel = NULL,
                                      mode = "incremental") {
```

Add a `match.arg` after the `species` validation (after the `if (!is.character(species) || length(species) == 0L) stop(...)` block, ~line 604):

```r
  mode <- match.arg(mode, c("incremental", "full"))
```

Inside the per-species `tryCatch({ ... })`, replace the universe call (currently `universe <- pelsa_refresh_accession_universe(uploaded_gcts, io$existing, fasta_map = io$fasta_map)`, ~line 625) with the mode-routed selection:

```r
      universe <- if (identical(mode, "full")) {
        pelsa_full_universe(uploaded_gcts, io$existing,
                            fasta_map = io$fasta_map)
      } else {
        pelsa_incremental_universe(uploaded_gcts, io$existing,
                                   fasta_map = io$fasta_map)
      }
```

Pass `mode` into the `pelsa_refresh_species_cache` call (find `res <- pelsa_refresh_species_cache(species = sp, universe = universe, ...)`, ~line 639) — add `mode = mode`:

```r
      res <- pelsa_refresh_species_cache(
        species = sp, universe = universe, species_dir = species_dir,
        fetch_fn = fetch_fn, existing = io$existing, progress = sub_progress,
        should_cancel = should_cancel, mode = mode
      )
```

Add `mode = mode` to the success result record (the `list(species = sp, n_features = res$n_features, ...)`, ~line 644) — insert after `had_existing = had_existing`:

```r
      list(species = sp, n_features = res$n_features,
           n_unresolved = res$n_unresolved,
           n_transient_unresolved = res$n_transient_unresolved,
           n_accessions = res$n_accessions,
           n_retained_from_cache = res$n_retained_from_cache,
           had_existing = had_existing, mode = mode, path = res$path,
           canceled = isTRUE(res$canceled), error = NULL)
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: the 2 new mode-routing tests PASS. The existing `run_species_refresh` tests default `mode = "incremental"`; the `universe_size sums per-species universes (datasets union cache)` test (line ~508) will FAIL — it is migrated in Task 5.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_refresh_helpers.R tests/testthat/test-pelsa-refresh.R
git commit -m "feat(pelsa): route pelsa_run_species_refresh universe by mode (full=fasta, incremental=gap-fill)"
```

---

### Task 5: Make `pelsa_refresh_universe_size` mode-aware

**Files:**
- Modify: `R/tab_pelsa_refresh_helpers.R` (`pelsa_refresh_universe_size`, lines ~755-766)
- Test: `tests/testthat/test-pelsa-refresh.R` (replace the `universe_size sums per-species universes` test, ~lines 508-520)

**Interfaces:**
- Consumes: `pelsa_full_universe` / `pelsa_incremental_universe` (Task 1), `pelsa_species_refresh_inputs` (existing).
- Produces: `pelsa_refresh_universe_size(species, database_dir, uploaded_gcts, mode = "incremental")` returning the true to-be-fetched count for that mode.

- [ ] **Step 1: Write the failing tests**

Replace the `universe_size sums per-species universes (datasets union cache)` test (lines ~508-520) in `tests/testthat/test-pelsa-refresh.R` with:

```r
test_that("universe_size full mode counts the FASTA proteome (per species)", {
  db <- withr::local_tempdir()
  sd <- file.path(db, "9606"); dir.create(sd)
  dir.create(file.path(sd, "fasta"))
  writeLines(c(">sp|P00001|A t", "MKV", ">sp|P00002|B t", "AAA",
               ">sp|P00003|C t", "CCC"),
             file.path(sd, "fasta", "p.fasta"))
  # Dataset accessions present but full mode ignores them.
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P99999",
                              stringsAsFactors = FALSE))
  sz <- pelsa_refresh_universe_size("9606", db, gcts, mode = "full")
  expect_equal(unname(sz$per_species[["9606"]]), 3L)  # 3 FASTA accessions
  expect_equal(sz$total, 3L)
})

test_that("universe_size incremental mode counts (dataset U fasta) - cache", {
  db <- withr::local_tempdir()
  sd <- file.path(db, "9606"); dir.create(sd)
  dir.create(file.path(sd, "fasta"))
  writeLines(c(">sp|P00001|A t", "MKV"), file.path(sd, "fasta", "p.fasta"))
  pelsa_write_feature_cache(
    data.frame(accession = "P00001", feature_type = "domain", start = 1L,
               end = 5L, description = "d", feature_class = "folded_domain",
               class_score = 2L, coord_quality = "exact",
               stringsAsFactors = FALSE), sd)
  gcts <- list(d = data.frame(PG.ProteinAccessions = "P77777",
                              stringsAsFactors = FALSE))
  sz <- pelsa_refresh_universe_size("9606", db, gcts, mode = "incremental")
  # union {P00001, P77777} minus cache {P00001} = 1.
  expect_equal(unname(sz$per_species[["9606"]]), 1L)
  expect_equal(sz$total, 1L)
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: FAIL — `pelsa_refresh_universe_size` ignores `mode` (unused arg) and still calls the removed `pelsa_refresh_accession_universe` (errors).

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_refresh_helpers.R`, replace `pelsa_refresh_universe_size` (lines ~755-766) with:

```r
pelsa_refresh_universe_size <- function(species, database_dir, uploaded_gcts,
                                        mode = "incremental") {
  mode <- match.arg(mode, c("incremental", "full"))
  per <- vapply(species, function(sp) {
    species_dir <- file.path(database_dir, sp)
    io <- pelsa_species_refresh_inputs(species_dir, uploaded_gcts)
    universe <- if (identical(mode, "full")) {
      pelsa_full_universe(uploaded_gcts, io$existing, fasta_map = io$fasta_map)
    } else {
      pelsa_incremental_universe(uploaded_gcts, io$existing,
                                 fasta_map = io$fasta_map)
    }
    length(universe)
  }, integer(1))
  names(per) <- species
  list(total = sum(per), per_species = per)
}
```

Also update the function's roxygen `@param`/header comment to mention `mode` (the block above the function, ~lines 742-754): add a line `# @param mode "full" (FASTA proteome) or "incremental" ((dataset U fasta) - cache).` before `# @return`.

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: the 2 new size tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_refresh_helpers.R tests/testthat/test-pelsa-refresh.R
git commit -m "feat(pelsa): make pelsa_refresh_universe_size mode-aware (confirm count matches fetched set)"
```

---

### Task 6: Mode-aware wording in notifications + inline result UI

**Files:**
- Modify: `R/tab_pelsa_refresh_helpers.R` (`pelsa_refresh_notifications` lines ~677-740; `pelsa_refresh_result_ui` lines ~837-882)
- Test: `tests/testthat/test-pelsa-refresh.R` (add after the existing notification/result tests)

**Interfaces:**
- Consumes: per-species result records that now carry `$mode` (Tasks 3-4).
- Produces: full-mode summaries say "rebuilt" + a "(previous feature + membrane files cleared)" note and omit the retained count; incremental-mode summaries say "topped up" + keep the retained count. Transient/absent unresolved logic unchanged.

**Semantics:** Only the success-summary phrasing per species branches on `mode`. Errors, canceled, and the unresolved warning/neutral-note logic stay identical for both modes.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-pelsa-refresh.R` after the `notifications report a canceled species` test (~line 640):

```r
# ---- notifications + result UI: mode-aware wording --------------------------

test_that("notifications: full mode says 'rebuilt' and notes the wipe", {
  results <- list(
    list(species = "10090", n_features = 500L, n_unresolved = 0L,
         n_retained_from_cache = 0L, had_existing = TRUE, mode = "full",
         canceled = FALSE, error = NULL))
  msgs <- vapply(pelsa_refresh_notifications(results),
                 function(n) n$message, character(1))
  summary <- msgs[grepl("rebuilt", msgs, ignore.case = TRUE)]
  expect_length(summary, 1L)
  expect_match(summary, "cleared")  # wipe note
})

test_that("notifications: incremental mode says 'topped up' + retained count", {
  results <- list(
    list(species = "9606", n_features = 120L, n_unresolved = 0L,
         n_retained_from_cache = 100L, had_existing = TRUE,
         mode = "incremental", canceled = FALSE, error = NULL))
  msgs <- vapply(pelsa_refresh_notifications(results),
                 function(n) n$message, character(1))
  summary <- msgs[grepl("topped up", msgs, ignore.case = TRUE)]
  expect_length(summary, 1L)
  expect_match(summary, "100")  # retained count surfaced
})

test_that("result_ui: full mode line says rebuilt, incremental says topped up", {
  full <- list(list(species = "10090", n_features = 500L, n_unresolved = 0L,
                    n_retained_from_cache = 0L, had_existing = TRUE,
                    mode = "full", canceled = FALSE, error = NULL))
  incr <- list(list(species = "9606", n_features = 120L, n_unresolved = 0L,
                    n_retained_from_cache = 100L, had_existing = TRUE,
                    mode = "incremental", canceled = FALSE, error = NULL))
  expect_match(as.character(pelsa_refresh_result_ui(full)),
               "rebuilt", ignore.case = TRUE)
  expect_match(as.character(pelsa_refresh_result_ui(incr)),
               "topped up", ignore.case = TRUE)
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: FAIL — current wording says "refreshed"/"features, ... retained", not "rebuilt"/"topped up".

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_refresh_helpers.R`, in `pelsa_refresh_notifications`, replace the rolled-up success summary block (the `if (length(done) > 0L) { summaries <- vapply(...) ... }`, ~lines 732-738) with a mode-aware per-species line:

```r
  if (length(done) > 0L) {
    summaries <- vapply(done, function(r) {
      if (identical(r$mode, "full")) {
        sprintf(paste0("%s: rebuilt - %d features, %d unresolved (previous ",
                       "feature + membrane files cleared)"),
                r$species, r$n_features, r$n_unresolved)
      } else {
        sprintf("%s: topped up - %d features, %d unresolved, %d retained",
                r$species, r$n_features, r$n_unresolved,
                r$n_retained_from_cache)
      }
    }, character(1))
    add(paste0("UniProt annotation refresh complete. ",
               paste(summaries, collapse = "; ")), "message", 10)
  }
```

In `pelsa_refresh_result_ui`, replace the per-`done` `<li>` construction (the `for (r in done) { items <- c(items, list(shiny::tags$li(sprintf("%s: %d features, %d unresolved, %d retained from cache", ...)))) }`, ~lines 867-872) with:

```r
  for (r in done) {
    line <- if (identical(r$mode, "full")) {
      sprintf("%s: rebuilt - %d features, %d unresolved (cache cleared)",
              r$species, r$n_features %||% 0L, r$n_unresolved %||% 0L)
    } else {
      sprintf("%s: topped up - %d features, %d unresolved, %d retained from cache",
              r$species, r$n_features %||% 0L, r$n_unresolved %||% 0L,
              r$n_retained_from_cache %||% 0L)
    }
    items <- c(items, list(shiny::tags$li(line)))
  }
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: the 3 new wording tests PASS; the existing notification/result tests (which now include `mode` in their fixtures? — NO, the OLD fixtures omit `mode`) — check: the pre-existing tests at lines 427-491, 604-640 build result records WITHOUT `mode`. `identical(r$mode, "full")` on a NULL `$mode` is `FALSE`, so they fall to the incremental ("topped up") branch and still match their assertions (they assert on "retained" / colors / "9606" / "rat", not on the verb). Confirm all green.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_refresh_helpers.R tests/testthat/test-pelsa-refresh.R
git commit -m "feat(pelsa): mode-aware refresh summaries (full=rebuilt+wipe note, incremental=topped up+retained)"
```

---

### Task 7: Full-mode round-trip equivalence test (wipe -> write -> read schema integrity)

**Files:**
- Test: `tests/testthat/test-pelsa-refresh.R` (add a new block near the end)

**Interfaces:**
- Consumes: `pelsa_run_species_refresh(mode = "full")`, `pelsa_read_feature_cache`, `.fake_feature_df` (existing fixture).

**Semantics:** A full refresh through the orchestrator (stub fetcher returning a canned 8-col frame) wipes, writes, and the written cache round-trips byte-faithfully on the schema columns — guarding the wipe→write→read path didn't corrupt the schema. No live network.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-pelsa-refresh.R` near the end of the file:

```r
# ---- full-mode round-trip equivalence (no network) --------------------------

test_that("full refresh round-trips the fetched frame through wipe/write/read", {
  db <- withr::local_tempdir()
  sd <- file.path(db, "10090"); dir.create(sd)
  dir.create(file.path(sd, "fasta"))
  writeLines(c(">sp|P00001|A t", "MKV", ">sp|P00002|B t", "AAA"),
             file.path(sd, "fasta", "p.fasta"))
  # Pre-seed a stale cache the wipe must clear.
  pelsa_write_feature_cache(.existing_cache_df(), sd)

  canned <- .fake_feature_df()  # P00001(x2), P00002
  fake_fetch <- function(accessions, ...) list(features = canned,
                                               unresolved = character(0))

  results <- pelsa_run_species_refresh(
    species = "10090", database_dir = db, uploaded_gcts = NULL,
    fetch_fn = fake_fetch, mode = "full")
  expect_null(results[[1]]$error)

  back <- pelsa_read_feature_cache(sd)
  # Stale P1/P2/P3 gone; only the canned fetch frame remains, value-faithful.
  expect_identical(back$accession, canned$accession)
  expect_identical(back$start, canned$start)
  expect_identical(back$end, canned$end)
  expect_identical(back$feature_class, canned$feature_class)
  expect_identical(back$class_score, canned$class_score)
  expect_identical(back$coord_quality, canned$coord_quality)
})
```

- [ ] **Step 2: Run test to verify it fails (or passes — confirm intent)**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: PASS if Tasks 1-4 are correct (this is a regression guard, not new behavior). If it FAILS, the wipe/write/read path has a schema bug introduced in Tasks 2-4 — fix there, not here.

- [ ] **Step 3: (No implementation — regression guard only.)**

If Step 2 passed, proceed. If it failed, return to Task 3/4 and fix the orchestration until this passes.

- [ ] **Step 4: Run the WHOLE refresh test file**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: ALL tests in the file PASS (universe, wipe, mode orchestration, size, notifications, UI, round-trip, plus all unchanged pre-existing tests).

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/test-pelsa-refresh.R
git commit -m "test(pelsa): full-refresh wipe/write/read round-trip equivalence guard"
```

---

### Task 8: UI — relabel the full button + add the incremental button

**Files:**
- Modify: `R/tab_pelsa_section1_helpers.R` (`maint_section` in `pelsa_setup_box_ui`, lines ~954-983)
- Test: `tests/testthat/test-pelsa-refresh.R` (extend the "Setup UI exposes the refresh ... button ids" test, lines ~495-504)

**Interfaces:**
- Produces: rendered markup containing `ns("pelsa_refresh_btn")` (label "Full library refresh") and `ns("pelsa_incremental_btn")` (label "Incremental refresh").

- [ ] **Step 1: Write the failing test**

Replace the `Setup UI exposes the refresh species checklist + button ids` test (lines ~495-504) in `tests/testthat/test-pelsa-refresh.R` with:

```r
test_that("Setup UI exposes both refresh-mode buttons + pure helpers exist", {
  ns <- shiny::NS("PELSASection1Tab")
  html <- as.character(
    pelsa_setup_box_ui(species = c("Human" = "9606"), compounds = character(0),
                       ns = ns, refresh_species = c("Human" = "9606")))
  expect_match(html, ns("pelsa_refresh_btn"), fixed = TRUE)
  expect_match(html, "Full library refresh", fixed = TRUE)
  expect_match(html, ns("pelsa_incremental_btn"), fixed = TRUE)
  expect_match(html, "Incremental refresh", fixed = TRUE)

  expect_true(exists("pelsa_full_universe"))
  expect_true(exists("pelsa_incremental_universe"))
  expect_true(exists("pelsa_wipe_species_cache"))
  expect_true(exists("pelsa_write_feature_cache"))
  expect_true(exists("pelsa_refresh_species_cache"))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: FAIL — markup has no `pelsa_incremental_btn` and the label is the old "Refresh per-species UniProt annotation library".

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_section1_helpers.R`, replace the `maint_section` body's helpText + single button (lines ~957-983) so the helpText distinguishes the modes and there are two buttons. Replace the `shiny::helpText(...)` block and the single `shiny::actionButton(ns("pelsa_refresh_btn"), ...)` with:

```r
    shiny::helpText(
      "Rebuild the per-species feature cache used for volcano feature ",
      "annotation. ",
      shiny::tags$b("Full library refresh"),
      " clears the species' existing feature + membrane files and re-fetches ",
      "the entire FASTA proteome (several minutes). ",
      shiny::tags$b("Incremental refresh"),
      " adds only accessions from your uploaded data and FASTA that are not ",
      "already cached (requires an existing library). Both are independent of ",
      "Start Analysis."
    ),
    # SINGLE-select (radioButtons, not checkboxGroupInput): a refresh fetches the
    # uploaded datasets' accessions, so allowing multiple species would fan ONE
    # dataset's accessions into every checked species' cache (the human-into-mouse
    # spillover). Restricting to one species at a time makes that impossible.
    shiny::radioButtons(
      ns("pelsa_refresh_species"),
      label    = "Species to refresh",
      choices  = refresh_species,
      selected = character(0)
    ),
    # Two modes on the ONE selected species. Full = destructive proteome rebuild
    # (wipe then fetch FASTA). Incremental = non-destructive top-up (fetch only
    # the cache-miss accessions, append atop). The incremental button is enabled
    # by the observer ONLY when a populated feature cache already exists for the
    # selected species (see tab_pelsa_section1.R).
    shiny::div(
      class = "pelsa-refresh-buttons",
      shiny::actionButton(
        ns("pelsa_refresh_btn"),
        "Full library refresh",
        icon  = shiny::icon("rotate"),
        class = "pelsa-refresh-btn"
      ),
      shiny::actionButton(
        ns("pelsa_incremental_btn"),
        "Incremental refresh",
        icon  = shiny::icon("circle-plus"),
        class = "pelsa-refresh-btn pelsa-incremental-btn"
      )
    ),
```

(Leave the `shiny::uiOutput(ns("pelsa_refresh_status"))` line that follows unchanged.)

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: the UI test PASSES.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section1_helpers.R tests/testthat/test-pelsa-refresh.R
git commit -m "feat(pelsa): add Incremental refresh button + relabel Full library refresh in Setup UI"
```

---

### Task 9: Observer — two buttons, shared run, in-flight lock, incremental disable-guard, unconditional confirm

**Files:**
- Modify: `R/tab_pelsa_section1.R` (the refresh observer block, lines ~1003-1089)

**Interfaces:**
- Consumes: `pelsa_refresh_universe_size(..., mode)` (Task 5), `pelsa_run_species_refresh(..., mode)` (Task 4), `pelsa_gcts_for_species` (existing), `pelsa_read_feature_cache` (existing), `pelsa_refresh_eta_text` (existing), `setup_state$species`, `GCTs_and_params()`, `pelsa_database_dir()`.
- Produces: observer behavior only (no return value). Wires `input$pelsa_refresh_btn` (mode "full") + `input$pelsa_incremental_btn` (mode "incremental"); both confirm unconditionally; in-flight locks both buttons; an `observe()` disables `pelsa_incremental_btn` unless a species is selected AND its cache has >=1 row AND no fetch is in flight.

This task is UI/reactive wiring; it is verified manually (live app) + by the offline helper coverage. No new automated test (per the agreed test plan).

- [ ] **Step 1: Replace the `run_refresh` helper to take a `mode`**

In `R/tab_pelsa_section1.R`, replace the `run_refresh <- function(selected, uploaded_gcts) { ... }` block (lines ~1016-1038) with:

```r
    # The actual run (shared by both modes' confirmed paths). Drives the live
    # progress modal, runs the orchestrator with the chosen mode, and stores the
    # results for the inline panel. `mode` is "full" or "incremental".
    run_refresh <- function(selected, uploaded_gcts, mode) {
      refresh_in_flight(TRUE)
      shinyjs::disable("pelsa_refresh_btn")
      shinyjs::disable("pelsa_incremental_btn")
      on.exit({
        shinyjs::enable("pelsa_refresh_btn")
        # The incremental guard observer re-applies the correct enabled/disabled
        # state (cache presence) once in-flight clears; do not blanket-enable here.
        refresh_in_flight(FALSE)
      }, add = TRUE)

      results <- withProgress(
        message = "Refreshing UniProt annotation library", value = 0, {
          pelsa_run_species_refresh(
            species       = selected,
            database_dir  = pelsa_database_dir(),
            uploaded_gcts = uploaded_gcts,
            fetch_fn      = pelsa_fetch_uniprot,
            mode          = mode,
            set_progress  = function(value, detail) {
              setProgress(value = value, detail = detail)
            }
          )
        }
      )
      refresh_result(results)
    }
```

- [ ] **Step 2: Add a shared confirm-then-run launcher + the incremental disable-guard**

In `R/tab_pelsa_section1.R`, delete the existing single `observeEvent(input$pelsa_refresh_btn, { ... }, ignoreInit = TRUE)` block (lines ~1040-1089) and the now-unused `REFRESH_CONFIRM_THRESHOLD` line (~line 1007), and replace with the following (keep `refresh_in_flight`, `refresh_result`, and the `output$pelsa_refresh_status` renderUI above it):

```r
    # Resolve the selected species' uploaded GCTs (Defect #1 guard: only same-
    # species datasets) + return NULL-on-error size. Shared by both modes.
    refresh_gcts_for <- function(selected) {
      gp <- GCTs_and_params()
      uploaded_gcts <- if (is.null(gp)) NULL else gp$GCTs
      species_by_ds <- isolate(setup_state$species)
      pelsa_gcts_for_species(uploaded_gcts, species_by_ds, selected)
    }

    # TRUE iff the selected species has a feature cache with >= 1 row on disk.
    species_cache_has_rows <- function(selected) {
      if (is.null(selected) || length(selected) != 1L || !nzchar(selected)) {
        return(FALSE)
      }
      species_dir <- file.path(pelsa_database_dir(), selected)
      cache <- tryCatch(pelsa_read_feature_cache(species_dir),
                        error = function(e) NULL)
      is.data.frame(cache) && nrow(cache) > 0L
    }

    # Shared confirm-then-run for both modes. `mode` is "full" | "incremental".
    launch_refresh <- function(mode) {
      if (isTRUE(refresh_in_flight())) return()          # ignore overlapping clicks
      selected <- input$pelsa_refresh_species
      if (is.null(selected) || length(selected) == 0L) {
        showNotification("Select a species to refresh.", type = "warning",
                         duration = 4)
        return()
      }
      uploaded_gcts <- refresh_gcts_for(selected)
      database_dir  <- pelsa_database_dir()

      size <- tryCatch(
        pelsa_refresh_universe_size(selected, database_dir, uploaded_gcts,
                                    mode = mode),
        error = function(e) list(total = NA_integer_, per_species = integer(0)))
      eta <- if (is.na(size$total)) "an unknown number of accessions" else
        pelsa_refresh_eta_text(size$total)

      # BOTH modes confirm unconditionally (no size threshold). Full warns about
      # the destructive wipe; incremental about the append.
      text <- if (identical(mode, "full")) {
        sprintf(paste0("Full library refresh for <b>%s</b>.<br/><br/>This ",
                       "DELETES the existing UniProt feature and membrane files ",
                       "for this species and re-fetches the entire proteome ",
                       "(<b>%s</b>). It cannot be undone or stopped once ",
                       "started. Continue?"), selected, eta)
      } else {
        sprintf(paste0("Incremental refresh for <b>%s</b>.<br/><br/>This fetches ",
                       "only the <b>%s</b> not yet in the library and appends ",
                       "them to the existing cache. Continue?"), selected, eta)
      }
      shinyalert::shinyalert(
        title = if (identical(mode, "full")) "Rebuild the whole library?"
                else "Top up the library?",
        text = text, html = TRUE,
        type = if (identical(mode, "full")) "warning" else "info",
        showCancelButton = TRUE,
        confirmButtonText = if (identical(mode, "full")) "Delete & rebuild"
                            else "Fetch",
        cancelButtonText = "Cancel",
        callbackR = function(confirmed) {
          if (isTRUE(confirmed)) run_refresh(selected, uploaded_gcts, mode)
        }
      )
    }

    observeEvent(input$pelsa_refresh_btn, launch_refresh("full"),
                 ignoreInit = TRUE)
    observeEvent(input$pelsa_incremental_btn, launch_refresh("incremental"),
                 ignoreInit = TRUE)

    # Incremental disable-guard: enabled IFF a species is selected, its cache has
    # >= 1 row, and no fetch is in flight. Reactive on the selection, the in-flight
    # flag, AND the last result (so a just-finished Full refresh that populated the
    # cache flips Incremental on without re-selecting the species).
    observe({
      refresh_result()                                   # re-evaluate after a run
      in_flight <- isTRUE(refresh_in_flight())
      selected  <- input$pelsa_refresh_species
      enable_incremental <- !in_flight && species_cache_has_rows(selected)
      if (enable_incremental) shinyjs::enable("pelsa_incremental_btn")
      else shinyjs::disable("pelsa_incremental_btn")
    })
```

- [ ] **Step 3: Reload + smoke-check the package builds**

Run: `Rscript -e 'devtools::load_all("."); cat("loaded OK\n")'`
Expected: `loaded OK` with no parse/error output. (Confirms the observer edits are syntactically valid and reference real symbols.)

- [ ] **Step 4: Run the full PELSA refresh test file (no regressions)**

Run: `Rscript -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")'`
Expected: ALL PASS (observer changes don't touch the offline helpers).

- [ ] **Step 5: Manual verification checklist (live app)**

Run `Protigy::launchApp()`, upload a PELSA dataset, go to Setup:
- [ ] With NO species selected: both buttons present; Incremental disabled.
- [ ] Select a species whose cache is EMPTY/absent (e.g. a fresh species with only a FASTA): Incremental stays disabled; Full enabled.
- [ ] Click Full → confirm dialog warns about deletion + shows ETA → confirm → progress bar runs → inline panel says "rebuilt ... (cache cleared)" → Incremental becomes ENABLED automatically (no re-select).
- [ ] Click Incremental → confirm dialog says "top up" → confirm → fetches only cache-miss accessions → inline panel says "topped up ... retained".
- [ ] During any fetch: both buttons disabled; a second click is ignored.
- [ ] Verify on disk: after Full, `inst/database/<species>/uniprot_membrane/` is gone and `fasta/` remains.

- [ ] **Step 6: Commit**

```bash
git add R/tab_pelsa_section1.R
git commit -m "feat(pelsa): wire Full + Incremental refresh observers (unconditional confirm, in-flight lock, incremental cache-presence guard)"
```

---

### Task 10: Full suite + check sweep

**Files:** none (verification only).

- [ ] **Step 1: Reload and run the entire testthat suite**

Run: `Rscript -e 'devtools::load_all("."); devtools::test()'`
Expected: 0 failures. Pay attention to `test-pelsa-refresh.R` and any test that referenced `pelsa_refresh_accession_universe` (there should be none left — grep below).

- [ ] **Step 2: Confirm no dangling references to the removed function**

Run: `grep -rn "pelsa_refresh_accession_universe" R/ tests/`
Expected: NO output (the function and all its call sites are gone).

- [ ] **Step 3: ASCII-only check on edited R files**

Run: `LC_ALL=C grep -rnP "[^\x00-\x7F]" R/tab_pelsa_refresh_helpers.R R/tab_pelsa_section1_helpers.R R/tab_pelsa_section1.R`
Expected: NO output (no non-ASCII bytes introduced).

- [ ] **Step 4: Commit any final cleanup (if grep found issues, fix then commit)**

```bash
git add -A
git commit -m "chore(pelsa): final cleanup after full/incremental refresh split" --allow-empty
```

---

## Self-Review

**1. Spec coverage:**
- Rename current button → "Full library refresh": Task 8. ✓
- New "Incremental refresh" button, fetches only accessions not in cache: Tasks 8 (UI) + 9 (observer) + 1/4 (universe). ✓
- Incremental steps: read cache + FASTA + explode dataset accessions of selected species → `(dataset ∪ fasta) − cache` → fetch → append atop: `pelsa_incremental_universe` (Task 1) + merge-atop via `pelsa_refresh_species_cache` incremental mode (Task 3) + `pelsa_gcts_for_species` same-species filter in observer (Task 9). ✓
- "In cache" = ≥1 feature row: `.pelsa_cache_universe` reads `cache$accession` (Task 1); guard uses `nrow(cache) > 0L` (Task 9). ✓
- Incremental button greyed out when only FASTA (no feature files): disable-guard `species_cache_has_rows` requires ≥1 row (Task 9). ✓
- Re-enable incremental live after Full completes: guard `observe()` reacts on `refresh_result()` (Task 9). ✓
- Full deletes existing UniProt annotation files (clean slate, both features + membrane, spare fasta): `pelsa_wipe_species_cache` (Task 2), called pre-fetch in full mode (Task 3). ✓
- Full fetches FASTA only, ignores dataset accessions: `pelsa_full_universe` (Task 1), routed in run/size (Tasks 4-5). ✓
- Delete before fetch (accepted trade-off): wipe after cancel-check, before fetch (Task 3); failure → fasta-only, surfaced honestly (Task 6 wording + Task 9 error notes via existing `pelsa_refresh_notifications` error path). ✓
- Both confirm unconditionally: Task 9 (`launch_refresh` always shows shinyalert, threshold removed). ✓

**2. Placeholder scan:** No "TBD"/"handle edge cases"/"similar to Task N"/bare prose-only code steps. Every code step shows full code. ✓

**3. Type consistency:**
- `mode` is the literal `"full"`/`"incremental"` everywhere (`match.arg(mode, c("incremental", "full"))` in Tasks 3-5; passed as `"full"`/`"incremental"` strings in Tasks 4 and 9). ✓
- `pelsa_full_universe` / `pelsa_incremental_universe` / `pelsa_wipe_species_cache` signatures identical across definition (Tasks 1-2) and call sites (Tasks 4-5, 9). ✓
- Result records carry `$mode`, consumed in Task 6 wording and Task 9 (not consumed there, only produced). ✓
- Button ids `pelsa_refresh_btn` / `pelsa_incremental_btn` consistent between UI (Task 8) and observer (Task 9). ✓
- `species_cache_has_rows` / `refresh_gcts_for` / `launch_refresh` / `run_refresh` defined once (Task 9) and referenced after definition. ✓

No gaps found.

## Risks

| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| Pre-existing notification/result tests lack `$mode` and hit the wrong wording branch | Low | Low | `identical(NULL, "full")` is FALSE → incremental branch; old assertions check "retained"/colors/species names, not the verb (Task 6 Step 4 note). |
| Full fetch fails AFTER wipe → species left fasta-only | Medium | Medium (user-accepted) | Honest error notification via existing error path; inline panel + confirm dialog warn it cannot be undone (Tasks 6, 9). |
| `withr`/FASTA pipe-header parse mismatch in tests | Low | Low | Tests use `>sp|P00001|...` headers parsed by `pelsa_read_fasta(mode="uniprot")` via `pelsa_species_refresh_inputs`; matches the real path. |
| `shinyjs::enable/disable` race with the guard observer right after a run | Low | Low | `run_refresh` `on.exit` does NOT blanket-enable incremental; the guard observer (reactive on `refresh_result()` + `refresh_in_flight()`) is the single authority for incremental state (Task 9). |

## Notes

- No `@export`/`NAMESPACE` changes (all helpers `@noRd`), so `devtools::document()` is not required for this work — but run it if any roxygen `@import` is touched (none planned).
- The membrane file (`uniprot_membrane/`) is gitignored, app-unreadable, and not regenerated by this pipeline; its deletion in full mode is an explicit, user-confirmed decision (see Global Constraints).
- `pelsa_refresh_progress_ui` and the cancel propagation are untouched; both modes reuse the existing progress/cancel/atomic-write machinery.
