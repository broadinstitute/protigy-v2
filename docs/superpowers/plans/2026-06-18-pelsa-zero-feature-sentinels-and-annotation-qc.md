# PELSA Zero-Feature Sentinels + Three-Way Annotation QC Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Persist UniProt accessions that resolve with **zero features** as sentinel rows in the feature cache so incremental refresh stops re-fetching them, surface a three-way fetch breakdown (with-features / zero-features / unresolved) in the refresh panel, and split the Summary QC "Proteins failed annotation" metric into three granular counts (>=1 annotation / 0 annotations / failed annotation).

**Architecture:** `pelsa_fetch_uniprot` already separates resolved (entry returned) from unresolved (no entry). Today a resolved-but-0-feature accession contributes no cache row, so it's invisible and re-fetched every incremental run. We make it first-class: the fetch returns a new `zero_feature` vector; the orchestrator appends a **sentinel row** per zero-feature accession (`feature_type=""`, `start/end=NA`, `feature_class="none"`, `class_score=0`) to the cache. The single annotation choke point `pelsa_annotate_features` silently drops sentinel rows before the overlap join (every volcano/Woods consumer flows through it). The Summary tab's `pelsa_unannotated_accessions` is complemented by a new `pelsa_annotation_status_counts` that buckets a dataset's accessions into the three categories using the now-sentinel-aware cache.

**Tech Stack:** R / Shiny module, `testthat` (offline; injected `fetch_fn` stub — NO live network), `data.table`/`readr` (cache I/O), `shinydashboard` (value boxes).

## Global Constraints

- **ASCII-only R source.** No literal Unicode in `R/`; `\uXXXX` escapes only. (CLAUDE.md — enforced; breaks `R CMD check`.)
- **Reload before testing:** `devtools::load_all(".")` after any `R/` edit. All helpers here stay `@noRd` (no NAMESPACE/`document()` change).
- **No live network in tests.** `pelsa_fetch_uniprot` never called in tests; inject a stub `fetch_fn`.
- **`%||%`** is rlang (imported in `R/protigy-package.R`), not base.
- **Sentinel row shape (EXACT, single source of truth):** `accession=<acc>`, `feature_type=""`, `start=NA_integer_`, `end=NA_integer_`, `description=""`, `feature_class="none"`, `class_score=0L`, `coord_quality=""`.
- **`"none"`** is the existing `NONE_FEATURE_CLASS` (`R/tab_pelsa_annotation_helpers.R:61`) with a defined grey color — reuse it, do not invent a new label.
- **"resolved" = entry returned** (not feature presence). zero_feature ⊂ resolved; zero_feature ∩ unresolved = ∅.
- **Both refresh modes write sentinels** (full + incremental).
- **Backward compatibility:** a cache built before this change (or a self-curated species) has no sentinel rows; the Summary "0 annotations" bucket is then empty and such proteins fall into "failed annotation" until the cache is rebuilt. Accepted.
- Three Summary metrics replace the one "Proteins failed annotation" box: **>=1 annotation(s)**, **0 annotation**, **failed annotated**.

---

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `R/tab_pelsa_uniprot_fetch.R` | Fetch + parser. Add `zero_feature` to the return contract. | MODIFY — compute `zero_feature` = resolved accessions with no parsed feature rows; return it. |
| `R/tab_pelsa_refresh_helpers.R` | Orchestration. Append sentinel rows; thread zero-feature counts. | MODIFY — new `pelsa_zero_feature_rows()`; merge sentinels in `pelsa_refresh_species_cache`; carry `n_zero_feature` through results; mode-aware wording. |
| `R/tab_pelsa_annotation_helpers.R` | Annotation overlap + Summary QC. | MODIFY — drop sentinel rows silently in `pelsa_annotate_features`; add `pelsa_annotation_status_counts()`. |
| `R/tab_pelsa_analysis_helpers.R` | Per-dataset QC assembly. | MODIFY — store the 3-way counts in `entry$qc`. |
| `R/tab_pelsa_section2.R` | Summary value boxes. | MODIFY — replace 1 annotation box with 3; layout. |
| `tests/testthat/test-pelsa-uniprot-fetch-offline.R` | Fetch tests. | MODIFY — assert `zero_feature`. |
| `tests/testthat/test-pelsa-refresh.R` | Orchestration tests. | MODIFY — sentinel rows in cache; counts. |
| `tests/testthat/test-pelsa-annotation.R` (or the existing annotation test file) | Annotation tests. | MODIFY — sentinel drop; status counts. |

No new files.

---

## Interfaces (signatures other tasks rely on)

```r
# Fetch (new return field)
pelsa_fetch_uniprot(...) -> list(features=<8-col df>, unresolved=<chr>,
                                 zero_feature=<chr>,            # NEW
                                 transient_unresolved=<chr>, canceled=<lgl>)

# Sentinel rows (pure)
pelsa_zero_feature_rows(accessions) -> <8-col df>  # one sentinel row per accession

# Orchestration result records gain:
#   $n_zero_feature  (int)   per species

# Annotation QC (pure)
pelsa_annotation_status_counts(plot_df_or_accessions, feat_df)
  -> list(n_with_features=<int>, n_zero_feature=<int>, n_failed=<int>)

# pelsa_annotate_features(plot_df, feat_df)  -- unchanged signature; now drops
#   sentinel rows (feature_class=="none" with NA coords) BEFORE the corrupt-coord
#   warning, silently.
```

Unchanged + consumed as-is: `pelsa_empty_feature_frame`, `pelsa_merge_feature_cache`, `pelsa_write_feature_cache`, `pelsa_read_feature_cache`, `pelsa_unannotated_accessions`, `.pelsa_isoform_base`/`pelsa_isoform_base`, `NONE_FEATURE_CLASS`.

---

### Task 1: Fetch returns a `zero_feature` accession vector

**Files:**
- Modify: `R/tab_pelsa_uniprot_fetch.R` (`pelsa_fetch_uniprot` return block, lines ~548-578; the empty-input + empty-query early returns ~448-468)
- Test: `tests/testthat/test-pelsa-uniprot-fetch-offline.R`

**Interfaces:**
- Produces: `pelsa_fetch_uniprot(...)$zero_feature` — resolved accessions (entry returned) that yielded ZERO parsed feature rows. `zero_feature ⊂ resolved`, disjoint from `unresolved`.

**Semantics:** After parsing, `resolved` = input accessions whose entry came back. `zero_feature` = `resolved` minus the accessions that actually appear in `features$accession` (with isoform-base fallback, mirroring the resolved match). Empty-input / empty-query / cancel early-returns also carry `zero_feature = character(0)`.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-pelsa-uniprot-fetch-offline.R` (uses the existing `local_mocked_bindings(.pelsa_fetch_one_batch=...)` seam — match the file's existing pattern):

```r
test_that("fetch reports resolved-but-zero-feature accessions separately", {
  # Two accessions both RESOLVE (entries returned), but only P00001 has a
  # feature; P00002 comes back with an empty features list -> zero_feature.
  fake_batch <- function(base_req, accs, size) {
    list(entries = list(
      list(primaryAccession = "P00001",
           features = list(list(type = "Active site",
                                location = list(start = list(value = 10),
                                                end = list(value = 12))))),
      list(primaryAccession = "P00002", features = list())  # resolved, 0 features
    ), failed = FALSE)
  }
  testthat::local_mocked_bindings(.pelsa_fetch_one_batch = fake_batch,
                                  .package = "Protigy")

  res <- pelsa_fetch_uniprot(c("P00001", "P00002"),
                             base = "https://example.invalid")
  expect_setequal(res$zero_feature, "P00002")
  expect_false("P00002" %in% res$unresolved)   # resolved, not unresolved
  expect_true("P00001" %in% res$features$accession)
  expect_false("P00001" %in% res$zero_feature)  # has a feature
})

test_that("fetch zero_feature is empty on the empty-input fast path", {
  res <- pelsa_fetch_uniprot(character(0))
  expect_identical(res$zero_feature, character(0))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-uniprot-fetch-offline.R", reporter="summary")'`
Expected: FAIL — `res$zero_feature` is NULL (`expect_setequal(NULL, "P00002")` fails) and the empty-input test fails (`NULL` not `character(0)`).

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_uniprot_fetch.R`, the three early returns and the final return must all include `zero_feature`.

Empty-input fast path (~line 448):

```r
  if (length(accessions) == 0L) {
    return(list(features = pelsa_empty_feature_frame(),
                unresolved = character(0), zero_feature = character(0),
                transient_unresolved = character(0), canceled = FALSE))
  }
```

Empty-query path (~line 463):

```r
    return(list(features = pelsa_empty_feature_frame(),
                unresolved = accessions, zero_feature = character(0),
                transient_unresolved = character(0), canceled = FALSE))
```

Final return block (~lines 565-577) — compute `zero_feature` after `resolved`/`unresolved`:

```r
  entry_acc <- .pelsa_entry_accessions(entries)
  resolved <- accessions[
    accessions %in% entry_acc |
      pelsa_isoform_base(accessions) %in% entry_acc
  ]
  unresolved <- setdiff(accessions, resolved)
  transient_unresolved <- intersect(unresolved, failed_accessions)

  # zero_feature = RESOLVED accessions that produced no parsed feature row. An
  # entry can come back valid but featureless; it is NOT unresolved (UniProt
  # answered), it simply has nothing to annotate. We surface it so the caller can
  # cache a sentinel (stop re-fetching it) and report it as a distinct category.
  # Match feature presence with the same isoform-base fallback used for resolved.
  feat_acc <- if (nrow(features) > 0L) unique(as.character(features$accession))
              else character(0)
  has_feature <- resolved %in% feat_acc |
    pelsa_isoform_base(resolved) %in% feat_acc
  zero_feature <- unique(resolved[!has_feature])

  list(features = features, unresolved = unresolved,
       zero_feature = zero_feature,
       transient_unresolved = transient_unresolved, canceled = canceled)
```

If the mid-fetch cancel path has its own return, also add `zero_feature = character(0)` there (search for any other `return(list(features =` in the function and add the field; the cancel return is via the orchestrator, not here, so typically only the three above).

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-uniprot-fetch-offline.R", reporter="summary")'`
Expected: the 2 new tests PASS; existing fetch tests still PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_uniprot_fetch.R tests/testthat/test-pelsa-uniprot-fetch-offline.R
git commit -m "feat(pelsa): fetch returns resolved-but-zero-feature accessions as a distinct category"
```

---

### Task 2: `pelsa_zero_feature_rows` builds sentinel rows

**Files:**
- Modify: `R/tab_pelsa_refresh_helpers.R` (add near the top helper section, after the universe helpers)
- Test: `tests/testthat/test-pelsa-refresh.R`

**Interfaces:**
- Consumes: `pelsa_empty_feature_frame` (existing).
- Produces: `pelsa_zero_feature_rows(accessions) -> <8-col data.frame>` — one sentinel row per unique non-empty accession, in the canonical schema column order.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-pelsa-refresh.R` after the wipe tests:

```r
# ---- pelsa_zero_feature_rows (sentinel rows for 0-feature accessions) --------

test_that("zero_feature_rows builds one schema-shaped sentinel per accession", {
  out <- pelsa_zero_feature_rows(c("P00002", "P00003", "P00002"))  # dup dropped
  expect_setequal(out$accession, c("P00002", "P00003"))
  expect_identical(nrow(out), 2L)
  # Sentinel shape.
  expect_true(all(out$feature_type == ""))
  expect_true(all(is.na(out$start)))
  expect_true(all(is.na(out$end)))
  expect_true(all(out$feature_class == "none"))
  expect_true(all(out$class_score == 0L))
  # Canonical 8-col schema, correct order.
  expect_identical(colnames(out),
                   c("accession", "feature_type", "start", "end",
                     "description", "feature_class", "class_score",
                     "coord_quality"))
})

test_that("zero_feature_rows returns a 0-row schema frame for empty input", {
  out <- pelsa_zero_feature_rows(character(0))
  expect_identical(nrow(out), 0L)
  expect_identical(colnames(out), colnames(pelsa_empty_feature_frame()))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-refresh.R", reporter="summary")'`
Expected: FAIL — `could not find function "pelsa_zero_feature_rows"`.

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_refresh_helpers.R`, add after `pelsa_wipe_species_cache`:

```r
# ---- Helper 1c: zero-feature sentinel rows -----------------------------------

# Build SENTINEL feature rows for accessions UniProt resolved with ZERO features.
# A 0-feature accession has no natural feature row, so without a sentinel it
# leaves no trace in the cache and an incremental refresh re-fetches it forever.
# The sentinel marks "resolved, genuinely no features": it puts the accession in
# cache$accession (so incremental skips it) while carrying NA coordinates +
# feature_class "none" so the annotation overlap drops it silently (see
# pelsa_annotate_features) and the Summary QC counts it as "0 annotations"
# rather than "failed annotation".
#
# @param accessions character vector of resolved-but-0-feature accessions.
# @return 8-col feature data.frame (0 rows for empty input); one row per unique
#         non-empty accession, in canonical schema column order.
# @noRd
pelsa_zero_feature_rows <- function(accessions) {
  accs <- unique(as.character(accessions))
  accs <- accs[!is.na(accs) & nzchar(accs)]
  if (length(accs) == 0L) return(pelsa_empty_feature_frame())
  data.frame(
    accession     = accs,
    feature_type  = "",
    start         = NA_integer_,
    end           = NA_integer_,
    description   = "",
    feature_class = "none",
    class_score   = 0L,
    coord_quality = "",
    stringsAsFactors = FALSE
  )
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-refresh.R", reporter="summary")'`
Expected: the 2 sentinel-row tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_refresh_helpers.R tests/testthat/test-pelsa-refresh.R
git commit -m "feat(pelsa): add pelsa_zero_feature_rows (sentinel rows for resolved-0-feature accessions)"
```

---

### Task 3: Orchestrator appends sentinels + carries `n_zero_feature`

**Files:**
- Modify: `R/tab_pelsa_refresh_helpers.R` (`pelsa_refresh_species_cache` fresh-frame assembly ~lines 563-592; result list; `pelsa_run_species_refresh` result record)
- Test: `tests/testthat/test-pelsa-refresh.R`

**Interfaces:**
- Consumes: `pelsa_zero_feature_rows` (Task 2), `pelsa_fetch_uniprot`'s `zero_feature` (Task 1).
- Produces: cache now contains sentinel rows for zero-feature accessions (both modes); `pelsa_refresh_species_cache` result + each `pelsa_run_species_refresh` record gain `n_zero_feature` (int).

**Semantics:** After the fetch, build `fresh_with_sentinels = rbind(features, pelsa_zero_feature_rows(zero_feature))`. Use that as the frame fed to the merge (so sentinels persist in both full and incremental). `n_zero_feature = length(zero_feature)`. The sentinel accessions are NOT in `unresolved` (they're resolved), so the merge's retain logic is unaffected.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-pelsa-refresh.R` after the mode tests:

```r
# ---- sentinel persistence in the cache (both modes) -------------------------

test_that("refresh writes sentinel rows for zero-feature accessions", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)

  # Fetch: P00001 has a feature; P00002 resolved with zero features.
  fake_fetch <- function(accessions, ...) {
    list(features = data.frame(
           accession = "P00001", feature_type = "active site", start = 10L,
           end = 12L, description = "x", feature_class = "active_or_binding_site",
           class_score = 5L, coord_quality = "exact", stringsAsFactors = FALSE),
         unresolved = character(0), zero_feature = "P00002")
  }
  res <- pelsa_refresh_species_cache(
    species = "9606", universe = c("P00001", "P00002"),
    species_dir = species_dir, fetch_fn = fake_fetch, mode = "incremental")

  expect_identical(res$n_zero_feature, 1L)
  back <- pelsa_read_feature_cache(species_dir)
  # BOTH accessions are in the cache now (P00002 as a sentinel).
  expect_setequal(unique(back$accession), c("P00001", "P00002"))
  sentinel <- back[back$accession == "P00002", ]
  expect_true(is.na(sentinel$start))
  expect_identical(sentinel$feature_class, "none")
})

test_that("zero-feature accession is NOT re-fetched on the next incremental run", {
  db <- withr::local_tempdir()
  species_dir <- file.path(db, "9606")
  dir.create(species_dir)
  dir.create(file.path(species_dir, "fasta"))
  writeLines(c(">sp|P00001|A t", "MKV", ">sp|P00002|B t", "AAA"),
             file.path(species_dir, "fasta", "p.fasta"))

  # First incremental: P00001 feature, P00002 zero-feature.
  fetch1 <- function(accessions, ...) list(
    features = data.frame(accession = "P00001", feature_type = "domain",
      start = 1L, end = 5L, description = "d", feature_class = "folded_domain",
      class_score = 2L, coord_quality = "exact", stringsAsFactors = FALSE),
    unresolved = character(0), zero_feature = "P00002")
  pelsa_run_species_refresh("9606", db, uploaded_gcts = NULL,
                            fetch_fn = fetch1, mode = "incremental")

  # Second incremental: capture what gets fetched -- must be EMPTY (both cached:
  # P00001 as feature, P00002 as sentinel).
  seen <- NULL
  fetch2 <- function(accessions, ...) { seen <<- accessions
    list(features = pelsa_empty_feature_frame(), unresolved = character(0),
         zero_feature = character(0)) }
  expect_error(
    pelsa_run_species_refresh("9606", db, uploaded_gcts = NULL,
                              fetch_fn = fetch2, mode = "incremental"),
    "empty accession universe")
  expect_null(seen)  # fetch never called -- universe was empty
})
```

(Note: the second test asserts the empty-universe `stop()` — the existing contract when nothing needs fetching. If that is considered a poor UX it is handled in Task 7's notification wording, not here.)

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-refresh.R", reporter="summary")'`
Expected: FAIL — `res$n_zero_feature` NULL; P00002 absent from the cache; second test re-fetches P00002.

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_refresh_helpers.R` `pelsa_refresh_species_cache`, after extracting `fresh`/`unresolved` (~line 563) and before the merge, fold in sentinels:

```r
  fresh      <- fetched$features
  unresolved <- fetched$unresolved %||% character(0)
  transient_unresolved <- fetched$transient_unresolved %||% character(0)
  zero_feature <- fetched$zero_feature %||% character(0)

  # Persist resolved-but-0-feature accessions as sentinel rows so they live in
  # cache$accession and an incremental refresh stops re-fetching them. Sentinels
  # are merged exactly like feature rows (their accessions are resolved, so they
  # are not in `unresolved`).
  fresh <- rbind(fresh, pelsa_zero_feature_rows(zero_feature))
```

Then the existing merge block uses `fresh` (now sentinel-inclusive) unchanged. Add `n_zero_feature` to the success result list (alongside `n_unresolved` etc.):

```r
    n_unresolved           = length(unresolved),
    n_transient_unresolved = length(transient_unresolved),
    n_zero_feature         = length(zero_feature),
    n_accessions           = length(universe),
```

Add `n_zero_feature = 0L` to `.canceled_result` (no fetch happened):

```r
         n_retained_from_cache = 0L, n_zero_feature = 0L, mode = mode,
         canceled = TRUE)
```

In `pelsa_run_species_refresh`, add `n_zero_feature` to the per-species success record:

```r
           n_transient_unresolved = res$n_transient_unresolved,
           n_zero_feature = res$n_zero_feature,
           n_accessions = res$n_accessions,
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-refresh.R", reporter="summary")'`
Expected: the 2 new tests PASS; existing refresh tests still PASS (their stubs omit `zero_feature` → `%||% character(0)` → no sentinels, no behavior change).

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_refresh_helpers.R tests/testthat/test-pelsa-refresh.R
git commit -m "feat(pelsa): persist zero-feature sentinel rows in cache (both modes) + carry n_zero_feature"
```

---

### Task 4: `pelsa_annotate_features` drops sentinel rows silently

**Files:**
- Modify: `R/tab_pelsa_annotation_helpers.R` (`pelsa_annotate_features`, the `feat`/`feat_bad` block ~lines 437-454)
- Test: the existing annotation test file (find via `grep -l pelsa_annotate_features tests/testthat/*.R`)

**Interfaces:**
- Consumes: sentinel rows (Task 2/3 shape).
- Produces: sentinels excluded from the overlap join WITHOUT the "dropped N feature row(s) with NA coordinates" warning (that warning must still fire for genuinely corrupt non-sentinel NA rows).

**Semantics:** A sentinel = `feature_class == "none"` AND NA coords. Partition `feat_bad` (NA/inverted coords) into sentinels (silent drop) and genuine corruption (warn + drop).

- [ ] **Step 1: Write the failing test**

Find the file: `grep -l "pelsa_annotate_features" tests/testthat/*.R`. Add (adjust fixture builders to match that file's helpers; this uses inline frames):

```r
test_that("annotate_features drops sentinel rows silently (no NA-coord warning)", {
  plot_df <- data.frame(
    PG.ProteinAccessions = "P00002", pep_start = 5L, pep_end = 9L,
    stringsAsFactors = FALSE)
  # feat_df has ONLY a sentinel for P00002 (resolved, 0 features).
  feat_df <- data.frame(
    accession = "P00002", feature_type = "", start = NA_integer_,
    end = NA_integer_, description = "", feature_class = "none",
    class_score = 0L, coord_quality = "", stringsAsFactors = FALSE)

  expect_silent(out <- pelsa_annotate_features(plot_df, feat_df))
  # The peptide gets the no-overlap fallback (feature_class_primary "none").
  expect_identical(out$feature_class_primary, "none")
})

test_that("annotate_features STILL warns on genuinely corrupt NA-coord rows", {
  plot_df <- data.frame(
    PG.ProteinAccessions = "P00003", pep_start = 5L, pep_end = 9L,
    stringsAsFactors = FALSE)
  # NA coords but a REAL feature_class -> corruption, must warn.
  feat_df <- data.frame(
    accession = "P00003", feature_type = "domain", start = NA_integer_,
    end = NA_integer_, description = "x", feature_class = "folded_domain",
    class_score = 2L, coord_quality = "exact", stringsAsFactors = FALSE)

  expect_warning(pelsa_annotate_features(plot_df, feat_df),
                 "NA or inverted")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("<annotation test file>", reporter="summary")'`
Expected: FAIL — the sentinel test emits the "NA or inverted" warning (so `expect_silent` fails) because the current code treats all NA-coord rows as corruption.

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_annotation_helpers.R`, replace the `feat_bad` block (~lines 448-454) with sentinel-aware partitioning:

```r
  # Sentinel rows (feature_class "none" with NA coords) mark resolved-but-0-
  # feature accessions; they carry no interval and are DROPPED silently. Genuine
  # corruption (NA/inverted coords on a REAL feature_class) is also dropped but
  # WARNS so a bad cache surfaces in logs.
  na_or_inverted <- is.na(feat$start) | is.na(feat$end) |
    (!is.na(feat$start) & !is.na(feat$end) & feat$start > feat$end)
  is_sentinel <- na_or_inverted & (feat$feature_class == "none") &
    is.na(feat$start) & is.na(feat$end)
  feat_corrupt <- na_or_inverted & !is_sentinel
  if (any(feat_corrupt)) {
    warning("pelsa_annotate_features: dropped ", sum(feat_corrupt),
            " feature row(s) with NA or inverted (start > end) coordinates ",
            "from the feature cache.", call. = FALSE)
  }
  feat <- feat[!na_or_inverted]
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("<annotation test file>", reporter="summary")'`
Expected: both new tests PASS; existing annotation tests still PASS (a real cache with valid coords has no sentinels/NA rows, so behavior is unchanged).

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_annotation_helpers.R tests/testthat/<annotation test file>
git commit -m "feat(pelsa): annotate_features drops 0-feature sentinel rows silently (warn only on real corruption)"
```

---

### Task 5: `pelsa_annotation_status_counts` (three-way Summary bucketing)

**Files:**
- Modify: `R/tab_pelsa_annotation_helpers.R` (add after `pelsa_unannotated_accessions`, ~line 546)
- Test: the annotation test file

**Interfaces:**
- Consumes: a dataset's accession source (`plot_df` or vector) + `feat_df` (now sentinel-aware).
- Produces: `pelsa_annotation_status_counts(plot_df_or_accessions, feat_df) -> list(n_with_features=<int>, n_zero_feature=<int>, n_failed=<int>)`.

**Semantics (bucket each unique dataset accession token, isoform-base fallback):**
- `n_with_features` — token's accession (or its base) appears in `feat_df` with at least one **real feature** row (a row whose `feature_class != "none"` OR with non-NA coords).
- `n_zero_feature` — token resolves ONLY to sentinel rows in `feat_df` (in the cache, but every matching row is a sentinel).
- `n_failed` — token (and its base) absent from `feat_df` entirely (== today's `pelsa_unannotated_accessions`).
- The three are mutually exclusive and sum to the unique-token count.

- [ ] **Step 1: Write the failing test**

Add to the annotation test file:

```r
test_that("annotation_status_counts buckets accessions three ways", {
  # P1: real feature; P2: sentinel only (0 features); P3: absent (failed).
  feat_df <- data.frame(
    accession     = c("P1", "P2"),
    feature_type  = c("domain", ""),
    start         = c(1L, NA_integer_),
    end           = c(9L, NA_integer_),
    description   = c("d", ""),
    feature_class = c("folded_domain", "none"),
    class_score   = c(2L, 0L),
    coord_quality = c("exact", ""),
    stringsAsFactors = FALSE)
  plot_df <- data.frame(
    PG.ProteinAccessions = c("P1", "P2", "P3"), stringsAsFactors = FALSE)

  cnt <- pelsa_annotation_status_counts(plot_df, feat_df)
  expect_identical(cnt$n_with_features, 1L)  # P1
  expect_identical(cnt$n_zero_feature, 1L)   # P2 (sentinel only)
  expect_identical(cnt$n_failed, 1L)         # P3 (absent)
})

test_that("annotation_status_counts: isoform input resolves via base feature", {
  feat_df <- data.frame(
    accession = "P1", feature_type = "domain", start = 1L, end = 9L,
    description = "d", feature_class = "folded_domain", class_score = 2L,
    coord_quality = "exact", stringsAsFactors = FALSE)
  # Input "P1-2" should count as with-features via base P1.
  cnt <- pelsa_annotation_status_counts(c("P1-2"), feat_df)
  expect_identical(cnt$n_with_features, 1L)
  expect_identical(cnt$n_zero_feature, 0L)
  expect_identical(cnt$n_failed, 0L)
})

test_that("annotation_status_counts: failed bucket equals legacy unannotated", {
  feat_df <- data.frame(
    accession = "P1", feature_type = "domain", start = 1L, end = 9L,
    description = "d", feature_class = "folded_domain", class_score = 2L,
    coord_quality = "exact", stringsAsFactors = FALSE)
  plot_df <- data.frame(PG.ProteinAccessions = c("P1", "PX", "PY"),
                        stringsAsFactors = FALSE)
  cnt <- pelsa_annotation_status_counts(plot_df, feat_df)
  expect_identical(cnt$n_failed,
                   length(pelsa_unannotated_accessions(plot_df, feat_df)))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("<annotation test file>", reporter="summary")'`
Expected: FAIL — `could not find function "pelsa_annotation_status_counts"`.

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_annotation_helpers.R`, add after `pelsa_unannotated_accessions`:

```r
# ---- Function 4: three-way annotation status counts (Summary QC) -------------

# Bucket a dataset's accessions into three mutually-exclusive annotation states
# against the (sentinel-aware) feature cache, for the Summary QC dashboard:
#   n_with_features  accession (or its isoform base) has >= 1 REAL feature row
#                    (feature_class != "none") in feat_df.
#   n_zero_feature   accession is in feat_df but ONLY as sentinel row(s)
#                    (feature_class "none") -- UniProt resolved it with zero
#                    features (requires a cache rebuilt with sentinels; an old
#                    cache has none, so such accessions fall into n_failed).
#   n_failed         accession (and its base) absent from feat_df entirely.
# The three sum to the unique dataset-accession-token count. n_failed equals the
# legacy pelsa_unannotated_accessions() length.
#
# @param plot_df_or_accessions data.frame (PG.ProteinAccessions / accession) or a
#        character vector of (possibly ;-delimited) accession strings.
# @param feat_df per-feature table with accession + feature_class columns.
# @return list(n_with_features=<int>, n_zero_feature=<int>, n_failed=<int>).
# @noRd
pelsa_annotation_status_counts <- function(plot_df_or_accessions, feat_df) {
  stopifnot(is.data.frame(feat_df))
  if (!all(c("accession", "feature_class") %in% colnames(feat_df))) {
    stop("pelsa_annotation_status_counts: feat_df needs accession + ",
         "feature_class columns")
  }

  # Reuse the legacy tokenizer for the dataset side by deferring to
  # pelsa_unannotated_accessions for the FAILED set, then derive the rest.
  if (is.data.frame(plot_df_or_accessions)) {
    if ("PG.ProteinAccessions" %in% colnames(plot_df_or_accessions)) {
      raw <- as.character(plot_df_or_accessions[["PG.ProteinAccessions"]])
    } else if ("accession" %in% colnames(plot_df_or_accessions)) {
      raw <- as.character(plot_df_or_accessions[["accession"]])
    } else {
      stop("pelsa_annotation_status_counts: data.frame needs ",
           "PG.ProteinAccessions or accession")
    }
  } else {
    raw <- as.character(plot_df_or_accessions)
  }
  tokens <- trimws(unlist(strsplit(raw, ";", fixed = TRUE), use.names = FALSE))
  if (is.null(tokens)) tokens <- character(0)
  tokens <- unique(tokens[!is.na(tokens) & nzchar(tokens)])
  if (length(tokens) == 0L) {
    return(list(n_with_features = 0L, n_zero_feature = 0L, n_failed = 0L))
  }

  # Accessions in feat_df WITH a real feature (feature_class != "none").
  real <- feat_df[!is.na(feat_df$feature_class) &
                    feat_df$feature_class != "none", , drop = FALSE]
  real_acc <- unique(as.character(real$accession))
  real_acc <- real_acc[!is.na(real_acc) & nzchar(real_acc)]
  real_set <- unique(c(real_acc, .pelsa_isoform_base(real_acc)))

  # All accessions present in feat_df (real OR sentinel).
  all_acc <- unique(as.character(feat_df$accession))
  all_acc <- all_acc[!is.na(all_acc) & nzchar(all_acc)]
  all_set <- unique(c(all_acc, .pelsa_isoform_base(all_acc)))

  token_base <- .pelsa_isoform_base(tokens)
  has_real    <- tokens %in% real_set | token_base %in% real_set
  in_cache    <- tokens %in% all_set  | token_base %in% all_set
  # zero-feature = in cache but no real feature; failed = not in cache at all.
  n_with <- sum(has_real)
  n_zero <- sum(in_cache & !has_real)
  n_fail <- sum(!in_cache)
  list(n_with_features = as.integer(n_with),
       n_zero_feature  = as.integer(n_zero),
       n_failed        = as.integer(n_fail))
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("<annotation test file>", reporter="summary")'`
Expected: the 3 new tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_annotation_helpers.R tests/testthat/<annotation test file>
git commit -m "feat(pelsa): add pelsa_annotation_status_counts (with-features / zero-feature / failed)"
```

---

### Task 6: Store the three-way counts in per-dataset QC

**Files:**
- Modify: `R/tab_pelsa_analysis_helpers.R` (`qc` list assembly ~lines 935-943; the `unannotated <- ...` site ~line 817)
- Test: `tests/testthat/test-pelsa-refresh.R` or the analysis test file (find via `grep -l "n_unannotated_accessions" tests/testthat/*.R`)

**Interfaces:**
- Consumes: `pelsa_annotation_status_counts` (Task 5).
- Produces: `entry$qc$n_annotated_with_features`, `entry$qc$n_annotated_zero_feature`, `entry$qc$n_unannotated_accessions` (unchanged name = the failed bucket).

**Semantics:** Compute the 3-way counts on the same `matched` + `feat_df` already used for `unannotated`. `n_unannotated_accessions` stays the failed bucket (== `n_failed`), preserving the existing field's meaning + any exporter that reads it.

- [ ] **Step 1: Write the failing test**

Find the analysis test (`grep -l "pelsa_run_analysis\|n_unannotated_accessions" tests/testthat/*.R`). Add a focused assertion (adapt to that file's analysis-invocation harness; if none exists, assert via a direct call to the QC assembly the file already exercises). Minimal version asserting the qc fields exist with correct values through the public path:

```r
test_that("per-dataset QC reports the three annotation buckets", {
  # Use the same harness the file already uses to build an `entry`; then:
  expect_true(all(c("n_annotated_with_features", "n_annotated_zero_feature",
                    "n_unannotated_accessions") %in% names(entry$qc)))
  # failed bucket unchanged == legacy unannotated length.
  expect_identical(entry$qc$n_unannotated_accessions,
                   length(entry$unannotated))
})
```

If the file has no existing `entry` harness, instead add the assertion to whichever test builds a full analysis `entry` (search for `$qc$n_unannotated_accessions` usages in tests) and extend it.

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("<analysis test file>", reporter="summary")'`
Expected: FAIL — `n_annotated_with_features` / `n_annotated_zero_feature` not in `entry$qc`.

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_analysis_helpers.R`, near the `unannotated <- pelsa_unannotated_accessions(matched, feat_df)` line (~817), add:

```r
  unannotated <- pelsa_unannotated_accessions(matched, feat_df)
  annotation_status <- pelsa_annotation_status_counts(matched, feat_df)
```

In the `qc <- list(...)` block (~935-943), add the two new counts (keep `n_unannotated_accessions` as the failed bucket):

```r
    unmatched_by_reason   = as.list(c(table(reasons))),
    n_unannotated_accessions    = length(unannotated),
    n_annotated_with_features   = annotation_status$n_with_features,
    n_annotated_zero_feature    = annotation_status$n_zero_feature
  )
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("<analysis test file>", reporter="summary")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_analysis_helpers.R tests/testthat/<analysis test file>
git commit -m "feat(pelsa): store three-way annotation QC counts per dataset"
```

---

### Task 7: Refresh panel + notifications report the three-way fetch breakdown

**Files:**
- Modify: `R/tab_pelsa_refresh_helpers.R` (`pelsa_refresh_notifications` summary ~lines 817-823; `pelsa_refresh_result_ui` per-done line ~867-875)
- Test: `tests/testthat/test-pelsa-refresh.R`

**Interfaces:**
- Consumes: per-species result records with `n_features`, `n_zero_feature`, `n_unresolved` (Task 3).
- Produces: panel + notification lines include the with-features / zero-feature / unresolved breakdown.

**Semantics:** Extend the success-summary wording (both modes) to mention zero-feature count. Keep mode prefix ("rebuilt"/"topped up").

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-pelsa-refresh.R`:

```r
test_that("notifications report the zero-feature count in the summary", {
  results <- list(
    list(species = "9606", n_features = 100L, n_unresolved = 2L,
         n_zero_feature = 30L, n_retained_from_cache = 0L, had_existing = FALSE,
         mode = "incremental", canceled = FALSE, error = NULL))
  msgs <- vapply(pelsa_refresh_notifications(results),
                 function(n) n$message, character(1))
  summary <- msgs[grepl("topped up", msgs, ignore.case = TRUE)]
  expect_length(summary, 1L)
  expect_match(summary, "30")               # zero-feature count surfaced
  expect_match(summary, "no features|zero", ignore.case = TRUE)
})

test_that("result_ui shows the zero-feature count", {
  res <- list(list(species = "9606", n_features = 100L, n_unresolved = 2L,
                   n_zero_feature = 30L, n_retained_from_cache = 0L,
                   had_existing = FALSE, mode = "incremental",
                   canceled = FALSE, error = NULL))
  html <- as.character(pelsa_refresh_result_ui(res))
  expect_match(html, "30")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-refresh.R", reporter="summary")'`
Expected: FAIL — current wording omits the zero-feature count.

- [ ] **Step 3: Write the implementation**

In `pelsa_refresh_notifications`, extend the per-species summary builder (the `if (length(done) > 0L)` block) to include zero-feature. Replace the two `sprintf` branches:

```r
    summaries <- vapply(done, function(r) {
      zf <- r$n_zero_feature %||% 0L
      if (identical(r$mode, "full")) {
        sprintf(paste0("%s: rebuilt - %d with features, %d with no features, ",
                       "%d unresolved (previous feature + membrane files ",
                       "cleared)"),
                r$species, r$n_features, zf, r$n_unresolved)
      } else {
        sprintf(paste0("%s: topped up - %d with features, %d with no features, ",
                       "%d unresolved, %d retained"),
                r$species, r$n_features, zf, r$n_unresolved,
                r$n_retained_from_cache)
      }
    }, character(1))
```

In `pelsa_refresh_result_ui`, extend the per-`done` line:

```r
  for (r in done) {
    zf <- r$n_zero_feature %||% 0L
    line <- if (identical(r$mode, "full")) {
      sprintf(paste0("%s: rebuilt - %d with features, %d with no features, ",
                     "%d unresolved (cache cleared)"),
              r$species, r$n_features %||% 0L, zf, r$n_unresolved %||% 0L)
    } else {
      sprintf(paste0("%s: topped up - %d with features, %d with no features, ",
                     "%d unresolved, %d retained from cache"),
              r$species, r$n_features %||% 0L, zf, r$n_unresolved %||% 0L,
              r$n_retained_from_cache %||% 0L)
    }
    items <- c(items, list(shiny::tags$li(line)))
  }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-refresh.R", reporter="summary")'`
Expected: the 2 new tests PASS; pre-existing notification/result tests still PASS (they assert on substrings like "retained" / "9606" / counts that remain present; the `%||% 0L` handles records without `n_zero_feature`).

NOTE: a few pre-existing notification tests assert exact-ish phrasing (e.g. `"%d features"`). If any now fail because the wording changed from "features" to "with features", update those assertions to match the new copy (they are testing wording, not logic). Re-run after.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_refresh_helpers.R tests/testthat/test-pelsa-refresh.R
git commit -m "feat(pelsa): refresh panel + notifications report with-features / zero-feature / unresolved"
```

---

### Task 8: Summary tab — replace one annotation box with three

**Files:**
- Modify: `R/tab_pelsa_section2.R` (`output$failed_annotation_count` ~lines 187-198; the dashboard `fluidRow` value-box layout ~lines 388-393 and 438-451 if collapsible; the `pelsa_section2_dashboard_ui` signature ~385)
- Test: manual (Shiny render) + the existing section2 construct-smoke test if present (`grep -l "pelsa_section2_dashboard_ui\|failed_annotation_count" tests/testthat/*.R`)

**Interfaces:**
- Consumes: `entry$qc$n_annotated_with_features`, `entry$qc$n_annotated_zero_feature`, `entry$qc$n_unannotated_accessions` (Task 6).
- Produces: three value boxes — "Proteins with >=1 annotation", "Proteins with 0 annotation", "Proteins failed annotation".

**Semantics:** Replace the single `failed_annotation_count` renderValueBox + its output slot with three. Layout: the row currently has 4 boxes at `width=3` (=12). Adding two more makes 6; move the three annotation boxes to a second `fluidRow` of `width=4` each (=12) so the dashboard stays a clean 4-up then 3-up grid.

- [ ] **Step 1: (Construct-smoke test, if a section2 UI test exists)**

If `tests/testthat/` has a section2 UI test, add id assertions; otherwise this task is manually verified (Step 5). Example if a test file exists:

```r
test_that("section2 dashboard exposes the three annotation value boxes", {
  ns <- shiny::NS("PELSASection2Tab")
  html <- as.character(pelsa_section2_dashboard_ui(ns, ome = "proteome"))
  expect_match(html, ns("annotated_with_features_count"), fixed = TRUE)
  expect_match(html, ns("annotated_zero_feature_count"), fixed = TRUE)
  expect_match(html, ns("failed_annotation_count"), fixed = TRUE)
})
```

- [ ] **Step 2: Run (if test added) to verify it fails**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("<section2 test file>", reporter="summary")'`
Expected: FAIL — the two new ids are absent. (Skip if no test file; rely on Step 5.)

- [ ] **Step 3: Write the implementation**

In `R/tab_pelsa_section2.R`, replace the single `output$failed_annotation_count` renderValueBox (~187-198) with three render blocks:

```r
    output$annotated_with_features_count <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else
        (entry$qc$n_annotated_with_features %||% NA_integer_)
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Proteins with >=1 annotation",
        icon     = icon("circle-check"), color = "green")
    })

    output$annotated_zero_feature_count <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else
        (entry$qc$n_annotated_zero_feature %||% NA_integer_)
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Proteins with 0 annotation",
        icon     = icon("circle-minus"), color = "yellow")
    })

    output$failed_annotation_count <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else
        (entry$qc$n_unannotated_accessions %||% length(entry$unannotated))
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Proteins failed annotation",
        icon     = icon("circle-question"), color = "orange")
    })
```

In `pelsa_section2_dashboard_ui` (~388-393), split the value boxes into two rows — keep the first three counts in row 1 (`width=4` each), and put the three annotation boxes in a new row 2 (`width=4` each):

```r
    fluidRow(
      shinydashboard::valueBoxOutput(ns("total_peptide_ids"), width = 4),
      shinydashboard::valueBoxOutput(ns("fully_quantified_count"), width = 4),
      shinydashboard::valueBoxOutput(ns("failed_match_count"), width = 4)
    ),
    fluidRow(
      shinydashboard::valueBoxOutput(ns("annotated_with_features_count"),
                                     width = 4),
      shinydashboard::valueBoxOutput(ns("annotated_zero_feature_count"),
                                     width = 4),
      shinydashboard::valueBoxOutput(ns("failed_annotation_count"), width = 4)
    ),
```

- [ ] **Step 4: Run (if test added) to verify it passes + reload**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); cat("loaded OK\n")'`
Expected: `loaded OK`. If a section2 test was added, it PASSES.

- [ ] **Step 5: Manual verification**

`Protigy::launchApp()`, run a PELSA analysis, open the Summary tab:
- [ ] Three annotation value boxes show: "Proteins with >=1 annotation" (green), "Proteins with 0 annotation" (yellow), "Proteins failed annotation" (orange).
- [ ] The three sum to the dataset's unique protein-accession count.
- [ ] On a cache rebuilt with sentinels (run a Full then Incremental refresh first), "0 annotation" is non-zero; on an old cache it's 0 and those proteins show under "failed annotation".

- [ ] **Step 6: Commit**

```bash
git add R/tab_pelsa_section2.R tests/testthat/<section2 test file if any>
git commit -m "feat(pelsa): Summary QC splits annotation into >=1 / 0 / failed protein counts"
```

---

### Task 9: Full suite + ASCII sweep

**Files:** none (verification only).

- [ ] **Step 1: Reload + run the entire suite**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); r <- devtools::test(); df <- as.data.frame(r); cat("failed:", sum(df$failed), "errored:", sum(df$error), "\n"); fr <- df[df$failed>0|df$error, c("file","test")]; if(nrow(fr)) print(fr) else cat("ALL GREEN\n")'`
Expected: 0 failures.

- [ ] **Step 2: ASCII sweep on edited files**

Run: `LC_ALL=C grep -rnP "[^\x00-\x7F]" R/tab_pelsa_uniprot_fetch.R R/tab_pelsa_refresh_helpers.R R/tab_pelsa_annotation_helpers.R R/tab_pelsa_analysis_helpers.R R/tab_pelsa_section2.R`
Expected: no output.

- [ ] **Step 3: Verify the fetch contract is consistent everywhere**

Run: `grep -rn "zero_feature" R/ | grep -v "tab_pelsa_uniprot_fetch.R\|tab_pelsa_refresh_helpers.R"`
Expected: no consumers outside the two owner files reference `zero_feature` directly (it flows through `n_zero_feature` records); if any do, confirm they handle its absence with `%||%`.

- [ ] **Step 4: Commit (final cleanup, if any)**

```bash
git add -A && git commit -m "chore(pelsa): final sweep after zero-feature sentinels + annotation QC" --allow-empty
```

---

## Self-Review

**1. Spec coverage:**
- "Additional fetched features added to cache so no re-fetch": already true for feature rows (incremental merge, Task 3 keeps it) — the gap was 0-feature accessions, now closed by sentinels (Tasks 1-3). ✓
- "Include accessions fetched with 0 features in the cache so incremental won't re-fetch": sentinel rows (Tasks 2-3) + universe subtraction (already in place: `(dataset∪fasta) − cache$accession`, and sentinels are in `cache$accession`). Test `zero-feature accession is NOT re-fetched` (Task 3). ✓
- "Count #>=1 feature / #0 feature / #unresolved for fetch summary": fetch returns `zero_feature` (Task 1), counts carried (Task 3), surfaced in panel + notifications (Task 7). ✓
- "Wire 0-feature into Summary QC: #>=1 annotation / #0 annotation / #failed": `pelsa_annotation_status_counts` (Task 5), stored in qc (Task 6), three value boxes (Task 8). ✓
- Sentinels must not break annotation overlap/volcano/Woods: single choke point `pelsa_annotate_features` drops sentinels silently (Task 4); all consumers flow through it (verified: section3 `feat_df` → `pelsa_annotate_features`). ✓

**2. Placeholder scan:** Two tasks reference "<annotation test file>" / "<analysis test file>" / "<section2 test file>" — these are resolved by a `grep -l` command stated in the task's Files/Step, not left as prose TODOs. Acceptable (the exact file is discovered at execution by a given command). All code steps show full code.

**3. Type consistency:**
- `zero_feature` (chr vector) from fetch → `pelsa_zero_feature_rows(zero_feature)` (Task 2/3) → `n_zero_feature` (int) in results (Task 3) → wording (Task 7). Consistent.
- `pelsa_annotation_status_counts` returns `n_with_features`/`n_zero_feature`/`n_failed` (Task 5) → stored as `n_annotated_with_features`/`n_annotated_zero_feature`/`n_unannotated_accessions` (Task 6) → read by the three value boxes (Task 8). Names consistent across tasks (note the deliberate rename failed→`n_unannotated_accessions` to preserve the legacy field).
- Sentinel shape identical in Task 2 (builder), Task 4 (drop predicate: `feature_class=="none"` + NA coords), Task 5 (bucketing: `feature_class != "none"` = real). Consistent.

No gaps found.

## Risks

| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| A pre-existing notification test asserts old "%d features" wording | Medium | Low | Task 7 Step 4 note: update wording-only assertions; logic tests unaffected. |
| Sentinel rows inflate cache size after a full proteome refresh | High | Low | Accepted (Global Constraints / brainstorm decision); one row per 0-feature accession, far smaller than feature rows. |
| Old cache (no sentinels) shows 0 in "0 annotation" bucket | Medium | Low | Accepted + documented; rebuilt cache populates it. Task 8 Step 5 verifies both states. |
| `rbind(features, sentinels)` column/type mismatch | Low | Medium | Both are the canonical 8-col schema (Task 2 builds in schema order; `pelsa_empty_feature_frame` types). Task 3 test reads back via `pelsa_read_feature_cache` to confirm round-trip. |
| Sentinel with NA coords reaches `foverlaps` and errors | Low | High | Task 4 drops ALL NA-coord rows (sentinel + corrupt) before the join; test `annotate_features drops sentinel rows silently`. |

## Notes

- The fetch's `zero_feature` uses the same isoform-base fallback as `resolved`, so an isoform input resolved under its base with features is correctly NOT counted zero-feature.
- `n_unannotated_accessions` keeps its name and meaning (the failed bucket) so any exporter reading it (e.g. `pelsa_analysis_helpers.R` QC export) is unaffected.
- No new exports; all helpers `@noRd`. No `devtools::document()` needed.
