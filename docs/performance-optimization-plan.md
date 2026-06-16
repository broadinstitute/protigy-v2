# Performance Optimization Plan: ProTIGY Pipeline + Export

> **Implementation status (verified 2026-06-16 against merged `feat/pelsa-integration`): NONE of the items
> in this doc (1a, 1b, 2a, 2b, 3a-3d) are implemented yet.** They target `sidebar_setup_helpers_GCT-processing.R`,
> `tab_export.R`, `tab_stat_summary.R`, and `tab_qc_cv.R` — all of which the perf branch left untouched. The work
> that *did* ship (the UI-freeze + volcano-render wins: INT-1/2/3, STAT-02, STAT-07) lives in the companion
> tracker `performance-implementation-phases.md` (Phase 1), not here. Per-item `[ ]` markers below. (`2b` is
> marked **Dropped** in the phases doc — <1ms at 2-4 omes.)

## Context
Review of the GCT data processing pipeline (triggered by "Submit" in the setup sidebar) and the multi-omics export path identified targeted, non-breaking optimizations. No new dependencies — `future`/`furrr`/`readr` are already in `DESCRIPTION`.

This plan was hardened against an adversarial multi-agent review. Several items from earlier drafts were dropped as false-positive microbenchmark wins; several proposed code blocks were broken and have been rewritten; the export section is new.

**Methodology note:** Realistic dataset sizes from the BRCA fixtures used in regression tests:
- Proteome: ~10,600 rows × 77 samples
- Phosphoproteome: ~34,400 rows × 77 samples
- 2–4 omes per multi-omics run
- Groups in group-normalization: 2–5 (rarely more)

Wins that don't show measurable improvement at these sizes have been dropped.

---

## Priority 1 — High Impact, Low Risk

### 1a. [ ] Replace `serialize/unserialize` deep copy in `df_deep_copy()`
**File:** `R/sidebar_setup_helpers_GCT-processing.R` — `df_deep_copy()` (~L475)

`unserialize(serialize(df, NULL))` traverses the full R object graph. For atomic-column data frames (which rdesc/cdesc always are) it is ~20–30× slower per call than the `as.data.frame()` round-trip that `safe_copy_rdesc()` already uses elsewhere in this file (L200–204).

**Action:** Replace the body of `df_deep_copy` with a call to (or inline copy of) `safe_copy_rdesc`'s pattern. Do NOT introduce a separate `lapply(df, function(col) col)` form — that does not physically copy until a write occurs and is a misleading "deep copy."

```r
df_deep_copy <- function(df) {
  if (is.null(df)) return(NULL)
  if (!is.data.frame(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  rownames(out) <- rownames(df)
  out
}
```

**Safety:** rdesc/cdesc columns are always atomic by the time this is called. No list-columns survive past `fix_gene_symbols`. Benchmarked saving: ~50–60 ms per two-ome pipeline run (5 invocations).

**Follow-up (optional):** consolidate to a single helper — `df_deep_copy` and `safe_copy_rdesc` are now equivalent.

---

### 1b. [ ] Vectorize `perform_missing_filter` with `rowMeans` + add `drop = FALSE` (bugfix)
**File:** `R/sidebar_setup_helpers_GCT-processing.R` — `perform_missing_filter()` (~L907)

Two changes in one swap:
1. **Perf:** `rowMeans(is.na(data))` is one C-level pass; the current `apply(data, 1, ...)` dispatches an R closure per row. Benchmarked: ~11 ms on proteome, ~58 ms on phospho → ~70 ms saving per two-ome run.
2. **Latent bugfix:** the current code lacks `drop = FALSE`. When exactly one row survives, the matrix collapses to a named numeric vector; downstream `data.frame(data, id = rownames(data))` in `perform_data_filtering` then crashes because `rownames()` of a vector is `NULL`.

```r
perform_missing_filter <- function(data, max_missing) {
  missing_percent <- rowMeans(is.na(data))
  data[missing_percent <= max_missing / 100, , drop = FALSE]
}
```

**Verification:** add a regression test in `tests/testthat/test-gct-processing.R` for the 1-row-surviving case to lock in the bugfix.

---

## Priority 2 — Largest Wins (require careful implementation)

### 2a. [ ] Vectorize `fix_gene_symbols` string operations (REWRITTEN — original draft was broken)
**File:** `R/sidebar_setup_helpers_GCT-processing.R` — `fix_gene_symbols()` (L11–96)

**This is the single largest win in the plan.** Benchmarked: ~178 ms (proteome) + ~545 ms (phospho) → ~4 ms + ~15 ms. **~700 ms saving per two-ome run.**

The earlier draft of this section silently changed behavior. The current `strsplit + trimws + filter` loop applies `trimws()` **per pipe-delimited symbol part** and drops whitespace-only parts. Naive `gsub("\\|{2,}", "|", x)` does NOT collapse pipes separated by whitespace and does NOT trim spaces adjacent to pipes.

**Correct vectorized replacement** (covers both the main split block AND the trailing/leading pipe block at L72–92):

```r
# 1. Normalize delimiter: semicolons -> pipes
rdesc$geneSymbol <- gsub(";", "|", rdesc$geneSymbol, fixed = TRUE)
# 2. Trim whitespace around every pipe (handles "EGFR| BRCA1" and "EGFR | KRAS")
rdesc$geneSymbol <- gsub("\\s*\\|\\s*", "|", rdesc$geneSymbol)
# 3. Collapse any sequence of pipes (now adjacent after step 2) to a single pipe
rdesc$geneSymbol <- gsub("\\|{2,}", "|", rdesc$geneSymbol)
# 4. Strip leading/trailing pipes and whitespace
rdesc$geneSymbol <- trimws(rdesc$geneSymbol)
rdesc$geneSymbol <- sub("^\\|+", "", rdesc$geneSymbol)
rdesc$geneSymbol <- sub("\\|+$", "", rdesc$geneSymbol)
# 5. Empty -> NA
rdesc$geneSymbol[!nzchar(rdesc$geneSymbol)] <- NA_character_
```

**Mandatory verification before merge:**
- Run `tests/testthat/test-gct-processing.R` and `tests/testthat/test-id-to-gene-symbol.R` in full
- Add new test cases for: `"EGFR| BRCA1"`, `"EGFR|  |KRAS"`, `"  |EGFR|"`, `";EGFR;BRCA1;"`, `"|"`, `""`
- Confirm outputs match the current implementation byte-for-byte on the BRCA fixture rdesc

---

### 2b. [~] Pre-compute ome→columns map for two merge loops (NARROWED) — DROPPED (phases doc)
**File:** `R/sidebar_setup_helpers_GCT-processing.R` — `merge_processed_gcts()` (~L1059–1310)

Earlier draft made two wrong claims that have been removed: (a) the "stale `merged_cdesc_subset`" bug — the refresh already exists in the loop right after the NA-fill mutation; (b) applying to the conflict-DETECTION loop — that loop iterates `names(gct@cdesc)` per ome and gains nothing from the precompute.

**Apply ONLY to:** the conflict-removal loop (~L1265) and the missing-column loop (~L1301), where the inner `sapply(GCTs_processed, function(gct) col %in% names(gct@cdesc))` is re-scanned per column.

```r
# Pre-compute once before both loops:
ome_col_sets <- lapply(GCTs_processed, function(gct) names(gct@cdesc))

# Inside each affected loop, replace the per-iteration sapply with:
omes_with_col <- names(Filter(function(cols) col %in% cols, ome_col_sets))
```

Also fix the quadratic accumulation pattern at ~L1226/L1256:
```r
# Current (quadratic — copies whole vector each iteration):
conflict_columns <- unique(c(conflict_columns, conflict_columns_ome))

# Replace with collect-then-unique-once:
conflict_columns_list <- vector("list", length(GCTs_processed))
# ... fill conflict_columns_list[[i]] inside the loop ...
conflict_columns <- unique(unlist(conflict_columns_list, use.names = FALSE))
```

**Impact:** small absolute win (<1 ms for typical 2–3 omes) but the code is clearer and removes a quadratic pattern that would bite hard if ome count grew. Ship with the surrounding fixes.

---

## Priority 3 — Export / Download (Multi-omics)

This section is new. The reviewers found the largest cluster of unaddressed wins in the export path. Multi-omics exports touch `tab_export.R` plus per-tab download functions in `tab_stat_summary.R`, `tab_stat_plot.R`, `tab_qc_cv.R`, etc.

### 3a. [ ] Lower zip compression level
**File:** `R/tab_export.R` — `zip::zip()` call (~L254)

`zip::zip` defaults to `compression_level = 9` (maximum). Multi-omics exports are PDF-heavy (volcano plots, heatmaps, pval histograms × N omes × N contrasts) — PDFs are already compressed internally, so re-compressing at level 9 burns CPU for ~1% size delta.

```r
zip::zip(
  file,
  file.path(dir_name, list.files(exports_dir)),
  recurse = TRUE,
  root = zip_dir,
  compression_level = 1
)
```

**Estimated saving:** 15–40 s → 2–5 s on a 4-ome PDF-heavy archive.

---

### 3b. [ ] Single-pass reactive evaluation in the export handler
**File:** `R/tab_export.R` — `downloadHandler` content function (~L129–260)

Currently every `exports[[tab_name]]()` reactive is read **twice**: once in the pre-loop at L178–188 to compute `total_exports` for the progress bar, then again in the write loop at L195–246. For tabs whose export reactives are non-trivial, this doubles the prep cost.

```r
# Snapshot once before any progress / writing:
exports_snapshot <- lapply(selected_tabs, function(tab_name) {
  if (is.reactive(exports[[tab_name]])) exports[[tab_name]]() else exports[[tab_name]]
})
names(exports_snapshot) <- selected_tabs

# Compute total from snapshot:
total_exports <- sum(vapply(exports_snapshot, function(e) {
  sum(lengths(e[intersect(selected_omes, names(e))]))
}, integer(1)))

# Write loop reads from exports_snapshot, never the reactive directly.
```

Also adds `on.exit(unlink(exports_dir, recursive = TRUE), add = TRUE)` immediately after `dir.create(exports_dir, ...)` to stop the temp-dir leak across repeated exports in a long-running Shiny session.

---

### 3c. [ ] Memoize repeated `stat_results()[[ome]]` reads
**File:** `R/tab_stat_summary.R` — `stat_results_export_function` (~L686–708), `de_summary_export_function` (~L750–820)

`stat_results()[[ome]]` is currently materialized 3× per call in the stat-results export (write.csv + two dplyr `select`s) and re-evaluated again in `de_summary_export_function`. For 30k-row phospho stat tables this is 3–5 full data.frame copies per ome per export.

```r
stat_results_export_function <- function(dir_name) {
  for (ome in names(stat_results())) {
    df <- stat_results()[[ome]]                       # snapshot once
    readr::write_csv(df, file.path(dir_name, paste0("stat_results_", ome, ".csv")))

    sign_mask <- startsWith(colnames(df), "sign.logP")
    mat   <- as.matrix(df[, sign_mask, drop = FALSE])
    rdesc <- df[, !sign_mask, drop = FALSE]
    # ... existing GCT assembly ...
  }
}
```

Apply the same snapshot pattern in `de_summary_export_function`. If the underlying DE-summary computation duplicates `output$de_summary_table`'s logic, extract a `compute_de_summary(df, params, precision)` helper and share it between the renderer and the exporter.

---

### 3d. [ ] Use `readr::write_csv` instead of `utils::write.csv`
**Files:**
- `R/tab_stat_summary.R` — L692, L815
- `R/tab_qc_cv.R` — L383, L409
- `R/tab_stat_plot.R` — L1178

`readr` is already in `Imports`. `readr::write_csv` is a C++ writer that is roughly an order of magnitude faster than `utils::write.csv` on wide tables.

**Estimated saving on a 4-ome multi-omics export with phospho-scale stat tables:** 10–15 s → 1–2 s.

Keep `utils::write.table` for tab-separated parameter dumps (the `stat_parameters_*.txt` writes). Keep `utils::write.csv` for the experimental-design template download — that file is tiny and not on any hot path.

**Caveat:** `readr::write_csv` does not emit row names. Confirm none of the swapped call sites rely on `row.names = TRUE`; the current calls pass `row.names = FALSE` already.

---

## What was REMOVED from earlier drafts (false positives)

These items were in prior versions of this plan and have been cut after benchmarking against realistic dataset sizes:

| Item | Why dropped |
|------|-------------|
| Pre-allocate normalization matrix (`normalize.data` cbind→matrix) | Benchmarked 0.24 ms (4 groups) / 10 ms (worst-case 36 groups). Microbenchmark complaint dressed as "O(n²)" — the cbind is over groups (typically 2–5), not over columns. |
| Vectorize `convert_discrete_numerics` loops | ~5 µs saved per pipeline run. Pure code churn. Acceptable as a readability refactor; not a perf win. |
| `cmapR::subset_gct` refactor in `apply_sample_filter` / `apply_row_filter` | Wider blast radius than appearance suggests; no measurable speedup; current code is clean and tested. |
| `getUniqueColumns` rewrite | Original `length(unique(x)) == nrow(data)` form silently dropped the `is.character()` guard and NA exclusion — a correctness regression for ~0.018 ms saved on a disk-I/O-gated call. Drop entirely. |
| **`furrr::future_map` per-ome parallelism** | See below — kept out of this plan as a hard requirement, not just a perf decision. |

### Why `furrr` per-ome parallelism is out of scope

The earlier draft proposed `future::plan(future::multisession, ...)` + `furrr::future_map2` over per-ome processing. Adversarial review found **four blockers**, all silent-failure modes:

1. **Global state mutation:** `future::plan()` is process-global. In multi-session shinyapps deployments, concurrent users would clobber each other's plans. The proposal never restored the plan with `on.exit(plan("sequential"))`.
2. **Broken error handler:** every per-ome body is wrapped in `my_shinyalert_tryCatch`, which calls `shiny::showNotification()` unconditionally. In `multisession` workers there is no Shiny session — the error handler itself throws, and the worker returns NULL, which the caller interprets as pipeline failure.
3. **2-component normalization is stochastic.** `normalmixEM` draws from the RNG; `furrr_options(seed = TRUE)` uses L'Ecuyer-CMRG, a different stream than sequential execution. Results would silently differ for the same input.
4. **Worker boot cost > sequential cost** for the realistic case. The package has 50+ Bioconductor/cmapR imports; each `multisession` worker reloads them (~3–8 s). Sequential per-ome processing is ~130 ms–2 s per ome. For 2–3 omes (the typical case), parallel is **net negative**.

The narrower "group-norm only" alternative hits the same worker-boot dominance and the same RNG-stream issue for 2-component normalization.

**If this becomes worth revisiting later**, prerequisites are:
- Refactor `my_shinyalert_tryCatch` to return conditions as data rather than calling `showNotification` directly
- Wrap any `future::plan()` change in `on.exit(plan("sequential"))` or `with(plan(...), ..., local = TRUE)`
- Hard-bypass `method == "2-component"` from the parallel path
- Add a fallback to `sequential` when `multisession` cluster creation fails
- Benchmark against the realistic 2–3 ome case, not a hypothetical 10-ome case

Until then, the dead `furrr` / `future` imports in `R/protigy-package.R` should be removed (separate cleanup commit).

---

## Files to Modify

| File | Changes |
|------|---------|
| `R/sidebar_setup_helpers_GCT-processing.R` | 1a `df_deep_copy`, 1b `perform_missing_filter`, 2a `fix_gene_symbols`, 2b merge loops |
| `R/tab_export.R` | 3a zip compression level, 3b single-pass reactive eval + `on.exit` cleanup |
| `R/tab_stat_summary.R` | 3c memoize `stat_results()[[ome]]`, 3d `readr::write_csv` swap |
| `R/tab_qc_cv.R` | 3d `readr::write_csv` swap |
| `R/tab_stat_plot.R` | 3d `readr::write_csv` swap |

---

## Implementation Order

1. **1b** (rowMeans + `drop=FALSE` bugfix) — ship first with a regression test for the 1-row case. Behavioral change is a bugfix.
2. **1a** (`df_deep_copy`) — pure algorithmic swap; consolidate with `safe_copy_rdesc` if scope allows.
3. **3a + 3b + 3d** (export quick wins) — zip level, single-pass reactive snapshot, `readr::write_csv`. Largest user-visible improvement on multi-omics exports.
4. **3c** (memoize `stat_results`) — bundle with the export PR.
5. **2b** (merge loop cleanup) — small win but removes a quadratic pattern.
6. **2a** (`fix_gene_symbols`) — largest single win but requires the test additions above. Ship last, in its own PR, with explicit before/after diff on BRCA fixture outputs.

---

## Verification

1. `devtools::test()` — all existing tests must pass unchanged.
2. **New regression tests required:**
   - `perform_missing_filter` with exactly 1 surviving row (covers 1b bugfix)
   - `fix_gene_symbols` with whitespace-padded pipes, all-whitespace middle parts, leading/trailing pipes (covers 2a behavior preservation)
3. Smoke test via `Protigy::launchApp()`:
   - Upload proteome + phosphoproteome BRCA fixtures
   - Run with Median normalization + StdDev filter; confirm result is byte-identical to pre-optimization
   - Run with group normalization (2+ groups); confirm column order is preserved
   - Trigger full export; confirm zip contents match pre-optimization (filenames, GCT contents, CSV row/col counts)
4. **Performance smoke:**
   - `system.time(processGCTs(...))` before and after on the phospho fixture — expect ~700 ms reduction (2a dominant)
   - `system.time(...)` on the export download flow — expect 10–20 s reduction on a 4-ome run (3a + 3d dominant)

---

## Out of scope / follow-up tickets

Real issues found during review but tracked separately to keep this PR focused:

- **Observer accumulation in `sidebar_setup.R:396–490`** — dynamic `observeEvent` re-registration on every `accumulated_files()` invalidation. Memory leak for long sessions with many add/remove cycles.
- **`parse_gctx_preserve_cdesc` reads each GCT file twice** — `parse_gctx` + `readLines`. Could share the lines buffer.
- **`cmapR::write_gct` row-by-row `cat(append=TRUE)`** — 30k+ file syscalls per GCT export. Fix is upstream in cmapR or via a local writer.
- **Redundant `safe_copy_rdesc` double-call at L502–503 of `apply_gene_symbol_from_params`** — second call is gratuitous; delete it.
- **Volcano export rendering has no cache** — every export re-runs `plotVolcano()` for every ome × contrast. Candidate for a shared `volcano_gg_cache` reactive consumed by both the interactive plotly path and the PDF export.
- **Hoist `read_yaml(system.file(...))`** out of per-file loops in `R/sidebar_setup_helpers_csv-excel-processing.R` (~L156–200, L545–546).
- **`apply(tab, 1, sd, na.rm=T)`** in `R/sidebar_setup_helpers_data-filtering.R:28` — `matrixStats::rowSds` is faster (matrixStats already available at runtime).
- **Serial stat-test loop** in `R/tab_stat_setup.R:1180` — parallelizable in principle, but blocked by the same furrr prerequisites listed above.
- **Dead imports:** `furrr`, `future`, `WriteXLS` are in `DESCRIPTION` but unused in code. Either wire up or remove.
- **No performance regression tests in the suite.** Worth adding `tests/testthat/test-perf-smoke.R` with `system.time` thresholds on the BRCA fixture.

---

## What NOT to Change

- **2-component normalization retry loop** — intentional algorithm; the 20-sample cap already guards the worst case.
- **`cmapR::merge_gct` internals** — external package.
- **Reactive chain structure** in the data-processing path — Shiny invalidation is correct (note: the observer accumulation issue above is a UI sidebar concern, not the processing reactive chain).
- **`Reduce()` for iterative GCT merging** — must be sequential; each merge depends on the previous result.
