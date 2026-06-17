# Test Suite Remediation Plan — completeness & accuracy

Plain-language plan to fix the unit-test suite's **accuracy** problems (tests that pass even when the code is
broken) and fill its highest-value **coverage** gaps. Each item shows **what happens now**, **what happens
after**, and **the fix**, with `file:line` references. Work proceeds in phases;
**after each phase: run the affected tests + a code review, then commit, then move to the next phase.**

> **How this plan was produced:** six parallel Opus subagents audited the whole `R/` tree against
> `tests/testthat/`, one subsystem each (setup pipeline, QC, statistics, customize/heatmap/summary/export/util,
> PELSA data layer, PELSA sections/UI). The 4 "confirmed bug" items below were each re-verified by the main
> agent by reading the exact source lines and the call sites — they are not subagent-only claims.

> **Guiding principle:** a test that asserts against a *copy* of the logic, or swallows the error, is worse
> than no test — it reports green while the real code rots. Phase 1 fixes bugs the current tests hide; Phase 2
> deletes/rewrites the false-confidence tests; Phase 3 fills structural gaps.

---

## Overall state of the suite

- **Well tested (real, hand-computed assertions — leave alone):** PELSA data layer (fasta, uniprot parsing,
  annotation, explode, rollup, coverage, depth, cv, intensity), PELSA volcano/recolor/woods/marker/refresh
  pure helpers, QC CV helpers, QC PCA-loadings helpers, color YAML round-trip, `utilities.R`, `validateGCT`
  and the `perform_*` processing primitives.
- **False confidence (tests exist but cannot fail — Phase 2):** statistics core, QC plot helpers, multi-ome
  heatmap, customize color generation, file-upload removal.
- **Untested real logic (Phase 3):** the export *execution* path, `summary_workflow`, the PELSA container,
  the Start-Analysis / refresh observers, the live volcano highlight, static export-figure builders, and
  almost all Shiny server wiring.

---

## Phase 1 — Confirmed bugs the tests are hiding (fix + failing-first regression test)

Do these first. Each is a real defect verified in source; each existing test missed it because the test was
lenient. Pair every fix with a test that fails before the fix and passes after.

### P1.1 Gradient midpoint is silently the minimum, not the midpoint
- **Plain language:** Three continuous-color plots compute the gradient midpoint as
  `mean(min(group), max(group))`. In R, `mean(x, y)`'s second argument is `trim=`, **not** more data — so this
  returns `min(group)` and ignores the max entirely. Continuous color scales are therefore centered on the
  minimum value.
- **Now -> After:** gradient midpoint wrong on every continuous-annotation boxplot / profile / PCA plot ->
  correct midpoint.
- **Fix:** change to `mean(c(min(group, na.rm=TRUE), max(group, na.rm=TRUE)))` (or `(min+max)/2`) at
  `R/tab_qc_boxplots_helpers.R:85`, `R/tab_qc_profile_plots_helpers.R:71`, `R/tab_qc_PCA_helpers.R:205`.
- **Test:** build each plot on a known continuous vector and assert the `scale_colour_gradient2` midpoint
  equals the true midpoint (not the min).

### P1.2 `create_corr_heatmap` crashes on a NULL color map
- **Plain language:** `create_corr_heatmap` does `if (custom_color_map$is_discrete)` with **no NULL guard**.
  The correlation server passes `custom_color_map = NULL` whenever the selected annotation column has no entry
  in the color map (`R/tab_qc_correlation.R:187` and `:238`, used at `:194`/`:245`). `NULL$is_discrete` is
  `NULL`, and `if (NULL)` errors with *"argument is of length zero"*. The sibling `create_corr_boxplot`
  already guards this (`R/tab_qc_correlation_helpers.R:153`), confirming the intent.
- **Now -> After:** correlation heatmap errors for any annotation column absent from the custom-color map ->
  falls back to default colors like the boxplot does.
- **Fix:** add `if (!is.null(custom_color_map) && custom_color_map$is_discrete)` (plus an `else` default
  branch mirroring `create_corr_boxplot`) at `R/tab_qc_correlation_helpers.R:31`.
- **Test:** `create_corr_heatmap(gct, "group", "ome", custom_color_map = NULL)` returns a heatmap, no error.

### P1.3 "Features w/o quantification" row never appears in the dataset summary
- **Plain language:** `summary_dataset` computes the count of all-NA rows, then calls `append(...)` to add a
  "Features w/o quantification" row — but **the result is discarded** (never reassigned to
  `dataset_summary`). The row silently never shows up in the table.
- **Now -> After:** unquantified-feature count missing from the Summary tab + export -> present.
- **Fix:** `dataset_summary <- append(dataset_summary, list(...), after = ...)` at
  `R/tab_summary_helpers.R:244-249`.
- **Test:** `summary_dataset` with an all-NA row present asserts the row appears with the right count.

### P1.4 Stale skipped test asserts the wrong UniProt contract
- **Plain language:** `pelsa_fetch_uniprot` returns three components (`features`, `unresolved`, `canceled` —
  see `R/tab_pelsa_uniprot_fetch.R:376-377,391-392`). One test (`test-pelsa-uniprot.R` block 16) asserts only
  two names. It never runs (triple-skipped: cran/ci/offline), so the contradiction is invisible until someone
  runs it locally.
- **Now -> After:** dead test enshrining a wrong contract -> matches the 3-name contract (blocks 13/14
  already do).
- **Fix:** update the `expect_named(...)` to `c("features","unresolved","canceled")`.

**Phase 1 gate:** `devtools::load_all(".")` + `devtools::test()` on the affected files (qc-module, summary,
pelsa-uniprot) + a quick code review of the four diffs; commit.

---

## Phase 2 — Replace false-confidence tests (delete/rewrite)

These tests currently report coverage they do not provide. Rewriting them is higher value than adding new
tests, because they actively mislead.

### P2.1 Statistics suite tests re-implementations, not `stat.testing`
- **Plain language:** `test-batch-contrast.R` and the `test_*_t_test` helpers in `test-statistics-module.R`
  define their own copies of limma code and assert on the copies. `stat.testing` itself is never called. One
  copy even uses a different model than the real code
  (`cbind(ref=1, as.numeric(groups))` vs `model.matrix(~ 0 + groups)` + `makeContrasts`). The real
  One-sample branch and the Moderated-F post-hoc-contrast block (`R/tab_stat_setup_helpers.R:166-231`) have
  **zero** real coverage.
- **Fix:** add tests that call the real `stat.testing` on the existing synthetic mock GCTs for: two-sample,
  one-sample, F-test (incl. post-hoc), and a **multi-ome + multi-contrast** case (regression guard for the
  STAT-07 accumulator fix). Assert real output columns (`logFC.*`, `adj.P.Val.*`, `sign.logP.*`,
  `significant.*`) and value sanity (p in [0,1], adj >= nominal). Retire the divergent re-implementation
  helpers, or keep one only as a cross-check that compares to `stat.testing` output.

### P2.2 Multi-ome heatmap tests swallow their own failures
- **Plain language:** Most `myComplexHeatmap` tests wrap assertions in
  `tryCatch(..., error = function(e) expect_true(is.character(e$message)))`, so any thrown error becomes a
  pass. The dataset-reorder, GENEMAX-cap, filtering, and clustering assertions only run on success and never
  fail the test.
- **Fix:** remove the `tryCatch` swallow; either let it run with `skip_if_not_installed("ComplexHeatmap")` or
  assert directly on the returned `$Table` / `cluster_columns` / `cluster_rows`. Concretely assert dataset
  ordering via `levels($Table$ome)`, GENEMAX cap via unique-gene count, and filtering via `all($Table$ome == ...)`.

### P2.3 Tautological re-implementation tests
- **Plain language:** Several tests compute the expected value with the same expression they're testing, or
  re-inline the function body, so they assert arithmetic against itself.
- **Fix (delete or re-point at the real function):**
  - `test-statistics-module.R:1179-1315` (p-value/volcano cutoff) -> already covered by
    `test-volcano-labeling.R` `build_volcano_df` tests; delete.
  - `test-statistics-module.R:1546-1819` (annotation suitability) -> drive the real reactive via `testServer`
    or extract a pure predicate and test that.
  - `test-gct-processing.R` geneSymbol-selection block -> call `apply_gene_symbol_from_params`.
  - `test-shiny-helpers.R` 2-component-filtering block -> assert on real `gctSetupUI` output.
  - `test-pelsa-summary.R:208-230` (empty tables) -> drive `output$unmatched_table` /
    `output$unannotated_table` via `testServer`.
  - merge tests in `test-multi-ome_heatmap.R` / `test-error-handling.R` -> exercise the real
    `merge_processed_gcts` (extract the pure merge step so it can run without `withProgress`).

### P2.4 `test-file-upload-removal.R` tests the wrong algorithm
- **Plain language:** It asserts `gsub("[^a-zA-Z0-9_]","_",name)`, but production uses `gct_remove_btn_id`
  (hex-encoded, injective). The naive gsub is the known-buggy approach the real function was written to
  replace; these tests would pass if `gct_remove_btn_id` were deleted.
- **Fix:** rewrite to call `gct_remove_btn_id` and assert injectivity (`a-b.gct` vs `a_b.gct` differ),
  stability, and round-trip. (`test-sidebar-setup-remove-observers.R` already has the collision test — align
  with it.)

### P2.5 No-op smoke assertions
- **Fix:** `sd.filter` percentile test asserts on NA counts / `values.filtered` (rows are never dropped, so
  `nrow <=` is always true); replace the `perform_data_normalization` "20 samples" `expect_true(TRUE)` /
  either-or branches with an explicit assertion on the >20-sample 2-component guard.

**Phase 2 gate:** `devtools::test()` full suite (expect the rewritten files to now genuinely exercise the
code); code review focusing on "can this test fail?"; commit.

---

## Phase 3 — Fill high-value coverage gaps (new tests)

Real logic with no test today, ordered by risk/value.

### P3.1 Export execution path (highest value — data-leak + correctness surface)
- **Gap:** `exportTabServer`'s download handler is never invoked. The params-YAML write that strips
  `gct_file_path` (a path-leak guard), the `color_scheme.yaml` write, and the zip assembly are tested only
  against hand-copied logic in `test-export-hygiene.R`.
- **Add:** a `testServer(exportTabServer, ...)` test that drives the download, unzips, and asserts: per-ome
  `*_parameters.yaml` exists and contains no `gct_file_path`; `customization/color_scheme.yaml` round-trips;
  folder layout is `ome/tab/file`. Also exercise the per-module export functions (summary's 4, multiome PDF,
  PELSA `pelsa_intensity_export_ggplot` / `pelsa_woods_export_ggplot`, incl. `.pelsa_pack_lanes`).

### P3.2 `summary_workflow` branching (`R/tab_summary_helpers.R`)
- **Gap:** conditional rows for id-mapping, sample/row filters, group normalization, StdDev percentile, and
  the `id_mapping_n_unmapped = total - bad` computation — all untested.
- **Add:** one test per branch asserting row presence and order.

### P3.3 PELSA container + Start-Analysis / refresh observers
- **Gap:** `pelsaContainer_Server` (dataset-switcher sync — recent bug M4; analyzed-datasets seam; pin logic;
  `active_dataset` fallback), the Start-Analysis observer (validate -> progress -> run -> seam -> navigate),
  and the 5C refresh observer (confirm-gate threshold, in-flight guard) are all untested at the server level.
- **Add:** `test-pelsa-container.R` + section1 observer `testServer` tests (see the per-subsystem audit H1/H2/M5).

### P3.4 Live volcano highlight is illusory-covered
- **Gap:** `pelsa_volcano_recolor` is fully unit-tested but **no longer wired** — production switched to the
  gold-overlay `addTraces` path. The actual highlight (two gold observers, `gold_present` bookkeeping,
  `onFlushed` re-add after a base rebuild) has no test.
- **Add:** a section3 `testServer` test that asserts the gold-overlay path is driven on selection and re-added
  exactly once after a color-mode/contrast rebuild.

### P3.5 UniProt fetcher internals (offline)
- **Gap:** batching, the consecutive-failure circuit breaker, and the 4xx-vs-5xx split are only reachable via
  live network (skipped on CI).
- **Add:** mock `httr2` (`with_mocked_responses` / `local_mocked_bindings`) to cover batching count, breaker
  tripping at `.PELSA_BREAKER_LIMIT`, 4xx->unresolved vs 5xx->breaker, and the `on_batch` callback.

### P3.6 Setup pipeline safety nets (mutation + export hygiene)
- **Gap:** `deep_clone_gct` / `df_deep_copy` (anti-mutation guards), `repackage_transformed_gct_with_upload_rdesc`
  / `strip_gene_symbol_mapping_columns` (stop `geneSymbol_original*` leaking into exports), and the
  `merge_processed_gcts` conflict-rename / missing-column-NA-fill branches are untested.
- **Add:** mutate-the-clone-source-unchanged tests; column-strip tests; a two-ome merge with a conflicting and
  a one-ome-only column.

### P3.7 Remaining smaller gaps
- `get_pvals` nominal-vs-adjusted column disambiguation (the `P.Value` vs `Log.P.Value` collision that
  `get_volcano_cols` guards but `get_pvals` does not — `R/tab_stat_summary_helpers.R:34`).
- `output$de_summary_table` DE-count math (`R/tab_stat_summary.R:471`).
- `extractGenes` / `getHMTable` / `dynamicHeightHM`; color-mod utils (`color_mod`/`color_dist`/`color_range`);
  `get_plot_export_dimensions` / `get_ggsave_params` / `get_pdf_params`.
- PELSA: zero-row `pelsa_explode_accessions` / `pelsa_thin_background`; `pelsa_depth_summary(c(0,0,0))` cv-NA
  guard; malformed-JSON `pelsa_parse_uniprot_json`; no-header FASTA `stop`; `pelsa_safe_name` sanitization.
- De-flake `test-pelsa-intensity-data.R` block 5 (`skip_on_ci`/`skip_on_cran` — it asserts wall-clock, not
  correctness); anchor the bare `expect_error()` calls with regex matchers.

**Phase 3 gate:** `devtools::test()` + `devtools::check()`; code review; commit per sub-item.

---

## Suggested sequencing

1. **Phase 1** (4 bug fixes + tests) — small, ships correctness fixes immediately.
2. **Phase 2** (rewrite false-confidence tests) — removes misleading green; do before relying on coverage numbers.
3. **Phase 3** (new coverage) — largest; can be split across several commits by sub-item.

Dead-code note (decide during Phase 2/3, do not silently delete): `detect_control_group` and
`generate_all_vs_reference` (`R/tab_stat_setup_contrast_helpers.R`) are tested but have no caller;
`multiome_heatmap_custom_colors` and `pelsa_placeholder_box` look superseded. Either re-wire or remove with
their tests rather than leaving tested-but-unused code inflating the coverage count.
