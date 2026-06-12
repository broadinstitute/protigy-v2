# Export Section — Performance Optimization Plan

Tracking doc for runtime/performance improvements to the **Export** module and the
**statistics plot (volcano) export** path on large datasets. Findings come from a
benchmark + architecture review (R 4.6.0, synthetic inputs sized to the bundled BRCA
omes: proteome 10,569 / phospho 34,364 features × 77 samples).

> Branch: `perf/data-processing-optimization`

---

## TL;DR

The volcano / stat-plot export path dominates large-dataset export time — but the
bottleneck is **`print()` / PDF rasterization and ggrepel label layout**, NOT data prep.
Data prep (`build_volcano_df`, `get_volcano_cols`) is sub-millisecond. Total export
wall-time scales as **n_omes × n_contrasts × per-contrast render**, run sequentially and
blocking the Shiny session.

Recommendations **#1 + #2 together should cut large-dataset export times by ~an order of
magnitude.**

---

## Architecture summary (for context)

- `R/tab_export.R` is the only consumer of `all_exports`, assembled in
  `R/app_server.R:127-144`. It inherits `all_exports`, `GCTs_and_params`, and `globals`.
- Export contract: `exports[[tab]][[ome]][[name]] = function(dir_name)`; each function
  writes one file into `dir_name`. See `dev/module_requirements.md` → "Exporting from a Module".
- Download flow (`tab_export.R:129-276`): tempdir + per-ome folders → params/colors YAML →
  **first pass calls every reactive export once just to count files** → second pass writes
  files → `zip::zip`.
- Export functions **re-generate output from scratch** and reuse nothing from the live UI.
  The on-screen volcano is a *plotly* object for one contrast; the export re-calls the pure
  ggplot `plotVolcano()` once per **ome × contrast** and `print()`s each to PDF
  (`R/tab_stat_plot.R:921-1049`).

---

## Benchmark evidence (measured unless noted)

| Operation (phospho, 34,364 feat) | Time |
|---|---|
| `build_volcano_df` / `get_volcano_cols` | sub-millisecond |
| `plotVolcano` ggplot object build (top-20 labels) | ~24 ms |
| `print(top-20 volcano)` → PDF | ~270 ms |
| `print(ALL-significant volcano)` → PDF | ~7.3 s |
| Full export: 10 contrasts, top-20 → 1 PDF | ~10.3 s |

ggrepel cost (34K points): 20 labels ~250 ms · 2,000 labels ~2.4 s · 2,000 @ `max.overlaps=Inf` ~6.2 s.

**Ruled out as bottlenecks (with evidence):** data prep, ggplot object build, `zip::zip`,
and the O(n²) overlap loop in `add_volcano_labels` (that's the on-screen plotly path, not
PDF export).

**Unverified caveat:** benchmarks used synthetic inputs shaped to the documented contract
(correct dims + column naming), not a live `stat.testing()` result (which needs a Shiny
reactive context). Relative timings are representative; absolute numbers vary with hardware
and real label distributions.

---

## Optimization tasks

### [ ] #1 — Clamp ggrepel work for "All significant" label mode  · HIGH impact / LOW risk
- **Where:** `R/tab_stat_plot_helpers.R:243-249` (label-set selection) and `:288-301` (ggrepel call).
- **Problem:** "All significant" funnels thousands of labels into ggrepel even though
  `max.overlaps=20` (hardcoded at `tab_stat_plot_helpers.R:294`) discards nearly all of
  them — most of the ~7.3 s is layout work for labels that get thrown away.
- **Change:** pre-truncate the label set to a sane ceiling (~50–100, e.g. reuse
  `volcano_label_top_significant_subset`) before ggrepel; keep `max.overlaps` finite.
- **Expected:** ALL-sig export ~7.3 s → ~0.3 s per contrast (>20×).
- **Risk:** low — overlap pruning already drops those labels, so visual output barely changes.

### [ ] #2 — Parallelize the export loop with the already-imported `furrr`  · HIGH impact / MEDIUM risk
- **Where:** `R/tab_export.R:195-246` (sequential nested `lapply`).
- **Problem:** independent per-ome/per-tab exports are embarrassingly parallel and
  CPU-bound (graphics rendering), but run sequentially and block the session. `future`/`furrr`
  are declared deps (`R/protigy-package.R:41-42`) but used nowhere.
- **Change:** wrap inner export execution in `furrr::future_map` with
  `future::plan(multisession, workers = availableCores()-1)`.
- **Expected:** near-linear speedup with cores (sum-of-parts → ~max-of-parts).
- **Risk:** medium — export closures must read reactive values into **plain values before
  dispatch** (reactivity isn't available in `future` workers). PDF writes are independent
  files (no I/O contention). The `withProgress` bar needs `progressr` or coarser updates.

### [ ] #3 — Drop the redundant counting-pass; memoize per-(ome, contrast) df builds  · MEDIUM impact / LOW risk
- **Where:** `R/tab_export.R:177-188` (first pass) and `:197-201` (real pass);
  `R/tab_stat_plot.R:994-1046` and `:1133-1146` (CSV export).
- **Problem:** the progress-bar counting pass calls every reactive export once, doubling
  evaluation for reactive top-level exports. Separately, `build_volcano_df`/`get_volcano_cols`
  are recomputed for the same contrasts across the render, plot-export, and CSV-export branches.
- **Change:** compute the export list once into a local and reuse; memoize df builds per
  `(ome, contrast)` within an export call.
- **Expected:** removes one full redundant reactive eval (df rebuilds are cheap).
- **Risk:** low.

### [ ] #4 — Reuse the live-rendered contrast instead of rebuilding all from scratch  · MEDIUM impact / LOW-MEDIUM risk
- **Where:** `R/tab_stat_plot.R:736-865` (live render) vs `:921-1049` (export rebuild).
- **Problem:** the currently-selected contrast is already built on screen; export rebuilds
  it (plus all others) from scratch.
- **Change:** cache the per-contrast ggplot via `memoise`, keyed on
  `(ome, contrast, label settings, cutoff, stat)`, so render and export share one build and
  repeat downloads skip the rebuild.
- **Expected:** avoids one full rebuild for the active contrast and all rebuilds on re-download.
- **Risk:** low-medium — cache invalidation on param/label changes must be correct.

### [ ] #5 — Hoist `get_pdf_params()` disk read out of the per-export loop  · LOW impact / NO risk
- **Where:** `R/tab_stat_plot.R:936`, `R/tab_stat_summary.R:600,649`, `R/utilities.R:129-167`.
- **Problem:** reads `setupDefaults.yaml` from disk on every export-function call.
- **Change:** read once at module/app init; pass the cached dims.
- **Expected:** trivial (~ms), removes pointless repeated disk I/O under a tight loop.
- **Risk:** none.

---

## Suggested order

1. **#1** — highest value, lowest risk; start here. Implement TDD against the volcano label helpers.
2. **#2** — biggest absolute speedup; do after #1 so each export unit is already cheaper.
3. **#3**, **#5** — quick cleanups.
4. **#4** — most involved (cache invalidation); do last.

## Verification per task

- After each change: `devtools::load_all(".")` then `devtools::test_active_file()` /
  `devtools::test()`.
- Re-benchmark the affected path against the bundled BRCA omes
  (`data(brca_retrospective_v5.0_phosphoproteome_gct)` etc.) to confirm the expected speedup.
- Confirm exported PDFs/CSVs are visually/structurally unchanged (esp. #1 and #4).
