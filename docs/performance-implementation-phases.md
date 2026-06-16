# Protigy Performance Optimization — Phase-to-Phase Implementation Plan

> Derived from `performance-optimization-review-v2.md` (synthesized + adversarially validated).
> Every phase ships with a byte-identity regression test against the current implementation on the
> BRCA fixture as a non-negotiable merge gate.

> **Implementation status (last verified 2026-06-16, against merged `feat/pelsa-integration`):**
> **Phase 1 is DONE** (INT-1, INT-2, INT-3, STAT-02, STAT-07 — verified in code). **All other phases
> are NOT yet implemented** — GCT-processing, export, and CSV/Excel conversion paths are untouched.
> Status is tracked with `[x]`/`[ ]` markers in the overview table and per phase below.

## Grouping rule

Tasks are grouped into phases by **three converging factors**:
1. **Shared module / file** — items touching the same helper land together so one PR has one edit surface.
2. **Shared regression-test focus** — items that need the *same* kind of proof (byte-diff of a GCT, a
   filtered-row-set diff, a shinytest2 interaction test) cluster so the test harness is written once.
3. **Shared risk gate / dependency** — items that introduce the same new dependency (`matrixStats`),
   require the same `devtools::document()` regen, or are blocked behind the same upstream item.

Phases are ordered by **user-visible win ÷ validated risk**, front-loading the byte-identity-verified wins
and deferring the speculative server-side volcano work behind `toWebGL`.

---

## Phase overview

| Phase | Status | Theme | Items | Primary win | Risk | Gate |
|-------|--------|-------|-------|-------------|------|------|
| **1** | **[x] DONE** | **Biggest verified wins (UI freeze + volcano render)** | INT-1, INT-2, INT-3, STAT-02, STAT-07 | ~630ms toggle freeze; volcano jank | Low (guarded) | shinytest2 toggle; scattergl click smoke |
| **2** | **[ ]** | **Gene-symbol vectorization** (Cluster A, gene-symbol subset) | dp-2a, dp-redundant | ~480ms→24ms/pass | Low | `geneSymbol`-column diff on fixture |
| **3** | **[x] DONE** | **matrixStats numerics** (Cluster C) | dp-norm, dp-sd, dp-1b | ~250ms/ome + crash bugfix | None/Low | filtered-row-set diff; NA/Inf parity; 1-row test |
| **4** | **[ ]** | **Package attach trim** (Cluster D) | START-01, START-02 | ~1–1.4s cold open | None | re-grep + `check()` clean |
| **5** | **[ ]** | **Deep-copy + setup I/O + observer hygiene** | dp-1a, START-04, START-03 | smaller per-copy + long-session stability | Low | list-column guard; cold-cache bench; add/remove test |
| **5b** | **[ ]** | **CSV/Excel → GCT conversion (post-"Start" build)** | INPUT-1, INPUT-2 | faster GCT build after exp-design upload | Low | byte-identical classification + cdesc on fixture |
| **6** | **[ ]** | **Export CSV + hygiene** (Cluster E, CSV half) | EXP-2, EXP-4 cleanup, EXP-5 | ~1–3s export | Low (caveat) | confirm CSVs terminal; release-note byte change |
| **7 (conditional)** | **[ ]** | **Server-side volcano refactor** (Cluster F, deferred) | STAT-03, STAT-05, STAT-01, STAT-08, EXP-6 | per-render ms on 34k rows | Low–Med (unverified) | only if Phase 1 `toWebGL` insufficient; per-item byte-diff |
| **8** | **[ ]** | **Statistics-tab volcano → PELSA-parity (native scattergl + proxy labeling)** | SVOL-1, SVOL-2, SVOL-3 | volcano render + per-selection refresh | Med | see Phase 8 below |
| **Blocked** | **[ ]** | **dp-double** (gene-symbol runs twice/ome) | dp-double | halves dominant processing cost | Risky | repackage fallback-branch test (Open Q #3) |
| **Dropped** | **[~]** | EXP-1 (GCT writer), STAT-04 (point thinning), dp-2b (merge loops) | — | — | keep cmapR / breaks-results / negligible | — |

---

## Phase 1 — Biggest verified wins  **[x] DONE (verified 2026-06-16)**

The highest-leverage, lowest-validated-risk changes, each mapping directly to a user-reported symptom.
Grouped together because each is **standalone, independently testable, and unblocks deferring lower-priority
items**; they share no edit surface so they can even be parallel sub-PRs, but they form one "ship the obvious
wins first" wave.

> **EXP-1 (single-pass GCT writer) is DROPPED by decision** — we keep `cmapR::write_gct` even though the
> proposed writer was byte-identical. The export path is unchanged. Symptom D (slow export) is instead
> addressed only by Phase 6 (EXP-2 CSV writer + hygiene); the dominant per-row-`cat` GCT cost in cmapR is
> accepted as-is. See "Dropped" below.

- [x] **INT-1 + INT-2 + INT-3** — Intensity-toggle freeze → Symptom B (page grey-out). *Gate: shinytest2 + reactlog.* (INT-1 in `sidebar_setup.R` + `sidebar_setup_helpers_discrete-cache.R`; INT-2 in `sidebar_setup_helpers_discrete-cache.R`; INT-3 in `utilities.R`.)
- [x] **STAT-02** — `toWebGL` volcano → volcano jank. *Gate: scattergl click smoke; PDF path untouched.* (Implemented at `R/tab_stat_plot.R:864` — wraps the existing ggplotly object in `plotly::toWebGL()` with an SVG fallback. NOTE: this is the band-aid wrap, **not** the native scattergl rebuild PELSA uses — see Phase 8.)
- [x] **STAT-07** — latent `results_list` shadowing rename (rides with STAT-02's file; correctness). *Gate: single-ome byte-identical.* (Implemented in `R/tab_stat_setup_helpers.R`.)

---

## Phase 2 — Gene-symbol vectorization  **[ ] NOT IMPLEMENTED**
dp-2a (faithful vectorized `fix_gene_symbols`, fuzz-verified 70k+ cases) + dp-redundant (drop the gratuitous
second `safe_copy_rdesc`). **dp-double stays blocked.** Gate: `geneSymbol`-column diff incl. list-column + UTF-8
fuzz; dp-redundant failure-path assertion.

## Phase 3 — matrixStats numerics  **[x] DONE (verified 2026-06-16)**
Implemented via fully-qualified `matrixStats::colMedians/colMads/rowSds` + base `rowMeans`. Validated by
a standalone OLD-vs-NEW harness (`dev/perf_phase3/`), a live-package equivalence check, `test-perf-phase3.R`,
and an independent Opus stats-validation review (VERDICT: safe; no input changes a stored result; only
~1e-16 reduction-order noise that never flips a filtered set, and NaN-vs-NA on all-NA columns that is always
masked by NA propagation). dp-1b also fixes a latent single-survivor-row crash.

dp-norm (`colMedians`/`colMads`), dp-sd (`rowSds`), dp-1b (`rowMeans(is.na)` + `drop=FALSE` bugfix). Introduces
the `matrixStats` DESCRIPTION/roxygen dependency **once**. Gate: dp-sd filtered-row-set diff (not just
`all.equal`); dp-norm NA/Inf/zero-MAD fixture diff; dp-1b 1-row & 0-row regression tests.

## Phase 4 — Package attach trim  **[ ] NOT IMPLEMENTED** (`WriteXLS`/`future`/`furrr` still in DESCRIPTION)
START-01 (lazy-load vsn/mixtools/mclust/preprocessCore via `pkg::fn()`, keep in Imports) + START-02 (remove dead
`furrr`/`future`/`WriteXLS`). One `devtools::document()` regen. Gate: re-grep R/ + tests/ for bare symbols;
`devtools::check()` clean.

## Phase 5 — Deep-copy + setup I/O + observer hygiene  **[ ] NOT IMPLEMENTED**
dp-1a (`as.data.frame` deep copy with list-column guard), START-04 (bound the second `.gct` read to the header
region), START-03 (fix observer accumulation on file add/remove). Gate: dp-1a list-column guard; START-04
warm/cold bench + byte-identical cdesc incl. `'001'`; START-03 add/remove/re-add/clear shinytest2.

## Phase 5b — CSV/Excel → GCT conversion (post-"Start" build)  **[ ] NOT IMPLEMENTED** (`classifyColumns` still loops per-column; `read_yaml` still in-loop at L185/L545)
**New finding (2026-06-14), not in the original review.** User reported that GCT generation is slow
*immediately after clicking "Start"* once data + experimental design are uploaded — i.e. the CSV/Excel →
GCT conversion path, distinct from the `.gct`-upload and Submit/processing paths the original review covered.
Path: `processCSVExcelWorkflow*` → `convertToGCT` → `classifyColumns` / `createCdesc`
(`R/sidebar_setup_helpers_csv-excel-processing.R`).

- **INPUT-1 — vectorize `classifyColumns` (L392–434).** Currently loops over every sample column and runs
  `experimentalDesign[experimentalDesign$columnName == col_name, ]` — a full O(rows) table scan + subset
  **per column** (O(samples × design_rows); ~77 scans on the BRCA design, ×N for multi-omic). Also recomputes
  `metadata_columns <- setdiff(...)` inside the loop and grows `rdesc_columns`/`sample_columns` with
  quadratic `c(...)`. **Fix:** replace with a single vectorized `match(sample_ids, experimentalDesign$columnName)`
  lookup + a vectorized all-blank test over the matched block, hoisting `metadata_columns`. The sibling
  functions `filterExperimentalColumns` (L439) and `createCdesc` (L501) **already use this `match()` pattern**,
  so this only makes `classifyColumns` consistent with its neighbors. **Result-preserving:** same
  sample/rdesc partition, computed set-wise. *Gate: assert identical `sample_columns`/`rdesc_columns` on a
  fixture before/after, incl. columns missing from the design, all-blank-metadata columns, and the
  no-metadata-column case.*
- **INPUT-2 — hoist `yaml::read_yaml(system.file("setup_parameters/setupDefaults.yaml"))`.** Currently parsed
  **once per uploaded file** inside the loop (L185–187) and again in `createCdesc` (L545–546). The file is
  static/read-only. **Fix:** read once before the loop (or memoize at package scope) and reuse. **Result-preserving:**
  identical parsed list. (Same class of fix as the prior review's START-01/follow-up YAML-hoist note.)
- **INPUT-3 (needs-benchmark, lower priority).** `readr::read_csv`/`read_excel` do column-type guessing on wide
  matrices; passing `col_types` and `show_col_types = FALSE` could trim it. Measure before bothering — inherent
  I/O may dominate.

**Risk:** Low. The dominant win (INPUT-1) is algorithmic with a same-file precedent; INPUT-2 is a pure hoist.
Gate the whole phase on a byte-identical `@rdesc`/`@cdesc`/`@mat` diff of the resulting GCT on a CSV+design
fixture before/after.

## Phase 6 — Export CSV + hygiene  **[ ] NOT IMPLEMENTED** (`tab_export.R` untouched; no `readr::write_csv`)
EXP-2 (`readr::write_csv` at 5 terminal sites) + EXP-4 temp-dir `on.exit(unlink)` cleanup + EXP-5 snapshot style
cleanup. Gate: confirm every swapped CSV is a terminal zip artifact never re-ingested; note the user-visible byte
change (quoting/exponent/NA-literal) in release notes.

## Phase 7 (conditional) — Server-side volcano refactor  **[ ] NOT IMPLEMENTED (deferred)**
STAT-03 → STAT-05 → STAT-01 → STAT-08 + EXP-6. **Ship ONLY if Phase 1's `toWebGL` proves insufficient on real
hardware.** Highest-effort/lowest-certainty; each item gated on its own byte-diff (Open Questions #6, #8, #9) and
a measured per-render saving. **Superseded in practice by Phase 8** — the PELSA-parity native rebuild below is the
concrete answer to "Phase 1 `toWebGL` insufficient," replacing the abstract Cluster F refactor.

## Phase 8 — Statistics-tab volcano → PELSA-parity (native scattergl + proxy labeling)  **[ ] NOT IMPLEMENTED (planned 2026-06-16)**

**Motivation.** Phase 1's STAT-02 only *wraps* the existing ggplot pipeline in `toWebGL()`. The base plot is
still built `ggplot -> ggplotly() -> add_volcano_labels() -> toWebGL()` **entirely inside one `renderPlotly`**
(`R/tab_stat_plot.R` ~L760-889). Consequences the user reports:
1. `ggplotly()` is the lossy/slow path and emits the recurring `Ignoring unknown aesthetics: text` warning
   (the `text` aes is for the hover tooltip; ggplotly drops it, toWebGL drops `hoveron`).
2. **Every label-mode toggle, top-N change, and point-click re-runs the WHOLE render** — rebuild ggplot,
   re-`ggplotly`, re-add labels, re-`toWebGL`. On the 34k-row phospho contrast this is the multi-hundred-ms
   "takes a while to refresh when a point is selected" symptom.

The PELSA volcano already solved this. Adopt its architecture for the Statistics-tab volcano. **Reference
implementation (do NOT re-invent — port it):**
- `pelsa_volcano_build_plot()` — `R/tab_pelsa_section3_helpers.R:516` — native `plot_ly()` +
  `add_trace(type = "scattergl", mode = "markers")`, built **label-free**. No ggplot, no ggplotly, no toWebGL.
- `pelsa_volcano_label_annotation_list()` / `pelsa_volcano_current_annotations()` —
  `R/tab_pelsa_section3_helpers.R:723` / `:789` — compute the COMPLETE annotation list for the current
  label settings; sent whole via relayout (a whole-list replace clears+redraws all labels in one call).
- `pelsa_volcano_gold_trace()` — `R/tab_pelsa_section3_helpers.R:467` — the selection/highlight overlay as a
  plain trace list ready for `plotlyProxyInvoke("addTraces", ...)`.
- Proxy wiring in `R/tab_pelsa_section3.R`: label relayout observer (`label_proxy` ~L818-852, applied on an
  `onFlushed` so relayout lands AFTER each render), gold-overlay observers (`gold_proxy` ~L760-809, addTraces /
  deleteTraces), and the click handler reading `event_data("plotly_click", source = ...)` (~L878).

### Items

- **SVOL-1 — Native scattergl base build (replaces ggplotly + STAT-02 toWebGL).**
  Add a `build_volcano_plotly()` helper (new, in `R/tab_stat_plot_helpers.R`) that takes the existing
  `df_plot` (from `build_volcano_df()`) and emits a native `plot_ly()`/`add_trace(scattergl)` figure built
  **label-free**, mirroring `pelsa_volcano_build_plot()`: separate traces for non-sig / up / down, hover via the
  `text` key on each trace (no `tooltip="text"` ggplotly hack), threshold lines via `layout(shapes=...)`.
  Delete the `ggplotly()` call (`tab_stat_plot.R:773`) and the `toWebGL()` wrap+warning-muffle block
  (`:872-886`). *Gate: scattergl click smoke (click$x/$y still map to a feature id via `get_clicked_feature_id`);
  visual parity screenshot vs current; PDF export path (`volcano_plot_export_function`) stays ggplot/SVG and is
  UNTOUCHED.*
- **SVOL-2 — Labels via `plotlyProxyInvoke("relayout", {annotations})` (no rebuild).**
  Pull label assembly OUT of `renderPlotly`. Port `add_volcano_labels()`'s output into a
  `volcano_label_annotations()` reactive returning the full annotation list (port
  `pelsa_volcano_current_annotations`), and a `label_proxy` observer that fires the whole-list relayout on every
  label-mode / top-N / POI / contrast change AND on first paint (via `session$onFlushed(..., once = TRUE)` so it
  lands after the render). Preserve all existing label semantics: union mode, `significant`/`significant_top20`
  mutual-exclusion, `hidden_label_count`, label-column/trim/split options. *Gate: toggling each label mode and
  changing top-N issues a relayout with NO `renderPlotly` re-entry (verify via reactlog / a render counter);
  label set is byte-identical to current `add_volcano_labels` output on the BRCA fixture.*
- **SVOL-3 — Selection highlight via overlay trace proxy (no rebuild on click).**
  Port `pelsa_volcano_gold_trace()` + the gold addTraces/deleteTraces observers so clicking a point (or POI
  change) pushes/removes a highlight scattergl trace through `plotlyProxy` instead of re-rendering. The base
  figure is built once and never rebuilt on click/clear. *Gate: shinytest2 — click a point, confirm highlight
  appears with zero base-figure re-render; clear restores; selection persists across a label-mode toggle.*

**Risk: Med.** Larger surface than Phase 1 and changes the interactive render backend, but it is a faithful PORT
of a shipped, tested module (the PELSA volcano), not new design. The PDF/SVG export path is explicitly out of
scope and untouched. **Supersedes the abstract Phase 7 (Cluster F) server-side refactor** — implement Phase 8
instead of Phase 7 if Phase 1's toWebGL proves insufficient.

**Files:** `R/tab_stat_plot.R` (render path L760-889), `R/tab_stat_plot_helpers.R` (new build/annotation/overlay
helpers, ported from `tab_pelsa_section3_helpers.R`). No DESCRIPTION change (`plotly` already imported).

---

## Blocked — dp-double  **[ ] BLOCKED**
Ships only after a forced row-id-mismatch test exercises the `repackage_transformed_gct_with_upload_rdesc`
fallback branch and a before/after `geneSymbol` byte-diff (ID-conversion ON+OFF) passes (Open Question #3). Would
then amplify Phase 2's dp-2a.

## Dropped  **[~] WON'T DO**
- **EXP-1** (single-pass GCT writer) — **dropped by decision: keep `cmapR::write_gct`.** The proposed writer
  was byte-identical and ~24× faster, but we are committed to staying on the cmapR package for GCT writing
  rather than maintaining a local writer that must track cmapR's ver=3 format. The per-row-`cat` export cost
  (~11s on a 2-ome export) is accepted. Export speedups now come solely from Phase 6 (CSV writer + hygiene).
- **STAT-04** (non-sig point thinning) — changes the displayed point set = a result change.
- **dp-2b** (de-quadratic merge loops) — <1ms at 2–4 omes; land only if it falls out of an unrelated edit.

---

*Status as of 2026-06-16: Phase 1 (INT-1/2/3, STAT-02, STAT-07) is implemented and merged. All other phases
remain proposals. Phase 8 (Statistics-tab volcano PELSA-parity) is newly added and not yet started.*
