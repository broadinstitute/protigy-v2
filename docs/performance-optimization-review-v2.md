# Protigy Performance Optimization Plan — Synthesized + Validated

> **Planning document only. NO code has been changed by this document.** Every item below is a *proposal to be decided on*, and carries an explicit result-preservation statement plus a post-validation verdict. The overriding constraint is that optimizations must **never** change analysis results (normalized values, filtered row sets, statistics, or displayed/exported data).
>
> Synthesized from 5 domain-reviewer reports (Startup/Setup, Intensity-toggle reactive chain, Core data-processing, Export/download, Statistics/volcano), then stress-tested by 3 adversarial validators (result-integrity, feasibility-correctness, scope-risk). Reviewers benchmarked against the BRCA fixtures (proteome 10.6k×77, phospho 34.4k×77). Validators grounded their verdicts in reading the actual functions plus targeted R experiments (70k+ fuzz cases for `fix_gene_symbols`; matrixStats vs base-R numeric equality; `write.csv` vs `readr` byte diff; `is.discrete` sort/no-sort equivalence; `cmapR::write_gct` internals).

---

## 0. Validation summary

- **The plan is unusually well-grounded.** Validators independently confirmed every load-bearing API and structural claim against the *installed* packages and the *actual* source: `cmapR::write_gct` really does one `cat(append=TRUE)` syscall per row (24x win is real, not a microbenchmark artifact); the Intensity-toggle's `collectInputs()` really forces the full `renderUI` rebuild; `matrixStats::colMads` default `constant=1.4826` matches `stats::mad`; `matrixStats` is installed but **undeclared** in DESCRIPTION; `furrr`/`future`/`WriteXLS` are genuinely dead. The prior plan's microbenchmark mirages (EXP-3/4/5, START-05, furrr) stay correctly refuted.
- **Cross-cutting guard — every vectorization/writer swap needs a byte-identical-output regression test on the BRCA fixture *before merge*, not just `all.equal`.** This is the non-negotiable gate. Specifically: numeric filters (dp-sd) need a **filtered-row-set** diff (not `all.equal` on the SD vector, which is TRUE but not `identical` — 2.2e-16 drift could in principle flip a near-boundary row); the GCT writer (EXP-1) needs a **byte diff vs `cmapR::write_gct`** including a synthetic GCT with numeric/NA rdesc columns and scientific-notation matrix values; gene-symbol swaps (dp-2a/dp-double) need a **`geneSymbol`-column diff** on the fixture.
- **Three labels were materially wrong in the draft and are corrected here.** (1) **EXP-2** (`readr::write_csv`) is *not* byte-identical to `write.csv` — it changes quoting, scientific-notation format, and float sig-digits; downgraded None → **safe-with-caveat** (safe only because these CSVs are terminal zip artifacts never re-ingested; external user pipelines parsing the bytes could differ). (2) **dp-sd** (`rowSds`) downgraded None → **safe-with-caveat** for the filtered-row-set reason above. (3) **INT-1 Option A** (bare `collectInputs()` removal) is **not value-preserving** per the result-integrity validator: the observer reads `parameters$data_normalization`/`parameters$max_missing` *from* the reactiveVal that `collectInputs()` populates, so dropping it can seed the updated widgets from stale pre-toggle state in an edit-then-toggle sequence. **Use Option B (guarded `identical()` write) or read live widget values** — see INT-1.
- **One finding is blocked, one is dropped on result grounds.** **dp-double** is moved to *Blocked pending guards*: it is the highest result-blast-radius data-processing item — skipping `transformGCTs`' gene-symbol work is only safe when `repackage_transformed_gct_with_upload_rdesc` overwrites the exported `geneSymbol`; the **rids-mismatch fallback branch** (`strip_gene_symbol_mapping_columns`) must be explicitly exercised before it can ship. **STAT-04** (non-sig point thinning) stays *Dropped* — it changes the displayed point set (a result change) and `toWebGL` already removes the jank.
- **Volcano cluster is re-prioritized.** **STAT-02 (`toWebGL`) does almost all the real perceived-perf work** and is *lower* risk than the plan stated — `get_clicked_feature_id` matches purely on `click$x`/`click$y` (never `curveNumber`/`pointNumber`/`customdata`), so the SVG→scattergl switch is essentially immune. The server-side volcano items (**STAT-01, STAT-03, STAT-05, STAT-08, EXP-6**) are "tens of ms on 34k rows" wins layered on a path whose dominant cost moves client-side after STAT-02; all are **downgraded to "ship only if `toWebGL` proves insufficient."** **dp-2b** is **dropped** (validators: <1ms at 2–4 omes, not worth review attention).
- **Strongly-verified GOOD (keep as-is):** dp-2a faithful `fix_gene_symbols` byte-identical over **70k+** adversarial fuzz cases incl. internal-whitespace tokens; `colMedians`/`colMads` bit-identical to base; dp-1b `rowMeans(is.na)` identical; INT-3 sort-drop genuinely result-identical (NA arithmetic self-cancels at the cutoff); START-01 lazy-load changes only attach timing; START-02 dead-import removal cannot change output.

---

## 1. Executive summary — highest-leverage changes (post-validation)

| # | Change | One-line impact | Validated result-risk |
|---|--------|-----------------|------------------------|
| 1 | ~~**Single-pass GCT writer** replacing cmapR's row-by-row `cat(append=TRUE)` (EXP-1)~~ — **DROPPED BY DECISION: keep `cmapR::write_gct`** | ~~~11s → ~0.2s; 24x per file~~ — not pursued; export stays on cmapR | **n/a** — writer was byte-identical but we are committed to the cmapR package; see §6 |
| 2 | **Decouple Intensity-toggle from full-panel rebuild** (INT-1) | Eliminates ~630ms (phospho) / ~206ms (proteome) grey-out on every checkbox toggle | **Low w/ guard** — use Option B (guarded write) or live-widget read, **not** bare Option A; shinytest2 edit-then-toggle |
| 3 | **Cache / cheapen `is.discrete` row-filter scan** (INT-2 + INT-3) | Removes ~616ms of the ~630ms panel rebuild; `sort()` drop is provably result-identical | **Low** (cache, key on `GCTs_unprocessed`) / **None** (drop `sort()`) |
| 4 | **Lazy-load normalization-only deps** (vsn/mixtools/mclust/preprocessCore) (START-01) | ~0.95–1.4s+ off cold app-open critical path; vsn pulls Biobase/affy | **None** — same calls, lazy load timing only |
| 5 | **Faithful vectorized `fix_gene_symbols`** (dp-2a) | ~486ms → ~24ms per pass (~20x); largest *safe* data-processing win | **Low** — fuzz-verified 70k+ cases byte-identical; *plan's prior version was NOT byte-identical* |
| 6 | **`toWebGL` on the 34k-point volcano** (STAT-02) | GPU rendering removes browser pan/zoom/hover jank; identical points/labels; does nearly all the real perceived-perf work | **Low** — keep-as-is; click handler is coordinate-only so essentially immune |
| 7 | **Vectorize row/col stats** — `rowMeans` (dp-1b), `matrixStats::rowSds` (dp-sd), `colMedians/colMads` (dp-norm) | ~45ms + ~135ms + ~50–100ms saved per ome per pass | **None / Low** — dp-sd needs **filtered-row-set** diff (not just `all.equal`); dp-norm needs NA-parity + Inf/NaN fixture diff |

> **dp-double** (stop running gene-symbol work twice per ome) — the largest *raw* data-processing win — is **Blocked pending guards** (§6) because of the repackage fallback branch. **STAT-01** (cache volcano base plot) is **downgraded** to ship only if STAT-02 is insufficient.

Remove dead imports (`furrr`/`future`/`WriteXLS`, START-02) as a free cleanup bundled with #4.

---

## 2. User-reported symptoms → root causes

### Symptom A — "App opening is slow"
- **START-01 (primary):** NAMESPACE eagerly attaches normalization-only deps (vsn ~0.49s incl. Biobase/affy chain, mixtools ~0.46s, mclust, preprocessCore) before the UI renders, even though none run until Submit. ~0.95–1.4s+ of the cold-open path. *Validated: attach-timing only, RNG unaffected.*
- **START-02 (secondary):** Dead imports `furrr`/`future`/`WriteXLS` attach (~0.12s) but are never called. *Validated dead via grep across R/ and tests/.*
- **Not the cause (refuted):** Eager UI/help-markdown building — measured at only ~85ms (START-05). The dominant cost is package attach (~5–7s mandatory Bioconductor/CRAN stack), most of which is unavoidable; the actionable slice is the deferrable normalization deps.

### Symptom B — "Intensity data page greys out / freezes for several seconds"
- **INT-1 (root cause of the invalidation):** Toggling the checkbox fires an `observeEvent` whose first action is `collectInputs()`, which unconditionally calls `parameters_internal_reactive(new_parameters)`. `output$sideBarMain` is a `renderUI` reading that reactiveVal, so the whole `gctSetupUI` panel rebuilds and greys out — even though only two widgets actually need updating. *Root cause verified by all three validators.*
- **INT-2 / INT-3 (why the rebuild is slow):** ~616ms of the ~630ms rebuild is the row-filter-column discovery running `is.discrete()` over all 25 rdesc columns; `is.discrete` does a needless `sort(unique())` + five `grepl` passes on high-cardinality string columns (~34k unique). rdesc is immutable during setup, so this recomputes identically every rebuild.
- Either fix alone removes most of the lag; together they eliminate it.

### Symptom C — "Slow file processing" (Submit path)
- **dp-2a + dp-double (primary):** `fix_gene_symbols` (~486ms/pass on phospho) runs **twice per ome** (`processGCTs` + `transformGCTs`), and the transform pass's result is largely discarded by `repackage_transformed_gct_with_upload_rdesc`. With ID conversion on, a redundant `AnnotationDbi::mapIds` round-trip (can be seconds) also runs twice. *dp-2a validated safe; dp-double blocked on the fallback branch — see §6.*
- **dp-norm (default-method hot cost):** per-column `apply` median/MAD normalization (~80ms / ~134ms) vectorizes via `matrixStats::colMedians/colMads`.
- **dp-sd:** StdDev filter `apply(tab,1,sd)` (~141ms) → `matrixStats::rowSds` (~7ms). *Validator caveat: `rowSds` is `all.equal` but not `identical` — verify the filtered row set, not just the SD vector.*
- **dp-1b:** missing-value filter `apply(,1,...)` (~48ms) → `rowMeans(is.na())` (~4ms), plus a `drop=FALSE` crash bugfix.
- **dp-1a:** `serialize/unserialize` deep copy (~9ms) → `as.data.frame` round-trip (~0ms).
- **START-04 (setup-submit I/O):** each `.gct` read twice — `parse_gctx` + a full-file `readLines` that only needs the header region. *Validator: benefit unquantified — needs-benchmark on warm vs cold cache.*

### Symptom D — "Slow multi-omics export"
- **EXP-1 (dominant, ~75%+ of export time):** `cmapR::write_gct` writes one append-syscall per matrix row. The summary tab writes both original and processed GCTs per ome: ~2.6s (proteome) + ~8.8s (phospho) = ~11.4s for a 2-ome run, 20–35s for 4 omes. A single-pass `writeLines` writer is 24x faster.
- **EXP-2 (secondary):** `utils::write.csv` → `readr::write_csv` (~4.3x; ~1–3s on a 4-ome run). *Validator: NOT byte-identical — quoting/exponent/sig-digit differences; safe only because these CSVs are terminal artifacts. Downgraded to safe-with-caveat.*
- **EXP-6 (minor, downgraded):** Volcano PDF export rebuilds `build_volcano_df` redundantly in union mode.
- **Not the cause (refuted):** zip compression level (EXP-3, ~0.6s at 154MB), double reactive read (EXP-4, closures not data), `stat_results` memoization (EXP-5, already a reactiveVal).

---

## 3. Ranked findings table (re-sorted post-validation)

Sorted by criticality (impact × certainty ÷ risk), adjusted for validator verdicts. "Validation" column = consensus recommendation. Items the validators flagged **risky / breaks-results** are pulled into §6.

| Rank | ID | Title | File : Function | Symptom | Expected Impact | Validated Risk | Validation verdict |
|------|----|-------|-----------------|---------|-----------------|----------------|--------------------|
| — | EXP-1 | ~~Single-pass GCT writer vs row-by-row `cat`~~ | tab_summary.R : GCT_*_export_function | Slow export | ~11.4s→~0.3s (2-ome) — **not pursued** | n/a | **DROPPED BY DECISION** — keep `cmapR::write_gct` (§6) |
| 2 | INT-1 | Intensity toggle forces full panel rebuild | sidebar_setup.R : current_intensity observeEvent | Page grey-out freeze | −630ms/−206ms per toggle | Low (use Option B) | **keep-with-guard** — guarded write / live-widget read, not bare Option A |
| 3 | STAT-02 | `toWebGL` for 34k-point volcano | tab_stat_plot.R : volcano renderPlotly | Sluggish volcano | GPU render, jank removed | **Low (lower than stated)** | **keep-as-is** — coordinate-only click handler, immune |
| 4 | INT-2 | `is.discrete` O(rows) scan on every rebuild | sidebar_setup_helpers_shiny.R : gctSetupUI | Panel rebuild slow | −616ms of 630ms rebuild | Low (safe-with-caveat) | **keep-with-guard** — cache key on `GCTs_unprocessed` identity |
| 5 | dp-2a | Faithful vectorized `fix_gene_symbols` | GCT-processing.R : fix_gene_symbols | Slow processing | 486ms→24ms (~20x) | Low | **keep-with-guard** — fixture geneSymbol diff + list-col/UTF-8 fuzz |
| 6 | INT-3 | `is.discrete` drops needless `sort()` | utilities.R : is.discrete | Panel/processing slow | −30–50% per call | None | **keep-as-is** |
| 7 | START-01 | Lazy-load vsn/mixtools/mclust/preprocessCore | protigy-package.R / normalization helper | Slow app open | −0.95–1.4s+ cold open | None | **keep-as-is** — keep all four in Imports; document() |
| 8 | dp-sd | `apply(,1,sd)` → `matrixStats::rowSds` | data-filtering.R : sd.filter | Slow processing | 141ms→7ms | Low (safe-with-caveat) | **keep-with-guard** — filtered-row-set diff, not just `all.equal` |
| 9 | dp-norm | `colMedians/colMads` normalization | normalization.R : normalize.data.helper | Slow processing | Median 80→30ms; MAD 134→95ms | Low | **keep-with-guard** — NA/Inf/NaN fixture diff; preserve dimnames |
| 10 | dp-1b | `rowMeans(is.na())` + `drop=FALSE` bugfix | GCT-processing.R : perform_missing_filter | Slow processing | 48ms→4ms; fixes 1-row crash | None | **keep-as-is** — add 1-row & 0-row regression test |
| 11 | EXP-2 | `readr::write_csv` over `utils::write.csv` | tab_stat_summary.R + 3 others | Slow export | 270ms→63ms per table (~4.3x) | **Low (safe-with-caveat, was None)** | **keep-with-guard** — confirm terminal; note byte change in release notes |
| 12 | dp-1a | `as.data.frame` deep copy vs serialize | GCT-processing.R : df_deep_copy | Slow processing | 9ms→0ms per copy | None | **keep-as-is** — guard/fallback if any list-column appears |
| 13 | START-02 | Remove dead `furrr`/`future`/`WriteXLS` | protigy-package.R / DESCRIPTION | Slow app open (minor) | −0.12s + smaller dep surface | None | **keep-as-is** — re-grep R/ + tests/; document() |
| 14 | dp-redundant | Drop double `safe_copy_rdesc` | GCT-processing.R : apply_gene_symbol_from_params | Slow processing (minor) | −1 rdesc copy on ID path | Low | **keep-with-guard** — assert backup unchanged on failure path |
| 15 | START-04 | Bound second `.gct` read to header region | GCT-processing.R : parse_gctx_preserve_cdesc | Slow processing/setup | Avoids 2nd full-file scan | Low | **needs-benchmark** — warm/cold cache; byte-identical cdesc test |
| 16 | START-03 | Observer accumulation on file add/remove | sidebar_setup.R : bare observe() L396 | Sidebar slowdown (long sessions) | Bounded observer count | Low | **keep-with-guard** — shinytest2 add/remove/re-add/clear |
| 17 | STAT-07 | Latent `results_list` shadowing rename | tab_stat_setup_helpers.R : two-sample branch | Correctness (masked) | Unblocks multi-ome call | None | **keep-as-is** — assert single-ome output byte-identical |
| 18 | STAT-03 | Volcano builds plotting df twice/render | tab_stat_plot.R : volcano renderPlotly | Slow on input change | Halves per-render grep/copy | Low | **downgrade-priority** — ship only after STAT-02; byte-diff the two df paths |
| 19 | STAT-05 | Narrow wide stat df before plotting | tab_stat_setup_helpers.R / plot path | Slow volcano render | ~100→~6–8 col scans | Low | **downgrade-priority** — confirm selector/search read full df |
| 20 | STAT-01 | Cache volcano base; re-apply labels only | tab_stat_plot.R : volcano renderPlotly | Slow on label tweaks | ~0.4s → near-instant per tweak | Low (unverified) | **downgrade-priority** — highest-effort/lowest-certainty; ship only if STAT-02 insufficient |
| 21 | EXP-6 | Dedup `build_volcano_df` in volcano PDF export | tab_stat_plot.R : volcano_plot_export_function | Slow export (minor) | −1 df build/contrast (union) | Low | **needs-benchmark** — system.time a 3-contrast export first |
| 22 | STAT-08 | bindCache p-value histograms | tab_stat_summary.R : *_pval_hist_plot | Re-render on revisit (minor) | Avoids recompute on revisit | Low | **downgrade-priority** — key must capture contrast + stat_results token; low value |
| — | dp-double | Gene-symbol mapping runs twice per ome | GCT-processing.R : processGCTs/transformGCTs | Slow processing | Halves dominant cost; −1 mapIds/ome | **Risky** | **BLOCKED** — see §6 (repackage fallback branch) |
| — | dp-2b | De-quadratic merge loops | GCT-processing.R : merge_processed_gcts | (negligible at 2-4 omes) | <1ms; clarity only | None | **DROP** — see §6 (not worth review attention) |
| — | STAT-04 | Non-sig point thinning (deferred) | tab_stat_plot_helpers.R | Volcano payload | 0.40→0.23s | **Breaks-results** | **DROP** — see §6 (changes displayed point set) |
| — | EXP-3/4/5 | Zip level / double-read / memoize (refuted) | tab_export.R / tab_stat_summary.R | (false positives) | ~0 | None | **keep-as-is (refuted)** — retain only `on.exit(unlink)` |
| — | START-05 | Eager UI/markdown build (refuted) | app_ui.R | (false lead) | ~85ms, no action | None | **drop** |
| — | STAT-06 | eBayes already vectorized (no-op) | tab_stat_setup_helpers.R : stat.testing | (none) | Redirects effort | None | **keep-as-is (refuted)** |

---

## 4. Findings grouped by common module / function (PR clustering)

### Cluster A — `R/sidebar_setup_helpers_GCT-processing.R` (data-processing hot path)
The single highest-density file. Shared root causes: **apply-over-rows**, **repeated deep-copies**, and **gene-symbol work done twice**.
- dp-2a (faithful vectorized `fix_gene_symbols`) — **keep-with-guard**
- dp-double (eliminate duplicate gene-symbol/mapIds pass) — **BLOCKED (§6)**; *would amplify dp-2a once unblocked*
- dp-1b (`rowMeans` + `drop=FALSE`) — **keep-as-is**
- dp-1a (`as.data.frame` deep copy) — **keep-as-is**
- dp-redundant (drop double `safe_copy_rdesc`) — **keep-with-guard**; pairs with dp-1a
- ~~dp-2b (merge loop restructure)~~ — **DROPPED (§6)**
- START-04 (bound the second `readLines` to header region) — **needs-benchmark**

### Cluster B — Intensity-toggle reactive chain + `is.discrete`
Shared root cause: an unnecessary full-panel `renderUI` invalidation amplified by an expensive O(rows) scan.
- INT-1 (`R/sidebar_setup.R` — decouple toggle; **Option B / live-widget read**, not bare Option A)
- INT-2 (`R/sidebar_setup_helpers_shiny.R` — cache the per-ome discrete-column vector; key on `GCTs_unprocessed`)
- INT-3 (`R/utilities.R` — drop `sort()` in `is.discrete`; also benefits GCT-processing.R:933/958 and tab_customize_helpers.R:496)

### Cluster C — Normalization & filtering numerics + `matrixStats` adoption
Shared root cause: **per-column / per-row `apply` closures** replaceable by single C-pass `matrixStats`. **All three require adding `matrixStats` to DESCRIPTION Imports + a roxygen `@importFrom` (currently installed but undeclared — validators confirmed it is absent from DESCRIPTION/NAMESPACE), then `devtools::document()`.**
- dp-norm (`colMedians`/`colMads`, `R/sidebar_setup_helpers_normalization.R`)
- dp-sd (`rowSds`, `R/sidebar_setup_helpers_data-filtering.R`) — **needs filtered-row-set diff**
- (dp-1b's `rowMeans` is base R, no matrixStats needed, but lands naturally with this theme)

### Cluster D — Package attach surface (`R/protigy-package.R` + DESCRIPTION + normalization roxygen)
Shared root cause: eager `importFrom` forcing namespace attach at launch.
- START-01 (lazy `pkg::fn()` for vsn/mixtools/mclust/preprocessCore) — keep all four in Imports
- START-02 (remove dead `furrr`/`future`/`WriteXLS`)
- Both require one `devtools::document()` regen of NAMESPACE.

### Cluster E — Export writers (`R/tab_summary.R`, `R/tab_stat_summary.R`, `R/tab_qc_cv.R`, `R/utilities.R`)
Shared root cause: slow R-level serializers in the download path.
- EXP-1 (single-pass GCT writer in `R/utilities.R`, called from 4 sites: tab_summary.R L341/349, tab_stat_summary.R L707, tab_qc_cv.R L441)
- EXP-2 (`readr::write_csv` at 5 sites: tab_stat_summary.R L692/L815, tab_qc_cv.R L383/L409, tab_stat_plot.R L1178) — **safe-with-caveat**
- EXP-4 (temp-dir `on.exit(unlink(...))` cleanup — memory hygiene, bundle here)
- EXP-5 (`df <- stat_results()[[ome]]` snapshot — style cleanup, bundle with EXP-2)

### Cluster F — Volcano / statistics render path (`R/tab_stat_plot.R`, `R/tab_stat_plot_helpers.R`, `R/tab_stat_setup_helpers.R`)
Shared root cause: full re-render on label-only changes + redundant df builds + SVG rendering of 34k points + a wide-df carried through the plot reactive. **Validator consensus: STAT-02 does nearly all the real work; the rest are server-side microbenchmark-tier wins to ship only if STAT-02 is insufficient.**
- STAT-02 (`toWebGL`) — **ship first**, lowest risk, biggest perceived win
- STAT-01 (cache base plot, re-apply labels) — **downgraded** (structural, highest-effort)
- STAT-03 (single column-resolve + df build per render) — **downgraded**
- STAT-05 (narrow the wide stat df before plotting) — **downgraded**
- STAT-08 (bindCache p-value histograms) — **downgraded** (lowest value)
- EXP-6 (volcano PDF export df dedup — same helper family) — **needs-benchmark**
- STAT-07 (rename shadowed `results_list` — correctness, ship with this cluster)

---

## 5. Detailed findings

> Each finding ends with a **Validation verdict** capturing the consensus across the three validators: result-preservation status, the REQUIRED guard/test before implementation, benchmark credibility, and the final recommendation.

### EXP-1 — Single-pass GCT writer (replaces cmapR row-by-row `cat`) — **DROPPED BY DECISION (keep cmapR)**
> **Decision (2026-06-13):** Not pursued. We keep `cmapR::write_gct` for GCT writing even though the proposed
> single-pass writer was validated byte-identical and ~24× faster. Rationale: stay on the maintained cmapR
> package rather than carry a local writer that must track cmapR's ver=3 format. The per-row-`cat` export cost
> is accepted; export speedups come only from EXP-2 (CSV). The analysis below is retained as the rationale
> record but is **not** to be implemented.

**Location:** `R/tab_summary.R` L341/349 (`GCT_original_export_function` / `GCT_processed_export_function`); also `tab_stat_summary.R` L707, `tab_qc_cv.R` L441. Underlying cost in `cmapR::write_gct`.
**Root cause:** `cmapR::write_gct` writes the matrix with `for (ii in seq_len(nr)) cat(paste(...), file=ofile, append=TRUE)` — one open/write/close append syscall per row.
**Fix:** Add a local single-pass writer in `R/utilities.R` that builds header + meta lines, rounds *only the matrix* with `round(m, precision)` (precision=4) exactly as cmapR does, assembles the body via `paste(..., sep="\t")` over columns, and emits with one `writeLines()` to a single open connection. Match cmapR ver=3 format byte-for-byte (id-column handling, `"na"` filler rows for cdesc, column ordering). Call from all four sites.
**Benchmark:** single-pass `writeLines` vs `cmapR::write_gct` on 34.4k×4: **1.512s → 0.063s (24x)**. Full-width 2-ome export: ~11s of writing removed. *Validated credible at fixture scale — genuinely O(rows) syscalls, not a microbenchmark artifact.*
**Result-preservation:** Output must be **byte-identical** to `cmapR::write_gct` ver=3.
**Validation verdict — keep-with-guard (safe-with-caveat).** Validators read the cmapR ver=3 source and surfaced two traps the draft prose understated: **(a)** cmapR builds each row as `paste(c(rid, rdesc[ii,], round(m[ii,],4)))` — `rdesc[ii,]` becomes a list and `as.character` renders **rdesc numeric columns at FULL precision (NOT rounded)**; a vectorized writer that rounds rdesc or uses `format()` diverges. **(b)** Numbers must be stringified via `as.character`/`paste` (`'-1e-04'`, `'1e+20'`, `'NA'`), **never** `format()`/`formatC()` (which pad/align column-wise and change scientific notation). Also reproduce header trailing-newline, `na` filler rows (numeric cdesc rounded, character cdesc verbatim), id-column drop, and column order. **REQUIRED guard:** blocking regression test that diffs the new writer's bytes against `cmapR::write_gct(ver=3, precision=4, appenddim=FALSE)` on BOTH BRCA proteome and phospho original+processed GCTs, **AND** on a synthetic GCT whose rdesc contains numeric/integer/logical/NA columns plus matrix values forcing scientific notation (1e-7, 1e20, exact-.00005 rounding) — assert `identical(readBin(...))` and explicitly assert rdesc numeric columns are written **unrounded**. (Open Question #1, blocking.)

### INT-1 — Intensity toggle forces full setup-panel rebuild
**Location:** `R/sidebar_setup.R` — `current_intensity()` observeEvent (L767), `collectInputs()` (L769→1274 `parameters_internal_reactive(new_parameters)`), `output$sideBarMain` renderUI (L724–730), which reads `parameters$data_normalization` (L788) and `parameters$max_missing` (L805).
**Root cause:** The observer's first statement is `collectInputs()`, which unconditionally writes the `parameters_internal_reactive` reactiveVal. The `sideBarMain` renderUI reads that reactiveVal, so the write invalidates and rebuilds the whole `gctSetupUI` panel — yet the observer already directly updates the only two affected widgets via `updateSelectInput`/`updateNumericInput`.
**Fix (preferred — Option B / live-widget read, NOT bare Option A):** Guard the reactiveVal write with `if (!identical(new_parameters, isolate(parameters_internal_reactive()))) ...`, **or** have the observer read live widget values via `input[[paste0(label,'_data_normalization')]]` / `input[[paste0(label,'_max_missing')]]` instead of the reactiveVal. Verify with reactlog that toggling no longer invalidates `output$sideBarMain`.
**Benchmark:** `gctSetupUI` rebuild eliminated from the toggle path: **~630ms (phospho) / ~206ms (proteome)** per toggle, plus the renderUI round-trip and client re-render.
**Validation verdict — keep-with-guard. SPLIT VERDICT, resolved conservatively.** The feasibility and scope validators read the navigation path and judged **Option A** (bare `collectInputs()` removal) **safe** because `intensity_data` is re-collected on Next/Back/Submit, so the *submitted* value is identical. **However, the result-integrity validator flagged Option A as risky:** the observer reads `parameters$data_normalization`/`parameters$max_missing` **from** the reactiveVal that `collectInputs()` populates, so dropping it makes the update seed the widgets from **stale pre-toggle state** in an *edit-normalization-then-toggle* sequence — which then becomes the persisted normalization config and flows into normalized values. **Consensus: do NOT ship bare Option A; use Option B (guarded `identical()` write) or read live widget values directly.** **REQUIRED guard:** shinytest2 — change normalization, toggle intensity twice, submit, assert submitted `parameters$data_normalization` and `max_missing` are identical to current behavior; reactlog check that toggling no longer invalidates `output$sideBarMain`.

### INT-2 — `is.discrete` O(rows) row-filter scan on every rebuild
**Location:** `R/sidebar_setup_helpers_shiny.R` L108–116 (`row_filter_columns_choices`); `is.discrete` at `R/utilities.R` L171.
**Root cause:** `all_rdesc_columns[vapply(..., is.discrete, logical(1))]` calls `is.discrete` on every rdesc column; high-cardinality string columns (~34k unique each) cost 70–110ms each → ~616ms of the ~630ms rebuild. rdesc is immutable during setup.
**Fix:** (1) Drop `sort()` from `is.discrete` (see INT-3). (2) **Cache** the per-ome discrete-column vector once after parse and reuse it in `gctSetupUI`.
**Benchmark:** Caching removes the ~616ms scan entirely → `gctSetupUI` rebuild ~15–30ms. *The ~616ms-of-630ms attribution was not independently re-profiled by validators (no profiler run); the mechanism is sound but the magnitude is unverified.*
**Validation verdict — keep-with-guard (safe-with-caveat).** Result-identical *as long as the cache is invalidated whenever `GCTs_unprocessed` change*. Risk is purely a stale-cache wiring bug (e.g., cache survives a re-upload / file removal that changes rdesc), not a numeric risk. **REQUIRED guard:** key the cache on a token derived from `GCTs_unprocessed` (e.g., per-ome rdesc column-name + nrow signature), **not** merely the label; add a shinytest2 that uploads, removes, and re-uploads a file with different rdesc columns and asserts the row-filter column choices update correctly.

### INT-3 — `is.discrete` sorts unique values unnecessarily
**Location:** `R/utilities.R` L175 (`annot_vals <- sort(unique(annot_col))`).
**Root cause:** The return value depends only on the count of unique values and whether they are all-numeric — both order-independent. The `sort()` of up to 34k strings is dead work.
**Fix:** Replace with `annot_vals <- unique(annot_col)`. Optionally add an early return: if `length(annot_vals) <= nfactor_cutoff` return `TRUE` immediately.
**Benchmark:** ~30–50% reduction on high-cardinality calls; benefits every call site.
**Validation verdict — keep-as-is (safe).** Verified empirically by the result-integrity validator: `sort()` drops NA whereas `unique()` keeps it (changing `annot_vals`), **but** the return depends on `(length − n_na)` and `is_numeric`, and the `NA→'NA'` string is counted in BOTH `length` and `n_na`, so the arithmetic **self-cancels**. Tested numeric+NA, numeric-noNA, the exact 20-vs-21 cutoff boundary, factor path, and string+NA: sort and no-sort give identical TRUE/FALSE in every case. The optional `<= cutoff` early-return matches the existing branch. Pure speedup, no guard required.

### dp-2a — Faithful vectorized `fix_gene_symbols`
**Location:** `R/sidebar_setup_helpers_GCT-processing.R` L33–91.
**Root cause:** Per-row `vapply(strsplit/trimws/paste)` plus per-element substring loops for pipe trimming — the single most expensive string step. **The prior plan's proposed `gsub("\\s*\\|\\s*","|")` + `trimws()` is NOT result-preserving** (it trims whitespace adjacent to pipes and at string edges; `"EGFR | KRAS"` → plan yields `"EGFR|KRAS"`).
**Fix (faithful):**
```r
gs <- gsub(";", "|", gs)
repeat { g2 <- gsub("\\|[[:space:]]*\\|", "|", gs); if (identical(g2, gs)) break; gs <- g2 }
gs <- sub("^[[:space:]]*\\|", "", gs)
gs <- sub("\\|[[:space:]]*$", "", gs)
gs[!is.na(gs) & (gs == "" | (!grepl("\\|", gs) & trimws(gs) == ""))] <- NA
```
**Benchmark:** 34.4k symbols: **~486–497ms → 21–24ms (~20x)**.
**Validation verdict — keep-with-guard (safe-with-caveat).** The result-integrity validator extracted the current per-string logic and the proposed faithful version and ran **70k+ comparisons** — 21 targeted edge cases (leading/trailing/double pipes, semicolons, whitespace-only parts, tabs/newlines) + 20k generic + 50k internal-whitespace-token fuzz cases (`'A B'`, `' X '` surrounded by `' | '`, `'| '`, `'; '`). **ZERO diffs.** The claim that the prior version was non-faithful is also confirmed. "with-caveat" only because the original is intricate and **list-column / non-ASCII paths weren't exhaustively fuzzed**, and the leading/trailing strip allows leading whitespace where the original substring check did not (in practice the part-dropping step removes such tokens first). **REQUIRED guard:** run `test-gct-processing.R` / `test-id-to-gene-symbol.R` and diff the `geneSymbol` column against the current implementation on the BRCA fixture rdesc; add the **list-column input path** and a **non-ASCII/UTF-8 gene-symbol** case to the fuzz suite; the corpus MUST explicitly include `' |EGFR'`, `'EGFR| '`, `' | EGFR | '`. (Open Question #4.)

### START-01 — Lazy-load normalization-only deps
**Location:** `R/sidebar_setup_helpers_normalization.R` roxygen `@importFrom`; `R/protigy-package.R`. NAMESPACE: `importFrom(vsn,justvsn)`, `importFrom(mixtools,normalmixEM)`, `importFrom(mclust,Mclust,mclustBIC)`, `importFrom(preprocessCore,normalize.quantiles)`.
**Root cause:** `@importFrom` forces those namespaces to attach at `library(Protigy)`/`load_all` time, though they only run inside `normalize.data` during post-startup processing.
**Fix:** Remove the four `@importFrom` tags; call fully-qualified at use sites (`vsn::justvsn`, `mixtools::normalmixEM`, `mclust::Mclust`/`mclust::mclustBIC`, `preprocessCore::normalize.quantiles`). **Keep them in DESCRIPTION Imports** (do not move to Suggests). Re-run `devtools::document()`.
**Benchmark:** vsn 0.489s (pulls Biobase/affy/BiocGenerics), mixtools 0.462s, mclust 0.004s, preprocessCore ~0s → **~0.95–1.4s+** off the cold-open path.
**Validation verdict — keep-as-is (safe).** All three validators concur: switching to `pkg::fn()` changes only namespace-attach timing; identical function objects and arguments. `normalmixEM`'s RNG stream is a property of the function+package, **not** of attach timing, so results are unaffected. Progress-bar correctness unaffected (calls already run inside the post-startup normalization path). **REQUIRED guard:** grep for bare `justvsn`/`normalmixEM`/`Mclust`/`mclustBIC`/`normalize.quantiles` and convert ALL to `::`; confirm no use site relies on an attached generic/S4 method dispatch from these packages being on the search path; re-run `devtools::document()` + `devtools::check()` to catch undeclared-import notes.

### dp-sd — `apply(tab,1,sd)` → `matrixStats::rowSds`
**Location:** `R/sidebar_setup_helpers_data-filtering.R` L28 (`sd.filter`).
**Root cause:** `apply(tab,1,sd,na.rm=T)` dispatches an R `sd()` closure per row.
**Fix:** `sd.tab <- matrixStats::rowSds(tab, na.rm=TRUE)`. Add `matrixStats` to DESCRIPTION + `@importFrom matrixStats rowSds` + `document()`.
**Benchmark:** 34.4k×77: **141.5ms → 7ms**.
**Validation verdict — keep-with-guard (safe-with-caveat, downgraded from None).** `rowSds` is `all.equal` but **NOT `identical`** to `apply(,1,sd)` (max abs diff 2.2e-16). `sd.filter` computes a quantile threshold from the same SD vector then filters with strict `<` / `>=`, so a row whose SD sits within ~1e-16 of the threshold could **in principle flip** in/out of the set-to-NA set. The result-integrity validator **could not induce a flip in 5200 trials** (incl. 1e-9 tie clusters), so the risk is vanishingly small but **not provably zero**. **REQUIRED guard:** add a **filtered-row-set diff** (`identical(sort(filt.idx))`) between `apply(,1,sd)` and `rowSds` on BOTH BRCA fixtures at the actual `sd.perc` used — not merely `all.equal` on the SD vector. If any mismatch ever appears, retain `apply` for `sd.filter`.

### dp-norm — Vectorize Median / Median-MAD normalization
**Location:** `R/sidebar_setup_helpers_normalization.R` L143–178 (Median / Median (non-zero) / Median-MAD / Median-MAD (non-zero) branches).
**Root cause:** `apply(data,2,function(x) x-median(x,na.rm=T))` — an R closure per column plus R-level median/mad.
**Fix:** `med <- matrixStats::colMedians(data, na.rm=TRUE); data.norm <- sweep(data, 2, med, "-")`. For MAD: `mads <- matrixStats::colMads(data, na.rm=TRUE)` and divide. Preserve colnames via `safe_set_colnames`. Add `matrixStats` to DESCRIPTION + `@importFrom matrixStats colMedians colMads`.
**Benchmark:** Median **80ms → 30ms** (runs in both passes ⇒ ~100ms/submit), Median-MAD **134ms → 95ms**.
**Validation verdict — keep-with-guard.** The result-integrity validator verified `colMedians`/`colMads` are **BIT-identical** (maxdiff 0) to `apply(,2,median/mad)` on NA-bearing data, and `colMads` default `constant=1.4826` matches `stats::mad` exactly — this resolves the draft's blocking Open Q#2 in the affirmative. Only residuals: must preserve colnames and match the original's `na.rm=TRUE`. **REQUIRED guard:** assert `identical(dimnames)` after `sweep`, and a fixture-level diff of the normalized matrix for all four branches (Median, Median(non-zero), Median-MAD, Median-MAD(non-zero)) **including an all-NA column** and a **zero-MAD** column (confirm identical Inf/NaN propagation). (Open Question #2.)

### dp-1b — `rowMeans(is.na())` + `drop=FALSE` bugfix
**Location:** `R/sidebar_setup_helpers_GCT-processing.R` L1075–1079 (`perform_missing_filter`).
**Root cause:** `apply(data,1,function(x) sum(is.na(x))/length(x))` builds a closure per row; also a missing `drop=FALSE` collapses a 1-row survivor to a vector and crashes downstream `data.frame(data, id=rownames(data))`.
**Fix:** `missing_percent <- rowMeans(is.na(data)); data[missing_percent <= max_missing/100, , drop=FALSE]`.
**Benchmark:** 34.4k×77: **48.5ms → 4ms**.
**Validation verdict — keep-as-is (safe).** `rowMeans(is.na(m))` is **`identical`** (not just `all.equal`) to `apply(,1,sum(is.na)/length)` — verified; the threshold comparison operates on identical values, so the kept-row set is unchanged. `drop=FALSE` is a pure bugfix that does not alter which rows survive, only the object class. **REQUIRED guard:** add the regression test the plan already calls for — exactly-1-surviving-row case asserting a data.frame (not vector) result and that `data.frame(data, id=rownames)` does not crash, **plus a 0-surviving-row case**.

### EXP-2 — `readr::write_csv` over `utils::write.csv`
**Location:** `tab_stat_summary.R` L692/L815, `tab_qc_cv.R` L383/L409, `tab_stat_plot.R` L1178.
**Root cause:** `utils::write.csv` is an R-level formatter. All five sites already pass `row.names = FALSE`, so `readr::write_csv` (already in DESCRIPTION Imports) is a near drop-in.
**Fix:** Swap at the five hot sites. Keep `utils::write.table` for the tiny tab-separated params/template dumps (sidebar_setup.R:1482, experimental-design.R:102).
**Benchmark:** 34.4k×21 stat table: **0.270s → 0.063s (4.3x)**.
**Validation verdict — keep-with-guard (safe-with-caveat, DOWNGRADED from None).** Empirically the two writers are **NOT byte-identical**: (1) **quoting** — `write.csv` quotes all strings incl. headers, `readr` quotes only when needed (`EGFR|KRAS` loses quotes); (2) **exponent format** — `'-1.23e-05'`/`'1e+15'` vs `'-1.23e-5'`/`'1e15'`; (3) **float sig-digits** — `'9.99999999999999e-301'` vs `'9.999999999999994e-301'`; (4) **NA representation** — `readr` writes empty string, `write.csv` writes `NA` (a genuine cell-content difference). Validators verified via grep that **every `read_csv`/`read.csv` is on the upload side** — these exported CSVs are **terminal zip artifacts never re-ingested by Protigy**, so internal results are unchanged. **But the downloaded file bytes change and any external user pipeline keyed on quoted CSV / exact float text / `NA` literal could differ.** **REQUIRED guard:** add an assertion/test documenting these outputs are terminal (not re-read); confirm no consumer relies on forced-quoting or the `NA` literal; **note the user-visible byte change in release notes.**

### dp-1a — `as.data.frame` deep copy vs serialize
**Location:** `R/sidebar_setup_helpers_GCT-processing.R` L551–559 (`df_deep_copy`).
**Root cause:** `unserialize(serialize())` walks the whole object graph; `as.data.frame()` achieves an equivalent independent copy for atomic-column frames far faster.
**Fix:** `out <- as.data.frame(df, stringsAsFactors=FALSE); rownames(out) <- rownames(df); out` (keep NULL / non-data.frame guards).
**Benchmark:** phospho rdesc: **9ms → ~0ms**.
**Validation verdict — keep-as-is (safe).** For atomic-column data.frames, `as.data.frame(df, stringsAsFactors=FALSE)` + restored rownames yields an identical, independent copy; `identical()` verified. Risk is only if any rdesc/cdesc carries a **list-column or factor** at copy time — `as.data.frame` could alter a list-column's structure vs serialize's exact graph copy. The plan asserts no list-columns survive `fix_gene_symbols` (plausible). **REQUIRED guard:** `identical(out, df)` test on BRCA rdesc/cdesc post-pipeline; add an assertion/guard that errors (or falls back to serialize) **if any column is a list**, so a future list-column doesn't silently change copy semantics.

### START-04 — Bound second `.gct` read to header region
**Location:** `R/sidebar_setup_helpers_GCT-processing.R` (`parse_gctx_preserve_cdesc`), `readLines` at L622 (`read_gct_cdesc_as_character`).
**Root cause:** Each `.gct` is fully read twice — `cmapR::parse_gctx` + a full-file `readLines` to recover raw cdesc strings (e.g. `"001"`). The second read needs only the first `3 + nchd` lines.
**Fix:** Two-stage bounded read: read line 2 for `nchd`, then read only `3 + nchd` lines. The existing `setequal(rownames, cid)` guard + dims validation already falls back to full `.gct` parse on mismatch.
**Benchmark:** Qualitative — no microbenchmark wired through parse.
**Validation verdict — needs-benchmark (safe-with-caveat).** Feasibility is fine, but the benefit is **unquantified and possibly modest**: `readLines` of a 34k-line file is fast on a warm OS page cache, so the real saving may be small except on cold cache. Truncated-read risk (mis-read `nchd`, header row spanning differently) is contained but not eliminated by the `setequal` fallback. **REQUIRED guard:** measure header-only read vs full `readLines` on **warm AND cold cache**; add a BRCA-fixture test asserting **byte-identical cdesc** including a leading-zero string column like `'001'`, and a malformed/short-header test confirming the fallback fires. (Open Question #5.)

### START-03 — Observer accumulation on file add/remove
**Location:** `R/sidebar_setup.R` L396 — bare `observe()` wrapping `lapply(1:nrow(files), function(i) observeEvent(input[[btn_id]], ...))`.
**Root cause:** Observers created inside an `observe()` are not auto-destroyed when the parent re-executes; handlers accumulate as O(total files ever added) and stale handlers keep firing.
**Fix:** (a) one delegated observer reading the clicked button id (structurally cleaner); or (b) keep a list of handles and `$destroy()` the previous batch at the top of the `observe()`.
**Benchmark:** Responsiveness/memory issue, not on a reported hot path.
**Validation verdict — keep-with-guard (safe).** Touches setup interaction wiring only, no analysis computation. Real but niche (long sessions). **REQUIRED guard:** shinytest2 covering add → remove → re-add → clear; verify a removed file's stale handler no longer fires (the actual symptom).

### STAT-02 — `toWebGL` for the 34k-point volcano
**Location:** `R/tab_stat_plot.R` — `output$volcano_plot` renderPlotly; wrap after `add_volcano_labels`, before `event_register`.
**Root cause:** `ggplotly` defaults to SVG scatter; 34k SVG DOM nodes are the classic plotly slowdown. Browser, not server, is the bottleneck.
**Fix:** Wrap the final plotly object in `plotly::toWebGL(p)` as the last step (so overlaid label markers also become `scattergl`).
**Benchmark:** Base + label traces convert to `scattergl`; no server-time change; eliminates browser pan/zoom/hover jank.
**Validation verdict — keep-as-is (safe, LOWER risk than the draft stated).** The strongest result across all validators: `get_clicked_feature_id` (helpers L362) matches purely on `click$x`/`click$y` via Euclidean nearest-neighbour — it **never reads `curveNumber`/`pointNumber`/`customdata`**, which are exactly the WebGL-fragile fields. `scattergl` returns x/y in `plotly_click` reliably, so the click handler is **essentially immune** to the SVG→WebGL switch. Wrapping after `add_volcano_labels` and before `event_register` is the correct ordering. **REQUIRED guard (light):** smoke-test one `plotly_click` round-trip under `scattergl` confirming `get_clicked_feature_id` returns the correct feature id; assert the exported/static volcano **PDF path does NOT go through `toWebGL`** (export stays ggplot/SVG so PDF bytes are unaffected); eyeball anti-aliasing. (Open Question #7.)

### STAT-07 — Latent `results_list` variable-shadowing rename
**Location:** `R/tab_stat_setup_helpers.R` — L393 (outer accumulator), L513 (inner `vector('list', n_contrasts)`), L570 (`results_list[[ome_name]] <-`).
**Root cause:** The same name is used for the outer per-ome accumulator and the inner per-contrast list; with `chosen_omes` length >1, only the last ome survives.
**Fix:** Rename the inner list to `contrast_results_list`.
**Validation verdict — keep-as-is (safe).** Pure rename of an inner list. The current caller always passes a single ome (`tab_stat_setup.R` L1250), so today's output is byte-identical; the rename only unblocks a future multi-ome batched call. **REQUIRED guard:** assert single-ome stat output is byte-identical before/after the rename on the BRCA fixture (this touches the stat accumulation code path).

### START-02 — Remove dead `furrr`/`future`/`WriteXLS`
**Location:** `R/protigy-package.R` L28,41,42; DESCRIPTION Imports L36,63,64.
**Root cause:** grep across `R/` finds zero uses of `future_map`/`future_map2`/`future::plan`/`availableCores`/`furrr`/`WriteXLS`.
**Fix:** Remove the three `@importFrom` lines and DESCRIPTION Imports entries; `devtools::document()`. Move `WriteXLS` to Suggests only if a future feature wants it.
**Validation verdict — keep-as-is (safe).** Symbols never referenced in `R/` outside the import declarations; removal cannot change runtime behavior. **REQUIRED guard:** re-grep `R/` **AND `tests/`** for `future_map`/`future_map2`/`future::plan`/`availableCores`/`furrr`/`WriteXLS` to confirm zero references, then `devtools::document()` + `check()` to confirm NAMESPACE regenerates cleanly.

### dp-redundant — Drop double `safe_copy_rdesc`
**Location:** `R/sidebar_setup_helpers_GCT-processing.R` L502–503 (`apply_gene_symbol_from_params`).
**Root cause:** `rdesc_backup <- safe_copy_rdesc(rdesc); rdesc <- safe_copy_rdesc(rdesc)` — the second copy is gratuitous; `rdesc_backup` already holds an independent copy.
**Fix:** Keep the backup copy, drop the second; rely on copy-on-write (subsequent column adds/overwrites create new columns leaving the backup intact).
**Benchmark:** ~9ms now, ~0 post-dp-1a.
**Validation verdict — keep-with-guard (safe-with-caveat).** Safe **provided no in-place modification** (e.g., `rdesc$x[i] <- ...` on an existing shared column) mutates a column shared with `rdesc_backup` before the failure path returns it. **REQUIRED guard:** assert `rdesc_backup` is returned **byte-identical** to the original on the ID-mapping failure path (L519) after the operations on `rdesc`; test the failure branch explicitly.

### STAT-03 — Volcano builds plotting df twice per render *(downgraded)*
**Location:** `R/tab_stat_plot.R` L749–783 (`plotVolcano` then `get_volcano_cols` + `build_volcano_df`).
**Root cause:** `plotVolcano` internally greps id/logFC/logP/adjP/pval columns + threshold to build its df; the renderer then re-derives them via `get_volcano_cols` + `build_volcano_df`. Two column-grep + NA-filter passes on a 34k×~100-col df.
**Fix:** Compute `cols` and `df_plot` once and pass them into `plotVolcano`.
**Benchmark:** Halves per-render grep/copy (tens of ms on 34k rows) — *server-side, microbenchmark-tier; not profiled by validators.*
**Validation verdict — downgrade-priority (safe-with-caveat, unverified).** Consolidation is result-preserving **only if** `plotVolcano`'s internal column resolution + `y_cutoff` and `get_volcano_cols`/`build_volcano_df` derive **byte-identical** columns and threshold — `plotVolcano` uses **perl lookahead patterns** the plan calls "near-identical," and "near" is the risk: a grep-pattern mismatch would silently change which column is treated as logFC/adjP. **REQUIRED guard:** byte-diff the resolved column names, `y_cutoff`, and resulting df row set on the BRCA fixture; only consolidate after `identical()` confirmation. **Ship only with the volcano refactor PR, after STAT-02 lands.** (Open Question #6.)

### STAT-05 — Narrow the wide stat df before plotting *(downgraded)*
**Location:** `R/tab_stat_setup_helpers.R` L244–247 / L376–379 / L565–568 (normalized_df `left_join` in all three test branches); consumed in the volcano path.
**Root cause:** The full normalized matrix (~77 sample cols) is `left_join`ed onto each ome's results, so `stat_results[[ome]]` is a 34k×~100+ col df scanned on every render.
**Fix:** In the volcano path, subset `stat_results[[ome]]` to the columns `get_volcano_cols` resolves (plus id + label/geneSymbol) **once** before building/labeling — narrow only the **plot-internal** copy, never the stored `stat_results`.
**Benchmark:** ~100+ → ~6–8 col scans on 34k rows — *server-side, microbenchmark-tier, not profiled.*
**Validation verdict — downgrade-priority (safe-with-caveat, unverified).** Value-safe only if the narrowed set includes every column the **label-column selector** and **protein search** consume — if either reads a column not in the narrowed set, labels/search break (visible). **REQUIRED guard:** confirm the label-column dropdown and `protein_search` read the **full (un-narrowed)** df; assert the narrowed set is a superset of `get_volcano_cols` output plus id + label/geneSymbol; test a search/label on a column outside the core scatter columns.

### STAT-01 — Cache volcano base plot; re-apply labels only *(downgraded)*
**Location:** `R/tab_stat_plot.R` L736–865 (`output$volcano_plot` renderPlotly).
**Root cause:** The expensive base scatter (34k geom_point → ggplotly, ~0.4s) is rebuilt even when only the label **set** changes.
**Fix:** Split into (a) a `bindCache`'d base-plot reactive keyed on every scatter-affecting input; (b) renderPlotly re-applies only `add_volcano_labels`.
**Benchmark:** Label-only interactions drop from ~0.4s rebuild + full re-transfer to near-instant — *base per-render cost not independently profiled.*
**Validation verdict — downgrade-priority (risky / unverified). DOMINANT RISK in the volcano cluster.** Correctness hinges **entirely** on the `bindCache` key enumerating EVERY input affecting scatter geometry/hover/significance coloring (ome, contrast/group, `label_column`, `label_split_enabled/sep`, `label_display_trim_enabled`, significance cutoffs, union toggle, **stat_results version token**). `plotVolcano` also calls `stat_params()`/`stat_results()` **internally** (not just via the df arg), so the cached reactive must still take a correct reactive dependency. If any key is omitted, a **stale cached base plot displays WRONG points/colors** — a visible result error. This is the **highest-effort, lowest-certainty** item; it serializes a multi-MB plotly object. **REQUIRED guard:** statically enumerate every input `plotVolcano` + `build_volcano_df` + `get_volcano_cols` read and assert the key is a superset; add a test that flips each scatter-affecting input one at a time and confirms the base plot updates (no stale cache). **Ship STAT-02 first and re-measure; only pursue STAT-01 if the base build remains the bottleneck.** (Open Question #8.)

### EXP-6 — Dedup `build_volcano_df` in volcano PDF export *(downgraded)*
**Location:** `R/tab_stat_plot.R` (`volcano_plot_export_function`); union-mode branch.
**Root cause:** Export calls `plotVolcano()` fresh per ome×contrast, and union mode additionally recomputes `build_volcano_df` per contrast just to derive label IDs.
**Fix:** Compute `build_volcano_df(...)` **once** per contrast and reuse it for both union-mode label-ID derivation and the `plotVolcano` draw.
**Benchmark:** Modest (few-hundred-ms/ome), secondary to EXP-1; the PDF draw itself is inherent.
**Validation verdict — needs-benchmark (safe-with-caveat, unverified).** Safe only if the prebuilt df is **byte-identical** to what `plotVolcano` builds internally (same cols/cutoff/stat/ordering) — same verification burden as STAT-03. **REQUIRED guard:** `system.time` on a 3-contrast phospho export to confirm the win is worth the consolidation; `identical()` the passed-in df vs `plotVolcano`'s internal df before merging; regenerate and visually diff the exported PDF label set. (Open Question #9.)

### STAT-08 — bindCache p-value histograms *(downgraded)*
**Location:** `R/tab_stat_summary.R` (`output$adj_pval_hist_plot` / `output$nom_pval_hist_plot`).
**Root cause:** No bindCache; `get_pvals` re-greps the wide stat df per render.
**Fix:** `bindCache` both outputs on (ome, contrast/group, stat_results token).
**Validation verdict — downgrade-priority (safe-with-caveat, unverified).** Histograms are cheap and not a reported bottleneck; `bindCache` adds key-correctness blast radius for a negligible saving. Value-safe **only if** the key captures contrast/group **and** a stat_results version token (a missing element shows a stale histogram for a different contrast). **REQUIRED guard:** if kept at all, include contrast/group + stat_results version token in the key and test that switching contrast updates both histograms; otherwise drop as not worth the burden.

---

## 6. Blocked pending guards / Dropped (validator-flagged risky or breaks-results)

### dp-double — Gene-symbol mapping runs twice per ome — **BLOCKED pending guards**
**Location:** `R/sidebar_setup_helpers_GCT-processing.R` `processGCTs` L850/856, `transformGCTs` L750/756; `repackage_transformed_gct_with_upload_rdesc` L604–605, fallback at L606–607.
**Why blocked (validator consensus: risky):** This is the **single highest result-blast-radius item** in the data-processing cluster — it changes which rdesc feeds the QC/export "original" GCTs. `transformGCTs`' rdesc is overwritten by `repackage_transformed_gct_with_upload_rdesc` **only when `all(rids %in% rownames(upload rdesc))`**; otherwise it falls back to `strip_gene_symbol_mapping_columns` on the **transformed** rdesc, preserving the transform-pass `geneSymbol`. So skipping/reusing the gene-symbol work in `transformGCTs` is **only safe IF the repackage path always replaces the exported `geneSymbol`.** If the fallback branch ever fires (row-id mismatch), the exported `geneSymbol` would now differ. The `mapIds` round-trip de-dup must also produce an identical mapping. **Not empirically verified.**
**REQUIRED guards before it can move out of this section:** (1) before/after **byte-diff of `transformation_output` rdesc (specifically the exported `geneSymbol` column)** on the BRCA fixture with ID-conversion **ON and OFF**; (2) an explicit **forced row-id-mismatch test** that exercises the repackage **fallback branch** (L606–607) and confirms the exported `geneSymbol` is unchanged. (Open Question #3.) Until both pass, do not ship; dp-2a (the faithful vectorization) ships independently in PR 3.

### EXP-1 — Single-pass GCT writer — **DROPPED BY DECISION (keep cmapR)**
**Why dropped (user decision, 2026-06-13):** Although validated byte-identical to `cmapR::write_gct(ver=3, precision=4)` and ~24× faster (the single largest export win), we are committed to using the `cmapR` package for GCT writing rather than maintaining a local writer that must track cmapR's output format across upstream versions. The per-row-`cat(append=TRUE)` cost (~11s on a 2-ome export) is accepted as-is. **Export speedups are now limited to EXP-2 (`readr::write_csv`)** in Phase 6. If GCT-write time later becomes intolerable, the better path is an upstream cmapR fix, not a local fork.

### dp-2b — De-quadratic merge loops — **DROPPED**
**Why dropped (validator consensus):** The plan itself self-labels this **<1ms at 2–4 omes** — negligible. Pure restructuring, none-risk, same column sets/order, but at the realistic 2–4 ome scale it is a **non-win** and including it costs review attention for zero measurable benefit. The merge column renaming uses `make.names(unique=TRUE)` + `.after`, so final cdesc column order depends on first-seen `conflict_columns` order — a `unique(unlist(list))` refactor preserves it only if per-ome iteration order is preserved, which is an avoidable correctness surface for no gain. **Only land if it falls out naturally while editing `merge_processed_gcts` for another reason; do not spend a dedicated change/benchmark on it.** (If ever revisited: diff the full ordered `names(GCTs_merged@cdesc)` and merged cdesc contents before/after on a multi-ome fixture with overlapping conflicting columns.)

### STAT-04 — Non-significant point thinning / rasterization — **DROPPED (breaks-results)**
**Why dropped (validator consensus):** Thinning **removes displayed non-significant points**, changing the rendered point SET — a visible analysis-output change, which violates the plan's overriding result-preservation constraint. Rasterization additionally changes hover behavior and adds a `ggrastr` dependency. With **STAT-02 (`toWebGL`) handling the 34k-point jank at zero result risk**, this is unnecessary; the 0.40→0.23s figure is a payload microbenchmark, not user-perceived after WebGL. **Do not implement in the first wave.** If ever revisited, gate behind an explicit user toggle, document that the displayed point set is decimated, and **never thin the exported PDF or the click-target data** — treat the displayed-point-set change as a result change requiring explicit sign-off. (Open Question #10.)

---

## 7. Dropped / false-positive candidates (carried from the draft, validator-confirmed)

| ID | Claim (prior plan) | Why dropped | Validator note |
|----|--------------------|-------------|----------------|
| EXP-3 | Lowering zip `compression_level` 9→1 saves 15–40s | **Refuted.** 0.594/0.599/0.553s at levels 9/6/1 on a 154MB 4-ome archive; deflate is lossless so decompressed content is unchanged. **No action.** | Confirmed: level changes only archive size/speed, never file content. |
| EXP-4 (perf half) | Reading `exports[[tab]]()` twice doubles prep cost | **Refuted as perf.** The reactives return lists of **closures**, not data. Sub-ms. **Keep only the `on.exit(unlink(...))` temp-dir cleanup.** | Confirmed: reading twice copies closures, not data. |
| EXP-5 | `stat_results()[[ome]]` materialized 3–5× | **Refuted.** `stat_results` is a `reactiveVal(list())` — reads are cached lookups. Style cleanup only. | Confirmed. |
| START-05 | Eager UI / help-markdown building is a startup bottleneck | **Refuted.** ~85ms total — below the noise floor vs multi-second package attach. **No action.** | Confirmed below noise floor. |
| dp-2a (prior version) | Plan's `gsub("\\s*\\|\\s*","|") + trimws()` is byte-for-byte identical | **Refuted.** Trims whitespace around pipes and at edges. Replaced by the faithful fuzz-verified version (dp-2a §5). | Confirmed non-faithful; faithful version verified over 70k+ cases. |
| STAT-06 | Per-row / per-contrast eBayes refit | **Refuted.** Already a single `lmFit` + `contrasts.fit` + one `eBayes`. **No action on the fit path.** | Confirmed. |
| furrr parallelism (all reviewers) | Parallelize per-ome processing/stat loops | **Out of scope (confirmed).** Global `future::plan()` state with no `on.exit` restore; `shinyalert`/`showNotification` throws in workers; **`normalmixEM` seedless RNG (L254–282) would silently change results** vs sequential under L'Ecuyer-CMRG streams; worker boot (~3–8s reloading Bioc/cmapR) exceeds sequential per-ome cost at 2–4 omes. In-process vectorization is the correct path. | Feasibility validator independently confirmed the seedless-RNG blocker as result-breaking. |

---

## 8. Open questions / needs-benchmark-before-implementing

1. **EXP-1 byte-identity (blocking):** diff-against-`cmapR::write_gct` ver=3 on BRCA proteome+phospho original+processed GCTs **and** a synthetic GCT with numeric/integer/logical/NA rdesc columns + scientific-notation matrix values; assert `identical(readBin(...))` and that **rdesc numeric columns are written unrounded**; use `as.character`/`paste`, never `format()`/`formatC()`.
2. **dp-norm parity (blocking, largely resolved):** `colMads constant=1.4826` matches `stats::mad` (validator-confirmed bit-identical); still run an `all.equal`/dimnames + Inf/NaN fixture diff for all four branches incl. zero-MAD and all-NA columns.
3. **dp-double diff (BLOCKING — see §6):** before/after byte-diff of `transformation_output` rdesc `geneSymbol` (ID-conversion ON+OFF) **and** a forced row-id-mismatch test exercising the repackage **fallback branch**.
4. **dp-2a fixture diff:** run `test-gct-processing.R` / `test-id-to-gene-symbol.R`, diff `geneSymbol` on the BRCA fixture; add list-column and UTF-8 cases and leading/trailing-whitespace-adjacent-pipe corpus entries.
5. **START-04:** measure header-only read vs full `readLines` on **warm AND cold cache**; byte-identical cdesc test incl. `'001'`; malformed-header fallback test.
6. **STAT-03:** byte-diff `plotVolcano`'s internal df vs `get_volcano_cols`+`build_volcano_df` (perl-regex patterns must match exactly) before consolidating.
7. **STAT-02:** smoke-test `get_clicked_feature_id` under `scattergl`; assert the exported PDF path does **not** go through `toWebGL`; eyeball anti-aliasing.
8. **STAT-01:** statically enumerate every input `plotVolcano`/`build_volcano_df`/`get_volcano_cols` consume (incl. a stat_results version token); cache-correctness test flipping each input. **Only after STAT-02 is shown insufficient.**
9. **EXP-6:** `system.time` a 3-contrast phospho export; `identical()` the passed-in df vs `plotVolcano`'s internal df.
10. **STAT-04 (dropped):** only revisit if `toWebGL` proves insufficient; any thinning is a result change requiring explicit sign-off and must never touch the exported PDF or click-target data.

---

## 9. Suggested implementation order & PR grouping (post-validation)

Ordering principle: maximize user-visible win per unit risk, ship the byte-identity-verified wins first, batch by file/theme so each PR has one coherent regression-test focus, and **defer the server-side volcano items behind `toWebGL`.**

**~~PR 1 — Export GCT writer (EXP-1)~~ — DROPPED BY DECISION.** Keep `cmapR::write_gct`; the export GCT path is not changed. Open Question #1 is moot. Export wins now come solely from PR 8 (EXP-2 CSV writer).

**PR 2 — Intensity-toggle freeze (INT-1 + INT-2 + INT-3).** Directly kills Symptom B. INT-3 (`sort()` drop, none-risk) + INT-2 (cache, keyed on `GCTs_unprocessed`) make the rebuild cheap; **INT-1 uses Option B (guarded write) or live-widget read — NOT bare Option A.** *Cluster B.* **Gate: shinytest2 edit-normalization-then-toggle assertion + reactlog invalidation check.**

**PR 3 — Volcano `toWebGL` (STAT-02) + latent rename (STAT-07).** Moved up: STAT-02 is the biggest perceived volcano win at the lowest verified risk and unblocks deferring STAT-01/03/05/08. *Cluster F (core).* **Gate: Open Question #7; byte-identical single-ome stat output for STAT-07.**

**PR 4 — Faithful gene-symbol vectorization (dp-2a + dp-redundant).** Largest *safe* data-processing win. **dp-double is NOT in this PR — it is blocked (§6).** *Cluster A (gene-symbol subset).* **Gate: Open Question #4; dp-redundant failure-path assertion.**

**PR 5 — matrixStats numerics (dp-norm + dp-sd + dp-1b).** Introduces the `matrixStats` DESCRIPTION/roxygen dependency once. dp-1b's `rowMeans` + `drop=FALSE` crash bugfix rides along. *Cluster C.* **Gate: Open Question #2 + dp-sd filtered-row-set diff + dp-1b 1-row/0-row tests.**

**PR 6 — Package attach (START-01 + START-02).** One `devtools::document()` regen; trims cold-open. *Cluster D.* **Gate: re-grep R/ + tests/; `check()` clean.**

**PR 7 — Deep-copy + I/O cleanups (dp-1a + START-04).** Lower-risk data-processing remainder. *(dp-2b dropped.)* *Cluster A (copy/IO subset).* **Gate: Open Question #5; dp-1a list-column guard.**

**PR 8 — Export CSV + hygiene (EXP-2 + EXP-4 cleanup + EXP-5 snapshot).** Secondary export wins + temp-dir `on.exit` cleanup. *Cluster E (CSV half).* **Gate: confirm CSVs are terminal; note byte change in release notes.**

**PR 9 (conditional) — Server-side volcano refactor (STAT-03 → STAT-05 → STAT-01 → STAT-08 + EXP-6).** **Only if PR 3's `toWebGL` proves insufficient on real hardware.** Highest-effort/lowest-certainty; each item gated on its byte-diff (Open Questions #6, #8, #9) and on a measured per-render saving. *Cluster F (deferred).*

**Blocked — dp-double:** ships only after the §6 guards (Open Question #3 incl. the repackage fallback branch) pass; would then amplify dp-2a.

**Separable anytime — START-03** (observer accumulation): independent of cold-open and submit paths; ship whenever the shinytest2 add/remove/clear coverage is ready.

**Dropped — STAT-04, dp-2b** (§6).

Each PR must carry its result-preservation regression test (byte-diff against the prior implementation on the BRCA fixture) before merge — this is the non-negotiable gate for every item. **No code has been changed by this document; all items above are proposals to be decided on.**
