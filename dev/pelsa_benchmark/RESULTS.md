# PELSA backend benchmark — Python vs reticulate vs native R

**Date:** 2026-06-12 · **Machine:** macOS arm64 (Sonoma), R 4.6.0, Python 3.13
(anaconda), pandas 3.0.3 / numpy 2.4.6 / pyarrow 24.0.0, reticulate 1.46.0,
arrow 24.0.0.

**Question:** keep the PELSA analysis backend in Python (driven from R via
reticulate) as originally proposed, or port it to native R? This benchmark runs
the **same** representative peptide-level workload three ways and measures the
overhead that actually drives the decision.

**Workload** (`pelsa_workload.py` / `pelsa_bench_native.R`): synthetic
peptide-level frame **300,000 rows × 24 samples × 31 cols**, then the real
backend hot path — `;`-accession **explode**, **within-condition CV**
(delinearize → CV%, n≥3), **best-peptide-per-protein rollup**. Median of 5 runs,
after warm-up.

## Headline numbers

| Scenario | Full workload (median) |
|----------|------------------------|
| **Native Python** (`pelsa_bench_native.py`) | **~2.26 s** |
| **R + reticulate**, summary return (`pelsa_bench_reticulate.R`, A) | ~2.21 s |
| **R + reticulate**, big-table auto-convert (B) | ~2.17 s |
| **R + reticulate**, parquet handoff (C) | ~2.53 s |
| **Native R, naive** (`apply(lin, 1, sd)`) | ~17.8 s |
| **Native R, vectorized** (`matrixStats::rowSds`) | **~2.05 s** |

## The decisive finding

**Python is NOT inherently faster here. A competently vectorized native-R port
ties native Python (~2.0 s vs ~2.3 s).** The 17.8 s native-R figure was caused by
a single bad idiom — a row-wise `apply(lin, 1, sd)` in the CV step — not by R.

Per-stage profile (300k rows):

| Stage | Time |
|-------|------|
| explode_peptides (tidyr unnest) | 760 ms |
| **CV — row-wise `apply(1, sd)`** | **15,854 ms** |
| **CV — `matrixStats::rowSds`** | **294 ms** (54× faster) |
| best_peptide_rollup (dplyr) | 476 ms |

The CV step alone went from ~16 s to ~0.3 s by swapping the row-wise apply for a
vectorized `rowSds`. That one change is the entire gap.

## reticulate boundary costs (isolated, 300k × 31 frame)

| Boundary operation | Cost |
|--------------------|------|
| Near-empty call (pure bridge latency) | ~13 ms |
| pandas DataFrame → R data.frame, in-memory (300k rows) | ~105 ms |
| parquet handoff (py write + R `arrow::read_parquet`) | ~605 ms |

So the reticulate bridge itself is cheap (~13 ms/call) and an in-memory copy of a
full 300k-row frame is only ~105 ms. **For result-sized frames (the 16k-row
rollup), the boundary cost is in the noise.** Notably, **in-memory auto-convert
BEAT the parquet handoff here** (~105 ms vs ~605 ms) — parquet write+read only
pays off for frames much larger than this, or when the data must persist to disk
anyway (which the PELSA cache does). Don't add a parquet round-trip purely to
"speed up" the boundary at this scale.

## Annotation step (the highest re-port-risk piece) — measured 2026-06-12

The first benchmark left the **UniProt feature-overlap + 9-bucket annotation**
uncovered. It is not a vectorizable matrix reduce — it is a *range-overlap join*
(a peptide `[start,end]` overlaps a feature `[start,end]` for the same
accession), then a priority-rank reduction to one winning class per peptide.
Workload: **300,000 peptide rows × 8,000 accessions × 6 features each** (offline
synthetic, seeded — network has no effect). Mirrors `pelsa_qc_helpers.annotation`.

| Annotation path | Median |
|-----------------|--------|
| **Native R** (`data.table::foverlaps`, indexed interval join) | **~246 ms** |
| Python via reticulate (isolated R process) | ~351 ms |
| Native Python (pandas `merge` + boolean overlap filter) | ~407–507 ms |

**Native R is the fastest (~2× native Python).** `data.table::foverlaps` is an
*indexed* interval join; the pandas version does a brute many-to-many `merge` on
accession then filters by span overlap, which materialises far more intermediate
rows. So the step flagged as "hardest to port" is not only portable — idiomatic R
is faster. (Native-vs-reticulate Python differences here are run-to-run variance
on a sub-second task, not a real bridge effect; treat them as one ~350–500 ms band.)

> ⚠ **Parity not yet proven.** These are *speed* numbers. The R `foverlaps`
> implementation has NOT been parity-checked against `pelsa_qc_helpers.annotation`
> on shared synthetic data — the real annotation has subtleties the benchmark
> omits (comma-in-token intra-protein hits like `"2,167"` that the explosion
> drops; `;`-token alignment back onto `PG.ProteinAccessions`; isoform-base TM
> fallback). Equal speed ≠ equal output. The parity gate still governs.

> ⚠ **OpenMP runtime conflict — a real hybrid-app risk.** Loading `data.table`
> **and** python `numpy`/`pyarrow` in the *same* R process aborts with
> `OMP: Error #15: Initializing libomp.dylib, but found libomp.dylib already
> initialized` — both link their own OpenMP runtime. The only bypass is
> `KMP_DUPLICATE_LIB_OK=TRUE`, which the OpenMP project documents as *"unsafe,
> unsupported … may cause crashes or silently produce incorrect results."* The
> reticulate number above was therefore measured in an **isolated R process with
> `data.table` not loaded**. This means a reticulate-hybrid app effectively cannot
> use `data.table` (the fastest R path for exactly this join) in-process — a
> concrete strike against the hybrid, on top of the pandas-3.0 teardown segfault.

## API-fetch code complexity — Python vs R (no code run; UniProt is a live API)

The user asked to compare fetch-code complexity, not just speed. The notebook's
fetch lives in `pelsa_qc_helpers/uniprot.py` (**428 lines**) and leans on **three
third-party libraries** for production-grade fetching:

| Concern | Python (`uniprot.py`) | R equivalent (`httr2`) |
|---------|-----------------------|------------------------|
| HTTP request | `requests` | `req_perform()` |
| Retry + exponential backoff | `tenacity` (`@retry`, `wait_exponential`, `stop_after_attempt`) | **built-in** `req_retry(max_tries=, backoff=)` |
| Honor `Retry-After` on 429/503 | hand-written `_Retryable(sleep_s=…)` + tenacity hook | **built-in** `req_retry(after=)` (httr2 reads `Retry-After` natively) |
| Client-side rate limit (10 req/s) | `pyrate_limiter` (`Limiter`, `Rate`, `Duration`) | **built-in** `req_throttle(rate=)` |
| Circuit breaker (N consec. 5xx) | hand-written `threading.Lock` + counter + `UniProtCircuitOpen` | hand-written (or `req_error()` + a small counter) — ~15 lines |
| Batch (≤500 accessions/req) | `fetch_uniprot_json_batch` | `req_url_query()` / `req_body_*` loop |
| Lazy dep import to keep package importable | `_ensure_deps()` (~40 lines of guarded imports) | n/a — `httr2` is a single declared Import |

**Verdict: the R fetch is *less* code, not more.** Three of the Python module's
load-bearing concerns — retry/backoff, `Retry-After`, and rate-limiting — are
**built into `httr2`** (`req_retry`, `req_throttle`), each one a single piped call,
whereas Python pulls in `tenacity` + `pyrate_limiter` and still hand-writes the
`Retry-After` handling. Only the circuit breaker is genuinely hand-rolled in both
(~15 lines). A faithful R port of `uniprot.py` is realistically **~60–120 lines
with one dependency (`httr2`)** vs **428 lines + 3 dependencies** in Python — and
it runs once at Start-Analysis (the top-up fetch), off the reactive path. The
fetch is therefore a *point in favor of R*, not a re-port risk.

(No fetch code was written or executed here — UniProt is a live external service;
this is a static complexity comparison of the existing Python module against the
documented `httr2` API.)

## Implications for the architecture decision

1. **Speed does not justify Python.** Native R ties Python on this workload once
   vectorized. If the backend goes native-R (the current plan of record), there
   is **no measurable performance penalty** — provided hot numeric steps use
   vectorized primitives (`matrixStats`, `rowSums`/`colSums`, matrix ops) and
   **never** per-row `apply`.
2. **If reticulate-hybrid is chosen instead**, the bridge is not the bottleneck:
   ~13 ms/call, ~105 ms to ferry a full peptide frame in-memory. Cross the
   boundary a fixed number of times (once per Start-Analysis) and return
   result-sized frames; you will not notice the bridge.
3. **The real Python-vs-R driver remains REUSE + correctness risk** (re-porting
   the UniProt feature-overlap / 9-bucket annotation faithfully), exactly as the
   planning doc argues — *not* raw speed. This benchmark removes "Python is
   faster on big data" from the argument entirely.

## Caveats (honest scope)

- Synthetic data, not the real `pelsa_qc_helpers` pipeline. The annotation
  overlap **is** now benchmarked (above) and native R wins, but its **parity**
  with `pelsa_qc_helpers.annotation` is not yet proven — speed ≠ identical output.
- Single machine, warm caches, GIL-free single-threaded; no `future`/`furrr`
  parallelism applied to either side.
- pandas 3.0 + reticulate 1.46 segfaulted on R *exit* during interpreter
  teardown (after all timings printed) — measurements are valid; the crash is a
  cleanup-time issue, but it is a yellow flag for shipping reticulate against a
  bleeding-edge pandas in a packaged app.

## Scripts

The intermediate benchmark scripts (`pelsa_workload.py`, `pelsa_bench_native.{py,R}`,
`pelsa_bench_reticulate.R`, `annotation_workload.py`, `annotation_bench.R`) were
**removed after benchmarking** — they were one-off measurement tools, not part of
the package. This `RESULTS.md` is the durable record. The synthetic workloads they
used (300k peptide rows × 24 samples with `;`-multi-accession/shared-peptide
structure, per-contrast `logFC`/`adj.P.Val`, NA holes; and the
300k-peptide × 8k-accession × 6-feature overlap workload) are described above and are
the reference shape for the R parity-test synthetic generator
(see `docs/pelsa-module-planning.md` → Parity-test gate). Re-create from this
description if the benchmark ever needs re-running.
