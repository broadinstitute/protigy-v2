# PELSA UniProt-annotation refresh — runtime characterization (human)

**Date:** 2026-06-14 · **Machine:** macOS arm64, R 4.6.0 · **Scope:** the Setup
tab "Maintenance: UniProt annotation library" control
(`pelsa_run_species_refresh` → `pelsa_refresh_species_cache` →
`pelsa_fetch_uniprot`), measured for **human**.

Benchmark/characterization tests:
`tests/testthat/test-pelsa-refresh-benchmark.R` (hermetic — no live network; the
fetch is the injected `fetch_fn` seam with a calibrated latency model).

---

## TL;DR

The refresh is **far slower than the UI's "several minutes per species" promise**,
and the cause is one design choice, not the pipeline.

| Path (human) | Accessions | **Current serial design** | Batched `/stream` design |
|---|---:|---:|---:|
| No datasets uploaded → **FASTA fallback** (whole proteome) | ~70,000 | **~4.3 hours** | **~2 minutes** |
| Mid-size single dataset | ~8,000 | ~29 min | ~14 s |
| Small dataset | ~5,000 | ~18 min | ~9 s |

- **Orchestration (merge + TSV write + schema.json) is ~50 ms even at 35k
  accessions** — measured, not modelled. **~100% of the wall-clock is the
  network fetch.**
- A batched-stream redesign is **~122× faster** in the model **with byte-identical
  parsed output** (the classifier is per-feature and pure, so batching cannot
  change a single class/score/coordinate).

---

## What the refresh actually does

1. **Universe** (`pelsa_refresh_accession_universe`): the accessions to fetch.
   - With datasets uploaded: `unique(exploded PG.ProteinAccessions) ∪ existing cache`.
   - **No datasets uploaded (the maintenance-on-fresh-install case): the FASTA
     accessions** — i.e. the *entire* `inst/database/human/fasta/*.fasta`. The
     committed human FASTA has **69,845 headers**.
2. **Fetch** (`pelsa_fetch_uniprot`): the slow part — see below.
3. **Merge over cache** (`pelsa_merge_feature_cache`) + **atomic write**
   (`pelsa_write_feature_cache`). Both are cheap and already well-built
   (data-loss-safe merge, tempfile + rename atomic write).

## Why it's slow — the bottleneck, precisely

`pelsa_fetch_uniprot` issues **one HTTP GET per accession**
(`{accession}.json`), **serially**, in a plain `for` loop, throttled to
`req_throttle(capacity = 10, fill_time_s = 1)` (~10 req/s):

```
wall-clock ≈ N_accessions × (1/10 s throttle slot + ~0.12 s RTT)
```

For human that is:

- **70k × 0.22 s ≈ 15,400 s ≈ 4.3 h** (fallback), and that's the *floor* —
  add `req_retry` backoff on any 429/5xx and it climbs further.
- Even **8k × 0.22 s ≈ 29 min** for an ordinary dataset.

The original code comment justifies per-accession fetching as "adequate for the
app's top-up (a handful of cache misses)". That assumption is correct for an
incremental top-up but **breaks for a full species rebuild**, which is exactly
what this maintenance button triggers — and it's the only way a user regenerates
the gitignored cache.

## Evidence (from the benchmark run)

```
[bench] human n=5000: orchestration=0.059s  vs modelled serial network=1100s (18.3 min)
[bench] orchestration for n=35000: 0.045 s
[bench] human fallback n=70000: serial model=15400s (4.3 h)
[bench] dataset n=8000:           serial model=1760s (29.3 min)
[bench] n=  5000: serial=    1100s  batched=    9.0s  speedup=122x
[bench] n=  8000: serial=    1760s  batched=   14.4s  speedup=122x
[bench] n= 70000: serial=   15400s  batched=  126.0s  speedup=122x
```

---

## Optimization strategies (ranked; accuracy-preserving)

### 1. Batched `/stream` (or `/search`) fetch — **the fix, ~50–120× faster**

UniProt's REST API returns **many entries in one request**:
`https://rest.uniprot.org/uniprotkb/stream?query=accession:P1+OR+accession:P2+…&format=json`
→ `{"results": [ {primaryAccession, features, …}, … ]}`. Verified against the
live endpoint; each entry has the same `primaryAccession` + `features` shape the
existing pure parser already consumes.

Plan:
- Chunk the universe into pages of ~250–500 accessions (URL-length-bounded),
  OR'd into one `accession:(…)` query.
- One request per page → `ceil(70000/500) = 140` requests instead of 70,000.
- Parse each page's `results` array with the **existing**
  `pelsa_parse_uniprot_json_batch` (already handles a list of entries → 8-col
  frame). **No classifier change → identical output.**
- Accessions absent from a page's `results` are `unresolved` (same contract as
  today; the data-loss-safe merge already covers them).
- Use httr2's `req_perform_iterative(..., iterate_with_cursor("cursor", …))` for
  UniProt's cursor/Link-header pagination when a page query itself exceeds the
  result window.

**Accuracy:** identical. The benchmark asserts
`pelsa_parse_uniprot_json_batch(entries) == rbind(per-accession parses)`.

### 2. Add a `fields=` projection — smaller payloads, same parsed columns

Append `&fields=accession,ft_domain,ft_region,ft_act_site,ft_binding,…` (the
feature types the classifier reads) so each response carries only feature data,
not full entries. Cuts bytes/parse time materially on top of (1). The parser
only reads `primaryAccession` + `features[*].{type,description,location,ligand}`,
so projecting to those fields is lossless for our schema.

### 3. Bounded parallelism for the page requests — another 4–8×

With pages (not 70k items), `req_perform_parallel(reqs, max_active = 6,
on_error = "continue")` + a polite `req_throttle` fetches several pages at once.
Safe because the page count is small and UniProt tolerates a handful of
concurrent streams. `resps_successes()/resps_failures()` keeps per-page error
isolation (one bad page → those accessions `unresolved`, not a whole-run abort).

### 4. Cap / confirm the FASTA-fallback universe — stop the worst case at the source

The 4.3 h case is the *fallback* fetching the whole proteome when no dataset is
loaded. Options (do at least one):
- **Surface the size before fetching** and require confirmation past a threshold
  (e.g. "About to fetch 69,845 accessions (~N min). Continue?").
- Prefer the **dataset-driven** universe whenever datasets are present (already
  the case) and document that a full proteome rebuild is a deliberate, long op.

### 5. Skip already-cached, unchanged accessions — incremental refresh

For a *top-up*, fetch only `universe \ cache_accessions` instead of re-fetching
the whole union. The existing cache already covers 33,179 human accessions; a
refresh that only fetches genuinely-missing ones is dramatically smaller. (Keep
a "force full rebuild" affordance for when UniProt itself changed.)

### 6. Progress granularity — perceived speed + cancelability

Report progress per page (not per species), and make the in-flight loop
**cancelable** so a user who started a 4 h fallback by accident can stop it.
Today progress jumps `0.05 → 0.85 → 1.0` around an opaque multi-minute fetch.

---

## Status — implemented (2026-06-15)

**Strategy 1 (batched `/search`) is implemented** in `pelsa_fetch_uniprot`
(`R/tab_pelsa_uniprot_fetch.R`): accessions are chunked (200/query), each chunk
fetched as one `accession:(P1 OR P2 OR ...)` query, cursor-paginated via
`req_perform_iterative(..., iterate_with_link_url("next"))`, with a
consecutive-failed-batch breaker and per-batch error isolation. The
`list(features=, unresolved=)` contract is unchanged.

**Strategy 2 (field projection) was deliberately dropped** for correctness: a
`fields=ft_*` projection must enumerate every feature type the classifier reads,
and any omission silently drops features. Omitting `fields` returns full entries
(all feature types) so the batched parse is byte-identical to per-accession.

**Verified correct (live):** `tests/testthat/test-pelsa-uniprot.R` compares the
batched fetch against per-accession ground truth for TP53/EGFR/BRCA1/Trypsin
(`expect_equal` — byte-identical, 2204 rows) and asserts hand-read P00761
features (active sites 48/92/185, Ca2+ binding 60/62/65/70, site 179, no TM).
An offline test proves a `/search` `results` array parses identically to
per-accession entries.

Remaining (4: confirm-before-fetch on the fallback; 3: bounded parallel pages;
5: incremental top-up; 6: per-page progress + cancel) are still open.

## Recommended sequencing

1. **(1)+(2)** batched `/stream` with `fields=` projection — turns hours into
   ~2 min and minutes into seconds; this is the core win and is accuracy-neutral.
2. **(4)** confirm-before-fetch on the fallback — removes the foot-gun while (1)
   is in flight.
3. **(5)** incremental top-up + **(3)** bounded parallel pages — squeeze the rest.
4. **(6)** per-page progress + cancel — UX.

All of (1)–(3),(5) reuse the existing pure parser/classifier and the injected
`fetch_fn` seam, so they are unit-testable with **no live network** exactly like
the current code. The benchmark file is the regression harness for the speed-up.
