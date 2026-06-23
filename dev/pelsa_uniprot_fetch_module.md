# PELSA UniProt Feature-Fetch Module — Reproduction Guide

This document captures the UniProt feature-annotation **fetch** subsystem that
formerly lived in Protigy (`R/tab_pelsa_uniprot_helpers.R` +
`R/tab_pelsa_species_resolve.R`), removed in favor of a per-dataset upload model.
It is everything needed to rebuild the fetcher as a standalone workflow whose
output Protigy now consumes.

The fetcher's job: given a set of UniProt accessions (from a FASTA proteome
and/or a dataset), produce a **per-feature annotation table** that Protigy reads
to color/annotate the PELSA volcano and Woods plots.

> Source of truth for exact code: the git history of this repo just before commit
> `71163cf` (`git show 71163cf~1:R/tab_pelsa_uniprot_helpers.R`) and `ac50bb4~1`
> for `tab_pelsa_species_resolve.R`.

---

## 1. What Protigy now expects (the output contract)

Protigy uploads consume a **raw feature TSV** (one row per feature). Required
columns; Protigy derives `feature_class` + `class_score` itself on load
(`pelsa_read_annotation_file()` in `R/tab_pelsa_annotation_io.R`):

| column | type | meaning |
|---|---|---|
| `accession` | chr | UniProt accession (base; isoforms allowed but overlap is exact-match) |
| `feature_type` | chr | raw UniProt feature type (`Domain`, `Active site`, `Transmembrane`, …) |
| `start` | int | 1-based AA start (inclusive) |
| `end` | int | 1-based AA end (inclusive) |
| `description` | chr | feature description (UniProt `description`, or ligand name fallback) |
| `coord_quality` | chr | optional; `"exact"` or `"fuzzy"`; defaults to `"exact"` |

The fetcher MAY also emit the derived `feature_class` + `class_score` columns
(the internal 8-column schema), but Protigy recomputes them regardless, so the
classifier is the single source of truth.

**Internal 8-column schema** (what the fetcher built end-to-end):
`accession, feature_type, start, end, description, feature_class, class_score, coord_quality`.

---

## 2. REST contract

- **Feature search base:** `https://rest.uniprot.org/uniprotkb`, path `search`.
- **Query form:** `accession:(P1 OR P2 OR ... )` — URL-encoded via the client.
- **Page format:** `format=json`, `size=100`; paginate by following the
  `rel="next"` `Link` header (cursor pagination) until none remains.
- **Taxonomy (species name validation):** `GET https://rest.uniprot.org/taxonomy/{id}`
  → `scientificName` / `commonName`.
- **User-Agent:** `pelsa_qc/0.1 (PELSA data pipeline)` (set on every request).

HTTP client: **httr2** (`request`, `req_url_path_append`, `req_url_query`,
`req_user_agent`, `req_throttle`, `req_retry`, `req_error`, `req_timeout`,
`req_perform_iterative` + `iterate_with_link_url(rel = "next")`, `resp_status`,
`resp_body_json`).

---

## 3. Batching, throttle, retry, circuit breaker

- **Batch size = 100 accessions per `/search` query** (`.PELSA_BATCH_SIZE`).
  This is a HARD cap: UniProt's `accession:` OR-filter rejects > 100 OR
  conditions. Chunk the (valid, deduped, isoform-based) accessions into batches
  of ≤ 100.
- **Page size = 100**, cursor-paginated within each batch.
- **Throttle:** `req_throttle(capacity = 10, fill_time_s = 1)` → ≤ 10 req/s.
- **Retry:** `req_retry(max_tries = 5, is_transient = …)` — exponential backoff,
  honors `Retry-After`. Transient = HTTP **429, 500, 502, 503, 504**.
- **Error policy:** `req_error(is_error = function(resp) resp_status(resp) >= 500)`
  — a 4xx (e.g. a query matching nothing) is a NORMAL response, not an error;
  only 5xx/network count as failures.
- **Circuit breaker:** abort with a clear "UniProt unavailable" error after
  **5 consecutive failed batches** (`.PELSA_BREAKER_LIMIT`). A successful batch
  resets the counter (failures must be consecutive).
- **Partial-page survival:** `req_perform_iterative(on_error = "return")` keeps
  the pages already fetched when a later page 5xxs — those entries are retained
  and their accessions count as resolved.

### Accession validation (critical)
A single malformed `accession:` term makes UniProt 400 the **whole batch**
(dropping every valid accession in it). So filter to syntactically-valid UniProt
accessions before querying, and query the **isoform base** (UniProt's
`accession:` index matches only the base, so `P12345-3` matches nothing):

```r
.PELSA_ACCESSION_RE <- paste0(
  "^([OPQ][0-9][A-Z0-9]{3}[0-9]|",
  "[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})(-[0-9]+)?$"
)
# query_accs = unique(isoform_base(accessions[valid]))
```
Non-UniProt FASTA keys (smORFs, contaminants like `B99901`, `smORF_G1|X`) fail
this and are excluded from the query but still counted as `unresolved`.

---

## 4. JSON → feature parse (per entry)

Port of `pelsa_parse_uniprot_json(entry)` — for one parsed UniProt entry
(`resp_body_json()` / `fromJSON(simplifyVector = FALSE)`):

- `accession = entry$primaryAccession`.
- For each `entry$features[[i]]`:
  - `start = location$start$value`, `end = location$end$value`; **SKIP the
    feature if either is NULL.**
  - `feature_type = feature$type`.
  - `description = feature$description`; if empty, fall back to
    `feature$ligand$name`.
  - `coord_quality = "exact"` iff both `location$start$modifier` and
    `location$end$modifier` are `"EXACT"` (default `"EXACT"` when absent), else
    `"fuzzy"`.
  - `feature_class = feature_to_class(feature_type, description)`;
    `class_score = SCORES[feature_class]`.
- Batch parse = rbind of per-entry frames (`data.table::rbindlist`).

---

## 5. The parity-locked classifier (LIVES IN PROTIGY)

`pelsa_feature_to_class()` + `pelsa_feature_class_scores()` now live in Protigy
(`R/tab_pelsa_annotation_io.R`) and are the parity source of truth — a
standalone fetcher should match them exactly (classifier_version `"fixed_v1"`,
ported from the notebook's `uniprot_features.py::feature_to_class`).

**CHECK ORDER is parity-critical** (first match wins; implemented by evaluating
in reverse-priority so earlier checks overwrite later ones):

1. `compositional bias` → `low_complexity_or_disorder` (highest)
2. site set (`active site`, `binding site`, `metal binding`,
   `nucleotide binding`, `site`, `dna binding`) → `active_or_binding_site`
3. TM/signal set (`transmembrane`, `signal peptide`, `topological domain`,
   `intramembrane`, `signal`) → `transmembrane_or_signal`
4. **description-keyword disorder check** (`low complexity`,
   `compositionally biased`, `disordered`) → `low_complexity_or_disorder`
   — this BEATS repeat/region/domain (step 4 before steps 5–7)
5. repeat set (`repeat`, `coiled-coil`, `coiled coil`) → `repeat_or_coiled_coil`
6. `domain` → `catalytic_domain` if the description contains a catalytic keyword
   (`kinase`, `methyltransferase`, `transferase`, `atpase`, `helicase`,
   `protease`, `dehydrogenase`), else `folded_domain`
7. `region` / `motif` → `region_or_motif`
8. else → `other`

`feature_type` and `description` are lower-cased + trimmed (NA → `""`).

**class_score lookup** (parity-locked):

```
active_or_binding_site     =  5
catalytic_domain           =  3
folded_domain              =  2
region_or_motif            =  1
repeat_or_coiled_coil      = -1
transmembrane_or_signal    =  0
low_complexity_or_disorder = -3
other                      =  0
```

**Priority ladder** for resolving ONE feature class per peptide (used downstream
in `pelsa_annotate_features`, highest → lowest): `active_or_binding_site`,
`catalytic_domain`, `folded_domain`, `region_or_motif`,
`transmembrane_or_signal`, `repeat_or_coiled_coil`,
`low_complexity_or_disorder`, `other`. **NOTE:** the ladder puts
`transmembrane_or_signal` BEFORE `repeat_or_coiled_coil` — this is the notebook's
order and differs from the schema's level order; the notebook wins.

---

## 6. Resolved vs feature-bearing vs unresolved

The fetcher tracked three categories against the INPUT accession universe:

- **resolved** = an ENTRY was returned (matched on `primaryAccession`,
  `secondaryAccessions`, OR the isoform base of the input). Resolution is ENTRY
  presence, **not** feature presence.
- **zero_feature** = resolved but produced 0 parsed features (UniProt answered,
  nothing to annotate). Persisted as a **sentinel row** so incremental refresh
  never re-fetches it: `feature_class = "none"`, `start/end = NA`,
  `class_score = 0`.
- **unresolved** = no entry returned (404-equivalent / failed batch / not-yet
  fetched on cancel). `transient_unresolved` ⊆ unresolved = the failed-batch
  subset (re-running recovers these; genuinely-absent ones never recover).

> In the new Protigy upload model there is no fetch, so an accession **absent
> from the uploaded annotation file counts as FAILED** (not zero-feature). The
> fetcher may still emit `feature_class = "none"` sentinel rows to mark
> genuinely-zero-feature accessions if desired.

---

## 7. Cache build / merge / atomic write (for a library-maintaining tool)

The old refresh maintained a per-species on-disk cache
(`<species>/uniprot_features/uniprot_features.tsv` + `schema.json`). Useful if
the standalone tool maintains a reusable library:

- **Universe builders:** FULL = all FASTA-proteome accessions; INCREMENTAL =
  `(dataset ∪ fasta) − cache` accessions (skips accessions already cached,
  including zero-feature sentinels).
- **Merge:** fresh rows win for resolved accessions; old rows are RETAINED for
  accessions whose batch failed (no data loss on a flaky refresh); a resolved
  accession with zero fresh rows correctly drops its stale rows (replaced by a
  sentinel).
- **Atomic write:** write to a tempfile in the SAME directory, then rename — a
  partial/failed write never corrupts a pre-existing cache. Leave no leftover
  temp files on success.
- **Wipe (FULL mode):** delete `uniprot_features/` + `uniprot_membrane/` but
  spare `fasta/`; rebuild from the FASTA only.

---

## 8. Species / taxonomy resolution

A species was a subfolder of `inst/database/`. Its NAME was the sole signal
(`pelsa_resolve_species`):

- **All-digits name** (`9606`, `10090`) = a UniProt **taxon code** → pipe-aware
  FASTA parse (`>sp|P12345|NAME`), UniProt annotation fetch, and name validation
  via `GET /taxonomy/{id}` (`pelsa_fetch_taxon`: 3-try retry, transient
  429/5xx, raises only on ≥ 500 so a 404 = `not_found` is distinguishable).
- **Any other name** = self-curated → first-token FASTA parse, NO annotation
  fetch, accession-based labels.

Verdicts cached in a gitignored `species_meta.json`. In the new Protigy model
this is replaced by a per-dataset **"self-curated" checkbox** (UniProt-style
FASTA by default; checkbox switches to first-token parse and disables the
annotation upload).

---

## 9. FASTA parsing

`pelsa_read_fasta(path, mode = c("uniprot", "self_curated"))` (kept in Protigy):

- **uniprot** (pipe-aware): header `>sp|P12345|NAME_HUMAN` → take the middle
  pipe field; bare `>ACC DESC` → first whitespace token.
- **self_curated** (first-token): always the first whitespace token, ignoring
  pipes.
- Multiline sequence blocks concatenated; duplicate accessions → first-wins with
  a warning; vectorized (`readLines` once, headers located, sequences grouped via
  `cumsum`).

---

## 10. Learnings / gotchas

- **The 100-OR cap is non-negotiable.** Exceeding it 400s the whole batch.
- **One malformed accession poisons the whole batch** (400). Validate against the
  accession regex and drop non-UniProt keys BEFORE building the query.
- **Query the isoform base.** `accession:` matches only the base; `P12345-3`
  matches nothing. Account resolution against the input universe with an
  isoform-base fallback, or every isoform looks unresolved.
- **Resolved ≠ feature-bearing.** Track ENTRY presence separately from feature
  presence, else zero-feature proteins inflate the "failed" QC count and stale
  rows linger.
- **Throttle is required** to stay a good UniProt citizen (10 req/s).
- **Partial-page survival matters** on large proteomes — keep good pages when a
  later page 5xxs (`on_error = "return"`).
- **Atomic write** prevents a flaky refresh from corrupting a good cache.
- **Classifier parity is the whole game.** The keyword sets, the CHECK ORDER, and
  the SCORES must match the notebook exactly. The hardest case: a `Domain` whose
  description is "Disordered" classifies as `low_complexity_or_disorder` (the
  disorder check at step 4 beats the domain branch at step 6) — even with a
  catalytic keyword also present. Protigy keeps the classifier + its tests
  (`tests/testthat/test-pelsa-uniprot.R`); match them.
- **NA-safety:** `feature_class` can be NA from a blank TSV cell; any NA-aware
  predicate must be NA-safe (a bare `== "none"` yields NA that crashes
  `if (any(...))`).
- **`readr` reads a missing cell as `NA`, not `""`** — matters when reproducing
  data-handling around blank description/coord_quality cells.
