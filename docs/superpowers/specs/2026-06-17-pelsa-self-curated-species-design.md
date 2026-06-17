# PELSA self-curated species + taxonomy-code database convention — Design

**Date:** 2026-06-17
**Status:** Approved (brainstorming complete; ready for implementation plan)
**Scope:** PELSA subsystem only (`R/tab_pelsa_*`, `inst/database/`, PELSA tests)

## Problem

PELSA today supports exactly two species — `human` and `mouse` — as hand-named
subfolders under `inst/database/`. Adding a species requires (a) dropping a folder
with a FASTA and (b) app-side wiring. Two limitations motivate this work:

1. **Two-step onboarding.** New species are not "drop a folder and go". We want a
   single, self-describing convention so the species list grows by adding folders.
2. **UniProt-only annotation assumption.** Every species is assumed to be a UniProt
   proteome with fetchable feature annotations. Self-curated databases (custom
   FASTA headers, organisms not in UniProtKB — e.g. *Hoylesella timonensis* strain
   C0091E11) have no UniProt annotations to fetch and must be handled gracefully:
   parse the custom headers, skip annotation fetching, and degrade the
   annotation-dependent UI without breaking position mapping / coverage / the
   Woods plot.

## Convention (the single signal)

**A species is identified by its folder name under `inst/database/`. The folder
name is the sole, sufficient signal for how the species is treated.**

- Folder name matches `^[0-9]+$` (all digits) → **candidate UniProt species**.
  The digits are interpreted as an NCBI/UniProt taxonomy ID. FASTA is parsed in
  UniProt mode (pipe-aware `sp|`/`tr|`), and UniProt feature annotations apply.
- Folder name is **non-numeric** → **self-curated species**. FASTA is parsed in
  self-curated mode (first whitespace-delimited token = accession), and all
  UniProt-annotation features are disabled.

There is **no per-folder override file**. Naming a folder by its numeric taxon ID
*is* the declaration "treat as UniProt"; naming it descriptively *is* the
declaration "self-curated". A power user with a numeric-taxon strain DB that they
want treated as self-curated simply names the folder descriptively.

### Required migration

The bundled folders are renamed to follow the convention:

- `inst/database/human` → `inst/database/9606`
- `inst/database/mouse` → `inst/database/10090`

`git mv` moves the whole folder (its `fasta/`, `uniprot_features/`,
`uniprot_membrane/`, `schema.json`) intact. A full grep-sweep updates every
hardcoded `"human"` / `"mouse"` **species literal** in `R/` and `tests/`.

**Out of scope / unaffected:** `R/sidebar_setup_helpers_GCT-processing.R`
`org_db_for_species()` keys on upload-time species *display labels*
(`"Homo sapiens"`, `"human"`, …) for org.*.eg.db ID mapping — this is the general
Statistics/setup pipeline, NOT the PELSA database folder, and is left untouched.

## Classification + validation

Classification is **structural + API-validated**:

1. Structural test: `^[0-9]+$`.
2. For a numeric folder, validate the taxon against UniProt:
   `GET https://rest.uniprot.org/taxonomy/{id}` (returns
   `{scientificName, commonName, taxonId, mnemonic, …}`).
   - Reuse the existing httr2 stack pattern from `pelsa_fetch_uniprot`
     (`rest.uniprot.org` host, `req_user_agent`, `req_throttle`, `req_retry` with a
     transient 429/5xx predicate). **Same host as the annotation fetcher** — one
     mental model; "validated against UniProt" means validated against the same
     service that supplies annotations.

### Verdict cache: `inst/database/species_meta.json`

A single registry at the top of the database dir, keyed by folder name. Holds the
resolved verdict + display metadata for every species. **Runtime-generated and
gitignored** (regenerated on first run; never committed).

```json
{
  "9606":  {"type": "uniprot", "taxon_id": 9606, "scientific_name": "Homo sapiens",
            "common_name": "Human", "validated": true},
  "10090": {"type": "uniprot", "taxon_id": 10090, "scientific_name": "Mus musculus",
            "common_name": "Mouse", "validated": true},
  "9999999": {"type": "uniprot", "taxon_id": 9999999, "scientific_name": null,
              "validated": false},
  "hoylesellaTimonensis": {"type": "self_curated",
                           "display_name": "hoylesellaTimonensis", "validated": true}
}
```

- Network is touched **only** when a numeric folder has no cached verdict, or when
  re-validating an unvalidated entry at app start. Normal re-renders read the cache
  — the setup-box listing stays off the network (honoring the existing
  network-free reactive-path architecture).
- Add `inst/database/species_meta.json` to `.gitignore` (alongside the existing
  regenerable feature/membrane cache rules).

### Validation-failure handling (the key edge case)

When validation cannot complete after retries (confirmed network/API failure, not
a clean 404):

- **Numeric + a UniProt feature cache exists on disk** (`uniprot_features/` parquet
  or tsv present) → treat as **UniProt-unvalidated**: parse/color as UniProt, all
  annotation features ON, `validated: false`, display name pending. A shipped/built
  feature cache is unambiguous evidence the folder is a real UniProt species; a
  transient network failure must not reclassify it and corrupt its parsing.
- **Numeric + no feature cache** → **demote to self-curated** for this run.
- A clean 404 (taxon does not exist) → self-curated regardless of cache (the ID is
  fake/fat-fingered).

> **Fresh-clone nuance.** The feature/membrane caches are *already* gitignored —
> a fresh clone ships only `fasta/*.fasta` + `schema.json` for `9606`/`10090`. So
> "has feature cache" reflects **local** state (post-refresh), not git. On a fresh
> clone with no network AND no built cache, a numeric folder demotes to
> self-curated for that session and self-corrects once network returns (the FASTA
> is still UniProt-formatted, but self-curated first-token parsing of a `sp|...`
> header is acceptable degradation for that rare offline-first-run window — it
> self-heals on the next online start).

### Retry-on-start + promotion

On **every app start**, re-attempt validation for every structurally-numeric folder
whose cached entry is `validated: false` (or missing). On success, rewrite
`species_meta.json` (promote to `validated: true`, fill `scientific_name`). This
recovers the "first run was offline / API was down, second run is fine" case.

## Resolver (the linchpin)

A single pure helper resolves a folder to a typed species struct; every consumer
reads it instead of re-deriving type from strings.

```r
pelsa_resolve_species(database_dir, folder) -> list(
  folder            = <chr, the stable key, unchanged everywhere downstream>,
  type              = "uniprot" | "self_curated",
  display           = <chr, the picker label / export string>,
  taxon_id          = <int or NA>,
  scientific_name   = <chr or NA>,
  validated         = <lgl>,
  has_feature_cache = <lgl>
)
```

- `is_self_curated <- (type == "self_curated")` is derived **once** here and carried
  in the analysis cache. It is the single flag that gates FASTA mode, the feature
  cache read, and the annotation-dependent UI.
- **Label ≠ type.** The display label is cosmetic and never gates functionality.
  Gating is always on the resolved `type`.
- Self-curated → the analysis pipeline sets `feat_df <- pelsa_empty_feature_frame()`
  and does **not** look for a `uniprot_features` cache. Downstream annotation
  overlap already handles an empty feature frame (zero overlaps), which yields the
  blank Woods feature track for free.

## FASTA parsing

`pelsa_read_fasta(path, mode = c("uniprot", "self_curated"))`:

- `uniprot` (default): **byte-identical** to today's logic (pipe-aware `sp|`/`tr|`
  accession extraction). The shared machinery (readLines, blank-line drop, cumsum
  grouping, duplicate-accession handling) is untouched; existing parity tests pass
  unchanged. `pelsa_read_fasta()` is the documented "highest parity-risk piece" —
  the UniProt branch must not change.
- `self_curated`: accession = the first whitespace-delimited token of the header;
  everything after the first field is the protein description and is **ignored**
  (not threaded through — the return contract stays `accession -> sequence`).
- **Mode is decided by the resolved folder type, never by header content.** A
  self-curated header that happens to contain a `|` is still first-token-parsed.

## Display: three label states

The setup-box species `selectInput` uses **named choices** — label = display name,
**value = folder name** (the stable key; nothing downstream of selection changes).
The same display string is written to the export YAML.

| State | Label | Annotation features |
|-------|-------|--------------------|
| UniProt, validated | `Homo sapiens (9606)` | ON |
| UniProt, name-pending + has cache | `9606 (annotations available, name pending)` | ON |
| Self-curated | `hoylesellaTimonensis (customized)` | OFF |

The name-pending UniProt state is a **distinct** marker (not collapsed into
`(customized)`) precisely because its annotation features are ON — the label must
not imply "self-curated / no annotations" while the annotation track is populated.
Gating is on `type`, so this label honestly reflects that it is still a UniProt
species.

## Self-curated UI (all gated on resolved `type`, not label)

- **Volcano "Color points by"** (`pelsa_color_mode` radio,
  `R/tab_pelsa_section3.R`): the `"UniProt feature class"` option is **disabled**
  and selection is forced to `"Significance"`.
- **Volcano right-hand "UniProt feature colors" legend**: **replaced with a short
  note** (e.g. "Feature annotations unavailable — self-curated database") rather
  than dimmed in place (a dimmed full color key would imply the colors mean
  something here).
- **Woods plot**: the feature/annotation track is blank/absent. Achieved via the
  empty `feat_df` → `nrow(feats) == 0` path; gate the lane allocation on
  `nrow(feats) == 0` (a generally-correct improvement that also covers a UniProt
  protein with no features), so `is_self_curated` need not reach the Woods
  renderer. Ruler, sequence-coverage spans, and peptide logFC bars still render.
- **Species refresh checklist** (`pelsa_refresh_species`): filtered to
  `type == "uniprot"`. Self-curated species are omitted entirely. Name-pending
  UniProt species DO appear (they are refreshable).
- **Volcano labels**: self-curated **forces** `<accession>_aa<pos>`. Thread
  `is_self_curated` into `.pelsa_volcano_labels` (and the parallel
  `pelsa_build_multilabel`) so any `PG.Genes` the input report carries is ignored
  for self-curated species. (For UniProt species, the existing gene→accession
  fallback is unchanged.)
- **Fixed tooltip gene field**: self-curated **forces truly empty** (`Gene: ` with
  nothing after) — consistent with forced-accession labels; both driven by the same
  `is_self_curated` flag, so a self-curated point can never show a label/tooltip
  gene mismatch.

## `uniprot_membrane`

`inst/database/<species>/uniprot_membrane/*.tsv` is a separate, differently-sourced
annotation (a `*_membraneLoc.tsv` export, not produced by the refresh path; refresh
leaves it untouched). It lives inside the species folder, so the `git mv` rename
moves it cleanly. No code keys on the literal `human`/`mouse` for the membrane path,
so the rename does not orphan it. Self-curated species simply have no membrane file
(absent → no membrane annotation, same as a UniProt species without one).

## Testing (no live network; synthetic fixtures)

Mirror the existing PELSA discipline: pure logic + injectable fetch fn; the
taxonomy validation fn is injectable and stubbed in tests (the taxonomy API is
never hit in CI). New / updated tests:

1. `pelsa_read_fasta(path, mode = "self_curated")` — first-token accession, on a new
   synthetic custom-header FASTA fixture (a few proteins with Hoylesella-style
   headers). UniProt-mode parity tests remain unchanged.
2. Classification logic with an injected `validate_fn`:
   - all-digits + success → uniprot/validated;
   - all-digits + failure + has cache → uniprot-unvalidated;
   - all-digits + failure + no cache → self-curated;
   - all-digits + clean 404 → self-curated;
   - non-numeric → self-curated (offline; no network).
3. `species_meta.json` read / write / round-trip + the retry-on-start →
   promote → rewrite path.
4. `pelsa_resolve_species()` struct correctness for each type, including
   `has_feature_cache` detection and `is_self_curated` derivation.
5. Volcano label + tooltip force-accession / force-blank-gene for self-curated
   (flag → behavior), and unchanged gene→accession fallback for UniProt.
6. Setup-box label formatting for all three states.

Update the existing `tests/testthat/test-pelsa-refresh.R` and
`test-pelsa-integration.R` species literals (`"human"`/`"mouse"` → `"9606"`/
`"10090"`) as part of the migration sweep.

## Out of scope

- A membrane-annotation refresh path (pre-existing TODO, untouched).
- Any change to the UniProt feature classifier / annotation-overlap parity logic.
- Per-folder override files or mnemonic-based naming.
- Capturing/surfacing the self-curated protein description anywhere in the UI.

## Decision log (resolved during brainstorming)

- Folder rename to taxon codes (not keep-names-plus-marker-file).
- Display format `<scientific name> (<taxon>)`, also in export YAML.
- Classification: structural + API validation (not structural-only).
- Verdict cached in a single gitignored `inst/database/species_meta.json` (not
  per-folder, not committed).
- Validation failure: numeric + has-cache → UniProt-unvalidated; numeric + no-cache
  → self-curated; clean 404 → self-curated.
- Retry-and-promote on every app start.
- `pelsa_read_fasta` gets a `mode` arg (not a separate function); description ignored.
- Single `pelsa_resolve_species()` struct carried in the analysis cache; gating on
  resolved `type`, never on the label.
- selectInput value stays the folder name; three distinct label states.
- Self-curated UI: disabled feature radio + replaced legend note + blank Woods
  track (via empty-frame) + refresh checklist filtered + forced-accession labels +
  forced-empty tooltip gene.
- Empty gene renders truly empty (not `NA`, not a dash).
- Taxonomy endpoint: `rest.uniprot.org/taxonomy/{id}` (same host as annotations),
  not the EBI Proteins API.
- No override mechanism; folder naming is the sole signal.
