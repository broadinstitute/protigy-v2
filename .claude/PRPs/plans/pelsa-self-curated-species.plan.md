# Plan: PELSA self-curated species + taxonomy-code database convention

## Summary
Replace PELSA's hardcoded `human`/`mouse` species folders with a self-describing
convention: a folder under `inst/database/` named by a UniProt **taxonomy code**
(all digits) is a UniProt species (UniProt FASTA parse + annotation fetch); any
other folder name is a **self-curated** species (first-token FASTA parse, no
annotation fetch, annotation-dependent UI disabled). A single resolver classifies
each folder (structural test + API validation, cached in a gitignored
`species_meta.json`) and every consumer reads the resolved struct.

## User Story
As a PELSA user analyzing a non-model organism with a self-curated protein database,
I want to drop its FASTA folder into `inst/database/` and have the app list it,
parse its custom headers, map peptide positions, and show the Woods/volcano without
demanding UniProt annotations, so that I can run PELSA on organisms UniProt does not
annotate — without app-side wiring.

## Problem → Solution
Species are hardcoded as `human`/`mouse` folders and every species is assumed to be
a fetchable UniProt proteome → Species are identified by folder name (taxon code vs.
descriptive), classified once by a resolver, and self-curated species degrade
gracefully (custom-header parsing, no annotation fetch, disabled annotation UI,
accession-based labels).

## Metadata
- **Complexity**: Large
- **Source PRD**: N/A
- **Design Spec**: `docs/superpowers/specs/2026-06-17-pelsa-self-curated-species-design.md`
- **Estimated Files**: ~12 (3 new helpers/fixtures, ~6 edited R files, ~3 edited/new test files, `.gitignore`, folder rename)

---

## UX Design

### Before
```
Setup ▸ Species:  [ human ▾ ]      (only human / mouse; hand-wired)
                  [ mouse  ]
Volcano ▸ Color points by:  (•) Significance   ( ) UniProt feature class
Woods:  ruler + coverage + peptides + UniProt feature track
Refresh checklist:  [x] human  [x] mouse
```

### After
```
Setup ▸ Species:  [ Homo sapiens (9606) ▾ ]                  <- UniProt, validated
                  [ Mus musculus (10090) ]
                  [ 9606 (annotations available, name pending) ]  <- UniProt, name fetch failed but cache present
                  [ hoylesellaTimonensis (customized) ]      <- self-curated

  (self-curated selected)
Volcano ▸ Color points by:  (•) Significance   ( ) UniProt feature class  [DISABLED]
          right legend column -> "Feature annotations unavailable - self-curated database"
Woods:  ruler + coverage + peptides   (feature track blank/absent)
Volcano labels:  <accession>_aa<pos>  (forced; ignores any PG.Genes)
Fixed tooltip:   Gene:                (empty)
Refresh checklist:  [x] Homo sapiens (9606)  [x] Mus musculus (10090)   (self-curated omitted)
```

### Interaction Changes
| Touchpoint | Before | After | Notes |
|---|---|---|---|
| Species picker | folder name = label = value | label = display name, value = folder name | named choices vector |
| Adding a species | folder + app wiring | drop a folder, name = signal | numeric=UniProt, else self-curated |
| Self-curated volcano color | both modes selectable | feature-class disabled, forced Significance | gated on resolved `type` |
| Self-curated Woods track | feature track drawn | blank/absent | empty `feat_df` -> existing `nrow==0` guard |
| Self-curated labels | gene if present else accession | always `<accession>_aa<pos>` | `is_self_curated` threaded into label builder |
| Self-curated tooltip gene | report gene / fallback | empty | forced blank |

---

## Mandatory Reading

| Priority | File | Lines | Why |
|---|---|---|---|
| P0 | `docs/superpowers/specs/2026-06-17-pelsa-self-curated-species-design.md` | all | The approved design + decision log |
| P0 | `R/tab_pelsa_fasta_helpers.R` | 37-99 | `pelsa_read_fasta` — add `mode` arg, keep UniProt branch byte-identical |
| P0 | `R/tab_pelsa_section1_helpers.R` | 40-91 | `pelsa_database_dir`, `pelsa_list_species` — classification + resolver live near here |
| P0 | `R/tab_pelsa_section1.R` | 144-168, 815-853, 916-934 | species selectInput wiring, refresh checklist source, Start-Analysis FASTA read |
| P0 | `R/tab_pelsa_volcano_helpers.R` | 95-118, 170-299 | `.pelsa_volcano_labels` + `pelsa_build_volcano_df` — thread `is_self_curated` |
| P1 | `R/tab_pelsa_uniprot_fetch.R` | 256-271, 391-428 | httr2 stack pattern to mirror for the taxonomy fetch |
| P1 | `R/tab_pelsa_annotation_helpers.R` | 122-170 | `pelsa_read_feature_cache` (errors if absent — `has_feature_cache` must be a separate `file.exists`) |
| P1 | `R/tab_pelsa_refresh_helpers.R` | 512-529, 554-600 | `pelsa_species_refresh_inputs`/`pelsa_run_species_refresh` call `pelsa_read_fasta` without a mode |
| P1 | `R/tab_pelsa_section1_helpers.R` | 627-760 | `pelsa_setup_box_ui` — species choices + refresh checklist UI |
| P1 | `R/tab_pelsa_section3.R` | 75-77, 248-266, 648-708 | default_annotations, feat_df read, color radio + legend |
| P2 | `R/tab_pelsa_woods_helpers.R` | 302-340 | `pelsa_feature_track_ggplot` already guards `nrow==0` (no change, confirm) |
| P2 | `R/tab_pelsa_analysis_helpers.R` | 128-154, 179-220 | `pelsa_species_fasta_path`, `pelsa_validate_setup` |
| P2 | `tests/testthat/test-pelsa-refresh.R` | 280-600 | species literals to migrate; refresh stub pattern |
| P2 | `.gitignore` | 30-38 | existing PELSA cache-ignore block to extend |

## External Documentation

| Topic | Source | Key Takeaway |
|---|---|---|
| UniProt taxonomy endpoint | `https://rest.uniprot.org/taxonomy/{id}` (verified live) | Returns JSON `{scientificName, commonName, taxonId, mnemonic, rank}`. 404 for a non-existent taxon. Same host as the annotation fetcher. |

```
KEY_INSIGHT: rest.uniprot.org/taxonomy/9606 -> {"scientificName":"Homo sapiens","commonName":"Human","taxonId":9606}
APPLIES_TO: taxonomy validation + display-name fetch (Task 3)
GOTCHA: A clean 404 means "fake taxon -> self-curated"; a network/5xx after retries means
        "can't tell -> use feature-cache evidence (Task 4 verdict logic)". Distinguish them.
```

No other external research needed — everything else uses established internal PELSA patterns.

---

## Patterns to Mirror

### NAMING_CONVENTION
```r
# SOURCE: R/tab_pelsa_section1_helpers.R:47, 80; tab_pelsa_fasta_helpers.R:37
# Pure helpers are snake_case `pelsa_*`, @noRd, take params (not reactive),
# validate args with a single-string stop() guard, return plain data structures.
pelsa_database_dir <- function() system.file("database", package = "Protigy")
pelsa_list_species <- function(database_dir) {
  if (!is.character(database_dir) || length(database_dir) != 1L) {
    stop("pelsa_list_species(): `database_dir` must be a single string.", call. = FALSE)
  }
  ...
}
```

### ERROR_HANDLING
```r
# SOURCE: R/tab_pelsa_annotation_helpers.R:123-131
# Boundary validate single-string path; clear stop() on misuse.
# BUT the feature cache read STOPS if the tsv is absent — so do NOT call it to
# probe existence. Probe with file.exists() instead (see Task 4).
if (!file.exists(tsv)) stop("pelsa_read_feature_cache: ... not found at '", tsv, "'")
```

### NETWORK_PATTERN (httr2 stack to mirror for the taxonomy fetch)
```r
# SOURCE: R/tab_pelsa_uniprot_fetch.R:256-271, 414-427
.PELSA_UNIPROT_BASE <- "https://rest.uniprot.org/uniprotkb"   # taxonomy uses /taxonomy
.PELSA_UNIPROT_UA   <- "pelsa_qc/0.1 (PELSA data pipeline)"
.pelsa_is_transient <- function(resp) httr2::resp_status(resp) %in% c(429L,500L,502L,503L,504L)
base_req <- httr2::request(base)
base_req <- httr2::req_user_agent(base_req, .PELSA_UNIPROT_UA)
base_req <- httr2::req_throttle(base_req, capacity = rate, fill_time_s = 1)
base_req <- httr2::req_retry(base_req, max_tries = max_tries, is_transient = .pelsa_is_transient)
base_req <- httr2::req_error(base_req, is_error = function(resp) httr2::resp_status(resp) >= 500)
```

### INJECTABLE_FETCH (testability seam — mirror for taxonomy)
```r
# SOURCE: R/tab_pelsa_section1.R:798-806 (fetch_fn = pelsa_fetch_uniprot injected)
# Tests inject a stub; production passes the real fn. The taxonomy resolver takes
# `validate_fn = pelsa_fetch_taxon` defaulting to the real httr2 call.
pelsa_run_species_refresh(species = selected, database_dir = ...,
                          fetch_fn = pelsa_fetch_uniprot, set_progress = ...)
```

### VOLCANO_LABEL (the gene->accession fallback to convert into a forced flag)
```r
# SOURCE: R/tab_pelsa_volcano_helpers.R:108-112
lid <- data.table::fifelse(
  is.na(dt$gene) | !nzchar(trimws(dt$gene)), dt$accession, dt$gene)   # current
dt[, entry := paste0(lid, "_aa", pep_start)]
# Task 7: when is_self_curated, force lid <- dt$accession unconditionally.
```

### SELECTINPUT_NAMED_CHOICES
```r
# SOURCE: R/tab_pelsa_section1_helpers.R:657-662 (current: choices = species)
shiny::selectInput(ns("pelsa_species"), label = "Species",
                   choices = species, selected = selected_species)
# Task 6: choices becomes a NAMED vector  c("Homo sapiens (9606)" = "9606", ...)
#         value stays the folder name; label is the display string.
```

### TEST_STRUCTURE
```r
# SOURCE: tests/testthat/test-pelsa-refresh.R (testthat 3e; tempdir db; injected stub fetch)
test_that("...", {
  db <- withr::local_tempdir()         # or tempfile()+dir.create; see file
  dir.create(file.path(db, "9606"))
  stub <- function(accessions, ...) list(features = ..., unresolved = ...)
  res <- pelsa_run_species_refresh(species = "9606", database_dir = db,
                                   uploaded_gcts = gcts, fetch_fn = stub, ...)
  expect_identical(res[[1]]$species, "9606")
})
```

---

## Files to Change

| File | Action | Justification |
|---|---|---|
| `inst/database/human/` → `inst/database/9606/` | RENAME (`git mv`) | taxon-code convention; caches/fasta/membrane move intact |
| `inst/database/mouse/` → `inst/database/10090/` | RENAME (`git mv`) | same |
| `.gitignore` | UPDATE | add `inst/database/species_meta.json` |
| `R/tab_pelsa_species_resolve.R` | CREATE | new: `pelsa_classify_folder`, `pelsa_fetch_taxon`, `pelsa_read_species_meta`/`pelsa_write_species_meta`, `pelsa_resolve_species`, `pelsa_species_display_label` |
| `R/tab_pelsa_fasta_helpers.R` | UPDATE | add `mode` arg to `pelsa_read_fasta` |
| `R/tab_pelsa_section1_helpers.R` | UPDATE | species choices = display labels; refresh checklist filtered to uniprot |
| `R/tab_pelsa_section1.R` | UPDATE | resolve species on listing; pass mode + empty feat_df for self-curated; refresh-on-start; thread struct |
| `R/tab_pelsa_analysis_helpers.R` | UPDATE | Start-Analysis: resolve type, FASTA mode, skip feature cache for self-curated |
| `R/tab_pelsa_refresh_helpers.R` | UPDATE | `pelsa_species_refresh_inputs` passes UniProt mode (only ever called for uniprot species) |
| `R/tab_pelsa_volcano_helpers.R` | UPDATE | thread `is_self_curated` -> force accession label + blank winning_gene |
| `R/tab_pelsa_section3.R` | UPDATE | disable feature-class radio + replace legend note + force blank tooltip gene for self-curated |
| `tests/testthat/fixtures/pelsa/self_curated.fasta` | CREATE | synthetic custom-header FASTA |
| `tests/testthat/test-pelsa-species-resolve.R` | CREATE | classification/resolver/meta-cache/fasta-mode/label tests |
| `tests/testthat/test-pelsa-refresh.R`, `test-pelsa-integration.R` | UPDATE | migrate `"human"`/`"mouse"` literals to `"9606"`/`"10090"` |

## NOT Building
- Per-folder override file or mnemonic-based naming (folder name is the sole signal).
- Membrane-annotation refresh path (pre-existing TODO, untouched).
- Capturing/surfacing the self-curated protein description anywhere.
- Any change to the UniProt feature classifier / annotation-overlap parity logic.
- Committing `species_meta.json` or the feature caches (both gitignored).
- Touching `R/sidebar_setup_helpers_GCT-processing.R::org_db_for_species` (non-PELSA, upload-time ID mapping).

---

## Step-by-Step Tasks

### Task 1: Rename bundled folders + .gitignore
- **ACTION**: `git mv inst/database/human inst/database/9606` and
  `git mv inst/database/mouse inst/database/10090`.
- **IMPLEMENT**: Append `inst/database/species_meta.json` to the PELSA block of `.gitignore`
  (after line 38).
- **GOTCHA**: The feature/membrane caches are ALREADY gitignored — a fresh clone ships
  only `fasta/*.fasta` + `schema.json`. The `.fasta` and `schema.json` move with `git mv`;
  the local-only caches (if present) move too but aren't tracked. Confirm
  `inst/database/9606/fasta/*.fasta` exists post-rename.
- **VALIDATE**: `ls inst/database` shows `9606 10090`; `git status` shows renames; no
  dangling references in `git grep -n '"human"\|"mouse"' R/ tests/` except the intentional
  non-PELSA `org_db_for_species` matches.

### Task 2: `pelsa_read_fasta(path, mode)` — self-curated parse mode
- **ACTION**: Add `mode = c("uniprot", "self_curated")` param to `pelsa_read_fasta`
  (`R/tab_pelsa_fasta_helpers.R:37`).
- **IMPLEMENT**: `mode <- match.arg(mode)`. Keep ALL shared machinery (readLines, blank
  drop, header detection, cumsum grouping, sequence concat, dup handling) byte-identical.
  Only the key derivation (lines 58-64) branches:
  - `uniprot`: existing `first_tok` + pipe-aware logic (UNCHANGED).
  - `self_curated`: `keys <- sub("\\s.*$", "", headers)` (first whitespace token); no pipe split.
- **MIRROR**: NAMING_CONVENTION; keep the @noRd banner accurate (document the mode).
- **GOTCHA**: This is the documented "highest parity-risk piece." The UniProt branch must
  remain byte-identical so existing parity tests pass UNCHANGED. Do not refactor shared lines.
- **VALIDATE**: `devtools::test_active_file("tests/testthat/test-pelsa-fasta.R")` (or the
  fasta test file) passes unchanged; new self-curated assertion (Task 11) passes.

### Task 3: `pelsa_fetch_taxon` — taxonomy name/validation fetch
- **ACTION**: New helper in `R/tab_pelsa_species_resolve.R`:
  `pelsa_fetch_taxon(taxon_id, base = "https://rest.uniprot.org", max_tries = 3L, rate = 10L)`.
- **IMPLEMENT**: Build the httr2 request mirroring NETWORK_PATTERN; GET
  `/{taxonomy}/{taxon_id}` with `Accept: application/json`. Return a structured result:
  `list(status = "ok"|"not_found"|"network_error", scientific_name = , common_name = , taxon_id = )`.
  - 200 -> `ok` with parsed `scientificName`/`commonName`.
  - 404 -> `not_found` (fake taxon).
  - network/5xx after retries -> `network_error`.
- **MIRROR**: NETWORK_PATTERN, INJECTABLE_FETCH (this fn is the injectable seam).
- **IMPORTS**: `httr2` (already an Import; used by `tab_pelsa_uniprot_fetch.R`).
- **GOTCHA**: Use `req_error(is_error = \(resp) resp_status(resp) >= 500)` so a 404 is NOT
  an error (we must distinguish 404 from 5xx). DO NOT call against the live network in tests.
- **VALIDATE**: Unit test with an injected fake (Task 11) covers ok/not_found/network_error;
  no live call in CI.

### Task 4: Classification + verdict cache + resolver
- **ACTION**: In `R/tab_pelsa_species_resolve.R` add:
  `pelsa_classify_folder(folder)`, `pelsa_read_species_meta(database_dir)`,
  `pelsa_write_species_meta(database_dir, meta)`, `pelsa_resolve_species(database_dir, folder, validate_fn = pelsa_fetch_taxon, meta = NULL)`,
  `pelsa_species_has_feature_cache(database_dir, folder)`.
- **IMPLEMENT**:
  - `pelsa_classify_folder`: `grepl("^[0-9]+$", folder)` -> `"numeric"` else `"named"`.
  - `pelsa_species_has_feature_cache`: `file.exists(file.path(database_dir, folder, "uniprot_features", "uniprot_features.tsv"))` OR the `.parquet`. (Do NOT call `pelsa_read_feature_cache` — it stops if absent.)
  - `pelsa_read/write_species_meta`: JSON at `file.path(database_dir, "species_meta.json")`
    via `jsonlite`. Read returns `list()` when absent. Write is atomic (write temp + rename),
    mirroring the cache-write discipline in `pelsa_write_feature_cache`.
  - `pelsa_resolve_species`: returns the struct
    `list(folder, type, display, taxon_id, scientific_name, validated, has_feature_cache)`.
    Verdict logic (from the spec):
    1. named folder -> `type="self_curated"`, `display="<folder> (customized)"`, `validated=TRUE`.
    2. numeric + cached validated entry -> use it.
    3. numeric + no/`validated=FALSE` entry -> call `validate_fn(folder)`:
       - `ok` -> `type="uniprot"`, validated=TRUE, `display="<scientific_name> (<folder>)"`; persist.
       - `not_found` -> `type="self_curated"`, `display="<folder> (customized)"`; persist `validated=FALSE,type=self_curated`.
       - `network_error` + `has_feature_cache` -> `type="uniprot"`, validated=FALSE,
         `display="<folder> (annotations available, name pending)"`; persist validated=FALSE.
       - `network_error` + no cache -> `type="self_curated"` (transient), `display="<folder> (customized)"`; DO NOT persist as final (leave entry absent/validated=FALSE so it retries).
  - `is_self_curated <- (type == "self_curated")` is what consumers derive.
- **MIRROR**: NAMING_CONVENTION, ERROR_HANDLING, atomic write from `pelsa_write_feature_cache`.
- **IMPORTS**: `jsonlite` — verify it is in DESCRIPTION Imports; if not, add it +
  `@importFrom jsonlite read_json write_json` and re-run `devtools::document()`.
- **GOTCHA**: Classification must be deterministic offline EXCEPT the one validation call;
  never let the setup-box re-render trigger a network call when a cached verdict exists.
  `has_feature_cache` reflects LOCAL state (caches gitignored), not git.
- **VALIDATE**: Task 11 unit tests for all five verdict branches with an injected `validate_fn`.

### Task 5: Refresh-on-start (promote unvalidated numeric folders)
- **ACTION**: A `pelsa_refresh_species_meta_on_start(database_dir, validate_fn = pelsa_fetch_taxon)`
  helper that re-attempts validation for numeric folders whose entry is missing or
  `validated=FALSE`, rewriting `species_meta.json` on success.
- **IMPLEMENT**: List folders, classify, for each numeric-unvalidated call `validate_fn`;
  on `ok` promote to validated and persist. Call it ONCE from the PELSA container/section1
  module init (not on the reactive render path) — e.g. an `observeEvent(once = TRUE, ...)`
  or a plain call in the server body guarded so it runs once per session.
- **MIRROR**: INJECTABLE_FETCH.
- **GOTCHA**: Must run off the reactive path (once per app start), like the species refresh
  observer. Keep it non-blocking/best-effort: a failure here must not break the species list.
- **VALIDATE**: Task 11 test: seed a `validated=FALSE` numeric entry, run with a stub that now
  returns `ok`, assert the meta file is rewritten to `validated=TRUE` with the name.

### Task 6: Species picker display + refresh checklist filter
- **ACTION**: In `pelsa_setup_box_ui` (`R/tab_pelsa_section1_helpers.R:627-760`) make the
  species `selectInput` use NAMED choices, and filter the refresh checklist to UniProt.
- **IMPLEMENT**:
  - Add a `species_choices` named vector built by the caller (section1 server) via
    `pelsa_resolve_species` per folder -> `setNames(folders, displays)`. Change the UI
    param from `species` (plain) to this named vector; `choices = species_choices`,
    `selected = selected_species` (value = folder name, unchanged).
  - Refresh checklist (lines ~750): pass only `type == "uniprot"` folders (with their
    display labels) as `choices`.
- **MIRROR**: SELECTINPUT_NAMED_CHOICES.
- **GOTCHA**: `setup_state$species` stores `input$pelsa_species` = the VALUE = folder name.
  Nothing downstream changes. Only labels change. The setup box re-renders live; the caller
  must pass freshly-resolved labels each render (reading the cached meta — cheap, no network).
- **VALIDATE**: Task 11 label-format test for the three states; manual: picker shows display
  names, refresh checklist omits self-curated.

### Task 7: Volcano label + winning_gene force for self-curated
- **ACTION**: Thread an `is_self_curated` flag into `.pelsa_volcano_labels`
  (`R/tab_pelsa_volcano_helpers.R:95`) and `pelsa_build_volcano_df` (line 170) +
  `.pelsa_build_volcano_all` (line 223) + `pelsa_build_multilabel`.
- **IMPLEMENT**:
  - `.pelsa_volcano_labels(matched, key_col, is_self_curated = FALSE)`: when TRUE,
    `lid <- dt$accession` unconditionally (skip the gene fallback at lines 109-111).
  - `pelsa_build_volcano_df(..., is_self_curated = FALSE)`: pass the flag to
    `.pelsa_volcano_labels`; ALSO set `df$winning_gene <- ""` (line 270) and
    `df$PG.Genes <- NA_character_`-equivalent display when self-curated, so the tooltip
    gene resolves to empty (Task 8 reads `winning_gene`/`PG.Genes`).
  - `pelsa_build_multilabel`: same forced-accession branch (keep it the canonical single-label
    builder consistent with `.pelsa_volcano_labels`).
- **MIRROR**: VOLCANO_LABEL.
- **GOTCHA**: Default the flag `FALSE` so the UniProt path is byte-identical and existing
  volcano label tests pass unchanged. The flag comes from the resolved species struct via
  the section3 server (Task 8).
- **VALIDATE**: Task 11: self-curated -> label `<acc>_aa<pos>` even when a gene is present;
  uniprot -> unchanged fallback.

### Task 8: Section 3 — disable feature radio, replace legend, force blank tooltip gene
- **ACTION**: In `R/tab_pelsa_section3.R` gate the annotation UI on the resolved
  `is_self_curated` for the active dataset.
- **IMPLEMENT**:
  - Obtain `is_self_curated` from the analysis cache entry's species struct (Task 9 stores it).
  - Color radio (line 649): when self-curated, disable the `"feature"` option and force
    `selected = "significance"` via `shinyjs::disable` + `updateRadioButtons`.
  - Right legend column (lines 698-705): when self-curated, render the note
    `"Feature annotations unavailable - self-curated database"` instead of
    `.pelsa_feature_legend_ui()`.
  - Pinned/tooltip gene (lines 958-970 + the volcano build call): pass `is_self_curated`
    into `pelsa_build_volcano_df`; the pinned marker frame's gene becomes `""` for
    self-curated (winning_gene forced empty in Task 7).
- **MIRROR**: existing `shinyjs::toggleState` usage at `R/tab_pelsa_section1.R:664`.
- **GOTCHA**: ASCII-only R source — use a hyphen `-` not an em dash in the note string.
  Gate on resolved `type`, never on the label text.
- **VALIDATE**: Manual: select self-curated -> feature radio disabled, legend note shown,
  tooltip gene blank, labels are `<acc>_aa<pos>`.

### Task 9: Start-Analysis — resolve type, FASTA mode, skip feature cache
- **ACTION**: In `R/tab_pelsa_section1.R` Start-Analysis block (lines 916-934) and
  `pelsa_run_analysis`/validator, use the resolved struct.
- **IMPLEMENT**:
  - Resolve `sp_struct <- pelsa_resolve_species(database_dir, snapshot$species)`.
  - `fasta_map <- pelsa_read_fasta(fasta_path, mode = if (sp_struct$type == "uniprot") "uniprot" else "self_curated")`.
  - `feat_df <- if (sp_struct$type == "self_curated") pelsa_empty_feature_frame() else pelsa_read_feature_cache(species_dir)`.
  - Carry `is_self_curated` (and the struct) into the analysis cache so sections 3 read it
    without re-resolving.
- **MIRROR**: existing FASTA-read warning handler (lines 924-930) preserved.
- **GOTCHA**: `pelsa_validate_setup` (analysis_helpers:179) checks FASTA existence — self-curated
  must pass (FASTA exists) but must NOT require a feature cache. Ensure no validation rule
  demands `uniprot_features` for self-curated.
- **VALIDATE**: Integration test (Task 11) with the self-curated fixture: analysis completes,
  matched positions present, `feat_df` empty, no annotation-cache error.

### Task 10: Refresh-inputs FASTA mode
- **ACTION**: `pelsa_species_refresh_inputs` (`R/tab_pelsa_refresh_helpers.R:526`) reads the
  FASTA with `pelsa_read_fasta(fastas[[1]])`.
- **IMPLEMENT**: Pass `mode = "uniprot"`. Refresh only ever runs for `type=="uniprot"` species
  (checklist filtered in Task 6), so UniProt mode is always correct here.
- **GOTCHA**: Keep it explicit (`mode = "uniprot"`) rather than relying on the default, to
  document intent.
- **VALIDATE**: existing refresh tests pass (with migrated `9606`/`10090` literals).

### Task 11: Tests
- **ACTION**: Create the fixture + new test file; migrate species literals.
- **IMPLEMENT**:
  - `tests/testthat/fixtures/pelsa/self_curated.fasta`: ~3 proteins, Hoylesella-style headers
    (e.g. `>BalskusLab_HoyT_0001 hypothetical protein`), known sequences so position mapping
    is assertable.
  - `tests/testthat/test-pelsa-species-resolve.R`: covers
    (a) `pelsa_read_fasta(mode="self_curated")` first-token keys on the fixture;
    (b) `pelsa_classify_folder`;
    (c) `pelsa_resolve_species` five verdict branches with an injected `validate_fn`
        (ok / not_found / network+cache / network+no-cache / named);
    (d) `species_meta.json` read/write round-trip;
    (e) `pelsa_refresh_species_meta_on_start` promote-and-rewrite;
    (f) `.pelsa_volcano_labels` + `pelsa_build_multilabel` force-accession when self-curated;
    (g) `pelsa_species_display_label` formatting for all three states.
  - Migrate `"human"`/`"mouse"` -> `"9606"`/`"10090"` in `test-pelsa-refresh.R` and
    `test-pelsa-integration.R`.
- **MIRROR**: TEST_STRUCTURE; the injected-stub pattern from `test-pelsa-refresh.R`.
- **GOTCHA**: NEVER hit the live taxonomy API in tests — always inject `validate_fn`.
  Run `devtools::load_all(".")` before `devtools::test()` (tests exercise the loaded package).
- **VALIDATE**: see Validation Commands.

### Task 12: Roxygen / NAMESPACE / docs
- **ACTION**: If `jsonlite` was newly used, add to DESCRIPTION Imports + `@importFrom`, then
  `devtools::document()`. Add a short note to `CLAUDE.md` / `dev/` about the folder convention.
- **VALIDATE**: `devtools::document()` clean; `devtools::check()` no new NOTES/WARNINGS.

---

## Testing Strategy

### Unit Tests
| Test | Input | Expected Output | Edge Case? |
|---|---|---|---|
| self-curated parse | fixture FASTA, mode="self_curated" | keys = first tokens (`BalskusLab_HoyT_0001`) | header with a `|` |
| uniprot parse parity | shipped FASTA, mode="uniprot" | unchanged keys (pipe field) | regression guard |
| classify numeric | `"9606"` | `"numeric"` | leading zeros `"009606"` still numeric |
| classify named | `"hoylesellaTimonensis"` | `"named"` | mixed `"strain1"` -> named |
| resolve ok | numeric, stub ok | type=uniprot, display="Homo sapiens (9606)", validated=TRUE | — |
| resolve 404 | numeric, stub not_found | type=self_curated, "(customized)" | fake taxon |
| resolve network+cache | numeric, stub network_error, cache present | type=uniprot, validated=FALSE, "(annotations available, name pending)" | offline-with-cache |
| resolve network+no-cache | numeric, stub network_error, no cache | type=self_curated (transient) | offline-first-run |
| meta round-trip | write then read | identical struct | absent file -> list() |
| refresh-on-start | seeded validated=FALSE, stub now ok | meta rewritten validated=TRUE | promotion |
| label force | self-curated, gene present in matched | `<acc>_aa<pos>` | gene ignored |
| label uniprot | gene present | `<gene>_aa<pos>` | unchanged fallback |
| display label | each of 3 states | correct format string | — |

### Edge Cases Checklist
- [ ] Empty input (no folders) — `pelsa_list_species` returns `character(0)`; picker empty
- [ ] Numeric folder with leading zeros — still classified numeric
- [ ] Self-curated header containing `|` — first-token still wins
- [ ] Invalid types — single-string `stop()` guards on all path params
- [ ] Network failure — verdict logic distinguishes 404 vs 5xx/network
- [ ] Permission denied — `species_meta.json` write failure is best-effort (does not crash listing)

---

## Validation Commands

### Static Analysis / Docs
```r
devtools::load_all(".")
devtools::document()
```
EXPECT: NAMESPACE regenerates cleanly; no roxygen errors.

### Unit Tests (affected area)
```r
devtools::load_all(".")
devtools::test_active_file("tests/testthat/test-pelsa-species-resolve.R")
devtools::test_active_file("tests/testthat/test-pelsa-fasta.R")
devtools::test_active_file("tests/testthat/test-pelsa-refresh.R")
```
EXPECT: all pass; the fasta parity tests pass UNCHANGED.

### Full Test Suite
```r
devtools::load_all(".")
devtools::test()
```
EXPECT: no regressions (especially volcano-label and annotation-overlap tests).

### Full check
```r
devtools::check()
```
EXPECT: no new ERRORs/WARNINGs/NOTEs (ASCII-only source; deps documented).

### Manual Validation
- [ ] Launch app (`Protigy::launchApp()`); Setup ▸ Species shows `Homo sapiens (9606)`, `Mus musculus (10090)`.
- [ ] Drop a self-curated folder (e.g. `hoylesellaTimonensis` with the Balskus FASTA); it lists as `hoylesellaTimonensis (customized)`.
- [ ] Run analysis on the self-curated species: Woods shows coverage + peptides, blank feature track; volcano feature-class radio disabled; legend note shown; labels `<acc>_aa<pos>`; tooltip gene blank.
- [ ] Refresh checklist omits the self-curated species.
- [ ] Offline-then-online: confirm a numeric folder's name resolves on the next start (meta rewritten).

---

## Acceptance Criteria
- [ ] All tasks completed
- [ ] All validation commands pass
- [ ] Tests written and passing; fasta parity tests unchanged
- [ ] `devtools::check()` clean
- [ ] Matches UX design (three label states; self-curated UI degraded as specified)

## Completion Checklist
- [ ] Code follows discovered `pelsa_*` snake_case @noRd pure-helper pattern
- [ ] Error handling matches single-string `stop()` boundary style
- [ ] httr2 taxonomy fetch mirrors the existing UniProt fetch stack
- [ ] Tests inject `validate_fn` (no live network)
- [ ] No hardcoded `0.05` / species literals introduced
- [ ] ASCII-only R source (no em dashes / Unicode literals)
- [ ] `species_meta.json` + caches gitignored; not committed
- [ ] Self-contained — no questions needed during implementation

## Risks
| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| `pelsa_read_fasta` UniProt branch drift breaks parity | Low | High | Branch only the key-derivation lines; default mode="uniprot"; run parity tests unchanged |
| Network call sneaks onto the reactive render path | Medium | High | Resolver reads cached meta; validation only on cache-miss + once-on-start; never per-render |
| `species_meta.json` write race (two sessions) | Low | Medium | Atomic write (temp+rename), mirroring `pelsa_write_feature_cache` |
| Missed `"human"`/`"mouse"` literal | Medium | Medium | `git grep` sweep in Task 1 + Task 11; exclude the known non-PELSA `org_db_for_species` |
| `jsonlite` not in Imports | Low | Low | Verify + add to DESCRIPTION; `devtools::document()` |
| 404-vs-network conflation reclassifies a real species | Low | High | `req_error` 5xx-only; explicit status mapping in `pelsa_fetch_taxon` |

## Notes
- The Woods blank-track behavior needs NO new code: `pelsa_feature_track_ggplot`
  (`R/tab_pelsa_woods_helpers.R:311`) already guards `nrow(features_lanes) == 0L`, and
  self-curated passes an empty `feat_df` (Task 9). Confirm visually only.
- `pelsa_read_feature_cache` STOPS if the tsv is absent — `has_feature_cache` MUST be a
  separate `file.exists` probe (Task 4), never a try/catch around the reader on the hot path.
- `org_db_for_species` (`R/sidebar_setup_helpers_GCT-processing.R:219`) is intentionally NOT
  touched — it's the general upload-time ID-mapping helper keyed on display labels, unrelated
  to the PELSA database folder convention.
- Decision log lives in the design spec; this plan is its executable form.
