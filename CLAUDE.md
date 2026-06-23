# CLAUDE.md

Protigy — an R/Shiny package (Bioconductor + CRAN) for proteomics/multi-omics QC,
statistics, and visualization. Code lives in `R/`; the app is a set of Shiny modules.

## Critical workflow gotcha
Editing a file is NOT enough — the running app loads the *installed* package.
After any change to `R/`, reload before testing: `devtools::load_all(".")`.
After editing roxygen `@import`/`@importFrom` or `@export`, also run `devtools::document()`
to regenerate `NAMESPACE`.

## Commands (run from repo root in R)
- `devtools::load_all(".")`     # reload package after editing R/ (Cmd/Ctrl+Shift+L)
- `Protigy::launchApp()`        # launch the Shiny app
- `devtools::document()`        # regenerate NAMESPACE/man after roxygen changes
- `devtools::test()`            # run testthat suite; `devtools::test_active_file()` for one file
#   (run `devtools::load_all(".")` first — tests exercise the loaded package, not source files)
#   headless one file: Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-X.R", reporter="summary")'
#   machine-readable pass/fail: wrap the result in as.data.frame(r) and sum(df$failed)/sum(df$error)
- `devtools::check()`           # full R CMD check (also runs in CI on push)
- `source("setup.R")`           # one-time: install Bioc + CRAN deps + the package

shinytest2 (browser) tests are heavier and gated — see `dev/shinytest2_testing_guide.md`.
CI: `.github/workflows/check-standard.yaml` runs `devtools::check()` on push;
`shinytest2-on-demand.yaml` runs the browser tests on demand.

## Architecture
- `R/launchApp.R` → `app_UI` (`R/app_ui.R`) + `app_server` (`R/app_server.R`) + `app_onStart`.
- Each feature is a Shiny **module**: a `*_UI` / `*_Server` pair. Server functions are
  wired in `app_server()`; UI functions in `app_ui()`.
- File naming: `tab_<feature>.R` = module; `tab_<feature>_helpers_*.R` = its helpers;
  `sidebar_setup*.R` = upload/processing pipeline. Keep this split when adding code.
- Templates for new modules: `R/tab_TEMPLATE.R` and `R/tab_TEMPLATE_SINGLE-OME.R`.
  Full guide: `dev/module_requirements.md`.
- **PELSA** is the largest subsystem (~14 of 60 `R/` files but ~45% of R/ lines): a
  `tab_pelsa_container.R` tab with numbered sub-modules (`tab_pelsa_section1/2/3.R`) plus
  many `tab_pelsa_*_helpers.R` (analysis/annotation/export/panel/uniprot/volcano + constants).
  It extends the naming rule above — follow the existing `section`/`_helpers` split there.
  The volcano is a native plotly `scattergl` (WebGL) build: per-point `marker.color`
  restyle and `plotlyProxyInvoke("relayout", annotations=)` do NOT reliably render on
  WebGL — bake labels/annotations into `pelsa_volcano_build_plot()` and do highlights
  as addTraces/deleteTraces overlay traces (only pan/zoom/select avoid a rebuild).
- **PELSA species convention**: a species is a subfolder of `inst/database/`. Its NAME
  is the sole signal (`R/tab_pelsa_species_resolve.R::pelsa_resolve_species`): an
  all-digits name (`9606`, `10090`) is a UniProt taxon code (pipe-aware FASTA parse +
  UniProt annotation fetch + name validated via `rest.uniprot.org/taxonomy/{id}`); any
  other name is a self-curated species (first-token FASTA parse, NO annotation fetch,
  annotation UI disabled, accession-based labels). Verdicts cache in a gitignored
  `inst/database/species_meta.json`. The reactive render path resolves CACHE-ONLY
  (`allow_fetch = FALSE`); network is touched only at Start-Analysis and once per app
  start (`pelsa_refresh_species_meta_on_start`). `pelsa_read_fasta(path, mode=)` picks
  the parse mode from the resolved type.
- **PELSA feature cache = resolved vs feature-present**: `pelsa_fetch_uniprot` returns
  `features` (>=1 row per resolved+feature-bearing accession), `zero_feature` (resolved,
  entry returned, 0 features), and `unresolved` (no entry). "Resolved" = ENTRY returned,
  not feature presence. 0-feature accessions are persisted as SENTINEL rows
  (`feature_class="none"`, NA coords) so `cache$accession` includes them — this is what
  lets INCREMENTAL refresh skip them (universe = `(dataset U fasta) - cache$accession`).
  FULL refresh wipes `<species>/` except `fasta/` then rebuilds from the FASTA only.
- **`pelsa_annotate_features` soft-fails on a corrupt cache** (warn + drop the row, never
  error) and silently drops sentinel rows. Any NA-aware predicate there MUST be NA-safe:
  `feature_class` can be NA from a blank TSV cell, and a bare `== "none"` yields NA that
  crashes `if (any(...))`.

## Data-flow contract (passed into every module server)
- `GCTs_and_params()` — reactiveVal with `$GCTs` (named list of per-ome cmapR GCTs),
  `$GCTs_merged` (all omes, ome identified by the `protigy.ome` rdesc column),
  and `$parameters` (per-ome setup params). Update only during setup, and replace the
  WHOLE object (it is one giant reactiveVal — no per-field assignment).
- `globals` — reactiveValues; `globals$colors`, `globals$default_ome`.
- `GCTs_original` — reactiveVal of the unprocessed uploaded GCTs.
- Multi-ome data uses the reserved name `"multi_ome"` in named lists.
See `dev/module_requirements.md` for the full contract and export pattern.

## Export pattern
Modules return a nested list `[[ome]] -> list(name = export_function(dir_name))`; each
`export_function` writes one file into `dir_name`. `app_server()` gathers these into
`all_exports`, and `R/tab_export.R` iterates them, dumps params/colors as YAML, and zips.
Export functions re-generate their output from scratch at export time (they do not reuse
on-screen rendered objects). See `dev/module_requirements.md` → "Exporting from a Module".

## Conventions / gotchas
- **Uploads are read with `readr::read_tsv`/`read_delim`** (`R/sidebar_setup_helpers_csv-excel-processing.R`),
  which renders a missing cell as `NA`, not `""`. When reproducing a setup/PELSA
  data-handling bug, read with `readr` (NOT `read.delim`, which gives `""`) or the
  NA-vs-blank divergence will hide the failure.
- **`ns()`**: required for every `inputId`/`outputId` in module UI and inside `renderUI()`;
  do NOT use it when referencing `input$`/`output$` or in `update*Input()`. Rules + examples
  in `dev/README.md`.
- **`.data$col` / `.data[[var]]`** in dplyr/ggplot2 to avoid `R CMD check` global-variable
  notes.
- **Dependencies live in two places**: `DESCRIPTION` Imports AND a roxygen `@import`/
  `@importFrom` (`R/protigy-package.R` or at the function); re-run `devtools::document()` after.
  Note: `%||%` is NOT base R before 4.4 -- it must be imported from rlang (already in
  `R/protigy-package.R`); don't assume it's globally available.
- **GCT manipulation: prefer cmapR over hand-rolling.** Use `subset_gct(g, rid=, cid=)`
  for id-based subset/reorder, and `mat()`/`meta()`/`ids()`/`melt_gct()` accessors,
  rather than rebuilding `data.frame`s from `mat()`+rownames (which silently mangles
  non-syntactic sample names and can desync rdesc/cdesc order). cmapR is already an Import.
- **Significance cutoff is shared**: the PELSA volcano and the Statistics tab both read
  `stat_params()[[ome]]$cutoff` (set in Statistics > Summary, code in `R/tab_stat_setup.R` +
  `R/tab_stat_summary_helpers.R`). Don't hardcode `0.05`.
- Reusable helpers: `R/utilities.R`.
- **ASCII-only R source**: no literal Unicode in `R/`; use `\uXXXX` escapes (e.g.
  `"●"` for a filled bullet). Enforced in practice — non-ASCII bytes break `R CMD check`.

## Test data
`data(brca_retrospective_v5.0_proteome_gct)` (also `_phosphoproteome_`, `_rnaseq_`);
sample files in `inst/extdata/`.
Tests live in `tests/testthat/test-*.R` (~46 files, 13 of them `test-pelsa-*`). PELSA tests build on synthetic ground-truth
fixtures in `tests/testthat/fixtures/pelsa/` (`generate_synthetic.R` + canned UniProt
JSON) — prefer these over real data for deterministic assertions.
Adding/removing a field on a list-returning helper (e.g. `pelsa_fetch_uniprot`) breaks
`expect_named()` contract tests in sibling files — grep the field set across
`tests/testthat/` before changing it.
