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
- `devtools::check()`           # full R CMD check (also runs in CI on push)
- `source("setup.R")`           # one-time: install Bioc + CRAN deps + the package

shinytest2 (browser) tests are heavier and gated — see `dev/shinytest2_testing_guide.md`.

## Architecture
- `R/launchApp.R` → `app_UI` (`R/app_ui.R`) + `app_server` (`R/app_server.R`) + `app_onStart`.
- Each feature is a Shiny **module**: a `*_UI` / `*_Server` pair. Server functions are
  wired in `app_server()`; UI functions in `app_ui()`.
- File naming: `tab_<feature>.R` = module; `tab_<feature>_helpers_*.R` = its helpers;
  `sidebar_setup*.R` = upload/processing pipeline. Keep this split when adding code.
- Templates for new modules: `R/tab_TEMPLATE.R` and `R/tab_TEMPLATE_SINGLE-OME.R`.
  Full guide: `dev/module_requirements.md`.
- **PELSA** is the largest subsystem (~40% of `R/`): a `tab_pelsa_container.R` tab with
  numbered sub-modules (`tab_pelsa_section1/2/3.R`) plus many `tab_pelsa_*_helpers.R`.
  It extends the naming rule above — follow the existing `section`/`_helpers` split there.

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
- **`ns()`**: required for every `inputId`/`outputId` in module UI and inside `renderUI()`;
  do NOT use it when referencing `input$`/`output$` or in `update*Input()`. Rules + examples
  in `dev/README.md`.
- **`.data$col` / `.data[[var]]`** in dplyr/ggplot2 to avoid `R CMD check` global-variable
  notes.
- **Dependencies live in two places**: `DESCRIPTION` Imports AND a roxygen `@import`/
  `@importFrom` (`R/protigy-package.R` or at the function); re-run `devtools::document()` after.
- Reusable helpers: `R/utilities.R`.
- **ASCII-only R source**: no literal Unicode in `R/`; use `\uXXXX` escapes (e.g.
  `"●"` for a filled bullet). Enforced in practice — non-ASCII bytes break `R CMD check`.

## Test data
`data(brca_retrospective_v5.0_proteome_gct)` (also `_phosphoproteome_`, `_rnaseq_`);
sample files in `inst/extdata/`.
Tests live in `tests/testthat/test-*.R`. PELSA tests build on synthetic ground-truth
fixtures in `tests/testthat/fixtures/pelsa/` (`generate_synthetic.R` + canned UniProt
JSON) — prefer these over real data for deterministic assertions.
