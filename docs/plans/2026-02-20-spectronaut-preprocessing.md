# Plan: Spectronaut Report Preprocessing + Data Preview

## Context

Users working with Spectronaut proteomics software export pivot reports (TSV) with raw run-label column names and multiple quantification metrics (PG.Quantity, PG.IBAQ, etc.). Currently they must manually preprocess these files before loading into ProTIGY. This feature adds in-app Spectronaut preprocessing: ID column creation, condition-based column renaming, quantification metric selection, and a live data preview panel. It also adds a gene symbol delimiter option for Spectronaut-style semicolon-delimited gene columns.

## Branch

`feature/spectronaut-preprocessing`

## Architecture

The CSV/Excel workflow currently flows:
```
Upload → Labels → Identifier Selection → Exp Design Upload → processCSVExcel → GCT param setup
```

After this feature, a new Spectronaut step is inserted after Identifier Selection:
```
Upload → Labels → Identifier Selection → [Spectronaut step]
  ├── Spectronaut ON → preprocess (ID col + column renaming) → exp design upload → GCT param setup
  └── Spectronaut OFF → exp design upload (unchanged) → GCT param setup
```

The condition setup file renames columns to human-readable condition names (e.g. `wholeCell_R1`) to make the experimental design step easier — the user still uploads an exp design file, but now the column names are meaningful instead of raw run labels.

The data preview panel lives in `dashboardBody` above the `navbarPage`, shows first 20 rows, and updates live as Spectronaut options change.

---

## Files to Create

### `R/sidebar_setup_helpers_spectronaut.R`

Pure processing functions (no Shiny deps — fully testable):

1. **`extract_protigy_id(data, source_column, separator = ";")`**
2. **`read_spectronaut_condition_setup(file_path)`**
3. **`detect_quant_suffixes(data_columns, run_labels)`**
4. **`apply_spectronaut_condition_setup(data, condition_setup, selected_suffix, merge_condition_replicate = FALSE)`**
5. **`buildExpDesignFromConditionSetup(condition_data, merge_condition_replicate = FALSE)`**
6. **`split_gene_symbol_column(rdesc, column, separator = ";")`**
7. **`spectronautSetupUI(ns, data_columns)`** — UI function

## Files to Modify

- `R/sidebar_setup.R` — new reactive state, step routing, observeEvent handlers, preview reactivity
- `R/sidebar_setup_helpers_csv-excel-processing.R` — add `read_uploaded_data_preview()`
- `R/sidebar_setup_helpers_shiny.R` — update `gctSetupUI()` with gene symbol split block
- `R/sidebar_setup_helpers_GCT-processing.R` — add spectronaut gene symbol split in pipeline
- `R/app_ui.R` — add data preview panel above navbarPage
- `inst/setup_parameters/setupDefaults.yaml` — add spectronaut keys
- `R/protigy-package.R` — add DTOutput/renderDT imports

## Verification

1. Unit tests in `tests/testthat/test-spectronaut-processing.R`
2. Manual smoke test with Spectronaut pivot TSV
3. `devtools::check()` — no new warnings/errors
4. `devtools::test()` — all existing tests pass
