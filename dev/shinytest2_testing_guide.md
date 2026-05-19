# shinytest2 testing guide for Protigy

## Why this exists

Protigy now includes browser-based integration tests using `shinytest2`.
These tests complement unit tests by exercising real app behavior: upload flows,
reactive UI state, conditional panels, and tab navigation.

## Prerequisites

Before running `shinytest2` tests, ensure all of the following:

1. R package dependencies are installed:
   - `testthat`
   - `shinytest2`
   - `chromote`
2. A Chrome/Chromium browser is installed and discoverable on your machine.
3. You are running commands from the package root.

Notes:
- The tests call `skip_if_no_shinytest2()` and will auto-skip if dependencies
  or Chrome are missing.
- You can explicitly disable these tests with `PROTIGY_SKIP_SHINYTEST2=true`.

## Enable/disable toggle

### Disable shinytest2 tests (fast local iteration)

```bash
export PROTIGY_SKIP_SHINYTEST2=true
```

### Enable shinytest2 tests

```bash
unset PROTIGY_SKIP_SHINYTEST2
```

If using an IDE terminal session, set/unset this variable in the same terminal
you run tests from.

## Step-by-step: run shinytest2 tests during development

1. Go to the repo root.
2. Enable tests (`unset PROTIGY_SKIP_SHINYTEST2`).
3. Reload package code.
4. Run a targeted test file first (faster feedback).
5. If targeted tests pass, run the full test suite before opening/merging PR.

Example R commands:

```r
devtools::load_all(".")
devtools::test(filter = "smoke-shinytest2")
devtools::test(filter = "file-upload-shinytest2")
devtools::test()
```

You can also run specific files:

```r
testthat::test_file("tests/testthat/test-smoke-shinytest2.R")
testthat::test_file("tests/testthat/test-file-upload-shinytest2.R")
```

## When to run shinytest2 tests

Run these tests whenever changes may affect rendered app behavior, including:

- UI structure, labels, tabs, or input IDs.
- `conditionalPanel` logic and visibility behavior.
- Server reactivity that drives UI state.
- File upload/setup workflow changes (GCT, CSV, TSV, XLSX).
- Feature flag behavior (including Spectronaut-related paths).
- Cross-module interactions that can break user flows.

## When you can usually skip (locally)

You can usually skip `shinytest2` in early local iteration for:

- Pure utility/helper function changes with strong unit coverage.
- Internal refactors that do not touch UI behavior or reactive wiring.
- Documentation-only changes.

Even in these cases, run at least smoke integration tests before merge.

## Suggested team workflow

1. During implementation:
   - Run unit tests frequently.
   - Run targeted `shinytest2` tests for changed workflows.
2. Before PR update/merge:
   - Run full `shinytest2`-relevant tests (or full `devtools::test()`).
3. In CI:
   - Keep browser integration tests enabled for branch protection.

## Troubleshooting

### Tests are all skipped

Check:
- `PROTIGY_SKIP_SHINYTEST2` is not set to `true`.
- `shinytest2` and `chromote` are installed.
- Chrome/Chromium is installed and available.

### Flaky failures/timeouts

- Re-run the failing file once to rule out transient timing.
- Confirm no heavy background load is slowing the machine.
- If reproducible, review wait logic in `tests/testthat/helper-shinytest2.R`.

### Spectronaut tests

Some Spectronaut-related tests are scaffolding for not-yet-implemented UI
paths and are intentionally skipped until that module work lands.
