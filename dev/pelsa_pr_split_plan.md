# PELSA PR-Split Plan

> Regenerated 2026-06-22 from a deep-read workflow over `git diff main...feat/pelsa-integration`
> (267 commits, 120 files). Supersedes the earlier commit-based cherry-pick plan, which is now
> obsolete: 17 commits interleave PELSA and non-PELSA source, so the split must be done at the
> **working-tree / hunk level**, not by replaying commits.

## 1. Verdict & shape

The split is **not clean at the file level**. ~15 net-new files are pure PELSA; ~30+ modified/new
files are pure OOS; **three files are mixed** and need attention:

- `R/protigy-package.R` — PELSA imports + OOS dead-dep removals + shared `%||%`/`matrixStats` (genuine hunk-level surgery)
- `R/app_server.R` — *looks* mixed but is **all-PELSA**; take whole, just don't drag adjacent unchanged context
- `tests/testthat/test-misc-helpers.R` — OOS color/export/heatmap tests + a trailing PELSA edge-case section

One real code cross-edge (`tab_qc_cv_helpers.R` -> `pelsa_delinearize`, **OOS -> PELSA**) and two shared
package-level deps (`%||%`, `matrixStats`). **PR#1 (PELSA) is self-contained; PR#2 (OOS) depends on PR#1.**

## 2. PR #1 (PELSA) contents

- **New PELSA source (take whole):** every new `R/tab_pelsa_*.R` (container, section1/2/3, *_helpers, constants, volcano, panel, annotation, uniprot, species_resolve, analysis, export_helpers).
- **New PELSA assets (take whole):** `inst/database/9606/fasta/.gitkeep`, `inst/pelsa/compound_markers.yaml`, `inst/custom.css` (all +182 lines are `.pelsa-*` selectors).
- **New PELSA tests/fixtures (take whole):** `tests/testthat/fixtures/pelsa/*`, all `test-pelsa-*.R` (analysis, annotation, container-ui, fixtures, integration, peptide-pipeline, section3, setup, species, summary, uniprot-cache, uniprot).
- **Modified files — take WHOLE (all-PELSA despite location):** `R/app_server.R` (module wiring, export gather), `R/app_ui.R` (`navbarMenu("PELSA", ...)` mount).
- **Mixed file — PELSA hunks only:** `R/protigy-package.R` -> add stringi / data.table / httr2 / jsonlite `write_json` / IRanges / ragg imports, the full `utils::globalVariables(...)` block, rlang `%||%`, and `matrixStats rowMeans2`. (See §4.)
- **Mixed test — PELSA section only:** lift the trailing "PELSA edge cases" block out of `test-misc-helpers.R` into a PELSA test file (`pelsa_safe_name`, `pelsa_depth_summary`, `pelsa_parse_uniprot_json`, `pelsa_read_fasta`).
- **No new `man/pelsa_*.Rd`** (PELSA helpers are internal). Run `devtools::document()` on the branch to confirm no NAMESPACE churn.

## 3. PR #2 (out-of-scope) contents — bucketed

- **QC:** `tab_qc_PCA.R`, `tab_qc_PCA_helpers.R`, `tab_qc_boxplots_helpers.R`, `tab_qc_correlation.R`, `tab_qc_correlation_helpers.R`, `tab_qc_profile_plots_helpers.R`, `tab_qc_cv.R`, `tab_summary_helpers.R`. Tests: `test-qc-gradient-midpoint.R`, `test-qc-min-samples.R`, `test-qc-module.R`, `test-corr-heatmap-null-colors.R`.
- **QC<->PELSA cross-dep file (OOS, depends on PR#1):** `R/tab_qc_cv_helpers.R` (adds `log_base`, calls `pelsa_delinearize`). See §5.
- **Statistics:** `tab_stat_setup_helpers.R`, `tab_stat_plot.R` (ascii + `plotly::toWebGL()` Statistics-volcano perf), `tab_stat_plot_helpers.R`, `tab_stat_summary.R`. Tests: `test-batch-contrast.R`, `test-statistics-module.R`, `test-stat-summary-pvals.R`.
- **Setup-perf:** `sidebar_setup.R`, `sidebar_setup_helpers_GCT-processing.R`, `sidebar_setup_helpers_data-filtering.R`, `sidebar_setup_helpers_normalization.R`, `sidebar_setup_helpers_shiny.R`, **`sidebar_setup_helpers_discrete-cache.R` (new file — OOS, NOT PELSA)**. Tests: `test-discrete-cache.R`, `test-gct-cdesc-header-read.R`, `test-perf-phase3.R`, `test-setup-pipeline-hygiene.R`, `test-sidebar-setup-remove-observers.R`, `test-data-filtering.R`, `test-file-upload-removal.R`, `test-gct-processing.R`, `test-sidebar_setup.R`, `test-shiny-helpers.R`, `test-intensity-data-param.R`.
- **Export:** `tab_export.R`. Tests: `test-export-hygiene.R`, `test-export-roundtrip.R`.
- **Cleanup (ASCII/R-CMD-check):** `inst/extdata/spectronaut-ui-extdata/generate_test_data.R`, ascii hunks in `tab_qc_cv_helpers.R`. Test: `test-ascii-source.R`.
- **Shared-utils:** `tab_customize_helpers_color-mod.R` (`.data$` NSE), `utilities.R` (INT-3 + new `min_samples_message()`). Test: `test-utilities.R`.
- **Man pages (color helpers + author sync, regenerated in `463ba56`):** `Protigy-package.Rd`, `colors_structure_signature.Rd`, `get_preset_palette.Rd`, `import_colors_from_yaml.Rd`, `import_colors_from_yaml_full.Rd`, `is_valid_hex_color.Rd`, `normalize_hex_color.Rd`, `export_colors_to_yaml.Rd`.
- **Other OOS tests/harness:** `test-multi-ome_heatmap.R`, `test-error-handling.R`, `test-regression-pipeline.R`, `test-volcano-labeling.R`, `test-volcano-ui.R` (Statistics volcano), `test-qc_cv_helpers.R` (comment-only pelsa mention), `test-summary-dataset.R`, `test-summary-workflow.R`, `fixtures/generate-gold-standard.R`, the 5 shinytest2 files, `apps/full-app/app.R`, `apps/full-app-spectronaut/app.R`.
- **`protigy-package.R` OOS hunks:** remove `@importFrom WriteXLS WriteXLS`, `future plan availableCores`, `furrr future_map future_map2`; widen `matrixStats` import to add `rowSds` + `colMedians`.

## 4. Files needing hunk-level surgery

### `R/protigy-package.R` (the one unavoidable shared file)
| Change | PR | Notes |
|---|---|---|
| `+ rlang %||%` | BOTH (union) | PELSA (10 files) + pre-existing OOS usage. PR#1 adds it; PR#2 diff becomes a no-op on this line. |
| `+ matrixStats rowSds rowMeans2 rowMedians colMedians` | SPLIT/union | `rowMeans2`=PR#1 (PELSA); `rowSds`+`colMedians`=PR#2 (OOS); `rowMedians` unused. |
| stringi / data.table / httr2 / jsonlite write_json / IRanges / ragg | PR#1 | PELSA-only call sites. |
| `utils::globalVariables(c(...))` block | PR#1 | All PELSA NSE symbols. |
| `- WriteXLS` / `- future` / `- furrr` removals | PR#2 | Dead-dep trim (PELSA uses `future::plan` fully-qualified). |

Run `devtools::document()` on **each** branch. Expect a **merge conflict on `protigy-package.R` + `NAMESPACE`** when the second PR rebases — resolve by **union** (no logic coupling).

### `tests/testthat/test-misc-helpers.R`
- PR#1: trailing "PELSA edge cases" block -> move into a PELSA test file.
- PR#2: everything else (color/export/heatmap).
- Fallback: keep whole in PR#2 and accept PELSA tests are red until PR#1 merges.

## 5. Cross-dependency & merge order

| Edge | Caller (PR) | Symbol | Definer (PR) | Direction |
|---|---|---|---|---|
| 1 (only real) | `tab_qc_cv_helpers.R:58` (PR#2) | `pelsa_delinearize` | `tab_pelsa_analysis_helpers.R:1843` (PR#1) | OOS -> PELSA |
| shared | `protigy-package.R` | `%||%` (rlang) | — | both |
| shared | `protigy-package.R` | `matrixStats` | — | both |

- **No PR#1 -> PR#2 edge.** All 8 net-new OOS symbols grepped against every `tab_pelsa_*.R`: zero hits.
- `add_css_attributes` (utilities.R) and `parse_protein_search_input` (tab_stat_plot_helpers.R) are PELSA depending on **stable `main` code** (definitions untouched by either PR) — no constraint.

**Resolution options:**
- **Option A (zero-effort, recommended):** keep `pelsa_delinearize` in PELSA; merge **PR#1 first**, then PR#2. Trade-off: PR#2 won't pass `R CMD check` in isolation.
- **Option B (independent CI):** relocate `pelsa_delinearize` (~25-line pure numeric util) to `R/utilities.R` in PR#1; update its 5 PELSA call sites + test grep-guards. Both PRs then order-independent.

## 6. Uncertain / human-decision items

1. **`docs/sequence-coverage-testing/*` (~60k lines, mostly generated CSV).** Standalone PELSA coverage-validation harness. **Recommendation: DROP from both PRs** (port the script to a `tests/testthat/` fixture if a regression guard is wanted; don't track the CSVs). If kept, goes to PR#1.
2. **Option A vs B for `pelsa_delinearize`** (§5).
3. **`test-misc-helpers.R` split vs keep-whole** (§4).
4. **`man/Protigy-package.Rd` author-list change** — bucketed PR#2; confirm if PELSA-contributor sync should ride PR#1. Low risk.
5. **One-sample T-test `id`-column fix (`tab_stat_setup_helpers.R`)** — OOS, but PELSA volcano consumes `stat.testing()`'s `id` output (soft coupling, no code dep). Stays PR#2; flag for awareness.

## 7. Mechanical execution recipe

Cherry-pick won't work (17 interleaved commits). Build both branches by selecting **content** vs `main`.

```bash
git fetch origin
FEATURE=feat/pelsa-integration
git diff main...$FEATURE > /tmp/full.diff
git diff --name-status main...$FEATURE > /tmp/files.txt
```

### PR#1 (PELSA) — branch off `main`, add PELSA content only
```bash
git switch main && git switch -c pr/pelsa-subsystem

# new PELSA files + tests + fixtures + assets
git checkout $FEATURE -- $(git diff --name-only --diff-filter=A main...$FEATURE \
  | grep -E 'R/tab_pelsa_|tests/testthat/test-pelsa-|tests/testthat/fixtures/pelsa/|inst/pelsa/|inst/database/9606/fasta/.gitkeep')

# all-PELSA wiring + asset
git checkout $FEATURE -- R/app_server.R R/app_ui.R inst/custom.css

# mixed protigy-package.R: simplest correct version =
#   main's version + PELSA import lines + globalVariables block + %||% + matrixStats rowMeans2,
#   leaving WriteXLS/future/furrr INTACT (their removal is OOS).
git checkout main -- R/protigy-package.R   # then hand-add the PELSA lines

Rscript -e 'devtools::document()'
Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_dir("tests/testthat", filter="pelsa", reporter="summary")'
git add -A && git commit -m "feat(pelsa): PELSA subsystem (split PR #1)"
```

### PR#2 (OOS) — branch off PR#1 (Option A) or `main` (Option B)
```bash
git switch pr/pelsa-subsystem && git switch -c pr/oos-qc-stat-setup-export

# all-OOS files (take whole) — see §3 for the full list:
git checkout $FEATURE -- \
  R/tab_qc_PCA.R R/tab_qc_PCA_helpers.R R/tab_qc_boxplots_helpers.R \
  R/tab_qc_correlation.R R/tab_qc_correlation_helpers.R \
  R/tab_qc_profile_plots_helpers.R R/tab_qc_cv.R R/tab_qc_cv_helpers.R \
  R/tab_summary_helpers.R \
  R/tab_stat_setup_helpers.R R/tab_stat_plot.R R/tab_stat_plot_helpers.R R/tab_stat_summary.R \
  R/sidebar_setup.R R/sidebar_setup_helpers_GCT-processing.R \
  R/sidebar_setup_helpers_data-filtering.R R/sidebar_setup_helpers_normalization.R \
  R/sidebar_setup_helpers_shiny.R R/sidebar_setup_helpers_discrete-cache.R \
  R/tab_export.R R/tab_customize_helpers_color-mod.R R/utilities.R \
  inst/extdata/spectronaut-ui-extdata/generate_test_data.R \
  man/Protigy-package.Rd man/colors_structure_signature.Rd man/get_preset_palette.Rd \
  man/import_colors_from_yaml.Rd man/import_colors_from_yaml_full.Rd \
  man/is_valid_hex_color.Rd man/normalize_hex_color.Rd man/export_colors_to_yaml.Rd
# + all OOS tests (see §3)

# mixed protigy-package.R: re-apply OOS hunks on PR#1's version
#   (remove WriteXLS/future/furrr; widen matrixStats to add rowSds + colMedians).
# mixed test-misc-helpers.R: take it, then delete the trailing PELSA edge-case block.
git checkout $FEATURE -- tests/testthat/test-misc-helpers.R

Rscript -e 'devtools::document()'
Rscript -e 'suppressMessages(devtools::load_all(".")); df<-as.data.frame(devtools::test(reporter="summary")); cat("failed=",sum(df$failed)," errors=",sum(df$error),"\n")'
git add -A && git commit -m "fix/perf/chore: out-of-scope QC/stat/setup/export/cleanup (split PR #2)"
```

**Option B variant:** branch PR#2 off `main`; in PR#1 also `git mv` `pelsa_delinearize` into `R/utilities.R` (update 5 PELSA call sites + test grep-guards). PR#2 then checks standalone.

### Verification
- **PR#1:** `load_all` + PELSA tests + `devtools::check()`, standalone.
- **PR#2 (Option A):** check stacked **on top of PR#1**; full `devtools::test()` with `sum(failed)+sum(error)==0`.
- **Expected conflict:** `R/protigy-package.R` + `NAMESPACE` — resolve by union.
