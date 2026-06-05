# LM Sandbox

Ground-truth scaffolding for the Linear Model migration.

## Purpose

Provide a reproducible, version-pinned set of synthetic datasets and "golden"
limma outputs that the production `lm.regression()` and its derivatives must
match exactly (within tolerance). This is **golden-file regression testing**,
not manual derivation. The oracle is limma run in isolation in this sandbox;
the assumption is that limma is correct.

## Two testing layers

This sandbox feeds **two complementary** test layers:

1. **Golden-file regression** (`tests/testthat/test-lm-golden-regression.R`):
   does `lm.regression()` match limma run in isolation? Oracle = limma; catches
   *drift*. Uses the `type*`/`continuous`/`intensity` fixtures + `golden/*.rds`.
2. **Ground-truth correctness** (`tests/testthat/test-lm-ground-truth.R`): is the
   statistical backbone *correct* against effects we planted ourselves? Catches
   *wrongness* that golden regeneration would hide (because a buggy path would
   produce a buggy golden). Uses the `gt_*` fixtures; no golden files — it asserts
   statistical properties (uniform null p-values, FDR control, sign algebra,
   recovery/power, blocking) directly.

## Layout

- `synthesize_datasets.R` — generates five `data/<name>.rds` fixtures:
  `type1_rm_with_groups`, `type2_rm_only`, `type3_contrasts`,
  `continuous_covariate`, `intensity_trend`. Seeds and structure are baked in.
- `synthesize_ground_truth.R` — generates five `data/gt_*.rds` fixtures for the
  ground-truth layer: `gt_pure_null` (calibration/FDR), `gt_sign_convention`
  (sign & magnitude algebra), `gt_power_recovery` (sensitivity/specificity),
  `gt_blocking` (within-subject correlation; blocking must beat the unblocked
  fit), `gt_rank_deficient` (graceful degradation on an aliased design). Seeds
  201–205; regenerates byte-identically. The ground-truth test auto-regenerates
  these if missing, so they need not be committed.
- `manual/run_*.R` — one script per golden fixture; runs limma directly (no
  protigy wrappers) and saves the canonical output frame to `golden/<name>.rds`.
- `golden/*.rds` — committed expected outputs (golden layer only).
- `compare/assert_equivalent.R` — tolerance-based comparison helper used by
  both the sandbox self-check and the `testthat` regression suite.

## Tolerances

| Statistic | Type | Threshold |
|---|---|---|
| `logFC`, `P.Value`, `adj.P.Val` (topTable) | absolute | `1e-6` |
| `consensus.correlation` (duplicateCorrelation) | relative | `1e-3` |
| `F` statistic (per-factor F test) | absolute | `1e-4` |

These hold for fixed seed + limma version. Larger drift across limma versions
will surface as a failure; the fix is to regenerate the goldens after
inspecting the diff.

## Environment

- limma `3.66.0` (Bioconductor)
- statmod `1.5.1`
- R `4.5.x`

Captured at first regeneration; reflect in `docs/lm-migration-implementation-log.md`.

## Workflow

```sh
Rscript tests/lm-sandbox/synthesize_datasets.R
Rscript tests/lm-sandbox/manual/run_type1.R
Rscript tests/lm-sandbox/manual/run_type2.R
Rscript tests/lm-sandbox/manual/run_type3.R
Rscript tests/lm-sandbox/manual/run_continuous.R
Rscript tests/lm-sandbox/manual/run_intensity_trend.R
Rscript tests/lm-sandbox/compare/assert_equivalent.R
```

`assert_equivalent.R` doubles as a self-check: it re-runs limma and confirms it
matches the saved goldens. CI should never regenerate; humans do (and update
the implementation log).
