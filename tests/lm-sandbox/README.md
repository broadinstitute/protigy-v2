# LM Sandbox

Ground-truth scaffolding for the Linear Model migration.

## Purpose

Provide a reproducible, version-pinned set of synthetic datasets and "golden"
limma outputs that the production `lm.regression()` and its derivatives must
match exactly (within tolerance). This is **golden-file regression testing**,
not manual derivation. The oracle is limma run in isolation in this sandbox;
the assumption is that limma is correct.

## Layout

- `synthesize_datasets.R` — generates five `data/<name>.rds` fixtures:
  `type1_rm_with_groups`, `type2_rm_only`, `type3_contrasts`,
  `continuous_covariate`, `intensity_trend`. Seeds and structure are baked in.
- `manual/run_*.R` — one script per fixture; runs limma directly (no protigy
  wrappers) and saves the canonical output frame to `golden/<name>.rds`.
- `golden/*.rds` — committed expected outputs.
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
