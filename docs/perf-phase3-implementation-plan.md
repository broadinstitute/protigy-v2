# Phase 3 Implementation Plan — matrixStats Numerics (dp-1b, dp-sd, dp-norm)

**Goal:** replace per-row / per-column `apply()` loops in the numeric processing path with
compiled `matrixStats` / base routines that produce **identical** results, plus one latent
crash bugfix (dp-1b). Validation is empirical: synthetic ground-truth datasets run through the
OLD and NEW implementations as standalone scripts, compared for exact equivalence, before any
in-app edit.

**Pre-req status:** `matrixStats` (v1.5.0) is already installed and already imported in
`R/protigy-package.R:44` (`rowSds`, `colMedians`, `rowMedians`, `rowMeans2`). We must ADD
`colMads` to that `@importFrom`, confirm `matrixStats` is in `DESCRIPTION` Imports, and re-run
`devtools::document()`. No other dependency change.

---

## The three changes

### dp-1b — `perform_missing_filter` (`R/sidebar_setup_helpers_GCT-processing.R:~907`, called at L884)

**Current:**
```r
perform_missing_filter <- function(data, max_missing) {
  missing_percent <- apply(data, 1, function(x) sum(is.na(x))/length(x))
  data <- data[missing_percent <= max_missing/100, ]
  return(data)
}
```
Two problems: (1) per-row R closure; (2) no `drop = FALSE` — if exactly ONE row passes the
filter, `data[mask, ]` collapses the matrix to a named numeric vector, and the downstream
`data.frame(data, id = rownames(data))` crashes (a vector has no `rownames`).

**New:**
```r
perform_missing_filter <- function(data, max_missing) {
  missing_percent <- rowMeans(is.na(data))         # base; identical to sum(is.na)/length per row
  data[missing_percent <= max_missing / 100, , drop = FALSE]
}
```
`rowMeans(is.na(data))` = mean of a 0/1 logical row = fraction NA = `sum(is.na(x))/length(x)`.
Exact equivalence (same arithmetic, IEEE-deterministic). Behavior change is ONLY the 1-row case
(crash -> correct), which is the bugfix.

### dp-sd — `sd.filter` (`R/sidebar_setup_helpers_data-filtering.R:28`)

**Current:** `sd.tab <- apply(tab, 1, sd, na.rm=T)`
**New:** `sd.tab <- matrixStats::rowSds(tab, na.rm = TRUE); names(sd.tab) <- rownames(tab)`

`rowSds` uses the same sample (n-1) variance and the same `na.rm` semantics as `stats::sd`.
We re-attach `names` so the returned `values.filtered` list (which stores `filt.idx`) is
byte-identical, not merely set-identical. Downstream uses `sd.tab` only via `quantile(..., na.rm=TRUE)`
and `which(sd.tab < val)` / `which(sd.tab >= val)`, both of which ignore NA/NaN identically, so even
the all-NA / single-value rows (where `sd`=NA and `rowSds` may=NaN) yield an identical filtered set.

### dp-norm — `normalize.data.helper` (`R/sidebar_setup_helpers_normalization.R`)

Replace the per-column `apply(data, 2, median)` and `apply(data, 2, (x-median)/mad)` in the four
median/MAD branches (`Median`, `Median (non-zero)`, `Median-MAD`, `Median-MAD (non-zero)`).

**Current pattern (Median-MAD shown):**
```r
data.norm <- apply(data, 2, function(x) (x - median(x, na.rm=T))/mad(x, na.rm=T))
data.norm <- safe_set_colnames(data.norm, data)
```
**New pattern:**
```r
med <- matrixStats::colMedians(data, na.rm = TRUE)
md  <- matrixStats::colMads(data, na.rm = TRUE)        # default constant 1.4826, center = colMedians
data.norm <- sweep(sweep(data, 2L, med, "-"), 2L, md, "/")
dimnames(data.norm) <- dimnames(data)
```
`Median`-only branch drops the `/mad` step. The `all_medians` median-of-medians shift
(`+ median(all_medians, na.rm=T)`) is unchanged and reuses the `med` vector.

**Out of dp-norm scope (left as-is):** `Quantile`, `VSN`, `2-component`, `Upper-quartile`,
`Median (non-zero)`'s/`Median-MAD`'s upstream behavior beyond the median/MAD compute. We touch
only the median/MAD arithmetic.

---

## Equivalence concerns to PROVE empirically (not assume)

| # | Risk | Why it might differ | Test |
|---|------|---------------------|------|
| E1 | `colMedians` vs `median` | even-n averaging, NA drop | random matrices incl. even/odd row counts |
| E2 | `colMads` vs `stats::mad` | constant (1.4826), center (median), low-n | random + hand cases |
| E3 | `rowSds` vs `sd` | n-1 divisor, na.rm | random matrices |
| E4 | **NaN vs NA** on degenerate cols/rows | matrixStats returns `NaN` where base returns `NA` for all-NA / single-value | all-NA column, all-NA row, single-non-NA row |
| E5 | zero-MAD column | `/0` -> `Inf`/`NaN` | constant column (MAD = 0) |
| E6 | `Inf` in data | median/mad/sd ordering with Inf | inject `Inf`/`-Inf` |
| E7 | dimnames / single-row | `sweep` vs `apply` collapse | 1-row, 1-col matrices |
| E8 | dp-1b 1-row & 0-row | `drop=FALSE` | filter to exactly 1 and 0 surviving rows |

**Resolution policy:** for E1-E3, E5-E8 we REQUIRE exact `identical()` (or `all.equal` with
`tolerance = 0` for doubles). For E4 (NaN vs NA): if a mismatch is found, we determine whether
such a column/row can occur in the real path (a fully-NA column survives normalization?). If it
can, we coerce to match base (`x[is.nan(x)] <- NA`) so results are unaltered; if it cannot occur
(e.g. sd.filter's NA/NaN both fall out of `which()`), we document why the OUTPUT is still identical
and add a regression test asserting the output set, not the intermediate.

---

## Validation harness (standalone, BEFORE touching the app)

`dev/perf_phase3/` (git-ignored scratch):
1. `oracle.R` — verbatim copies of the CURRENT three functions, named `*_old`.
2. `candidate.R` — the proposed NEW three functions, named `*_new`.
3. `synthetic.R` — deterministic (`set.seed`) ground-truth generators: dense matrices, matrices
   with scattered NA, all-NA columns/rows, constant (zero-MAD) columns, Inf-laced, 1-row, 1-col,
   even/odd dimensions, plus a realistic BRCA-scale matrix (~10k x 77). Also a hand-built
   known-answer set (e.g. median of `c(1,2,3,4)` = 2.5; MAD of `c(1,1,1)` = 0).
4. `compare.R` — runs every dataset through `*_old` and `*_new`, asserts equivalence per the
   policy above, and prints a PASS/FAIL table + a microbenchmark (old vs new timing).

Gate: `compare.R` must report ALL PASS before any edit to `R/`.

---

## Implementation order

1. Build + run the validation harness; confirm ALL PASS (this validates the math in isolation).
2. Confirm `matrixStats` semantics against Context7 docs (colMedians/colMads/rowSds).
3. Edit the three functions in `R/`; add `colMads` to `@importFrom matrixStats`; ensure DESCRIPTION
   has `matrixStats`; `devtools::document()`.
4. `devtools::load_all(".")`; re-run the harness against the LIVE package functions (not just the
   candidate copies) to confirm the in-app code equals the oracle.
5. `devtools::test()` — full suite, focusing on `test-gct-processing.R` (normalization, missing
   filter) and any sd-filter / data-filtering tests; add regression tests for E4-E8 + dp-1b 1-row/0-row.
6. Dispatch an Opus subagent (code-reviewer) for an independent code review + statistics validation
   (confirm the matrixStats substitutions are statistically equivalent and the edge handling is sound).

---

## POST-IMPLEMENTATION CORRECTION (Opus stats-validation review, 2026-06-16)

The "bit-identical / IEEE-deterministic" language below and in the E-table is accurate ONLY for the
dp-1b `rowMeans` change. For the `matrixStats` substitutions (colMedians/colMads/rowSds), finite results
differ from base R at floating-point **reduction-order** level (worst observed ~4.4e-16). This was proven
NOT to change any stored result: across 363k adversarial `sd.filter` trials there were **0 filtered-set
flips**, and normalized matrices differ by <=4.4e-16 per cell (below GCT write precision). The only
categorical divergence — `colMedians` returning `NaN` (vs base `NA`) on an all-NA column — is always masked,
because that same column's `data - median` is `NA - NaN = NA`, and the median-of-medians shift uses
`na.rm = TRUE` which drops NaN and NA identically. VERDICT: safe; results preserved.

## SELF-REVIEW — weaknesses in this plan (adversarial pass)

- **W1 (NaN/NA, the real risk).** matrixStats is documented to return `NaN` (not `NA`) for all-NA
  reductions in some versions. If a fully-NA column reaches `normalize.data.helper`, OLD gives `NA`
  (from `median(NA, na.rm=T)` -> `NA` with warning), NEW may give `NaN`. The harness MUST include an
  all-NA column and we must decide coercion. Do NOT hand-wave this.
- **W2 (mad center/constant).** `stats::mad` defaults: `center = median(x)`, `constant = 1.4826`,
  `na.rm = FALSE`. `colMads` must use the SAME center and constant. Verify the colMads default
  `constant` is 1.4826 AND that it centers on the per-column median (not mean). If the API differs,
  pass explicit args. Confirm via Context7 + empirical.
- **W3 (`data.matrix` coercion).** `normalize.data.helper` calls `data <- data.matrix(data)` and a
  numeric-coercion guard BEFORE the median/MAD block. The harness oracle must replicate that
  preprocessing so we compare apples to apples.
- **W4 (group normalization path).** `normalize.data` calls the helper per group then `cbind`s and
  reorders columns. dp-norm only changes the helper's internals, but the harness should run at least
  one grouped case end-to-end through `normalize.data` to catch dimname/order regressions.
- **W5 (`safe_set_colnames` removal).** Replacing `apply` with `sweep` makes `safe_set_colnames`
  unnecessary for the touched branches (sweep preserves dims and never collapses a >=2-row matrix).
  But `safe_set_colnames` is still used by `Upper-quartile`; do NOT delete it. Only stop calling it in
  the touched branches, and confirm dimnames are still set correctly via `dimnames(data.norm) <- dimnames(data)`.
- **W6 (single-row normalization).** The helper already early-returns unchanged for `nrow(data)==1`
  (L126-129), so the median/MAD branches never see a 1-row matrix. The harness should still test 1-row
  to confirm we did not alter that guard.
- **W7 (floating-point order).** `sweep(data,2,med,"-")` subtracts the same scalar per column as
  `x - median(x)`; IEEE subtraction is deterministic, so results are bit-identical. But `colMedians`
  internal algorithm could differ from `median` for even n if it uses a partial-sort midpoint average
  in a different order — verify with even-row matrices in the harness (E1).
- **W8 (`rowMeans` integer/logical).** `rowMeans(is.na(data))` operates on a logical matrix; R coerces
  logical->double for the mean. Confirm `data` is always a matrix at the dp-1b call site (it is
  `data.norm`, a matrix) — if a data.frame ever reaches it, `rowMeans` and `apply` could differ. Add a
  guard test with a data.frame input.
- **W9 (scope creep to other phases).** This plan is Phase 3 only. The same harness pattern
  (`dev/perf_<phase>/` oracle/candidate/synthetic/compare + Opus review) will be replicated for
  Phases 4, 5, 5b, 6. Each phase gets its own ground-truth datasets appropriate to its code.
