# Missed-Cleavage Contiguous X-Axis Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the PELSA missed-cleavage bar chart show every integer missed-cleavage value from 0 to the maximum observed as an evenly-spaced bar, including a visible empty slot (count 0) for any intermediate value that no peptide has.

**Architecture:** The chart is a ggplot `geom_col` over a factor of `df$missed`, rendered via `ggplotly()`. Today `pelsa_missed_cleavage_data()` returns only the distinct values that occur, so gaps (e.g. no peptide with 6 missed cleavages) collapse — the axis jumps `5 -> 7`. We fix this in the DATA helper: return one row for EVERY integer from 0 to `max(missed)`, filling missing values with `count = 0` and `percent = 0`. The plot then builds its factor over the now-contiguous set, so `geom_col` reserves an (invisible, zero-height) bar and an axis tick at each missing value — giving even spacing with a visible empty slot. No plot-function logic changes are required beyond what already exists; the tooltip already formats `count` and `percent`, so empty slots read "Peptides: 0 / Percent: 0.0%".

**Tech Stack:** R, ggplot2, plotly (`ggplotly`), testthat. Package reloaded with `devtools::load_all(".")`.

## Global Constraints

- ASCII-only R source: no literal Unicode in `R/`; use `\uXXXX` escapes if any non-ASCII glyph is needed (none required here).
- `.data$col` in ggplot2 `aes()` to avoid `R CMD check` global-variable notes.
- The percentage denominator stays `nrow(peptide_metrics)` (= `qc$n_peptides` = "Total peptides identified"). Gap-filling does NOT change the denominator — added zero-count rows contribute 0 to the numerator and nothing to the denominator logic. Percentages still sum to <=100%.
- Counts format with `format(x, big.mark = ",")`; percentages format as `%.1f%%` (existing conventions in `R/tab_pelsa_section2.R`).
- Editing `R/` is not enough to test in the app: run `devtools::load_all(".")` before exercising the app or tests. Tests exercise the *loaded* package.
- Missed-cleavage values are non-negative integers (a tryptic count; see `R/tab_pelsa_peptide_helpers.R::pelsa_missed_cleavages`). The fill range therefore starts at the lower of 0 and the minimum observed value, and runs to the maximum observed value.

---

### Task 1: Fill missing missed-cleavage values with zero-count rows

**Files:**
- Modify: `R/tab_pelsa_section2_helpers.R:232-262` (function `pelsa_missed_cleavage_data` and its doc comment)
- Test: `tests/testthat/test-pelsa-summary.R` (existing file; add one test after the existing empty-result test that ends at line 199)

**Interfaces:**
- Consumes: `peptide_metrics` data.frame with an integer-coercible `missed_cleavages` column (unchanged input contract). `nrow(peptide_metrics)` is the total peptides identified.
- Produces: `pelsa_missed_cleavage_data(peptide_metrics)` returns a data.frame with columns `missed` (integer), `count` (integer), `percent` (numeric). CHANGE: rows now cover EVERY integer from `min(0, min(observed))` to `max(observed)` inclusive, in ascending order. Values that no peptide has get `count = 0L` and `percent = 0`. The empty-input case is unchanged (zero-row data.frame with the three columns). Existing callers that pass contiguous data see no change (the fill is a no-op when values are already contiguous from 0).

- [ ] **Step 1: Write the failing test**

Add this test to `tests/testthat/test-pelsa-summary.R` immediately after the existing test that ends at line 199 (`test_that("missed-cleavage empty result carries a numeric percent column", ...)`):

```r
test_that("missed-cleavage data fills gaps with zero-count rows for even spacing", {
  # Peptides have 0, 1, 2, 3, 5, 7 missed cleavages -> 4 and 6 are gaps.
  # The helper must emit a contiguous 0..7 sequence, with count 0 / percent 0
  # at the missing values so the bar chart can draw a visible empty slot.
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:8),
    missed_cleavages = c(0L, 0L, 1L, 2L, 3L, 5L, 7L, 7L),
    peptide_length   = rep(8L, 8L),
    stringsAsFactors = FALSE
  )
  mc <- pelsa_missed_cleavage_data(pm)
  # Contiguous 0..7 (max observed is 7), no gaps.
  expect_identical(mc$missed, 0:7)
  # Observed counts at 0,1,2,3,5,7; zeros filled at 4 and 6.
  expect_identical(mc$count, c(2L, 1L, 1L, 1L, 0L, 1L, 0L, 2L))
  # Gap rows carry percent 0; observed rows are count / nrow(pm) * 100.
  expect_equal(mc$percent[mc$missed == 4L], 0)
  expect_equal(mc$percent[mc$missed == 6L], 0)
  expect_equal(mc$percent[mc$missed == 0L], 25)   # 2 / 8 * 100
  expect_equal(mc$percent[mc$missed == 7L], 25)   # 2 / 8 * 100
  # Percentages still sum over the same numerator (8 finite peptides / 8 total).
  expect_equal(sum(mc$percent), 100)
})
```

- [ ] **Step 2: Run the new test to verify it fails**

```bash
Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-summary.R")'
```

Expected: the new test FAILS — the current helper returns only the observed values `c(0,1,2,3,5,7)`, so `expect_identical(mc$missed, 0:7)` fails (length 6 vs 8, missing 4 and 6). All previously-passing tests still pass (the other missed-cleavage tests use contiguous-from-0 data, so the fill is a no-op for them).

- [ ] **Step 3: Implement gap-filling in the helper**

Edit `R/tab_pelsa_section2_helpers.R`. First update the doc comment (lines 235-236) to describe the new contract:

Replace:

```r
# @return data.frame(missed = integer, count = integer), one row per distinct
#         missed-cleavage value in ascending order. Empty when none.
```

with:

```r
# @return data.frame(missed = integer, count = integer, percent = numeric), one
#         row per integer from min(0, observed) to max(observed) in ascending
#         order. Values no peptide has are filled with count 0 / percent 0 so the
#         bar chart shows evenly-spaced bars with a visible empty slot at gaps.
#         Empty when there are no finite values.
```

Then replace the function body. The current body (lines 238-262) computes `tb <- table(v)` and builds the data.frame from only the observed names. Replace the section FROM `tb <- table(v)` THROUGH the closing `)` of the returned `data.frame(...)` with a version that builds a contiguous integer sequence and joins counts onto it.

Replace this block:

```r
  tb <- table(v)
  count <- as.integer(tb)
  data.frame(
    missed  = as.integer(names(tb)),
    count   = count,
    percent = count / total * 100,
    stringsAsFactors = FALSE
  )
```

with:

```r
  tb <- table(v)
  observed <- as.integer(names(tb))
  observed_count <- as.integer(tb)
  # Contiguous axis: every integer from min(0, observed) to max(observed). Gaps
  # (values no peptide has) are filled with count 0 so the bar chart draws an
  # evenly-spaced, visibly-empty slot instead of collapsing the gap.
  full <- seq.int(min(0L, min(observed)), max(observed))
  count <- integer(length(full))
  count[match(observed, full)] <- observed_count
  data.frame(
    missed  = as.integer(full),
    count   = count,
    percent = count / total * 100,
    stringsAsFactors = FALSE
  )
```

- [ ] **Step 4: Run the new test to verify it passes**

```bash
Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-summary.R")'
```

Expected: PASS — the new gap-fill test passes, AND all previously-passing missed-cleavage tests still pass unchanged (`0:1`, `0:2`, NA-handling, empty-input), because those inputs are already contiguous from 0 so the fill adds no rows.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section2_helpers.R tests/testthat/test-pelsa-summary.R
git commit -m "feat(pelsa): fill missed-cleavage gaps with zero-count rows"
```

---

### Task 2: Verify the plot draws contiguous, evenly-spaced bars with empty slots

**Files:**
- No `R/` changes expected. `pelsa_missed_cleavage_plot()` (`R/tab_pelsa_section2.R:671-690`) already builds its factor over `df$missed` and formats the tooltip from `count`/`percent`; the gap-filled data flows through unchanged. This task is a verification + a regression test that locks the end-to-end behavior in.
- Test: `tests/testthat/test-pelsa-summary.R` (add one test after the tooltip-aesthetic test that ends near line 861).

**Interfaces:**
- Consumes: `pelsa_missed_cleavage_data(peptide_metrics)` returning gap-filled `missed`/`count`/`percent` (from Task 1).
- Produces: `pelsa_missed_cleavage_plot(peptide_metrics)` returns a ggplot whose x factor levels are the contiguous integer sequence (gaps included), and whose built tooltip text includes a "Peptides: 0" / "Percent: 0.0%" entry for each filled gap. No new function or signature.

- [ ] **Step 1: Write the failing test**

Add this test to `tests/testthat/test-pelsa-summary.R` after the existing tooltip-aesthetic test (`test_that("missed-cleavage plot bakes count + percent into a tooltip text aesthetic", ...)`, which ends near line 861):

```r
test_that("missed-cleavage plot shows contiguous x positions with an empty-slot tooltip", {
  # Gap at 6 (no peptide has 6 missed cleavages). The plot must reserve an
  # axis slot at 6 and give it a 'Peptides: 0' tooltip, not collapse 5 -> 7.
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:7),
    missed_cleavages = c(0L, 1L, 2L, 3L, 4L, 5L, 7L),
    peptide_length = rep(8L, 7L),
    stringsAsFactors = FALSE
  )
  p <- pelsa_missed_cleavage_plot(pm)
  expect_s3_class(p, "ggplot")
  # X factor levels are contiguous 0..7 (gap value 6 included as a level).
  built <- ggplot2::ggplot_build(p)
  expect_identical(levels(built$plot$data$missed),
                   as.character(0:7))
  # The gap slot (6) carries an explicit zero-count tooltip.
  txt <- built$data[[1]]$text
  expect_true(any(grepl("Missed cleavages: 6", txt, fixed = TRUE)))
  expect_true(any(grepl("Peptides: 0", txt, fixed = TRUE)))
  expect_true(any(grepl("Percent: 0.0%", txt, fixed = TRUE)))
})
```

- [ ] **Step 2: Run the new test to verify it passes (data fix already in place)**

```bash
Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-summary.R")'
```

Expected: PASS immediately — Task 1 already made the data contiguous, and `pelsa_missed_cleavage_plot()` already factors over `df$missed` and formats the tooltip per row, so the gap row at 6 yields a level "6" and a "Peptides: 0 / Percent: 0.0%" tooltip. (This is a characterization test that pins the end-to-end behavior; it does not require a separate RED because the plot function needs no change. If it FAILS, the plot function is NOT honoring the gap-filled data — investigate `pelsa_missed_cleavage_plot` before proceeding.)

- [ ] **Step 3: Visually confirm the rendered plotly tooltip + spacing**

```bash
Rscript -e 'suppressMessages(devtools::load_all(".")); pm <- data.frame(PEP.StrippedSequence = paste0("PEP", 1:7), missed_cleavages = c(0L,1L,2L,3L,4L,5L,7L), peptide_length = rep(8L,7L)); p <- pelsa_missed_cleavage_plot(pm); b <- plotly::plotly_build(plotly::ggplotly(p, tooltip = "text")); cat("x ticktext:", paste(b$x$layout$xaxis$ticktext, collapse=", "), "\n"); cat(b$x$data[[1]]$text, sep="\n----\n")'
```

Expected: `x ticktext:` lists `0, 1, 2, 3, 4, 5, 6, 7` (6 present as an evenly-spaced tick), and the printed tooltip blocks include one reading `Missed cleavages: 6<br />Peptides: 0<br />Percent: 0.0%`.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test-pelsa-summary.R
git commit -m "test(pelsa): lock contiguous missed-cleavage axis with empty-slot tooltip"
```

---

### Task 3: Full-suite regression check

**Files:**
- No code changes. Verification only.

**Interfaces:**
- Consumes: the PELSA test files that touch `missed_cleavages` and the export path that reuses `pelsa_missed_cleavage_plot`. Produces: confidence Tasks 1-2 broke nothing.

- [ ] **Step 1: Run the PELSA summary test file**

```bash
Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-summary.R")'
```

Expected: 0 failures.

- [ ] **Step 2: Run the analysis + export-helpers files**

```bash
Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-pelsa-analysis.R"); testthat::test_file("tests/testthat/test-pelsa-export-helpers.R")'
```

Expected: 0 failures. (These exercise `missed_cleavages` computation and the static-export path that calls `pelsa_missed_cleavage_plot`.)

- [ ] **Step 3: Confirm the static PNG export still renders**

```bash
Rscript -e 'suppressMessages(devtools::load_all(".")); pm <- data.frame(PEP.StrippedSequence = paste0("PEP", 1:7), missed_cleavages = c(0L,1L,2L,3L,4L,5L,7L), peptide_length = rep(8L,7L)); p <- pelsa_missed_cleavage_plot(pm); tmp <- tempfile(fileext=".png"); ggplot2::ggsave(tmp, p, width=5, height=4, dpi=72); cat("static export ok:", file.exists(tmp), "size>0:", file.info(tmp)$size > 0, "\n")'
```

Expected: `static export ok: TRUE size>0: TRUE`.

- [ ] **Step 4: No commit needed**

This task adds no changes. If any failure surfaced, fix it in the relevant task above and re-run before proceeding.

---

## Self-Review

**1. Spec coverage:** The request was "change it to evenly-spaced numeric positions with empty bars for missing counts (e.g. a visible empty 6)." Task 1 makes the data contiguous (0..max) with zero-count fill rows; Task 2 verifies the plot reserves an evenly-spaced axis slot and an explicit zero tooltip at the gap. Covered.

**2. Placeholder scan:** No TBD/TODO/"handle edge cases" placeholders. Every code step shows the full edit and exact commands with expected output.

**3. Type consistency:** `pelsa_missed_cleavage_data` returns `missed` (integer), `count` (integer), `percent` (numeric) in Task 1, consumed with those exact names in Task 2 and by the unchanged `pelsa_missed_cleavage_plot`. `seq.int(min(0L, min(observed)), max(observed))` yields integers; `as.integer(full)` keeps the `missed` column integer to match the existing `expect_identical(mc$missed, 0:7)`-style assertions (`0:7` is an integer vector in R). `count` is built as an `integer(length(full))` vector, so `expect_identical(mc$count, c(...L))` integer comparisons hold.

**Design notes / edge cases:**
- **No-op for contiguous-from-0 data:** when observed values already run 0,1,...,n with no gaps, `full` equals `observed` and the fill adds nothing — so the three pre-existing data tests (0:1, 0:2, NA-handling) pass unchanged. This is why Task 1 needs no edits to those tests.
- **Denominator unchanged:** zero-count fill rows add 0 to the numerator and do not touch `total = nrow(peptide_metrics)`, so percentages are unaffected and still sum to <=100%.
- **Zero-height bars are invisible by design; the axis tick is what makes the empty slot "visible".** Because the x-axis is a contiguous factor, the gap value gets its own labeled tick and a hoverable (zero-height) position — that is the intended "visible empty 6". No special-casing or a minimum bar height is introduced (YAGNI). If, after seeing it live, a faint baseline marker for empty slots is wanted, that is a separate follow-up.
- **Range floor:** the fill starts at `min(0L, min(observed))`. In practice missed-cleavage counts are non-negative so this is just 0, but guarding with `min(0L, ...)` keeps the sequence valid even if a negative ever appeared, rather than producing a reversed/empty `seq`.
