# Missed-Cleavage Tooltip Percentage Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the percentage of all identified peptides (alongside the existing count) to each bar's floating tooltip in the PELSA missed-cleavage bar chart.

**Architecture:** The missed-cleavage bar chart is a ggplot `geom_col` rendered to an interactive plot via `ggplotly()`. We compute a `percent` column in the data-shaping helper (`pelsa_missed_cleavage_data`), bake a pre-formatted tooltip string into a `text` aesthetic in the plot builder (`pelsa_missed_cleavage_plot`), and tell `ggplotly()` to use only that `text` aesthetic for the tooltip (`tooltip = "text"`). No new files, no new dependencies.

**Percentage denominator:** "Percent out of all peptides identified" uses the SAME number as the "Total peptides identified" metric in the PELSA summary dashbar, which is `entry$qc$n_peptides = nrow(peptides)` (the original input data row count — see `R/tab_pelsa_analysis_helpers.R:936`). `peptide_metrics` is built one row per input peptide (`R/tab_pelsa_analysis_helpers.R:886-889`), so `nrow(peptide_metrics)` is identically equal to `qc$n_peptides`. We therefore use `nrow(peptide_metrics)` as the denominator inside the existing helper — no signature changes, no extra plumbing. Because the denominator is ALL identified peptides (not just those with a finite missed-cleavage count), the per-bar percentages may sum to slightly less than 100% when some peptides have a non-finite `missed_cleavages` value; that is correct and intended.

**Tech Stack:** R, ggplot2, plotly (`ggplotly`), testthat. Package reloaded with `devtools::load_all(".")`.

## Global Constraints

- ASCII-only R source: no literal Unicode in `R/`; use `\uXXXX` escapes if any non-ASCII glyph is needed (none required here).
- `.data$col` in ggplot2 `aes()` to avoid `R CMD check` global-variable notes.
- Percentages format as `%.1f%%` (one decimal, trailing percent), matching the existing convention in `R/tab_pelsa_section2.R`.
- Counts format with `format(x, big.mark = ",")`, matching the existing convention in the same file.
- The percentage denominator is `nrow(peptide_metrics)` (= total peptides identified = `qc$n_peptides`). Do NOT use `sum(count)` — that would silently exclude peptides with non-finite missed-cleavage values from the denominator.
- Editing `R/` is not enough to test in the app: run `devtools::load_all(".")` before exercising the app or tests. Tests exercise the *loaded* package.

---

### Task 1: Add a `percent` column to the missed-cleavage data helper

**Files:**
- Modify: `R/tab_pelsa_section2_helpers.R:238-254` (function `pelsa_missed_cleavage_data`)
- Test: `tests/testthat/test-pelsa-summary.R` (existing file; add one test near the existing shape test at line 147)

**Interfaces:**
- Consumes: `peptide_metrics` data.frame with an integer-coercible `missed_cleavages` column (unchanged input contract). `nrow(peptide_metrics)` is the total peptides identified (= `qc$n_peptides`, the dashbar number).
- Produces: `pelsa_missed_cleavage_data(peptide_metrics)` returns a data.frame with columns `missed` (integer), `count` (integer), and NOW ALSO `percent` (numeric, `count / nrow(peptide_metrics) * 100`, one row per distinct missed-cleavage value, ascending). The empty-case data.frame also gains a `percent = numeric(0)` column. The denominator is the TOTAL peptide-metrics row count (all identified peptides), NOT `sum(count)` — so percentages may sum to <100% when some peptides have non-finite missed-cleavage values.

- [ ] **Step 1: Write the failing test**

Add this test to `tests/testthat/test-pelsa-summary.R` immediately after the existing test that ends at line 158 (`test_that("length values + missed-cleavage data shape correctly", ...)`):

```r
test_that("missed-cleavage data includes percent of all identified peptides", {
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:4),
    missed_cleavages = c(0L, 0L, 1L, 2L),
    peptide_length   = rep(8L, 4L),
    stringsAsFactors = FALSE
  )
  mc <- pelsa_missed_cleavage_data(pm)
  expect_identical(mc$missed, c(0L, 1L, 2L))
  expect_identical(mc$count, c(2L, 1L, 1L))
  # percent is count / total peptides identified (nrow(pm)) * 100.
  expect_equal(mc$percent, c(50, 25, 25))
  # all 4 peptides have finite missed-cleavage values, so percents sum to 100.
  expect_equal(sum(mc$percent), 100)
})

test_that("missed-cleavage percent denominator is ALL identified peptides, not just finite", {
  # 2 of 4 peptides have a non-finite missed-cleavage value. The denominator is
  # still nrow(pm) = 4 (= qc$n_peptides / total peptides identified), so the
  # bars cover only 2 peptides and percents sum to 50, NOT 100.
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:4),
    missed_cleavages = c(0L, 1L, NA_integer_, NA_integer_),
    peptide_length   = rep(8L, 4L),
    stringsAsFactors = FALSE
  )
  mc <- pelsa_missed_cleavage_data(pm)
  expect_identical(mc$missed, c(0L, 1L))
  expect_identical(mc$count, c(1L, 1L))
  # denominator is 4 (all rows), not 2 (finite rows): 1/4 = 25% each.
  expect_equal(mc$percent, c(25, 25))
  expect_equal(sum(mc$percent), 50)
})

test_that("missed-cleavage empty result carries a numeric percent column", {
  mc <- pelsa_missed_cleavage_data(NULL)
  expect_identical(nrow(mc), 0L)
  expect_true("percent" %in% names(mc))
  expect_type(mc$percent, "double")
})
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pelsa-summary.R", filter = NULL)'
```

Expected: the three new tests FAIL — `mc$percent` is `NULL` so `expect_equal(mc$percent, ...)` errors / `"percent" %in% names(mc)` is `FALSE`. (Run from the repo root in R, or via `Rscript` as shown.)

- [ ] **Step 3: Add the `percent` column to the helper**

Edit `R/tab_pelsa_section2_helpers.R`. Replace the whole function body (lines 238-254) with:

```r
pelsa_missed_cleavage_data <- function(peptide_metrics) {
  empty <- data.frame(missed = integer(0), count = integer(0),
                      percent = numeric(0), stringsAsFactors = FALSE)
  if (is.null(peptide_metrics) || !is.data.frame(peptide_metrics) ||
      !("missed_cleavages" %in% names(peptide_metrics))) {
    return(empty)
  }
  # Denominator = total peptides identified = nrow(peptide_metrics), which is
  # identically qc$n_peptides (the "Total peptides identified" dashbar number;
  # peptide_metrics is built one row per input peptide). Capture it BEFORE
  # dropping non-finite missed-cleavage values, so peptides without a finite
  # count still count toward the denominator.
  total <- nrow(peptide_metrics)
  v <- suppressWarnings(as.integer(peptide_metrics$missed_cleavages))
  v <- v[is.finite(v)]
  if (length(v) == 0L || total == 0L) return(empty)
  tb <- table(v)
  count <- as.integer(tb)
  data.frame(
    missed  = as.integer(names(tb)),
    count   = count,
    percent = count / total * 100,
    stringsAsFactors = FALSE
  )
}
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pelsa-summary.R", filter = NULL)'
```

Expected: PASS — all tests in the file pass, including the two new ones and the pre-existing `pelsa_missed_cleavage_data` shape test at line 147 (which only checks `$missed` and `$count`, so it is unaffected).

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section2_helpers.R tests/testthat/test-pelsa-summary.R
git commit -m "feat(pelsa): add percent-of-peptides column to missed-cleavage data"
```

---

### Task 2: Show count + percentage in the bar tooltip

**Files:**
- Modify: `R/tab_pelsa_section2.R:671-683` (function `pelsa_missed_cleavage_plot`)
- Modify: `R/tab_pelsa_section2.R:240-242` (the `output$missed_plot <- renderPlotly({ ggplotly(...) })` render path)
- Test: `tests/testthat/test-pelsa-summary.R` (add one test after the existing plot test that ends at line 798)

**Interfaces:**
- Consumes: `pelsa_missed_cleavage_data(peptide_metrics)` returning `missed`, `count`, `percent` (from Task 1).
- Produces: `pelsa_missed_cleavage_plot(peptide_metrics)` returns a ggplot whose `geom_col` layer carries a `text` aesthetic mapped to a per-bar string of the form `"Missed cleavages: <m>\nPeptides: <count>\nPercent: <p>%"`. The render path passes `tooltip = "text"` to `ggplotly()` so the hover shows exactly that string. The plot still returns a `ggplot` object (the static export at `R/tab_pelsa_section2.R:910` continues to work, since `text` is an unused aesthetic for static rendering).

- [ ] **Step 1: Write the failing test**

Add this test to `tests/testthat/test-pelsa-summary.R` after the existing plot test that ends at line 798:

```r
test_that("missed-cleavage plot bakes count + percent into a tooltip text aesthetic", {
  pm <- data.frame(
    PEP.StrippedSequence = paste0("PEP", 1:4),
    missed_cleavages = c(0L, 0L, 1L, 2L),
    peptide_length = rep(8L, 4L),
    stringsAsFactors = FALSE
  )
  p <- pelsa_missed_cleavage_plot(pm)
  expect_s3_class(p, "ggplot")
  # The geom_col layer must map a `text` aesthetic (used as the plotly tooltip).
  has_text_aes <- any(vapply(p$layers, function(l) {
    "text" %in% names(l$mapping)
  }, logical(1)))
  expect_true(has_text_aes)
  # The built data must contain the formatted tooltip strings with both the
  # count and the percent for the largest bar (count 2 = 50.0%).
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_true("text" %in% names(built))
  expect_true(any(grepl("50.0%", built$text, fixed = TRUE)))
  expect_true(any(grepl("Peptides: 2", built$text, fixed = TRUE)))
})
```

- [ ] **Step 2: Run the test to verify it fails**

```bash
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pelsa-summary.R", filter = NULL)'
```

Expected: the new test FAILS — the current `geom_col` has no `text` aesthetic, so `has_text_aes` is `FALSE` and `"text" %in% names(built)` is `FALSE`.

- [ ] **Step 3: Add the `text` aesthetic to the plot builder**

Edit `R/tab_pelsa_section2.R`. Replace the whole function body (lines 671-683) with:

```r
pelsa_missed_cleavage_plot <- function(peptide_metrics) {
  df <- pelsa_missed_cleavage_data(peptide_metrics)
  if (nrow(df) == 0L) {
    return(pelsa_blank_plot("No missed-cleavage data."))
  }
  # Pre-format the per-bar tooltip: count + percentage of all identified
  # peptides. Baked into a `text` aesthetic so ggplotly(tooltip = "text")
  # shows exactly this. \n becomes a line break in the plotly hover box.
  df$tooltip <- sprintf(
    "Missed cleavages: %d\nPeptides: %s\nPercent: %.1f%%",
    df$missed, format(df$count, big.mark = ","), df$percent
  )
  df$missed <- factor(df$missed, levels = sort(unique(df$missed)))
  ggplot(df, aes(x = .data$missed, y = .data$count, text = .data$tooltip)) +
    geom_col(fill = "#f28e2b") +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = "Missed cleavages", y = "# of peptides",
         title = "Missed-cleavage distribution") +
    theme_bw()
}
```

- [ ] **Step 4: Set `tooltip = "text"` in the render path**

Edit `R/tab_pelsa_section2.R` lines 240-242. Replace:

```r
    output$missed_plot <- renderPlotly({
      ggplotly(missed_plot_reactive())
    })
```

with:

```r
    output$missed_plot <- renderPlotly({
      ggplotly(missed_plot_reactive(), tooltip = "text")
    })
```

- [ ] **Step 5: Run the test to verify it passes**

```bash
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pelsa-summary.R", filter = NULL)'
```

Expected: PASS — the new test plus all pre-existing tests in the file pass. The pre-existing y-axis-label test at line 781 still passes (the `text` aesthetic does not affect the y scale).

- [ ] **Step 6: Confirm ggplotly round-trips the text aesthetic without error**

```bash
Rscript -e 'devtools::load_all("."); pm <- data.frame(PEP.StrippedSequence = paste0("PEP", 1:4), missed_cleavages = c(0L,0L,1L,2L), peptide_length = rep(8L,4L)); p <- pelsa_missed_cleavage_plot(pm); pl <- plotly::ggplotly(p, tooltip = "text"); cat("ok class:", paste(class(pl), collapse=","), "\n")'
```

Expected: prints `ok class: plotly,htmlwidget` (or similar including `plotly`) with no error — confirms `ggplotly(..., tooltip = "text")` builds successfully on the new aesthetic.

- [ ] **Step 7: Commit**

```bash
git add R/tab_pelsa_section2.R tests/testthat/test-pelsa-summary.R
git commit -m "feat(pelsa): show peptide count + percentage in missed-cleavage tooltip"
```

---

### Task 3: Full-suite regression check

**Files:**
- No code changes. Verification only.

**Interfaces:**
- Consumes: the full testthat suite. Produces: confidence that Tasks 1-2 broke nothing (especially the static-export path at `R/tab_pelsa_section2.R:910` and the summary-stat path that also reads `missed_cleavages`).

- [ ] **Step 1: Run the PELSA summary test file**

```bash
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pelsa-summary.R", filter = NULL)'
```

Expected: 0 failures.

- [ ] **Step 2: Run the broader PELSA test files that touch missed cleavages**

```bash
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pelsa-analysis.R"); testthat::test_file("tests/testthat/test-pelsa-export-helpers.R")'
```

Expected: 0 failures. (These exercise `missed_cleavages` computation and the export path that calls `pelsa_missed_cleavage_plot`.)

- [ ] **Step 3: No commit needed**

This task adds no changes. If any failure surfaced, fix it in the relevant task above and re-run before proceeding.

---

## Self-Review

**1. Spec coverage:** The request was "in the missed cleavage bar chart's floating tooltip per bar, not only show count, but also show percentage out of all peptides identified" with the denominator being the same number as the "Total peptides identified" dashbar metric. Task 1 computes the percentage (`count / nrow(peptide_metrics) * 100`, where `nrow(peptide_metrics) == qc$n_peptides == Total peptides identified`). Task 2 puts count + percentage into the per-bar floating tooltip. Covered.

**2. Placeholder scan:** No TBD/TODO/"add error handling" placeholders. Every code step shows the full replacement code and exact commands with expected output.

**3. Type consistency:** `pelsa_missed_cleavage_data` returns `missed`/`count`/`percent` in Task 1 and is consumed with those exact names in Task 2. The `text`/`tooltip` aesthetic name is consistent: the data column is `tooltip`, mapped to the ggplot `text` aesthetic, and `ggplotly(tooltip = "text")` references the aesthetic name `"text"` (correct — ggplotly's `tooltip` argument names aesthetics, not data columns). Empty-case data.frame gains `percent = numeric(0)` so column-set stays consistent across both code paths.

**Edge case note:** "percent out of all peptides identified" uses `nrow(peptide_metrics)` as the denominator — identically equal to `qc$n_peptides`, the "Total peptides identified" dashbar number (per the user's instruction to reuse that number). This denominator includes peptides whose `missed_cleavages` is `NA`/non-finite (which are still identified peptides but draw no bar), so per-bar percentages may sum to slightly LESS than 100% in that case. This is intentional and matches the dashbar semantics. The `nrow` is captured before the finite-value filter so the filter does not shrink the denominator. The all-finite test confirms percentages sum to 100; the mixed-finite test confirms the denominator stays at the full row count.
