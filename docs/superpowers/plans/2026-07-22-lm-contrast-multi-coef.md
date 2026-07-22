# LM Contrast Builder — Single/Multi Coefficient Redesign — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the Simple/Advanced contrast builder with Single-coef (1×1) and Multi-coef (2×2) modes — four dropdowns wired into a fixed `(A − B) − (C − D)` template — removing all free-text authoring.

**Architecture:** The Multi expression is assembled from four coefficient dropdowns and pushed through the **unchanged** `validate_advanced_expr` → `limma::makeContrasts` → `lm.regression()` backend. Only the authoring surface changes; the fit path is untouched. Row state gains `num2`/`den2` and drops `advanced_expr`.

**Tech Stack:** R / Shiny modules, testthat, `devtools::load_all`, cmapR/limma (backend, unchanged).

## Global Constraints

- **ASCII-only R source.** No literal Unicode in `R/`; use `\uXXXX` escapes (the minus glyph is `−`, em-dash `—`). Non-ASCII bytes break `R CMD check`.
- **Reload before testing:** after any `R/` edit run `devtools::load_all(".")` — the app/tests exercise the loaded package, not source files.
- **Headless single-file test command:**
  `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-X.R", reporter="summary")'`
- **`ns()` rules:** wrap every `inputId`/`outputId` in module UI and inside `renderUI()`; do NOT wrap when referencing `input$`/`output$`.
- **`%||%`** is imported from rlang (already in `R/protigy-package.R`) — available in this module.
- **Backend is frozen:** do NOT modify `validate_advanced_expr`, `contrast_specs`'s makeContrasts path semantics, `lm.regression`, or `contrast_validation_summary`'s validation logic. They must keep working for the assembled Multi expression.
- **No roxygen export changes expected**, but run `devtools::document()` once at the end to be safe (new helpers are internal, unexported — matching the existing contrast helpers).

---

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `R/tab_lm_setup_helpers_contrasts.R` | Pure contrast helpers | Add `build_multi_expr`, `make_multi_label`; delete `direction_sentence_simple`, `enumerate_pairwise_simple_rows` |
| `R/tab_lm_setup.R` | Contrast builder module (UI + observers) | Row model, render, persist, toggle, buttons, helptext |
| `tests/testthat/test-lm-setup-helpers-contrasts.R` | Helper unit tests | Add multi-helper tests; delete direction-sentence tests |
| `tests/testthat/test-lm-pairwise-enumerator.R` | Pairwise enumerator tests | **Delete file** |
| `tests/testthat/test-lm-multi-contrast-seam.R` | New end-to-end seam test | **Create file** |

---

## Task 1: Add `build_multi_expr` and `make_multi_label` pure helpers (TDD)

**Files:**
- Modify: `R/tab_lm_setup_helpers_contrasts.R` (add two functions after `build_simple_expr`, before `direction_sentence_simple`)
- Test: `tests/testthat/test-lm-setup-helpers-contrasts.R` (append tests)

**Interfaces:**
- Consumes: `strip_shared_prefix(num, den)` (existing, returns length-2 char vector).
- Produces:
  - `build_multi_expr(a, b, c, d) -> character(1)`: `"(a - b) - (c - d)"`; `""` if any arg empty/NULL.
  - `make_multi_label(a, b, c, d) -> character(1)`: per-pair prefix-stripped nested label e.g. `"(GR4-GR3)-(Drug-Vehicle)"`; `""` if any arg empty/NULL; whitespace-free.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-lm-setup-helpers-contrasts.R` (before the final blank line):

```r
test_that("build_multi_expr assembles a nested diff-of-differences expression", {
  expect_equal(
    build_multi_expr("SubgroupGR4", "SubgroupGR3", "treatmentDrug", "treatmentVehicle"),
    "(SubgroupGR4 - SubgroupGR3) - (treatmentDrug - treatmentVehicle)"
  )
})

test_that("build_multi_expr returns empty when any slot is empty or NULL", {
  expect_equal(build_multi_expr("", "b", "c", "d"), "")
  expect_equal(build_multi_expr("a", "", "c", "d"), "")
  expect_equal(build_multi_expr("a", "b", "", "d"), "")
  expect_equal(build_multi_expr("a", "b", "c", ""), "")
  expect_equal(build_multi_expr(NULL, "b", "c", "d"), "")
})

test_that("make_multi_label strips a shared prefix within each pair independently", {
  expect_equal(
    make_multi_label("SubgroupGR4", "SubgroupGR3", "treatmentDrug", "treatmentVehicle"),
    "(GR4-GR3)-(Drug-Vehicle)"
  )
})

test_that("make_multi_label keeps a pair verbatim when it shares no prefix", {
  expect_equal(
    make_multi_label("Drug", "Control", "treatmentDrug", "treatmentVehicle"),
    "(Drug-Control)-(Drug-Vehicle)"
  )
})

test_that("make_multi_label returns empty when any slot is empty or NULL", {
  expect_equal(make_multi_label("", "b", "c", "d"), "")
  expect_equal(make_multi_label("a", "b", "c", NULL), "")
})

test_that("make_multi_label output contains no whitespace", {
  lbl <- make_multi_label("a b", "c", "d", "e")
  expect_false(grepl("\\s", lbl))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-lm-setup-helpers-contrasts.R", reporter="summary")'`
Expected: FAIL — `could not find function "build_multi_expr"` (and `make_multi_label`).

- [ ] **Step 3: Write the minimal implementation**

In `R/tab_lm_setup_helpers_contrasts.R`, insert immediately AFTER the `build_simple_expr` function (which ends at the line `  paste(num, "-", den)\n}`) and BEFORE the `direction_sentence_simple` roxygen block:

```r
#' Build a Multi (2x2) contrast expression: (a - b) - (c - d).
#'
#' Assembles the fixed difference-of-differences template from four design
#' coefficients. Feeds the SAME limma::makeContrasts backend as a simple
#' contrast. Returns "" if any of the four slots is empty/NULL.
#'
#' @param a,b,c,d Character scalars, design-column names for the four slots.
#' @return Character scalar, e.g. "(A - B) - (C - D)". Empty if any slot empty.
build_multi_expr <- function(a, b, c, d) {
  slots <- list(a, b, c, d)
  if (any(vapply(slots, function(x) is.null(x) || !nzchar(x), logical(1)))) {
    return("")
  }
  paste0("(", a, " - ", b, ") - (", c, " - ", d, ")")
}


#' Auto-generate a nested readable label for a Multi (2x2) contrast.
#'
#' Applies strip_shared_prefix() to (a, b) and to (c, d) INDEPENDENTLY, then
#' wraps each stripped pair in parentheses joined by "-", per the naming
#' convention documented in this file's header. Whitespace-free (CSV/TSV safe).
#' Returns "" if any slot is empty/NULL.
#'
#' @param a,b,c,d Character scalars, design-column names for the four slots.
#' @return Character scalar, e.g. "(GR4-GR3)-(Drug-Vehicle)". Empty if any empty.
make_multi_label <- function(a, b, c, d) {
  slots <- list(a, b, c, d)
  if (any(vapply(slots, function(x) is.null(x) || !nzchar(x), logical(1)))) {
    return("")
  }
  p1 <- strip_shared_prefix(a, b)
  p2 <- strip_shared_prefix(c, d)
  lbl <- paste0("(", p1[1], "-", p1[2], ")-(", p2[1], "-", p2[2], ")")
  gsub("\\s+", "", lbl)
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-lm-setup-helpers-contrasts.R", reporter="summary")'`
Expected: PASS — all new tests green, no regressions in the existing helper tests.

- [ ] **Step 5: Verify ASCII-only**

Run: `LC_ALL=C grep -nP "[^\x00-\x7F]" R/tab_lm_setup_helpers_contrasts.R || echo "ASCII clean"`
Expected: `ASCII clean` (no non-ASCII bytes introduced).

- [ ] **Step 6: Commit**

```bash
git add R/tab_lm_setup_helpers_contrasts.R tests/testthat/test-lm-setup-helpers-contrasts.R
git commit -m "feat(lm): add build_multi_expr and make_multi_label contrast helpers"
```

---

## Task 2: Delete dead helpers and their tests (`direction_sentence_simple`, `enumerate_pairwise_simple_rows`)

**Files:**
- Modify: `R/tab_lm_setup_helpers_contrasts.R` (remove two functions)
- Modify: `tests/testthat/test-lm-setup-helpers-contrasts.R` (remove direction-sentence tests)
- Delete: `tests/testthat/test-lm-pairwise-enumerator.R`

**Interfaces:**
- Removes: `direction_sentence_simple()`, `enumerate_pairwise_simple_rows()`. No task after this may reference them (Task 3 removes their last `R/tab_lm_setup.R` call sites — do Task 2 and Task 3 together if a subagent cannot tolerate a transient broken state; otherwise Task 3's edits remove the call sites, so run the full suite only at the end of Task 3).

> **Ordering note:** After Task 2, `R/tab_lm_setup.R` still references `direction_sentence_simple` and `enumerate_pairwise_simple_rows` — the package will `load_all` fine (R resolves at call time) but those UI paths would error if exercised. Task 3 removes the call sites. Do NOT run the app between Task 2 and Task 3. The helper-file test run below is safe because it doesn't load the module UI.

- [ ] **Step 1: Delete `direction_sentence_simple` from the helper file**

In `R/tab_lm_setup_helpers_contrasts.R`, remove this entire block (roxygen + function):

```r
#' Direction sentence for a simple contrast.
#'
#' @param label Character scalar, the auto-generated label.
#' @param num Character scalar, numerator design-column name (original).
#' @param den Character scalar, denominator design-column name (original).
#' @return Character scalar, e.g. "Positive log2FC = higher in Drug than in Vehicle".
direction_sentence_simple <- function(label, num, den) {
  if (!nzchar(label)) return("")
  stripped <- strip_shared_prefix(num, den)
  paste0("Positive log2FC = higher in ", stripped[1], " than in ", stripped[2])
}
```

- [ ] **Step 2: Delete `enumerate_pairwise_simple_rows` from the helper file**

In `R/tab_lm_setup_helpers_contrasts.R`, remove the entire `enumerate_pairwise_simple_rows` block — its roxygen doc (starting `#' Enumerate all pairwise Simple contrast rows for a factor variable.`) through the closing `}` of the function (the block that ends with the `lapply(pairs, function(pair) { ... })` returning row drafts). Leave `new_contrast_row_id()` (which follows it) intact.

- [ ] **Step 3: Delete the direction-sentence tests**

In `tests/testthat/test-lm-setup-helpers-contrasts.R`, remove these two tests:

```r
test_that("direction_sentence_simple interprets the sign of log2FC", {
  sent <- direction_sentence_simple("Drug-Vehicle", "treatmentDrug", "treatmentVehicle")
  expect_match(sent, "Positive log2FC")
  expect_match(sent, "Drug")
  expect_match(sent, "Vehicle")
})

test_that("direction_sentence_simple is empty when label is empty", {
  expect_equal(direction_sentence_simple("", "a", "b"), "")
})
```

- [ ] **Step 4: Delete the pairwise-enumerator test file**

```bash
git rm tests/testthat/test-lm-pairwise-enumerator.R
```

- [ ] **Step 5: Confirm no `R/` reference to the deleted helpers remains outside `tab_lm_setup.R`**

Run: `grep -rn "direction_sentence_simple\|enumerate_pairwise_simple_rows" R/`
Expected: matches ONLY in `R/tab_lm_setup.R` (removed in Task 3). Zero matches in `R/tab_lm_setup_helpers_contrasts.R`.

- [ ] **Step 6: Run the helper tests**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-lm-setup-helpers-contrasts.R", reporter="summary")'`
Expected: PASS — remaining helper tests green; no `direction_sentence_simple` tests present.

- [ ] **Step 7: Commit**

```bash
git add R/tab_lm_setup_helpers_contrasts.R tests/testthat/test-lm-setup-helpers-contrasts.R
git commit -m "refactor(lm): remove direction-sentence and pairwise-enumerator helpers"
```

---

## Task 3: Rewrite the contrast builder module — row model, render, persist, toggle, buttons, helptext

This is the core UI task. It is one task because the row-model change (`advanced_expr` → `num2`/`den2`, `type` values `simple`/`multi`) touches the render, persist, add, clear, seed, and specs blocks together — a reviewer cannot accept one without the others, and the module must `load_all` cleanly only when all are consistent.

**Files:**
- Modify: `R/tab_lm_setup.R` — the state comment (~L33-44), `contrast_builder_ui` render (buttons + helptext), the seed observer, `contrast_rows_ui` render, the persist observer, `add_contrast` observer, `add_contrast_advanced` observer (delete), `suggest_pairwise_contrasts` observer (delete), `clear_contrasts` observer, `contrast_specs` reactive.

**Interfaces:**
- Consumes: `build_multi_expr`, `make_multi_label` (Task 1); `make_simple_label`, `build_simple_expr`, `sanitize_label`, `new_contrast_row_id`, `validate_advanced_expr` (existing); `design_coefs()`, `simple_coefs` derivation (existing).
- Produces: contrast rows of shape `list(id, type ∈ {"simple","multi"}, num, den, num2, den2, label, label_user_edited)`; `contrast_specs()` yields `list(id, label, expr, type)` where `expr` for a multi row is `build_multi_expr(...)`. Downstream (`contrast_validation_summary`, `lm.regression` apply) is unchanged.

### Sub-step group A — state model + seed + add/clear

- [ ] **Step 1: Update the row-schema comment**

Replace the comment block at the top of the module (currently):

```r
    # Structured contrast state: list of rows, each with stable schema
    #   list(
    #     id,             # stable internal key: "C1", "C2", ... - used as the
    #                     # column prefix in the fitted contrasts matrix
    #     type,           # "simple" | "advanced"
    #     num,            # simple: design-matrix column name (numerator)
    #     den,            # simple: design-matrix column name (denominator)
    #     advanced_expr,  # advanced: free-text limma contrast expression
    #     label,          # human-readable; default auto-generated, editable
    #     label_user_edited  # bool: true means don't auto-regenerate on level change
    #   )
    # Empty rows are kept and ignored at submit time.
```

with:

```r
    # Structured contrast state: list of rows, each with stable schema
    #   list(
    #     id,             # stable internal key: "C1", "C2", ... - used as the
    #                     # column prefix in the fitted contrasts matrix
    #     type,           # "simple" (1x1) | "multi" (2x2)
    #     num,            # slot A: design-matrix column name (numerator)
    #     den,            # slot B: design-matrix column name (denominator)
    #     num2,           # multi slot C: design-matrix column name
    #     den2,           # multi slot D: design-matrix column name
    #     label,          # human-readable; default auto-generated, editable
    #     label_user_edited  # bool: true means don't auto-regenerate on level change
    #   )
    # simple expr = num - den; multi expr = (num - den) - (num2 - den2).
    # Empty rows are kept and ignored at submit time.
```

- [ ] **Step 2: Update the seed observer**

Replace:

```r
    observe({
      if (length(contrast_rows()) == 0) {
        contrast_rows(list(list(
          id = new_contrast_row_id(),
          type = "simple",
          num = "",
          den = "",
          advanced_expr = "",
          label = "",
          label_user_edited = FALSE
        )))
      }
    })
```

with:

```r
    observe({
      if (length(contrast_rows()) == 0) {
        contrast_rows(list(list(
          id = new_contrast_row_id(),
          type = "simple",
          num = "",
          den = "",
          num2 = "",
          den2 = "",
          label = "",
          label_user_edited = FALSE
        )))
      }
    })
```

- [ ] **Step 3: Rewrite the `add_contrast` observer, delete `add_contrast_advanced`**

Replace the `add_contrast` observer:

```r
    observeEvent(input$add_contrast, {
      contrast_rows(c(contrast_rows(),
                      list(list(id = new_contrast_row_id(),
                                type = "simple",
                                num = "", den = "",
                                advanced_expr = "",
                                label = "",
                                label_user_edited = FALSE))))
    })
```

with:

```r
    observeEvent(input$add_contrast, {
      contrast_rows(c(contrast_rows(),
                      list(list(id = new_contrast_row_id(),
                                type = "simple",
                                num = "", den = "",
                                num2 = "", den2 = "",
                                label = "",
                                label_user_edited = FALSE))))
    })
```

Then DELETE the entire `add_contrast_advanced` observer:

```r
    # Add Advanced contrast card
    observeEvent(input$add_contrast_advanced, {
      contrast_rows(c(contrast_rows(),
                      list(list(id = new_contrast_row_id(),
                                type = "advanced",
                                num = "", den = "",
                                advanced_expr = "",
                                label = "",
                                label_user_edited = FALSE))))
    })
```

- [ ] **Step 4: Delete the `suggest_pairwise_contrasts` observer**

DELETE the entire block beginning with the comment `# Suggest all pairwise contrasts for a single-factor design.` through the closing `})` of `observeEvent(input$suggest_pairwise_contrasts, { ... })` (the block that ends with the `showNotification(paste0("Added ", length(new_rows), " pairwise contrasts."), ...)` call). This is the ~60-line observer that calls `enumerate_pairwise_simple_rows`.

- [ ] **Step 5: Update the `clear_contrasts` observer**

Replace:

```r
    observeEvent(input$clear_contrasts, {
      contrast_rows(list(list(id = new_contrast_row_id(),
                              type = "simple",
                              num = "", den = "",
                              advanced_expr = "",
                              label = "",
                              label_user_edited = FALSE)))
    })
```

with:

```r
    observeEvent(input$clear_contrasts, {
      contrast_rows(list(list(id = new_contrast_row_id(),
                              type = "simple",
                              num = "", den = "",
                              num2 = "", den2 = "",
                              label = "",
                              label_user_edited = FALSE)))
    })
```

### Sub-step group B — buttons + helptext (`contrast_builder_ui`)

- [ ] **Step 6: Rewrite the helptext and button row**

In `output$contrast_builder_ui`, replace the `helpText(...)` call:

```r
        helpText(
          "Each contrast card builds one linear combination of model coefficients, ",
          "tested AFTER fitting. Contrasts add columns to the results keyed by your ",
          "editable label (e.g. \"Drug-Vehicle\"). Use ",
          tags$b("Simple"), " for one-coefficient-vs-one-coefficient differences, or ",
          tags$b("Advanced"), " for interaction / weighted / multi-coef expressions ",
          "like ", tags$code("(A - B) - (C - D)"), ". Empty cards are ignored at run time."
        ),
```

with:

```r
        helpText(
          "Each contrast card builds one linear combination of model coefficients, ",
          "tested AFTER fitting. Contrasts add columns to the results keyed by your ",
          "editable label (e.g. \"Drug-Vehicle\"). Use ",
          tags$b("Single coef (1x1)"), " for a one-coefficient-vs-one-coefficient ",
          "difference, or ", tags$b("Multi coef (2x2)"),
          " to build a difference-of-differences (interaction) contrast ",
          tags$code("(A - B) - (C - D)"), " from four coefficient dropdowns. ",
          "Empty cards are ignored at run time."
        ),
```

Then replace the button `div(...)` block:

```r
        div(
          style = "display: flex; justify-content: flex-start; margin-top: 10px; gap: 16px; flex-wrap: wrap;",
          actionButton(ns("add_contrast"), "+ Add Simple",
                       class = "btn btn-sm btn-default"),
          actionButton(ns("add_contrast_advanced"), "+ Add Advanced",
                       class = "btn btn-sm btn-default"),
          actionButton(ns("suggest_pairwise_contrasts"),
                       "+ Suggest all pairwise",
                       class = "btn btn-sm btn-default"),
          shinyBS::bsTooltip(
            ns("suggest_pairwise_contrasts"),
            title = paste0(
              "Insert one Simple card for every level-vs-level pair of the ",
              "selected factor. Works only when exactly one factor variable ",
              "is in the model."
            ),
            placement = "top",
            trigger = "hover"
          ),
          actionButton(ns("clear_contrasts"), "Clear all",
                       class = "btn btn-sm btn-danger")
        ),
```

with:

```r
        div(
          style = "display: flex; justify-content: flex-start; margin-top: 10px; gap: 16px; flex-wrap: wrap;",
          actionButton(ns("add_contrast"), "+ Add contrast",
                       class = "btn btn-sm btn-default"),
          actionButton(ns("clear_contrasts"), "Clear all",
                       class = "btn btn-sm btn-danger")
        ),
```

- [ ] **Step 7: Update the empty-state helpText in `contrast_rows_ui`**

Replace:

```r
        return(helpText("No contrasts defined. Click '+ Add Simple' to add one."))
```

with:

```r
        return(helpText("No contrasts defined. Click '+ Add contrast' to add one."))
```

- [ ] **Step 8: Remove the `.direction-sentence` CSS block**

In the `tags$style(HTML( ... ))` inside `output$contrast_builder_ui`, delete this rule:

```r
           .lm-contrast-card .direction-sentence {
             font-style: italic; color: #495057; font-size: 12px;
             padding: 2px 6px; background: #eef3f8; border-radius: 3px;
             margin-top: 4px;
           }
```

### Sub-step group C — the card renderer (`contrast_rows_ui`)

- [ ] **Step 9: Replace the per-row panel construction, label, direction-sentence, and radio**

Inside `output$contrast_rows_ui`'s `lapply(seq_along(rows), function(i) { ... })`, make these coordinated edits.

**(9a)** Add two per-row input ids. After the existing id lines, add `num2_id`/`den2_id`. Replace:

```r
        # Per-row input ids
        type_id  <- paste0("type_", r$id)
        num_id   <- paste0("num_",  r$id)
        den_id   <- paste0("den_",  r$id)
        swap_id  <- paste0("swap_", r$id)
        expr_id  <- paste0("expr_", r$id)
        label_id <- paste0("label_", r$id)
        rm_id    <- paste0("rm_",    r$id)
```

with:

```r
        # Per-row input ids
        type_id  <- paste0("type_", r$id)
        num_id   <- paste0("num_",  r$id)
        den_id   <- paste0("den_",  r$id)
        num2_id  <- paste0("num2_", r$id)
        den2_id  <- paste0("den2_", r$id)
        swap_id  <- paste0("swap_", r$id)
        label_id <- paste0("label_", r$id)
        rm_id    <- paste0("rm_",    r$id)
```

(`expr_id` is removed — the Advanced text area is gone.)

**(9b)** Replace the entire `if (identical(r$type, "advanced")) { ... } else { ... }` panel/validation block.

> **ASCII-ONLY CAUTION:** The R source must stay ASCII. The minus glyph shown as `−` in the code below MUST be written as the escape `"−"` in the actual `.R` file (this is how the existing `simple` panel already encodes it — see the current source). Likewise the hint string `"( A - B ) - ( C - D )  ..."` must use `−` for each minus, written as a normal R string with the escapes. Do NOT paste a literal Unicode minus. After editing, Step 14's `grep` for non-ASCII bytes must pass.

The current block is:

```r
        # Build type-specific panel
        if (identical(r$type, "advanced")) {
          # Advanced panel: free-text expression
          advanced_panel <- tagList(
            tags$label(
              tags$span(style = "font-weight: 600;", "Contrast expression:"),
              tags$span(style = "font-size: 11px; color: #6c757d; margin-left: 6px;",
                        "(refer to design coefficients shown on the right)")
            ),
            textAreaInput(ns(expr_id), label = NULL, value = r$advanced_expr %||% "",
                          width = "100%", rows = 2,
                          placeholder = "e.g. (groupA.X - groupA.Y) - (groupB.X - groupB.Y)")
          )
          expr_for_preview <- sanitize_label(r$advanced_expr %||% "")
          # Validate
          validation <- validate_advanced_expr(r$advanced_expr %||% "", coefs)
        } else {
          # Simple panel: numerator / denominator dropdowns + swap
          advanced_panel <- div(
            style = paste(
              "display: grid;",
              "grid-template-columns: 1fr 24px 1fr 40px;",
              "align-items: center;",
              "column-gap: 8px;"
            ),
            tags$div(
              tags$label("Numerator", style = "font-size: 12px; color: #6c757d; margin-bottom: 2px;"),
              selectizeInput(ns(num_id), label = NULL,
                             choices = choices, selected = r$num %||% "",
                             options = list(placeholder = "choose coefficient"),
                             width = "100%")
            ),
            tags$div(style = "text-align: center; font-weight: bold; font-size: 16px; padding-top: 18px;", "\u2212"),
            tags$div(
              tags$label("Denominator", style = "font-size: 12px; color: #6c757d; margin-bottom: 2px;"),
              selectizeInput(ns(den_id), label = NULL,
                             choices = choices, selected = r$den %||% "",
                             options = list(placeholder = "choose coefficient"),
                             width = "100%")
            ),
            tags$div(
              style = "text-align: center; padding-top: 18px;",
              actionButton(ns(swap_id), label = NULL,
                           icon = icon("exchange-alt"),
                           title = "Swap numerator and denominator",
                           class = "btn btn-sm btn-default")
            )
          )
          expr_for_preview <- build_simple_expr(r$num %||% "", r$den %||% "")
          # Validate: both must be non-empty AND present in design coefs
          if (!nzchar(r$num %||% "") || !nzchar(r$den %||% "")) {
            validation <- list(ok = FALSE, message = "(choose numerator and denominator)",
                               unknown = character(0))
          } else if (identical(r$num, r$den)) {
            validation <- list(ok = FALSE, message = "numerator and denominator are identical (zero contrast)",
                               unknown = character(0))
          } else {
            validation <- validate_advanced_expr(expr_for_preview, coefs)
          }
        }
```

Replace that whole block with:

```r
        # A single labelled coefficient dropdown, reused across slots.
        coef_dropdown <- function(input_id, lbl_text, selected_val) {
          tags$div(
            tags$label(lbl_text, style = "font-size: 12px; color: #6c757d; margin-bottom: 2px;"),
            selectizeInput(ns(input_id), label = NULL,
                           choices = choices, selected = selected_val %||% "",
                           options = list(placeholder = "choose coefficient"),
                           width = "100%")
          )
        }
        minus_glyph <- function() {
          tags$div(style = "text-align: center; font-weight: bold; font-size: 16px; padding-top: 18px;", "\u2212")
        }

        # Build type-specific panel + preview expr + validation
        if (identical(r$type, "multi")) {
          # Multi (2x2) panel: four dropdowns wired into (A - B) - (C - D).
          slot_panel <- tagList(
            tags$div(
              style = "font-size: 12px; color: #6c757d; margin-bottom: 4px;",
              "( A \u2212 B ) \u2212 ( C \u2212 D )  \u2014  difference of differences"
            ),
            div(
              style = paste(
                "display: grid;",
                "grid-template-columns: 1fr 24px 1fr;",
                "align-items: center; column-gap: 8px; margin-bottom: 6px;"
              ),
              coef_dropdown(num_id,  "A", r$num),
              minus_glyph(),
              coef_dropdown(den_id,  "B", r$den)
            ),
            tags$div(style = "text-align: center; font-weight: bold; font-size: 16px; margin: 2px 0;", "\u2212"),
            div(
              style = paste(
                "display: grid;",
                "grid-template-columns: 1fr 24px 1fr;",
                "align-items: center; column-gap: 8px;"
              ),
              coef_dropdown(num2_id, "C", r$num2),
              minus_glyph(),
              coef_dropdown(den2_id, "D", r$den2)
            )
          )
          expr_for_preview <- build_multi_expr(r$num %||% "", r$den %||% "",
                                                r$num2 %||% "", r$den2 %||% "")
          slots <- c(r$num %||% "", r$den %||% "", r$num2 %||% "", r$den2 %||% "")
          if (any(!nzchar(slots))) {
            validation <- list(ok = FALSE, message = "(choose all four coefficients A, B, C, D)",
                               unknown = character(0))
          } else {
            validation <- validate_advanced_expr(expr_for_preview, coefs)
          }
        } else {
          # Single (1x1) panel: numerator / denominator dropdowns + swap
          slot_panel <- div(
            style = paste(
              "display: grid;",
              "grid-template-columns: 1fr 24px 1fr 40px;",
              "align-items: center;",
              "column-gap: 8px;"
            ),
            coef_dropdown(num_id, "Numerator", r$num),
            minus_glyph(),
            coef_dropdown(den_id, "Denominator", r$den),
            tags$div(
              style = "text-align: center; padding-top: 18px;",
              actionButton(ns(swap_id), label = NULL,
                           icon = icon("exchange-alt"),
                           title = "Swap numerator and denominator",
                           class = "btn btn-sm btn-default")
            )
          )
          expr_for_preview <- build_simple_expr(r$num %||% "", r$den %||% "")
          if (!nzchar(r$num %||% "") || !nzchar(r$den %||% "")) {
            validation <- list(ok = FALSE, message = "(choose numerator and denominator)",
                               unknown = character(0))
          } else if (identical(r$num, r$den)) {
            validation <- list(ok = FALSE, message = "numerator and denominator are identical (zero contrast)",
                               unknown = character(0))
          } else {
            validation <- validate_advanced_expr(expr_for_preview, coefs)
          }
        }
```

**(9c)** Replace the `effective_label` computation:

```r
        # Compute effective label (auto or user-edited)
        effective_label <- if (isTRUE(r$label_user_edited) && nzchar(r$label %||% "")) {
          r$label
        } else if (identical(r$type, "advanced")) {
          # Default advanced label: the id + truncated expr
          if (nzchar(r$advanced_expr %||% "")) sanitize_label(r$advanced_expr) else ""
        } else {
          make_simple_label(r$num %||% "", r$den %||% "")
        }
```

with:

```r
        # Compute effective label (auto or user-edited)
        effective_label <- if (isTRUE(r$label_user_edited) && nzchar(r$label %||% "")) {
          r$label
        } else if (identical(r$type, "multi")) {
          make_multi_label(r$num %||% "", r$den %||% "", r$num2 %||% "", r$den2 %||% "")
        } else {
          make_simple_label(r$num %||% "", r$den %||% "")
        }
```

**(9d)** Delete the direction-sentence computation:

```r
        # Direction sentence (only meaningful for simple)
        dir_sent <- if (identical(r$type, "simple") && validation$ok) {
          direction_sentence_simple(effective_label, r$num, r$den)
        } else {
          ""
        }
```

Remove that block entirely (do not replace).

**(9e)** Replace the radio-button choices. Change:

```r
              radioButtons(ns(type_id), label = NULL,
                           choices = c("Simple" = "simple", "Advanced" = "advanced"),
                           selected = r$type %||% "simple",
                           inline = TRUE)
```

to:

```r
              radioButtons(ns(type_id), label = NULL,
                           choices = c("Single coef (1x1)" = "simple",
                                       "Multi coef (2x2)" = "multi"),
                           selected = r$type %||% "simple",
                           inline = TRUE)
```

**(9f)** Replace the body reference and remove the direction-sentence render. Change:

```r
          # Type-specific body
          advanced_panel,
```

to:

```r
          # Type-specific body
          slot_panel,
```

And delete the direction-sentence render line:

```r
          # Direction sentence (simple only)
          if (nzchar(dir_sent)) div(class = "direction-sentence", dir_sent),
```

(Remove it entirely; leave the expr-preview and validation-msg blocks that follow.)

### Sub-step group D — persist observer + swap + contrast_specs

- [ ] **Step 10: Rewrite the persist observer for the new fields and toggle-clears-slots**

Replace the persist observer body (the `observe({ ... })` starting with `rows <- contrast_rows()` and the comment about persisting per-row edits) — specifically the `new_rows <- lapply(rows, function(r) { ... })` internals. The current version:

```r
      new_rows <- lapply(rows, function(r) {
        # Type radio
        type_val <- input[[paste0("type_", r$id)]]
        if (!is.null(type_val) && !identical(type_val, r$type)) {
          r$type <- type_val
          changed <<- TRUE
        }
        # Simple fields
        if (identical(r$type, "simple")) {
          num_val <- input[[paste0("num_", r$id)]]
          den_val <- input[[paste0("den_", r$id)]]
          if (!is.null(num_val) && !identical(num_val, r$num)) {
            r$num <- num_val; changed <<- TRUE
          }
          if (!is.null(den_val) && !identical(den_val, r$den)) {
            r$den <- den_val; changed <<- TRUE
          }
        }
        # Advanced field
        if (identical(r$type, "advanced")) {
          expr_val <- input[[paste0("expr_", r$id)]]
          if (!is.null(expr_val) && !identical(expr_val, r$advanced_expr %||% "")) {
            r$advanced_expr <- expr_val; changed <<- TRUE
          }
        }
        # Label input
        label_val <- input[[paste0("label_", r$id)]]
        if (!is.null(label_val)) {
          label_clean <- sanitize_label(label_val)
          # Compute what the auto label would be right now
          auto_label <- if (identical(r$type, "simple")) {
            make_simple_label(r$num %||% "", r$den %||% "")
          } else {
            sanitize_label(r$advanced_expr %||% "")
          }
          if (!identical(label_clean, r$label %||% "")) {
            r$label <- label_clean
            # If the new label differs from the auto-label, flag as user-edited
            r$label_user_edited <- !identical(label_clean, auto_label)
            changed <<- TRUE
          }
        }
        r
      })
```

Replace with:

```r
      new_rows <- lapply(rows, function(r) {
        # Type radio. On an actual mode switch, clear ALL slots (both modes
        # share num/den; multi adds num2/den2) so no stale value carries across
        # the Single<->Multi shape change.
        type_val <- input[[paste0("type_", r$id)]]
        if (!is.null(type_val) && !identical(type_val, r$type)) {
          r$type <- type_val
          r$num <- ""; r$den <- ""; r$num2 <- ""; r$den2 <- ""
          if (!isTRUE(r$label_user_edited)) r$label <- ""
          changed <<- TRUE
          # Skip reading the (now-stale) slot inputs this cycle; the re-render
          # will repopulate empty dropdowns.
          return(r)
        }
        # Slot A/B (both modes)
        num_val <- input[[paste0("num_", r$id)]]
        den_val <- input[[paste0("den_", r$id)]]
        if (!is.null(num_val) && !identical(num_val, r$num)) {
          r$num <- num_val; changed <<- TRUE
        }
        if (!is.null(den_val) && !identical(den_val, r$den)) {
          r$den <- den_val; changed <<- TRUE
        }
        # Slot C/D (multi only)
        if (identical(r$type, "multi")) {
          num2_val <- input[[paste0("num2_", r$id)]]
          den2_val <- input[[paste0("den2_", r$id)]]
          if (!is.null(num2_val) && !identical(num2_val, r$num2)) {
            r$num2 <- num2_val; changed <<- TRUE
          }
          if (!is.null(den2_val) && !identical(den2_val, r$den2)) {
            r$den2 <- den2_val; changed <<- TRUE
          }
        }
        # Label input
        label_val <- input[[paste0("label_", r$id)]]
        if (!is.null(label_val)) {
          label_clean <- sanitize_label(label_val)
          auto_label <- if (identical(r$type, "multi")) {
            make_multi_label(r$num %||% "", r$den %||% "", r$num2 %||% "", r$den2 %||% "")
          } else {
            make_simple_label(r$num %||% "", r$den %||% "")
          }
          if (!identical(label_clean, r$label %||% "")) {
            r$label <- label_clean
            r$label_user_edited <- !identical(label_clean, auto_label)
            changed <<- TRUE
          }
        }
        r
      })
```

- [ ] **Step 11: Confirm the swap observer needs no change**

The swap observer (`observe({ rows <- contrast_rows(); lapply(rows, function(r) { swap_id <- ...`) only swaps `num`/`den` and regenerates via `make_simple_label` — correct for Single, and Multi has no swap button so `swap_<id>` never fires for a multi row. Verify visually that it still references only `x$num`/`x$den` and `make_simple_label`; no edit needed. (If it references `advanced_expr`, remove that — but per the current source it does not.)

- [ ] **Step 12: Rewrite `contrast_specs` for the two types**

Replace the `contrast_specs <- reactive({ ... })` body's `specs <- lapply(...)`:

```r
      specs <- lapply(seq_along(rows), function(i) {
        r <- rows[[i]]
        did <- dids[i]
        if (identical(r$type, "advanced")) {
          e <- r$advanced_expr %||% ""
          if (!nzchar(trimws(e))) return(NULL)
          lbl <- if (isTRUE(r$label_user_edited) && nzchar(r$label %||% ""))
                    r$label else sanitize_label(e)
          list(id = did, label = lbl, expr = e, type = "advanced")
        } else {
          if (!nzchar(r$num %||% "") || !nzchar(r$den %||% "")) return(NULL)
          if (identical(r$num, r$den)) return(NULL)
          e <- build_simple_expr(r$num, r$den)
          lbl <- if (isTRUE(r$label_user_edited) && nzchar(r$label %||% ""))
                    r$label else make_simple_label(r$num, r$den)
          list(id = did, label = lbl, expr = e, type = "simple")
        }
      })
```

with:

```r
      specs <- lapply(seq_along(rows), function(i) {
        r <- rows[[i]]
        did <- dids[i]
        if (identical(r$type, "multi")) {
          slots <- c(r$num %||% "", r$den %||% "", r$num2 %||% "", r$den2 %||% "")
          if (any(!nzchar(slots))) return(NULL)
          e <- build_multi_expr(r$num, r$den, r$num2, r$den2)
          lbl <- if (isTRUE(r$label_user_edited) && nzchar(r$label %||% ""))
                    r$label else make_multi_label(r$num, r$den, r$num2, r$den2)
          list(id = did, label = lbl, expr = e, type = "multi")
        } else {
          if (!nzchar(r$num %||% "") || !nzchar(r$den %||% "")) return(NULL)
          if (identical(r$num, r$den)) return(NULL)
          e <- build_simple_expr(r$num, r$den)
          lbl <- if (isTRUE(r$label_user_edited) && nzchar(r$label %||% ""))
                    r$label else make_simple_label(r$num, r$den)
          list(id = did, label = lbl, expr = e, type = "simple")
        }
      })
```

### Sub-step group E — verify Task 3

- [ ] **Step 13: Confirm no stragglers remain**

Run: `grep -n "advanced_expr\|add_contrast_advanced\|suggest_pairwise_contrasts\|enumerate_pairwise_simple_rows\|direction_sentence_simple\|\"advanced\"\|expr_id" R/tab_lm_setup.R`
Expected: **zero matches.** (If any remain, they are leftover references — remove them before proceeding.)

- [ ] **Step 14: Verify ASCII-only**

Run: `LC_ALL=C grep -nP "[^\x00-\x7F]" R/tab_lm_setup.R || echo "ASCII clean"`
Expected: `ASCII clean`.

- [ ] **Step 15: Load the package and run the full LM-adjacent suite**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); library(testthat); r <- test_dir("tests/testthat", filter="lm|contrast", reporter="summary"); df <- as.data.frame(r); cat("FAILED:", sum(df$failed), "ERROR:", sum(df$error), "\n")'`
Expected: `FAILED: 0 ERROR: 0`. (This runs all `test-lm-*` and `test-*contrast*` files against the loaded, refactored module.)

- [ ] **Step 16: Commit**

```bash
git add R/tab_lm_setup.R
git commit -m "feat(lm): replace Simple/Advanced contrast modes with Single/Multi-coef"
```

---

## Task 4: End-to-end seam test — Multi expression recovers a planted interaction

Proves the authoring layer (`build_multi_expr`) connects to the frozen backend. This is a thin seam check — the deep numeric oracles already live in `test-lm-advanced-contrast.R` and stay untouched.

**Files:**
- Create: `tests/testthat/test-lm-multi-contrast-seam.R`

**Interfaces:**
- Consumes: `build_multi_expr` (Task 1); `lm.regression` (existing backend).

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-lm-multi-contrast-seam.R`:

```r
################################################################################
# Seam test: a Multi (2x2) contrast expression assembled by build_multi_expr()
# recovers a planted interaction when run through lm.regression()'s contrast
# path. This proves the AUTHORING layer connects to the frozen backend; the
# deep numeric oracles for the contrast path live in test-lm-advanced-contrast.R.
################################################################################

library(testthat)

# Balanced 2x2 factorial with a planted Drug:T2 interaction; references
# Vehicle / T1. Cell-means coded (~ 0 + treatment:timepoint) so all four cells
# are real design coefficients that build_multi_expr can reference by name.
make_2x2_cellmeans_gct <- function(seed = 202L, effect = 3.0, reps = 12L,
                                    n_null = 40L, n_true = 20L) {
  set.seed(seed)
  n_feat <- n_null + n_true
  grid <- expand.grid(
    treatment = c("Vehicle", "Drug"),
    timepoint = c("T1", "T2"),
    rep = seq_len(reps),
    stringsAsFactors = FALSE
  )
  n_samp <- nrow(grid)
  sample_ids <- paste0("s", sprintf("%03d", seq_len(n_samp)))
  feat_ids   <- paste0("F", sprintf("%03d", seq_len(n_feat)))
  cdesc <- data.frame(
    id        = sample_ids,
    treatment = factor(grid$treatment, levels = c("Vehicle", "Drug")),
    timepoint = factor(grid$timepoint, levels = c("T1", "T2")),
    row.names = sample_ids, stringsAsFactors = FALSE
  )
  rdesc <- data.frame(
    id = feat_ids, geneSymbol = paste0("G", seq_len(n_feat)),
    row.names = feat_ids, stringsAsFactors = FALSE
  )
  mat <- matrix(rnorm(n_feat * n_samp, mean = 10, sd = 1),
                nrow = n_feat, dimnames = list(feat_ids, sample_ids))
  drug_t2 <- cdesc$treatment == "Drug" & cdesc$timepoint == "T2"
  mat[seq_len(n_true), drug_t2] <- mat[seq_len(n_true), drug_t2] + effect
  gct <- methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
                      rid = feat_ids, cid = sample_ids)
  list(gct = gct, n_true = n_true, n_null = n_null, effect = effect)
}

test_that("build_multi_expr output recovers the planted 2x2 interaction via lm.regression", {
  fx <- make_2x2_cellmeans_gct()

  # Cell-means design: coefficients are the four cells.
  # (Drug:T2 - Drug:T1) - (Vehicle:T2 - Vehicle:T1) == the interaction.
  expr <- build_multi_expr(
    "treatmentDrug:timepointT2", "treatmentDrug:timepointT1",
    "treatmentVehicle:timepointT2", "treatmentVehicle:timepointT1"
  )
  expect_equal(
    expr,
    "(treatmentDrug:timepointT2 - treatmentDrug:timepointT1) - (treatmentVehicle:timepointT2 - treatmentVehicle:timepointT1)"
  )

  res <- lm.regression(
    gct = fx$gct,
    formula_string = "~ 0 + treatment:timepoint",
    variable_types = list(treatment = "factor", timepoint = "factor"),
    contrasts_list = stats::setNames(list(expr), "DoD")
  )

  expect_true(all(c("logFC.DoD", "P.Value.DoD", "adj.P.Val.DoD") %in% colnames(res)))
  tp <- seq_len(fx$n_true)
  nl <- (fx$n_true + 1L):(fx$n_true + fx$n_null)
  # Planted interaction recovered on true features; nulls ~ 0.
  expect_equal(median(res$logFC.DoD[tp]), fx$effect, tolerance = 0.5)
  expect_lt(abs(median(res$logFC.DoD[nl])), 0.4)
  # True block well separated from nulls, and detected at BH 5%.
  expect_gt(median(res$logFC.DoD[tp]), median(res$logFC.DoD[nl]) + 1.5)
  expect_gt(mean(res$adj.P.Val.DoD[tp] < 0.05), 0.9)
})
```

- [ ] **Step 2: Run the test to verify it passes**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_file("tests/testthat/test-lm-multi-contrast-seam.R", reporter="summary")'`
Expected: PASS.

> **If the cell-means coefficient names differ** (e.g. limma/model.matrix emits `treatment:timepointDrug:T2`-style names): first inspect the real design column names, then correct the four tokens in the test. Get the true names with:
> `Rscript -e 'suppressMessages(devtools::load_all(".")); fx <- source("tests/testthat/test-lm-multi-contrast-seam.R", local=new.env())' ` is not runnable standalone — instead run inline:
> `Rscript -e 'cd <- data.frame(treatment=factor(rep(c("Vehicle","Drug"),4)), timepoint=factor(rep(c("T1","T2"),each=4))); colnames(model.matrix(~ 0 + treatment:timepoint, cd))'`
> and use those exact names in both the `build_multi_expr(...)` call and the expected-string assertion.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-lm-multi-contrast-seam.R
git commit -m "test(lm): seam test that Multi 2x2 contrast recovers planted interaction"
```

---

## Task 5: Full-suite verification + manual browser check

**Files:** none (verification only).

- [ ] **Step 1: Regenerate docs (safety — new internal helpers)**

Run: `Rscript -e 'devtools::document()'`
Expected: no NAMESPACE diff (helpers are unexported). If `NAMESPACE`/`man` change unexpectedly, review — internal helpers should NOT be exported. Commit only if a legitimate `man/` doc was regenerated for an already-exported symbol.

- [ ] **Step 2: Run the entire test suite**

Run: `Rscript -e 'suppressMessages(devtools::load_all(".")); library(testthat); r <- test_dir("tests/testthat", reporter="summary"); df <- as.data.frame(r); cat("FAILED:", sum(df$failed), "ERROR:", sum(df$error), "\n")'`
Expected: `FAILED: 0 ERROR: 0`.

- [ ] **Step 3: Final straggler grep across the whole package**

Run: `grep -rn "advanced_expr\|add_contrast_advanced\|suggest_pairwise_contrasts\|enumerate_pairwise_simple_rows\|direction_sentence_simple" R/ tests/`
Expected: **zero matches** anywhere.

- [ ] **Step 4: Manual browser verification** (requires the user to launch the app from their host)

The user runs, in their R session:
```r
devtools::load_all(".")
Protigy::launchApp()
```
Then in the Linear Model tab → contrast builder, confirm:
1. Radio shows **Single coef (1x1)** / **Multi coef (2x2)**; no Advanced.
2. Only **+ Add contrast** and **Clear all** buttons (no "+ Add Simple/Advanced", no "+ Suggest all pairwise").
3. Switch a card to **Multi coef (2x2)** → four labelled dropdowns A/B/C/D appear in the `(A − B) − (C − D)` layout; all slots empty.
4. Pick four coefficients → `expr:` preview reads `(A - B) - (C - D)`; label auto-fills the nested parenthesized form; card turns green ("valid"); no direction sentence anywhere.
5. Toggle the card back to Single → slots clear; toggling to Multi again → slots empty (no stale values).
6. A valid Single or Multi contrast makes the Run/validation summary report "contrast(s) valid and ready to run", and running produces `logFC.<label>` columns.

- [ ] **Step 5: If any commits pending (docs), commit; else done**

```bash
git status --short
# commit only if step 1 produced a legitimate tracked change
```

---

## Self-Review Notes (author)

- **Spec coverage:** modes rename (T3 9e, 6), Advanced removal (T3 3,4,9b,12), Multi 2×2 template (T1, T3 9b), row model `num2`/`den2` + drop `advanced_expr` (T3 1,2,3,5,10,12), unchanged backend (constraint + no edits to validate/makeContrasts/lm.regression), auto-label nested parens (T1 `make_multi_label`, T3 9c), direction sentence removed both modes (T2 1,3; T3 8,9d,9f), single `+ Add contrast` + `Clear all`, `+ Suggest all pairwise` removed (T3 4,6), Intercept excluded (reuses existing `simple_coefs`/`choices`, unchanged), tests (T1, T2, T4), Q9 cell-means dependency (documented in spec; seam test T4 uses `~ 0 + ...` to exercise it). All covered.
- **Type consistency:** `type` values `"simple"`/`"multi"` used identically in render (9b,9c,9e), persist (10), specs (12). Field names `num`/`den`/`num2`/`den2` consistent across seed(2), add(3), clear(5), render(9a,9b), persist(10), specs(12). `build_multi_expr`/`make_multi_label` signatures `(a,b,c,d)` consistent between T1 definition and all call sites.
- **Placeholder scan:** all code steps show full code; the one conditional in T4 Step 2 gives an exact runnable command to resolve coefficient names rather than a vague "adjust as needed".
