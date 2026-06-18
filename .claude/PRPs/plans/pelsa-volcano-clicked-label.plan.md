# Plan: PELSA volcano dark-gold label for the clicked peptide

## Summary
When a peptide is clicked on the PELSA volcano (or a segment is clicked on the
Woods plot, which also drives `selection()`), draw ONE dark-gold text label
`<gene>_aa<pep_start>` for the clicked peptide only — in addition to the existing
gold highlight of the clicked peptide + its same-accession siblings. The label
updates on every selection change and clears with the selection, delivered as a
proxy `addTraces`/`deleteTraces` text trace so the ~100k-point WebGL cloud never
rebuilds.

## User Story
As a PELSA user inspecting a volcano, I want the peptide I clicked to be labeled
with its `<gene>_aa<position>` in dark gold, so that I can tell the clicked
peptide apart from its gold-highlighted siblings at a glance.

## Problem → Solution
Today a click golds the clicked peptide and all same-accession siblings, but
nothing identifies WHICH dot was clicked. → Add a single dark-gold text label on
the clicked peptide, riding the existing gold-overlay proxy mechanism, that
updates/clears with the selection and does not touch the existing Top-N/label-mode
baked annotations.

## Metadata
- **Complexity**: Small
- **Source PRD**: `docs/superpowers/specs/2026-06-17-pelsa-volcano-clicked-label-design.md`
- **PRD Phase**: N/A (standalone design spec)
- **Estimated Files**: 4 (3 source + 1 test)

---

## UX Design

### Before
```
Click a dot ─▶ clicked peptide + all same-accession
               siblings turn GOLD. No text. You cannot
               tell which gold dot you actually clicked.
```

### After
```
Click a dot ─▶ clicked peptide + siblings turn GOLD,
               AND the clicked dot gets a dark-gold
               "<gene>_aa<pos>" label (white halo).
Click again ─▶ label moves to the new clicked peptide.
Clear sel.  ─▶ gold + label both removed.
Woods click ─▶ fixed panel refreshes, volcano siblings
               gold, clicked peptide gets the label.
```

### Interaction Changes
| Touchpoint | Before | After | Notes |
|---|---|---|---|
| Volcano click | Gold highlight only | Gold + dark-gold label on clicked peptide | Label = `<gene>_aa<pep_start>` |
| Click another point | Gold moves | Gold + label both move | Single label always |
| Clear selection / find replaces | Gold removed | Gold + label removed | |
| Woods segment click | Sets selection, siblings gold | Same + clicked peptide labeled | Woods sets `row = NA`; resolve by seq |
| Top-N / label-mode change | Baked labels rebuild | Unchanged; clicked label re-applied after flush | No interference |

---

## Mandatory Reading

| Priority | File | Lines | Why |
|---|---|---|---|
| P0 | `R/tab_pelsa_section3_helpers.R` | 439-480 | `pelsa_volcano_tip` (stem logic to reuse) + `pelsa_volcano_gold_trace` (the trace-list shape to mirror) |
| P0 | `R/tab_pelsa_section3.R` | 828-883 | Gold overlay observer + `apply_gold_overlay()` + `gold_present()` bookkeeping to extend |
| P0 | `R/tab_pelsa_section3_recolor_helpers.R` | 38-146 | `pelsa_volcano_resolve_click` (sets `selection$row/peptide_seq/accession`) + `pelsa_volcano_highlight_mask` (row-resolution convention) |
| P1 | `R/tab_pelsa_section3.R` | 911-927, 1147-1163 | Volcano + Woods click observers that set `selection()` (Woods sets `row = NA`) |
| P1 | `R/tab_pelsa_constants.R` | 1-9 | Where `.PELSA_GOLD` lives; add `.PELSA_GOLD_DARK` here |
| P2 | `tests/testthat/test-pelsa-recolor.R` | 1-60 | Test file/style to mirror (`.mk_df()` + `test_that`) |

## External Documentation

No external research needed — feature uses established internal patterns
(plotly `scattergl` text trace + `plotlyProxyInvoke` already used by the gold
overlay). The WebGL constraint (proxy `relayout(annotations=)` unreliable;
`addTraces` reliable) is already documented in `CLAUDE.md` and
`R/tab_pelsa_section3.R:800-814` and is the reason for the trace-based approach.

---

## Patterns to Mirror

### NAMING_CONVENTION (helper functions)
```r
// SOURCE: R/tab_pelsa_section3_helpers.R:467
pelsa_volcano_gold_trace <- function(df, selection = NULL, find_mask = NULL) {
```
Public PELSA helpers: `pelsa_volcano_<verb>` snake_case, `@noRd` roxygen, args
default-valued. Private constants: `.PELSA_*` SCREAMING. New helper name:
`pelsa_volcano_clicked_label_trace`.

### TRACE_LIST_SHAPE (what to return — mirror exactly)
```r
// SOURCE: R/tab_pelsa_section3_helpers.R:472-479
list(
  type = "scattergl", mode = "markers",
  x = as.numeric(d$logFC), y = as.numeric(d$logP),
  text = pelsa_volcano_tip(d), hoverinfo = "text",
  marker = list(color = .PELSA_GOLD, size = 7,
                line = list(color = .PELSA_VOLCANO_MARKER_EDGE, width = 0.5)),
  showlegend = FALSE, meta = "pelsa_gold"
)
```
A plain list ready for `plotlyProxyInvoke("addTraces", ...)`. Our label trace
uses `mode = "text"`, `meta = "pelsa_gold_label"`.

### STEM / LABEL LOGIC (reuse verbatim — do NOT reinvent)
```r
// SOURCE: R/tab_pelsa_section3_helpers.R:443-448
gene_fb <- ifelse(is.na(d$winning_gene) | !nzchar(d$winning_gene),
                  d$PG.Genes, d$winning_gene)
acc_fb <- ifelse(is.na(d$winning_accession) | !nzchar(d$winning_accession),
                 d$PG.ProteinAccessions, d$winning_accession)
stem <- ifelse(is.na(gene_fb) | !nzchar(gene_fb), acc_fb, gene_fb)
pep_lab <- paste0(stem, "_aa", d$pep_start)
```
This is the canonical `<gene>_aa<pos>` build (accession fallback included).
Self-curated species already have `winning_gene` blanked upstream, so the
accession fallback fires automatically — NO self-curated special-casing.

### ROW RESOLUTION (selection -> df row index)
```r
// SOURCE: R/tab_pelsa_section3.R:971-974 (pin metadata render)
row <- sel$row
if (is.null(row) || is.na(row)) {
  row <- match(sel$peptide_seq, as.character(df$id))
}
```
`selection$row` is an index into `active_volcano_df()` (the click resolver runs
against it, `tab_pelsa_section3.R:922`). The Woods observer sets `row = NA`
(`tab_pelsa_section3.R:1162`), so the `match()` fallback is REQUIRED.

### PROXY OVERLAY OBSERVER (extend this — the integration point)
```r
// SOURCE: R/tab_pelsa_section3.R:836-857
gold_present <- reactiveVal(FALSE)
gold_proxy   <- plotly::plotlyProxy("pelsa_volcano_plot", session)

apply_gold_overlay <- function() {
  df <- tryCatch(active_volcano_df(), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0L) return()
  if (isTRUE(gold_present())) {
    plotly::plotlyProxyInvoke(gold_proxy, "deleteTraces", list(2L))
    gold_present(FALSE)
  }
  fr <- find_result()
  tr <- pelsa_volcano_gold_trace(
    df, selection(), if (is.null(fr)) NULL else fr$mask)
  if (!is.null(tr)) {
    plotly::plotlyProxyInvoke(gold_proxy, "addTraces", tr)
    gold_present(TRUE)
  }
}
```
Base figure has exactly 2 point traces (bg=0, markers=1). Overlays start at
index 2. The label, when present, is index 3.

### TEST_STRUCTURE
```r
// SOURCE: tests/testthat/test-pelsa-recolor.R:1-29
library(testthat)
.mk_df <- function() {
  data.frame(
    id = c("PEPA1", "PEPA2", "PEPB1", "PEPMK"),
    winning_accession = c("ACCA", "ACCA", "ACCB", "ACCMK"),
    is_marker = c(FALSE, FALSE, FALSE, TRUE),
    sig_color = c("#1f4e9c", "darkred", "gray70", "gray70"),
    feature_color = c("#111111", "#222222", "#333333", "#444444"),
    stringsAsFactors = FALSE
  )
}
test_that("highlight_mask: selected peptide + same-protein + find, uniform", {
  df <- .mk_df()
  sel <- list(accession = "ACCA", peptide_seq = "PEPA1")
  m <- pelsa_volcano_highlight_mask(df, selection = sel, find_mask = NULL)
  expect_equal(which(m), c(1L, 2L))
})
```
Plain `data.frame` fixtures, no Shiny harness, direct function calls. Tests
exercise the LOADED package, so `devtools::load_all(".")` before running.

---

## Files to Change

| File | Action | Justification |
|---|---|---|
| `R/tab_pelsa_constants.R` | UPDATE | Add `.PELSA_GOLD_DARK` constant |
| `R/tab_pelsa_section3_helpers.R` | UPDATE | Add `pelsa_volcano_clicked_label_trace()` pure helper |
| `R/tab_pelsa_section3.R` | UPDATE | Extend `apply_gold_overlay()` + `gold_present` → `overlay_n` bookkeeping |
| `tests/testthat/test-pelsa-volcano-clicked-label.R` | CREATE | Unit tests for the new helper |

## NOT Building
- A true boxed annotation (white bg + dark-gold border). Not available on a
  scattergl trace; the unreliable relayout path / per-click rebuild are rejected.
  Approximated with dark-gold text + a white halo marker (user-accepted).
- Labels for the sibling peptides — only the clicked peptide is labeled.
- Any change to the best-peptide panel (`pelsa_volcano_best_plot`), the Find
  highlight, the baked Top-N/label-mode annotations, or the static export.
- A label for a MULTI-accession Find (no single peptide) — that path sets
  `selection()` to `NULL`, so the helper returns `NULL` and the gold find-mask
  highlight is untouched. NOTE: a SINGLE-accession Find sets `origin="find"`
  with a concrete `row`/`peptide_seq` (it "opens" one peptide, like a click), so
  it IS labeled — this is intentional and consistent with the click behavior.

---

## Step-by-Step Tasks

### Task 1: Add the dark-gold constant
- **ACTION**: Add a constant for the clicked-label text color.
- **IMPLEMENT**: In `R/tab_pelsa_constants.R`, after the existing `.PELSA_GOLD`
  block (line 8), add:
  ```r
  .PELSA_GOLD_DARK <- "#8B6914"  # clicked-peptide label text (dark gold)
  ```
- **MIRROR**: `.PELSA_GOLD <- "#D4AF37"` style (SCREAMING `.PELSA_*`, trailing
  comment).
- **IMPORTS**: none.
- **GOTCHA**: ASCII-only R source (CLAUDE.md) — `#8B6914` is fine; no Unicode.
- **VALIDATE**: `devtools::load_all(".")` then `.PELSA_GOLD_DARK` resolves to
  `"#8B6914"`.

### Task 2: Add the `pelsa_volcano_clicked_label_trace()` helper
- **ACTION**: Add a pure helper returning a one-point text trace list, or `NULL`.
- **IMPLEMENT**: In `R/tab_pelsa_section3_helpers.R`, add directly AFTER
  `pelsa_volcano_gold_trace` (ends line 480, before the
  `# ---- shared plot-assembly` banner at line 482):
  ```r
  # Build the dark-gold LABEL overlay trace for the CLICKED peptide only (NOT its
  # siblings): a one-point scattergl text trace, ready for
  # plotlyProxyInvoke("addTraces", ...). The label is "<gene>_aa<pep_start>" built
  # with the SAME stem logic as pelsa_volcano_tip (gene -> accession fallback;
  # self-curated rows already carry a blanked winning_gene so the accession
  # fallback fires). Dark-gold text + a white halo marker (a true boxed annotation
  # is not available on a scattergl trace; proxy relayout(annotations=) is
  # unreliable on this WebGL volcano). Returns NULL when nothing is selected or the
  # clicked row cannot be resolved (e.g. a pure-Find selection). @noRd
  pelsa_volcano_clicked_label_trace <- function(df, selection = NULL) {
    if (!is.data.frame(df) || nrow(df) == 0L || is.null(selection)) return(NULL)
    row <- selection$row
    if (is.null(row) || length(row) != 1L || is.na(row)) {
      seq <- selection$peptide_seq
      if (is.null(seq) || length(seq) != 1L || is.na(seq) || !nzchar(seq)) {
        return(NULL)
      }
      row <- match(as.character(seq), as.character(df$id))
    }
    if (is.na(row) || row < 1L || row > nrow(df)) return(NULL)
    d <- df[row, , drop = FALSE]
    if (is.na(d$logFC) || is.na(d$logP)) return(NULL)

    gene_fb <- ifelse(is.na(d$winning_gene) | !nzchar(d$winning_gene),
                      d$PG.Genes, d$winning_gene)
    acc_fb <- ifelse(is.na(d$winning_accession) | !nzchar(d$winning_accession),
                     d$PG.ProteinAccessions, d$winning_accession)
    stem <- ifelse(is.na(gene_fb) | !nzchar(gene_fb), acc_fb, gene_fb)
    if (is.na(stem) || !nzchar(stem)) return(NULL)
    label <- paste0(stem, "_aa", d$pep_start)

    list(
      type = "scattergl", mode = "text",
      x = as.numeric(d$logFC), y = as.numeric(d$logP),
      text = label, textposition = "top right",
      textfont = list(color = .PELSA_GOLD_DARK, size = 11, family = "Arial"),
      marker = list(color = "rgba(255,255,255,0.9)", size = 14,
                    line = list(width = 0)),
      hoverinfo = "skip", showlegend = FALSE, meta = "pelsa_gold_label"
    )
  }
  ```
- **MIRROR**: TRACE_LIST_SHAPE + STEM / LABEL LOGIC + ROW RESOLUTION patterns
  above (snippets copied from the same file / `tab_pelsa_section3.R`).
- **IMPORTS**: none (returns a plain list; `.PELSA_GOLD_DARK` from Task 1, same
  package namespace).
- **GOTCHA**: `df[row, ]` yields a 1-row frame so the `ifelse`/`paste0` are
  scalar — do NOT vectorize over the whole df (siblings must stay unlabeled).
  Guard `logFC`/`logP` NA so a coordinate-less row yields `NULL`. ASCII-only.
- **VALIDATE**: `devtools::load_all(".")`; call with a 1-row-resolvable selection
  returns a list with `meta == "pelsa_gold_label"`; `NULL` for `selection = NULL`.

### Task 3: Extend `apply_gold_overlay()` to manage the label trace
- **ACTION**: Manage the overlay as a set of up to two traces (gold markers at
  index 2, label at index 3), replacing the boolean `gold_present` with a count.
- **IMPLEMENT**: In `R/tab_pelsa_section3.R`:
  1. Replace `gold_present <- reactiveVal(FALSE)` (line 836) with:
     ```r
     overlay_n <- reactiveVal(0L)  # how many overlay traces (gold, label) on client
     ```
  2. Replace the body of `apply_gold_overlay` (lines 843-857) with:
     ```r
     apply_gold_overlay <- function() {
       df <- tryCatch(active_volcano_df(), error = function(e) NULL)
       if (is.null(df) || nrow(df) == 0L) return()
       # Delete existing overlays HIGHEST-index-first (label=3, gold=2) so the
       # remaining indices stay valid mid-delete.
       n <- overlay_n()
       if (n >= 2L) plotly::plotlyProxyInvoke(gold_proxy, "deleteTraces", list(3L))
       if (n >= 1L) plotly::plotlyProxyInvoke(gold_proxy, "deleteTraces", list(2L))
       overlay_n(0L)

       fr <- find_result()
       gold_tr <- pelsa_volcano_gold_trace(
         df, selection(), if (is.null(fr)) NULL else fr$mask)
       added <- 0L
       if (!is.null(gold_tr)) {
         plotly::plotlyProxyInvoke(gold_proxy, "addTraces", gold_tr)
         added <- added + 1L
         # The clicked-peptide label only makes sense alongside a gold highlight
         # (a selected peptide). It rides at index 3, on top of the gold markers.
         lab_tr <- pelsa_volcano_clicked_label_trace(df, selection())
         if (!is.null(lab_tr)) {
           plotly::plotlyProxyInvoke(gold_proxy, "addTraces", lab_tr)
           added <- added + 1L
         }
       }
       overlay_n(added)
     }
     ```
  3. In the base-rebuild observer (lines 879-882), replace
     `gold_present(FALSE)` with `overlay_n(0L)`:
     ```r
     session$onFlushed(function() {
       overlay_n(0L)   # the rebuild already cleared the overlay traces
       apply_gold_overlay()
     }, once = TRUE)
     ```
- **MIRROR**: PROXY OVERLAY OBSERVER pattern above.
- **IMPORTS**: none new (`plotly::` already used here; helper is in-package).
- **GOTCHA**: Delete highest index FIRST — deleting index 2 before 3 would
  renumber the label to 2 and the second delete would remove the wrong/absent
  trace. The label is added ONLY when a gold trace exists, so a pure-Find
  highlight (gold present, but `clicked_label_trace` returns `NULL`) correctly
  yields just the gold trace (`added == 1`). The two `observeEvent` blocks that
  call `apply_gold_overlay()` (selection/find observer line 863; base-rebuild
  observer line 875) are UNCHANGED — they already fire on `selection()`, which
  the volcano AND Woods click observers both set.
- **VALIDATE**: `devtools::load_all(".")`, launch app, click a volcano point →
  gold + dark-gold label appear; click another → label moves; clear → both gone;
  Woods click → siblings gold + clicked peptide labeled.

### Task 4: Unit tests for the helper
- **ACTION**: Create `tests/testthat/test-pelsa-volcano-clicked-label.R`.
- **IMPLEMENT**:
  ```r
  library(testthat)

  # Volcano-df-shaped frame. Columns the label trace reads: id, logFC, logP,
  # winning_gene, winning_accession, PG.Genes, PG.ProteinAccessions, pep_start.
  .mk_label_df <- function() {
    data.frame(
      id                   = c("PEPA1", "PEPA2", "PEPB1"),
      logFC                = c(1.5, -0.8, 2.1),
      logP                 = c(3.0, 1.2, 4.4),
      winning_gene         = c("GENEA", "GENEA", ""),       # B blanked (self-curated)
      winning_accession    = c("ACCA", "ACCA", "ACCB"),
      PG.Genes             = c("GENEA", "GENEA", NA_character_),
      PG.ProteinAccessions = c("ACCA", "ACCA", "ACCB"),
      pep_start            = c(101L, 222L, 55L),
      stringsAsFactors     = FALSE, check.names = FALSE
    )
  }

  test_that("labels the clicked peptide as <gene>_aa<pos> via selection$row", {
    df <- .mk_label_df()
    tr <- pelsa_volcano_clicked_label_trace(
      df, list(row = 1L, peptide_seq = "PEPA1"))
    expect_equal(tr$text, "GENEA_aa101")
    expect_equal(tr$x, 1.5)
    expect_equal(tr$y, 3.0)
    expect_equal(tr$meta, "pelsa_gold_label")
    expect_equal(tr$mode, "text")
    expect_equal(tr$textfont$color, .PELSA_GOLD_DARK)
  })

  test_that("falls back to accession when gene is blank (self-curated)", {
    df <- .mk_label_df()
    tr <- pelsa_volcano_clicked_label_trace(
      df, list(row = 3L, peptide_seq = "PEPB1"))
    expect_equal(tr$text, "ACCB_aa55")
  })

  test_that("resolves the row by peptide_seq when selection$row is NA (Woods)", {
    df <- .mk_label_df()
    tr <- pelsa_volcano_clicked_label_trace(
      df, list(row = NA_integer_, peptide_seq = "PEPA2"))
    expect_equal(tr$text, "GENEA_aa222")
    expect_equal(tr$x, -0.8)
  })

  test_that("returns NULL for no selection, empty df, or unresolvable peptide", {
    df <- .mk_label_df()
    expect_null(pelsa_volcano_clicked_label_trace(df, NULL))
    expect_null(pelsa_volcano_clicked_label_trace(
      df[0, , drop = FALSE], list(row = 1L, peptide_seq = "PEPA1")))
    expect_null(pelsa_volcano_clicked_label_trace(
      df, list(row = NA_integer_, peptide_seq = "NOPE")))
    expect_null(pelsa_volcano_clicked_label_trace(
      df, list(row = NA_integer_, peptide_seq = NA_character_)))
  })
  ```
- **MIRROR**: TEST_STRUCTURE (`test-pelsa-recolor.R`).
- **IMPORTS**: `library(testthat)` (top of file).
- **GOTCHA**: `check.names = FALSE` so `PG.Genes` / `PG.ProteinAccessions` keep
  their dots. Run `devtools::load_all(".")` BEFORE `devtools::test()` (tests hit
  the loaded package, per CLAUDE.md).
- **VALIDATE**: `devtools::test_active_file()` on the new file → all pass.

---

## Testing Strategy

### Unit Tests
| Test | Input | Expected Output | Edge Case? |
|---|---|---|---|
| Normal label via row | `row=1, seq=PEPA1` | `text="GENEA_aa101"`, `(x,y)=(1.5,3.0)` | No |
| Accession fallback | `row=3, seq=PEPB1` (gene blank) | `text="ACCB_aa55"` | Yes (self-curated) |
| Row by seq (Woods) | `row=NA, seq=PEPA2` | `text="GENEA_aa222"`, `x=-0.8` | Yes (Woods path) |
| No selection | `selection=NULL` | `NULL` | Yes |
| Empty df | `df[0,]` | `NULL` | Yes |
| Unresolvable peptide | `row=NA, seq="NOPE"` | `NULL` | Yes |
| NA peptide_seq + NA row | `row=NA, seq=NA` | `NULL` | Yes (pure-Find) |
| meta / styling | any valid | `meta="pelsa_gold_label"`, `textfont$color=.PELSA_GOLD_DARK` | No |

### Edge Cases Checklist
- [x] Empty input (empty df → NULL)
- [x] Invalid types / unresolvable (`row=NA` + missing seq → NULL)
- [x] NA coordinates (logFC/logP NA → NULL)
- [x] Self-curated (blank gene → accession fallback)
- [x] Woods path (`row = NA`, resolve by seq)
- [ ] Concurrent access — N/A (pure function)
- [ ] Network failure — N/A

---

## Validation Commands

### Static Analysis / Load
```r
devtools::document()   # only if roxygen @export/@import changed (it does NOT here)
devtools::load_all(".")
```
EXPECT: package loads clean; `.PELSA_GOLD_DARK` and
`pelsa_volcano_clicked_label_trace` resolve.

### Unit Tests (affected file)
```r
devtools::load_all(".")
devtools::test_active_file("tests/testthat/test-pelsa-volcano-clicked-label.R")
```
EXPECT: all new tests pass.

### Full Test Suite
```r
devtools::load_all(".")
devtools::test()
```
EXPECT: no regressions (especially `test-pelsa-recolor.R`, `test-pelsa-volcano-*`).

### R CMD check
```r
devtools::check()
```
EXPECT: no new NOTEs/WARNINGs (watch for non-ASCII / undefined-global notes).

### Browser Validation
```r
Protigy::launchApp()
```
EXPECT: PELSA tab → volcano → click behaviors below.

### Manual Validation
- [ ] Click a volcano dot → clicked peptide + siblings gold; clicked dot shows a
      dark-gold `<gene>_aa<pos>` label with a white halo.
- [ ] Click a different dot → label moves to the new peptide (only one label).
- [ ] Click "Clear selection & highlight" → gold + label both disappear.
- [ ] Type a MULTI-match string into Find → gold find-highlight appears, NO
      label (multi-accession Find sets `selection()` to `NULL`).
- [ ] Type a SINGLE accession into Find → its best peptide is opened, gold +
      dark-gold label appear on that peptide (same as a click; intentional).
- [ ] Pin a protein, click a Woods-plot segment → fixed panel refreshes, volcano
      siblings gold, clicked peptide gets the dark-gold label.
- [ ] Toggle color-mode / Top-N / change contrast → cloud rebuilds, the clicked
      label re-appears after the flush (not dropped).
- [ ] Existing Top-N baked labels still render unchanged alongside the gold label.
- [ ] Self-curated species dataset → label shows `<accession>_aa<pos>`.

---

## Acceptance Criteria
- [ ] All 4 tasks completed.
- [ ] All validation commands pass.
- [ ] New unit tests written and passing; no regressions.
- [ ] No R CMD check NOTEs/WARNINGs introduced.
- [ ] Matches UX: single dark-gold label on the clicked peptide, updates/clears
      with selection, no rebuild on click.

## Completion Checklist
- [ ] Follows discovered patterns (helper naming, trace-list shape, stem logic).
- [ ] Error handling matches style (guard + return NULL; `tryCatch` already wraps
      the caller).
- [ ] Tests follow `test-pelsa-recolor.R` structure.
- [ ] No hardcoded color outside the `.PELSA_GOLD_DARK` constant.
- [ ] ASCII-only R source.
- [ ] No scope additions (siblings unlabeled, best panel/Find/baked labels untouched).
- [ ] Self-contained — no codebase search needed during implementation.

## Risks
| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| Delete order renumbers traces, dropping the magenta marker trace | Low | Med | Delete index 3 before 2; only delete indices that exist (`overlay_n`); never touch indices 0/1 |
| Text trace not rendering on WebGL | Very Low | Med | `addTraces` of a scattergl trace is the proven-reliable path (same as gold markers); text mode is supported on scattergl |
| `selection$row` stale vs active df after a rebuild | Low | Low | Base-rebuild observer re-applies after `onFlushed`; row resolved against current `active_volcano_df()`, with `match()` fallback by seq |
| White halo marker visually clashes with dense cloud | Low | Low | Single point only, drawn on top (highest index); halo is semi-transparent white |

## Notes
- The label trace is index 3 ONLY when a gold trace (index 2) is present, by
  construction in `apply_gold_overlay()`. This keeps the gold-only (pure-Find)
  case at a single overlay trace and the click case at two.
- No roxygen `@export`/`@import` changes → `devtools::document()` is optional;
  `load_all()` suffices. The helper is `@noRd` like its neighbors.
- `.PELSA_GOLD_DARK` (`#8B6914`) is intentionally darker than `.PELSA_GOLD`
  (`#D4AF37`) so the text reads against the gold dots and the white halo.
```
