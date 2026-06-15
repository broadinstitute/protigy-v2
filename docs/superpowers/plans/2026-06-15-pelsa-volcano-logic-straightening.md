# PELSA Volcano Logic-Straightening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Straighten the PELSA volcano + pinned-panel + Woods behaviors to a single-selection model (click OR find, mutually exclusive), add a Find-accession control, recolor (not desaturate) on selection, expand the metadata panel, and recolor the Woods track by significance magnitude.

**Architecture:** One `selection()` reactiveVal (origin click|find) is the single source of truth driving both a gold proxy-restyle highlight on the volcano and the fixed panel. Pure interaction helpers live in a NEW file `R/tab_pelsa_section3_recolor_helpers.R` (TDD'd against the seeded synthetic generator's closed-form ground truth); the module server wires them with one composite restyle observer that survives color-mode rebuilds via `session$onFlushed`.

**Tech Stack:** R, Shiny, plotly (proxy restyle, `meta`-tagged traces), ggplot2, IRanges/data.table (already deps), testthat. ASCII-only source; files <800 lines; `devtools::load_all` after R/ edits; `devtools::document` after roxygen `@export`/`@import` changes (none expected here — all helpers `@noRd`).

**Design spec:** `docs/superpowers/specs/2026-06-15-pelsa-volcano-logic-straightening-design.md` (read it; decisions list at the bottom is authoritative).

---

## How to work this plan (per phase)

Each PHASE ends with a fixed gate BEFORE the next phase starts:
1. **Test:** `devtools::load_all('.')` then run the phase's testthat file(s) — 0 fail.
2. **Full suite:** run the whole PELSA suite (`devtools::test(filter = "pelsa")`) — 0 fail (no regressions in sibling tests).
3. **Code review:** dispatch a `code-reviewer` subagent on the phase's diff; address CRITICAL/HIGH.
4. **Validation:** the phase's stated VALIDATION check (synthetic ground-truth assertion or render smoke) passes.
5. **ASCII + size:** `grep -nP "[^\x00-\x7F]" <edited files>` empty; helper files <800 lines.
6. **Commit** the phase as one semantic commit (message given per phase).

Synthetic ground truth (the testing backbone) comes from
`tests/testthat/fixtures/pelsa/generate_synthetic.R` via
`source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))` then
`pelsa_make_synthetic(seed = 7, n_extra_peptides = 200)`. Key handles used here:
- `$tie_accession = "TIEPROT"` with two peptides `TIEPEPONEK` (pep_start 3) /
  `TIEPEPTWOK` (pep_start 15), identical `adj.P.Val = 0.0420`, `logFC = 1.2345`.
- `$shared_peptide` "SHAREDPEPTIDEK" -> accessions SHARED1/2/3 (starts 5/11/2).
- `$isoform_accession "P12345-2"` / `$isoform_base_accession "P12345"`.
- `$contrasts = "AY9944_vs_DMSO"` -> stat cols `logFC.<c>` / `adj.P.Val.<c>`.

A reusable test fixture (built once per test file) turns that into a matched
cache + a 3A volcano df. The exact builder is given in Phase 1 Task 1.1 and
re-used by later phases via `source()` of a shared fixtures snippet OR copied
inline (each test file is self-contained — repeat the builder; do not cross-source
test helpers).

---

## PHASE 0 — File split + shared constants (no behavior change)

**Why first:** both `tab_pelsa_section3.R` (~970) and `tab_pelsa_section3_helpers.R`
(~1091) exceed the 800-line ceiling; later phases add to them. Move the existing
interaction helpers into a new file FIRST (pure move, behavior-preserving) so the
new helpers have a home and the helpers file drops under 800.

**Files:**
- Create: `R/tab_pelsa_section3_recolor_helpers.R`
- Modify: `R/tab_pelsa_section3_helpers.R` (remove moved funcs + dup constant)
- Modify: `R/tab_pelsa_woods_helpers.R` (drop duplicate gold constant -> shared)
- Test: existing `tests/testthat/test-pelsa-volcano-ui.R` (must still pass unchanged)

- [ ] **Step 1: Create the new file with the shared constants and moved helpers.**

Create `R/tab_pelsa_section3_recolor_helpers.R` beginning with this header +
constants, then CUT (move verbatim) these functions out of
`tab_pelsa_section3_helpers.R` into it: `pelsa_volcano_resolve_click`,
`pelsa_volcano_sibling_mask`, `pelsa_volcano_pin_opacity` (it will be retired in
Phase 1 but move it now so Phase 0 is a pure move). Keep their roxygen blocks.

```r
################################################################################
# Module: PELSA Section 3 - volcano SELECTION/INTERACTION pure helpers.
#
# The single-selection model's pure logic: resolve a click to a peptide, compute
# the gold recolor arrays for the proxy restyle, the Find-accession match mask,
# and the pinned-panel metadata rows. No Shiny; unit-tested against the seeded
# synthetic generator's closed-form ground truth.
################################################################################

# Shared gold + selection styling (ONE definition; was duplicated as
# .PELSA_VOLCANO_GOLD and .PELSA_WOODS_GOLD).
.PELSA_GOLD          <- "#D4AF37"   # selection fill + coverage-track fill
.PELSA_GOLD_RING_W   <- 2           # same-protein peptide ring width
.PELSA_SEL_DARK_RING <- "#333333"   # the selected peptide's dark outline
.PELSA_SEL_DARK_RING_W <- 1.2
```

- [ ] **Step 2: Repoint the old constant names so nothing else breaks.**

In `tab_pelsa_section3_helpers.R` replace the definition
`.PELSA_VOLCANO_GOLD <- "#D4AF37"` with `.PELSA_VOLCANO_GOLD <- .PELSA_GOLD` (alias,
so any remaining reference still resolves). In `tab_pelsa_woods_helpers.R` replace
`.PELSA_WOODS_GOLD <- "#D4AF37"` with `.PELSA_WOODS_GOLD <- .PELSA_GOLD`.
(R sources all package files, so `.PELSA_GOLD` is visible across files.)

- [ ] **Step 3: Fix the `.PELSA_VOLCANO_BG_ALPHA` double-definition (latent bug).**

In `tab_pelsa_section3_helpers.R` there are TWO defs: `<- 0.8` (~line 32) and
`<- 0.6` (~line 486). DELETE the second (`0.6`) one so the documented `0.8` wins.
Leave `.PELSA_VOLCANO_BG_ALPHA_DIM` / `.PELSA_VOLCANO_FADE_ALPHA` in place for now
(retired in Phase 1 with `pin_opacity`).

ALSO fix export parity: in `.pelsa_export_ggplot` (same file, ~line 1077) the
background `geom_point` hard-codes `alpha = 0.6`. Change that literal to
`alpha = .PELSA_VOLCANO_BG_ALPHA` so the exported PDF cloud matches the (now 0.8)
on-screen cloud. (This is the ONLY export change in the whole plan — the export
data/labels/intensities paths are untouched.)

- [ ] **Step 4: Reload and run the existing volcano-ui tests (pure move = still green).**

Run (in R): `devtools::load_all('.'); devtools::test_active_file("tests/testthat/test-pelsa-volcano-ui.R")`
Expected: PASS, 0 fail (this phase changed NO behavior; `resolve_click`/
`sibling_mask`/`pin_opacity` tests at lines ~297-376 still pass from the new file).

- [ ] **Step 5: ASCII + size check.**

Run: `grep -nP "[^\x00-\x7F]" R/tab_pelsa_section3_recolor_helpers.R R/tab_pelsa_section3_helpers.R R/tab_pelsa_woods_helpers.R` -> empty.
Run: `wc -l R/tab_pelsa_section3_helpers.R` -> should now be < 800.

- [ ] **Step 6: Commit.**

```bash
git add R/tab_pelsa_section3_recolor_helpers.R R/tab_pelsa_section3_helpers.R R/tab_pelsa_woods_helpers.R
git commit -m "refactor(pelsa): extract volcano interaction helpers + unify gold/BG_ALPHA constants

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

**PHASE 0 GATE:** existing suite green (pure move); `tab_pelsa_section3_helpers.R`
< 800 lines; ASCII clean.

---

## PHASE 1 — Recolor helper (the selection -> gold arrays) [TDD]

**Goal:** a pure `pelsa_volcano_recolor` that, given the volcano df + a selection
(+ an optional multi-find mask) + the color mode, returns per-trace color/ring
arrays for the proxy restyle. Plus a deterministic trace-index resolver.

**Files:**
- Modify: `R/tab_pelsa_section3_recolor_helpers.R`
- Modify: `R/tab_pelsa_section3_helpers.R` (retire `pin_opacity` + dim constants + sibling rebuild branch)
- Test: `tests/testthat/test-pelsa-recolor.R` (new)

- [ ] **Step 1: Write the failing test file.**

Create `tests/testthat/test-pelsa-recolor.R`:

```r
library(testthat)

# Minimal volcano-df-shaped frame: 2 proteins, one a marker. Columns the recolor
# reads: id, winning_accession, is_marker, sig_color, feature_color.
.mk_df <- function() {
  data.frame(
    id                = c("PEPA1", "PEPA2", "PEPB1", "PEPMK"),
    winning_accession = c("ACCA", "ACCA", "ACCB", "ACCMK"),
    is_marker         = c(FALSE, FALSE, FALSE, TRUE),
    sig_color         = c("#1f4e9c", "darkred", "gray70", "gray70"),
    feature_color     = c("#111111", "#222222", "#333333", "#444444"),
    stringsAsFactors  = FALSE
  )
}

test_that("recolor: NULL selection + no find -> base fills, no rings", {
  df <- .mk_df()
  out <- pelsa_volcano_recolor(df, selection = NULL, find_mask = NULL,
                               color_mode = "significance")
  split <- pelsa_volcano_marker_split(df)
  # background = non-marker rows (3), markers = 1
  expect_length(out$background$color, nrow(split$background))
  expect_length(out$markers$color,   nrow(split$markers))
  # base = original sig colors; no gold anywhere; rings all transparent / 0
  expect_setequal(out$background$color, c("#1f4e9c", "darkred", "gray70"))
  expect_true(all(out$background$line.width == 0))
  expect_true(all(out$markers$line.width == 0))
})

test_that("recolor: click selection -> gold fill + dark ring on the clicked peptide", {
  df <- .mk_df()
  sel <- list(origin = "click", accession = "ACCA", peptide_seq = "PEPA1")
  out <- pelsa_volcano_recolor(df, sel, NULL, "significance")
  split <- pelsa_volcano_marker_split(df)
  bg_id <- as.character(split$background$id)
  # clicked peptide -> gold fill + dark ring
  expect_equal(out$background$color[bg_id == "PEPA1"], .PELSA_GOLD)
  expect_equal(out$background$line.color[bg_id == "PEPA1"], .PELSA_SEL_DARK_RING)
  # same-accession sibling (PEPA2) -> original fill + GOLD ring
  expect_equal(out$background$color[bg_id == "PEPA2"], "darkred")
  expect_equal(out$background$line.color[bg_id == "PEPA2"], .PELSA_GOLD)
  expect_gt(out$background$line.width[bg_id == "PEPA2"], 0)
  # unrelated point (PEPB1) -> original, no ring
  expect_equal(out$background$color[bg_id == "PEPB1"], "gray70")
  expect_equal(out$background$line.width[bg_id == "PEPB1"], 0)
})

test_that("recolor: a clicked MARKER goes gold in the marker trace (gold wins)", {
  df <- .mk_df()
  sel <- list(origin = "click", accession = "ACCMK", peptide_seq = "PEPMK")
  out <- pelsa_volcano_recolor(df, sel, NULL, "significance")
  # the single marker row -> gold fill
  expect_equal(out$markers$color, .PELSA_GOLD)
})

test_that("recolor: multi-find mask -> uniform gold fill, no dark ring", {
  df <- .mk_df()
  # find matched ACCA's two peptides (mask over df rows)
  mask <- df$winning_accession == "ACCA"
  out <- pelsa_volcano_recolor(df, selection = NULL, find_mask = mask,
                               color_mode = "significance")
  split <- pelsa_volcano_marker_split(df)
  bg_id <- as.character(split$background$id)
  expect_equal(out$background$color[bg_id %in% c("PEPA1", "PEPA2")],
               c(.PELSA_GOLD, .PELSA_GOLD))
  # no dark ring for a find (no single "the one")
  expect_true(all(out$background$line.color[bg_id %in% c("PEPA1","PEPA2")]
                  != .PELSA_SEL_DARK_RING))
})

test_that("recolor: feature color mode uses feature_color as the base", {
  df <- .mk_df()
  out <- pelsa_volcano_recolor(df, NULL, NULL, "feature")
  expect_true("#111111" %in% out$background$color)
})

test_that("trace_index: finds the meta-stamped bg/marker traces", {
  df <- .mk_df()
  p <- pelsa_volcano_build_plot(df, full_df = df, color_mode = "significance",
                                label_mode = "none", source_id = "s")
  idx <- .pelsa_volcano_trace_index(p)
  expect_true(is.numeric(idx$background))
  expect_true(is.numeric(idx$markers))
  expect_false(identical(idx$background, idx$markers))
})
```

- [ ] **Step 2: Run to verify failure.**

Run: `devtools::load_all('.'); devtools::test_active_file("tests/testthat/test-pelsa-recolor.R")`
Expected: FAIL — `could not find function "pelsa_volcano_recolor"` (and `_trace_index`).

- [ ] **Step 3: Implement `pelsa_volcano_recolor` + `.pelsa_volcano_trace_index`.**

Append to `R/tab_pelsa_section3_recolor_helpers.R`:

```r
# Compute the per-trace recolor arrays for the volcano proxy restyle under the
# single-selection model. Returns fills + ring color/width for BOTH restyled
# traces (background == pelsa_volcano_marker_split(df)$background row order,
# markers == $markers row order).
#
# selection: NULL, or list(origin="click"|"find", accession, peptide_seq).
# find_mask: NULL, or a logical over df rows (the MULTI-accession find highlight;
#            uniform gold fill, no dark ring). Ignored when selection is non-NULL
#            of origin "find" with a single accession (that path pins instead).
# color_mode: "significance" | "feature" -> the BASE fill column.
# @return list(background=list(color,line.color,line.width),
#              markers=list(color,line.color,line.width)). @noRd
pelsa_volcano_recolor <- function(df, selection, find_mask = NULL,
                                  color_mode = "significance") {
  split <- pelsa_volcano_marker_split(df)
  mk_one <- function(sub) {
    n <- nrow(sub)
    base <- if (identical(color_mode, "feature")) {
      as.character(sub$feature_color)
    } else {
      as.character(sub$sig_color)
    }
    color <- base
    line.color <- rep("rgba(0,0,0,0)", n)
    line.width <- rep(0, n)
    if (n == 0L) return(list(color = color, line.color = line.color,
                             line.width = line.width))
    ids <- as.character(sub$id)
    wacc <- as.character(sub$winning_accession)

    sel_seq <- if (!is.null(selection)) selection$peptide_seq else NA_character_
    sel_acc <- if (!is.null(selection)) selection$accession   else NA_character_

    # Same-accession peptides (excluding the selected one) -> gold ring.
    if (!is.na(sel_acc) && nzchar(sel_acc)) {
      sib <- !is.na(wacc) & wacc == sel_acc & ids != (sel_seq %||% "")
      line.color[sib] <- .PELSA_GOLD
      line.width[sib] <- .PELSA_GOLD_RING_W
    }
    # The selected peptide -> gold fill + dark ring (wins over magenta marker).
    if (!is.na(sel_seq) && nzchar(sel_seq)) {
      hit <- ids == sel_seq
      color[hit] <- .PELSA_GOLD
      line.color[hit] <- .PELSA_SEL_DARK_RING
      line.width[hit] <- .PELSA_SEL_DARK_RING_W
    }
    # Multi-find highlight -> uniform gold fill (only when no click selection).
    if (!is.null(find_mask) && is.null(selection)) {
      fm_sub <- find_mask[match(ids, as.character(df$id))]
      fm_sub[is.na(fm_sub)] <- FALSE
      color[fm_sub] <- .PELSA_GOLD
    }
    list(color = color, line.color = line.color, line.width = line.width)
  }
  list(background = mk_one(split$background), markers = mk_one(split$markers))
}

# Resolve the background / marker trace JS indices (0-based) of a built volcano
# plotly by the `meta` tag the build stamps (pelsa_volcano_build_plot). Returns
# list(background=<int|NA>, markers=<int|NA>). @noRd
.pelsa_volcano_trace_index <- function(p) {
  metas <- vapply(p$x$data, function(tr) {
    m <- tr$meta
    if (is.null(m) || length(m) != 1L) NA_character_ else as.character(m)
  }, character(1))
  bg <- which(metas == "pelsa_bg")
  mk <- which(metas == "pelsa_mk")
  list(background = if (length(bg)) bg[1L] - 1L else NA_integer_,
       markers    = if (length(mk)) mk[1L] - 1L else NA_integer_)
}
```

- [ ] **Step 4: Stamp `meta` on the bg/marker traces in `pelsa_volcano_build_plot`.**

In `R/tab_pelsa_section3_helpers.R`, in `pelsa_volcano_build_plot`, AFTER the
`p <- .pelsa_strip_hoveron(p)` / `toWebGL` step and BEFORE the annotation/return,
stamp the traces. The background geom is added first, markers later; ggplotly
preserves geom order in `p$x$data`. Add:

```r
  # Tag the background + marker point traces so the recolor proxy restyle can find
  # them by index regardless of how many optional traces (hline, labels) exist.
  # The build adds background (if any) then sibling (none here: sibling_acc NULL)
  # then markers (if any); tag the FIRST geom_point trace as bg and the marker one
  # by its fixed magenta marker color.
  if (length(p$x$data) > 0L) {
    tagged_bg <- FALSE
    for (k in seq_along(p$x$data)) {
      tr <- p$x$data[[k]]
      mc <- tryCatch(tr$marker$color, error = function(e) NULL)
      is_pts <- !is.null(tr$mode) && grepl("markers", tr$mode)
      if (!is_pts) next
      if (!is.null(mc) && length(mc) == 1L &&
          toupper(as.character(mc)) == toupper(.PELSA_VOLCANO_MARKER_COLOR)) {
        p$x$data[[k]]$meta <- "pelsa_mk"
      } else if (!tagged_bg) {
        p$x$data[[k]]$meta <- "pelsa_bg"; tagged_bg <- TRUE
      }
    }
  }
```

(If the marker fill is serialized on `marker.line`/`fillcolor` rather than
`marker.color`, broaden the check to those fields — confirm by inspecting
`plotly_build(p)$x$data` during Step 6 validation and adjust the field read.)

- [ ] **Step 5: Retire `pin_opacity` + dim constants + the sibling rebuild branch.**

In `R/tab_pelsa_section3_recolor_helpers.R` DELETE `pelsa_volcano_pin_opacity`
(moved here in Phase 0). In `R/tab_pelsa_section3_helpers.R` DELETE the constants
`.PELSA_VOLCANO_BG_ALPHA_DIM` and `.PELSA_VOLCANO_FADE_ALPHA`, and in
`pelsa_volcano_build_plot` REMOVE the `sibling_acc`-driven `bg_alpha` branch +
the sibling trace block (always build with the base `bg_alpha`; drop the
`sibling_acc` parameter entirely OR keep it as an ignored deprecated arg — prefer
removing it and updating the two call sites in the module in Phase 4). NOTE: the
module still calls `build_plot(..., sibling_acc = NULL)`; until Phase 4 rewires,
keep the parameter in the signature but unused to avoid a broken intermediate.

- [ ] **Step 6: Run the recolor tests + validate trace tagging.**

Run: `devtools::load_all('.'); devtools::test_active_file("tests/testthat/test-pelsa-recolor.R")`
Expected: PASS (all 6).
VALIDATION (trace tagging on a REAL build): in R,
`p <- pelsa_volcano_build_plot(.mk_df_from_test, label_mode="none"); str(lapply(p$x$data, function(t) list(mode=t$mode, meta=t$meta, mc=t$marker$color)))`
Confirm exactly one trace tagged `pelsa_bg` and one `pelsa_mk`.

- [ ] **Step 7: Remove the now-dead `pin_opacity` tests.**

In `tests/testthat/test-pelsa-volcano-ui.R` DELETE the two `test_that` blocks at
~lines 335 ("pin_opacity: per-point background opacity ...") and ~359
("pin_opacity: opacity vector aligns ..."). They assert the retired dim model.

- [ ] **Step 8: Run the full PELSA suite (no regressions).**

Run: `devtools::test(filter = "pelsa")`
Expected: 0 fail.

- [ ] **Step 9: Code review + commit.**

Dispatch a `code-reviewer` subagent on the diff (`git diff`); address CRITICAL/HIGH.
```bash
git add R/tab_pelsa_section3_recolor_helpers.R R/tab_pelsa_section3_helpers.R tests/testthat/test-pelsa-recolor.R tests/testthat/test-pelsa-volcano-ui.R
git commit -m "feat(pelsa): selection recolor helper + meta-tagged volcano traces; retire dim model

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

**PHASE 1 GATE:** recolor tests pass; trace tagging verified on a real build;
full PELSA suite 0 fail; pin_opacity fully removed (function + constants + tests).

---

## PHASE 2 — Find-accession mask + pinned-metadata rows [TDD]

**Goal:** two pure helpers — `pelsa_volcano_find_mask` (accession typed -> matched
peptide mask + matched accession set + count) and `pelsa_pin_metadata_rows`
(volcano-df row + count -> the 2-column panel table). Tested against the synthetic
generator's ground truth (isoform base, multi-accession, no-match).

**Files:**
- Modify: `R/tab_pelsa_section3_recolor_helpers.R`
- Test: `tests/testthat/test-pelsa-find-metadata.R` (new)

- [ ] **Step 1: Write the failing test file (synthetic ground truth).**

Create `tests/testthat/test-pelsa-find-metadata.R`:

```r
library(testthat)
source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# Build a small volcano-df-shaped frame directly (find_mask/metadata read only
# id / winning_accession / PG.ProteinAccessions / winning_gene / PG.Genes /
# pep_start / pep_end / logFC / adj.P.Val).
.find_df <- function() {
  data.frame(
    id                   = c("PEP1", "PEP2", "PEP3", "ISOPEPTIDEK"),
    winning_accession    = c("P12345", "P12345", "Q99999", "P12345-2"),
    PG.ProteinAccessions = c("P12345", "P12345;EXTRA", "Q99999", "P12345-2"),
    winning_gene         = c("GA", "GA", "GB", ""),
    PG.Genes             = c("GA", "GA", "GB", ""),
    pep_start            = c(7L, 40L, 5L, 7L),
    pep_end              = c(17L, 50L, 15L, 17L),
    logFC                = c(1.1, -0.5, 2.0, 0.3),
    adj.P.Val            = c(0.01, 0.20, 0.001, 0.50),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
}

test_that("find_mask: exact winning_accession match (single accession)", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "Q99999")
  expect_equal(which(out$mask), 3L)
  expect_equal(out$accessions, "Q99999")
  expect_equal(out$count, 1L)
})

test_that("find_mask: case-insensitive + trims", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "  q99999 ")
  expect_equal(out$count, 1L)
})

test_that("find_mask: isoform base P12345 also matches P12345-2", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "P12345")
  # P12345 (rows 1,2) + P12345-2 (row 4, base P12345) all match
  expect_setequal(which(out$mask), c(1L, 2L, 4L))
  expect_equal(out$count, 3L)
})

test_that("find_mask: PG.ProteinAccessions token match (EXTRA)", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "EXTRA")
  expect_equal(which(out$mask), 2L)
})

test_that("find_mask: no match -> empty mask, count 0", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "NOPE")
  expect_equal(out$count, 0L)
  expect_false(any(out$mask))
})

test_that("find_mask: empty/NA input -> count 0", {
  df <- .find_df()
  expect_equal(pelsa_volcano_find_mask(df, "")$count, 0L)
  expect_equal(pelsa_volcano_find_mask(df, NA)$count, 0L)
})

test_that("metadata_rows: 2-col (label,value) df with the panel fields", {
  df <- .find_df()
  rows <- pelsa_pin_metadata_rows(df, row = 1L, n_peptides = 2L)
  expect_s3_class(rows, "data.frame")
  expect_named(rows, c("label", "value"))
  lv <- setNames(rows$value, rows$label)
  expect_equal(lv[["Peptide"]], "GA_aa7")
  expect_equal(lv[["Accession"]], "P12345")
  expect_equal(lv[["Gene"]], "GA")
  expect_equal(lv[["Quantified peptides (this contrast)"]], "2")
  expect_equal(lv[["Sequence"]], "PEP1")
  expect_equal(lv[["Position"]], "7-17")
  expect_match(lv[["adj.P"]], "0.01")
  expect_match(lv[["logFC"]], "1.1")
})

test_that("metadata_rows: empty gene -> accession fallback label, Gene = NA", {
  df <- .find_df()
  rows <- pelsa_pin_metadata_rows(df, row = 4L, n_peptides = 3L)
  lv <- setNames(rows$value, rows$label)
  expect_equal(lv[["Peptide"]], "P12345-2_aa7")  # gene blank -> accession
  expect_equal(lv[["Gene"]], "NA")
})
```

- [ ] **Step 2: Run to verify failure.**

Run: `devtools::test_active_file("tests/testthat/test-pelsa-find-metadata.R")`
Expected: FAIL — functions not found.

- [ ] **Step 3: Implement both helpers.**

Append to `R/tab_pelsa_section3_recolor_helpers.R`:

```r
# Strip a trailing UniProt isoform suffix ("-2") to the base accession. @noRd
.pelsa_iso_base <- function(x) sub("-[0-9]+$", "", as.character(x))

# Match a typed accession against the volcano df. A peptide matches when its
# winning_accession OR any PG.ProteinAccessions token equals the input, OR shares
# its isoform base. Case-insensitive, trimmed.
# @return list(mask=<logical over df rows>, accessions=<distinct matched
#   winning_accession>, count=<# matched rows>). @noRd
pelsa_volcano_find_mask <- function(df, accession) {
  n <- if (is.data.frame(df)) nrow(df) else 0L
  empty <- list(mask = rep(FALSE, n), accessions = character(0), count = 0L)
  if (n == 0L) return(empty)
  q <- toupper(trimws(as.character(accession)[1L] %||% ""))
  if (is.na(q) || !nzchar(q)) return(empty)
  qbase <- .pelsa_iso_base(q)

  wacc <- toupper(as.character(df$winning_accession %||% rep(NA, n)))
  wbase <- .pelsa_iso_base(wacc)
  pg <- toupper(as.character(df$PG.ProteinAccessions %||% rep(NA, n)))

  hit <- (!is.na(wacc) & (wacc == q | wbase == qbase))
  # PG token match: split each row's ;-list and test membership (base-tolerant).
  pg_hit <- vapply(seq_len(n), function(i) {
    if (is.na(pg[i]) || !nzchar(pg[i])) return(FALSE)
    toks <- trimws(strsplit(pg[i], ";", fixed = TRUE)[[1]])
    any(toks == q | .pelsa_iso_base(toks) == qbase)
  }, logical(1))
  mask <- hit | pg_hit
  mask[is.na(mask)] <- FALSE
  accs <- unique(as.character(df$winning_accession)[mask])
  list(mask = mask, accessions = accs[!is.na(accs) & nzchar(accs)],
       count = sum(mask))
}

# Build the pinned-panel metadata as a 2-column (label, value) data.frame from a
# volcano-df row. The Peptide label is the winning-accession label
# "<winning_gene>_aa<pep_start>" (gene->accession fallback when gene is empty).
# n_peptides is the count the caller computed (distinct peptides PLOTTED for this
# accession in the active contrast). @noRd
pelsa_pin_metadata_rows <- function(volcano_df, row, n_peptides) {
  r <- volcano_df[row, , drop = FALSE]
  acc_fb <- if (!is.na(r$winning_accession) && nzchar(r$winning_accession))
    r$winning_accession else as.character(r$PG.ProteinAccessions)[1L]
  gene <- if (!is.na(r$winning_gene) && nzchar(r$winning_gene))
    r$winning_gene else as.character(r$PG.Genes)[1L]
  gene_disp <- if (is.na(gene) || !nzchar(gene)) "NA" else gene
  label_stem <- if (gene_disp == "NA") acc_fb else gene_disp
  pep_label <- paste0(label_stem, "_aa", r$pep_start)
  data.frame(
    label = c("Peptide", "Accession", "Gene",
              "Quantified peptides (this contrast)", "Sequence", "Position",
              "adj.P", "logFC"),
    value = c(pep_label, acc_fb, gene_disp, as.character(as.integer(n_peptides)),
              as.character(r$id),
              paste0(r$pep_start, "-", r$pep_end),
              sprintf("%.2g", r$adj.P.Val), sprintf("%.2g", r$logFC)),
    stringsAsFactors = FALSE
  )
}
```

- [ ] **Step 4: Run to verify pass.**

Run: `devtools::test_active_file("tests/testthat/test-pelsa-find-metadata.R")`
Expected: PASS (all). Note: `metadata_rows` logFC test expects "1.1" — `%.2g`
of 1.1 is "1.1"; adj.P 0.01 -> "0.01". If a format mismatch appears, adjust the
`sprintf` width to match the asserted strings (keep `%.2g`).

- [ ] **Step 5: Full PELSA suite + commit.**

Run: `devtools::test(filter = "pelsa")` -> 0 fail.
Dispatch `code-reviewer` on the diff; address CRITICAL/HIGH.
```bash
git add R/tab_pelsa_section3_recolor_helpers.R tests/testthat/test-pelsa-find-metadata.R
git commit -m "feat(pelsa): find-accession mask + pinned-metadata rows helpers

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

**PHASE 2 GATE:** find/metadata tests pass (incl. isoform-base + PG-token +
no-match + NA-gene ground truth); full suite 0 fail.

---

## PHASE 3 — Tooltip + Woods recolor + feature-overlap tooltip [TDD]

**Goal:** compact 4-line volcano tooltip; Woods track colored by -log10(adj.P)
white->red (clamp Inf), gold outline dropped; feature tooltip lists overlapping
peptides.

**Files:**
- Modify: `R/tab_pelsa_section3_helpers.R` (`tip()` in `pelsa_volcano_build_plot`)
- Modify: `R/tab_pelsa_woods_helpers.R` (`pelsa_woods_track_ggplot`, `pelsa_feature_track_ggplot`, new `pelsa_feature_overlap_peptides`)
- Modify: `tests/testthat/test-pelsa-volcano-ui.R` (tooltip assertion), `tests/testthat/test-pelsa-woods.R`

- [ ] **Step 1: Write/adjust the failing tests.**

In `tests/testthat/test-pelsa-woods.R` append:

```r
test_that("feature_overlap_peptides: lists overlapping peptide aa-labels", {
  # feature [10,20]; peptides at starts 5(end 12), 30(end 40), 15(end 25)
  out <- pelsa_feature_overlap_peptides(
    feat_starts = c(10L), feat_ends = c(20L),
    pep_starts = c(5L, 30L, 15L), pep_ends = c(12L, 40L, 25L))
  expect_equal(out, "aa5;aa15")     # sorted by position, deduped; 30 excluded
})

test_that("feature_overlap_peptides: no overlap -> 'none'", {
  out <- pelsa_feature_overlap_peptides(c(100L), c(110L), c(5L), c(12L))
  expect_equal(out, "none")
})

test_that("woods track: -log10(adj.P) coloring, no gold-outline segment, builds", {
  pep <- data.frame(
    peptide_seq = c("A","B"), pep_start = c(1L,5L), pep_end = c(4L,9L),
    logFC = c(-2, 1.5), adj.P.Val = c(1e-9, 0.4), sig = c(TRUE, FALSE),
    stringsAsFactors = FALSE)
  gg <- pelsa_woods_track_ggplot(pep, prot_len = 20L)
  expect_s3_class(gg, "ggplot")
  # The -log10 column is clamped (1e-9 -> -log10 = 9 -> clamp 5); just assert build.
})
```

In `tests/testthat/test-pelsa-volcano-ui.R` REPLACE the tooltip test (~lines 396-422)
body so it asserts the NEW compact 4-line format. The build-plot tip is internal;
assert via a tiny df and `plotly_build`:

```r
test_that("volcano tooltip is the compact 4-line Peptide/Position/logFC/adj.P", {
  df <- data.frame(
    id = "PEPX", logFC = 1.23, logP = 3, adj.P.Val = 0.004, P.Value = 0.001,
    Significant = TRUE, sig_color = "darkred", feature_color = "#111",
    feature_class_primary = "none", winning_accession = "ACCX",
    winning_gene = "GX", PG.Genes = "GX", PG.ProteinAccessions = "ACCX",
    pep_start = 7L, pep_end = 17L, is_marker = FALSE, label = "GX_aa7",
    stringsAsFactors = FALSE, check.names = FALSE)
  p <- pelsa_volcano_build_plot(df, full_df = df, label_mode = "none",
                                source_id = "s")
  b <- plotly::plotly_build(p)
  txt <- unlist(lapply(b$x$data, function(t) t$text))
  txt <- txt[!is.na(txt) & nzchar(txt)]
  expect_true(any(grepl("Peptide: GX_aa7", txt, fixed = TRUE)))
  expect_true(any(grepl("Position: 7-17", txt, fixed = TRUE)))
  expect_true(any(grepl("logFC: 1.23", txt)))
  expect_true(any(grepl("adj.P: 0.004", txt)))
  # compact: the old Accession:/Gene:/Sequence: lines are NOT in the hover
  expect_false(any(grepl("Accession:", txt, fixed = TRUE)))
})
```

- [ ] **Step 2: Run to verify failure.**

Run: `devtools::test_active_file("tests/testthat/test-pelsa-woods.R"); devtools::test_active_file("tests/testthat/test-pelsa-volcano-ui.R")`
Expected: FAIL (function missing; old tooltip format still emitted).

- [ ] **Step 3: Compact the volcano `tip()`.**

In `R/tab_pelsa_section3_helpers.R`, in `pelsa_volcano_build_plot`'s `tip()`,
REPLACE the body that builds Accession/Gene/Position/logFC/adj.P with the compact
4-line version:

```r
  tip <- function(d) {
    if (nrow(d) == 0L) return(character(0))
    no_span <- is.na(d$pep_start) | is.na(d$pep_end)
    pos <- ifelse(no_span, "unknown", paste0(d$pep_start, "-", d$pep_end))
    gene_fb <- ifelse(is.na(d$winning_gene) | !nzchar(d$winning_gene),
                      d$PG.Genes, d$winning_gene)
    acc_fb <- ifelse(is.na(d$winning_accession) | !nzchar(d$winning_accession),
                     d$PG.ProteinAccessions, d$winning_accession)
    stem <- ifelse(is.na(gene_fb) | !nzchar(gene_fb), acc_fb, gene_fb)
    pep_lab <- paste0(stem, "_aa", d$pep_start)
    lfc_chr  <- ifelse(is.na(d$logFC), "NA", sprintf("%.2f", d$logFC))
    adjp_chr <- ifelse(is.na(d$adj.P.Val), "NA", sprintf("%.2g", d$adj.P.Val))
    paste0("Peptide: ", pep_lab, "<br>",
           "Position: ", pos, "<br>",
           "logFC: ", lfc_chr, "<br>",
           "adj.P: ", adjp_chr)
  }
```

(The tooltip test asserts `logFC: 1.23` — `%.2f` of 1.23 is "1.23". Good.)

- [ ] **Step 4: Implement `pelsa_feature_overlap_peptides`.**

In `R/tab_pelsa_woods_helpers.R` add (mirrors `pelsa_woods_overlap_annotations`
but reversed — features as the query, peptides as the subject):

```r
# For each feature span, the DISTINCT overlapping peptide aa-labels ("aa12;aa45"),
# sorted by position; "none" when a feature overlaps no peptide. data.table
# foverlaps. @noRd
pelsa_feature_overlap_peptides <- function(feat_starts, feat_ends,
                                           pep_starts, pep_ends) {
  nf <- length(feat_starts)
  out <- rep("none", nf)
  if (nf == 0L) return(out)
  if (length(pep_starts) == 0L) return(out)
  fe <- data.table::data.table(
    .fid = seq_len(nf),
    start = suppressWarnings(as.integer(feat_starts)),
    end   = suppressWarnings(as.integer(feat_ends)))
  fe <- fe[!is.na(fe$start) & !is.na(fe$end)]
  if (nrow(fe) == 0L) return(out)
  pep <- data.table::data.table(
    start = suppressWarnings(as.integer(pep_starts)),
    end   = suppressWarnings(as.integer(pep_ends)))
  pep <- pep[!is.na(pep$start) & !is.na(pep$end) & pep$end >= pep$start]
  if (nrow(pep) == 0L) return(out)
  data.table::setkey(pep, start, end)
  ov <- data.table::foverlaps(fe, pep, type = "any", nomatch = NULL)
  if (nrow(ov) == 0L) return(out)
  agg <- tapply(ov$start, ov$.fid, function(s) {
    paste0("aa", sort(unique(as.integer(s))), collapse = ";")
  })
  out[as.integer(names(agg))] <- as.character(agg)
  out
}
```

- [ ] **Step 5: Recolor the Woods track by -log10(adj.P); drop the gold outline.**

In `R/tab_pelsa_woods_helpers.R` add the cap constant near `.PELSA_WOODS_GOLD`:

```r
.PELSA_WOODS_NEGLOG_CAP <- 5  # clamp -log10(adj.P) so tiny p-values don't flatten
```

In `pelsa_woods_track_ggplot`, REMOVE the `sig`-subset gold-outline `geom_segment`
block, and REPLACE the colored segment's `scale_color_gradient2(... logFC ...)`
with a -log10(adj.P) magnitude scale. Add a `neglogp` column (clamped, NA->0):

```r
  pk$y <- pk$logFC
  pk$neglogp <- pmin(-log10(pmax(pk$adj.P.Val, .Machine$double.xmin)),
                     .PELSA_WOODS_NEGLOG_CAP)
  pk$neglogp[is.na(pk$adj.P.Val)] <- 0
  gg <- ggplot2::ggplot(pk, ggplot2::aes(text = .data$.tip)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey70")
  gg +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$pep_start, xend = .data$pep_end,
                   y = .data$y, yend = .data$y, color = .data$neglogp),
      linewidth = 1.8, lineend = "round", alpha = 0.95) +
    ggplot2::scale_color_gradient(low = "grey92", high = "#B2182B",
      limits = c(0, .PELSA_WOODS_NEGLOG_CAP), name = "-log10(adj.P)") +
    ggplot2::scale_x_continuous(limits = c(1, prot_len), expand = c(0, 0)) +
    ggplot2::labs(x = "Residue position", y = "logFC") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
                   panel.border = ggplot2::element_rect(color = "grey60",
                                                        fill = NA, linewidth = 0.4))
```

- [ ] **Step 6: Add the overlapping-peptide line to the feature tooltip.**

In `pelsa_feature_track_ggplot`, change the `.tip` to include an overlap line when
the lanes frame carries a precomputed `.overlap_peps` column (the module attaches
it in Phase 4); fall back gracefully when absent:

```r
  ftype <- if ("feature_type" %in% colnames(f)) f$feature_type else f$feature_class
  ov <- if (".overlap_peps" %in% colnames(f)) f$.overlap_peps else "none"
  f$.tip <- sprintf("%s\n%d-%d\nOverlapping peptides: %s",
                    ftype, f$start, f$end, ov)
```

(Keep the `feature_class` fill + legend as-is.)

- [ ] **Step 7: Run the phase tests.**

Run: `devtools::load_all('.'); devtools::test_active_file("tests/testthat/test-pelsa-woods.R"); devtools::test_active_file("tests/testthat/test-pelsa-volcano-ui.R")`
Expected: PASS.

- [ ] **Step 8: Full suite + code review + commit.**

Run: `devtools::test(filter = "pelsa")` -> 0 fail.
Dispatch `code-reviewer` on the diff; address CRITICAL/HIGH.
```bash
git add R/tab_pelsa_section3_helpers.R R/tab_pelsa_woods_helpers.R tests/testthat/test-pelsa-woods.R tests/testthat/test-pelsa-volcano-ui.R
git commit -m "feat(pelsa): compact volcano tooltip + Woods -log10(adj.P) coloring + feature overlap tooltip

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

**PHASE 2/3 VALIDATION (synthetic ground truth):** in R, build the synthetic
feature/peptide overlap for `TIEPROT` (peptides at 3-12 and 15-24 by construction)
and a feature spanning [3,12]; assert `pelsa_feature_overlap_peptides` returns
"aa3" for that feature and that `pelsa_woods_peptide_data("TIEPROT", ...)`'s two
peptides both have `adj.P.Val == 0.042` (the seeded tie). Document the assertion
in the test file's comments.

**PHASE 3 GATE:** woods + tooltip tests pass; full suite 0 fail; ASCII clean.

---

## PHASE 4 — Module wiring: selection model + Find + Clear + observer

**Goal:** rewire the server to the one-selection model. NO new pure logic — this
phase is Shiny plumbing, verified by the existing UI/integration tests + a manual
render smoke (Shiny reactivity is not unit-tested here; the pure helpers carry the
logic and were tested in Phases 1-3).

**Files:**
- Modify: `R/tab_pelsa_section3.R`

- [ ] **Step 1: Replace `pinned()` with `selection()` and add `find_*` state.**

In `PELSASection3_Ome_Server`, rename the `pinned <- reactiveVal(NULL)` to
`selection <- reactiveVal(NULL)` and add:

```r
    find_query  <- reactiveVal(NULL)   # last submitted Find text (or NULL)
    find_result <- reactiveVal(NULL)   # list(mask, accessions, count) or NULL

    # ONE place to clear the whole transient selection + find highlight.
    clear_selection <- function() {
      selection(NULL); find_query(NULL); find_result(NULL)
      updateTextInput(session, "pelsa_find_acc", value = "")
    }
```

Replace the contrast-switch clear `observeEvent(current_contrast_key(), { pinned(NULL) ...})`
body with `clear_selection()`. Update every later `pinned()` read to `selection()`
(the metadata render, the intensity `pinned_line_data`, the Woods `pinned_woods`).

- [ ] **Step 2: Click sets selection(origin="click").**

Replace the existing `plotly_click` (source `pelsa_volcano`) observer body:

```r
    observeEvent(plotly::event_data("plotly_click", source = ns("pelsa_volcano")), {
      ev <- plotly::event_data("plotly_click", source = ns("pelsa_volcano"))
      res <- tryCatch(pelsa_volcano_resolve_click(ev, active_volcano_df()),
                      error = function(e) NULL)
      if (!is.null(res)) res$origin <- "click"
      find_result(NULL)            # a click replaces any find highlight
      selection(res)
    }, ignoreInit = TRUE)
```

- [ ] **Step 3: Add the Find + Clear controls to the sidebar (sectioned).**

In `output$pelsa_volcano_sidebar`, restructure into Data / Highlight / Display
sections. Insert the Highlight block (Find input + button + notice + Clear) near
the top:

```r
        tags$strong("Find / highlight a protein:"),
        textInput(ns("pelsa_find_acc"), label = NULL,
                  placeholder = "accession e.g. P12345"),
        actionButton(ns("pelsa_find_go"), "Highlight", class = "btn-sm"),
        actionButton(ns("pelsa_clear_sel"), "Clear selection & highlight",
                     class = "btn-sm"),
        uiOutput(ns("pelsa_find_notice")),
        hr(),
```

Also add a static **Color key** block (plotly has no legend):

```r
        hr(),
        tags$strong("Color key"),
        tags$ul(class = "pelsa-color-key",
          tags$li(tags$span(style="color:#FF00FF;","●"), " marker protein"),
          tags$li(tags$span(style=sprintf("color:%s;", .PELSA_GOLD),"●"),
                  " selected peptide (gold), same protein = gold ring"),
          tags$li(tags$span(style="color:darkred;","●"), " significant up"),
          tags$li(tags$span(style="color:#1f4e9c;","●"), " significant down"),
          tags$li(tags$span(style="color:gray;","●"), " not significant")
        )
```

(Use the unicode escape `●` — ASCII source rule; do NOT paste a literal dot.)

- [ ] **Step 4: Find + Clear observers (auto-pin single match).**

```r
    observeEvent(input$pelsa_find_go, {
      df <- tryCatch(active_volcano_df(), error = function(e) NULL)
      if (is.null(df)) return()
      fm <- pelsa_volcano_find_mask(df, input$pelsa_find_acc)
      find_query(input$pelsa_find_acc)
      if (fm$count == 0L) { find_result(fm); selection(NULL); return() }
      if (length(fm$accessions) == 1L) {
        # auto-pin the best (smallest adj.P.Val) peptide of the single accession
        rows <- which(fm$mask)
        best <- rows[which.min(as.numeric(df$adj.P.Val[rows]))]
        selection(list(origin = "find",
                       accession = as.character(df$winning_accession[best]),
                       peptide_seq = as.character(df$id[best]),
                       label = as.character(df$label[best]), row = best))
        find_result(NULL)          # single match pins; not a multi-highlight
      } else {
        selection(NULL)            # multi-match: highlight only
        find_result(fm)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$pelsa_clear_sel, { clear_selection() }, ignoreInit = TRUE)

    output$pelsa_find_notice <- renderUI({
      fr <- find_result(); sel <- selection()
      if (!is.null(sel) && identical(sel$origin, "find")) {
        return(helpText(sprintf("Opened %s below.", sel$accession)))
      }
      if (is.null(fr)) return(NULL)
      if (fr$count == 0L)
        return(helpText(sprintf("No peptides found for '%s'.", find_query())))
      helpText(sprintf("%d proteins / %d peptides highlighted - type one accession to open it.",
                       length(fr$accessions), fr$count))
    })
```

- [ ] **Step 5: ONE composite restyle observer (replaces the pin-opacity + Woods observers).**

DELETE the old `observeEvent(pinned(), { ... pin_opacity ... })` and the inline
Woods `plotly_click` restyle. Add:

```r
    volcano_proxy <- plotly::plotlyProxy("pelsa_volcano_plot", session)

    apply_highlight <- function() {
      df <- tryCatch(active_volcano_df(), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0L) return()
      fr <- find_result()
      rc <- pelsa_volcano_recolor(
        df, selection = selection(),
        find_mask = if (is.null(fr)) NULL else fr$mask,
        color_mode = input$pelsa_color_mode %||% "significance")
      idx <- .pelsa_volcano_trace_index(
        tryCatch(plotly::plotly_build(volcano_built()), error = function(e) NULL))
      # Fall back to bg=0 if the build isn't captured (see Step 6 for volcano_built()).
      bg_i <- idx$background %||% 0L; mk_i <- idx$markers
      plotly::plotlyProxyInvoke(volcano_proxy, "restyle",
        list(`marker.color` = list(rc$background$color),
             `marker.line.color` = list(rc$background$line.color),
             `marker.line.width` = list(rc$background$line.width)), list(bg_i))
      if (!is.na(mk_i)) {
        plotly::plotlyProxyInvoke(volcano_proxy, "restyle",
          list(`marker.color` = list(rc$markers$color),
               `marker.line.color` = list(rc$markers$line.color),
               `marker.line.width` = list(rc$markers$line.width)), list(mk_i))
      }
    }

    # Re-apply on selection / find / color-mode change. color_mode also REBUILDS
    # the base plot (it is a render dep), so defer the restyle to AFTER the flush
    # so it lands on the NEW figure (else it restyles the about-to-be-replaced one).
    observeEvent(list(selection(), find_result(), input$pelsa_color_mode), {
      session$onFlushed(function() apply_highlight(), once = TRUE)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
```

- [ ] **Step 6: Capture the built plot for trace-index resolution.**

The render builds the plot; capture it in a reactiveVal so `apply_highlight` can
read its trace indices without rebuilding. In `output$pelsa_volcano_plot`:

```r
    volcano_built <- reactiveVal(NULL)
    output$pelsa_volcano_plot <- plotly::renderPlotly({
      df <- plot_df()
      validate(need(nrow(df) > 0L, "No peptides to plot for this contrast."))
      p <- pelsa_volcano_build_plot(
        df = df, full_df = df,
        color_mode = input$pelsa_color_mode %||% "significance",
        label_mode = label_mode_for_contrast(), n_top = top_n_for_contrast(),
        source_id = ns("pelsa_volcano"), register_click = TRUE)
      volcano_built(p)
      p
    })
```

(Drop the now-unused `sibling_acc = NULL` arg per Phase 1 Step 5; if the param
still exists as deprecated, pass nothing.)

- [ ] **Step 7: Woods click -> selection(origin="click") (same mechanism).**

Replace the Woods `plotly_click` observer's inline restyle with a `selection()` set
(it then flows through `apply_highlight`):

```r
    observeEvent(plotly::event_data("plotly_click", source = ns("pelsa_woods")), {
      ev <- plotly::event_data("plotly_click", source = ns("pelsa_woods"))
      w  <- tryCatch(pinned_woods(), error = function(e) NULL)  # uses selection()
      if (is.null(ev) || is.null(w) || nrow(w$pep) == 0L) return()
      pep <- w$pep
      in_span <- !is.na(ev$x) & pep$pep_start <= ev$x & ev$x <= pep$pep_end
      cand <- which(in_span); if (!length(cand)) cand <- seq_len(nrow(pep))
      j <- cand[which.min(abs(pep$logFC[cand] - (ev$y %||% pep$logFC[cand])))]
      sel_seq <- pep$peptide_seq[[j]]
      cur <- selection()
      selection(list(origin = "click",
                     accession = if (is.null(cur)) NA_character_ else cur$accession,
                     peptide_seq = sel_seq, label = sel_seq,
                     row = NA_integer_))
    }, ignoreInit = TRUE)
```

(The Woods peptide belongs to the already-pinned protein, so keep `cur$accession`.)

- [ ] **Step 8: Expand the metadata render + compute the reconciled count.**

Replace `output$pelsa_pin_metadata`:

```r
    output$pelsa_pin_metadata <- renderUI({
      sel <- selection()
      if (is.null(sel) || is.null(sel$row) || is.na(sel$row)) {
        # Woods-origin selections may lack a df row; resolve by peptide_seq.
        df <- tryCatch(active_volcano_df(), error = function(e) NULL)
        if (is.null(sel) || is.null(df)) return(helpText("No peptide selected yet."))
        rr <- match(sel$peptide_seq, as.character(df$id))
        if (is.na(rr)) return(helpText("No peptide selected yet."))
        sel$row <- rr
      }
      df <- active_volcano_df()
      w  <- tryCatch(pinned_woods(), error = function(e) NULL)
      n_pep <- if (!is.null(w) && is.data.frame(w$pep))
        length(unique(w$pep$peptide_seq)) else NA_integer_
      rows <- pelsa_pin_metadata_rows(df, sel$row, n_pep)
      tags$table(class = "table table-condensed",
        tags$tbody(lapply(seq_len(nrow(rows)), function(i)
          tags$tr(tags$td(tags$strong(rows$label[i])), tags$td(rows$value[i])))))
    })
```

- [ ] **Step 9: Default label mode None + the marker-count caption.**

The default already follows `.PELSA_VOLCANO_DEFAULT_LABEL_MODE` (set to "none" in
Phase 5 Step 1 — do it now): in `R/tab_pelsa_section3_helpers.R` set
`.PELSA_VOLCANO_DEFAULT_LABEL_MODE <- "none"`. Add a marker-count caption under the
volcano box in the layout `helpText(textOutput(ns("pelsa_marker_count")))` and:

```r
    output$pelsa_marker_count <- renderText({
      df <- tryCatch(active_volcano_df(), error = function(e) NULL)
      if (is.null(df)) return("")
      n <- length(unique(df$winning_accession[df$is_marker %in% TRUE]))
      sprintf("%d marker protein(s) shown in magenta.", n)
    })
```

- [ ] **Step 10: Update the stale Woods helpText.**

In the layout, replace the Woods box `helpText(...)` ("gold outline = significant"
/ "highlight ... in gold") with:

```r
              helpText(paste0("Coverage (gold = residues with peptide evidence); ",
                              "UniProt features (hover for overlapping peptides); ",
                              "Woods plot (y = logFC direction; color = significance ",
                              "magnitude, -log10 adj.P). Click a Woods peptide to ",
                              "select it.")),
```

- [ ] **Step 11: Attach per-feature overlap peptides in `pinned_woods`.**

In `pinned_woods()`, after computing `lanes`, attach the overlap column before the
feature track consumes it:

```r
      if (is.data.frame(lanes) && nrow(lanes) > 0L && nrow(pep) > 0L) {
        lanes$.overlap_peps <- pelsa_feature_overlap_peptides(
          lanes$start, lanes$end, pep$pep_start, pep$pep_end)
      }
```

- [ ] **Step 12: Update the default-label-mode test.**

In `tests/testthat/test-pelsa-volcano-ui.R` line ~202 change
`expect_identical(.PELSA_VOLCANO_DEFAULT_LABEL_MODE, "best_per_marker")` to
`expect_identical(.PELSA_VOLCANO_DEFAULT_LABEL_MODE, "none")`.

- [ ] **Step 13: Reload, run the full suite, manual render smoke.**

Run: `devtools::load_all('.'); devtools::test(filter = "pelsa")` -> 0 fail.
MANUAL render smoke (the human runs `Protigy::launchApp()`):
- Click a point -> it goes gold + dark ring; same-protein peptides gold ring;
  others KEEP their color (no wash-out); panel + Woods open.
- Type a known single accession + Highlight -> panel opens for it (auto-pin).
- Type a gene/accession matching multiple -> gold highlight + "K proteins" notice,
  no panel.
- Toggle color mode while a selection is active -> highlight SURVIVES.
- Clear selection -> all gold gone, Woods card collapses.
- Woods click -> that peptide goes gold on the volcano.
- Hover a volcano point -> 4-line tooltip. Hover a feature -> overlapping peptides.

- [ ] **Step 14: ASCII + size + code review + commit.**

Run: `grep -nP "[^\x00-\x7F]" R/tab_pelsa_section3.R` -> empty.
Run: `wc -l R/tab_pelsa_section3.R` (note if still >800; flag, do not force a
fragile UI split — the helpers split already recovered the helpers file).
Dispatch `code-reviewer` on the full diff; address CRITICAL/HIGH (focus: observer
races, onFlushed correctness, selection NULL-safety).
```bash
git add R/tab_pelsa_section3.R R/tab_pelsa_section3_helpers.R tests/testthat/test-pelsa-volcano-ui.R
git commit -m "feat(pelsa): single-selection model + Find-accession + Clear + composite recolor observer

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

**PHASE 4 GATE:** full suite 0 fail; manual render smoke passes all 7 checks
above; ASCII clean; code review clean.

---

## PHASE 5 — Integration validation + cleanup [synthetic ground truth]

**Goal:** an end-to-end synthetic assertion that the selection/find/metadata
pipeline is COHERENT on the seeded ground truth, plus final cleanup.

**Files:**
- Modify: `tests/testthat/test-pelsa-integration.R` (add a selection-coherence block)

- [ ] **Step 1: Add a selection-coherence integration test.**

In `tests/testthat/test-pelsa-integration.R`, after the existing volcano-df build,
add a block that uses the built volcano df + the synthetic ground truth:

```r
test_that("selection/find/metadata coherent on synthetic ground truth", {
  ib <- .int_build(seed = 7, n_extra = 200)   # existing integration builder
  vdf <- ib$volcano_df                          # the 3A all-peptide df
  # 1. Find the tie protein by accession -> both its peptides match.
  fm <- pelsa_volcano_find_mask(vdf, ib$syn$tie_accession)  # "TIEPROT"
  expect_gte(fm$count, 2L)
  expect_true(ib$syn$tie_accession %in% fm$accessions)
  # 2. Isoform base finds the isoform peptide.
  fmi <- pelsa_volcano_find_mask(vdf, ib$syn$isoform_base_accession) # "P12345"
  expect_gte(fmi$count, 1L)
  # 3. recolor: clicking a TIEPROT peptide golds it + rings its sibling.
  trow <- which(as.character(vdf$winning_accession) == ib$syn$tie_accession)[1]
  sel <- list(origin = "click",
              accession = as.character(vdf$winning_accession[trow]),
              peptide_seq = as.character(vdf$id[trow]))
  rc <- pelsa_volcano_recolor(vdf, sel, NULL, "significance")
  expect_true(.PELSA_GOLD %in% rc$background$color ||
              .PELSA_GOLD %in% rc$markers$color)
  # 4. metadata count reconciles with the woods peptide set for that accession.
  sdf <- pelsa_volcano_stat_df(ib$stat_raw, ib$matched)
  wp <- pelsa_woods_peptide_data(sel$accession, ib$matched, sdf,
                                 ib$syn$contrasts, sig_cutoff = 0.05)
  rows <- pelsa_pin_metadata_rows(vdf, trow, length(unique(wp$peptide_seq)))
  cnt <- as.integer(rows$value[rows$label == "Quantified peptides (this contrast)"])
  expect_equal(cnt, length(unique(wp$peptide_seq)))
})
```

(If `.int_build` does not already expose `$volcano_df` / `$stat_raw` / `$matched` /
`$syn`, extend its return list in the same file to surface them — they are built
internally already; just add them to the returned list.)

- [ ] **Step 2: Run the integration test.**

Run: `devtools::test_active_file("tests/testthat/test-pelsa-integration.R")`
Expected: PASS.

- [ ] **Step 3: Confirm export path unaffected (POI/labels/intensities unchanged).**

The exports were not modified. Confirm the export tests still pass:
Run: `devtools::test(filter = "pelsa")` -> 0 fail (whole suite).

- [ ] **Step 4: Final orphan sweep.**

Run: `grep -rn "pin_opacity\|BG_ALPHA_DIM\|FADE_ALPHA\|PELSA_WOODS_GOLD\|PELSA_VOLCANO_GOLD\b" R/ tests/`
Expected: no live references to retired names (aliases from Phase 0 may remain as
single definitions; if `.PELSA_VOLCANO_GOLD`/`.PELSA_WOODS_GOLD` aliases are now
unused, delete them so only `.PELSA_GOLD` remains). Re-run the suite after deletion.

- [ ] **Step 5: `devtools::document()` (no-op safety) + ASCII + sizes.**

Run: `devtools::document()` (all helpers are `@noRd`; expect NO NAMESPACE diff —
if a diff appears, investigate before committing).
Run: `grep -nP "[^\x00-\x7F]" R/tab_pelsa_section3*.R R/tab_pelsa_woods_helpers.R` -> empty.
Run: `wc -l R/tab_pelsa_section3_recolor_helpers.R R/tab_pelsa_section3_helpers.R R/tab_pelsa_woods_helpers.R`
(helper files < 800).

- [ ] **Step 6: Final full-suite run + code review + commit.**

Run: `devtools::test(filter = "pelsa")` -> 0 fail. (Optionally `devtools::test()`
whole package if time permits.)
Dispatch a final `code-reviewer` subagent on the cumulative diff
(`git diff main...HEAD -- R/ tests/`); address CRITICAL/HIGH.
```bash
git add tests/testthat/test-pelsa-integration.R R/
git commit -m "test(pelsa): selection/find/metadata integration coherence + orphan cleanup

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

**PHASE 5 GATE (final acceptance — ratified criteria):**
1. Selection gold highlight is correct AND survives a color-mode toggle (manual).
2. Single-accession Find opens its panel in one action; multi-match highlights +
   notices (manual).
3. Panel "Quantified peptides (this contrast)" == # peptides drawn in that pin's
   Woods/intensity panel (integration test Step 1 + manual).
4. "Clear selection & highlight" resets everything + collapses the Woods card
   (manual).
5. First-time viewer sees "N marker protein(s) ... magenta" caption (manual).
6. Full PELSA suite 0 fail; ASCII clean; helper files < 800 lines.

---

## Spec coverage map (self-review)

| Spec item | Phase / Task |
|---|---|
| Selection model (one reactiveVal) | P4 S1-S2, S7 |
| Recolor (gold fill + ring, no desaturation) | P1 (helper), P4 S5-S6 (wire) |
| Deterministic trace index (meta stamp) | P1 S3-S4 |
| Composite restyle observer + onFlushed (A3) | P4 S5 |
| V1 default label None + marker caption | P4 S9 |
| V3 compact 4-line tooltip | P3 S3 |
| V4 Find control (single auto-pin / multi highlight) | P2 (mask), P4 S3-S4 |
| P2 metadata expand + reconciled count | P2 (rows), P4 S8 |
| P3 intensity (verify only) | P4 manual smoke S13 |
| W2 feature overlap-peptide tooltip | P3 S4, S6; P4 S11 |
| W3 Woods -log10(adj.P) white->red, drop outline | P3 S5 |
| File split | P0 |
| Gold constant consolidation | P0 S1-S2 |
| BG_ALPHA dup + export parity | P0 S3 (both: delete dup + set `.pelsa_export_ggplot` alpha to `.PELSA_VOLCANO_BG_ALPHA`) |
| Clear button + shared clear | P4 S1, S4 |
| Plot Controls sectioning + Color key | P4 S3 |
| Stale Woods helpText | P4 S10 |
| Retire pin_opacity + dim/fade + sibling branch | P1 S5, S7 |
| Acceptance criteria | P5 GATE |
| Synthetic ground-truth testing | P2/P3 validation, P5 S1 |
