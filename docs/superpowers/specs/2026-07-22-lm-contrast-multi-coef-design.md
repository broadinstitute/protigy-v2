# LM Contrast Builder — Single/Multi Coefficient Redesign

**Date:** 2026-07-22
**Module:** Linear Model setup (`R/tab_lm_setup.R`, `R/tab_lm_setup_helpers_contrasts.R`)
**Branch:** `feat/linear-model`

## Problem

The contrast builder offers two modes: **Simple** (numerator/denominator dropdowns)
and **Advanced** (free-text `limma` contrast expression). Advanced is the only way
to author interaction / difference-of-differences contrasts like `(A - B) - (C - D)`,
but it forces users to **hand-type design-coefficient names** (e.g.
`treatmentDrug:timepointT2`). Copy-pasting coefficient strings is error-prone and
not friendly to non-expert users.

## Goal

Replace free-text authoring with a fully guided, dropdown-only experience:

- **Remove Advanced (free-text) mode entirely.**
- Rename the two radio modes to **`Single coef (1x1)`** and **`Multi coef (2x2)`**.
- **Single coef (1x1):** unchanged behavior — `[Numerator] − [Denominator]`.
- **Multi coef (2x2):** four coefficient dropdowns wired into a fixed
  difference-of-differences template `([A] − [B]) − ([C] − [D])`.

No coefficient name is ever typed; every slot is a dropdown. Impossible to typo a
coefficient name.

## Non-goals / accepted costs

- **Only two contrast shapes are authorable after this change:** `A − B` (Single)
  and `(A − B) − (C − D)` (Multi). Contrasts that Advanced could express but these
  two cannot — 3-way interactions, weighted averages like `A − (B + C)/2`, any
  contrast whose coefficient count is not 2 or 4 — become **unreachable from the
  UI**. This is an accepted, deliberate trade (clean removal of the error-prone
  escape hatch). The `lm.regression()` backend still accepts arbitrary contrast
  strings; only the UI authoring surface is constrained.
- **Multi does not derive coefficients from factor levels.** The four dropdowns
  hold **design-matrix coefficient names** (the same `simple_coefs` set Single
  uses), not raw factor levels. Building a statistically correct interaction still
  requires the user to pick the right four coefficients — the UI removes name typos,
  not contrast-design mistakes. A level→coefficient mapping engine is explicitly
  out of scope.
- **Multi can only combine cells that exist as design coefficients.** A four-cell
  diff-of-differences such as
  `(treatmentA:genotypeA − treatmentA:genotypeCtrl) − (DMSO:genotypeA − DMSO:genotypeCtrl)`
  is the correct treatment×genotype interaction and Multi assembles it verbatim —
  **but only when the design is cell-means coded** (`~ 0 + A:B`, intercept off) so
  all four cells are real columns. Under default reference coding
  (`~ A + B + A:B`, intercept on) the interaction collapses to a single coefficient
  (`treatmentA:genotypeA`) and the reference cells are folded into the intercept, so
  the four literal cell handles do not exist. The dropdowns are populated from
  `design_coefs()`, so the user can only ever pick tokens that actually exist —
  this is a safety feature, not a bug. **Decision (Q9): leave this as-is; no
  helptext hint, no auto-derivation.** Users who want four-cell handles uncheck
  "Include intercept" so the cells appear.

## Architecture

### Unchanged backend (the key simplification)

The assembled Multi expression `(A - B) - (C - D)` flows through the **exact same**
backend path Advanced used:

- `validate_advanced_expr()` (in `tab_lm_setup_helpers_contrasts.R`) — tokenize,
  `make.names()`-normalize, check tokens against design coefs.
- `contrast_specs()` reactive — build `list(id, label, expr, type)` specs.
- `contrast_validation_summary` — live `limma::makeContrasts()` against the design.
- `lm.regression(..., contrasts_list=)` — the fit.

None of these change. Multi is a **new authoring surface for the same backend**.
This is why the deep numeric-recovery tests in `test-lm-advanced-contrast.R` remain
valid: they test `lm.regression()` with contrast strings directly, never the UI.

### Row state model

Current row shape:
```r
list(id, type, num, den, advanced_expr, label, label_user_edited)
# type ∈ {"simple", "advanced"}
```

New row shape:
```r
list(id, type, num, den, num2, den2, label, label_user_edited)
# type ∈ {"simple", "multi"}
```

- `advanced_expr` **removed**.
- `num2`, `den2` **added** — slot C/D for Multi. (`num`/`den` = slot A/B, shared
  with Single.)
- For a Single row, `num2`/`den2` are unused (empty).

### New pure helpers (in `tab_lm_setup_helpers_contrasts.R`)

```r
# Assemble the fixed 2x2 difference-of-differences expression.
# Returns "" if any of the four slots is empty.
build_multi_expr(a, b, c, d)
#   -> "(a - b) - (c - d)"

# Auto-label for a Multi row: per-pair shared-prefix strip, nested parens.
# Reuses strip_shared_prefix() on (a,b) and (c,d) independently.
make_multi_label(a, b, c, d)
#   e.g. ("SubgroupGR4","SubgroupGR3","treatmentDrug","treatmentVehicle")
#     -> "(GR4-GR3)-(Drug-Vehicle)"
#   Returns "" if any slot empty. Whitespace-free (CSV/TSV safe).
```

The parenthesized label form is kept verbatim — it matches the `( )` / `-`
convention already documented in the helpers file header, is whitespace-free, and
is user-overridable via the label field.

## UI changes (`tab_lm_setup.R`, `contrast_rows_ui` render block ~L822–990)

### Radio labels

```
Single coef (1x1)   Multi coef (2x2)
```
(values `"simple"` / `"multi"`).

### Single panel — unchanged

Numerator / `−` / Denominator dropdowns + swap button, using `simple_coefs`
(design coefs minus `(Intercept)`).

### Multi panel — new

Four dropdowns laid out as the template, all using the **same `simple_coefs`**
choice set (Intercept excluded):

```
( [ A ▼] − [ B ▼] )  −  ( [ C ▼] − [ D ▼] )
```

- Grid layout mirroring the existing Single grid style.
- Validation: red/invalid until **all four** are chosen; "identical within a pair"
  and unknown-coef checks reuse the existing validator on the assembled `expr`.

### Direction sentence — REMOVED (both modes)

The italic "Positive log2FC = higher in X than in Y" sentence is removed from
**Single and Multi**. Consequently:

- `direction_sentence_simple()` helper deleted.
- `.direction-sentence` CSS block deleted.
- `dir_sent` computation + render deleted from the card.
- `direction_sentence_simple` tests deleted.

### Auto-label

- Single: `make_simple_label(num, den)` (unchanged) → e.g. `4-3`.
- Multi: `make_multi_label(num, den, num2, den2)` → e.g. `(GR4-GR3)-(Drug-Vehicle)`.

### Mode toggle behavior

Flipping a card's mode (Single↔Multi) **clears all four slots**
(`num`/`den`/`num2`/`den2` → empty). No stale values carried across shapes. The
card returns to invalid/"choose …" until the new mode's slots are filled. The
persist observer must distinguish an actual `type` change (→ clear slots) from a
normal re-render (→ leave slots).

### Buttons

- **Single `+ Add contrast`** button — adds a Single row. (Replaces both
  `+ Add Simple` and `+ Add Advanced`.)
- **`Clear all`** — kept as-is.
- **`+ Suggest all pairwise`** — **removed** (button + `bsTooltip`).

### Helptext

Rewritten to describe Single (1×1) vs Multi (2×2); the free-text `(A - B) - (C - D)`
"Advanced" framing is removed. New text explains that Multi builds a
difference-of-differences (interaction) contrast from four coefficient dropdowns.

## Dead code to remove

| Item | Location |
|---|---|
| Advanced radio option, advanced render panel, `textAreaInput` | `tab_lm_setup.R` render block |
| `add_contrast_advanced` observer | `tab_lm_setup.R` ~L1125 |
| Advanced branch in persist observer | `tab_lm_setup.R` ~L1018 |
| Advanced branch in `contrast_specs` | `tab_lm_setup.R` ~L1192 |
| `suggest_pairwise_contrasts` observer + tooltip | `tab_lm_setup.R` ~L1063–1121, L782 |
| `enumerate_pairwise_simple_rows()` helper | `tab_lm_setup_helpers_contrasts.R` L186 |
| `test-lm-pairwise-enumerator.R` | tests |
| `direction_sentence_simple()` + `.direction-sentence` CSS | helpers + render |
| direction-sentence tests | test-lm-setup-helpers-contrasts.R |

`advanced_expr` field is dropped from the row model everywhere it's read/written
(seed row, add observers, clear observer, persist observer, `contrast_specs`).

## What stays (backend + its tests)

- `validate_advanced_expr()` — Multi feeds it. **Kept.** (Name is now a slight
  misnomer; keeping it avoids churning its 6 unit tests. Optional rename deferred.)
- `contrast_specs()`, `contrast_validation_summary`, `makeContrasts` path — kept.
- `test-lm-advanced-contrast.R` — tests `lm.regression()` contrast path directly,
  not the UI. **Every assertion kept.** Its composite test
  (`(treatmentDrug + treatmentDrug:timepointT2) - treatmentDrug`) already proves the
  grouped `(...) - ...` arithmetic Multi produces works end-to-end. Optional file
  rename to `test-lm-contrast-backend.R` deferred (keep content).
- `strip_shared_prefix`, `make_simple_label`, `build_simple_expr`,
  `sanitize_label`, `new_contrast_row_id` — kept (Multi reuses `strip_shared_prefix`).

## Testing plan (TDD)

New tests (pure functions — analytic oracles, no app):

1. `build_multi_expr(a,b,c,d)`:
   - four non-empty slots → `"(a - b) - (c - d)"`.
   - any empty slot → `""`.
2. `make_multi_label(a,b,c,d)`:
   - per-pair prefix strip → `"(GR4-GR3)-(Drug-Vehicle)"`.
   - no shared prefix in a pair → pair kept verbatim.
   - any empty slot → `""`.
   - whitespace-free output.

One new end-to-end seam test (proves authoring layer connects to backend):

3. Feed `build_multi_expr()` output for the 2×2 factorial through
   `lm.regression(..., contrasts_list=)` and assert it recovers the planted
   interaction (reuse the `make_2x2_dod_gct` fixture / assertions already in
   `test-lm-advanced-contrast.R`). This is a thin seam check, NOT a re-derivation
   of the deep numeric oracles that file already owns.

Deleted tests: `test-lm-pairwise-enumerator.R`; `direction_sentence_simple` tests.

Contract tests: verified there is **no** `expect_named()` asserting `advanced_expr`
as a live contract outside the deleted files. The only row-field-shape assertion
lives in `test-lm-pairwise-enumerator.R` (which includes `advanced_expr` in the
draft shape) — and that whole file is being deleted, so no field-set contract test
needs updating. `validate_advanced_expr` tests in
`test-lm-setup-helpers-contrasts.R` pass strings, not row lists, and are unaffected.
Re-grep `tests/testthat/` for `advanced_expr` after implementation to confirm zero
stragglers remain.

## Verification

- `devtools::load_all(".")` then `devtools::document()` (no roxygen exports change
  expected, but run to be safe).
- `devtools::test()` — full suite green.
- Manual browser check: add a Multi card, pick 4 coefs, confirm expr preview shows
  `(A - B) - (C - D)`, label shows nested parens, validation goes green, Run works.
