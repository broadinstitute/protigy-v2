# PELSA Volcano - logic-straightening pass (design)

**Date:** 2026-06-15 - **Branch:** feat/pelsa-integration - **Module:** PELSA Section 3 (Volcano + pinned panel)

A behavior-correction pass over the already-built volcano + pinned-panel + Woods
panel, plus a new **Find-accession** control. Reviewed by a 4-lens Opus team
(backend, software, UI/UX, product); their findings and the user's decisions are
folded in below.

**The central model (decision 2026-06-15): ONE selection.** Protein identity
enters the *plot* in exactly one transient way at a time - a `selection` whose
origin is either a **click** (pin a peptide) or a **Find** (type an accession).
A new selection of either kind REPLACES the prior one (click replaces a find;
find replaces a click). There is never a "both active" state, which dissolves the
found-vs-clicked color collision the reviewers flagged. The persistent identity
sets (Setup markers = magenta overlay; POI registry/export) are unchanged and
orthogonal to `selection`.

Key files:
- `R/tab_pelsa_section3.R` - module server (click/find -> selection, restyle, metadata UI, renders).
- `R/tab_pelsa_section3_helpers.R` - volcano build, tooltip, label modes, intensity ggplot.
- `R/tab_pelsa_section3_recolor_helpers.R` - **NEW** file: the interaction/selection pure helpers (recolor, find-mask, metadata-rows) - see "File split" below.
- `R/tab_pelsa_volcano_helpers.R` - `pelsa_build_volcano_df` (df columns: `id`, `logFC`, `logP`, `adj.P.Val`, `P.Value`, `Significant`, `sig_direction`, `sig_color`, `feature_class_primary`, `feature_color`, `winning_accession`, `winning_gene`, `PG.Genes`, `PG.ProteinAccessions`, `pep_start`, `pep_end`, `label`, `is_marker`).
- `R/tab_pelsa_woods_helpers.R` - coverage/feature/Woods builders + tooltip joins.
- `R/tab_pelsa_intensity_helpers.R` - intensity line data (already `show_all`).

---

## The selection model (replaces the old pin/dim model)

A single `selection()` reactiveVal: `NULL`, or
`list(origin = "click"|"find", accession, peptide_seq, label, row)`.
- A **click** resolves (via `pelsa_volcano_resolve_click`) to a peptide -> sets
  `selection(list(origin="click", peptide_seq=<clicked>, accession=<winning_acc>, ...))`.
- A **Find** that resolves to exactly ONE accession auto-pins that accession's
  BEST peptide (smallest adj.P.Val among its peptides in the active df) ->
  `selection(list(origin="find", accession=<acc>, peptide_seq=<best>, ...))`.
  A Find that matches MULTIPLE accessions highlights all matched peptides gold but
  does NOT pin (no single protein to focus); the panel stays on the prior
  selection or empty, and the match notice says "K accessions / N peptides - type
  a single accession to open its panel."
- Setting either replaces the other (one reactiveVal).
- Cleared on contrast switch and by the **Clear selection** button.

`selection()` drives BOTH (a) the gold highlight on the volcano and (b) the fixed
panel (metadata + intensity + Woods). One source of truth.

### Volcano gold highlight (proxy restyle, no rebuild)
On `selection()` change the volcano is recolored via a single
`plotlyProxyInvoke("restyle", ...)` (NOT a rebuild):
- the **selected peptide** (`peptide_seq`) -> GOLD FILL `#D4AF37` + a thin DARK
  ring + slightly larger size (so the one driving the panel stays identifiable);
- **same-accession peptides** (`winning_accession == selection$accession`, minus
  the selected one) -> original fill + GOLD RING (`marker.line.color = gold`,
  `marker.line.width ~ 2`) - a shape channel, CVD-safe;
- for a **multi-accession Find** (no pin): every matched peptide -> GOLD FILL
  (uniform), nothing dark-ringed (no single "the one");
- **every other point** -> ORIGINAL fill (`sig_color`/`feature_color`; magenta for
  non-selected markers), no ring, full opacity (NO desaturation).
- GOLD FILL wins over magenta for the selected peptide; a same-accession marker
  keeps magenta fill + gains the gold ring; non-selected markers stay plain magenta.

The recolor pure helper:
`pelsa_volcano_recolor(df, selection, find_mask = NULL, color_mode)` returns, keyed
to the two restyled traces from `pelsa_volcano_marker_split(df)`:
```
list(
  background = list(color = <chr[nrow(split$background)]>,
                    line.color = <chr...>, line.width = <num...>),
  markers    = list(color = <chr[nrow(split$markers)]>,
                    line.color = <chr...>, line.width = <num...>)
)
```
- `color`: gold for selected/found, else the point's original color (from
  `color_mode`); `line.color`/`line.width`: gold ring for siblings + dark ring for
  the selected peptide, else `"rgba(0,0,0,0)"` / `0`.
- `find_mask` (a logical over df rows, from `pelsa_volcano_find_mask`) is the
  multi-accession highlight set; when `selection$origin == "find"` and single, the
  pin path is used instead (mask == that accession). When `selection` is NULL and
  no find, the helper returns base fills + no rings (full restore).
- Unit-tested: array LENGTHS equal `nrow(split$background)` / `nrow(split$markers)`;
  selected -> gold; sibling -> gold line; others -> original; NULL -> base.

> **CRITICAL wiring notes (from the review - correctness, not polish):**
> - **Deterministic trace identity (BE#2/#3, SW#5):** `pelsa_volcano_build_plot`
>   emits a VARIABLE number of traces (background only if `nrow(bg)>0`, markers
>   only if `nrow(mk)>0`, plus the hline + label annotations), and ggplotly gives
>   them no usable name. STAMP an identifiable tag at build time: set
>   `p$x$data[[k]]$meta <- "pelsa_bg"` / `"pelsa_mk"` on the background/marker
>   traces right after the build, and resolve indices via a small
>   `.pelsa_volcano_trace_index(p)` that scans `meta`. The Woods cross-highlight's
>   hard-coded `list(0L)` is fixed the same way. (Confirmed: ggplotly serializes a
>   `shape=21` point's `fill=` as scattergl `marker.color`, so a `marker.color`
>   restyle DOES recolor the magenta markers.)
> - **Main plot built with `sibling_acc = NULL`** so there are exactly TWO point
>   traces (background == `split$background` row order; markers == `split$markers`).
>   The recolor arrays key to those two. The `sibling_acc != NULL` rebuild branch
>   in `pelsa_volcano_build_plot` is retired (no remaining caller) along with
>   `pelsa_volcano_pin_opacity` and the dim constants.
> - **ONE composite restyle observer (C3):** a single observer reads `selection()`
>   + the find mask, computes `pelsa_volcano_recolor`, and emits ONE restyle per
>   trace covering `marker.color` + `marker.line.color` + `marker.line.width`. No
>   separate Woods-outline observer (the Woods click now just sets `selection()`
>   of origin "click", going through the same recolor - so Woods cross-highlight
>   and pin are the SAME mechanism; the prior inline Woods restyle is removed).
> - **Re-apply after a color-mode rebuild (A3):** the base plot rebuilds when
>   `input$pelsa_color_mode` / label mode change, dropping the proxy restyle.
>   Re-apply via `session$onFlushed(function() <restyle>, once = TRUE)` scheduled
>   from the render (or a `plotly_afterplot` JS hook) so the highlight is
>   re-drawn after the new figure registers client-side. The composite observer
>   also depends on `input$pelsa_color_mode`. (Mechanism decided: `onFlushed`;
>   if it proves flaky in the running app, fall back to folding the highlight into
>   the build on color-mode change only.)

---

## SURFACE 1 - Volcano plot

### V1. Default label mode = "None"
**Now:** `.PELSA_VOLCANO_DEFAULT_LABEL_MODE <- "best_per_marker"`.
**Change:** `<- "none"`. PM#5 flagged the loss of the at-a-glance "which markers"
cue; mitigate with an always-on caption "N marker proteins (magenta)" in the
volcano (cheap, preserves the cue without label clutter). The `none` mode + radio
option already exist.

### V2. Click recolors via the selection model (no desaturation)
Replaces the old `pin_opacity` dim model entirely - see "The selection model"
above. Clicking sets `selection(origin="click")`; the composite observer recolors.

### V3. Floating tooltip - compact identity + effect size (DECIDED: 4 lines)
**Now:** `tip()` shows Accession, Gene, Position, logFC, adj.P (5 lines).
**Change (UI/UX T1 + SW#12 - decide now, not "trim later"):** the persistent
panel (P2) carries the FULL field set, so the hover stays COMPACT:
1. `Peptide: <winning_gene>_aa<pep_start>` (winning-accession label; gene->acc
   fallback when gene empty; NOT the full multilabel).
2. `Position: <start>-<end>`
3. `logFC: <..>`
4. `adj.P: <..>`
Pure change inside `tip()`. (If, in the running app, 4 lines feels too sparse, the
Accession/Gene/Sequence lines are one-line adds - but we ship 4.)

### V4. "Find accession" control (new; in Plot Controls)
A `textInput` + a **"Highlight"** `actionButton` (submit on Enter), placed in a
**Highlight** section near the TOP of Plot Controls (review F3). On submit:
- `pelsa_volcano_find_mask(df, input)` -> `list(mask, accessions, count)`:
  trim + uppercase; a peptide matches when its `winning_accession` OR any
  `PG.ProteinAccessions` token equals the input OR shares its isoform base (strip
  trailing `-\d+`). `accessions` = the distinct matched `winning_accession` set.
- **If exactly ONE accession matches:** auto-pin its BEST peptide ->
  `selection(origin="find", accession=<acc>, peptide_seq=<best>)`. The panel opens.
  Notice: "GENE (P12345): N peptides - panel opened below."
- **If MULTIPLE accessions match:** highlight all matched peptides gold (no pin);
  notice: "K proteins / N peptides highlighted - type one accession to open it."
- **No match:** notice "No peptides found for <input>." Empty input: clear notice
  + clear any find highlight.
- Multi-accession list input (comma/space) is a documented nice-to-have, deferred.
- Find membership uses the SAME widened rule the count reports, so "N highlighted"
  == the gold points (review F2/BE#5). For consistency, pin-sibling membership
  also widens to this rule so click and find agree on "same protein" (BE#5).

---

## SURFACE 2 - Fixed (pinned) panel

### P1. Gold highlight on selection (same mechanism as V2)
Driven by the selection model: the selected peptide is gold-fill + dark-ring, its
same-accession peptides gold-ring, all others untouched.

### P2. Expand the metadata panel
**Now:** 3 rows (Peptide=peptide_seq, Protein=accession, Label) - confusing overlap.
**Change:** rows built by a pure `pelsa_pin_metadata_rows(volcano_df, row, n_peptides)`
returning a **2-column data.frame `(label, value)`** (SW#3) the UI loops into a
`<table>`; order:
- **Peptide:** `<winning_gene>_aa<pep_start>` (gene->acc fallback).
- **Accession:** `winning_accession` (PG fallback).
- **Gene:** `winning_gene` (PG.Genes fallback; "NA" when none).
- **Quantified peptides (this contrast):** `n_peptides` - see the count decision
  below.
- **Sequence:** the selected stripped sequence.
- **Position:** `<pep_start>-<pep_end>`.
- **adj.P:** `%.2g`.
- **logFC:** `%.2f`.

**Peptide-count semantics (DECIDED - reconcile with what's drawn, BE#4/PM#4):**
count = distinct `peptide_seq` actually PLOTTED for this accession in the active
contrast (i.e. the rows `pelsa_woods_peptide_data` / the intensity builder draw),
NOT the raw matched-cache total - so the number matches the Woods/intensity
panels. Label it "Quantified peptides (this contrast)" so it is unambiguous.
Computed in the server (it already has the Woods peptide frame) and passed in.

### P3. Intensity line plot - all peptides, two significance panels (VERIFY ONLY)
Already correct: `pinned_line_data` -> `pelsa_intensity_line_data(..., show_all=TRUE)`;
`pelsa_intensity_line_plot` (the plotly wrapper - distinct from the
`_ggplot` builder, SW#7) renders the vertical Significant/Non-significant subplot;
selected line gold. Verify (a) both panels when both groups non-empty, (b)
single-group renders, (c) single-CONDITION data renders a lone point with a note
(UX#8), (d) the selected line highlight still resolves under the selection model.
No code change unless a discrepancy surfaces.

---

## SURFACE 3 - Woods panel (3 tracks)

### W1. Coverage track - unchanged
Grey backbone + gold covered intervals (`pelsa_coverage_intervals`, IRanges::reduce).

### W2. Feature track - tooltip lists overlapping peptides
**Lane packing:** `pelsa_feature_lanes` (IRanges::disjointBins) already realizes
"earliest-start on top, later-start pushed down" (lane 1 top via `scale_y_reverse`).
No change.
**Tooltip rewrite:** show Feature name (`feature_type` else `feature_class`),
`start-end`, and **Overlapping peptides** `aa<startA>;aa<startB>;...` (de-duplicated,
sorted by position; "none" when no overlap). New pure helper
`pelsa_feature_overlap_peptides(feat_starts, feat_ends, pep_starts, pep_ends)` ->
character per feature (reverse of `pelsa_woods_overlap_annotations`, same
`data.table::foverlaps`). Computed in `pinned_woods()` and attached to the lanes
frame before `pelsa_feature_track_ggplot`.

### W3. Woods track - color by -log10(adj.P) white->red; drop gold outline
**DECISION (user, 2026-06-15):** color segments by `-log10(adj.P.Val)`,
`scale_color_gradient(low = near-white, high = red "#B2182B", name = "-log10(adj.P)")`,
so red = most significant (magnitude). REMOVE the gold significance underline.
- **Inf/NA guard (BE#6):** `adj.P == 0` (or below machine precision) -> `-log10`
  is `Inf`; CLAMP `-log10(adj.P)` to a ceiling constant `.PELSA_WOODS_NEGLOG_CAP`
  (default 5) so the most-significant peptides map to RED, not Inf->grey. NA adj.P
  -> the low (white/grey) end.
- **Direction caveat (BE#6c, UX#3):** this is a SIGNIFICANCE-MAGNITUDE scale, so a
  significant DOWN peptide reads RED here while it is BLUE on the volcano. To
  prevent the cross-track misread, label the legend "-log10(adj.P)" AND add a
  one-line caption under the Woods box: "Color = significance magnitude (not
  direction); logFC sign is on the y-axis." (The y-axis already carries direction.)
- `sig` column stays on the data (intensity panel / exports); only the Woods
  outline is dropped. Tooltip shows RAW adj.P (human-readable), not the -log10.

---

## Cross-cutting / cleanup

### File split (DECIDED: split in this pass)
Both `tab_pelsa_section3.R` (~970) and `tab_pelsa_section3_helpers.R` (~1091)
already exceed 800. Create `R/tab_pelsa_section3_recolor_helpers.R` for the new
interaction cluster: `pelsa_volcano_recolor`, `pelsa_volcano_find_mask`,
`pelsa_pin_metadata_rows`, `.pelsa_volcano_trace_index`, plus the shared gold
constants. Move the EXISTING `pelsa_volcano_resolve_click` / `_sibling_mask` there
too (they are the same interaction concern) to pull the helpers file back under
800. Re-run `devtools::document()` (all `@noRd`, no NAMESPACE change). If the
server file can't reach <800 by extraction alone, note it and keep the split to
helpers (server-side Shiny wiring is harder to extract cleanly) - flag, don't
force a fragile UI split.

### Gold constants (SW#6/UX#10)
Consolidate `.PELSA_VOLCANO_GOLD` + `.PELSA_WOODS_GOLD` (both `#D4AF37`) into ONE
`.PELSA_GOLD` in the recolor-helpers file; add `.PELSA_GOLD_RING_WIDTH` (2) and
`.PELSA_SEL_DARK_RING` (selected peptide's dark outline). Note for the legend:
coverage-interval gold AND selection gold are the same hue - the color key
disambiguates by context (coverage = a track fill; selection = point fill/ring).

### BG_ALPHA duplicate + export parity (BE#9, SW#7)
`.PELSA_VOLCANO_BG_ALPHA` is defined twice (`0.8` then `0.6`; the `0.6` wins, so
the live cloud is currently 0.6). Collapse to ONE `0.8` and update the static
export ggplot (`.pelsa_export_ggplot`, hard-coded `alpha = 0.6`) to the shared
constant so screen and PDF match. (This DARKENS the live cloud slightly - intended.)
Retire the now-orphaned `.PELSA_VOLCANO_BG_ALPHA_DIM` / `.PELSA_VOLCANO_FADE_ALPHA`
with `pin_opacity` and the `sibling_acc` rebuild branch.

### Clear selection button (review I2)
An `actionButton("Clear selection")` in the Highlight section resets `selection()`
(collapsing the Woods card) AND clears the Find box + find highlight, via ONE
shared `clear_selection()` helper (not duplicated). Both Clear and contrast-switch
route through it (SW#8).

### Plot Controls grouping (UX#5)
The box is overcrowded. Section it with headers: **Data** (contrast) /
**Highlight** (Find input + Highlight button + match notice + Clear selection) /
**Display** (color mode, label mode, top-N, best-peptide checkbox). Find/Clear in
the Highlight group; Clear labeled "Clear selection & highlight" so it doesn't read
as Find-only.

### Color key (UX#1 - the biggest visual gap)
plotly renders NO legend (colors are set outside `aes()`). Add a small static
**Color key** block in Plot Controls listing: magenta = marker protein; gold fill =
selected peptide; gold ring = same-protein peptide; red = significant up; blue =
significant down; grey = not significant. (Feature-color mode shows the UniProt
class palette instead of the sig three.)

### Stale Woods helpText (review A4)
The layout helpText still says "gold outline = significant" + "highlight ... in
gold". Rewrite: "Coverage (gold = residues with peptide evidence); UniProt
features (hover for overlapping peptides); Woods plot (y = logFC direction, color
= significance magnitude -log10 adj.P). Click a Woods peptide to select it."

---

## Sequencing (TDD)

1. **Pure helpers + tests first**, in the new recolor-helpers file:
   `pelsa_volcano_recolor` (length + gold/sibling/other/NULL cases; trace-array
   shape), `pelsa_volcano_find_mask` (exact / isoform-base / PG-token / empty /
   present-in-cache-absent-from-df -> count 0), `pelsa_pin_metadata_rows`
   (2-col df, field values, NA-gene -> "NA"), `pelsa_feature_overlap_peptides`,
   `.pelsa_volcano_trace_index`. Tooltip 4-line change via a build-smoke assert.
   Woods -log10 color (smoke: ggplot, no gold-outline segment, Inf clamp). Fix the
   `BG_ALPHA` duplicate + export parity.
2. **Update EXISTING tests that the changes turn red (enumerated, SW#4):**
   - `test-pelsa-volcano-ui.R`: `DEFAULT_LABEL_MODE == "best_per_marker"` assertion
     -> `"none"`; the two `pin_opacity` tests -> delete (model retired) or rewrite
     against `pelsa_volcano_recolor`; the tooltip line-format test -> new 4-line set.
   - Confirm `test-pelsa-woods.R` (smoke + `sig` only) needs no change for W3.
3. **Wire the module:** selection() reactiveVal (replaces pinned()); click + Find +
   Woods-click all set selection(); composite restyle observer (onFlushed re-apply);
   Find control + match notice; Clear button + shared clear; expand metadata;
   per-feature overlap in `pinned_woods`; default label mode none; Plot Controls
   sectioning + Color key + caption; file split + `devtools::document()`.
4. **Verify:** `devtools::load_all`, full PELSA testthat suite 0-fail, ASCII-clean,
   files <800 (helpers; server best-effort). Manual render smoke: click recolor,
   single/multi Find, Clear, color-mode toggle survives highlight, Woods color +
   caption, tooltip, metadata count reconciles with drawn peptides.

## Acceptance criteria (PM-proposed, ratified)
1. With a selection active and a color-mode toggle, the gold highlight is correct
   and SURVIVES the toggle (no stale/lost highlight).
2. Typing a single known accession opens its panel in one action (auto-pin);
   typing a multi-match highlights + tells the user how to open one.
3. The panel's "Quantified peptides (this contrast)" equals the number of peptides
   drawn in that pin's Woods/intensity panels.
4. "Clear selection & highlight" resets selection + Find + collapses the Woods card
   in one click and is discoverable (not "switch contrast to clear").
5. A first-time viewer can tell which proteins are markers (magenta + the always-on
   "N marker proteins" caption) without extra clicks.

## DECISIONS (locked 2026-06-15)
1. **ONE selection** (origin click|find), mutually exclusive - a new selection of
   either kind replaces the prior. No found-vs-clicked color collision.
2. **Highlight:** selected peptide = gold fill + dark ring + slightly larger;
   same-accession peptides = gold ring; others keep original color, full opacity
   (no desaturation). Restyle background + marker traces; trace indices resolved
   via a build-time `meta` stamp (not hard-coded).
3. **Find:** textInput + Highlight button; single-accession match AUTO-PINS its
   best peptide (opens panel); multi-match highlights only (+notice). Matching =
   winning_accession OR PG token OR isoform-base; pin-sibling membership widened to
   match. Cleared on contrast switch + Clear button.
4. **Woods 3rd track:** `-log10(adj.P)` white->red (magnitude), clamp Inf at cap 5,
   NA->low end; DROP gold outline; legend + caption note "magnitude not direction,
   logFC sign on y-axis".
5. **Single-peptide label** (tooltip + panel): winning-accession label only
   (`<winning_gene>_aa<pos>`).
6. **Peptide count:** distinct peptides PLOTTED in the active contrast (reconciles
   with the Woods/intensity panels), labeled "Quantified peptides (this contrast)".
7. **Tooltip:** compact 4 lines (Peptide, Position, logFC, adj.P); full set in the panel.
8. **Composite restyle observer** owns the highlight, depends on `pelsa_color_mode`,
   re-applies after rebuild via `session$onFlushed(once=TRUE)`.
9. **File split** in this pass: new `tab_pelsa_section3_recolor_helpers.R`.
10. **BG_ALPHA** collapsed to 0.8; export alpha unified; dim/fade constants +
    `pin_opacity` + `sibling_acc` rebuild branch retired.
11. **Color key** static block + "N marker proteins" caption added (plotly has no legend).
12. **Default label mode** = None.
