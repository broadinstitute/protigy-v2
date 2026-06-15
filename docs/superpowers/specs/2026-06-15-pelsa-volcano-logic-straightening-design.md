# PELSA Volcano - logic-straightening pass (design)

**Date:** 2026-06-15 - **Branch:** feat/pelsa-integration - **Module:** PELSA Section 3 (Volcano + pinned panel)

A behavior-correction pass over the already-built volcano + pinned-panel + Woods
panel. Nothing is net-new architecture; each item below straightens the *logic*
of an existing feature to match the agreed interaction model. Grouped by the
three surfaces: the **volcano**, the **fixed (pinned) panel**, and the **Woods
panel**.

Key files:
- `R/tab_pelsa_section3.R` - module server (click -> pin, proxy restyle, metadata UI, renders).
- `R/tab_pelsa_section3_helpers.R` - volcano build, tooltip, pin-opacity/recolor, intensity ggplot.
- `R/tab_pelsa_volcano_helpers.R` - `pelsa_build_volcano_df` (df columns: `id`, `logFC`, `logP`, `adj.P.Val`, `P.Value`, `Significant`, `sig_direction`, `sig_color`, `feature_class_primary`, `feature_color`, `winning_accession`, `winning_gene`, `PG.Genes`, `PG.ProteinAccessions`, `pep_start`, `pep_end`, `label`, `is_marker`).
- `R/tab_pelsa_woods_helpers.R` - coverage/feature/Woods builders + tooltip joins.
- `R/tab_pelsa_intensity_helpers.R` - intensity line data (already `show_all`).

---

## SURFACE 1 - Volcano plot

### V1. Default label mode = "None"
**Now:** `.PELSA_VOLCANO_DEFAULT_LABEL_MODE <- "best_per_marker"`.
**Change:** `<- "none"`. The `none` mode already exists in
`pelsa_volcano_label_rows` (returns `integer(0)`) and the radio control already
lists "None". One-constant change; the per-contrast label-mode registry default
follows it. No labels drawn until the user opts in.

### V2. Point coloring is correct; the CLICK must NOT desaturate others
**Now (correct):** marker peptides magenta (`#FF00FF`, `shape=21`, on top), sig
up = `sig_color` darkred, sig down = blue `#1f4e9c`, ns = grey - via
`pelsa_volcano_color_column` (significance mode) or `feature_color` (feature
mode). Background cloud at `bg_alpha`.
**Now (WRONG per spec):** on pin, `pelsa_volcano_pin_opacity` dims every
non-sibling background point to `0.12` (the "wash to faint" the user rejects) and
lifts siblings to opacity 1. That is a *desaturation/dim* model.
**Change - replace the dim model with a RECOLOR model (decision: recolor via
proxy restyle, gold wins even for markers):**

> **CRITICAL wiring notes (from UI/UX review - these are correctness, not polish):**
> - **Dynamic trace indices (C1):** `pelsa_volcano_build_plot` emits a VARIABLE
>   number of traces - background only if `nrow(bg)>0`, markers only if
>   `nrow(mk)>0`, plus the threshold hline + label annotations. Do NOT hard-code
>   trace 0 = background / trace 1 = markers. Discover the indices at build time
>   by inspecting `p$x$data` trace names (the background/marker geoms carry
>   distinguishable legend/trace metadata) and thread them to the observer (e.g.
>   store on the plot or recompute via a small `.pelsa_volcano_trace_index(p)`
>   helper). The Woods cross-highlight currently hard-codes `list(0L)` - fix it
>   the same way.
> - **ONE composite restyle owner (C3):** pin-recolor, Find-highlight, AND the
>   Woods->volcano outline ALL restyle the same volcano. Route them through a
>   SINGLE observer that reads `pinned()`, `find_accession()`, and
>   `woods_selected()` (a new reactiveVal replacing the inline Woods restyle) and
>   emits ONE restyle covering `marker.color` + `marker.line.color` +
>   `marker.line.width` for every relevant trace. Independent observers race and
>   leave stale highlights.
> - **Re-apply after rebuild (A3):** the base plot REBUILDS when
>   `input$pelsa_color_mode` (or label mode) changes, which drops any proxy
>   restyle. The composite restyle observer MUST also depend on
>   `input$pelsa_color_mode` (and re-fire after the render) so an active
>   pin/find highlight survives a color-mode toggle.

- New pure helper `pelsa_volcano_pin_recolor(df, accession, clicked_seq,
  find_accession = NULL, color_mode = "significance")` returns per-point **color**
  arrays for BOTH restyled traces (background + markers), each aligned to that
  trace's point order from `pelsa_volcano_marker_split` (background ==
  `split$background` row order, markers == `split$markers` row order). MUST be
  unit-tested for `length(bg_colors) == nrow(split$background)` and
  `length(mk_colors) == nrow(split$markers)` (C2):
  - the **clicked peptide** (`id == clicked_seq`) -> GOLD FILL `#D4AF37`
    (`marker.color`);
  - **same-accession siblings** (`winning_accession == accession`, excluding the
    clicked one) -> ORIGINAL fill + a GOLD RING (`marker.line.color = #D4AF37`,
    `marker.line.width ~ 2`). DECISION: gold ring, not a second yellow fill -
    shape/stroke is a second channel (CVD-safe, readable at small size, no
    collision with the grey ns cloud). So siblings keep their sig/feature fill and
    gain an outline.
  - **every other point** -> its ORIGINAL color (`sig_color`/`feature_color` for
    the background; magenta for non-selected markers), NO ring. No opacity change -
    all points stay fully visible.
  - Gold FILL wins over magenta: a clicked marker is recolored gold in the marker
    trace too (decision: gold wins). A sibling marker keeps magenta fill + gains
    the gold ring. Non-selected markers stay plain magenta.
- The helper returns, per restyled trace, BOTH a `marker.color` array (fills:
  gold for clicked/found, else original) AND `marker.line.color` /
  `marker.line.width` arrays (gold ring for siblings + the Woods-clicked peptide,
  else transparent / 0). The composite observer emits these in ONE restyle per
  trace. Color-mode-aware (the "original" fills come from the active color mode).
  On unpin / clear / contrast-switch the helper returns base fills + no rings
  (full restore).
- **Retire** `pelsa_volcano_pin_opacity` from the wired path (keep the function +
  its test, marked KEPT-BUT-UNWIRED like `pelsa_volcano_thin_note`, OR delete if
  no caller remains - decide at implementation; prefer delete if truly orphaned).
- The Woods->volcano cross-highlight (gold marker.line outline) is unaffected
  *mechanically* but now coexists with a gold FILL; keep it (outline still marks
  the Woods-clicked peptide). It already restyles `marker.line.*` on trace 0 only;
  extend to trace 1 for consistency if the clicked Woods peptide is a marker.

### V3. Floating tooltip - compact identity + effect size
**Now:** `tip()` shows Accession, Gene, Position, logFC, adj.P (5 lines).
The user's spec lists 7 fields for the hover tooltip, but the UI/UX review (T1)
flags a 7-line transient hover over a dense plot as too much to read. **Decision:
keep the hover tooltip COMPACT (the full field set lives in the persistent
pinned panel, P2).** Floating tooltip lines (order):
1. `Peptide: <winning_gene>_aa<pep_start>` (gene->accession fallback when gene
   empty; winning-accession label only - derived from `winning_gene`/
   `winning_accession` + `pep_start`, NOT the full multilabel `label`).
2. `Accession: <winning_accession or PG fallback>`
3. `Gene: <winning_gene or PG.Genes fallback; "NA" when none>`
4. `Sequence: <id>` (the peptide stripped sequence)
5. `Position: <start>-<end>`
6. `logFC: <..>`
7. `adj.P: <..>`
The user explicitly enumerated these 7 fields for the tooltip, so we honor the
full set here; if it reads too long in practice we trim to (Peptide, Position,
logFC, adj.P) and rely on the panel for the rest. Pure change inside `tip()` in
`pelsa_volcano_build_plot`. "NA gene": when both `winning_gene` and `PG.Genes`
are empty/NA, render the literal `NA`.

### V4. "Find accession" highlight control (new, in the Plot Controls panel)
**New:** a Find module in `pelsa_volcano_sidebar`: a `textInput` where the user
types an accession; on submit (an adjacent "Highlight" `actionButton`, or
debounced text), ALL peptides whose `winning_accession == <entered>` are colored
GOLD on the volcano. A small `helpText`/notice reports the count
("N peptides highlighted" / "No peptides found for <acc>").
**Mechanism (reuses V2's recolor, ONE restyle path):** the recolor helper is
generalized to take an optional `find_accession` in addition to the pinned
`(accession, clicked_seq)`:
- `pelsa_volcano_pin_recolor(df, accession, clicked_seq, find_accession = NULL,
  color_mode)` returns the per-point fill + ring arrays where:
  - pinned clicked peptide -> GOLD FILL; pinned siblings -> original fill + GOLD
    RING;
  - peptides of `find_accession` -> GOLD FILL (uniform; no clicked/sibling split -
    a Find has no clicked peptide);
  - PRECEDENCE when a peptide is both pinned and found: a clicked peptide is
    already gold fill (wins); a pinned SIBLING that is ALSO in `find_accession`
    is promoted from ring-only to GOLD FILL (Find wants the whole accession solid
    gold). Everything else original fill, no ring.
- The Find accession lives in a `reactiveVal` (`find_accession()`), set by the
  button/text observer and CLEARED on contrast switch (like `pinned()`); an empty
  input clears it. The pin observer and the Find observer BOTH call the same
  restyle, reading both `pinned()` and `find_accession()` so the two highlights
  compose in one message (no fighting restyles).
- The Find highlight is independent of pinning: a user can Find without clicking.
  It does NOT open the fixed panel / Woods (those stay click-pin driven) - Find is
  purely a volcano-highlight aid. **Discoverability (review I3):** the match
  notice teaches the model, e.g. "12 peptides highlighted - click one to pin its
  protein view."
- **Control shape (review F1):** a `textInput` + a "Highlight" `actionButton`
  (submit on Enter), NOT debounce-only - a debounced field fires partial-match
  restyles mid-typing on a ~100k-point figure. The Clear button (see cross-cutting)
  also clears this box.
- **Matching (review F2 - make definite, not "optional"):** trim + uppercase the
  input; match a peptide when its `winning_accession` OR any `PG.ProteinAccessions`
  token equals the input OR shares its isoform base (strip a trailing `-\d+`).
  The reported count is the union (so "N highlighted" == the points actually gold).
  Multi-accession input (comma/space list) is a documented nice-to-have, deferred.
- **Placement (review F3):** near the TOP of Plot Controls (it is an action, not a
  display toggle), grouped with an `hr()`.
- New pure helper `pelsa_volcano_find_mask(df, accession)` -> list(mask, count),
  unit-tested: exact match, isoform-base match, PG-token match, empty/NA input,
  accession present in cache but absent from the active df -> count 0 (review A2).

---

## SURFACE 2 - Fixed (pinned) panel

### P1. Gold highlight on click (same mechanism as V2)
Driven by V2's recolor restyle - the pinned peptide is GOLD FILL on the volcano,
its same-accession peptides get a GOLD RING (original fill kept), all others
untouched. No separate work beyond V2; the composite observer fires on `pinned()`.

### P2. Expand the metadata panel
**Now:** `pelsa_pin_metadata` shows 3 rows (Peptide=peptide_seq, Protein=accession,
Label). The "Peptide" row currently shows the raw sequence and "Label" the
multilabel - confusing overlap.
**Change:** show these rows (mirrors the floating tooltip, plus the count), in
order, built from the pinned volcano-df row (looked up by `pin$row`) + a matched-
cache count:
- **Peptide:** `<winning_gene>_aa<pep_start>` (winning-accession label; gene->acc
  fallback).
- **Accession:** `winning_accession` (PG fallback).
- **Gene:** `winning_gene` (PG.Genes fallback; "NA" when none).
- **# peptides in protein:** count of DISTINCT `PEP.StrippedSequence` in the
  matched cache whose `accession == pin$accession` (decision: distinct sequences).
- **Sequence:** `pin$peptide_seq` (the stripped sequence).
- **Position:** `<pep_start>-<pep_end>`.
- **adj.P:** formatted `%.2g`.
- **logFC:** formatted `%.2f`.
New pure helper `pelsa_pin_metadata_rows(volcano_df, row, n_peptides)` -> a small
named-list/data.frame of (label, value) pairs, so the UI render is a thin table
loop and the field logic is unit-tested. `n_peptides` is computed in the server
(needs the matched cache) and passed in.

### P3. Intensity line plot - all peptides, two significance panels
**Now (correct):** `pinned_line_data` calls `pelsa_intensity_line_data(..., show_all
= TRUE)` -> every peptide of the pinned accession; `pelsa_intensity_line_plot`
renders a vertical `plotly::subplot` of "Significant" (top) / "Non-significant"
(bottom) single-panel ggplots; each line labeled `aa<pos>`; pinned line gold.
**Change:** NONE expected - this already matches the spec. Verify at
implementation that (a) both panels appear when both groups are non-empty, (b) a
single-group protein still renders, (c) each line's end label is `aa<pos>`.
If a discrepancy is found, fix minimally; otherwise no code change.

---

## SURFACE 3 - Woods panel (3 tracks)

### W1. Coverage track - unchanged
Grey backbone + gold covered intervals via `pelsa_coverage_intervals`
(IRanges::reduce). No change.

### W2. Feature track - overlap resolution + tooltip rewrite
**Now (lane packing):** `pelsa_feature_lanes` uses `IRanges::disjointBins`
(greedy min-lane first-fit). This already realizes "sort overlapping by start,
push later-start features to a lower sub-track": disjointBins assigns lane 1 to
the earliest-start feature in an overlap cluster and bumps each subsequent
overlapping feature to the next free lane. Confirm at implementation that the
first lane renders on TOP (the track uses `scale_y_reverse`, so lane 1 is top -
correct). No code change to packing.
**Now (tooltip - WRONG per spec):** feature tooltip shows
`feature_type / feature_class / start-end` - it does NOT list the overlapping
peptides.
**Change:** rewrite the feature tooltip to:
- Feature name (`feature_type` when present else `feature_class`).
- `start-end`.
- **Overlapping peptides:** `aa<startA>;aa<startB>;...` - the peptides (by
  `pep_start`) that overlap this feature's `[start,end]`. No gene name (single
  protein). De-duplicate + sort by position; "none" when no peptide overlaps.
- New pure helper `pelsa_feature_overlap_peptides(feat_starts, feat_ends, pep_starts,
  pep_ends)` -> character per feature (the REVERSE of
  `pelsa_woods_overlap_annotations`; same `data.table::foverlaps`, peptide labels
  `aa<pep_start>`). The feature-track builder takes the peptide spans (or the
  precomputed per-feature string) so the `.tip` includes the overlap list.
  Wiring: `pinned_woods()` already has `pep` + `feats`; compute the per-feature
  overlap string there and attach to `lanes` before `pelsa_feature_track_ggplot`.

### W3. Woods track - color by adj.P [0,1], drop gold sig outline
**Now:** `pelsa_woods_track_ggplot` colors segments by `logFC`
(`scale_color_gradient2` blue/grey/red, midpoint 0) AND draws a thick gold
underline for `sig` peptides.
**Change (DECISION 2026-06-15: color by -log10(adj.P), white->red; drop gold
outline):**
- Color the segment by `-log10(adj.P.Val)` on a `scale_color_gradient(low =
  "white"/very-light-grey, high = red "#B2182B", name = "-log10(adj.P)")`, so
  RED = significant - MATCHING the volcano's red=significant convention and
  spreading the significant range (vs the original raw-[0,1] blue->red, which
  reads backwards and crowds all significant peptides into one color).
- Optionally clamp `-log10(adj.P)` at a ceiling (e.g. 5) so a handful of tiny
  p-values don't flatten the rest; document the clamp in the legend if used.
- NA adj.P -> -log10 undefined: map to the low (non-significant) end / grey.
- REMOVE the gold significance underline segment + the `sig`-subset draw.
- `sig` column stays on the data (still used by the intensity panel / exports);
  only the Woods *outline* is dropped.
- Tooltip keeps seq / span / logFC / adj.P / annotations (shows the RAW adj.P,
  not the -log10, so the number is human-readable).

---

## Cross-cutting: a latent bug to fix while here

`.PELSA_VOLCANO_BG_ALPHA` is **defined twice** in `tab_pelsa_section3_helpers.R`
(line ~32 `<- 0.8`, line ~486 `<- 0.6`). The second wins, so the documented "fairly
opaque 0.8" background is actually 0.6. Collapse to ONE definition (keep 0.8, the
intended value per the A4 comment) and delete the duplicate. **Also update the
static export ggplot** (`.pelsa_export_ggplot`, hard-coded `alpha = 0.6`) to the
same shared constant so the on-screen cloud and the exported PDF match (review C4).
Verify the dim constant `.PELSA_VOLCANO_BG_ALPHA_DIM` is only needed if
`pin_opacity` survives; if `pin_opacity` is retired, the dim constant may become
orphaned (remove if so).

### Clear / unpin affordance (review I2)
Add an explicit **"Clear selection"** `actionButton` in Plot Controls that resets
`pinned()` (collapsing the bottom Woods card) and clears the Find box +
`find_accession()`. Today the only way to drop a pin is a contrast switch.
Specify the gesture clearly in helpText.

### Color-mode switch must re-apply the highlight (review A3)
See the CRITICAL wiring note under V2 - the composite restyle observer depends on
`input$pelsa_color_mode` so a pin/find highlight is re-drawn after the
color-mode-driven rebuild instead of silently vanishing.

### Update the stale Woods helpText (review A4)
The layout helpText under the Woods box still says "gold outline = significant".
W3 drops that outline; rewrite the helpText to describe the adj.P color encoding
(including the blue=significant caveat, pending the W3 color decision below).

---

## Sequencing

1. **Pure helpers + their tests first (TDD):**
   `pelsa_volcano_pin_recolor` (incl. `find_accession`), `pelsa_volcano_find_mask`,
   `pelsa_pin_metadata_rows`, `pelsa_feature_overlap_peptides`; tooltip-line change
   covered by a build-smoke assertion; Woods adj.P color (smoke: returns ggplot,
   no gold outline trace). Fix the `BG_ALPHA` duplicate.
2. **Wire into the module:** swap the pin observer from `pin_opacity` to
   `pin_recolor` (restyle traces 0 + 1, reading pinned() + find_accession());
   add the Find textInput/button + `find_accession()` reactiveVal + count notice;
   expand `pelsa_pin_metadata`; attach the per-feature overlap string in
   `pinned_woods` + pass to the feature track; default label mode -> none.
3. **Verify:** `devtools::document()` (no roxygen surface change expected),
   `devtools::load_all`, full PELSA testthat suite 0-fail, ASCII-clean, files
   <800 lines (note: both section3 files are already >800; this pass adds little
   - keep an eye, split deferred unless a file crosses materially).
4. Render smoke in the running app (manual) for the click recolor + tooltip +
   panel + Woods color.

## DECISIONS (locked 2026-06-15)
1. **Click highlight:** recolor via proxy restyle (no rebuild). Clicked = solid
   GOLD FILL; same-accession siblings = original fill + GOLD RING (not a second
   yellow fill - CVD-safe second channel); all others keep original color + full
   opacity (NO desaturation). Restyle BOTH background AND marker traces; resolve
   trace indices DYNAMICALLY (do not hard-code 0/1).
2. **Marker vs gold:** clicked marker -> gold fill (gold wins); sibling marker ->
   magenta fill + gold ring; non-selected markers stay plain magenta; revert on
   unpin/clear.
3. **Woods 3rd track:** color by `-log10(adj.P)` white->red (red = significant,
   matches the volcano); DROP the gold significance outline. (Supersedes the
   earlier raw-[0,1] blue->red idea - reversed convention + low dynamic range.)
4. **Single-peptide label** (tooltip + panel): winning-accession label only
   (`<winning_gene>_aa<pos>`), not the full multilabel.
5. **Peptide count** in panel: distinct PEP.StrippedSequence mapped to the
   accession.
6. **Find control:** textInput + "Highlight" button (not debounce-only); typed
   accession highlights all its peptides GOLD FILL via the SAME composite restyle
   as the click-pin; matches winning_accession OR PG token OR isoform-base, count
   = union; composes with an active pin (a sibling that is also found -> promoted
   to gold fill); Find does NOT open the fixed/Woods panel (notice teaches "click
   to pin"); cleared on contrast switch + by the Clear button; reports matched count.
7. **One composite restyle observer** owns pin + find + Woods highlight, depends
   on `input$pelsa_color_mode` so the highlight survives a color-mode rebuild.
8. **Clear selection** button resets pinned() + find_accession().
9. **BG_ALPHA** duplicate collapsed to 0.8; export ggplot alpha unified to match.
