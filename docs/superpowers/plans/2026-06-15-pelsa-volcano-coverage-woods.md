# PELSA Volcano - polish fixes + new Coverage/Woods panel

**Date:** 2026-06-15 · **Branch:** feat/pelsa-integration · **Module:** PELSA Section 3 (Volcano)

Two scopes: **(A)** a batch of polish fixes to the existing volcano + pinned-peptide
intensity panel, and **(B)** a net-new per-protein **sequence-coverage + UniProt-feature
track + Woods plot** in the pinned (fixed) panel.

Grounded in two Opus explorations: the section-3 data-contract map and a Woods/coverage
R-visualization design. Key files: `R/tab_pelsa_section3.R` (module server),
`R/tab_pelsa_section3_helpers.R` (volcano build + stat_df + label modes),
`R/tab_pelsa_intensity_helpers.R` (intensity line data + ggplot),
`R/tab_pelsa_annotation_helpers.R` (`PELSA_FEATURE_COLORS`, feat_df),
`R/tab_pelsa_coverage_helpers.R` (union coverage).

---

## SCOPE A - Polish fixes (existing volcano + pinned panel)

### A1. Volcano label readability (box + white opaque background)
**Now:** `geom_text(size=2.6, vjust=-0.8, check_overlap=TRUE)` (`tab_pelsa_section3_helpers.R:700`) - labels sit ON the points, no background.
**Fix:** switch to `ggrepel::geom_label_repel` (ggrepel + the `label` geom is already a dep via `@import ggrepel`) with a white opaque fill, thin outline, and leader lines so labels never overlap the point. Mirrors the Statistics > Volcano section's boxed labels.
- `geom_label_repel(aes(label=label), fill="white", alpha=0.9, label.size=0.25, box.padding=..., max.overlaps=Inf, segment.color="grey50", size=2.6)`.
- Caveat: `ggrepel` + `ggplotly` + `toWebGL` - repel geoms do NOT round-trip through ggplotly cleanly (ggrepel renders at draw time; plotly can't compute repulsion). **Decision needed** (see Open Questions): either (a) keep labels as a plotly annotation layer drawn server-side with a white bg box (native plotly `add_annotations(bgcolor="white", bordercolor=..., ...)` after the ggplotly build - reliable, boxed, opaque), or (b) accept static-export-only repel. **Recommended: (a)** - add boxed labels via `plotly::add_annotations()` on the built plot (white `bgcolor`, grey `bordercolor`, `opacity=0.9`), positioned at each labeled point; this gives the exact "box + outline + white opaque bg" the user asked for AND survives toWebGL.

### A2. Default label mode = "Best peptide per marker"
**Now:** the label-mode control defaults to `top_n` (`.PELSA_VOLCANO_DEFAULT_TOP_N`).
**Fix:** set the Label-peptides input default to `best_per_marker` (`pelsa_volcano_label_rows` mode `"best_per_marker"` already exists). One-line change to the `selectInput`/`radioButtons` `selected=` in the sidebar builder.

### A3. Add "None" and "All significant peptides" label modes
**Now:** `pelsa_volcano_label_rows` supports `all_markers | best_per_marker | top_n` (`tab_pelsa_section3_helpers.R:251`).
**Fix:** add two modes:
- `"none"` -> `integer(0)` (no labels).
- `"all_significant"` -> all rows where `Significant == TRUE` (the volcano df already carries `Significant`/`sig_direction`). Label every significant peptide.
- Update the sidebar choices vector + the mode validation `match.arg`-style guard. Update `pelsa_volcano_label_rows` arg validation to accept the 5 modes.

### A4. Marker point size + non-marker visibility (de-emphasize less)
**Now:** background `size=1, alpha=0.6` colored by mode; markers `size=2.4` magenta `shape=21`. The user says markers are too big and non-markers too faint.
**Fix:**
- Markers: `size 2.4 -> ~1.6` (only slightly larger than background's 1).
- Background: raise `alpha 0.6 -> ~0.8` and keep `size=1` (or `1.1`) so non-marker peptides read clearly in their real sig/feature colors. The pin-fade alpha (0.12) for siblings-mode stays.
- These are constants in `pelsa_volcano_build_plot` - keep them named (e.g. `.PELSA_VOLCANO_MARKER_SIZE`, `.PELSA_VOLCANO_BG_ALPHA`) for one-place tuning.

### A5. Intensity panel - rename + reposition the grey bands; bold/capitalize
**Now:** `pelsa_intensity_line_ggplot` (`tab_pelsa_section3_helpers.R:754`) facets by `panel` with values `"significant"`/`"other"`; the "grey bars overlapping the lines" are the **facet strip backgrounds** sitting inside the panel, labeled lowercase "other"/"significant" (per Image #2).
**Fix:**
- Rename panel values: `"other" -> "Non-significant"`, `"significant" -> "Significant"` (capitalized first letter). Source: `pelsa_intensity_line_data` sets `panel` (`tab_pelsa_intensity_helpers.R`); rename there + anywhere the literal is matched.
- Bold the strip text: `theme(strip.text = element_text(face="bold"))`.
- Fix overlap: give facet strips a clear position (`strip.position="top"`, opaque strip background ABOVE the panel, not overlapping data) and add panel spacing; ensure `scales="free_y"` / consistent y so the band can't sit on the lines. If the current single-plot-with-shaded-rects interpretation is what's drawn (not facet strips), instead move the `annotate("rect", ...)` significance band to a non-overlapping y-region or convert to a faceted layout. **Confirm at implementation by rendering.**

### A6. Highlight the pinned peptide in the intensity legend (bold gold)
**Now:** legend colored by `aa_label` (e.g. aa14/aa462/aa5); the clicked peptide is not visually distinguished.
**Fix:** the pinned peptide's `aa_label` is known from `pin$label` / `pin$peptide_seq` -> its `pep_start`. Bold + gold-color that one legend entry. ggplot legend per-key text styling is not native; approaches:
- Build a named vector of legend text colors/faces and apply via `theme(legend.text = element_text(...))` won't do per-key. Instead: use `scale_color_manual` with the pinned key's label wrapped, OR post-process the ggplotly legend (plotly `layout`/`restyle` legend item font is not per-item either).
- **Recommended:** append a marker to the pinned key's legend label (e.g. bold via an HTML `<b>` in the plotly legend text + set that trace's line/marker to gold), since plotly legend entries DO render per-trace and the pinned peptide is its own trace. Set the pinned trace's color to gold (`#D4AF37`) and its name to `<b>aa462 (selected)</b>`. This is reliable in plotly.

---

## SCOPE B - New Coverage + UniProt-feature + Woods panel

A per-protein panel in the pinned ("Pinned Peptide") box that appears when a peptide is
pinned. Three vertically-stacked tracks sharing a residue-position x-axis (1..protein_length):

1. **Coverage / residue ruler** (top, thin): full-length grey backbone; residues covered by >=1 peptide highlighted GOLD; x ticks at 1,10,20,...,end.
2. **UniProt feature track** (middle): colored segments per `feature_class` (the existing `PELSA_FEATURE_COLORS` palette), lane-packed so overlapping features stack.
3. **Woods plot** (bottom, dominant): each peptide a horizontal `geom_segment` from `pep_start..pep_end` at y = logFC (current contrast); peptides with significant change drawn with a GOLD outline (thick gold segment under the colored one).

### B1. Data plumbing (reuse what exists; one gap)
At pin time the module has: `pinned()$accession`, `cache_entry()$matched` (peptide spans:
`PEP.StrippedSequence, accession, pep_start, pep_end, pep_occurrence_idx`), `cache_entry()$coverage`
(`accession, protein_length, ...`), `feat_df()` (raw UniProt rows: `accession, start, end, feature_class` + optional type/description), `stat_df_raw()` + `active_contrast()` (per-peptide `logFC.<c>`, `adj.P.Val.<c>`), `marker_accessions()`.
- **protein_length:** from `cache_entry()$coverage$protein_length[acc]` - NO FASTA re-read needed for the ruler extent.
- **GAP - full residue letters:** the per-residue SEQUENCE string is NOT in the cache (the sketch's ruler shows numbers, not letters, so we only need LENGTH + covered intervals - letters are optional). **Decision:** the ruler shows positions + gold coverage bars using `protein_length` + covered-interval union; we do NOT need the residue letters. (If letter-level tooltips are later wanted, re-read FASTA via `pelsa_species_fasta_path(pelsa_database_dir(), species_r())` cached in a reactiveVal.)

### B2. New pure helpers (new file `R/tab_pelsa_woods_helpers.R`)
- `pelsa_woods_peptide_data(accession, matched, stat_df, contrast, sig_cutoff=0.05)` -> per-peptide df: `peptide_seq, pep_start, pep_end, logFC, adj.P.Val, sig` (sig = adj.P.Val < cutoff). Joins matched spans (for `accession`) to the contrast stat columns by `PEP.StrippedSequence`.
- `pelsa_coverage_intervals(starts, ends)` -> merged covered intervals (union). Use `IRanges::reduce` (new dep) OR a pure sweep-line fallback (the explore agent gave both; the codebase already has `.pelsa_union_length` in `tab_pelsa_coverage_helpers.R` - reuse/extend its sweep-line to RETURN intervals, avoiding a new dep).
- `pelsa_feature_lanes(features)` -> features + `lane` (greedy interval packing). Pure data.table greedy (no new dep) or `IRanges::disjointBins`.
- `pelsa_woods_overlap_annotations(peptide_span, features)` -> for a peptide's tooltip: the list of feature regions it overlaps (`findOverlaps`-style; data.table `foverlaps` already used in annotation helpers).

**Dependency decision:** prefer **no new dep** - the sweep-line union + greedy lane packing are ~30 lines total and the codebase already does interval math with `data.table::foverlaps` (annotation helpers) and a sweep-line (`.pelsa_union_length`). Only add `IRanges` if the hand-rolled versions prove fiddly. (Documented as Open Question.)

### B3. New plot builders (in the helpers file)
- `pelsa_woods_track_ggplot(peptides, prot_len)` - `geom_segment` Woods (gold halo for sig, `scale_color_gradient2` by logFC, `geom_hline(0)`), with a `.tip` text column (sequence, start-end, len, adj.P, logFC, overlapping annotations) for `tooltip="text"`.
- `pelsa_coverage_track_ggplot(intervals, prot_len)` - grey backbone `geom_rect` + gold covered `geom_rect`; x breaks at 1,10,20,...,N.
- `pelsa_feature_track_ggplot(features_with_lanes, prot_len, palette=PELSA_FEATURE_COLORS)` - colored `geom_rect` per lane; `.tip` per feature.
- `pelsa_woods_panel(...)` - assemble the three via `plotly::subplot(nrows=3, shareX=TRUE, heights=c(0.12,0.28,0.60) top->bottom or as sketched, titleY=TRUE)`, then `event_register("plotly_click")` on the Woods source. Keep these SVG (NO toWebGL - segment/rect hover fidelity; only a few hundred peptides/features).

### B4. Module wiring (`tab_pelsa_section3.R`)
- Add `plotly::plotlyOutput(ns("pelsa_woods_panel"), height=...)` into the pinned box (`column(3)`, after the intensity plot) OR below the volcano in `column(6)` given horizontal extent. **Recommended: below the intensity plot in the pinned column, full width of that column**, with a clear section header "Sequence coverage & Woods plot".
- `output$pelsa_woods_panel <- plotly::renderPlotly({ req(pinned()); ... })` computing the three track data frames off the cache + stats for `pinned()$accession`, lazily (only when pinned).
- **Cross-plot click (Woods -> volcano gold highlight):** register `plotly_click` on `source=ns("pelsa_woods")`; its observer resolves the clicked peptide to its `accession`/`peptide_seq` directly (the Woods data has explicit identity - no coordinate matching needed) and writes a NEW reactiveVal `woods_selected()`. A `plotlyProxyInvoke("restyle", marker.line.color/width)` on the volcano output draws a GOLD outline on the matching point(s) - distinct from the magenta marker fill. (Mirrors the existing sibling-fade proxy at `tab_pelsa_section3.R:681`; no volcano rebuild.)
- Clear `woods_selected()` on contrast switch (same as `pinned()`).

### B5. Tooltips (Woods peptides)
Pre-join each peptide to overlapping features (`pelsa_woods_overlap_annotations`) and build the
`.tip`: `"<seq><br>aa <start>-<end> (len N)<br>adj.P: <..><br>logFC: <..><br>Annotations: <class@start-end; ...>"`.
Surface via `ggplotly(..., tooltip="text")`.

### B6. Export
Add a `coverage_woods` entry to the Section-3 export list (`tab_pelsa_section3.R:828`): a `safe_export` writing the per-peptide Woods table (seq, span, logFC, adj.P, sig) + covered intervals + feature lanes for the pinned/active protein, re-derived from cache + stats.

### B7. Tests (pure helpers; no Shiny/network)
- `pelsa_woods_peptide_data`: correct join, sig flag at cutoff boundary, NA-span peptides dropped.
- `pelsa_coverage_intervals`: union of overlapping/adjacent/disjoint intervals; single residue; empty.
- `pelsa_feature_lanes`: overlapping features get distinct lanes; nested; disjoint share lane 0/1.
- `pelsa_woods_overlap_annotations`: a peptide overlapping 0 / 1 / many features.
- Plot builders: smoke (returns ggplot/plotly; ASCII-clean; correct color palette keys).
- A focused volcano label-mode test for the new `none` + `all_significant` modes.

---

## Sequencing

1. **Scope A** first (small, independent, immediately shippable): A2/A3/A4 (constants + label modes + default) -> A1 (boxed labels) -> A5/A6 (intensity panel). Commit as one `fix/style(pelsa)` batch with tests.
2. **Scope B** next (the new module): helpers + tests (B2/B3/B7) -> module wiring (B4/B5) -> cross-plot highlight -> export (B6). Commit separately as `feat(pelsa): coverage+Woods panel`.
3. Verify after each: ASCII-clean, files <800 lines, full PELSA suite 0-fail, render smoke.

## DECISIONS (locked 2026-06-15)
1. **A1 boxed labels:** native plotly `add_annotations` per labeled point - white `bgcolor` with **alpha** (matching the Statistics > Volcano boxed-label style) and a **border colored to match the labeled point's color** (sig_color or feature_color per the active color mode). Survives toWebGL. Small deterministic vertical stagger to reduce box-on-box overlap; rely on default mode `best_per_marker` + the new `none` mode to keep counts low.
2. **B2 dependency:** **USE IRanges** (Bioconductor, already installed v2.46.0). `IRanges()` + `reduce()` (covered union), `disjointBins()` (feature lane packing), `findOverlaps()`+`queryHits/subjectHits` (peptide<->feature tooltip join). Add to DESCRIPTION Imports + roxygen `@importFrom IRanges IRanges reduce disjointBins findOverlaps queryHits subjectHits start end width`; re-run `devtools::document()`.
3. **B4 placement / LAYOUT:** an **L-shaped pinned card** for visual continuity. Card 1 (the L) = pinned-peptide metadata + intensity line plot (upper-left arm) CONTINUOUS with the full-width coverage/feature/Woods tracks along the bottom - one bordered card, all "about the pinned peptide". Card 2 = the volcano plot + customization controls, in the top-right notch of the L. Implemented as Shiny grid + card styling (not literal geometry): `fluidRow(column(left = metadata+intensity), column(right = volcano+controls))` then a full-width `fluidRow` for the 3 tracks; shared border/background on the left column + bottom row so they read as one card, volcano as its own card. The L's bottom arm + intensity render only on pin.
4. Woods y-axis: literal logFC (per sketch) confirmed; significant = gold outline confirmed. A1 border = labeled point's own color confirmed.
