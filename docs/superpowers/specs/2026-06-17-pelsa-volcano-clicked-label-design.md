# PELSA volcano: dark-gold label for the clicked peptide

## Goal

When a peptide is clicked on the PELSA volcano — or on the Woods plot, which
also refreshes the fixed info panel — the volcano already highlights the clicked
peptide *and* every sibling peptide mapped to the same accession in gold. This
feature adds **one** dark-gold text label, `<gene>_aa<pep_start>`, for the
**clicked peptide only** (never the siblings). The label:

- updates whenever the selection changes (click another point, or click a
  segment in the Woods plot),
- clears when the selection clears,
- does **not** rebuild the ~100k-point WebGL cloud,
- does **not** interfere with the existing baked peptide-label annotations
  (Top-N / label-mode labels).

## Background / constraints

The volcano is a native plotly `scattergl` (WebGL) build. Two relevant facts
from `CLAUDE.md` and the code (`R/tab_pelsa_section3.R:800-883`):

1. **A click never rebuilds the cloud.** The gold selection highlight is a
   separate `scattergl` trace pushed/removed via `plotlyProxyInvoke`
   `addTraces` / `deleteTraces`. Adding a trace renders reliably on WebGL.
2. **Proxy `relayout(annotations=)` is unreliable on this WebGL volcano** — it
   was tried for a label fast-path and abandoned. The existing peptide labels
   (`pelsa_volcano_label_annotation_list` → `layout(annotations=)`) are
   therefore **baked into the build**, and baking requires a rebuild.

So the existing label *delivery* (baked `layout` annotations) cannot be reused
for a click-time update: click deliberately avoids the rebuild, and the
proxy-relayout path is unreliable. We reuse the existing label's **content and
placement convention** but deliver it through the **proven addTraces/deleteTraces
proxy path** the gold highlight already uses.

A `scattergl` text trace cannot carry a true white-background / dark-gold-outline
annotation box. The agreed-upon look is **dark-gold text + a white halo marker**
(the closest reliable approximation on a trace). The exact box would force the
unreliable relayout path or a per-click rebuild; both are rejected.

## Selection model (existing)

`selection()` is a `reactiveVal` holding
`list(origin, accession, peptide_seq, label, row)`. It is set by:

- the volcano click observer (`tab_pelsa_section3.R:917`),
- the Woods click observer (`tab_pelsa_section3.R:1147`) — sets `row = NA`,
- Find.

`row` may be `NA` (Woods path); resolve it with
`match(selection$peptide_seq, df$id)` against the active volcano df.

## Approach

A new single-point `scattergl`, `mode = "text"` overlay trace, pushed/removed
through the same proxy path as the gold marker overlay.

### 1. New constant — `R/tab_pelsa_constants.R`

```r
.PELSA_GOLD_DARK <- "#8B6914"   # clicked-peptide label text (dark gold)
```

### 2. New pure helper — `R/tab_pelsa_section3_helpers.R`

`pelsa_volcano_clicked_label_trace(df, selection)`:

- Returns `NULL` when `df` is empty, `selection` is `NULL`, or the clicked row
  cannot be resolved.
- Resolve the clicked row index: `selection$row` when a non-`NA` integer,
  otherwise `match(selection$peptide_seq, as.character(df$id))`. `NULL` if still
  unresolved.
- Build the label text from that one row, reusing the **same stem logic as
  `pelsa_volcano_tip`**: `gene_fb` = `winning_gene` else `PG.Genes`;
  `acc_fb` = `winning_accession` else first `PG.ProteinAccessions` token;
  `stem` = `gene_fb` when non-empty else `acc_fb`; text = `paste0(stem, "_aa",
  pep_start)`. Because `winning_gene` is already blanked for self-curated
  species upstream, self-curated rows fall back to the accession automatically —
  no special-casing here.
- Return a trace list ready for `addTraces`:

```r
list(
  type = "scattergl", mode = "text",
  x = <clicked logFC>, y = <clicked logP>,
  text = <label>,
  textposition = "top right",         # offset up-and-right (Stats-tab convention)
  textfont = list(color = .PELSA_GOLD_DARK, size = 11, family = "Arial"),
  marker = list(color = "rgba(255,255,255,0.9)", size = 14,
                line = list(width = 0)),   # white halo behind the text
  hoverinfo = "skip", showlegend = FALSE, meta = "pelsa_gold_label"
)
```

Pure (function of its args). Unit-testable.

### 3. Wire into the overlay observer — `R/tab_pelsa_section3.R`

The gold overlay is currently ONE trace at index 2, managed by
`apply_gold_overlay()` with a `gold_present()` flag. Extend this same function
to manage the overlay as a **set of up to two traces** — gold markers at
index 2, the clicked-peptide label at index 3 — added/removed as one unit:

- Replace the boolean `gold_present` with a small count/flag of how many overlay
  traces are currently on the client (`overlay_n`, 0–2), so deletes target the
  right indices.
- On re-apply: delete existing overlay traces **highest-index-first**
  (`deleteTraces` index 3 then 2, only those present), then add the gold marker
  trace (`pelsa_volcano_gold_trace`, index 2) if non-`NULL`, then add the
  clicked-label trace (`pelsa_volcano_clicked_label_trace`, index 3) if
  non-`NULL`. Track the new count.
- The base figure always has exactly 2 point traces (bg=0, markers=1), so the
  overlay indices 2/3 stay deterministic.

This automatically covers every required case, because all three set
`selection()` and fire the existing `observeEvent(list(selection(),
find_result()), ...)`:

- click → click: label text updates to the new clicked peptide,
- click → clear: gold + label both removed,
- Woods-click: Woods sets `selection()`, siblings light up gold on the volcano,
  and the clicked peptide gets the dark-gold label.

The base-rebuild observer (`onFlushed`, fires on color-mode / contrast /
label-mode / Top-N) already resets the overlay count and re-applies — the label
rides along for free. The find-only highlight does not resolve a single clicked
peptide, so `pelsa_volcano_clicked_label_trace` returns `NULL` for a pure-find
selection and no label is drawn (gold mask still applies).

### No changes to

`pelsa_volcano_build_plot`, the baked label annotations
(`pelsa_volcano_label_annotation_list` / `.pelsa_volcano_label_annotations`),
the Find highlight, the best-peptide panel, or the static export.

## Files touched

- `R/tab_pelsa_constants.R` — add `.PELSA_GOLD_DARK`.
- `R/tab_pelsa_section3_helpers.R` — add `pelsa_volcano_clicked_label_trace()`.
- `R/tab_pelsa_section3.R` — extend `apply_gold_overlay()` + overlay-trace
  bookkeeping (`gold_present` → `overlay_n`).

## Testing

New `tests/testthat/test-pelsa-volcano-clicked-label.R`, using the synthetic
PELSA fixtures:

- text is `<gene>_aa<pep_start>` for a normal row (resolved via `selection$row`),
- text falls back to `<accession>_aa<pep_start>` when the gene is blank
  (self-curated / blanked `winning_gene`),
- row resolves via `match(peptide_seq, id)` when `selection$row` is `NA`
  (the Woods path),
- `(x, y)` equal the clicked row's `(logFC, logP)`,
- returns `NULL` for `selection = NULL`, an empty `df`, and an unresolvable
  peptide,
- `textfont$color == .PELSA_GOLD_DARK` and `meta == "pelsa_gold_label"`.

## Out of scope / tradeoffs

- **True boxed label** (white bg + dark-gold border) is not delivered — it needs
  the unreliable relayout path or a per-click rebuild. Approximated with
  dark-gold text + white halo marker (accepted).
- Only the clicked peptide is labeled; siblings stay gold-highlighted but
  unlabeled (as specified).
