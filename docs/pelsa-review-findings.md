# PELSA Plan — Specialist Review Findings (consolidated)

Four specialist reviewers (bioinformatics/proteomics scientist, biostatistician,
R/Shiny software engineer, UI/UX designer) reviewed `docs/pelsa-module-planning.md`
against the reference notebook, the benchmark, and the Protigy codebase on
2026-06-12. This is the deduplicated, prioritized synthesis. Reviewer tags:
**[Bio]** **[Stat]** **[Eng]** **[UX]**. Where two tags appear, the reviewers
flagged it independently — the strongest signal.

> Status: review only — no plan edits were made by the reviewers. Decisions and
> follow-up edits are tracked in the plan's Open Questions section.

---

## 🔴 CRITICAL — must resolve before implementation / fixtures

### X1. The "down-only / cell 28 / `sig_results`" description does not match the notebook  [Bio C3] [Stat C1]
Both science reviewers, independently, found the `20260609` notebook is **two-sided**:
`draw_volcano_significance` colors **both** up (red, `logFC>0`) and down (blue,
`logFC<0`) significant peptides, and **`adj.P.Val` is read from Protigy upstream —
there is no internal BH step and no `sig_results`/`sig_results_total` symbols in
this revision.** The plan elevates "down-only is the canonical significant set
matching the notebook" to CRITICAL, which is factually wrong for this notebook.
- **Impact:** blocks parity-fixture design (tests would encode the wrong behavior).
- **Resolution:** volcano significance coloring is **two-sided** (notebook parity);
  reserve **down-only** for the rollup tie-break (most-negative logFC) and
  intensity-line protein selection only. Correct the plan's "(BH)" wording — BH/
  `adj.P.Val` comes from Protigy, PELSA does not recompute it.

### X2. `pep_start` is FASTA-substring-derived in a monkey-patched helper the plan never cites  [Bio C1/C2]
The plan points implementers at `peptides.py::explode_peptide_rows` (tagged "Low
risk"), but that derives `pep_start` from Spectronaut's `PEP.PeptidePosition`. The
real FASTA positions come from **cell 11's `explode_peptide_rows_fasta`** (substring
`str.find` match against the parent FASTA, **one row per occurrence**
`pep_occurrence_idx`/`n_occurrences`, **drops** accession-not-in-FASTA and
peptide-not-found pairs). Cell 13 builds `exploded_cache` from the patched version.
- **Impact:** following the plan produces **wrong `aa<pos>` labels** — the actual
  scientific readout on the intensity line plots and the volcano `<gene>_aa<pos>`.
- **Resolution:** re-port `explode_peptide_rows_fasta`, not `peptides.py`. Reclassify
  **Moderate–High parity risk**. Build the `{accession: FASTA sequence}` map (Setup
  mentions FASTA only for storage). Add fixtures: peptide occurring ≥2× in one
  protein; peptide absent from its accession's FASTA; isoform accession. Also note
  there are **two** `pep_start` definitions in the notebook (FASTA in `exploded_cache`
  vs leading-Spectronaut-token in `prep_plot_df`) — the `<gene>_aa<pos>` label must
  use the FASTA one, and which occurrence labels a multi-occurrence peptide's single
  volcano dot must be pinned.

### X3. Volcano reuse of `tab_stat_plot.R` is overstated; 100k-point render + a missing stat stage  [Eng C1/C2/I1]
- `statPlot_*_Server` is hard-wired to the limma `stat_params`/`stat_results`
  contract and a `logFC.<g1>_over_<g2>` column grammar PELSA does not produce;
  `plotVolcano()` greps `Log.P.Value.*`. Reuse is **pattern-level, not drop-in** —
  fork the registry mechanism + pure label/search helpers; write a PELSA-specific
  tab/ome server + a PELSA `plotVolcano` consuming `logFC.<c>`/`adj.P.Val.<c>`.
- **`plotlyProxy` does NOT make a 100k-point SVG restyle cheap** — it only avoids the
  R-side `renderPlotly` re-exec; the browser still re-renders the trace. The codebase
  is SVG plotly (no `toWebGL` anywhere). **`toWebGL()`/`scattergl` + downsampling the
  non-significant cloud is a hard requirement** at peptide scale, and the
  "fade siblings" effect should put siblings in a **separate small trace** so the
  proxy restyle touches only that trace.
- **Missing pipeline stage:** who computes PELSA's `logFC.<c>`/`adj.P.Val.<c>`
  in-app? It is not in the re-port table or language-allocation table. This is a
  peptide-scale moderated-test step that must be ported + parity-tested.

### X4. Right-click as the only path to the intensity panel is undiscoverable + browser-menu conflict  [UX C1] [Eng N1]
plotly does not expose right-button state reliably; right-click pops the native
browser menu; no touch/trackpad story; zero on-screen affordance.
- **Resolution:** **left-click pins** the intensity panel (plotly-native, already used
  at `tab_stat_plot.R:773`); hover = tooltip only. Reserve right-click at most as an
  accelerator duplicating a visible "pin" control. Add a `helpText` caption
  ("Click a point to pin its peptide profile").

---

## 🟠 IMPORTANT — correctness / scale / validity

### X5. Winner's-curse + shared-peptide explosion inflate "N significant proteins"  [Stat C2] [Bio I3/I4]
Selecting the min-`adj.P.Val` peptide per accession (after exploding shared peptides
to every parent) is selection-on-the-winner: the retained peptide's p-value is
optimistically biased, and one shared significant peptide can make several proteins
look significant. Valid as a **display/prioritization** device; **not** protein-level
inference.
- **Resolution:** label the best-peptide panel and "proteins with ≥1 significant
  peptide" as **exploratory selection, not FDR-controlled protein calls**; don't
  report protein counts without that caveat. Optional principled path: ACAT/Simes
  per-protein p-value aggregation + BH across proteins  [Stat I1].

### X6. Peptide-level BH dependence + FDR-after-sign-filter  [Stat C3/C4]
Peptide-level BH over 100k+ tests with strong within-protein correlation is usually
conservative (OK) but error control is **per-peptide, not per-protein**; proteins with
many peptides dominate by count. If down-only is applied after a two-sided
`adj.P.Val`, the displayed set's FDR is no longer the nominal 0.05 — label it
"two-sided adj.P<0.05, then filtered to logFC<0" with **no new FDR claim**.

### X7. CV on linear scale for log-normal data  [Stat C5] [Bio I1]
Linear `CV=sd/mean` is skew-dominated and is a monotone function of `sigma_log`
anyway. Keep linear `cv_pct` for parity, but the **dashboard** should prefer
**geometric CV (from `sigma_log`)** and a **median**-CV summary line, not mean.
Also: per-condition sum-normalization is **compositional** (relative metric under
closure); different per-condition complete-case bases can bias cross-condition CV
comparison — consider a shared intersection basis, or document the artifact. Keep
`sigma_log` (don't drop it). `cv_pct_debiased` is "SD-debiased CV," not unbiased CV
— label accordingly  [Stat I3].

### X8. App-level dataset switcher: state threading + export-contract conflict  [Eng C3]
- Sibling section modules can't read each other's `input$`. Active dataset must be a
  **reactive threaded from a parent PELSA container module** (or a `globals` slot
  updated via the documented return-reactive→`observeEvent` pattern). Pick one.
- **Free-on-switch is a render/RAM optimization ONLY.** Export functions must
  re-generate **all analyzed datasets** from scratch (per the export contract), not
  just the "hot" one — else switching away silently drops a dataset from the zip.

### X9. Parity-harness determinism across R ↔ pandas  [Eng C4] [Stat nice-to-have]
`pandas.sort_values` is not stable by default; on `[adj.P.Val, logFC]` ties R
(`dplyr`/`data.table`, stable) and pandas pick **different** winning peptides →
flapping "exact match" tests. `foverlaps` vs `merge` row order differs too.
- **Resolution:** total-ordering tiebreak (e.g. final sort on `peptide_sequence`,
  then `accession`) applied identically both sides; pin pandas `kind="mergesort"`;
  canonical re-sort before `expect_equal`; **exclude KDE pixel values** from parity
  (R `density` bandwidth `nrd0` ≠ scipy) — test the underlying CV table instead.
  Relative float tolerance ~`1e-8`–`1e-6`; test `c4`/`sigma_log` against closed-form.

### X10. Setup UX: validation gate, progress, drag-stack scaling, color toggle  [UX C2/C3/C4] [Bio I7]
- **Start Analysis** has no pre-flight validation (no datasets checked? no condition
  column? order unconfirmed?), no progress/disable/cancel for the minutes-long
  UniProt fetch. Add a validation checklist + `withProgress` staged progress +
  disable-on-run + cancel for the fetch phase. State that an empty marker table is
  valid (overlay, not prerequisite).
- Per-condition `orderInput` stacks need **reset-to-default** buttons, scroll
  containment, and collapse single-replicate conditions to a static label.
- Replace the mutually-exclusive color **checkboxes** with a single `radioButtons`/
  segmented control (one source of truth, can't desync; removes ~40 lines of
  `shinyjs::runjs` disable juggling).
- **Missing science:** add a **missed-cleavage distribution** to Summary — for a
  limited-proteolysis assay this is primary digestion QC and `peptides.py::
  missed_cleavage_count` already exists  [Bio I7].

### X11. Dependency + packaging hygiene  [Eng I2/I3, N2]
- `arrow` is heavy for a Bioc/CRAN package and used for one job; the benchmark found
  parquet round-trips don't help at this scale — consider `.tsv`/`.rds` + `readr`, or
  make `arrow` **Suggests** with a readr fallback.
- `data.table` NSE (`.SD`, `:=`, bare columns) trips `R CMD check` global-variable
  NOTEs — declare via `utils::globalVariables()` + `@importFrom data.table .SD :=`.
- `matrixStats` is **not** in DESCRIPTION — make it a definite add (the whole perf
  argument depends on `rowSds`).

---

## 🟡 NICE-TO-HAVE / DELIGHTERS / A11Y

- **Accessibility** [UX A1–A4]: `shinyjqui::orderInput` has **no keyboard path** —
  add numeric-rank or up/down-button fallback. Significance + 9-bucket feature class
  are **color-only**; the palette has a red(`#d62728`)/green(`#2ca02c`) deuteranopia
  collision and gold markers ≈ olive `other` (`#bcbd22`) — add a **shape** channel for
  markers and a colorblind check.
- **Gene→accession disambiguation** [UX I1]: batch all pasted tokens into **one**
  modal (not N sequential prompts); pre-check the reviewed canonical accession;
  tooltip explaining "Reviewed = Swiss-Prot"; show invalid tokens inline. Note:
  reviewed/canonical flags come from **UniProt**, not `org.*.eg.db` [Eng I5].
- **Marker table** [UX I2]: per-row pending/spinner + explicit "not found" on UniProt
  lookups, not a blank gene cell.
- **Volcano control panel** [UX I5]: progressive disclosure — collapsible
  Contrast / Coloring / Labels / Markers / Advanced groups; the panel is already dense
  in `tab_stat_plot.R`.
- **Fade trigger** [UX I6]: tie the fade to the **pinned** selection, not transient
  hover (cheaper + less flickery).
- **Default label mode** [UX I7]: "best peptide per marker" as default, not "all
  peptides," to avoid a `_aa<pos>` label storm.
- **Sticky bar chrome** [UX I4]: z-index/offset so it doesn't overlap navbar
  dropdowns/toasts; render only on PELSA tabs.
- **File-size discipline** [Eng N3]: `tab_stat_plot.R` is ~1190 lines (>800 max);
  split PELSA volcano data-prep, intensity-line aggregation, and the foverlaps port
  into separate `tab_pelsa_section*_helpers.R` from the start.
- **Effect-size gate** [Stat nice-to-have]: optional logFC-magnitude gate for the
  *display* set so tiny-effect high-power peptides don't dominate the highlight.
- **Contaminant/decoy filtering** [Bio nice-to-have]: confirm Protigy removes
  keratins/trypsin/BSA/decoys upstream; if not, PELSA needs its own filter.
- **Top-N labeling** [Stat I6]: state explicitly it is visual-only, no inferential
  meaning.
- **`delinearize` mislabeled-base guard** [Bio nice-to-have]: surface the `<2 decades`
  failure as a user-facing Setup error (log10 data processed as log2 is the most
  likely real-world data problem).

---

## Cross-cutting "questions for the team" (raised by ≥1 reviewer)
1. Down-only vs two-sided volcano default (X1) — **blocks fixtures.**
2. `pep_start` source = FASTA `explode_peptide_rows_fasta` (X2).
3. Where do `logFC.<c>`/`adj.P.Val.<c>` come from in-app — ported stat stage or
   precomputed/uploaded? (X3)
4. Any protein-level count/claim reported off the rollup? If yes, ACAT/Simes or
   caveat-only? (X5)
5. `toWebGL`/downsampling acceptable for the peptide volcano? (X3)
6. `arrow` hard Imports vs `.tsv`/`.rds`+readr? (X11)
7. Geometric/median CV in dashboard while exporting linear `cv_pct` for parity? (X7)
8. Typical/max conditions×replicates and datasets-per-run (drives drag-stack vs
   alternative control, and segmented-bar vs dropdown). (X10, UX)
9. Empty marker table valid? (X10)
