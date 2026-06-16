# PELSA Export Plan

Planning doc for what the PELSA module writes into the global **Export** tab, the
folder structure it produces, and the framework that supports it.

Status: **implemented** — this doc is retained as the design record. The framework
(folders + figure helpers) and the per-stage output set below are live in the app
across all three PELSA sections; see `R/tab_pelsa_export_helpers.R` and the
`tab_pelsa_section1/2/3.R` export closures. The Open decisions in §6 are resolved.

---

## 1. How export works today (the contract)

The global export orchestrator is `R/tab_export.R`. The relevant seam:

- Each module returns a per-ome list of **export functions**. Shape:
  `exports[[tab_name]]() -> list(ome -> list(name = function(dir_name)))`.
- For every selected tab x ome, `tab_export.R`:
  1. creates `exports_dir/<ome>/<tab_name>/` (`tab_export.R:208`, `dir.create`),
  2. calls each export function with that path: `p(exports_in_tab_path)`
     (`tab_export.R:232`).
- Each export function writes one or more files **into the directory it is handed**.
- Everything is zipped into `protigy_exports.zip`.

PELSA currently registers **three** tab-names in `app_server.R:199-201`:

| Tab name (`app_server.R`)   | Section              | Currently writes |
|-----------------------------|----------------------|------------------|
| `PELSASection1_exports`     | Section 1 - Setup    | **nothing** (`return(list())`, `tab_pelsa_section1.R:1027`) |
| `PELSASection2_exports`     | Section 2 - Summary  | 6 CSVs: `cv`, `coverage`, `depth`, `unmatched`, `unannotated`, `peptide_metrics` |
| `PELSASection3_exports`     | Section 3 - Volcano  | 1 PDF (`volcano_plot`) + 3 CSVs: `proteins_of_interest`, `volcano_labels`, `plotted_intensities` |

### Reference pattern (display plotly, save ggplot)

Other tabs render interactive `plotly` on screen but **export the underlying
ggplot** via `ggsave`. Canonical example: `R/tab_qc_PCA.R:425-463`
(`qc_PCA_export_bundle`), sized by the shared `get_ggsave_params()` helper
(`R/utilities.R:155`). PELSA follows the same convention. Several PELSA figures
are native `plot_ly`/WebGL and need a **parallel static ggplot built for export**
(see §4.3 build notes).

---

## 2. Target folder structure

One nested `pelsa_exports/` tree **per ome**, three flat stage subfolders. Figures
and tables live **together** in each stage folder (no `figures/` vs `tables/` split).
Figures are written as **both PDF and PNG** (PNG via `ragg`).

```
<ome>/
└── pelsa_exports/
    ├── 01_setup/                  # Section 1 - run configuration + user inputs
    ├── 02_qc/                     # Section 2 - QC figures + summary tables
    └── 03_volcano/                # Section 3 - volcano / intensity / Woods
        ├── 01_volcano/            #   one volcano per contrast (+ best-peptide if enabled)
        ├── 02_intensity_line/
        │   ├── 01_marker/         #   one per marker protein (all its peptides)
        │   └── 02_significant/    #   one per protein with >=1 significant peptide
        └── 03_woods/
            ├── 01_marker/         #   one per marker protein
            └── 02_significant/    #   one per protein with >=1 significant peptide
```

Significant-protein rule (decision): a protein is "significant" if it is mapped by
>=1 significant peptide in **any** contrast - the significant set is the **union
across all contrasts**, pooled with **no contrast annotation** on the protein.
Folders stay **flat** (no per-contrast nesting); Woods filenames carry the
contrast token (Woods is per-contrast), intensity filenames do not (intensity data
is contrast-independent).

Dedup rule (decision): **marker proteins take precedence**. If a significant
peptide maps to a marker protein, that protein's intensity/Woods figures are
written under `01_marker/` only - never duplicated into `02_significant/`.

---

## 3. Framework to build (helpers + wiring)

The structure is contained **entirely inside PELSA's own code** - `tab_export.R`'s
generic loop is **not** modified. Each PELSA export function receives the handed
`dir_name` (= `<ome>/<tab_name>`) and creates its own subfolders inside it.

### 3.1 Stage-folder constants

Add to `R/tab_pelsa_constants.R`:

```r
.PELSA_STAGE_SETUP   <- "01_setup"
.PELSA_STAGE_QC      <- "02_qc"
.PELSA_STAGE_VOLCANO <- "03_volcano"

# Section 3 sub-stage folders
.PELSA_SUB_VOLCANO   <- "01_volcano"
.PELSA_SUB_INTENSITY <- "02_intensity_line"
.PELSA_SUB_WOODS     <- "03_woods"
.PELSA_GRP_MARKER    <- "01_marker"
.PELSA_GRP_SIGNIF    <- "02_significant"
```

### 3.2 New helper file: `R/tab_pelsa_export_helpers.R`

- **`pelsa_export_stage_dir(dir_name, ...)`** - `dir.create()` the nested subfolder
  path (recursive, idempotent) and return it. Gives the `pelsa_exports/<stage>/...` tree
  without touching the global loop.
- **`pelsa_save_figure(plot, dir_name, basename, plot_type = "default")`** - write
  one ggplot as **PDF + PNG**, sized by the existing `get_ggsave_params()`:

  ```r
  pelsa_save_figure <- function(plot, dir_name, basename, plot_type = "default") {
    p <- get_ggsave_params(plot_type)
    ggsave(paste0(basename, ".pdf"), plot, device = "pdf",
           path = dir_name, width = p$width, height = p$height, units = p$units)
    ggsave(paste0(basename, ".png"), plot, device = ragg::agg_png,
           path = dir_name, width = p$width, height = p$height,
           units = p$units, dpi = 300)
  }
  ```
- **`pelsa_safe_name(x)`** - sanitize gene/accession/contrast tokens for filenames
  (strip/replace path-unsafe chars, collapse whitespace to `_`). Contrast suffixes
  are already `A_over_B`-shaped; gene/accession may need cleaning.

### 3.3 Dependency: `ragg`

Per `CLAUDE.md`, deps live in **two** places - add both, then `devtools::document()`:
1. `DESCRIPTION` -> `Imports:` add `ragg`.
2. roxygen `@importFrom ragg agg_png` (in `R/protigy-package.R` or the helper).

---

## 4. Output files per section

Template names below are the **basename** (figures get `.pdf` + `.png`; tables get
`.csv`). `<...>` are runtime-substituted, sanitized tokens.

### 4.1 `01_setup/` (Section 1) - currently exports nothing

| Template name            | Type | Contents |
|--------------------------|------|----------|
| `pelsa_setup.yaml`       | yaml | species, treatment compound, condition/replicate columns, sample order, contrast(s) - enough to reconstruct the run |
| `pelsa_markers.csv`      | csv  | marker table: `accession`, `gene` |

*(Section 1 list is provisional - confirm whether setup export is in scope now or
deferred.)*

### 4.2 `02_qc/` (Section 2)

**Figures** (PDF + PNG):

| Template name              | Source reactive (`tab_pelsa_section2.R`) | Plot |
|----------------------------|------------------------------------------|------|
| `coverage_distribution`    | `coverage_plot_reactive`                 | per-protein sequence-coverage histogram |
| `peptide_length_density`   | `length_plot_reactive`                   | peptide-length density |
| `missed_cleavage_bar`      | `missed_plot_reactive`                   | missed-cleavage bar chart |
| `cv_kde`                   | `cv_plot_reactive`                       | per-condition CV KDE |
| `n_peptides_per_sample`    | `depth_plot_reactive`                    | quantified (non-NA) peptides per sample |

**Tables** (CSV) - three summary levels. The current 6 raw dumps (`cv`,
`coverage`, `depth`, `unmatched`, `unannotated`, `peptide_metrics`) are **dropped
entirely** (decision); only these three summaries are written. Column headers are
concise but explicit.

> Source-of-truth rule (decision): coverage and peptide-length distributions are
> being added to the **QC tab** as **per-condition** computations. The export
> **reads those QC-tab values** rather than recomputing. The QC tab currently
> computes medians; per the decision it will **also compute the per-condition
> means** (silently) so the export can report both - keeping one computation path.

`qc_sample_summary.csv` - **one row per sample**:

| Column                  | Meaning |
|-------------------------|---------|
| `sample`                | sample (matrix column) name |
| `n_peptides_quantified` | number of non-NA peptides in this sample (depth) |

`qc_condition_summary.csv` - **one row per condition** (all sourced from the QC tab):

| Column                  | Meaning |
|-------------------------|---------|
| `condition`             | condition label |
| `n_peptides_quantified` | peptides with >=1 non-NA value across the condition's replicates |
| `median_cv_pct`         | per-condition median CV (%) |
| `mean_cv_pct`           | per-condition mean CV (%) |
| `median_coverage`       | per-condition median sequence coverage |
| `mean_coverage`         | per-condition mean sequence coverage |
| `median_peptide_length` | per-condition median peptide length |
| `mean_peptide_length`   | per-condition mean peptide length |
| `mean_missed_cleavages` | per-condition mean missed cleavages |

`qc_experiment_summary.csv` - **one row** (whole experiment):

| Column                     | Meaning |
|----------------------------|---------|
| `n_peptides_total`         | total peptides = rows in original input (`qc$n_peptides`) |
| `n_unmatched_peptides`     | peptides that failed FASTA match (`qc$n_unmatched_rows`) |
| `pct_unmatched_peptides`   | percent of peptides failing FASTA match |
| `n_unannotated_proteins`   | proteins that failed feature annotation (`qc$n_unannotated_accessions`) |
| `pct_unannotated_proteins` | percent of proteins failing feature annotation |

### 4.3 `03_volcano/` (Section 3)

#### `01_volcano/` - one volcano per contrast

| Template name                          | When |
|----------------------------------------|------|
| `all_peptide_volcano_<contrast>`       | always (default) |
| `best_peptide_volcano_<contrast>`      | only if user enabled "Show best peptide per protein" (`input$pelsa_show_best_panel`) |

`<contrast>` = the stat-suffix form, e.g. `A_over_B`.

Build rules (match the in-app figure):
- **Coloring** follows the in-app customization (`input$pelsa_color_mode`:
  `significance` or `feature`). Existing static export already honors this.
- **Marker peptides** highlighted in magenta (`#FF00FF`); **no gold** selected/found
  points (static export already omits gold).
- **Peptide labels** follow the in-app option (`input$pelsa_label_mode` +
  `pelsa_top_n`), label format `<gene>_aa<position>`.

> Build note: the existing static export `.pelsa_export_ggplot()`
> (`tab_pelsa_section3_helpers.R:1068`) bakes color + markers but **not labels**
> (in-app labels are plotly annotations). Add label baking (geom_text/ggrepel) using
> `pelsa_volcano_label_rows()` so the static figure matches the chosen label mode.

#### `02_intensity_line/` - per-protein intensity line plots

| Subfolder         | One figure per...                                              | Template name |
|-------------------|---------------------------------------------------------------|---------------|
| `01_marker/`      | each marker protein (all peptides mapped to it)               | `intensityLine_<gene>_<accession>` |
| `02_significant/` | each protein significant in ANY contrast (excluding markers)  | `intensityLine_<gene>_<accession>` |

e.g. `intensityLine_FKBP3_Q00688.png`. No contrast token - intensity data is
contrast-independent (raw per-condition means); one figure per protein.

Significant-set derivation: across **all** contrasts, filter significant peptides
-> collect their `winning_accession`s (union) -> for each accession, plot **all**
its mapped peptides. Reuses `pelsa_intensity_line_data(..., show_all = TRUE)`.

> Build note: in-app intensity render is `pelsa_intensity_line_plot()` (ggplot ->
> ggplotly, or a 2-panel subplot for markers). Export needs the **ggplot** form
> (`pelsa_intensity_line_ggplot()` already exists) saved via `pelsa_save_figure`.

#### `03_woods/` - per-protein Woods plots

| Subfolder         | One figure per...                                                  | Template name |
|-------------------|-------------------------------------------------------------------|---------------|
| `01_marker/`      | each marker protein x each contrast                               | `woods_<gene>_<accession>_contrast_<A>_over_<B>` |
| `02_significant/` | each protein significant in ANY contrast x each contrast (non-marker) | `woods_<gene>_<accession>_contrast_<A>_over_<B>` |

e.g. `woods_FKBP2_P26885_contrast_rapamycin_1uM_over_DMSO.png`. Woods is
per-contrast (coloring + the log2FC y-axis are contrast-specific), so one figure
per (protein x contrast); membership in `02_significant/` is by the pooled
union-across-contrasts rule above.

Build rules:
- Peptide segments colored by **-log10(adj.P)** gradient (`grey92` -> `#B2182B`,
  capped at 5) - same as in-app.
- **Legend shows ALL UniProt feature classes** (the 9 in `PELSA_FEATURE_COLORS`),
  even those absent from the protein, so the reader knows the full feature set.
- Contrast suffix is in the filename because Woods coloring/significance is
  contrast-specific.

> Build note: in-app Woods is a 3-track plotly subplot (`pelsa_woods_panel()`,
> coverage ruler + feature track + Woods track). The static export needs a single
> stacked **ggplot** (e.g. patchwork/`cowplot` of the three `*_ggplot` track
> builders) with the feature legend forced to all classes (factor levels with
> `drop = FALSE`). This is the largest net-new export builder.

---

## 5. Per-section wiring summary

Each section's export function:
1. calls `pelsa_export_stage_dir(dir_name, <stage>, ...)` to get the target folder,
2. writes figures (`pelsa_save_figure`) and tables (`write.csv`) into it.

- **Section 1** (`tab_pelsa_section1.R:1027`): replace `return(list())` with the
  `01_setup` writer.
- **Section 2** (`tab_pelsa_section2.R`): replace the 6 raw CSVs with the three
  `02_qc` summaries + 5 figures.
- **Section 3** (`tab_pelsa_section3.R`): write the nested `03_volcano` tree -
  per-contrast volcanoes, plus per-protein intensity and Woods figures grouped into
  `01_marker` / `02_significant`.

Initial build can be **wired-but-empty stubs** (folder tree + helpers exercised
end-to-end) before filling figure/table contents.

---

## 6. Decisions (resolved)

1. **QC coverage/length metrics** - become **per-condition**, computed in the QC
   tab (medians + silently-added means); the export reads QC-tab values (§4.2).
2. **Contrast dimension** - significant proteins are pooled as the **union across
   all contrasts** (no contrast annotation on the protein). Folders stay flat.
   Woods is per-(protein x contrast) with the contrast in the filename; intensity
   is one-per-protein (contrast-independent) (§4.3).
3. **Raw Section 2 CSVs** - **drop all 6**; keep only the 3 summaries (§4.2).
4. **Protein figure naming** - `woods_<gene>_<accession>_contrast_<A>_over_<B>` and
   `intensityLine_<gene>_<accession>` (accession-only when gene missing).
5. **Section 1 setup export** - in scope: `pelsa_setup.yaml` + `pelsa_markers.csv`.

---

## 7. Figure design notes (from the visual-iteration prototypes)

The two layouts were first built as standalone ggplot2 scripts iterated against the
notebook's FKBP2 / FKBP3 reference figures, then ported into the app as
`pelsa_woods_export_ggplot` (`R/tab_pelsa_woods_helpers.R`) and
`pelsa_intensity_export_ggplot` (`R/tab_pelsa_section3_helpers.R`). The throwaway
prototype scripts and their example PNGs have been removed; the design notes below
are kept as the spec for those builders.

Woods design notes (single-panel, notebook layout + app schema):
- bold left title `GENE (ACC), N aa`; italic subtitle `Wood's plot: <contrast>`;
  y-axis `log2FC`; grey `Coverage: X% (N aa, M pep)` annotation in top headroom.
- peptide segments at `y = log2FC`, colored by `-log10(adj.P)` (`grey80`->`#B2182B`,
  cap 5; `grey80` low anchor keeps ns peptides legible); zero reference line.
- ALL UniProt features for the protein, lane-packed, drawn as a colored band at
  the bottom with a box outline separating it from the peptide panel; legend
  forced to all 9 `PELSA_FEATURE_COLORS` classes via a zero-area dummy layer.

Intensity design notes:
- centered bold title `GENE (ACC)`; centered subtitle `Mapped with N peptide(s)`
  (pluralized); y-axis `Average log<base>(intensity)` (`log_base` from setup, 2/10);
  two shared-y facets `Significant peptides (n)` | `Non-significant peptides (n)`.

Open feedback points:
- whether to add the in-app coverage ruler track above the Woods panel.

Next step after sign-off: port the two builders into the app
(`pelsa_woods_export_ggplot`, `pelsa_intensity_export_ggplot`), wire into the
Section 3 export, and add the framework helpers (§3).
