# PELSA Export Plan

Planning doc for what the PELSA module writes into the global **Export** tab, the
folder structure it produces, and the framework that supports it.

Status: **planning only** — no code written yet. The framework (folders + figure
helper) is agreed; the exact file list for each stage subfolder is **TBD** (to be
decided per section).

---

## 1. How export works today (the contract)

The global export orchestrator is `R/tab_export.R`. The relevant seam:

- Each module returns a per-ome list of **export functions**. Shape:
  `exports[[tab_name]]() -> list(ome -> list(name = function(dir_name)))`.
- For every selected tab × ome, `tab_export.R`:
  1. creates `exports_dir/<ome>/<tab_name>/` (`tab_export.R:208`, `dir.create`),
  2. calls each export function with that path: `p(exports_in_tab_path)`
     (`tab_export.R:232`).
- Each export function writes one or more files **into the directory it is handed**.
- Everything is zipped into `protigy_exports.zip`.

PELSA currently registers **three** tab-names in `app_server.R:199-201`:

| Tab name (`app_server.R`)   | Section              | Currently writes |
|-----------------------------|----------------------|------------------|
| `PELSASection1_exports`     | Section 1 – Setup    | **nothing** (`return(list())`, `tab_pelsa_section1.R:1027`) |
| `PELSASection2_exports`     | Section 2 – Summary  | 6 CSVs: `cv`, `coverage`, `depth`, `unmatched`, `unannotated`, `peptide_metrics` |
| `PELSASection3_exports`     | Section 3 – Volcano  | 1 PDF (`volcano_plot`) + 3 CSVs: `proteins_of_interest`, `volcano_labels`, `plotted_intensities` |

### Reference pattern (display plotly, save ggplot)

Other tabs render interactive `plotly` on screen but **export the underlying
ggplot** via `ggsave`. Canonical example: `R/tab_qc_PCA.R:425-463`
(`qc_PCA_export_bundle`), which calls `ggsave(..., device = "pdf")` sized by the
shared `get_ggsave_params()` helper (`R/utilities.R:155`). PELSA follows the same
convention. The only plots needing a **parallel static ggplot built for export**
are the native WebGL/`plot_ly` ones — the **volcano** (precedent already exists in
Section 3) and the **Woods panel**.

---

## 2. Target folder structure

One nested `pelsa/` tree **per ome**, with three flat stage subfolders. Figures and
tables live **together** in each stage folder (no `figures/` vs `tables/` split).

```
<ome>/
└── pelsa/
    ├── 01_setup/          # Section 1 — run configuration + user inputs
    ├── 02_qc/             # Section 2 — QC figures + their data tables
    └── 03_differential/   # Section 3 — volcano / Woods / intensity + data tables
```

Figures are written as **both PDF and PNG** (PNG via `ragg`). Example of how a
stage folder looks once populated (illustrative — final file list is TBD):

```
<ome>/pelsa/02_qc/
├── coverage_distribution.pdf
├── coverage_distribution.png
├── coverage.csv
└── ...
```

---

## 3. Framework to build (before any file list is finalized)

The structure is contained **entirely inside PELSA's own code** — `tab_export.R`'s
generic loop is **not** modified. Each PELSA export function receives the handed
`dir_name` (= `<ome>/<tab_name>`) and creates its own stage subfolder inside it.

### 3.1 Stage-folder constants

Add to `R/tab_pelsa_constants.R`:

```r
.PELSA_STAGE_SETUP        <- "01_setup"
.PELSA_STAGE_QC           <- "02_qc"
.PELSA_STAGE_DIFFERENTIAL <- "03_differential"
```

### 3.2 New helper file: `R/tab_pelsa_export_helpers.R`

Two helpers:

- **`pelsa_export_stage_dir(dir_name, stage)`** — `dir.create()` the
  `dir_name/<stage>/` subfolder (recursive, idempotent) and return its path.
  This is what gives the nested `pelsa/<stage>/` tree without touching the
  global loop.

- **`pelsa_save_figure(plot, dir_name, basename, plot_type = "default")`** —
  write one ggplot as **PDF + PNG**, sized by the existing `get_ggsave_params()`:

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

  `ragg::agg_png` is the project's PNG device (deterministic — no embedded
  timestamp, unlike `grDevices::png()`). Verified against current ragg docs.

### 3.3 Per-section wiring (stubs first)

Each section returns export functions that:
1. call `pelsa_export_stage_dir(dir_name, <stage>)` to get the stage folder,
2. write their figures (via `pelsa_save_figure`) and tables (`write.csv`) into it.

Initial build = **wired-but-empty stubs** so the folder tree and helpers are in
place and exercised end-to-end; the actual file contents are filled in per
section once the file list is chosen.

- **Section 1** (`tab_pelsa_section1.R`): replace the `return(list())` placeholder
  (line 1027) with a `01_setup` writer.
- **Section 2** (`tab_pelsa_section2.R`): redirect existing 6 CSVs into `02_qc/`
  and add QC figure exports.
- **Section 3** (`tab_pelsa_section3.R`): redirect existing 4 exports into
  `03_differential/` and add the best-peptide volcano + Woods + intensity figures.

### 3.4 Dependency: `ragg`

Per `CLAUDE.md`, deps live in **two** places — add both, then `devtools::document()`:
1. `DESCRIPTION` → `Imports:` add `ragg`.
2. roxygen `@importFrom ragg agg_png` (in `R/protigy-package.R` or the helper).

---

## 4. Candidate output files per stage (TBD — for review)

> Decision pending: which of these to actually include in each subfolder.
> Listed as the full candidate set from the export review; trim/confirm per stage.

### `01_setup/` (Section 1) — currently exports nothing
- [ ] `pelsa_setup.yaml` — species, treatment compound, condition/replicate
      columns, sample order, contrast (reproducibility: reconstruct the run).
- [ ] `pelsa_markers.csv` — Accession, Gene Symbol (the user's marker table).

### `02_qc/` (Section 2)
Figures (PDF + PNG):
- [ ] `coverage_distribution` — per-protein sequence-coverage histogram.
- [ ] `peptide_length_density` — peptide-length density.
- [ ] `missed_cleavage` — missed-cleavage bar chart.
- [ ] `cv_kde` — per-condition CV KDE.
- [ ] `depth_per_sample` — peptides quantified per sample.

Tables (CSV) — existing:
- [x] `cv`, `coverage`, `depth`, `unmatched`, `unannotated`, `peptide_metrics`.

Tables (CSV) — proposed additions:
- [ ] `matched` — FASTA peptide→protein positions (downstream depends on it).
- [ ] `annotation_features` — UniProt feature annotations (drives Woods tracks).
- [ ] `qc_summary` — the `qc` counts list (one-glance run sanity check).

### `03_differential/` (Section 3)
Figures (PDF + PNG):
- [x] `volcano_all_peptides` — exists today (currently `volcano_plot`).
- [ ] `volcano_best_peptide` — best-peptide rollup volcano (needs static ggplot).
- [ ] `woods_<protein>` — Woods stability panel (needs static ggplot).
- [ ] `intensity_<protein>` — pinned-protein intensity line plot.

Tables (CSV) — existing:
- [x] `proteins_of_interest`, `volcano_labels`, `plotted_intensities`.

Tables (CSV) — proposed additions:
- [ ] `volcano_all_peptides` — full all-peptide volcano DF (data behind the figure).
- [ ] `volcano_best_peptide` — best-peptide rollup DF (headline result).
- [ ] `woods_data` — per-pinned-protein peptide segments behind the Woods figure.

---

## 5. Open decisions

1. **File list per stage** — confirm which candidates in §4 to include.
2. **Folder-name visibility** — the nested tree means PELSA's three sections fold
   into one `pelsa/` tree. Confirm the three sections keep separate `tab_export.R`
   tab-names (writing into the same `pelsa/` tree via stage subfolders) vs.
   collapsing to a single shared tab-name.
3. **Per-protein figures** — Woods/intensity are per pinned protein. Decide whether
   to export only the currently-pinned protein, or a set (and how the set is chosen).
```
