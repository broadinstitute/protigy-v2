# PELSA

PELSA (Peptide-centric Local Stability Assay) analyzes **peptide-level** proteomics data
to localize ligand- or treatment-induced changes along each protein. Peptides are matched
to their protein sequences, and per-peptide differential statistics (from the **Statistics**
tab) are mapped onto sequence position and overlaid on UniProt structural/functional
features. Use it to ask *where* on a protein a treatment changes abundance — e.g. peptides
near a binding site or folded domain that shift with treatment in a known target.

PELSA is a separate top-level menu with three tabs (**Setup**, **Summary**, **Volcano Plot**),
each driven by a shared dataset switcher at the top. Configure and run **per dataset (ome)**.

> PELSA does **not** compute fold changes or p-values. It consumes `logFC`, `P.Value`, and
> `adj.P.Val` produced by the **Statistics** tab. Set up and run a contrast in Statistics for
> the same ome before using the PELSA volcano.

## Setup

Each uploaded dataset gets its own Setup form. Use **Skip PELSA analysis for this dataset**
to exclude an ome from the run (its configuration is preserved, and it is dropped from the
Summary/Volcano switcher). **Apply this dataset's setup to all others** copies the active
form to every other non-skipped dataset.

### Data inputs

- **FASTA file (.fasta / .fa)** — protein sequences peptides are matched against. Required.
- **Self-curated database (no annotation file)** checkbox:
  - **Unchecked (default)** — FASTA headers are parsed **UniProt-style** (accession taken
    from `>sp|P12345|NAME` / `>tr|...`), and a **feature annotation TSV is required**.
  - **Checked** — FASTA headers are parsed by **first whitespace token**, and the annotation
    uploader is **disabled**. Features are unavailable, so the volcano is forced to color by
    significance and peptide labels fall back to accession.
- **Feature annotation file (.tsv)** — one row per feature. Required columns:
  `accession`, `feature_type`, `start`, `end`, `description` (1-based, inclusive
  coordinates); optional `coord_quality` (`exact`/`fuzzy`, defaults to `exact`). This file
  is produced by an **external UniProt-fetch workflow** — there is no in-app fetching.

Protigy classifies each raw feature on load into a **feature class** (e.g.
*active/binding site*, *catalytic domain*, *folded domain*, *region/motif*,
*repeat/coiled-coil*, *transmembrane/signal*, *low-complexity/disorder*, *other*). An
accession that is **absent from the annotation file counts as a failed annotation** (and is
exported as `missing_accessions.txt`).

### Treatment compound and markers

- **Treatment compound** — choose a preset (from `compound_markers.yaml`) to replace this
  dataset's marker list with that compound's known targets, or add a new compound.
- **Marker proteins** — paste accessions (space/comma/semicolon/newline separated) and
  **Add markers**, or edit the marker table directly. Markers are always drawn in **magenta**
  on the volcano and given a significant/non-significant facet split in intensity views.
  **Set as default marker list for this compound** persists the table.

### Condition / replicate configuration

- **Condition grouping column** and **Replicate identifier column** — pick from the
  dataset's sample metadata columns.
- **Condition order** and **Replicate order within each condition** — drag to set the
  canonical sample order used by every downstream plot (Reset restores defaults).

### Run analysis

**Start Analysis** validates the setup, then runs all non-skipped datasets under a progress
bar and redirects to **Summary** on success. The analysis is deterministic and network-free.

## Summary

A read-only QC dashboard that reads the analysis cache (run **Start Analysis** first; it
never recomputes). Value boxes report total peptides, fully-quantified peptides, peptides
that failed FASTA matching, proteins with ≥1 / 0 annotated features, and failed annotations.
Coverage, peptide-length, and CV panels toggle between **Experiment-wide** and
**Per-condition** views. Collapsible tables list unmatched peptides and unannotated proteins,
each with a CSV export.

**What the analysis computes:**
- **Position mapping** — each peptide is matched as an **exact substring** of its protein's
  FASTA sequence (all occurrences emitted; **no I↔L / isobaric tolerance**, so a near-miss is
  a real miss). Isoform suffixes (`-2`) fall back to the base accession.
- **Sequence coverage** — union of mapped peptide spans ÷ protein length.
- **Within-condition CV** — `SD / mean × 100` per (peptide, condition), on **raw linear**
  (de-linearized) intensities, requiring ≥3 non-missing replicates.
- **Per-sample depth** — count of quantified peptides per sample (`finite & ≠ 0`).
- **Missed cleavages / peptide length** per peptide.

## Volcano Plot

Requires both a **Statistics** result and a completed PELSA run for the ome.

- **x-axis** — `logFC` for the selected contrast.
- **y-axis** — `-log10(P.Value)` (raw p-value). Each point is one peptide.
- **Significance is two-sided**: a peptide is significant when `adj.P.Val < cutoff` —
  **dark red** when up (`logFC > 0`), **blue** when down (`logFC < 0`), **gray** otherwise.
  A dashed line marks the empirical raw-p threshold. The **cutoff is shared with the
  Statistics tab** (`Statistics → Summary`); it is not set here (default 0.05).

### Controls

- **Contrast** — choose which Statistics contrast to plot.
- **Find / highlight a protein** — type an accession and **Highlight**; a single match opens
  (pins) that protein, multiple matches light up in gold. **Clear selection & highlight**
  resets.
- **Color points by** — *Significance* (default) or *UniProt feature class* (hidden for
  self-curated datasets).
- **Label peptides** — *None* (default), *All marker peptides*, *All significant peptides*,
  *Best peptide per marker*, or *Top-N per protein* (with a count input). Labels render as
  `<gene>_aa<position>`.
- **Show best peptide per protein** — adds a second volcano with one point per protein's
  most significant peptide.

Marker proteins are always shown in **magenta** regardless of the color mode. The plot
renders all points via WebGL (`scattergl`) and falls back to SVG automatically if the
browser lacks WebGL support.

### Pinned protein views

**Left-click any point** to pin that peptide. This opens a metadata panel (accession, gene,
coverage, position, sequence, `logFC`, `adj.P.Val`) and two visualizations:

- **Intensity line plot** — one line per peptide occurrence; y = mean processed (log2)
  intensity per condition, x = condition (in your configured order). The pinned peptide's
  line is gold. For marker proteins, peptides are split into *significant* vs *non-significant*
  facets for the current contrast.
- **Coverage / feature / Woods panel** (shared residue axis):
  - **Coverage ruler** — protein backbone with peptide-covered residues in gold.
  - **Feature track** — UniProt features as colored, lane-packed segments by feature class.
  - **Woods plot** — each peptide as a horizontal segment from its start to end at y = `logFC`,
    colored by `-log10(adj.P.Val)`. This is the core PELSA readout: it shows *where* along the
    protein the treatment effect localizes relative to annotated structure/function.

Clicking a Woods peptide cross-selects it on the volcano. **Add accession to marker list**
sends the pinned protein to this ome's Setup marker list.

## Exports

PELSA exports are written under `<ome>/pelsa_exports/`:

- **Volcano** — one static PNG per contrast (`all_peptide_volcano_<contrast>.png`; plus a
  best-peptide volcano if that option is on). Coloring and labels follow the on-screen
  settings and the shared significance cutoff.
- **Intensity line** — one PNG per protein, split into `marker/` and `significant/`.
- **Woods** — one PNG per protein × contrast, split into `marker/` and `significant/`.
- **Setup inputs** — a verbatim copy of the uploaded FASTA, the annotation TSV (omitted for
  self-curated datasets), and `missing_accessions.txt` (accessions absent from the annotation
  file).

Figures are PNG at 300 DPI, re-derived from the analysis cache at export time.

## Feature classes

When you upload a feature annotation file, Protigy classifies every UniProt
sequence feature into one of eight **feature classes**, used to colour the
volcano and label peptides. A peptide that overlaps several features is assigned
the single highest-priority class among them.

| Feature class | Priority | Example UniProt features |
|---|---|---|
| active_or_binding_site | 1 (highest) | Active site, Binding site, Metal/Nucleotide binding, Site, DNA binding |
| catalytic_domain | 2 | Domain whose note names a catalytic activity (kinase, protease, helicase, methyltransferase, transferase, ATPase, dehydrogenase) |
| folded_domain | 3 | Any other Domain (including inhibitor/inactive/pseudo domains) |
| region_or_motif | 4 | Region, Motif |
| transmembrane_or_signal | 5 | Transmembrane, Signal, Topological domain, Intramembrane |
| repeat_or_coiled_coil | 6 | Repeat, Coiled coil |
| low_complexity_or_disorder | 7 | Compositional bias; a Region/Motif described as disordered or low-complexity |
| other | 8 (lowest) | Everything else (Helix, Beta strand, Modified residue, Natural variant, Chain, Disulfide bond, Glycosylation, Zinc finger, Cross-link, Mutagenesis, ...) |

**How a feature is classified.** The type of the feature is checked first; a few
classes also look at the description text:

- A **Domain** is `catalytic_domain` if its note names a catalytic activity, and
  `folded_domain` otherwise.
- A **Region** or **Motif** whose note says *disordered*, *low complexity*, or
  *compositionally biased* becomes `low_complexity_or_disorder`; otherwise it is
  `region_or_motif`.
- **Compositional bias** is always `low_complexity_or_disorder`.

**Deliberate differences from the analysis pipeline.** Protigy's classifier
matches the upstream PELSA pipeline exactly, with two intentional refinements:

- A catalytic keyword in a domain note only makes it `catalytic_domain` when the
  note does **not** also say *inhibitor* or *inactive* (so a "kinase inhibitor"
  domain is treated as a folded, non-catalytic domain).
- A *disordered* / *low complexity* mention in a description reclassifies a
  feature only for **Region/Motif** types, not for experimental annotations
  (Mutagenesis, Chain, Natural variant) or named Domains that merely mention a
  disordered region in passing.

Because of these two refinements, a small number of features may be coloured
differently in Protigy than in a pre-computed pipeline annotation.

**Note on scores.** Each class also carries a numeric *binder-likelihood* score
used only by sidecar analyses; the volcano colour and the single-class-per-peptide
label are decided by the **priority** order above, not by the score.

## Scientific notes and caveats

- **Fold change and p-values come from the Statistics tab**, not from PELSA. Define the
  contrast there and keep the significance cutoff consistent.
- **Exact-substring matching only** — there is no isobaric (I/L) tolerance. Peptides that do
  not match the supplied FASTA exactly are reported as unmatched in the Summary.
- **Feature overlap uses exact accessions** — an isoform-suffixed accession will not inherit
  a base accession's features (though it will match the base for sequence coverage and the
  failed-annotation count).
- **CV** is meaningful only with adequate replication (≥3 replicates per condition) and is
  computed on raw linear intensities.
- The annotation file format is **provisional**; generate it with the external UniProt-fetch
  workflow rather than hand-editing.
