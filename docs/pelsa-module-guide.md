# PELSA Module — A Guide for Researchers

> **Who this is for.** Biologists and proteomics researchers using the PELSA section of
> Protigy. It explains *what each part does and why*, in plain terms, while staying
> technically accurate. You do **not** need to read any code. A companion technical spec
> (`docs/pelsa-module-planning.md`) has the implementation details.

## What PELSA is

**PELSA** (Proteome-wide Effective-target Limited-proteolysis Stability Assay) is a
peptide-level assay for finding which proteins a drug binds. The logic:

- A protease (trypsin) chops proteins into peptides.
- Where a drug **binds and stabilizes** a region of a protein, that region is **protected**
  from cutting — so peptides from that region are produced **less**, and their measured
  intensity **goes down** in the treated sample.
- So a drug target shows up as peptides with a **decreased** intensity (a **negative
  log fold-change**) in treated-vs-control.

PELSA works at the **peptide** level (not protein level), so datasets are large — hundreds
of thousands of peptide rows. This module migrates an existing, validated Python analysis
notebook into Protigy so the same analysis runs with an interactive interface. The R code
was checked against the notebook so the numbers match.

## How the module is organized

You move through three tabs, left to right:

1. **Setup** — tell the app what to analyze and how, then press **Start Analysis**.
2. **Summary** — a dashboard of data-quality (QC) metrics for the analyzed data.
3. **Volcano Plot** — the main result: which peptides/proteins change, with drill-down.

A **dataset switcher bar** sits under the menu once you've analyzed more than one dataset,
so you pick a dataset once and it stays selected as you move between tabs.

A key idea: **all the heavy computation happens once, when you press Start Analysis.** The
results are stored, and the Summary and Volcano tabs just *display* those stored results —
nothing is recomputed as you click around, which keeps the app responsive on large data.

---

## A few concepts used throughout

These come up in several places, so they're explained once here.

### "Exploding" shared peptides

A peptide can belong to **several proteins** at once (it matches more than one protein's
sequence). Spectronaut records these as a semicolon-separated list, e.g.
`P12345;Q99999;A0A...`. Whether the app treats that peptide as **one** thing or splits it
into **one copy per protein** ("exploding") depends on the question:

- **Counting how many peptides were identified** — *no explode*. A peptide shared by three
  proteins is **one** peptide ID.
- **Per-protein sequence coverage, best-peptide-per-protein, feature annotation** — *explode*.
  The shared peptide **contributes to every protein it belongs to**, so each protein's
  coverage/representative-peptide accounts for it.

This split matches the reference notebook and is applied consistently.

### Peptide position mapping (where a peptide sits in its protein)

To label a peptide by *where* it lies in its protein (e.g. `MTOR_aa1483`), the app finds the
peptide's sequence **inside the protein's full sequence (FASTA)** and reads off the residue
position. It uses the **same FASTA file that was used for the Spectronaut search**, so
essentially every peptide maps. A peptide that occurs **twice** in a protein gets **two**
positions (two labels / two lines on the intensity plot). Peptides that can't be mapped are
recorded in a troubleshooting table (see Summary → QC) so you can spot a wrong-species/FASTA
mistake. (Leucine/Isoleucine are indistinguishable by mass, so the app retries a failed
match with those treated as equal — a normal, benign case.)

### Two different "data worlds"

Two kinds of numbers are used, deliberately, and should not be conflated:

- **Reproducibility / QC (the CV metric)** uses the **raw, un-logged intensities**, after a
  **sum-normalization** step that corrects for differences in total signal between samples.
- **Significance (the volcano)** uses the **log2, normalized** values and the **moderated
  statistics from Protigy's Statistics tab** (the same limma-based test used elsewhere in
  the app). PELSA does **not** invent its own significance test.

---

## The Setup tab — what you configure

Setup is where you describe the experiment. Most controls are shared across datasets;
the condition/replicate parts are **per dataset** because two datasets can be organized
differently.

| Control | What it's for |
|---|---|
| **Datasets to analyze** | Tick which uploaded datasets to run PELSA on. |
| **Species** | Picks the protein-sequence database (FASTA) + annotation library for position mapping and feature coloring. The list is read live from the installed species folders (human, mouse, …). |
| **Treatment compound** | Pick the drug; this **auto-fills** a starter list of known marker proteins for that compound (e.g. Rapamycin → MTOR, FKBP1A). You can edit the list afterward. |
| **Marker proteins** | The proteins you want highlighted on the volcano (your expected targets). Paste accessions or gene symbols (any mix of spaces/commas/semicolons/newlines); they appear in an editable table. Markers are an **overlay**, not required to run. |
| **Condition grouping column** | Which experimental-design column defines your conditions (e.g. dose groups). |
| **Replicate identifier column** | Which column orders the replicates. |
| **Condition & replicate ordering** | Drag to set the order conditions and replicates appear in every plot. "Reset to default" restores the natural order; there's a keyboard option too. |
| **Apply the same setup to all datasets** | Copies one dataset's condition/replicate choices to the others. |
| **Refresh UniProt annotation library** | A maintenance button: re-downloads the protein feature annotations for chosen species. Safe — it **merges** new data over the existing library (a partial download can never erase previously-saved annotations) and writes atomically (an interrupted refresh leaves the old library intact). |
| **Start Analysis** | Runs everything. It first checks your setup (≥1 dataset, a condition column per dataset, an order, a species FASTA present) and shows a clear message for anything missing, then computes with a progress bar. |

**Inputs:** your uploaded datasets + the species FASTA + the compound/marker choices.
**Output of Start Analysis:** a stored per-dataset *analysis bundle* (peptide positions,
CV table, coverage, depth, missed-cleavage counts, feature annotations, QC counts) that the
Summary and Volcano tabs read.

---

## The Summary tab — data-quality dashboard

Reads the stored analysis bundle and shows QC for the active dataset. Nothing here is
recomputed; it's all from Start Analysis.

**Experiment-wide**
- **Total peptide IDs** — how many peptides were identified (shared peptides counted once).
- **Per-protein sequence coverage** — what fraction of each protein's sequence is covered by
  its mapped peptides (overlapping peptides counted once, not double-counted).
- **Peptide-length distribution** — a density curve with the **mean and median** marked.
  A typical tryptic peak sits around 9–14 amino acids.
- **Missed-cleavage distribution** — how many peptides have 0, 1, 2… missed tryptic cuts.
  This is **primary digestion QC for a limited-proteolysis assay**: PELSA's whole signal is
  differential cutting, so a shifted missed-cleavage profile (under-digestion) would confound
  the interpretation. The count uses the standard trypsin rule (cut after K or R, but not
  before proline).

**Per-condition reproducibility (CV)**
- A density curve of the **coefficient of variation** per condition — how reproducible the
  replicates are. Computed on **sum-normalized raw intensities** (so it reflects signal
  reproducibility, not loading differences), with a median line per condition. Conditions
  with too few measurements to be meaningful are skipped with a note.

**Per-sample depth**
- A bar per sample = how many peptides were quantified in it, in your chosen sample order,
  with a small table of mean/median and the CV of those counts.

**Mapping / annotation QC (collapsible, at the bottom)**
- Counts + drill-down tables of peptides that **couldn't be mapped** to a protein sequence,
  and proteins that **couldn't be annotated**. These are your tripwires: a large unmatched
  count usually means a wrong species/FASTA was selected. Exportable as CSV.

**Inputs:** the analysis bundle. **Outputs:** on-screen metrics + CSV exports.

---

## The Volcano Plot tab — the main result

A volcano plot puts **fold-change (left/right)** against **statistical significance
(up/down)**, one point per peptide. This is where you read out drug engagement.

**Before it works:** you must have run a **two-sample statistical test in Protigy's
Statistics tab** for the dataset (PELSA reuses those results — it doesn't compute its own).
If you haven't, the tab greys out with a message telling you what to do. (You also need to
have run Start Analysis.)

**Contrast selector** — pick which comparison (e.g. Treated vs DMSO) to view. Only the
selected one is drawn (kept fast); your marker list and settings carry over as you switch.

**The plot**
- One dot per peptide, all points drawn (the plot uses GPU rendering to stay smooth even
  with 100k+ points).
- **Coloring** — a single toggle:
  - *By significance* (default): **two-sided** — significantly **down** peptides (the
    stabilization/engagement signal) and significantly **up** peptides are both colored,
    non-significant ones grey.
  - *By UniProt feature class*: colors each peptide by the kind of protein region it falls in
    (active/binding site, catalytic domain, transmembrane, disordered, etc.), so you can see
    *where* in the protein the changes land.
- **Marker proteins** — peptides from your marker proteins are always drawn on top in
  **magenta**, so you can immediately see whether your expected targets moved (matching
  works across protein isoforms automatically).
- **Labels** — peptides are labeled `<gene>_aa<position>`. You choose: label all marker
  peptides, only the best peptide per marker protein, or the top few most-significant per
  protein.
- **Best peptide per protein** (optional second panel) — collapses to one representative
  (most-significant) peptide per protein, useful for a protein-level overview. *Note: this is
  representative-peptide selection for display, not a formal protein-level statistical test.*

**Click a point to pin it** — clicking a peptide pins a side panel showing that protein's
**intensity across conditions**: one line per peptide, so you can see the dose/treatment
trend and confirm the engagement pattern. For marker proteins this splits into
significantly-changed vs other peptides.

**Inputs:** the analysis bundle + the Statistics-tab results. **Outputs:** the interactive
plots + CSV/PDF exports (the volcano figure, your marker list, a per-peptide label table,
and the plotted intensity values).

---

## Under the hood — the building blocks (plain-language)

The analysis is built from small, independently-tested functions. Each was verified to
reproduce the reference notebook on synthetic test data with known answers (no live internet
needed for testing). Grouped by job:

| Building block | What it does | In / Out |
|---|---|---|
| **Accession explode** | Splits a shared peptide into one row per protein it belongs to, keeping gene/position aligned. | peptide table → long per-(peptide,protein) table |
| **FASTA position mapping** | Finds each peptide inside its protein's sequence; records the residue position(s); lists peptides that don't map. | peptides + protein sequences → positions + an "unmatched" table |
| **Missed-cleavage / peptide-length** | Counts missed tryptic cuts and peptide lengths; builds the `<gene>_aa<pos>` labels. | peptide sequences → counts / lengths / labels |
| **Within-condition CV** | Sum-normalizes raw intensities, then computes reproducibility per peptide per condition. | raw intensities + condition map → CV table |
| **Per-sample depth** | Counts quantified peptides per sample (+ summary stats). | processed intensities → per-sample counts |
| **Sequence coverage** | Per protein, the fraction of its sequence covered by its peptides. | mapped peptides + sequences → coverage per protein |
| **Best-peptide rollup** | Picks the single most-significant (and most-stabilized, on ties) peptide per protein. | peptides + stats → one representative per protein |
| **UniProt fetch + classifier** | Downloads protein feature annotations and sorts each feature into one of 9 region classes. | accessions → feature table |
| **Feature annotation** | For each peptide, finds which protein region(s) it overlaps and picks the most informative one. | peptide spans + features → a region class per peptide |
| **Marker matching** | Decides which peptides belong to your marker proteins (isoform-aware). | accessions + markers → marker flags |
| **Volcano data builder** | Assembles the per-peptide volcano table (fold-change, significance, color, label, marker flag). | analysis bundle + stats → volcano table |
| **Intensity-line data** | Builds the per-protein intensity-across-conditions lines for the pinned panel. | analysis bundle + processed intensities → line data |

**Start Analysis** runs these in order, once, and stores the result; **Summary** and
**Volcano** read that store.

---

## Things worth knowing (gotchas)

- **Run a Statistics-tab test first** — the volcano needs it; PELSA reuses those significance
  values rather than computing its own.
- **Use the matching species/FASTA** — if peptides don't map (Summary QC shows a high
  unmatched count), the most common cause is the wrong species selected.
- **CV and the volcano are different number worlds** — CV is on sum-normalized raw values
  (reproducibility); the volcano is on the log2 normalized statistical results. Don't compare
  them directly.
- **The "best peptide" view is for display, not protein-level statistics** — it picks a
  representative peptide; it is not an FDR-controlled protein call.
- **Refreshing the annotation library is safe** — it merges and writes atomically, so a
  flaky download can't corrupt or erase your existing annotations.

---

*Generated as part of the PELSA module implementation. For implementation detail and the
record of intentional differences from the reference notebook, see
`docs/pelsa-module-planning.md`.*
