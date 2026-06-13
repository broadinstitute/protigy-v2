# PELSA test fixtures

Synthetic peptide-level fixtures for testing the **all-R** PELSA compute helpers.

## Purpose

These fixtures exercise the PELSA QC compute helpers (CV, missed-cleavage
counting, FASTA peptide-position mapping, sequence coverage, and UniProt feature
annotation) without any dependency on real instrument data or external services.

## R-only parity model (firm project decision)

There is **NO Python in the committed test suite**. PELSA is implemented entirely
in R, and its tests verify the R implementation against ground truth that is
established in R:

- **Synthetic frames** are generated in R by `generate_synthetic.R` (added in
  Phase 1). Generation uses a **fixed seed**, so the frames are fully
  deterministic and reproducible across machines and CI runs.
- **Ground truth** is established one of two ways, both inside the test files
  themselves:
  - **Hand-computed / closed-form** in the test: e.g. CV via
    `sd / mean * 100`; missed-cleavage counts via the `[KR](?!P)` rule
    (cleavage after K or R unless followed by P); FASTA occurrence positions;
    and sequence coverage.
  - **Hand-set fixture coordinates**: e.g. UniProt feature annotation intervals
    are written by hand into the fixture so the interval-join logic can be
    checked against known answers.
- **No Python and no notebook capture is invoked by the test suite.** Nothing in
  `tests/` shells out to Python, executes a notebook, or reads notebook-captured
  output.

### Conceptual conversion target (not run by tests)

The original Python notebook (`PELSA_QC.20260609.ipynb`) remains the
**conceptual** conversion target: the R logic must reproduce the notebook's
logic. The notebook is **not** run by the tests — it is reference material only.

## Synthetic frame shape

Recorded from `dev/pelsa_benchmark/RESULTS.md`. The synthetic frame is a
**peptide-level** frame with the following characteristics:

- `;`-delimited multi-accession `PG.ProteinAccessions` (a peptide may map to
  several protein accessions).
- `;`-aligned `PG.Genes` and `PEP.PeptidePosition` (their `;`-delimited entries
  line up positionally with the accessions in `PG.ProteinAccessions`).
- A `PEP.StrippedSequence` column containing pure `[A-Z]` sequences (already
  mod-stripped by Spectronaut).
- **Shared peptides** across multiple accessions.
- Per-contrast `logFC` / `adj.P.Val` columns.
- Condition replicates.
- `NA` holes (missing values) in the intensity data.

## Real Spectronaut column reference

Confirmed from a real Spectronaut export. The key columns are:

- `PG.ProteinGroups`
- `PG.ProteinAccessions`
- `PG.Genes`
- `PEP.StrippedSequence`
- `PEP.PeptidePosition`
- Per-sample intensity columns named like `<condition>_R<n>` (e.g. `Ctrl_R1`,
  `Treat_R3`).
