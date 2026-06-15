# PELSA Module Functionality Planning Doc

Reference notebook: '/Users/cameronlian/Library/Application Support/Mountain Duck/Volumes.noindex/proteomics_storage_vast.localized/PELSA/PELSA_data_pipeline/PELSA_QC.20260609.ipynb' from '/Users/cameronlian/Library/Application Support/Mountain Duck/Volumes.noindex/proteomics_storage_vast.localized/PELSA/PELSA_data_pipeline' - this is an existing notebook where all of our PELSA analysis are being done, and we want to migrate it into Protigy for better UI/UX.

> **Refinement status:** This pass refines only the sections that already carried implementation
> detail — **Summary** and **Volcano Plot** — plus light structural treatment of **Setup** and an
> **Execution architecture (R vs Python)** decision section. **Wood's Plot** remains a stub. The
> notebook is treated as ground truth throughout; every place the prior wording conflicted with the
> notebook is flagged inline with a `> ⚠ Divergence from original doc:` callout, and architectural
> recommendations are flagged with `> 💡 Recommendation:` so they are easy to scan and approve.

## Performance notes

- Be mindful of resource overhead since PELSA analysis always handles peptide-level dataset, so the amount of data is large, therefore you have to be mindful on resource managment, for example: when entering the volcano plot section, only load the volcano plot of the active contrast being selected by the selector instead of loading all volcano plots in the background, and when the active contrast being switched, clear the previous contrast stat and its volcano plot from RAM to preserve space before loading the next volcano plot (however, for marker protein peptide labeling purposes, necessary information such as volcano customization settings and marker protein list should be retained between switching contrasts)
- And since the reference notebook is written in python, write the PELSA section in python as well - if you are not sure, fetch for information online to to do some research about having both R and python scripts in one shiny app, optimization and performance of python scritps in R
  - If you are okay to use R for volcano plot visualization in-app (plotly), it's okay to use R for that since we have already had an exisitng module for volcano plotting
  - Whenever you are converting the existing python script to R, you need to do tests to ensure the language conversion doesn't alter the results

> **Decision (2026-06-12):** the whole PELSA section is built in **R** — no Python at app runtime,
> no `reticulate`. The notebook's Python logic is **re-ported to R and parity-tested against the
> notebook**, which is the **gold standard**: for any discrepancy, the R code is changed to match the
> notebook, never the reverse. See [Execution architecture: all-R port](#execution-architecture-all-r-port).

## Additional input required for PELSA data
- Species: there will be a list of predefined species (for now, list human and mouse - there's possibility for adding more species in the future)
  - each species' fasta file will be stored in, for example: /Users/cameronlian/git/protigy-v2/inst/database/human
  - And when the user starts the analysis:
    - For newly imported database, fetch protein feature annotations and member annotations from UniProt as done in the reference notebook, and store those information in /Users/cameronlian/git/protigy-v2/inst/database/human/uniprot_features and /Users/cameronlian/git/protigy-v2/inst/database/human/uniprot_membrane subfolders (if not created, autogenerate these folders)
    - For already imported databsae with already generated annotation files, use the existing annotation files and top up with additional fetches if an accession was not included in the existing annotation library file
    - See reference notebook for additional details on the fetching and organization
- Treatment compounds: to be selected using a dropdown menu (e.g., Rapamycin, U-18666A, AY9944, etc.) - each compound will be linked with a list of predefined marker proteins for autofillnig the next field
- Marker proteins: here all marker proteins (either by autofilling or user input) will be listed in an reactive table list (2 columns: Accession number, Gene Symbol)
  - I'm planning to have an text input field where the user can enter either one or multiple protein accessions or gene symbols either separated by space or comma or semicolons or by having a protein every new line, etc.
  - If the user entered an accession number, the gene symbol for this accession number will be fetched from UniProt, and the marker protein will be listed in the reactive table with their gene symbol autofilled
  - If the user entered a gene symbol, since a gene symbol can map to multiple accession numbers, the user will be prompted to select the correct accession number from a list of options (this could be multi-selection so the user can select both canonical and non-canonical form) - in the list of accession numbers given to the user, the canonical version should be highlighted in a form or by a superscript marker, and UniProt-reviewed version should be highlighted using a marker, assuming the rest of accessions are un-reviewed from UniProt - once the user selected the protein accessions, these protein accessions will be filled into the reactive table with their gene name paired in the table
  - there needs to be a button for the user to remove selected marker proteins or "Clear All" to start fresh
- Condition grouping, which can be inferred from the experimental setup file, but the user has to choose which column from the experimental setup file they'd like to use for grouping replicates (this will be a dropdown selector)
- Replicate identifier: in addition to the selection of condition grouping identifier, the user will need to choose the column that they wanted to use as replicate identifier --> Replicate ranking
- Condition ordering: after selecting the condition grouping identifier, this will be shown as a list of movable blocks of each of all conditions, and the user can rank the conditions to their preference by drag and drop the blocks
- Replicate ordering: after condition order is confirmed, under each condition, there are multiple replicates showing as the movable blocks like the previous view for condition ordering, these replicates will be ordered automatically by the replicate identifier that the user has chosen, but the user can further optimize the ordering by drag & drop

## A few key notes

- **Peptide position mapping is FASTA-substring-based (monkey-patched style — see spec below).** Peptide
  positions are computed by **mapping the identified peptide sequence to the parent protein's FASTA
  sequence**, NOT from Spectronaut's `PEP.PeptidePosition`. This `pep_start` feeds the intensity-line
  `aa<pos>` labels and the volcano `<gene>_aa<pos>` marker labels. Full spec: see
  [Peptide position mapping (FASTA)](#peptide-position-mapping-fasta) below.
- **Built entirely in R** (final decision, 2026-06-12 — no Python at runtime, no `reticulate`). Because
  PELSA data is peptide-scale (100k+ rows), **every R function follows the vectorization / performance
  rules** in [Execution architecture: all-R port](#execution-architecture-all-r-port) → *Performance
  design rules*. The reference notebook stays the gold standard; converted helpers are parity-tested
  against the notebook on a shared **synthetic dataset** (see the Parity-test gate).

## Peptide position mapping (FASTA)

**Decision (C2, 2026-06-12): re-port the notebook's monkey-patched FASTA position mapping**
(`explode_peptide_rows_fasta`, notebook cell 11), NOT the plain `peptides.py::explode_peptide_rows`
(which uses Spectronaut's `PEP.PeptidePosition` token). This is the source of `pep_start` for every
`aa<pos>` label in the module.

**Why monkey-patched:** the notebook replaces the default exploder at runtime with a FASTA-substring
version because Spectronaut's reported position is unreliable / single-valued, while PELSA needs the
*true* residue position(s) of the peptide within each parent protein's sequence — including **every
occurrence** when a peptide appears more than once in a protein.

**Algorithm to re-port (R):**
1. **Build an `{accession → FASTA sequence}` map** for the selected species from
   `inst/database/<species>/fasta/…` (one-time per run; parse with a lightweight FASTA reader). **The
   per-species FASTA is assumed present** at this path (human is in place; mouse + future species are
   dropped in by the user before that species is used). If the FASTA is missing for the selected species,
   fail with a clear Setup-level error ("No FASTA for <species> — add it to inst/database/<species>/fasta/").
   **Assumption (load-bearing): the species FASTA here is the SAME FASTA used for the dataset's
   Spectronaut search**, so essentially every identified peptide maps and FASTA drops are rare. This is
   why downstream metrics (position labels, coverage) safely use *only* FASTA-mapped peptides with no
   Spectronaut-position fallback; the "failed to match FASTA" QC count is the tripwire if a mismatched
   FASTA/species is selected.
2. **Explode** each peptide's `;`-delimited `PG.ProteinAccessions` to (peptide, accession) pairs,
   position/gene aligned (this part is the cheap `tidyr::unnest`).
3. **Normalize the peptide sequence to bare amino acids first (required — prevents silent drops).**
   `stri_locate_all_fixed` is an **exact** substring match, so it returns nothing if the sequence string
   carries anything other than `[A-Z]` residues. Before matching: **assert/strip the sequence to a clean
   AA backbone** — remove modification tokens if present (`M(ox)`, `C[+57.0]`, lowercase mods, N-term
   `n[42]`, etc.). If a sequence still isn't pure `[A-Z]` after stripping, route it to the unmatched table
   with reason `bad_sequence_format` (don't let it silently fail to match). *(The exact strip rule depends
   on the Spectronaut export's sequence-column format — confirm at build time.)*
4. **For each (peptide, accession) pair, substring-search** the cleaned peptide against that accession's
   FASTA sequence (`stringi::stri_locate_all_fixed`). For **each match**, emit a row with `pep_start` =
   match start (1-based residue), `pep_end` = start + nchar − 1, plus `pep_occurrence_idx` and
   `n_occurrences`. **One row per occurrence** (a peptide occurring twice in a protein → two rows / two
   lines on the intensity plot).
   - **I/L isobaric retry:** Leu/Ile are mass-indistinguishable, so a reported sequence may not
     substring-match the FASTA's other assignment **even with the correct FASTA**. For a peptide that
     fails the exact match, **retry once with both peptide and FASTA I→L-normalized** (replace all `I`
     with `L` on both sides) before declaring it unmatched. Cheap, vectorizable, removes the most common
     benign non-match.
5. **Drop pairs** where the accession is absent from the FASTA, or the peptide is still not found after
   the I/L retry (mirror the notebook — these contribute no position and no `aa<pos>` line).
   **Capture every dropped (peptide, accession) pair into a side `unmatched` table** with columns
   `peptide_sequence`, `accession`, `gene`, **`pep_position`** (the `;`-aligned Spectronaut
   `PEP.PeptidePosition` token for that peptide×accession — there is no FASTA position since mapping
   failed, so the reported value is carried for troubleshooting), and **`reason`**
   (`accession_absent` | `sequence_not_found` | `bad_sequence_format`). The Summary's collapsible
   "peptides failed to match" table reads this; the reason lets a genuine wrong-FASTA/species problem
   (`accession_absent` / `sequence_not_found` dominating) be distinguished from benign formatting drops.
6. This produces the `exploded_cache` equivalent that feeds (a) intensity-line `aa<pos>` labels and
   (b) the annotation overlap join's peptide spans. The mapper **returns both** the matched cache **and**
   the `unmatched` table (the Summary QC metric reads the latter).

**Multi-occurrence / multi-protein → single volcano dot, multiple labels (RESOLVED 2026-06-12).** A
peptide row is **one dot** on the volcano but may map to multiple accessions and/or occur at multiple
FASTA positions. The dot gets **one or more `<gene>_aa<pos>` labels**, derived as:
1. Build the candidate label set = every `(gene, pep_start)` across all the peptide's
   accession×occurrence mappings (gene from the `;`-aligned `PG.Genes`; `pep_start` from the FASTA mapper).
2. **Collapse fully-identical `(gene, pep_start)` pairs** to a single label — e.g. the same gene at the
   same position contributed by two accessions → **one** label.
3. **Keep distinct pairs separate**, including the **same gene at different positions** (e.g.
   `GENEA_aa120` *and* `GENEA_aa130` both kept) and different genes (`GENEA_aa120`, `GENEB_aa88`) — these
   become separate `;`-joined entries (step 6).
4. **Gene fallback:** when a gene token is empty, use `<accession>_aa<pos>` for that label (consistent
   with the rest of the plan's `;`-alignment fallback rule).
5. **No cap** — include **all** distinct labels, however many (a peptide shared across many proteins will
   carry many labels; accepted per user decision).
6. **Join into a single label string with semicolons**, in `PG.ProteinAccessions` token order:
   `<geneA>_aa<posA>;<geneB>_aa<posB>;…`. The dot carries **one** text label that is the `;`-joined list
   of the distinct `<gene>_aa<pos>` entries (not multiple separate label annotations). Example: a peptide
   mapping to GENEA at aa120 and GENEB at aa88 → label text `GENEA_aa120;GENEB_aa88`. A single-protein
   peptide is just `GENEA_aa120` (no semicolon).

The **intensity-line plot** still draws **all** occurrences as separate `aa<pos>` lines (unchanged) —
the collapse + `;`-join above applies to the *volcano dot label string* only, not to the line-plot
occurrences.

**Parity risk: Moderate–High** (the re-port table reflects this). Required synthetic fixtures:
- a peptide occurring **≥2×** in one protein (assert two rows, correct `pep_occurrence_idx`/`n_occurrences`);
- a peptide **absent** from its annotated accession's FASTA (assert the pair is dropped);
- an **isoform** accession (`P12345-2`) — confirm FASTA-key resolution / base fallback;
- overlapping-repeat peptides (e.g. `AAA` in `AAAA`) — confirm the occurrence stepping (`pos = i+1`);
- a **shared peptide** (`A;B;C`) — assert it counts as **1** peptide ID (no explode) **and** contributes
  to all of A/B/C's **sequence coverage** (explode); confirms the explode-or-not split (see
  *Peptide explosion strategy*).

> ⚠ Two distinct `pep_start` definitions exist in the notebook (Bio review X2): the **FASTA** one above
> (in `exploded_cache`) vs a **leading-Spectronaut-token** one inside `prep_plot_df` (used internally,
> then overwritten by `winning_gene`). The R port must use the **FASTA** `pep_start` for all `aa<pos>`
> labels and ignore the Spectronaut-token variant.

## Peptide explosion strategy

A peptide can map to multiple protein groups (`PG.ProteinAccessions = "A;B;C"`, with `;`-aligned
`PG.Genes` / position tokens). **Whether to "explode" that peptide into one row per parent protein
depends on the metric.** This section is the single source of truth for explode-or-not across PELSA.

### How the reference notebook does it (volcano)

The notebook renders **two volcano frames** from two helpers (`volcano_frames.py`):

- **All-peptide frame (`prep_plot_df`) — NO explode.** One dot per source peptide row (one
  `stat_results` row). A peptide mapping to `A;B;C` is **one dot**. Color resolves across *all* `;`
  accession tokens (multi-protein feature-priority resolution → winning class), but the dot is singular.
  *(The notebook labels this dot with the **leading** gene + leading position only.* **Protigy differs
  here** — see the Protigy table row below: keep the one-dot-per-peptide model, but **label that dot with
  ALL of its mapped `<gene>_aa<pos>`, `;`-joined into one label string** (`geneA_aa..;geneB_aa..`),
  collapsing identical `<gene>_aa<pos>`. So `A;B;C` → one dot whose label is the `;`-joined list of its
  distinct gene+position entries. This is the all-peptide multi-label rule defined in
  *Peptide position mapping (FASTA)*.)
- **Best-peptide frame (`make_protein_level_df`) — WITH explode.** Explode `A;B;C` → one row per
  (peptide, accession) with `;`-aligned position/gene; within each **accession** group keep the smallest
  `adj.P.Val` (tie-break most-negative `logFC`); **one dot per protein**. A shared peptide that is "best"
  for both A and B yields **two dots**.
- **Best-peptide deciding** = that per-accession `sort([adj.P.Val, logFC]) → drop_duplicates(accession,
  keep="first")`.

### How Protigy does it (decisions 2026-06-12)

| Operation | Explode? | Rule |
|-----------|----------|------|
| **Count # peptides identified** (Total peptide IDs) | **No** | Count original GCT rows; `A;B;C` = 1 peptide. |
| **Per-protein sequence coverage** | **Yes** | Explode to every parent; union of `[pep_start,pep_end]` ÷ protein FASTA length, per accession. |
| **Volcano — all-peptide panel (default)** | **No** | One dot per peptide. Color = multi-protein priority resolution across all tokens. **Label = the multi-label rule** (all distinct `<gene>_aa<pos>`, collapse identical `(gene,pos)`, keep same-gene/diff-pos separate, accession fallback, no cap, **`;`-joined into one label string** `geneA_aa..;geneB_aa..` — see *Peptide position mapping (FASTA)*). *(Richer than the notebook's leading-token-only label.)* |
| **Volcano — best-peptide panel (toggle, default off)** | **Yes (then regroup by peptide)** | Explode → per-accession best peptide (`sort([adj.P.Val, logFC])` → first per accession, deterministic tiebreak). **Then draw ONE dot per distinct best-peptide** (a peptide has one `(adj.P.Val,logFC)`, so a peptide best for GENEA+GENEB is **one** dot, not two overlapping ones) and **label it with one `<gene>_aa<pos>` per won protein, `;`-joined** (collapse identical). *(Refines the notebook's per-accession-dot model.)* |
| **Best-peptide deciding / rollup** | **Yes** | As above; this is the `_rollup_to_proteins` equivalent (also feeds intensity-line protein selection). |
| **Marker highlighting** | n/a (matches across tokens) | Isoform-base, any-token, case-insensitive (see the *isoform-aware marker matching* bullet under Volcano → New for PELSA). |

**Net:** the all-peptide panel and peptide counts treat a shared peptide as **one** entity (notebook
parity); coverage, the best-peptide panel, and the rollup **explode** to per-protein (notebook parity),
with Protigy adding (a) richer multi-labels on the all-peptide dot and (b) `(gene,pos)` dot-collapsing on
the best-peptide panel to avoid fully-redundant dots. Parity tests cover both a non-exploded count and an
exploded coverage/rollup on the same synthetic shared-peptide fixture.

## Module structure

The four sub-sections map onto the already-scaffolded nested module functions. The scaffold currently
ships generic "Section 1/2/3" names; renaming them to the real section names is a *future code step*,
noted here so the plan and the code line up — do not rename anything as part of this planning pass.

| Sub-tab (this doc)   | `app_ui.R` navbarMenu `tabPanel` | Scaffold module pair (`R/tab_pelsa_section*.R`)              | Notes |
|----------------------|----------------------------------|--------------------------------------------------------------|-------|
| **Setup**            | `"Section 1"` → rename `"Setup"`         | `PELSASection1_Tab_{UI,Server}` + `PELSASection1_Ome_{UI,Server}` | **Per-dataset** config + "apply to all" checkbox (Decision #7); shared species/compound/markers. Hosts the **dataset multi-select** + the species-refresh control. |
| **Summary**          | `"Section 2"` → rename `"Summary"`       | `PELSASection2_Tab_{UI,Server}` + `PELSASection2_Ome_{UI,Server}` | Renders for the **active dataset** from the shared switcher (not its own per-ome tabset). |
| **Volcano Plot**     | `"Section 3"` → rename `"Volcano Plot"`  | `PELSASection3_Tab_{UI,Server}` + `PELSASection3_Ome_{UI,Server}` | Reuses `tab_stat_plot.R` machinery (below); active-dataset-driven. |
| **Wood's Plot**      | *(new 4th `tabPanel` to add)*            | *(new `tab_pelsa_section4.R`, or rename plan)*               | **STUB — not designed in this pass.** |

- The scaffold's `*_Tab_Server` already wires `GCTs_and_params`, `globals`, `GCTs_original` and builds
  a per-ome tabset via `add_css_attributes(... "box-with-tabs")`, exactly like `statPlot_Tab_Server`.
  Each `*_Ome_Server` receives `GCT_processed`, `parameters`, `default_annotation_column`, `color_map`.
- All four `*_Tab_Server` calls are already registered in `R/app_server.R` and their exports gathered
  into `all_exports` (`PELSASection{1,2,3}_exports`); the export pattern from
  `dev/module_requirements.md` (nested `[[ome]] -> list(name = export_fn(dir_name))`) applies unchanged.
- The placeholder UI helper `pelsa_placeholder_box()` in `R/tab_pelsa_helpers.R` should be replaced
  section-by-section; keep new pure/computation helpers in `tab_pelsa_helpers.R` (or section-specific
  `tab_pelsa_section*_helpers.R`) and out of the module servers so they stay unit-testable.

> ⚠ Divergence from original doc + scaffold: (1) The scaffold has **three** sections but the doc defines
> **four** sub-tabs (Setup, Summary, Volcano Plot, Wood's Plot) — a fourth `tabPanel` + module pair is
> needed for Wood's Plot. (2) **Setup is per-dataset (per-ome) with an "apply to all datasets" checkbox**
> (Decision #7): shared inputs (species, compound, markers) plus per-dataset condition/replicate columns +
> ordering. (3) **Summary and Volcano no longer build their own per-ome tabset** — they render for the
> **active dataset** chosen in the shared PELSA dataset switcher (below the navbar; see *Dataset selection
> & switching*). This replaces the scaffold's per-section `tabsetPanel(... ome_tabs ...)` with a single
> app-level switcher that all sections share. *(Eng review X8: thread the active dataset as a reactive
> from a parent PELSA container module; and export functions must recompute **all** analyzed datasets, not
> just the active one.)*

---

## Setup section

> Light structural treatment only — this enumerates the controls and the ordering mechanism; it does
> not invent the full UniProt-fetch / marker-resolution implementation (that follows the notebook's
> `pelsa_qc_helpers.uniprot_features` and the existing `org.Hs.eg.db`/`org.Mm.eg.db` mapping in
> `R/sidebar_setup_helpers_GCT-processing.R`).

This section houses every control from "Additional input required for PELSA data" plus a **"Start
Analysis"** `actionButton` that kicks off the pipeline. Concrete control mapping:

| Input (from the enumeration above) | Shiny control | Behaviour notes |
|------------------------------------|---------------|-----------------|
| **Datasets to analyze**            | `checkboxGroupInput("pelsa_datasets", choices = <uploaded omes>)` (multi-select) | **First control.** Choices = the omes/datasets the user already uploaded into Protigy (`names(GCTs_and_params()$GCTs)`). The rest of Setup + the whole analysis run only for the checked datasets. See **Dataset selection & switching** below. |
| Species                            | `selectInput("pelsa_species", choices = <db subfolders>)` | Choices = the species subfolders found in `inst/database/` (human, mouse, …), **read live** so a newly added species appears without an app restart. Resolves to `inst/database/<species>/` (`fasta/`, `uniprot_features/`, `uniprot_membrane/`). |
| **Refresh UniProt annotation library** | `checkboxGroupInput("pelsa_refresh_species", choices = <db subfolders>)` + `actionButton("pelsa_refresh_btn", "Refresh per-species UniProt annotation library")` | **Decision #6.** Checklist of species subfolders (live-read each time Setup opens). Clicking the button (re)fetches + rebuilds the checked species' `uniprot_features/` + `uniprot_membrane/` via the R `httr2` fetch + `data.table` annotation port, with a **progress bar** (`withProgress`/`shiny::Progress` — fetches can take minutes). Independent of Start-Analysis. |
| Treatment compound                 | `selectInput("pelsa_compound", choices = names(compound_markers$compounds))` | Choices + preset markers come from **`inst/pelsa/compound_markers.yaml`** (read via `yaml::read_yaml`, re-read when Setup opens so user edits show without a restart). Selecting a compound autofills the marker table with that compound's `markers:` (accession + gene). Ships with Rapamycin / AY9944 / U-18666A; users extend the YAML freely (no code change). |
| Marker-protein paste box           | `textAreaInput("pelsa_marker_input")` + `actionButton("pelsa_add_markers")` | Parse mixed delimiters (space / comma / semicolon / newline) — reuse `parse_protein_search_input()` from the volcano module. |
| Marker reactive table              | `DT::DTOutput("pelsa_marker_table")` (Accession, Gene Symbol) | Accession-entry → fetch gene from UniProt; gene-entry → prompt accession choice (canonical superscript + reviewed badge, multi-select). Remove-selected + **Clear All** buttons. |
| Condition grouping column          | `selectInput("pelsa_condition_col", choices = <rdesc cols>)` | Choices come from the experimental-setup columns (GCT `cdesc`). |
| Replicate identifier column        | `selectInput("pelsa_replicate_col", choices = <rdesc cols>)` | Drives the *initial* replicate sort. |
| **Condition ordering**             | `shinyjqui::orderInput("pelsa_condition_order", items = <conditions>)` | Drag-drop reorderable list; chosen order returns in `input$pelsa_condition_order`. |
| **Replicate ordering**             | One `shinyjqui::orderInput("pelsa_replicate_order_<cond>", ...)` **per condition**, wrapped per the UX refinements below | Pre-seed each list from the replicate-identifier column; user fine-tunes by drag. Keep per-condition lists independent (no `connect=`). |

- `shinyjqui` is already a dependency and already used in-app (`R/tab_multi-ome_heatmap_options.R`), so
  `orderInput` is a proven mechanism here — no new dependency. `orderInput(inputId, label, items,
  as_source, connect, item_class, placeholder)` returns the current order in `input$<id>`.
- The confirmed **condition order** and **per-condition replicate order** together define the canonical
  `sample_order` that every downstream plot (Summary bars, intensity line plots, contrast axes) must
  respect. Persist this object (e.g. in the module's return value or a `globals` slot) so Summary and
  Volcano can consume it.

**Replicate-ordering UX refinements (Decision #8, per UI/UX review X10/A1):**
- Add a **"Reset to default order"** button for both the condition list and each replicate list (the
  default = the replicate-identifier sort), so a user can always recover after dragging.
- Wrap each condition's replicate list in a **bordered, scroll-contained card** with the condition name
  as header (keeps a tall stack manageable for dose series / many conditions).
- **Collapse single-replicate conditions to a static label** — no drag widget for one item.
- Provide a **keyboard-accessible fallback** (numeric rank inputs, or up/down buttons per item):
  `orderInput` drag has **no keyboard path**, so drag-only would lock out keyboard users.
- Re-use the **observer-dedup registry pattern** from `tab_stat_plot.R` (the per-item remove-button
  observers) when (re)rendering these dynamic per-condition inputs, to avoid leaking observers on every
  dataset switch.

**Per-ome Setup scope (Decision #7):** Setup renders **per selected dataset** — each gets its own
condition/replicate column selectors and ordering (two datasets can have different experimental-setup
columns). Add an **"Apply the same setup to all datasets"** checkbox that copies one dataset's
condition/replicate config to every selected dataset (mirrors Protigy's existing "Apply settings to all
datasets" sidebar pattern). Species, compound, and the marker list are naturally **shared** across
datasets; the per-dataset part is the condition/replicate columns + ordering. *(This supersedes the
earlier "Setup is a single experiment-wide panel" note.)*

**Start-Analysis validation + progress (UX review X10).** The **Start Analysis** button must gate on a
pre-flight **validation checklist** (≥1 dataset checked; a condition column chosen per dataset; condition
order confirmed) and render specific inline errors for what's missing rather than crashing or no-op-ing.
An **empty marker table is valid** (markers are an overlay, not a prerequisite). On launch: **disable the
button**, show **staged `withProgress`** feedback ("Fetching UniProt annotations 142/600…",
"Computing CV…", "Building contrasts…"), and offer **cancel** during the fetch phase. The minutes-long
UniProt fetch runs off the reactive path (once per run), so its progress must be surfaced or users will
assume the app froze and re-trigger it.

### Dataset selection & switching

**Terminology:** in Protigy an uploaded **dataset = an "ome"** — `GCTs_and_params()$GCTs` is a named
list keyed by the user's labels (e.g. `"Prot"`, `"Phos"`), one entry per uploaded GCT. So "datasets the
user input into Protigy" *are* the omes, and "run PELSA on multiple datasets" means run it on a chosen
subset of omes.

**Selection (in Setup).** `checkboxGroupInput("pelsa_datasets", choices = names(GCTs_and_params()$GCTs))`
— multi-select, defaulting to all (or to `globals$default_ome`). Everything below it in Setup (condition
column, ordering, markers) and the whole analysis run apply **only to the checked datasets**. Per-dataset
Setup config (condition/replicate columns + ordering) is kept in a **named list keyed by dataset**, since
two datasets can have different experimental-setup columns — Setup should re-render the
condition/replicate controls per selected dataset (an inner accordion or per-dataset sub-panel), not
assume one shared condition scheme.

**Switching (after Start-Analysis) — the design question.** The doc asks for a switcher "on top of the
app window, below the tab menu," shared across the PELSA sub-tabs. This is a **deliberate change from the
scaffold**, which currently has each section build its own per-ome `tabsetPanel` independently.

> ⚠ Divergence from scaffold: replace the **per-section** ome tabset with **one app-level PELSA dataset
> switcher** that sits below the navbar and drives Summary, Volcano, and Wood's together. Rationale: with
> a shared switcher the user picks a dataset once and it persists as they move Summary → Volcano → Wood's
> (instead of re-selecting the ome inside every section), and only the active dataset's heavy
> peptide-level objects are held in RAM — which directly serves the performance requirement.

**Recommended UI — a persistent dataset bar (`shinyWidgets::radioGroupButtons`).** A horizontal segmented
button bar pinned directly under the navbar, visible on every PELSA sub-tab, showing one button per
*analyzed* dataset (only those checked at Setup), with the active one highlighted:

```r
# rendered once at PELSA level (not per section); only shown when >1 dataset was analyzed
shinyWidgets::radioGroupButtons(
  inputId  = "pelsa_active_dataset",
  label    = NULL,
  choices  = analyzed_datasets(),      # reactive: the checked subset from Setup
  selected = analyzed_datasets()[1],
  status   = "primary",
  size     = "sm"
)
```

- **Why this over alternatives:**
  - vs. a per-section `tabsetPanel` (scaffold default) — a shared bar keeps the active dataset stable
    across sections and centralizes the "free the inactive dataset" logic in one place.
  - vs. a `selectInput` dropdown — a segmented bar shows all analyzed datasets at once (usually 1–4),
    so switching is one click and the user always sees what's loaded; fall back to `pickerInput`
    (searchable dropdown) only if a run ever spans many datasets.
  - `shinyWidgets` is **already a dependency** (no new package).
- **Behaviour:**
  - The bar renders **only when ≥2 datasets were analyzed**; for a single dataset it is hidden (no
    redundant chrome) and that dataset is implicitly active.
  - `input$pelsa_active_dataset` is the **single source of truth** for which dataset every PELSA section
    renders. Each section (Summary/Volcano/Wood's) reacts to it and renders only that dataset's content —
    replacing the scaffold's inner per-ome tabset. Within a section, the contrast selector (Volcano) is
    still per-dataset and nested under the active dataset.
  - **Memory:** on switch, free the previous dataset's heavy objects (exploded frame, CV table, volcano
    df, intensity lines) and lazily compute the newly-active dataset's (cache in a `reactiveVal` keyed by
    dataset so re-visits are instant but only the active one is "hot"). This is the dataset-level analogue
    of the per-contrast freeing already mandated in Performance notes.
  - Sticky positioning: wrap the bar in a `div` with a small sticky/affixed style so it stays under the
    navbar while scrolling a long Summary/Volcano page.

**Implementation shape.** Lift the dataset tabset out of each `*_Tab_Server` and introduce a thin
**PELSA-level container** (a parent module, or coordination in `app_server`) that (1) owns
`pelsa_active_dataset`, (2) renders the switcher bar below the navbar, and (3) passes the active dataset
down to each section's server so sections become "render for the given dataset" rather than "build a
tabset over all omes." This is the one structural rework the scaffold needs for this feature; note it as
a planned refactor (don't rebuild the scaffold in this planning pass).

> ✅ RESOLVED (Decision #9): the switcher shows **only analyzed datasets** (label "Analyzed datasets"),
> with a small note when uploaded-but-not-analyzed datasets exist. (Kept here for context; see the
> Decisions list.)

---

## Summary section

- Similar to the existing summary tab, this section will be presented in a dashboard style listing metrics and stats of the dataset.
- Restructured below to match the notebook **exactly**, with each metric's **source GCT** made explicit
  (the notebook is very specific about original-vs-processed; the prior wording was vague here).

### Experiment-wide summary

- **Total number of peptide IDs** — count the **rows of the ORIGINAL, non-filtered GCT** data matrix
  (`GCTs_original()[[ome]]`), i.e. `nrow(mat)`. **Do NOT explode** shared peptides — a peptide mapping to
  `A;B;C` counts as **one** peptide ID (one GCT row). See *Peptide explosion strategy* below.
- **Per-protein sequence coverage** — **explode** shared peptides to every parent protein group first
  (one (peptide, accession) row per mapping), then compute coverage per protein = fraction of the
  protein's FASTA sequence spanned by its mapped peptides' `[pep_start, pep_end]` ranges (union of
  covered residues ÷ protein length). A shared peptide contributes to **every** protein it maps to. See
  *Peptide explosion strategy* below.
  - **Computed against ONLY FASTA-mapped peptides (RESOLVED 2026-06-12) — no Spectronaut fallback.** A
    peptide not found in a protein's FASTA does not contribute to that protein's coverage (and lands in
    the "failed to match FASTA" QC list). **No fallback to `PEP.PeptidePosition`.** This is safe because
    **the app uses the same FASTA as the dataset's Spectronaut search** (per species,
    `inst/database/<species>/fasta/`), so unmapped peptides should be rare; if coverage looks understated,
    the FASTA/species selection is the thing to check (the QC count surfaces it). Implementers: do **not**
    add a Spectronaut-position fallback for coverage.
- **Peptide-length distribution (RESOLVED)** — compute peptide length for every identified peptide and
  plot a **density** plot (Decision #2).
  - Mark **both** a vertical dashed **mean** line and a vertical dashed **median** line, each with a text
    annotation; **vertically dodge** the two annotations so they don't overlap (same dodge logic as the
    per-condition CV mean labels) (Decision #1).
  - Expected tryptic mode is ~9–14 aa.

  > ⚠ Divergence from notebook (intentional, Decisions #1–2): the notebook draws a **histogram**
  > (`plt.hist(..., bins=40)`) and tracks the **median** in its run-summary dashboard without an on-plot
  > marker. In-app uses a **density** plot with **both mean and median** dashed markers. This is a
  > deliberate dashboard-facing improvement, so on-screen will not byte-match the notebook figure.
  > *(Stat note: median is the more robust central tendency for a right-skewed length distribution; both
  > are shown so the mean/median gap itself signals skew.)*
- **Missed-cleavage distribution (RESOLVED — add now, Decision F).** Plot the per-peptide missed-cleavage
  count distribution (e.g. a bar/histogram of 0,1,2,… missed cleavages, or a density). For a
  **limited-proteolysis assay this is primary digestion QC** — PELSA's signal *is* differential cleavage,
  so a shifted missed-cleavage profile directly confounds logFC interpretation (under-digestion → long
  peptides + excess missed cleavages). Source = the identified peptide sequences (same peptide universe
  as the length distribution).
  - **Cleavage rule = the notebook's trypsin rule (RESOLVED 2026-06-12 — cross-checked against the
    original notebook).** Verified against Ella's original
    `…/00_original_from_Ella/…/TargetID_qc_notebook_with_dashboard_CsA_Astralimputation_1uM.ipynb`,
    `missed_cleavage_count`:
    ```python
    core = peptide[:-1]                    # drop the C-terminal residue
    matches = re.findall(r'[KR](?!P)', core)   # internal K or R NOT followed by P
    return len(matches)
    ```
    So: **count internal K or R that is NOT immediately followed by P, excluding the peptide's
    C-terminal residue** (the Keil trypsin rule). **`K-P` does NOT count** (matches the notebook). R port =
    the exact same logic (`stringr::str_count(substr(pep, 1, nchar(pep)-1), "[KR](?!P)")` with a
    look-ahead-capable engine, or an equivalent vectorized pass). Peptides shorter than 2 residues → 0.
  - **Parity:** test against this rule **exactly** (it matches the notebook; no divergence). Include
    fixtures: a `K-P` (must be 0/excluded), an internal `R` (counts), a C-terminal `K` (excluded), and a
    mixed peptide.
  - **Scope note:** this is the **tryptic** missed-cleavage count (the digestion-QC target); it does not
    model the LiP structure-probing protease. *(Although the protocol pairs trypsin with Lys-C — which
    biochemically cleaves K even before P — the established analysis uses the trypsin-only `[KR](?!P)`
    rule, so the plan follows the notebook for parity rather than introducing a Lys-C-aware K-P rule.)*

#### Mapping/annotation QC (troubleshooting)

These two metrics surface the **drops** that the FASTA position-mapping and feature-annotation steps
already produce (see *Peptide position mapping (FASTA)* and the annotation port). They exist purely
for QC / troubleshooting — they tell the user how much of their data could not be mapped/annotated and
which peptides/proteins to investigate.

**Layout:** the **counts** render inline as small QC metrics (near the other experiment-wide stats);
the **detail tables are collapsible (default collapsed) and pinned at the BOTTOM of the Summary
section**, so troubleshooting detail is one click away without cluttering the dashboard.

- **# peptides that failed to match the protein-accession FASTA sequence** — show the **count** inline as
  a metric. The **detail table is a collapsible data table pinned at the BOTTOM of the Summary section**
  (a `shinydashboardPlus::box(collapsible = TRUE, collapsed = TRUE)` / `bsCollapse` wrapping a
  `DT::DTOutput`, fixed height + vertical scroll), so it's available for troubleshooting without cluttering
  the dashboard. Columns:
  - **peptide sequence**
  - **protein accession** it failed to map to
  - **gene symbol**
  - **peptide position** — the **Spectronaut `PEP.PeptidePosition`** value reported for that
    peptide×accession (these peptides have no FASTA-derived position *because* mapping failed, so this
    column carries the search-engine-reported position for the user to investigate; clearly the
    Spectronaut value, not a FASTA position)
  - **reason** — `accession_absent` | `sequence_not_found` | `bad_sequence_format` (from the mapper's
    `unmatched` table). `accession_absent` / `sequence_not_found` dominating ⇒ **wrong-FASTA/species**
    (the intended tripwire); `bad_sequence_format` ⇒ **sequence-column formatting** (mod tokens), not a
    run-quality problem.

  *(Implementation: the FASTA mapper's `unmatched` table must carry the `;`-aligned `PEP.PeptidePosition`
  token alongside sequence/accession/gene/reason so this column can render. CSV-exportable.)*
- **# proteins that failed annotation feature match/fetch** — count of accessions for which **no UniProt
  feature record** could be matched or fetched (absent from the cached `uniprot_features` table **and** the
  live top-up fetch returned nothing / errored). Show the **count** plus an optional scrollable list of
  those accessions (+ gene symbol where known) so the user knows which proteins lack feature annotation
  (and may want to refresh the species library). *(Implementation: the annotation step emits the
  notebook's `n_unmapped_features` / `n_unmapped_tm` diagnostics — surface them here; the fetch path
  records accessions it could not resolve.)*

### Per-condition summary

- **Per-condition CV** — a coefficient-of-variation distribution, one curve per condition.

  > ⚠ Divergence from original doc (intentional, user decision 2026-06-12, updated): CV is computed
  > **per peptide-row** on **sum-normalized raw intensities** — start from the original raw
  > (un-log-transformed) values, **sum-normalize**, then compute CV on the normalized values. This
  > **mirrors the notebook's approach** (sum-normalize before CV) and is the single CV definition used
  > everywhere CV appears (per-condition KDE, the per-sample companion table's experiment-wide CV, etc.).

  - **Granularity:** per **peptide-row** CV within each condition, across that condition's replicates.
  - **Source + normalization (exact):** start from the **original raw intensities as uploaded —
    un-log-transformed (linear)**, NOT Protigy's processed/log2/median-normalized matrix. **Then
    sum-normalize** before computing CV (each sample's intensities scaled so columns are on a common
    total — the loading/depth correction), as the notebook does. CV is then computed on those
    **sum-normalized linear** values. Use this same source+normalization for **every** CV figure in the
    module so all CV numbers are comparable.
    - *Sum-normalization basis:* follow the notebook — sum-normalize on each condition's **complete-case
      feature basis** (features non-NA across that condition's replicates) so the per-condition CV isn't
      distorted by differing missingness. *(Confirm the exact basis against `normalization.py::
      sum_normalize` when the notebook volume is remounted; the intent is "sum-normalize like the
      notebook, then CV".)*
  - **Statistic:** `cv_pct = sd/mean*100` on the sum-normalized linear values (ddof=1 / sample SD).
  - **Inclusion threshold:** require `n_nonNA >= 3` per condition. Every row carries a
    `cv_status ∈ {ok, insufficient_replicates, non_finite}` (non_finite = mean 0 / NaN / Inf).
  - **Plot:** KDE density (one curve per condition, fill alpha ≈ .25, dashed outline), conditions drawn
    in the user's confirmed condition order; a vertical dashed line at each condition's **median CV**
    *(median, not mean — robust for the right-skewed CV distribution; Stat review X7)* with the labels
    **dodged vertically** to avoid overlap (sort, `y_top ≈ 0.90`, step ≈ 0.08); x-limit at the **99th
    percentile** of CV to tame the skew tail.
  - **Skip rule:** skip any condition with fewer than ~20 finite CVs (a KDE on fewer points is noise).
  - **Label the panel (Tech review #1).** Caption: *"CV of sum-normalized (un-logged) intensities —
    replicate reproducibility after loading/depth correction."* Sum-normalization removes the
    sample-loading variation that an un-normalized CV would conflate into reproducibility, so the CV now
    reflects signal reproducibility. Note this is still a **different data world** than the volcano
    (significance = limma on the **log2, median-normalized** matrix via the Statistics tab): sum-norm-linear
    here vs log-median-norm there — the caption keeps the two from being read as identical.
  - **Source of truth for re-render / export:** compute once into a tidy per-dataset CV table
    (`cv_pct, n_nonNA, cv_status, condition`), render the KDE from it, and re-use it for export.
  - **Parity note:** since the CV now mirrors the notebook (sum-normalize → `sd/mean*100`), parity is
    tested **both** ways — a **closed-form expectation on the synthetic fixture** (hand-computed
    sum-normalize then `sd/mean*100` on known raw values) **and**, where the sum-norm basis matches,
    against the notebook's `within_condition_cv.csv`. Any deliberate simplification of the sum-norm basis
    vs the notebook must be flagged so a basis difference isn't mistaken for a parity failure. KDE pixel
    values are **excluded** from parity (R `density` ≠ scipy); test the underlying CV table only.

### Per-sample summary

- **Number of peptides quantified per sample** — bar plot, one bar per sample.
  - **Source:** the **processed GCT** (`GCTs_and_params()$GCTs[[ome]]`, log2). A peptide is "quantified"
    for a sample if its value is **finite & > 0** (same mask the notebook uses for `avg_intensity`).
    Count per sample.

    > ⚠ Divergence from original doc: the doc said to use the *original, non-filtered* GCT for the
    > per-sample count. The notebook's `peptides_quantified_per_sample` (cell 25/26) uses the
    > **processed (log2) GCT** with the finite-&-positive mask. Aligning to the notebook. (Contrast with
    > Total-peptide-IDs above, which *does* use the original GCT — the two metrics deliberately differ.)

  - **Companion table** (rendered next to the bar plot): **mean peptide IDs**, **median peptide IDs**,
    and **experiment-wide CV** of the per-sample counts (`sd/mean*100` on the `n_quantified` vector —
    plain linear CV, consistent with the single CV definition above; this one is CV of counts, not of
    intensities). The notebook's sidecar CSV carries `Sample, n_quantified, total_n_peptides`
    (`total_n_peptides = len(data_df)`, all GCT rows); compute the summary rows from `n_quantified`.
  - **Ordering:** bars and table rows must respect the **user's condition + replicate order** from
    Setup (`sample_order`); fall back to alphabetical only if no order is set.

---

## Volcano plot section

- Reuse the volcano machinery from the Stat-comparison module (`R/tab_stat_plot.R`); apply the
  notebook's PELSA plotting logic on top. The mapping of **what to reuse as-is vs. what is new** is made
  explicit below so implementation is a wiring exercise, not a rewrite.

### Reuse from `R/tab_stat_plot.R` (do not rebuild)

- **Nested module structure:** `statPlot_Tab_{UI,Server}` (per-ome tabset) + `statPlot_Ome_{UI,Server}`
  (single ome). The PELSA Volcano section mirrors this shape.
- **Per-contrast registries held at the TAB level, passed by reference into each ome server:**
  `poi_registry`, `top_n_registry`, `label_mode_registry`, each a named list keyed
  `"<ome>::<contrast_key>"`. **This is exactly the mechanism the doc's performance requirement needs** —
  it retains the marker list + per-contrast customization across contrast switches while the heavy plot
  for the inactive contrast is free to be dropped from RAM. Reuse it verbatim; the PELSA marker list is
  just the seed value of `poi_registry[[key]]`.
- **Interactive plotting:** `plotVolcano()` builds a ggplot;
  `ggplotly(gg, source = ns("volcano_click"), tooltip = "text")`. Click-to-add/remove POI via
  `event_data("plotly_click", source = ns("volcano_click"))` + `event_register(p, "plotly_click")`.
- **Search + labeling helpers:** `parse_protein_search_input`, `get_volcano_cols`, `build_volcano_df`,
  `add_volcano_labels`, `volcano_labeled_feature_ids`, `resolve_volcano_label_text`,
  `get_clicked_feature_id`; the search box and label-column selector.
- **Exports:** the per-ome `list(volcano_plot = <pdf fn>, proteins_of_interest = <csv fn>)` export shape.

### New for PELSA (build on top of the reused machinery)

- **Stat source = the existing Statistics tab (RESOLVED, review X3-A).** PELSA does **not** compute its
  own differential statistics. The per-contrast `logFC.<c>` / `adj.P.Val.<c>` / `P.Value.<c>` columns
  come from **Protigy's existing Statistics tab** (`stat_setup_output$stat_results` /
  `$stat_params`, the limma moderated-test pipeline already wired in `app_server.R`). PELSA's volcano
  consumes those reactives the same way `statPlot_Tab_Server` does.
  - **Gate / grey-out:** the PELSA Volcano (and any section that needs contrasts) must be **greyed
    out / disabled with an explanatory message** until the user has run a proper statistical analysis in
    the Statistics tab (i.e. `stat_results()` for the active dataset exists and is non-empty). Mirror the
    existing `validate(need(stat_results(), "Statistical testing not yet run."))` guard from
    `tab_stat_plot.R`, surfaced as a clear "Run a statistical analysis in the Statistics tab first"
    notice on the PELSA tab. This removes the "missing in-app stat stage" gap (review still-open A) — it
    is intentionally delegated to the Statistics tab, not ported.

- **Contrast selector dropdown** — `selectInput("pelsa_volcano_contrast", ...)`. Loading is **lazy**:
  render only the active contrast's volcano; on switch, clear the prior contrast's stat/plot from RAM
  (registries persist marker list + settings, so nothing user-facing is lost). Contrast order follows the
  Statistics tab's contrast list. Each contrast is a column triplet: `logFC.<c>`, `adj.P.Val.<c>`,
  `P.Value.<c>` (from `stat_results`).

- **logFC direction — TWO-SIDED volcano coloring; down-only only for rollup/intensity (RESOLVED 2026-06-12).**

  > ✅ Decision (corrects an earlier mistaken claim): the volcano **significance coloring is TWO-SIDED** —
  > both up (red, `logFC>0`) and down (blue, `logFC<0`) significant peptides are colored, matching the
  > notebook's `draw_volcano_significance`. **Significance (`adj.P.Val < 0.05`) is read from Protigy's
  > upstream stat columns — PELSA does NOT recompute BH.** The **down-only (`logFC<0`)** stabilization
  > signature (limited proteolysis + drug binding → fewer cuts on stabilized regions → peptide intensity
  > *decreases*) is biologically central, but in-app it is applied **only** to:
  > - the **best-peptide-per-protein rollup tie-break** (ties broken toward most-negative logFC), and
  > - **intensity-line protein selection** (which proteins get a line-plot figure).
  >
  > It does **not** restrict the volcano coloring to one direction. *(The science reviewers found the
  > earlier "down-only is the canonical significant set / cell 28 partitions into `sig_results` /`(BH)`"
  > wording does not match the `20260609` notebook, which is two-sided with no internal BH. See
  > `docs/pelsa-review-findings.md` X1 and Decision #4.)* Destabilization (`logFC>0`) is real PELSA
  > signal and stays visible (red) — see still-open item B on whether to surface up/down counts.

- **Color toggle (RESOLVED: single mutually-exclusive toggle).** One control to choose the coloring
  basis — `Color by significance` (default) vs `Color by UniProt feature class`. **Implement as a single
  `radioButtons`/segmented control, NOT two mutually-exclusive checkboxes** (one source of truth, can't
  desync, and it drops the fragile `shinyjs::runjs` disable juggling the existing module uses — see
  `docs/pelsa-review-findings.md` X10).

  > ⚠ Divergence from original doc: the doc frames color as a single toggle "significance vs UniProt
  > features." The notebook (cells 32/33) actually renders **two separate volcano SETS to disk** —
  > `volcano/significance_annotation/` (gray/red/blue differential-abundance call,
  > `draw_volcano_significance` + `SIGNIFICANCE_COLORS`) and `volcano/uniprot_feature_annotation/`
  > (9-bucket feature-class palette, `draw_volcano` + `FEATURE_COLORS`) — and the **feature set drops
  > significance coloring entirely**. An in-app toggle between the two is a reasonable UX adaptation, but
  > the underlying **feature-class palette + multi-protein resolution must match the notebook** (the
  > coloring direction is two-sided per Decision #4). The feature-class palette (single source:
  > `pelsa_qc_helpers.plots.volcano` `FEATURE_PRIORITY`/`FEATURE_COLORS`/`annotate_feature_class`) is, by
  > priority high→low:
  >
  > 1. `active_or_binding_site` `#1f77b4`, 2. `catalytic_domain` `#ff7f0e`,
  > 3. `folded_domain` `#d62728`, 4. `region_or_motif` `#9467bd`,
  > 5. `transmembrane_or_signal` `#2ca02c`, 6. `repeat_or_coiled_coil` `#8c564b`,
  > 7. `low_complexity_or_disorder` `#7f7f7f`, 8. `other` `#bcbd22`,
  > 9. `none` `#d3d3d3` (synthetic: no overlapping feature; still plotted, de-emphasized).
  >
  > **Multi-protein resolution** (peptide panel): for a `;`-delimited peptide, compute overlapping
  > features for every (accession, peptide residue range), pick the highest-priority class across all;
  > tie-break by leading accession then feature start. Dot **color** = winning hit's class; dot **label**
  > = winning accession's gene (fallback to accession when the gene token is empty, per the repo's
  > `;`-alignment rule). In-app shows the two sets as a **single toggle** (Decision #3), not both panels.

- **Best-peptide-per-protein second panel** (`Show best peptide per protein`, default **off**). When on,
  render a second volcano **below** the all-peptides panel. The best-peptide roll-up (`_rollup_to_proteins`)
  is:
  1. `explode_peptide_rows(acc_col = "PG.ProteinAccessions", pos_col = "PEP.PeptidePosition",
     gene_col = "PG.Genes")` — explode `;`-accessions to **one row per (peptide, accession)** so a
     **shared peptide contributes to EVERY parent protein**.
  2. sort by `[adj.P.Val, logFC]` **ascending** with a **deterministic total-ordering tiebreak** (append
     `peptide_seq`, then `accession` to the sort keys); `groupby("accession").head(1)` keeps the single
     most-significant peptide per protein (logFC ties → most-negative logFC; full ties → the
     tiebreak picks a stable, R==pandas-identical row). *(Parity: R `arrange`/`data.table` order is
     stable, pandas `sort_values` is not — pin `kind="mergesort"` in the capture step and apply the same
     total order both sides. Exact `[adj.P.Val, logFC]` ties are very rare in practice but the tiebreak
     makes them deterministic.)*
  3. **One dot per distinct best-peptide, multi-labeled (Decision 2026-06-12).** A peptide has a single
     `(adj.P.Val, logFC)`, so it is **one point** — even if it won multiple accessions. Do **not** draw a
     separate overlapping dot per accession (the notebook's per-accession-dot behavior). Instead:
     - After the per-accession best-peptide selection (step 2), **group the winners back by peptide** so
       each distinct best-peptide is **one dot** at its `(adj.P.Val, logFC)`.
     - **Label that dot with one `<gene>_aa<pos>` per protein it is the best peptide for, `;`-joined into
       one label string.** If peptide P is best for GENEA (aa120) and GENEB (aa88) → one dot, label text
       `GENEA_aa120;GENEB_aa88`.
     - **Collapse identical `<gene>_aa<pos>`** across the won proteins (same gene + same position, e.g.
       two accessions of the same gene at aa120) → **one** entry. Distinct gene **or** position → separate
       entries. Empty gene → `<accession>_aa<pos>` fallback. No cap. Join the distinct entries with `;`.
     - This is the best-peptide analogue of the all-peptide multi-label rule: collapse by the *point*
       (the peptide's single coordinate), attach a label per won-protein.
  - **Fallback:** if `PG.ProteinAccessions` / `PEP.PeptidePosition` are missing, fall back to
    leading-accession-only and **log a warning — do not crash**.
  - Marker matching (for highlighting/labeling) is by **isoform-base, any-token, case-insensitive
    accession** — see the isoform-aware marker matching bullet below (a marker `AAAAAA` also matches
    `AAAAAA-2`).
    Not by gene. *(The rollup's per-accession grouping still keys on the exploded accession token; marker
    matching is the separate highlight step layered on top.)*

  > ✅ Decision (intentional, B): the full `;`-explosion is **deliberate** — a peptide mapping to
  > accessions A **and** B is counted as A's best peptide (if it is A's most-significant) *and*
  > independently evaluated for B (where another peptide may be B's best). This per-accession semantics
  > is the desired behavior, not a bug. It is **representative-peptide selection for display/prioritization,
  > not FDR-controlled protein-level inference** — if a "N proteins" count is ever surfaced, label it as
  > such. ACAT/Simes per-protein aggregation is deferred (not needed now). See `docs/pelsa-review-findings.md` X5.

- **Marker autofill + overlay.** The Setup marker list seeds `poi_registry[[key]]` at section entry; the
  user can still add/remove markers during analysis. Marker peptides render in **magenta `#FF00FF`**
  (black edge, drawn on top, **isoform-aware** any-token case-insensitive accession match — see below)
  **even when their label is suppressed**, so the user can always see which points are marker peptides.

- **Isoform-aware marker matching (RESOLVED 2026-06-12).** A marker is matched by **canonical base
  accession**, so setting a marker to `AAAAAA` also matches peptides mapped to **any of its isoforms**
  (`AAAAAA-2`, `AAAAAA-3`, …) — those peptides are highlighted/labeled too.
  - **Rule:** normalize both sides to the **isoform base** before comparing — strip the `-<n>` isoform
    suffix (`P12345-2` → `P12345`), compare **case-insensitively**, across **every** `;`-delimited token
    of `PG.ProteinAccessions` (any-token match). A peptide is a marker hit if **any** of its accession
    tokens, base-normalized, equals a marker's base-normalized accession.
  - **Symmetric:** also works the other way — if the user enters `AAAAAA-2` as the marker, peptides on
    `AAAAAA` (canonical) and other isoforms of the same base match as well.
  - **Reuse the existing base-fallback helper** the annotation/TM step already uses for isoform handling
    (`P12345-2` → `P12345`), so marker matching, feature annotation, and TM lookup all resolve isoforms
    the same way.
  - **Labels** for matched isoform peptides still follow the `<gene>_aa<pos>` rule (the gene comes from
    that peptide's own `;`-aligned `PG.Genes` token; position from the FASTA mapper against that specific
    isoform's sequence). *(Parity fixture: a marker `AAAAAA` + a peptide on `AAAAAA-2` → assert it is
    matched and labeled.)*

  > ✅ Decision (2026-06-12): use **Protigy's existing volcano highlight color, magenta `#FF00FF`** for
  > marker peptides — NOT the notebook's gold `#FFC107` — so PELSA matches the rest of the app
  > (`tab_stat_plot_helpers.R` uses magenta `#FF00FF` for POI/feature labels; significant points are
  > `darkred`, non-significant `gray`). This is an intentional divergence from the notebook for in-app
  > consistency. Magenta is distinct from both `darkred` (significant) and `gray` (non-significant) so
  > markers still pop.

- **Marker peptide labeling** — `<geneSymbol>_aa<peptidePosition>`; fall back to the **accession** when a
  marker has no matched gene symbol. A multi-protein / multi-occurrence peptide may carry **multiple**
  such labels on its single dot — see *Multi-occurrence / multi-protein → single volcano dot, multiple
  labels* in [Peptide position mapping (FASTA)](#peptide-position-mapping-fasta) for the collapse rule
  (de-dup identical `(gene, pos)`; keep same-gene/different-position distinct; no cap). Three labeling
  modes (`radioButtons` / `checkboxGroupInput`): **(1)** label all marker peptides, **(2)** label only the
  best peptide per marker protein, **(3)** label the **top N** peptides per protein (default **N = 3**,
  "top" = the N smallest adjusted p-values for that protein). Because the label text is fixed to
  `<gene>_aa<pos>`, the generic **label-column selector from `tab_stat_plot.R` can be removed** from the
  PELSA panel.

  > ⚠ Divergence from original doc: the doc deliberately wants marker labels in the form
  > `<gene>_aa<pos>`. The **notebook** labels markers with **`winning_gene` only — NO `_aa<pos>` suffix**
  > (and non-marker top-N labels are also gene-only). **Keeping the doc's `_aa<pos>` intent** (it is the
  > requested in-app improvement), but flagging that the notebook differs, so on-screen labels will not
  > byte-match the notebook's exported figures. The `<pos>` used in the suffix should be the
  > **FASTA-derived `pep_start`** (same position source as the intensity-line labels), not
  > Spectronaut's `PEP.PeptidePosition`.
  >
  > ⚠ Review X2 (Bio): the FASTA `pep_start` comes from the notebook's **monkey-patched
  > `explode_peptide_rows_fasta` (cell 11)** — substring-match against the parent FASTA, **one row per
  > occurrence**, drops not-found pairs — **not** from `peptides.py::explode_peptide_rows` (which uses the
  > Spectronaut token). The R port must target the FASTA version, build an `{accession: sequence}` map,
  > and pin which occurrence labels a multi-occurrence peptide's single volcano dot. **Reclassify this as
  > Moderate–High parity risk** (the re-port table currently under-rates it). See `docs/pelsa-review-findings.md` X2.

- **Threshold line & top-N:** dashed horizontal line at the empirical raw-p corresponding to
  `adj.P.Val == 0.05`; **no line** if nothing passes BH. Top-N labels = smallest adj.P.Val;
  non-significant points are unlabeled. The notebook's sidecar CSV (12 cols) is:
  `panel, peptide_sequence, gene, accession, pep_start, display_label, feature_class_primary,
  winning_accession, winning_gene, logFC, adj_p, raw_p` — mirror this for the PELSA volcano CSV export.

### Hover vs pin interaction (two behaviours)

> ✅ Decision C4 (2026-06-12): **left-click pins** the intensity panel (plotly-native, already used in
> `tab_stat_plot.R`). Hover = light tooltip only. **No right-click** (it was undiscoverable, conflicts
> with the browser context menu, and isn't reliably exposed by plotly). Add a `helpText` caption under
> the plot ("Click a point to pin its peptide profile"). Everywhere "right-click" appeared in this
> section, read **left-click-to-pin**.
>
> ✅ Decision C3 (rendering at scale): **use `plotly::toWebGL()`** for the volcano. *What `toWebGL` means:*
> it switches the plot's scatter traces from SVG (one DOM node per point — which janks/OOMs the browser
> at 100k+ points) to **WebGL/`scattergl`** (GPU-rendered, handles hundreds of thousands of points
> smoothly). The tradeoff is WebGL renders slightly differently from SVG, so on-screen will not pixel-match
> the notebook's SVG export figures — acceptable here.
> **Define the thinnable background set precisely (RESOLVED 2026-06-12):** a peptide is **thinnable iff
> ALL of**: (1) **non-significant**, AND (2) **|logFC| ≤ 0.5**, AND (3) **not a marker-protein peptide**.
> Everything else is **always drawn individually, never thinned**:
> - every **significant** peptide;
> - every peptide with **|logFC| > 0.5** (even if non-significant — a sizeable effect may still be worth
>   clicking; threshold tunable);
> - **every peptide belonging to a marker protein** (all marker-protein peptides are highlighted, so a
>   non-significant marker peptide must remain so the marker protein is fully represented).
>
> So only the **non-significant, |logFC| ≤ 0.5, non-marker** cloud is downsampled. Thin it
> **density-proportionally, not uniformly** (Tech review #6).
> - *What "density-proportional" means:* keep a **fraction** of the background points such that **dense
>   regions stay dense and sparse regions stay sparse** — e.g. bin the cloud (2-D hex/grid over logFC ×
>   −log10p, or by `logFC` bands) and sample a fixed *proportion* of points from each bin, OR sample
>   each point with a probability that scales with local density. The result looks like the same cloud
>   with fewer dots — the **shape, spread, and relative density are preserved**.
> - *vs. uniform thinning* (e.g. "keep every 5th row" or a flat random sample regardless of location):
>   that drops the same fraction everywhere, which **flattens** the cloud — a user eyeballing "where are
>   the points concentrated / how dense is it near the threshold" would be misled because the visual
>   density no longer reflects the real distribution.
> - Show a "showing N of M background points" note so the thinning is honest. The retained sets
>   (significant, |logFC|≥0.5, marker-protein peptides) are **never** thinned.
> Put the hovered/pinned protein's sibling peptides in a **separate small trace** so the fade restyle
> touches only that trace, and tie the fade to the **pinned** selection (not transient hover). See
> `docs/pelsa-review-findings.md` X3/X4.

The two-behaviour UX is deliberate: a cheap hover preview, and an expensive line plot only on an explicit
pin. Concrete plotly mechanisms (Context7 `plotly` for R,
`/websites/rdocumentation_packages_plotly_versions_4_12_0`):

- **Hover (cheap, always-on):** map the per-peptide tooltip via `aes(text = ...)` and
  `ggplotly(viz, tooltip = "text")` (or `tooltip = c("text", "size")` if more fields are wanted). Read
  hover with `event_data("plotly_hover", source = ns("pelsa_volcano"), priority = "event")`. Tooltip
  shows: **protein accession, gene symbol, peptide position `<start>-<end>`, peptide length** — metadata
  only, **no line plot** (so hovering stays light).
- **Fade-other-points effect on pin (not hover):** do this **without a full redraw** — use
  `plotlyProxy()` + `relayout`/`restyle` to dim every point **except** the pinned peptide and its sibling
  peptides from the same protein. (`plotlyProxy()` modifies the plotly object in place inside Shiny — no
  `renderPlotly` re-exec, essential at peptide scale.) Tie the fade to the **pinned** selection, not
  transient hover (UX I6).
- **Left-click pin (expensive, on demand):** on **`plotly_click`** (left button — see the left-click-pin
  decision at the top of this section; no right-click), populate a **fixed left-side panel**: a metadata
  table at the top and the **per-protein intensity line plot** below it, in a **separate `plotlyOutput`**
  so the heavy line plot is only computed on click and the hover path never touches it. The panel
  persists until the next click. Read clicks with
  `event_data("plotly_click", source = ns("pelsa_volcano"), priority = "event")` +
  `event_register(p, "plotly_click")`.

### Per-protein intensity line plot (tooltip preview data + pinned left-click panel)

Port of the notebook's `marker_protein_intensity_line_plot.R` (cells 34/35), one figure per protein:

- **x** = condition in the user's confirmed condition order; **y** = **mean processed-GCT log2 intensity
  as-is** (NO delinearize, NO z-score, NO re-normalize). One line per significant peptide-occurrence.
- **End-of-line labels** = `aa<pos>` where `pos` = the notebook's **FASTA-derived `pep_start`** from
  `exploded_cache` (NOT `PEP.PeptidePosition`). This matches the doc's "Peptide positions are calculated
  by mapping the identified sequence to the respective protein's sequence" note.
- **Selected peptide in front, saturated color**; **sibling peptides of the same protein** behind, in a
  **faded color** — so the current peptide pops. Label placement uses `adjustText`-style greedy y-stack;
  for ≤3 lines, annotate directly.
- **Which proteins get a figure:** YAML `markers:` ∪ accessions with ≥1 significant peptide
  (`adj.P.Val < 0.05` in ≥1 contrast). **Marker proteins render as a two-panel facet** (left =
  significantly regulated peptides, right = its other non-significant peptides; drop an empty side);
  non-marker significant proteins = a single panel of their significant peptides.
- Notebook's long CSV `plotted_intensities.csv` is the data source — mirror it for export.

---

## Wood's Plot section

> **STUB.** Not designed in this pass. A fourth navbarMenu `tabPanel` + module pair
> (`tab_pelsa_section4.R`, following the same nested per-ome pattern) will be added when this section is
> specified.

---

## Execution architecture: all-R port

**Decision (2026-06-12):** PELSA is built **entirely in R**. No Python runs at app runtime; **no
`reticulate` dependency** is added. The notebook's Python analysis is **re-ported to R**, and the
**reference notebook is the gold standard** — every converted helper is **parity-tested against the
notebook's output**, and for any discrepancy the **R code is corrected to match the notebook**, never
the other way around.

**Why all-R (the tradeoff being accepted).** The hybrid Python-via-`reticulate` option was considered
and rejected: although the notebook's pipeline is already validated in Python, keeping it in Python
means bundling a Python environment as a runtime dependency of a Bioconductor/CRAN package
(deploy/packaging burden, `reticulate` bridge overhead, an R⇄Python copy for big frames). The cost the
all-R path accepts in exchange is **re-port effort + correctness risk**, which is bought down to an
acceptable level by the **parity-test gate** below. R is already strong for everything else here —
`org.Hs.eg.db`/`org.Mm.eg.db`/`AnnotationDbi` gene mapping (already used in
`R/sidebar_setup_helpers_GCT-processing.R`), `plotly` interactivity, the existing volcano module
(`tab_stat_plot.R`), `cmapR`, and `future`/`furrr`.

### Benchmark evidence (2026-06-12) — R is not slower

The "is Python faster on large data?" question was settled empirically, not assumed. A representative
peptide-level workload (**300,000 rows × 24 samples**: `;`-accession explode, within-condition CV,
best-peptide rollup) was run three ways (median of 5; full writeup + reproducible scripts in
[`dev/pelsa_benchmark/`](../dev/pelsa_benchmark/RESULTS.md)):

| Scenario | Full workload (median) |
|----------|------------------------|
| Native Python (pandas/numpy) | ~2.26 s |
| R + reticulate (in-memory return) | ~2.17 s |
| **Native R, vectorized** (`matrixStats::rowSds`) | **~2.05 s** |
| Native R, *naive* (`apply(lin, 1, sd)`) | ~17.8 s |

**Conclusion: a competently vectorized native-R backend ties native Python (~2.0 s vs ~2.3 s) — there is
no Python speed advantage on this workload.** The 17.8 s naive figure was caused by a single bad R idiom
(a row-wise `apply(lin, 1, sd)` in the CV step); vectorizing it to `matrixStats::rowSds` cut that stage
from ~16 s to ~0.3 s (54×) and the whole workload to ~2 s. The reticulate bridge itself is cheap (~13 ms
per call; ~105 ms to ferry a full 300k-row frame in-memory), so the bridge was never the issue either.
This removes "Python is faster on big data" from the decision entirely; the only remaining argument for
Python is **reuse + re-port correctness risk**, handled by the parity gate.

> ⚠ **Vectorization rule (mandatory for every re-ported helper).** The benchmark's whole story is one
> idiom: per-row `apply(mat, 1, f)` / row-wise loops are catastrophically slow in R (54× here). Every
> numeric step in the R port **must** use vectorized primitives — `matrixStats` (`rowSds`, `rowMeans2`,
> `colMedians`), `rowSums`/`colSums`, whole-matrix arithmetic, `data.table`/`dplyr` group-bys — and
> **never** a per-row `apply`/`sapply`/`for` over peptides. A parity test that passes but runs in
> minutes is a failed port: treat a >5× slowdown vs the notebook on the same data as a regression to fix.

### Performance design rules for the R port (PELSA data is large)

PELSA always operates at **peptide level** (hundreds of thousands of rows × tens of samples), so every
re-ported helper and every new R function is designed for that scale from the start, not retrofitted.
The benchmark proved R *ties* Python here **only when written this way** — these are requirements, not
suggestions:

1. **Vectorize all numeric work; never per-row.** Operate on whole matrices/columns. Use `matrixStats`
   (`rowSds`, `rowMeans2`, `rowMedians`, `colCV`-style combos), `rowSums`/`colSums`, and matrix
   arithmetic. A `apply(mat, 1, …)` over peptides is an automatic review reject (it was the entire 54×
   gap in the benchmark).
2. **Operate on matrices, not data.frames, in hot paths.** Pull the intensity block to a numeric
   `matrix` once (`as.matrix` / cmapR `mat()`), compute, then reattach IDs. Avoid per-column
   `data.frame` coercion inside loops.
3. **Group-by once, vectorized.** For explode → best-peptide rollup and per-condition aggregation, use a
   single `data.table` (`dt[, .SD[…], by=accession]`) or `dplyr` group op — not nested loops over
   proteins/conditions. `data.table` is preferred for the largest joins/group-bys.
4. **Read cached data efficiently.** Read the UniProt feature cache with `readr::read_tsv` (or `readRDS`
   of a pre-built `.rds`) in one vectorized call (not a row-wise parse loop); select only needed columns.
   *(Decision X11: no `arrow` dependency — see below.)*
5. **Compute lazily + once per Start-Analysis.** Heavy results (CV table, exploded frame, feature
   annotation, rollups) are computed **once** when the user clicks Start-Analysis and cached in a
   `reactiveVal`/on disk — never recomputed inside `renderPlotly`/`renderUI`. The volcano section already
   mandates lazy per-active-contrast loading + freeing the previous contrast (see Performance notes).
6. **Parallelize independent per-ome / per-contrast work** with the already-imported `future`/`furrr`
   (`future_map`) when it measurably helps — but only after vectorizing; parallelism does not rescue a
   per-row apply.
7. **Avoid copies at scale.** Prefer in-place `data.table` updates (`:=`) and `data.table::setDT()` over
   repeated `mutate` copies on 300k-row frames; pre-allocate; avoid growing vectors in loops.
8. **Profile against the gold standard.** Each helper's parity test also records wall time; a >5×
   regression vs the notebook on the same fixture is treated as a bug (see the parity gate).

> These rules are the implementation contract for **every** R function written for PELSA — Setup
> helpers, Summary metrics, the volcano data prep, and the intensity-line aggregation alike — because
> all of them touch peptide-scale data.

### What gets re-ported (Python → R), by risk

| Notebook logic | R re-port approach | Re-port risk |
|----------------|--------------------|--------------|
| `;`-accession explode (peptide → one row per parent accession, pos/gene aligned) | `tidyr::unnest` / `dplyr` | Low |
| **FASTA peptide-position mapping** (`explode_peptide_rows_fasta`, cell 11 — substring match, one row per occurrence, drop not-found) | `stringi::stri_locate_all_fixed` over an `{acc→FASTA}` map, **after AA-strip + I→L-retry**; emits a reason-tagged `unmatched` table (see *Peptide position mapping (FASTA)*) | **Moderate–High** — occurrence semantics, drops, I/L + mod-token edge cases, isoforms |
| Best-peptide roll-up (sort `[adj.P.Val, logFC]` → first per accession) | `dplyr::arrange` + `slice(1)` per accession, **with a total-ordering tiebreak** (final sort on `peptide_seq` then `accession`) so R and pandas pick the same row | Low |
| Within-condition CV (**raw un-logged → sum-normalized** intensities; `sd/mean*100`; `n_nonNA≥3`; `cv_status`) | sum-normalize, then `matrixStats` row stats | Low–moderate (sum-norm basis; parity vs closed-form) |
| Down-only (`logFC<0`) handling — **rollup tie-break + intensity-line selection only** (NOT volcano coloring) | `dplyr::arrange`/`filter` | Low |
| Peptide length, peptides-quantified-per-sample, density/KDE | base R / `stats::density` / `ggplot2` | Low |
| **Missed-cleavage count** (notebook rule: `[KR](?!P)` on `peptide[:-1]` — internal K/R not before P, excl. C-term) | vectorized `stringr`/`stringi` regex over peptide sequences | Low (exact notebook match; parity vs notebook rule) |
| **Per-protein sequence coverage** (explode → union of `[pep_start,pep_end]` ÷ FASTA length) | `data.table` group-by over the exploded FASTA-mapped frame | Low–moderate (depends on FASTA position map) |
| **UniProt feature-overlap + 9-bucket feature-class annotation w/ multi-protein priority resolution** | `data.table::foverlaps` (indexed interval join) + priority/tie-break, in R; **also returns the set of accessions with no feature match** (notebook `n_unmapped_features`/`n_unmapped_tm`) for the Summary QC metric | **Moderate** — benchmarked **faster** in R (~246 ms vs ~507 ms Python); the one to **parity-test hardest** |
| UniProt **API fetch** (top-up + new-species build) | `httr2` (`req_retry`/`req_throttle`/`req_perform`); **records accessions it could not fetch/resolve** for the Summary QC metric | Low — fewer lines than Python (see below) |

> 💡 Decision (2026-06-12) — annotation AND fetch are **native R** (benchmark-backed). The earlier
> "reuse the cache, don't re-port the derivation" recommendation is **superseded**: the benchmark
> ([`dev/pelsa_benchmark/RESULTS.md`](../dev/pelsa_benchmark/RESULTS.md)) shows the annotation overlap is
> **faster in R** via `data.table::foverlaps` (~246 ms vs ~507 ms native Python — an *indexed* interval
> join beats pandas' brute many-to-many `merge`+filter), and a faithful R port of the fetch is **fewer
> lines with one dependency** than the Python original. So:
> - **Annotation derivation is re-ported to R** (`data.table::foverlaps` for the peptide-vs-feature span
>   overlap, then the priority-rank reduction to one winning 9-bucket class per peptide). It is the
>   highest **parity** risk (comma-in-token intra-protein hits like `"2,167"`, `;`-token alignment back
>   onto `PG.ProteinAccessions`, isoform-base TM fallback) — so it gets the hardest synthetic parity
>   tests, but it is **not** a Python dependency.
> - **The API fetch is re-ported to R with `httr2`** — `req_retry()` (backoff + `Retry-After`),
>   `req_throttle()` (rate limit), plus a ~15-line circuit breaker; reusing `org.Hs.eg.db`/`org.Mm.eg.db`
>   for gene mapping. Runs once at Start-Analysis, off the reactive path. Estimated ~60–120 lines + 1
>   dep, vs the notebook's 428-line `uniprot.py` + `requests`/`pyrate_limiter`/`tenacity`.
> - **The on-disk cache is reused as an *optimization*, not to dodge the port.** Read the existing
>   `inst/database/<species>/uniprot_features/uniprot_features.tsv` (+ membrane TSV) with **`readr`**
>   (or a pre-built `.rds`); only fetch+annotate accessions missing from the cache, then write back in
>   the same schema so subsequent runs are cache-hits. *(No `arrow` — the cache is the package's own
>   `.tsv`/`.rds`; the benchmark found parquet doesn't help at this scale, and `arrow` is a heavy
>   Bioc/CRAN dependency for one read. The `.parquet` file may stay on disk for the offline pipeline but
>   the app reads the `.tsv`/`.rds`.)* Both paths are parity-tested against the notebook so the
>   cache and the freshly-computed values agree.
>
> ⚠ **OpenMP note (why this is clean only because it's all-R):** loading `data.table` **and** Python
> `numpy`/`pyarrow` in one process aborts on an OpenMP runtime clash (`OMP: Error #15`), bypassable only
> with an unsafe flag. Going all-R means `data.table` (the fastest path for this join) is usable
> in-process with no conflict — a hybrid app could not safely combine the two. **No Python anywhere**,
> runtime or offline; new-species onboarding uses the same R fetch+annotate path.

### Language allocation per PELSA module (for review)

**Every PELSA module is implemented in R — no Python anywhere, runtime or offline** (decided
2026-06-12, after benchmarking the annotation step and the fetch complexity). The table is the explicit
list to review; the last column carries the language, the benchmarked cost where measured, and the
**parity risk** (which is now the only axis that varies — not language).

| Module / concern | Section | Language | Notes (cost / parity risk) |
|------------------|---------|----------|----------------------------|
| Setup UI (species, compound, marker table, ordering) | Setup | **R** | Shiny + `shinyjqui` `orderInput`. Pure UI. |
| Marker accession⇄gene resolution | Setup | **R** | `org.Hs.eg.db`/`org.Mm.eg.db`/`AnnotationDbi`, already in repo. |
| UniProt **feature/membrane cache read** | Setup/Volcano | **R** | `readr::read_tsv` / `readRDS` (no `arrow`). Cache is an optimization, not a Python crutch. |
| UniProt **API fetch** (top-up + new-species build) | Setup | **R** (`httr2`) | `req_retry`/`req_throttle` + ~15-line breaker. ~60–120 lines vs Python's 428. Off reactive path. |
| UniProt **feature-class derivation** (9-bucket, overlap + priority) | Setup/Volcano | **R** (`data.table`) | Benchmarked **~246 ms in R vs ~507 ms Python** (`foverlaps`). **Highest parity risk** — hardest synthetic tests. |
| Peptide explode (`;`-accessions) | Volcano/Summary | **R** | `tidyr::unnest`. ~760 ms / 300k rows. |
| Within-condition CV | Summary | **R** | `matrixStats::rowSds` (vectorized — ~294 ms; never per-row apply). |
| Best-peptide-per-protein rollup | Volcano | **R** | `dplyr::arrange` + `slice(1)`. ~476 ms. |
| Down-only (`logFC<0`) partition, sig sets | Volcano | **R** | `dplyr::filter`. |
| Peptide length / per-sample depth / **missed-cleavage** | Summary | **R** | base R + `ggplot2`; missed-cleavage = `peptides.py::missed_cleavage_count` port. |
| Volcano plot + interactivity | Volcano | **R** | `plotly`/`ggplotly`, reusing `tab_stat_plot.R`. |
| Intensity line plots (tooltip + pinned left-click panel) | Volcano | **R** | `ggplot2`→`plotly`; `ggrepel` for `aa<pos>` labels. |
| Multi-protein feature priority resolution (color/label) | Volcano | **R** | Second-highest parity risk after derivation; same `foverlaps` machinery, interval-overlap only (no fetch). |

> 💡 Recommendation: **everything in R; nothing in Python.** Benchmarks settled both open pieces — the
> annotation overlap is ~2× faster in R (`data.table::foverlaps`), and the `httr2` fetch is fewer lines
> with one dependency than the Python `requests`/`pyrate_limiter`/`tenacity` stack. The on-disk cache is
> kept purely as a speed optimization (skip re-fetch/re-derive for known accessions), not to avoid a
> port. This keeps a single-language codebase, sidesteps the `data.table`↔Python OpenMP conflict, and
> needs no `reticulate`. The residual work is **parity**, not language: the derivation + multi-protein
> resolution get the strictest synthetic parity tests.

### Parity-test gate — synthetic-dataset parity (the requirement that makes all-R safe)

No converted R helper is trusted until its output **matches the Python notebook's output on a shared
synthetic dataset**. Parity is established against **synthetic data**, not only real-data slices —
synthetic generation is deterministic, ships no PHI / external data-path coupling, and lets each test
target specific edge cases on demand. Three layers; (1) and (2) are required per helper, (3) is the
integration backstop.

**The synthetic fixture is generated in R (single shared input).** A deterministic **R** generator
(seeded) produces the peptide-level frame and **writes it to disk once** (parquet/CSV under
`tests/testthat/fixtures/pelsa/inputs/`). Both sides then read **that one file**: the R helper consumes
it directly, and the Python notebook helper is run once *on the same file* to mint the expected-output
fixture. Generating in R (not Python) means **no Python generator to drift from** and no Python needed to
produce inputs — only to capture expected outputs once. The generator mirrors the real table
(`;`-delimited multi-accession / shared-peptide structure, per-contrast `logFC`/`adj.P.Val`, condition
replicates, NA holes). *(The benchmark's synthetic generators have been removed now that benchmarking is
done; `dev/pelsa_benchmark/RESULTS.md` documents the data shape they used, which is the reference for the
new R test-helper generator.)* Generate small, edge-case-rich frames per test (shared peptides across ≥3
accessions; a condition with `n_nonNA < 3`; all-NA rows; ties in `[adj.P.Val, logFC]`; empty-gene tokens;
markers as leading vs non-leading accession; comma-in-token intra-protein hits like `"2,167"` for the
annotation step).

1. **Synthetic per-helper parity (testthat).** For each re-ported function:
   - **Generate the seeded synthetic frame in R** and write it to the shared input fixture (once).
   - Run the **Python notebook helper** on that committed input file once to capture the expected
     output, and commit it under `tests/testthat/fixtures/pelsa/expected/` (CSV/parquet). *(Capture is a
     one-time dev step — running the test suite itself needs no Python; it reads the committed input +
     expected output. See the fixtures README for the exact capture command + Python env.)*
   - Run the **R port** on the same input file and assert equality: `expect_equal` with a **relative**
     numeric `tolerance` (~`1e-8`–`1e-6`) for floating-point columns (CV%, logFC, −log10 p); **exact**
     match for IDs, accessions, gene symbols, feature-class labels, peptide positions, `cv_status`, and
     row counts.
   - **Determinism rules (required so "exact match" doesn't flap):** (a) apply the **same total-ordering
     sort** (e.g. `[adj.P.Val, logFC, peptide_seq, accession]`) on both sides and pin pandas
     `kind="mergesort"` in the capture step; (b) **canonical re-sort** both frames on a stable key before
     comparing; (c) **exclude KDE/`density()` pixel values from parity** (R `density` bandwidth `nrd0` ≠
     scipy) — test the underlying CV table, not the rendered curve. The **CV** helper is parity-tested
     against a **closed-form** `sd/mean*100` on the synthetic fixture (it intentionally diverges from the
     notebook — see Per-condition summary), not against the notebook CSV.
   - The helper is not wired into the module until green. Helpers to cover: `;`-accession explode,
     **FASTA position mapping** (occurrence semantics + drops), within-condition CV (`cv_pct`,
     `cv_status`), best-peptide rollup, peptide-length, per-sample depth, sequence coverage,
     missed-cleavage, **UniProt fetch parser**, and **feature-class annotation + multi-protein
     resolution**.
   - **Strictest fixtures (Tech review #8) — the two highest structural-risk ports.**
     - *Why annotation specifically:* the benchmark (`dev/pelsa_benchmark/RESULTS.md`) measured the R
       `data.table::foverlaps` annotation port for **speed only** (~2× faster than Python); its output was
       never verified to be **correct**. The annotation logic has structural subtleties (below) that a
       loose tolerance would let slide while the answer is wrong (a feature assigned to the wrong
       accession still yields a valid-looking class string).
     - **Annotation gold standard = manual UniProt lookup, NOT the notebook (RESOLVED 2026-06-12).** Do
       **not** treat the notebook's annotation output as ground truth for parity — the notebook's own
       fetch/derivation could be wrong, and the cached `uniprot_features` table is itself the notebook's
       output (circular). Instead, for a **small hand-picked fixture set of accessions** (~5–10, chosen to
       span feature classes — an enzyme with a catalytic domain + active site, a transmembrane protein, a
       multi-domain protein, a largely-disordered protein, plus one of the project's marker proteins),
       **manually look up the true UniProt sequence-feature records** (feature type, start–end residue
       coordinates) from UniProt directly and **commit them as the expected fixture**. Then assert the R
       fetch + annotation reproduces those features and that a peptide overlapping a known feature gets the
       correct 9-bucket class. This validates the **fetch + classification + overlap** end-to-end against
       the *real* source of truth, catching errors the notebook shares. (For the pure structural cases
       below — token alignment, multi-protein resolution — a synthetic fixture with hand-set feature
       coordinates is fine; UniProt truth is for the fetch/classification correctness.)
     - **Feature-class annotation structural fixtures (assert exactly, synthetic coords):**
       - the **comma-in-token intra-protein hit** (`PEP.PeptidePosition = "2,167"`: a peptide hitting one
         protein at two positions — the notebook handles the comma specially; a naive split mis-aligns);
       - the **`;`-token realignment** case: a peptide `A;B;C` where the middle accession `B` is dropped
         (no coords) must **still emit an empty annotation token at B's `;`-position**, so the output
         tokens stay aligned 1:1 with `PG.ProteinAccessions` — getting this wrong silently shifts every
         downstream gene/feature onto the wrong protein;
       - a **multi-protein peptide where the winning feature class comes from a NON-leading accession**
         (asserts the priority resolution picks across all tokens, not just the leading one);
       - a peptide overlapping **two features of different priority** in one accession (asserts the
         priority ladder + tie-break, not `SCORES` order).
     - **UniProt fetch fixtures (gold = manual UniProt):** for the hand-picked accessions above, assert
       the fetched feature records match the manually-verified UniProt truth (type, coords), so a broken
       fetch/parse is caught against the real source — not against a possibly-wrong cache.
     - **FASTA-drop fixtures:** one per drop `reason` — `accession_absent`, `sequence_not_found`, and a
       **modification-token sequence** (`bad_sequence_format`) — plus an **I/L isobaric** peptide that
       must match only after the I→L retry, and the `pep_position` column carrying the Spectronaut token.
       Assert the `unmatched` table's `reason` column exactly.
2. **Shared-input invariant.** There is exactly **one** synthetic dataset per test, generated in R and
   written to disk; both the R helper and the Python expected-output capture read that same file. No
   Python generator exists, so there is no generator-drift risk to guard against.
3. **End-to-end check on a fuller synthetic dataset (+ optional real-data spot check).** Run the
   assembled R pipeline on a larger synthetic frame and compare the final artifacts — per-condition **CV
   table**, each contrast's **volcano labels CSV**, the **intensity-line CSV** — against the notebook's
   output for that same synthetic frame. Catches integration drift the per-helper tests miss (column
   ordering, join semantics, sample-order handling). Optionally repeat once on a real PELSA sample (or
   `brca` adapted) as a final confidence pass, but synthetic parity is the gate.

Gold-standard rule, restated for implementers: **when R and the notebook disagree, the notebook is
right** — fix the R port, never the fixture (except to correct a capture mistake). Record any
*intentional* in-app deviation (e.g. the `<gene>_aa<pos>` marker label, density-vs-histogram) explicitly
as a `> ⚠ Divergence` callout so it is never mistaken for a parity failure.

**Dependencies to add** (all R, all CRAN/Bioc — no Python, **no `arrow`**): `httr2` (UniProt fetch),
`data.table` (the `foverlaps` interval join for feature annotation — the benchmarked fast path),
`matrixStats` (vectorized row stats), and `stringi` (FASTA substring position mapping) → `DESCRIPTION`
Imports **and** a roxygen `@import`/`@importFrom` in `R/protigy-package.R`, then `devtools::document()`.
`readr` is already a dependency (used for the cache read). **`data.table` NSE caveat:** declare its NSE
symbols via `utils::globalVariables()` + `@importFrom data.table .SD :=` to avoid `R CMD check`
global-variable NOTEs. Mirror the existing `inst/database/human/` layout (`fasta/`,
`uniprot_features/uniprot_features.tsv` (+ optional `.rds`/`.parquet`) + `schema.json`,
`uniprot_membrane/`) for `mouse/` and any future species.

### Implementation order (decided 2026-06-12)

Build **bottom-up: parity harness + pure compute helpers first, UI last.**

1. **Parity-test scaffold** — the R synthetic-data generator + the
   `tests/testthat/fixtures/pelsa/` layout + the fixtures README (capture command + Python env +
   determinism rules). *(The fixtures README is written at this step — it was deferred until
   implementation start.)*
2. **Pure compute helpers, each gated by a synthetic parity test before it is trusted** (no Shiny yet):
   `;`-accession explode → **FASTA position mapping** → **missed-cleavage** + peptide-length →
   **within-condition CV** (raw `sd/mean*100`) → per-sample depth → **best-peptide rollup** (with the
   total-ordering tiebreak) → **UniProt fetch (`httr2`)** → **feature-class annotation (`data.table::foverlaps`)**.
   These live in `tab_pelsa_*_helpers.R` and are the parity boundary.
3. **Wire helpers into the section servers** (Setup → Summary → Volcano), reusing the verified compute.
4. **UI / interaction layer last** — the dataset switcher (parent container), the WebGL volcano +
   left-click pin + intensity panel, Setup controls (species refresh, markers, drag ordering, validation
   + progress). The scaffold structural rework (rename sections, add Wood's 4th, lift the tabset) happens
   here.

Rationale: the compute is where correctness risk lives (it must match the notebook); verifying it under
synthetic parity *before* building UI means the UI builds on trusted functions and parity failures are
caught at the helper, not through the app.

---

## Decisions (resolved 2026-06-12) + remaining open items

These were open questions; the user has now decided them. They are recorded here and reflected in the
relevant sections above.

1. **Peptide-length markers — RESOLVED:** draw **both mean and median** vertical lines, with the
   annotation text **vertically dodged** to avoid overlap (same dodge approach as the CV mean labels).
2. **Peptide-length plot — RESOLVED:** **density** plot (dashboard-friendly), not the notebook histogram.
   Flagged as an intentional in-app divergence from the notebook.
3. **Volcano color — RESOLVED:** a single **mutually-exclusive toggle** (significance vs UniProt feature
   class), not both panels at once. *(UI note from review: implement the toggle as a single `radioButtons`/
   segmented control, not two mutually-exclusive checkboxes — one source of truth, can't desync.)*
4. **Volcano significance direction — RESOLVED: do as the notebook → TWO-SIDED.** The significance
   coloring shows **both** up (red, `logFC>0`) and down (blue, `logFC<0`) significant peptides, matching
   `draw_volcano_significance`. **Down-only (`logFC<0`) is used ONLY** for the best-peptide rollup
   tie-break (most-negative logFC wins) and intensity-line protein selection — **not** to restrict the
   volcano coloring. *(This corrects the earlier "down-only is the default significant set" wording, which
   the science reviewers found does not match the notebook; BH/`adj.P.Val` also comes from Protigy
   upstream, PELSA does not recompute it. See `docs/pelsa-review-findings.md` X1.)*
5. **Execution architecture — RESOLVED (2026-06-12):** all-R port, no `reticulate`; notebook is the
   gold standard; every converted helper is parity-tested **on a shared synthetic dataset** (per-helper
   + end-to-end). New R deps: `httr2`, `data.table`, `matrixStats`, `stringi` (**no `arrow`** — cache
   read uses `readr`/`readRDS`). See [Execution architecture: all-R port](#execution-architecture-all-r-port).
6. **Species extensibility — RESOLVED:** keep the `inst/database/<species>/` convention, **plus** a Setup
   control: a **"Refresh per-species UniProt annotation library"** button with a **checkbox list of the
   species subfolders** found in `inst/database/` (human, mouse, …). The user checks which species to
   refresh and clicks the button to (re)fetch + (re)build that species' `uniprot_features/` +
   `uniprot_membrane/`, with a **progress bar** (fetches can take minutes). The species checklist is
   **re-read live every time the Setup page opens** (e.g. on tab/session re-entry) so a newly added
   species folder appears without restarting the app. See Setup section.
7. **Setup scope — RESOLVED: per-ome (per-dataset), not a single shared panel.** Setup renders **per
   selected dataset** (each gets its own condition/replicate columns, ordering, etc.), **plus an
   "Apply the same setup to all datasets" checkbox** that, when ticked, copies one dataset's config to
   every selected dataset. *(This reverses the earlier "experiment-wide single panel" note. Some inputs —
   species, compound, markers — are still naturally shared; the per-dataset part is the
   condition/replicate columns + ordering, which can legitimately differ per dataset. See Setup.)*
8. **Replicate ordering control — RESOLVED (per UX review):** keep the per-condition `shinyjqui::orderInput`
   stack, **but** add: a **"Reset to default order"** button per section (condition + replicate);
   **scroll-contained, bordered cards** per condition; **collapse single-replicate conditions to a static
   label** (no drag widget for one item); and a **keyboard-accessible fallback** (numeric rank or up/down
   buttons) since `orderInput` drag has no keyboard path. See Setup + review X10/A1.
9. **Dataset switcher scope — RESOLVED:** show **only analyzed datasets** in the switcher bar (label it
   "Analyzed datasets"); surface a small note when uploaded-but-not-analyzed datasets exist (per UX I3).
10. **Switcher control — RESOLVED:** segmented `shinyWidgets::radioGroupButtons` bar (good for the
    typical 1–4 datasets); fall back to `pickerInput` only if a run is expected to span many datasets.

### Review items — now resolved (2026-06-12)
- **A. Stat source — RESOLVED:** `logFC.<c>`/`adj.P.Val.<c>` come from the **existing Statistics tab**
  (`stat_results`); PELSA greys out until stats are run. Not ported. (See Volcano *Stat source*.)
- **C. CV — RESOLVED:** one CV definition everywhere = `sd/mean*100` on **raw un-logged intensities that
  are sum-normalized first** (mirrors the notebook's sum-normalize→CV); **median**-CV summary line.
  (See Per-condition summary.)
- **D. Volcano render — RESOLVED:** `toWebGL` + downsample only the low-|logFC|, non-sig, non-marker
  background cloud (retain significant, |logFC|≥0.5, and all marker-protein peptides). (See Hover/pin.)
- **E. `arrow` — RESOLVED:** dropped; cache read uses `readr`/`readRDS`. (See deps.)
- **B. Protein-level counts — RESOLVED (intentional, deferred refinement):** the shared-peptide
  explosion + best-peptide-per-accession is **deliberate** — a peptide mapping to accessions A and B is
  counted as the best peptide for A (if it is A's most-significant peptide) while B keeps its own
  more-significant best peptide. This is the intended per-accession semantics, **not** treated as
  FDR-controlled protein inference. ACAT/Simes aggregation is **deferred** (not needed now); if a
  protein-count badge is shown later, label it "representative-peptide selection," not an FDR-controlled
  protein list.

- **F. Missed-cleavage QC — RESOLVED: add now.** A missed-cleavage distribution is added to the
  Experiment-wide Summary (`peptides.py::missed_cleavage_count` port). (See Summary.)
- **FASTA — RESOLVED:** per-species FASTA is assumed present at `inst/database/<species>/fasta/` (human
  ready; user adds mouse + future species). Missing FASTA → clear Setup error. (See FASTA section.)

### Still open (confirm during implementation — not blocking)
- **G. Typical/max conditions×replicates and datasets-per-run** — informs drag-stack sizing and the
  switcher control choice; not blocking, but useful to confirm.
