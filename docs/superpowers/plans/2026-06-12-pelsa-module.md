# PELSA Module Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement
> this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking. This plan is the orchestration
> spine; the **gold-standard spec is `docs/pelsa-module-planning.md`** — when in doubt, that doc wins, and
> every implementing subagent MUST be handed the relevant section of it.

**Goal:** Migrate the PELSA peptide-level QC/volcano analysis from the Python notebook
(`PELSA_QC.20260609.ipynb`) into Protigy as an all-R Shiny module (Setup + Summary + Volcano; Wood's Plot
deferred), with every numeric helper verified by R-native synthetic parity tests with known ground truth.

**Architecture:** All-R, no `reticulate`. Pure compute helpers (`R/tab_pelsa_*_helpers.R`) are built and
parity-gated FIRST (bottom-up), then wired into the three section servers, then the UI/interaction layer
(dataset switcher, WebGL volcano, Setup controls) last. Heavy peptide-scale work is vectorized
(`matrixStats`, `data.table`), computed once per Start-Analysis, cached in `reactiveVal`s, and freed per
inactive dataset/contrast.

**Tech Stack:** R / Shiny; new deps `httr2` (UniProt fetch), `data.table` (`foverlaps` interval join),
`matrixStats` (vectorized row stats), `stringi` (FASTA substring mapping). Reuse `tab_stat_plot.R` volcano
machinery, `shinyjqui::orderInput`, `shinyWidgets::radioGroupButtons`, `plotly`/`ggplotly`, `cmapR`,
`org.Hs.eg.db`/`org.Mm.eg.db`. **No Python in the shipped test suite** — parity uses R synthetic fixtures
with closed-form / hand-set ground truth (per user decision 2026-06-12).

---

## Execution model (subagent-driven, per user request)

Each **Phase** below is implemented as a unit. For every phase the orchestrator (the main session) MUST:

1. **Generate a per-phase technical plan** with an Opus model (fable if available, else opus) — expand the
   phase's tasks into concrete TDD steps grounded in `docs/pelsa-module-planning.md` and the real files.
2. **Dispatch implementation** — one subagent for a simple phase, an **agent team** for a complex one
   (helper-heavy or UI-heavy phases). Implementers MUST follow TDD (test first → fail → implement → pass)
   and the **vectorization rules** (no per-row `apply`; see planning doc → *Performance design rules*).
   Implementers use **context7 MCP** for `data.table`/`stringi`/`httr2`/`plotly`/`shinyWidgets` APIs.
3. **Code review** — dispatch a **fable-or-opus** `code-reviewer` (or `r`-aware reviewer) subagent;
   address CRITICAL/HIGH before moving on.
4. **Test verification** — dispatch a **sonnet** subagent to run `devtools::load_all(".")` +
   `devtools::test()` for the phase's tests and confirm green + that outputs are generated as expected.
5. **Escalate before breaking logic** — if any implementer/reviewer surfaces a scientifically or logically
   unreasonable design, or a breaking ambiguity, STOP and ask the user before continuing.

After ALL phases: dispatch an **opus agent team** to review the finished module for performance / runtime /
resource-management improvements (see *Phase 9*).

**Workflow gotchas (apply in every phase):** after editing `R/`, `devtools::load_all(".")` before testing;
after editing roxygen `@import`/`@importFrom`/`@export`, also `devtools::document()`. Use `.data$col` /
`.data[[var]]` in dplyr/ggplot2. Commit per-task with semantic messages (`feat(pelsa): …`,
`test(pelsa): …`). Commit ONLY changes for the current task; do not push unless the user asks.

---

## File structure (created / modified across the plan)

**New helper files (pure compute, parity boundary):**
- `R/tab_pelsa_explode_helpers.R` — `;`-accession explode (token-aligned).
- `R/tab_pelsa_fasta_helpers.R` — FASTA reader + FASTA-substring position mapping + `unmatched` table.
- `R/tab_pelsa_peptide_helpers.R` — missed-cleavage count, peptide length, multi-label string builder.
- `R/tab_pelsa_cv_helpers.R` — sum-normalize + within-condition CV.
- `R/tab_pelsa_depth_helpers.R` — per-sample quantified-peptide depth + summary stats.
- `R/tab_pelsa_coverage_helpers.R` — per-protein sequence coverage (union of spans / FASTA length).
- `R/tab_pelsa_rollup_helpers.R` — best-peptide-per-protein rollup (total-ordering tiebreak).
- `R/tab_pelsa_uniprot_fetch.R` — `httr2` UniProt fetch (retry/throttle/breaker) + parser.
- `R/tab_pelsa_annotation_helpers.R` — `foverlaps` feature-overlap + 9-bucket priority resolution +
  multi-protein winner + isoform-base helper + cache read/write.
- `R/tab_pelsa_marker_helpers.R` — isoform-aware marker matching; marker accession⇄gene resolution.
- `R/tab_pelsa_volcano_helpers.R` — PELSA volcano df builder (two-sided color, feature color), label
  modes, density-proportional background thinning.
- `R/tab_pelsa_intensity_helpers.R` — per-protein intensity-line data builder (which proteins, line data).

**Modified module files (wiring + UI):**
- `R/tab_pelsa_section1.R` (Setup), `R/tab_pelsa_section2.R` (Summary), `R/tab_pelsa_section3.R` (Volcano).
- `R/tab_pelsa_helpers.R` (keep shared placeholder util; add small shared helpers).
- `R/app_ui.R` (rename Section 1/2/3 tabPanels → Setup/Summary/Volcano Plot; add dataset-switcher bar).
- `R/app_server.R` (PELSA-level container: own `pelsa_active_dataset`, pass active dataset to sections).
- `R/protigy-package.R` (roxygen `@importFrom` for new deps) + `DESCRIPTION` Imports.
- `inst/database/<species>/uniprot_features/` etc. (mouse layout mirrors human; created by refresh path).

**New test + fixture files:**
- `tests/testthat/fixtures/pelsa/README.md` — generator + ground-truth conventions (R-only).
- `tests/testthat/fixtures/pelsa/generate_synthetic.R` — seeded R synthetic peptide-frame generator.
- `tests/testthat/test-pelsa-*.R` — one test file per helper group.

---

## Phase 0 — Dependencies & scaffolding prep

**Files:**
- Modify: `DESCRIPTION` (Imports)
- Modify: `R/protigy-package.R`
- Create: `tests/testthat/fixtures/pelsa/README.md`

- [ ] **Step 1: Add the four new Imports to `DESCRIPTION`**

Add under `Imports:` (alphabetical-ish, matching existing style), each on its own line with trailing comma:
```
    data.table,
    httr2,
    matrixStats,
    stringi,
```
Do NOT add `arrow` (Decision E — cache read uses `readr`/`readRDS`).

- [ ] **Step 2: Add roxygen imports to `R/protigy-package.R`**

Append these tags to the package-level roxygen block:
```r
#' @importFrom matrixStats rowSds rowMeans2 rowMedians colMedians
#' @importFrom stringi stri_locate_all_fixed stri_replace_all_fixed
#' @importFrom data.table data.table setDT setkey foverlaps .SD :=
#' @importFrom httr2 request req_url_query req_retry req_throttle req_perform resp_body_json
```
Also add, near the existing `utils::globalVariables()` call (or create one), the `data.table` NSE symbols
to silence `R CMD check` NOTEs:
```r
utils::globalVariables(c(".SD", ".N", ".I", "accession", "start", "end", "pep_start", "pep_end"))
```

- [ ] **Step 3: Regenerate NAMESPACE**

Run: `R -e 'devtools::document(".")'`
Expected: NAMESPACE gains `importFrom(...)` lines for the four packages; no errors.

- [ ] **Step 4: Confirm deps install / load**

Run: `R -e 'library(data.table); library(httr2); library(matrixStats); library(stringi); cat("OK\n")'`
Expected: `OK` (if any are missing, `install.packages()` them — they are all CRAN).

- [ ] **Step 5: Write the fixtures README** (`tests/testthat/fixtures/pelsa/README.md`)

Document (R-only parity model, per user decision): synthetic frames are generated in R by
`generate_synthetic.R` with a fixed seed; ground truth is **hand-computed / closed-form in the test
itself** (CV, missed-cleavage, FASTA occurrence, coverage) or **hand-set fixture coordinates** (annotation
features) — **no Python, no notebook capture in the committed suite**. Note that the original Python
notebook remains the *conceptual* conversion target (logic must match) but is not invoked by tests.
Record the synthetic frame shape (`;`-multi-accession / shared-peptide / NA holes / per-contrast
`logFC`/`adj.P.Val`) from `dev/pelsa_benchmark/RESULTS.md`.

- [ ] **Step 6: Commit**
```bash
git add DESCRIPTION R/protigy-package.R NAMESPACE tests/testthat/fixtures/pelsa/README.md
git commit -m "chore(pelsa): add data.table/httr2/matrixStats/stringi deps + fixtures README"
```

---

## Phase 1 — Synthetic fixture generator (R, seeded)

**Files:**
- Create: `tests/testthat/fixtures/pelsa/generate_synthetic.R`
- Create: `tests/testthat/test-pelsa-fixtures.R`

**Goal:** one deterministic R generator producing an edge-case-rich peptide-level frame mirroring the real
Spectronaut export columns (`PG.ProteinAccessions`, `PG.Genes`, `PEP.StrippedSequence`,
`PEP.PeptidePosition`, sample-intensity columns), plus a tiny `{accession→FASTA}` map, plus per-contrast
`logFC`/`adj.P.Val` columns. Edge cases baked in (so later helper tests can target them): shared peptide
across ≥3 accessions; a peptide occurring ≥2× in one protein; a peptide absent from its accession's FASTA;
an I/L-isobaric peptide; an isoform accession (`P12345-2`); empty-gene token; a condition with `n_nonNA<3`;
all-NA row; exact `[adj.P.Val, logFC]` tie; a `bad_sequence_format` row (contains a mod token).

- [ ] **Step 1: Write the failing test** (`test-pelsa-fixtures.R`)
```r
test_that("synthetic generator is deterministic and has required edge cases", {
  source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))
  a <- pelsa_make_synthetic(seed = 1)
  b <- pelsa_make_synthetic(seed = 1)
  expect_identical(a$peptides, b$peptides)                      # deterministic
  expect_true(any(grepl(";", a$peptides$PG.ProteinAccessions))) # shared peptide
  expect_true(any(a$peptides$PEP.StrippedSequence == a$dup_peptide))   # ≥2 occurrences seeded
  expect_true(a$absent_peptide %in% a$peptides$PEP.StrippedSequence)   # FASTA-absent seeded
  expect_true(is.list(a$fasta) && all(nchar(unlist(a$fasta)) > 0))     # {acc→seq}
  expect_true(any(is.na(a$peptides[[a$sample_cols[1]]])))             # NA holes
})
```
- [ ] **Step 2: Run test to verify it fails**
Run: `R -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pelsa-fixtures.R")'`
Expected: FAIL ("could not find function pelsa_make_synthetic").
- [ ] **Step 3: Implement `generate_synthetic.R`**
Write `pelsa_make_synthetic(seed, n_extra_peptides = 50)` returning a list with `$peptides` (data.frame of
the columns above), `$fasta` (named list accession→AA string), `$sample_cols`, `$condition_map`
(sample→condition), and named handles for each seeded edge case (`$dup_peptide`, `$absent_peptide`,
`$il_peptide`, `$isoform_accession`, `$tie_peptides`, `$bad_seq_peptide`). Use `set.seed(seed)`; build the
FASTA strings so the seeded peptides land at known positions (so coverage/occurrence ground truth is
closed-form). Keep it small (~30-60 rows) and pure base R + `stats`.
- [ ] **Step 4: Run test to verify it passes**
Run: same as Step 2. Expected: PASS.
- [ ] **Step 5: Commit**
```bash
git add tests/testthat/fixtures/pelsa/generate_synthetic.R tests/testthat/test-pelsa-fixtures.R
git commit -m "test(pelsa): add seeded R synthetic peptide-frame generator"
```

---

## Phase 2 — Pure compute helpers (parity-gated, no Shiny)

> COMPLEX PHASE → dispatch an **agent team** (split the helper groups across teammates; they share the
> Phase-1 generator). Each helper: TDD with closed-form / hand-set ground truth; vectorized; committed only
> when its test is green. Hand each implementer the exact planning-doc section named below.

Each helper group is a task block. Implementers MUST read the cited planning-doc section verbatim.

### Task 2A: `;`-accession explode — `R/tab_pelsa_explode_helpers.R`
Spec: planning doc → *Peptide explosion strategy* (the explode side).
- [ ] Test first (`test-pelsa-explode.R`): a peptide `A;B;C` with `;`-aligned `PG.Genes` and
  `PEP.PeptidePosition` explodes to 3 rows, tokens aligned 1:1; **gene recycling** when `PG.Genes` has one
  token for many accessions (real data: `SERPINB6` for 4 accessions → recycle/last-token fallback per the
  `;`-alignment rule); empty-gene token → `NA`/accession fallback flagged. Assert row count + alignment
  exactly.
- [ ] Implement `pelsa_explode_accessions(df, acc_col, gene_col, pos_col)` with `tidyr::unnest` /
  `data.table` — vectorized, no row loop. Return long frame keyed by `(row_id, accession)` with
  `gene`, `pep_position_token` aligned.
- [ ] Run → PASS → commit `feat(pelsa): token-aligned ;-accession explode helper`.

### Task 2B: FASTA reader + position mapping — `R/tab_pelsa_fasta_helpers.R`
Spec: planning doc → *Peptide position mapping (FASTA)* (FULL section — highest parity risk).
**Confirmed at build time:** the sequence column is `PEP.StrippedSequence` and is already pure `[A-Z]`
(no mod tokens in real exports) — so the "strip mod tokens" logic is a **validation assertion**, not an
active transform: try exact match on the sequence as-is; if a sequence is not pure `[A-Z]`, route it to
`unmatched` with `reason = "bad_sequence_format"` (do not attempt to repair).
- [ ] Test first (`test-pelsa-fasta.R`) using the seeded generator:
  - `$dup_peptide` (≥2 occurrences) → **two** rows, correct `pep_start`/`pep_end`,
    `pep_occurrence_idx ∈ {1,2}`, `n_occurrences == 2`.
  - overlapping-repeat (`AAA` in `AAAA`) → occurrence stepping `pos = i+1` (assert the two starts).
  - `$absent_peptide` → dropped, lands in `unmatched` with `reason == "accession_absent"` or
    `"sequence_not_found"` (assert which) and carries the Spectronaut `PEP.PeptidePosition` token.
  - `$il_peptide` → matches **only after** the I→L retry (assert it is matched, not in `unmatched`).
  - `$isoform_accession` (`P12345-2`) → FASTA-key resolves (base fallback if isoform key absent).
  - `$bad_seq_peptide` → `unmatched` with `reason == "bad_sequence_format"`.
- [ ] Implement: `pelsa_read_fasta(path)` (lightweight FASTA→named list, key on accession parsed from
  header); `pelsa_map_peptide_positions(exploded_df, fasta_map)` using
  `stringi::stri_locate_all_fixed`, one row per occurrence, I→L retry on miss, isoform base fallback.
  Returns `list(matched = <cache>, unmatched = <table with peptide_sequence, accession, gene, pep_position,
  reason>)`. Vectorized over peptide×accession pairs.
- [ ] Run → PASS → commit `feat(pelsa): FASTA-substring peptide position mapping with unmatched table`.

### Task 2C: missed-cleavage + peptide length + multi-label builder — `R/tab_pelsa_peptide_helpers.R`
Spec: planning doc → Summary *Missed-cleavage distribution* (the `[KR](?!P)` on `peptide[:-1]` rule) +
*Peptide-length distribution* + the *Multi-occurrence multi-label* `;`-join rule.
- [ ] Test first (`test-pelsa-peptide.R`): missed-cleavage fixtures — a `K-P` peptide → **0**; an internal
  `R` → counts; a C-terminal `K` → excluded; mixed peptide → known count. Length = `nchar`. Multi-label:
  `(GENEA,120)+(GENEB,88)` → `"GENEA_aa120;GENEB_aa88"`; duplicate `(GENEA,120)` collapses; same gene
  diff pos kept; empty gene → `<accession>_aa<pos>`; single protein → no `;`.
- [ ] Implement `pelsa_missed_cleavages(seq)` (vectorized
  `stringr::str_count(substr(seq, 1, nchar(seq)-1), "(?<=.)[KR](?!P)")` or `stringi` equivalent with
  look-ahead; seqs <2 aa → 0), `pelsa_peptide_length(seq)`, and
  `pelsa_build_multilabel(genes, positions, accessions)` (collapse identical `(gene,pos)`, accession
  fallback, `;`-join in token order, no cap).
- [ ] Run → PASS → commit `feat(pelsa): missed-cleavage, peptide-length, multi-label helpers`.

### Task 2D: within-condition CV — `R/tab_pelsa_cv_helpers.R`
Spec: planning doc → *Per-condition summary* (sum-normalize raw un-logged → `sd/mean*100`, `n_nonNA>=3`,
`cv_status`, complete-case per-condition basis).
- [ ] Test first (`test-pelsa-cv.R`) with **closed-form** ground truth: hand-pick a 2-condition ×
  3-replicate matrix of known raw values, hand-compute sum-normalization (per-condition complete-case
  basis, scale = mean) then `sd/mean*100` (ddof=1), assert `cv_pct` to `tolerance = 1e-8`. Assert
  `cv_status == "insufficient_replicates"` for a row with `n_nonNA < 3`; `"non_finite"` for a mean-0 row.
- [ ] Implement `pelsa_sum_normalize(mat, condition, ...)` and
  `pelsa_within_condition_cv(raw_mat, condition_map)` returning tidy
  `data.frame(row_id, condition, cv_pct, n_nonNA, cv_status)`. **Vectorized** with
  `matrixStats::rowSds`/`rowMeans2` per condition block — NEVER `apply(.,1,sd)` (planning doc bans it).
- [ ] Run → PASS → commit `feat(pelsa): sum-normalized within-condition CV (vectorized)`.

### Task 2E: per-sample depth — `R/tab_pelsa_depth_helpers.R`
Spec: planning doc → *Per-sample summary* (processed/log2 GCT, finite-&->0 mask; mean/median/CV-of-counts).
- [ ] Test first (`test-pelsa-depth.R`): a known processed matrix with NAs/zeros → per-sample
  `n_quantified` = count of finite & >0; companion stats mean/median/`sd/mean*100` of the count vector
  (closed-form). Assert `total_n_peptides == nrow`.
- [ ] Implement `pelsa_peptides_per_sample(processed_mat)` (`colSums(is.finite(mat) & mat > 0)`) +
  `pelsa_depth_summary(n_quantified)`.
- [ ] Run → PASS → commit `feat(pelsa): per-sample quantified-peptide depth helpers`.

### Task 2F: per-protein sequence coverage — `R/tab_pelsa_coverage_helpers.R`
Spec: planning doc → *Experiment-wide summary* coverage bullet (explode → union of `[pep_start,pep_end]` ÷
FASTA length; FASTA-mapped peptides only, no Spectronaut fallback).
- [ ] Test first (`test-pelsa-coverage.R`): a protein with two overlapping peptide spans → union length
  (not sum); coverage = union ÷ `nchar(fasta)`; shared peptide contributes to **every** mapped protein;
  FASTA-unmapped peptide excluded. Closed-form on the seeded FASTA.
- [ ] Implement `pelsa_sequence_coverage(matched_cache, fasta_map)` — `data.table` group-by accession,
  interval-union via sort+merge (vectorized), divide by FASTA length.
- [ ] Run → PASS → commit `feat(pelsa): per-protein sequence coverage (interval union)`.

### Task 2G: best-peptide rollup — `R/tab_pelsa_rollup_helpers.R`
Spec: planning doc → *Best-peptide-per-protein second panel* steps 1-3 + *Peptide explosion strategy*
best-peptide row + the total-ordering tiebreak note.
- [ ] Test first (`test-pelsa-rollup.R`): explode `A;B;C`; sort `[adj.P.Val, logFC, peptide_seq,
  accession]` ascending; `head(1)` per accession; assert the `$tie_peptides` fixture resolves
  deterministically (stable, total-order). Then **regroup winners by peptide** → one dot per distinct
  best-peptide, multi-label one `<gene>_aa<pos>` per won protein, `;`-joined (reuse `pelsa_build_multilabel`).
- [ ] Implement `pelsa_best_peptide_rollup(exploded_stat_df)` — `data.table`
  `setorder(...)` + `.SD[1L, by = accession]`; then group-by peptide for the display dots.
- [ ] Run → PASS → commit `feat(pelsa): best-peptide-per-protein rollup with deterministic tiebreak`.

### Task 2H: UniProt fetch + parser — `R/tab_pelsa_uniprot_fetch.R`
Spec: planning doc → *What gets re-ported* (UniProt API fetch row) + `dev/pelsa_benchmark/RESULTS.md`
(httr2 mapping table). **Use context7 for `httr2` API.**
- [ ] Test first (`test-pelsa-uniprot.R`): test the **parser** against a committed **canned JSON fixture**
  (a small hand-written UniProt-shaped JSON saved under fixtures — NO live network in tests): assert it
  extracts `accession, feature_type, start, end, description` into the `schema.json` columns and assigns
  the 9-bucket `feature_class` + `class_score` per `schema.json` `feature_class_scores`. Mark any
  network-touching test `skip_on_ci()` / `skip_if_offline()`.
- [ ] Implement `pelsa_parse_uniprot_json(json)` (pure, tested) and `pelsa_fetch_uniprot(accessions, ...)`
  (`httr2` with `req_retry(max_tries=, backoff=, is_transient=)`, `req_throttle()`, ~15-line circuit
  breaker; batches ≤500; records unresolved accessions). Reuse `org.Hs.eg.db`/`org.Mm.eg.db` for gene
  mapping. Off the reactive path.
- [ ] Run → PASS → commit `feat(pelsa): httr2 UniProt fetch + JSON parser`.

### Task 2I: feature-class annotation (`foverlaps`) — `R/tab_pelsa_annotation_helpers.R`
Spec: planning doc → *UniProt feature-overlap + 9-bucket* rows + Volcano *Multi-protein resolution* +
Parity gate *Feature-class annotation structural fixtures* (HARDEST parity). **Use context7 for
`data.table::foverlaps`.** Gold standard = **hand-set synthetic feature coordinates** (R-only; the
manual-UniProt option was for the Python-capture model, which the user dropped — synthetic known-truth is
the gate now).
- [ ] Test first (`test-pelsa-annotation.R`) with hand-set feature coords:
  - peptide overlapping a known feature → correct 9-bucket class (priority ladder from `schema.json`:
    active_or_binding_site > catalytic_domain > folded_domain > region_or_motif >
    transmembrane_or_signal > repeat_or_coiled_coil > low_complexity_or_disorder > other > none).
  - **comma-in-token** `PEP.PeptidePosition = "2,167"` (one protein, two positions) handled (not naive
    split).
  - **`;`-token realignment**: `A;B;C` with `B` dropped → empty annotation token at B's position, output
    stays 1:1 with `PG.ProteinAccessions`.
  - winning class from a **non-leading** accession (priority across all tokens, not just leading).
  - two features of different priority in one accession → higher-priority wins (tie-break leading
    accession then feature start).
  - cache read of `inst/database/human/uniprot_features/uniprot_features.tsv` via `readr` returns the
    `schema.json` columns; isoform-base fallback (`P12345-2`→`P12345`) resolves.
- [ ] Implement `pelsa_read_feature_cache(species_dir)` (readr, select needed cols),
  `pelsa_annotate_features(peptide_spans, feature_table)` using `setkey` + `foverlaps` then a vectorized
  priority-rank reduction to one winning class per peptide (multi-protein: across all tokens), returning
  also the set of accessions with **no** feature match (Summary QC `n_unmapped_features`).
- [ ] Run → PASS → commit `feat(pelsa): data.table foverlaps feature-class annotation`.

### Task 2J: isoform-aware marker matching + marker resolution — `R/tab_pelsa_marker_helpers.R`
Spec: planning doc → *Isoform-aware marker matching* + Setup marker-table resolution bullets.
- [ ] Test first (`test-pelsa-marker.R`): marker `AAAAAA` matches a peptide on `AAAAAA-2` (any-token,
  case-insensitive, isoform-base); symmetric (marker `AAAAAA-2` matches `AAAAAA`); non-match negative case.
  `pelsa_parse_markers` reuses `parse_protein_search_input` delimiter logic.
- [ ] Implement `pelsa_isoform_base(acc)`, `pelsa_match_markers(accession_tokens, marker_bases)`,
  `pelsa_resolve_marker_table(input, species)` (accession→gene via org.db; gene→accession choices with
  canonical/reviewed flags). Pure parts tested; the org.db lookups isolated behind a thin function.
- [ ] Run → PASS → commit `feat(pelsa): isoform-aware marker matching + resolution helpers`.

**Phase-2 gate:** All `test-pelsa-*.R` green via `devtools::test()`. No helper wired into a server yet.

---

## Phase 3 — Volcano + intensity data builders (pure)

**Files:** Create `R/tab_pelsa_volcano_helpers.R`, `R/tab_pelsa_intensity_helpers.R`;
`tests/testthat/test-pelsa-volcano-data.R`, `test-pelsa-intensity-data.R`.
Spec: planning doc → Volcano *New for PELSA* (two-sided color, feature color, label modes, threshold line),
*Hover vs pin* (density-proportional thinning), *Per-protein intensity line plot*.

- [ ] **Task 3A — volcano df builder.** Test first: from a stat frame + matched cache + annotation,
  `pelsa_build_volcano_df(...)` produces one row per source peptide (all-peptide panel, NO explode),
  `sig` two-sided (`adj.P.Val < 0.05`, color up=red/down=blue/ns=gray), `feature_class` color column,
  `label` = multi-label `;`-joined string, `is_marker` flag. Best-peptide variant uses the Phase-2G rollup.
  Implement; reuse `get_volcano_cols`/`build_volcano_df` conventions from `tab_stat_plot.R` where they fit.
  Run → PASS → commit.
- [ ] **Task 3B — density-proportional thinning.** Test first: define thinnable = non-sig AND |logFC|≤0.5
  AND non-marker; assert significant / |logFC|>0.5 / marker peptides are **never** dropped; assert a dense
  bin keeps proportionally more points than a sparse bin (bin-and-sample-a-fraction); assert "showing N of
  M" count is returned. Implement `pelsa_thin_background(df, keep_frac, ...)` (2-D bin over
  logFC × −log10p, sample a proportion per bin). Run → PASS → commit.
- [ ] **Task 3C — intensity-line data builder.** Test first: `pelsa_intensity_line_data(protein, ...)`
  returns one line per significant peptide-occurrence, x = condition in confirmed order, y = mean
  processed-GCT log2 **as-is** (no transform), end labels `aa<pos>` from FASTA `pep_start`; marker proteins
  get the two-panel split (sig vs non-sig peptides); protein set = markers ∪ accessions with ≥1 significant
  peptide. Implement. Run → PASS → commit.

---

## Phase 4 — PELSA-level container & dataset switcher (structural refactor)

**Files:** Modify `R/app_ui.R`, `R/app_server.R`; possibly new `R/tab_pelsa_container.R`.
Spec: planning doc → *Dataset selection & switching* + *Module structure* divergence notes.

- [ ] **Task 4A** — Rename the three navbar tabPanels in `R/app_ui.R`:
  `"Section 1"`→`"Setup"`, `"Section 2"`→`"Summary"`, `"Section 3"`→`"Volcano Plot"` (values stay stable
  or update consistently). Add the **dataset switcher bar** (`shinyWidgets::radioGroupButtons`, id
  `pelsa_active_dataset`, rendered once below the navbar, shown only when ≥2 datasets analyzed, sticky div).
- [ ] **Task 4B** — In `R/app_server.R`, introduce PELSA-level coordination that owns `analyzed_datasets()`
  (the checked subset from Setup) and `input$pelsa_active_dataset`, and **passes the active dataset down**
  to each `*_Tab_Server` so sections render for the active dataset instead of building their own per-ome
  tabset. Keep the existing export gathering (`PELSASection{1,2,3}_exports`) intact; export functions must
  recompute **all** analyzed datasets (Eng review X8). Add a per-dataset cache `reactiveVal` keyed by
  dataset; free inactive datasets' heavy objects on switch.
- [ ] **Task 4C** — Lift the per-ome `tabsetPanel` out of each `*_Tab_UI/_Tab_Server` (Setup keeps a
  per-dataset sub-panel; Summary/Volcano become "render for active dataset"). Smoke test: app launches,
  switcher appears with >1 ome, switching updates active dataset.
- [ ] Verify: `devtools::load_all(".")` + launch smoke (shinytest2 navigation test or manual) → commit
  `refactor(pelsa): app-level dataset switcher + active-dataset threading`.

---

## Phase 5 — Setup section wiring

**Files:** Modify `R/tab_pelsa_section1.R`; add `R/tab_pelsa_section1_helpers.R` if needed.
Spec: planning doc → *Setup section* (full control table + UX refinements + validation/progress +
per-ome scope + species refresh).

- [ ] **Task 5A — controls:** dataset checkboxGroup, species selectInput (live-read `inst/database/`),
  compound selectInput (read `inst/pelsa/compound_markers.yaml` via `system.file`/`yaml::read_yaml`,
  autofill marker table), marker paste box + `parse_protein_search_input`, marker `DT` table
  (accession/gene; remove-selected + Clear All), condition-col / replicate-col selectInputs (from GCT
  cdesc), `shinyjqui::orderInput` condition ordering + per-condition replicate ordering with the UX
  refinements (reset buttons, bordered scroll cards, single-replicate static label, keyboard fallback,
  observer-dedup registry).
- [ ] **Task 5B — per-ome scope:** render condition/replicate controls per selected dataset; add
  "Apply the same setup to all datasets" checkbox. Persist confirmed `sample_order` per dataset (return
  value or `globals` slot) for Summary/Volcano to consume.
- [ ] **Task 5C — species refresh:** `checkboxGroupInput` of species + button → calls Phase-2H/2I fetch +
  annotation rebuild with `withProgress`; independent of Start-Analysis; writes
  `uniprot_features/`+`uniprot_membrane/` (creating mouse layout mirroring human).
- [ ] **Task 5D — Start-Analysis:** validation checklist (≥1 dataset; condition col per dataset; order
  confirmed; empty marker table is valid) with inline errors; disable button + staged `withProgress`
  ("Fetching… / Computing CV… / Building contrasts…") + cancel during fetch. On click, compute the
  per-dataset heavy objects **once** (CV table, exploded+FASTA-mapped cache, annotation, coverage,
  depth, missed-cleavage) into the per-dataset cache.
- [ ] Review (fable/opus) + sonnet test pass (launch, exercise Setup) → commit per task.

---

## Phase 6 — Summary section wiring

**Files:** Modify `R/tab_pelsa_section2.R`.
Spec: planning doc → *Summary section* (every metric + its source GCT + Mapping/annotation QC).

- [ ] **Task 6A — experiment-wide:** Total peptide IDs (`nrow(GCTs_original[[ome]])`, NO explode);
  per-protein coverage (Phase-2F); peptide-length **density** plot with dodged mean+median lines;
  missed-cleavage distribution (Phase-2C). All from cached Start-Analysis objects.
- [ ] **Task 6B — per-condition CV KDE** (Phase-2D table): one curve/condition in confirmed order, median
  line dodged, 99th-pctile xlim, ≥20-finite skip, the exact caption from the doc.
- [ ] **Task 6C — per-sample depth** bar + companion table (Phase-2E), ordered by `sample_order`.
- [ ] **Task 6D — Mapping/annotation QC:** inline counts (FASTA-unmatched, annotation-unmatched) +
  collapsible `DT` tables pinned at the BOTTOM (default collapsed): FASTA-unmatched columns
  (peptide sequence, accession, gene, Spectronaut `PEP.PeptidePosition`, reason); annotation-unmatched
  accession list. CSV-exportable.
- [ ] **Task 6E — exports:** return the per-ome export list (CV table, coverage, depth, unmatched tables).
- [ ] Review + sonnet test pass → commit per task.

---

## Phase 7 — Volcano section wiring

**Files:** Modify `R/tab_pelsa_section3.R`.
Spec: planning doc → *Volcano plot section* (reuse-from `tab_stat_plot.R`, stat-source gate, contrast
selector, color toggle, label modes, best-peptide panel, marker overlay, hover/pin, intensity panel).

- [ ] **Task 7A — stat-source gate:** consume `stat_results`/`stat_params` from the Statistics tab exactly
  as `statPlot_Tab_Server`; grey out the PELSA volcano with "Run a statistical analysis in the Statistics
  tab first" until `stat_results()` for the active dataset is non-empty.
- [ ] **Task 7B — registries + lazy contrast:** reuse `poi_registry`/`top_n_registry`/`label_mode_registry`
  keyed `"<ome>::<contrast_key>"`; seed `poi_registry` with the Setup marker list; contrast
  `selectInput`; render only active contrast, free prior on switch (retain registries).
- [ ] **Task 7C — plot:** `plotVolcano`→`ggplotly(..., source=ns("pelsa_volcano"), tooltip="text")` +
  `toWebGL()`; two-sided significance color OR feature-class color via a single `radioButtons` toggle;
  background thinned via Phase-3B (sibling-peptide fade in a separate trace, tied to pin); magenta
  `#FF00FF` marker peptides always drawn on top; label modes (all / best-per-marker / top-N=3) with the
  `<gene>_aa<pos>` `;`-joined labels; threshold line at empirical raw-p for `adj.P.Val==0.05`.
- [ ] **Task 7D — best-peptide panel** (toggle, default off): second volcano below, Phase-2G rollup.
- [ ] **Task 7E — hover/pin + intensity panel:** hover tooltip (accession/gene/position/length, no line
  plot); left-click pin → fixed left panel with metadata table + per-protein intensity line plot
  (Phase-3C) in a separate `plotlyOutput`; `plotlyProxy()` fade restyle on pin; `helpText` caption.
- [ ] **Task 7F — exports:** per-ome `list(volcano_plot=<pdf fn>, proteins_of_interest=<csv fn>,
  volcano_labels=<csv 12-col>, plotted_intensities=<csv fn>)`.
- [ ] Review + sonnet test pass → commit per task.

---

## Phase 8 — End-to-end integration check

**Files:** `tests/testthat/test-pelsa-integration.R`.
Spec: planning doc → Parity gate layer (3) (R-synthetic end-to-end; no Python).

- [ ] Run the assembled R pipeline on a **larger synthetic frame** (Phase-1 generator, more rows) and
  assert the final artifacts (per-condition CV table, a contrast's volcano-labels CSV, the intensity-line
  CSV) match the closed-form / structural expectations — catches integration drift (column order, join
  semantics, sample-order handling) the per-helper tests miss. Optional: one spot check on a real PELSA
  sample if available, but synthetic known-truth is the gate.
- [ ] `devtools::test()` full suite green; `devtools::check()` no new ERRORs/WARNINGs from PELSA code.
- [ ] Commit `test(pelsa): end-to-end synthetic integration check`.

---

## Phase 9 — Performance / resource-management review (opus agent team)

After Phases 0-8 are merged and green, dispatch an **opus agent team** to review the finished PELSA module
for performance, runtime, and resource-management (efficiency) improvements. Mandate:

- Verify the *Performance design rules* are honored everywhere: **no per-row `apply`/`for` over peptides**
  (the 54× benchmark trap), matrices not data.frames in hot paths, single vectorized group-bys,
  `data.table` `:=` in-place at 300k scale, `readr`/`readRDS` cache reads.
- Verify the memory contract: heavy objects computed **once** per Start-Analysis and cached; inactive
  dataset/contrast objects freed on switch (Performance notes + switcher behaviour); `toWebGL` for the
  volcano; thinning only the non-sig/low-logFC/non-marker cloud.
- Profile the assembled pipeline on a large synthetic frame; flag any stage >5× the expected vectorized
  time as a regression. Propose `future`/`furrr` parallelism only where it measurably helps after
  vectorization.
- Produce a prioritized findings list (CRITICAL/HIGH/MEDIUM) with concrete diffs; apply CRITICAL/HIGH.

---

## Self-review (orchestrator, before executing)

- **Spec coverage:** Setup (Phase 5) ✓, Summary all metrics + QC (Phase 6) ✓, Volcano reuse + new
  (Phase 7) ✓, all-R helpers + parity (Phases 1-3) ✓, switcher refactor (Phase 4) ✓, deps (Phase 0) ✓,
  performance review (Phase 9) ✓. **Wood's Plot intentionally out of scope** (user decision) — note it as
  a follow-up, do not stub. **Manual-UniProt parity** intentionally replaced by R-synthetic hand-set
  coordinates (user decision: no Python entanglement in tests).
- **Placeholder scan:** every helper task names its file, its ground-truth basis, and its commit message;
  no "TBD"/"add tests" — the test intent is spelled out per task.
- **Type/name consistency:** helper names referenced across phases match (`pelsa_build_multilabel` reused
  in 2C/2G/3A; `pelsa_within_condition_cv` table consumed in 6B; `matched`/`unmatched` cache shape from 2B
  consumed in 2F/2I/6D/7E; `sample_order` from Phase 5 consumed in 6B/6C/7E).
