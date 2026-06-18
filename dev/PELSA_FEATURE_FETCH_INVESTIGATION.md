# PELSA UniProt Feature-Fetch Investigation

**Date:** 2026-06-18
**Scope:** Why the mouse (10090) `uniprot_features.tsv` cache is under-populated and
contained wrong-species data.
**Method:** Systematic debugging — root cause before fixes. All claims below were
verified by standalone testing against live `rest.uniprot.org`.

---

## Symptom

- Mouse `inst/database/10090/uniprot_features/uniprot_features.tsv`: ~260 rows / 44
  accessions, then ~437 rows / 54 accessions after a from-FASTA rebuild.
- Human `inst/database/9606/uniprot_features/uniprot_features.tsv`: 364,345 rows /
  33,179 accessions (genome-scale, correct).
- Mouse FASTA is healthy: 47,069 sequences, normal `sp|`/`tr|` UniProt headers.

## Four compounding defects (all confirmed)

### Defect #1 — Wrong-species universe (the human "spillover")
`pelsa_refresh_accession_universe()` (R/tab_pelsa_refresh_helpers.R:117-150) builds the
fetch universe from the **uploaded dataset's accessions** whenever ANY dataset is loaded
(line 121, 136), and applies that SAME universe to EVERY checked species in
`pelsa_run_species_refresh()` (R/tab_pelsa_refresh_helpers.R:595). The species' own FASTA
is only consulted in the no-dataset fallback (line 141).

So: a human dataset loaded + both 9606 and 10090 checked → mouse refresh fetches the
**human** accessions and writes human features into `10090/`. Nothing checks that the
returned organism matches the target folder (accessions are globally unique and
organism-agnostic in UniProt).

**Evidence:** All 44 accessions in the original mouse cache resolve to *Homo sapiens*
(organism 9606) in live UniProt; 44/44 are present in the human FASTA AND in the human
feature cache; 0/44 are in the mouse FASTA.

| Accession | UniProt organism | Gene |
|---|---|---|
| V9GYY3 | 9606 Homo sapiens | MTHFD1 |
| X6R3N0 | 9606 Homo sapiens | SLC27A3 |
| R4GNH3 | 9606 Homo sapiens | PSMC3 |
| U3KPR8 | 9606 Homo sapiens | MEIS1 |
| S4R325 | 9606 Homo sapiens | URGCP-MRPS24 |

### Defect #2 — Batch size 200 exceeds UniProt's 100-OR query limit
`.PELSA_BATCH_SIZE <- 200L` (R/tab_pelsa_uniprot_fetch.R:288). The fetch builds
`accession:(P1 OR P2 OR ... OR P200)` (`.pelsa_accession_query`, line 300). UniProt's
`/search` caps OR conditions at **100**.

**Evidence (boundary test):**
- n=100 → HTTP 200, 100 results
- n=101 → HTTP 400 `"Too many OR conditions in query. Maximum allowed is 100."`
- n=200 → HTTP 400

A 4xx is deliberately treated as "healthy server rejecting the query → 0 entries,
accessions fall into `unresolved`" and does NOT trip the circuit breaker
(R/tab_pelsa_uniprot_fetch.R:352-367). So every full 200-batch silently returned zero
entries. On the 47,069-accession mouse FASTA universe (sorted), only the trailing
`47069 mod 200 = 69`-accession batch was ≤100 and survived — producing exactly the 54
`Z4Y*` accessions observed. No error surfaced.

### Defect #3 — Isoform-suffixed accessions never match `/search`
17.4% of the mouse universe (8,176 of 47,069) carry an isoform suffix (`P12345-3`).
UniProt's `/search` `accession:` filter indexes only the BASE accession, so an
isoform-suffixed term matches NOTHING.

**Evidence:** In a 500-accession representative slice, 73 of 152 unresolved (48%) were
isoforms. Querying their 50 distinct BASE accessions → 0/50 unresolved (all resolve).
The code already strips isoform suffixes for *resolved-status accounting*
(`.pelsa_isoform_base`, used at R/tab_pelsa_uniprot_fetch.R:533) but still SENDS the
suffixed accession in the query.

Per-batch breakdown of the 500-sample (batch_size=100): `page_results = 100 −
n_isoforms_in_batch` exactly (batches 1-4: 97,98,82,71 results for 3,2,18,29 isoforms).

### Defect #4 — Non-UniProt accessions (smORFs/contaminants) 400 the whole batch
The mouse FASTA filename is `...553smORFs.264contams.fasta`. ~572 of 47,069 universe keys
are non-UniProt tokens (`smORF_G035940|LINC02081.2`, `B99901`...). The pipe-aware parser
extracts these as "accessions"; UniProt rejects the malformed value with HTTP 400, which
**drops the entire 100-accession batch** (up to 99 valid accessions lost with it).

**Evidence:** Batch 5 of the 500-sample (100 valid-looking terms) returned HTTP 400:
`"The 'accession' filter value 'smORF_G035940|LINC02081.2' has invalid format..."`.
Format check: 46,497 valid / 572 invalid accession keys in the universe.

## Combined effect explains all observations
- Original 44-accession cache: a small human dataset (≤100 accs = 1 sub-100 batch) →
  Defect #1 wrong species, but Defect #2 didn't bite (small batch).
- From-FASTA rebuild 54 accessions: Defect #2 dropped every full 200-batch; only the
  69-accession remainder survived.
- A batch-100 mouse rebuild still loses ~30%: Defects #3 (isoforms) + #4 (smORFs/contams
  400-ing batches).

## Gold-standard baseline (MEASURED — full independent fetch)
`dev/mouse_feature_baseline.R` (independent httr2 + parser, NOT the package functions)
computed the reference on 2026-06-18:

- **516,646 feature rows** across **34,408 distinct accessions**
- 38,325 base accessions queried; **38,325 resolved (100%)**; **0 unresolved**
- 572 invalid-format keys dropped (553 smORFs + 264 contaminants overlap); 0 failed batches
- runtime ~288 s; feature_type spread: Compositional bias 59,928 / Region 53,424 /
  Modified residue 50,645 / Domain 46,072 / Transmembrane 37,225 / Binding site 35,833 ...
- 4 random resolved accessions verified active 10090 (Nipal3, Unc13d, Mga, Atp6v0e2);
  Q8BGN5 feature rows coherent (TM helices/chain/regions correctly classified)

Artifacts (gitignored, `dev/baseline/`): `mouse_10090_features.baseline.tsv`,
`mouse_10090_baseline.summary.json`. This is the SUCCESS CRITERION the integrated app
result is diffed against (join on accession+feature_type+start+end ⇒ ~0 difference).

(Earlier partial probe, for the record: a 500-accession slice at batch_size=100 BEFORE the
isoform/format fixes gave 69.6% resolve / 4,695 rows; with #3+#4 fixed the full run reaches
100% resolve as above.)

## Files implicated
- `R/tab_pelsa_uniprot_fetch.R` — `.PELSA_BATCH_SIZE`, `.pelsa_accession_query`,
  `.pelsa_fetch_one_batch`, `pelsa_fetch_uniprot`, `.pelsa_isoform_base`.
- `R/tab_pelsa_refresh_helpers.R` — `pelsa_refresh_accession_universe`,
  `pelsa_run_species_refresh`, `pelsa_species_refresh_inputs`.

## Standalone scripts produced
- `dev/rebuild_mouse_feature_cache.R` — one-off FASTA-fallback rebuild (not committed).
