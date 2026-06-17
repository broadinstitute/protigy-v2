# Pre-Release Fix Plan — feat/pelsa-integration

Plain-language plan for the fixes agreed from the final review. Each item shows **what happens now**,
**what happens after**, and **the fix**. Complex items include a worked example. Work proceeds in phases;
**after each phase: run the affected tests + a code review, then commit, then move to the next phase.**

> **Validation status (all confirmed before planning):**
> - The 4 "should-fix" items and the 5 "M10" processing items were each validated by an Opus subagent.
> - M7 was re-examined against the "peptide-level reports => unique PEP.StrippedSequence" contract and **demoted** (see Phase 4).
> - cmapR usage was audited: the branch already uses cmapR well; only M8/M9 should *add* a cmapR call.
> - The API was overloaded during planning, so the systemic-pattern and cmapR audits were finished inline by the main agent rather than a subagent team.

---

## Phase 0 — Packaging & portability blockers (B1, B2, B3)
Fast, low-risk, and they unblock a clean `R CMD check`. Do these first.

### B1. `%||%` is used everywhere but never imported -> app breaks on the R versions we claim to support
- **Plain language:** `%||%` ("use the left value, or the right one if the left is missing") is used in ~15 files. It only became part of base R in version 4.4. Our package says it supports R 4.0+. On R 4.0-4.3 the app would crash with *"could not find function `%||%`"*. It works on your machine only because you run a newer R.
- **Now -> After:** Crashes on R 4.0-4.3 -> works on all supported versions.
- **Fix:** add `#' @importFrom rlang %||%` in `R/protigy-package.R` (rlang is already a dependency) and re-run `devtools::document()`. No logic changes.

### B2. 70 MB of FASTA ships inside the package
- **Plain language:** Two reference protein files (41 MB human + 29 MB mouse) sit in `inst/database/.../fasta/` and are **not** excluded from the built package. CRAN's hard limit is 5 MB; this also flags on Bioconductor.
- **Now -> After:** Built tarball is ~70 MB and rejected -> the FASTAs stay in the repo for dev/runtime but are excluded from the built package.
- **Fix:** add a pattern like `^inst/database/.*\.fasta$` to `.Rbuildignore`. (If they must be available at runtime after install, that's a separate "download-on-demand" design decision — flag for you; default is build-ignore.)

### B3. 13 non-ASCII characters in R source
- **Plain language:** Em-dashes (a non-ASCII character) appear in comments of 4 files. Project rule (CLAUDE.md) is ASCII-only in `R/` because non-ASCII can trip `R CMD check`.
- **Now -> After:** Potential check NOTE/WARNING + rule violation -> clean ASCII.
- **Fix:** replace the em-dashes with ` -- ` in `app_ui.R` (2), `sidebar_setup.R` (5), `sidebar_setup_helpers_discrete-cache.R` (4), `utilities.R` (2). Add a CI grep guard so it can't regress.

**Phase 0 gate:** `devtools::document()` + `devtools::check()` (expect the non-ASCII and size NOTEs to clear) + `devtools::load_all()` + targeted tests; review; commit.

---

## Phase 1 — User-facing PELSA interaction bugs (M4, M5/M6)

### M4. The dataset switcher shows three copies of the same control, and they fall out of sync
- **Plain language:** All three PELSA tabs draw a "which dataset?" button bar using the **same internal name** (`pelsa_active_dataset`). The app builds all tabs up front, so three identical-named controls exist at once (invalid HTML), and picking a dataset on one tab doesn't update the others. Which dataset actually gets used depends on internal ordering, so what a tab *shows* can disagree with what it *analyzes*.
- **Example:** You're on the Volcano tab and select "phospho". You switch to the Coverage tab — it still shows "proteome" highlighted, and depending on timing the analysis may run on the wrong one.
- **Now -> After:** Duplicate IDs + silent desync -> one source of truth; switching on any tab is reflected everywhere and matches what's analyzed.
- **Fix (pick one, lower-risk first):** (a) render only the *active* tab's switcher, or (b) keep one canonical control and add a top-level `observeEvent(input$pelsa_active_dataset, updateRadioGroupButtons(...))` that syncs the others. Minimum bar: a sync observer so the visible selection can never diverge from `active_dataset()`. Files: `tab_pelsa_container.R` (switcher renderUI), `app_ui.R:117/122/127`.

### M5 + M6. The "Add accession to marker list" button can silently do nothing while showing a green "Added" toast
- **Plain language:** Two separate problems make the new button unreliable. **M5:** after you add a marker, the volcano keeps showing a cached version that ignored the change, so the new marker isn't highlighted until you switch contrast. **M6:** the "add" request is sent through a channel that only reacts when the value *changes*; re-adding the exact same accession sends an identical value, so the receiver doesn't fire — but a success toast pops up anyway.
- **Example (M6):** Add accession A -> remove A in Setup -> click "Add A" again. The request payload is identical to last time, the receiver ignores it, A is not re-added, but you still see "Added A to the marker list."
- **Now -> After:** Button sometimes no-ops with a misleading success message -> every click reliably updates the marker list and the volcano refreshes; the toast only shows on the path that actually added.
- **Fix:** (M5) have the active volcano data depend on `marker_accessions()` (not an isolated/cached read), or clear the volcano cache when markers change. (M6) make each request distinct (wrap with a monotonic counter/nonce) **or** have the consumer reset the channel after handling it; show the success toast only when an add actually happens. Files: `tab_pelsa_section3.R:362-405,994-1006`, `tab_pelsa_section1.R:209-218`.

**Phase 1 gate:** `testServer`-driven tests for the switcher sync and the add-marker flow (add / remove / re-add-same / verify highlight refresh); review; commit.

---

## Phase 2 — PELSA data correctness (M2, M3)

### M2. Splitting multi-protein rows can attach the wrong gene/position when an accession field has an empty middle entry
- **Plain language:** A peptide row can list several proteins separated by `;`. The code splits accessions, genes, and positions into parallel lists and lines them up. When the accession field has an **empty middle slot**, the empty accession is dropped *before* lining up, but the gene/position index still points into the *un-pruned* list — so genes/positions shift and attach to the wrong protein.
- **Example:** `accession="A;;B"`, `gene="GA;GMID;GB"`, `pos="10;99;20"`. Correct: A->GA/10, B->GB/20. Current (buggy): A->GA/10, **B->GMID/99**. (Only *interspersed* empties cause this; trailing empties are harmless.)
- **Now -> After:** Wrong gene/position silently assigned for interspersed-empty accession rows -> each kept accession keeps its own gene/position.
- **Fix:** line up accession/gene/position **before** dropping empties, then apply the same "keep" mask to all three together. File: `tab_pelsa_explode_helpers.R:41-70,138-161`. Add a regression test with an interspersed-empty row.

### M3. The Woods coverage track crashes for very short proteins
- **Plain language:** The live Woods panel builds tick marks with a step size derived from protein length. For a protein **shorter than 10 residues** the step calculation produces an invalid sequence and R errors out; because the panel is only wrapped in `suppressWarnings` (not error-catching), the user sees a broken plot. The export path is already safe.
- **Now -> After:** Broken plot for short/edge proteins -> ticks render (or are simply sparse) and the panel always draws.
- **Fix:** guard the tick sequence so it isn't built when length < 10 (mirror the export path's safe `seq(0, len, by=10)` then drop 0). File: `tab_pelsa_woods_helpers.R:270`. Add a regression test with `prot_len = 5`.

**Phase 2 gate:** new explode + Woods regression tests + existing PELSA tests; review; commit.

---

## Phase 3 — GCT / processing layer (M8/M9, M11) — *cmapR-first where it fits*
> **Scope note:** M10 is **deferred** to a separate end-to-end data-handling review (see the M10 section).
> Active Phase 3 scope is **M8/M9** (CV alignment) and **M11** (export-failure detection).

### M8/M9. Coefficient-of-variation (CV) values rely on row positions matching between two tables that aren't guaranteed to match
- **Plain language:** CV is computed from the *original* (pre-processing) data, while the peptide list comes from the *processed* data. They're joined **by row position**, assuming both tables have the same rows in the same order. Nothing checks that. If processing ever drops or reorders rows, CV silently attaches to the wrong peptides.
- **Example:** Processing removes 3 all-empty rows. The original table still has them, so from that point on every CV value is shifted by 3 rows — quietly wrong, no error.
- **Now -> After:** Positional join (latent silent misalignment) -> join by stable peptide **id** so it's correct regardless of row order, or a hard assert if ids can't be used.
- **Fix (cmapR-first):** align the original GCT to the processed peptide order **by id** using `cmapR::subset_gct(original_gct, rid = processed_rids)` instead of positional indexing (this is the one place the audit says to *add* a cmapR call). If a full key join isn't feasible, at minimum assert the two matrices share `rid`/row order and stop with a clear message otherwise. Files: `tab_pelsa_analysis_helpers.R:708-768`.
- **Extra testing (you asked for this):** build small **synthetic GCT fixtures** simulating every scenario — identical order (baseline), rows dropped by filtering, rows reordered, duplicate ids, single row, zero rows — and assert CV maps to the correct peptide in each. This is the robustness gate for this item.

### M10. Five processing-layer issues (validated: all real; 2 active, 3 latent) — **DEFERRED**
> **DEFERRED (2026-06-16):** M10 is pulled out of this release pass. M10.2 and M10.3 both turn out to be
> symptoms of one unsettled question — *who owns sample/feature identity (names) as data moves through
> normalize -> filter -> recombine -> merge*. Patching the five sites piecemeal risks masking that
> structure. M10 will be addressed after a **comprehensive end-to-end review of data handling** in the
> app (a separate workstream). The verified findings below are preserved so the tracing is not lost.
>
> **Line numbers verified against HEAD on 2026-06-16.** The remaining Phase 3 scope is **M8/M9 + M11 only.**
- **M10.1 (latent)** `GCT-processing.R:881` reads `output_list$data.log.trans` which only works by accidental prefix-matching of `data.log.transform` (the field is `data.log.transform`, returned at `:1020`; sibling caller `:781` already uses the exact name). **Fix:** use the exact name. *Risk if a second `data.log.transform*` field is ever added: `$` becomes ambiguous and returns `NULL` silently.*
- **M10.2 (active)** Sample-name mangling crashes/​corrupts the **StdDev** filter path on names with spaces/dashes/`+`/leading-digit. **Two mangling sites, both need `check.names = FALSE`** (verified end-to-end):
  - `GCT-processing.R:1105` `data.frame(data, id = rownames(data))` mangles `S-1`->`S.1`; then `sd.filter` (`data-filtering.R:24`) selects `tab[, names(grp.vec)]` by the *original* names -> hard *"undefined columns selected"* error.
  - `data-filtering.R:48` `data.frame(ids, tab)` re-mangles even after `:1105` is fixed; the mangled names flow back as `data.filtered` colnames and then `GCT-processing.R:914` `rownames(cdesc) %in% colnames(data.filtered)` fails to match -> empty `cdesc` / `cid`-`cdesc` mismatch -> corrupt GCT.
  - **`janitor` rejected:** sample names are identifiers that must round-trip (cdesc/cid/merge-key/plots/exports); cleaning them forces a rename-everywhere-then-restore dance and risks collisions (`S-1` and `S.1` both -> `s_1`). The `method=="None"` branch already proves the rest of the pipeline handles non-syntactic names fine. `check.names = FALSE` (base R, 2 args) is the correct minimal fix.
- **M10.3 (latent)** `GCT-processing.R:914-915` reorders `cdesc`/`rdesc` with `%in%` (keeps *old* row order) while `cid`/`rid` use `data.filtered` order; the merge conflict check (`:1264-1268`) then compares `gct@cdesc[[col]]` (gct order) against a `cid`-ordered subset position-by-position -> **false conflicts** that spawn spurious `col.<ome>` columns and corrupt merged annotations. **Fix:** (a) name-index the reorder — `cdesc[colnames(data.filtered), , drop=FALSE]` / `rdesc[rownames(data.filtered), , drop=FALSE]`; (b) index the comparison's left side by `samples_in_ome` too so both sides share order. (`subset_gct` doesn't fit — `data.filtered` is a transformed matrix, not a subset of the input GCT.)
- **M10.4 (active, rare)** `utilities.R:111` "trim from the end" fallback uses end index `1` instead of `-1` -> `str_sub(x, -trim_length, 1)` returns `""` for strings longer than `trim_length`. Only hit on the both-ends-equally-unique tie with `default_trim="end"`. **Fix:** `str_sub(x, -trim_length, -1)`.
- **M10.5 (active)** `tab_stat_setup_helpers.R:155-160`: `AveExpr.*` columns are in factor-level order (`f <- factor(..., levels = unique(groups_valid))`, design `~0+f`), but `aggregate()` returns group means **alphabetically**; the positional assignment transposes them when the orders differ (e.g. levels `Tumor, Normal`). **Fix:** compute means in factor-level order — `avg <- sapply(levels(f), function(lv) rowMeans(data[, f==lv, drop=FALSE], na.rm=TRUE))`.

### M11. Export failures are silently hidden
- **Plain language:** The export step checks "did it work?" by testing whether a directory exists — but that directory always exists, so failures are never recorded. A module that fails to export simply vanishes from the zip with no warning.
- **Now -> After:** Failed exports silently dropped -> failures are detected and surfaced.
- **Fix:** capture success/failure from the actual `tryCatch` around each module export and record real failures. File: `tab_export.R:250`.

**Phase 3 gate:** synthetic-GCT CV alignment tests (all scenarios) + export-failure tests; `devtools::check()`; review; commit. *(M10's processing / F-test / merge tests move to the deferred data-handling review.)*

---

## Phase 4 — Robustness & hygiene (FASTA dup warning, error logging, thread-block, optional guards)

### FASTA duplicate-accession warning *(you requested this be added despite being unlikely)*
- **Plain language:** FASTA records are keyed by accession. If two records share an accession, only the first sequence is kept. Both shipped FASTAs have **zero** duplicates today, but a custom/edited FASTA could.
- **Now -> After:** Silent first-wins on duplicate accessions -> a warning loop detects duplicates and notifies the user (lists the duplicated accessions); behavior otherwise unchanged.
- **Fix:** in `pelsa_read_fasta()` (`tab_pelsa_fasta_helpers.R`), after building keys: `dups <- unique(keys[duplicated(keys)]); if (length(dups)) warning(...)`, and surface a `showNotification` at the load site so the user sees it. Test with a fixture FASTA containing a duplicated accession.

### Should-fix #3 — log swallowed export errors (Medium)
- **Plain language:** The PELSA export aggregator wraps each section in a "if it fails, return nothing" guard (intentional resilience), but throws away the error message, so a real bug just makes a section's exports disappear with no trace.
- **Fix:** keep the resilient flow, but add `warning("PELSA export (section N) failed: ", conditionMessage(e))` in each of the 3 catches. File: `app_server.R:198-200`.

### Should-fix #4 — remove the 100 ms thread freeze (Low)
- **Plain language:** Removing the last uploaded file calls `Sys.sleep(0.1)` to "let the notification show," but that's a misunderstanding of how Shiny sends messages — it just freezes the single R thread for 100 ms and changes nothing visible.
- **Fix:** delete `Sys.sleep(0.1)` and its comment. File: `sidebar_setup.R:497-498`.

### Optional (validated as not reachable / moot — include only if you want defense-in-depth)
- **Cache-write path guard:** species come from a fixed checkbox list, so traversal isn't reachable in this local app; optionally `intersect(selected, pelsa_list_species(...))` at the input boundary (also covers the read-path twins). **Decision needed.**
- **M7 fallback guard:** under the peptide-level contract, `PEP.StrippedSequence` is unique so the fallback join is safe; optionally add a `warning()` if duplicate keys are ever seen in the fallback, to protect against precursor-level/malformed input. **Decision needed.**

**Phase 4 gate:** FASTA-dup test + export-logging behavior + remove-file flow tests; review; commit.

---

## Cross-cutting: systemic patterns (addressed incrementally, not a separate phase)
- **Silent swallowing** (pattern #3): the genuine offenders (`app_server.R` export catch, `tab_export.R:250`, `export_helpers` `dir.create`, `section2.R:333`) are folded into Phases 3-4 above. Add a "log, don't swallow" rule going forward.
- **Inconsistent malformed-span policy** (pattern #4): M3 (Woods crash) is the acute case fixed in Phase 2. Broader unification (coverage hard-stop vs annotate warn-drop vs silent filter) is noted as a **follow-up**, not part of this release pass, to avoid scope creep.
- **match()-first-hit** (pattern #1 / M7): demoted to optional guard (Phase 4) per the peptide-level-uniqueness contract.

## Execution model
- **Per phase:** implement -> run affected tests (`devtools::load_all` first) -> code review (Opus subagent/agent-team when the API is healthy; inline review if overloaded) -> `devtools::check()` for phases touching the package surface (0, 3) -> commit -> next phase.
- **GCT-handling steps** use cmapR where the audit said it fits (M8/M9 via `subset_gct`); elsewhere the existing `cmapR::GCT()`/`parse_gctx` usage is already correct and kept.
- **Ordering rationale:** Phase 0 (cheap, unblocks check) -> Phase 1 (visible UX bugs) -> Phase 2 (data correctness) -> Phase 3 (processing/GCT, heaviest testing) -> Phase 4 (hygiene/robustness).
</content>
