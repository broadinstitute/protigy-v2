# PELSA Compound & Marker Preset Management — Design Spec

**Date:** 2026-06-18
**Status:** Approved (brainstormed + grilled)
**Area:** PELSA Setup module (`R/tab_pelsa_section1*.R`, `inst/pelsa/compound_markers.yaml`)

## Problem

During tech-dev, the preset compound → marker-protein lists used by PELSA Setup
live only in the source file `inst/pelsa/compound_markers.yaml`. Adding or
removing a compound, or re-markering an existing compound, requires hand-editing
that file in the source tree — slow and error-prone when the compound/marker
roster changes constantly.

Additionally, the current compound dropdown **merges** a newly selected
compound's preset markers into the existing marker table instead of replacing
them, so switching compounds accumulates markers from both.

## Goals

1. **Add a compound from the UI.** A text field + "Add" button on the PELSA
   Setup page creates a new compound with an empty marker preset list, persisted
   to the YAML.
2. **Set the default marker list for a compound from the UI.** A button below
   the marker table saves the table's current markers as that compound's preset
   list (full replace), behind a confirmation dialog, persisted to the YAML.
3. **Fix reselect semantics.** Selecting a different compound fully **replaces**
   the marker table with that compound's presets (selecting "(none)" clears it).

## Non-Goals

- No per-user writable fallback location. Presets persist only when the YAML's
  directory is writable (i.e. running from the source tree via
  `devtools::load_all`). A read-only installed package surfaces a clear error.
- No alias support. Aliases are removed entirely (see Decisions).
- No concurrency handling (last-writer-wins, matching `species_meta.json`).
- No preservation of YAML comments or per-marker inline notes.
- No `testServer`/shinytest2 harness for the new observers (pure-helper tests
  only, matching the existing section-1 test pattern).

## Architecture

All changes are confined to the existing PELSA Setup module and its pure
helpers. No new module, no change to the `GCTs_and_params()` / `globals`
data-flow contract.

1. **Writable YAML layer.** New pure helpers in `R/tab_pelsa_section1_helpers.R`
   operate on the *parsed compound-markers list* (immutable, testable). A thin
   atomic writer mirrors `pelsa_write_species_meta()` (tempfile in the YAML's own
   directory + `file.rename`), validates the directory is writable, and returns a
   logical success flag — it never silently swallows a user-initiated save.

2. **Two new UI affordances** in `pelsa_setup_box_ui()` (the Data-inputs layer):
   an "Add compound" text field + button under the compound dropdown, and a
   "Set as default marker list for this compound" button in the marker action
   row.

3. **Server wiring** in `R/tab_pelsa_section1.R`: a version-bump `reactiveVal`
   re-reads the YAML after a write; the compound dropdown is updated with a
   targeted `updateSelectInput` (the Setup box does **not** re-render on a write);
   the reselect observer changes from merge to replace.

The YAML file remains the single source of truth. The in-memory
`compound_markers()` reactive is invalidated (re-read) after any successful write
so dropdown and autofill stay consistent.

## Decisions (from grilling)

| # | Decision |
|---|----------|
| 1 | **No box re-render on write.** `renderUI` reads compound choices via `isolate(compound_markers())`; after a write, drive the dropdown with a targeted `updateSelectInput(session, "pelsa_compound", choices=, selected=)`. |
| 2 | **Keep the echo-guard, flip merge→replace.** `last_autofilled_compound` per-ome tracker stays (prevents a tab-switch re-emit from clobbering manual edits). A genuine reselect (different value) replaces the table; "(none)"/empty clears it. Re-picking the identical value can't re-fire (Shiny limitation) — accepted, no reset button. |
| 3 | **Empty-table save allowed.** "Set as default" with an empty table clears the compound's preset. No zero-marker guard; the confirm dialog's row-count text makes accidents obvious. Saving does not mutate the table. |
| 4 | **Plain `yaml::write_yaml`.** Accept comment loss; the file is now app-managed. `metadata` block preserved as data. |
| 5 | **No read-only fallback; explicit error.** On write failure, `showNotification(type="error")` names the cause (package library not writable; run from source via `devtools::load_all`) and includes the repo link `https://github.com/broadinstitute/protigy-v2.git`. |
| 6 | **Aliases removed entirely.** `.pelsa_resolve_compound_name` collapses to a single case-insensitive primary-key match. Strip `aliases:` from the committed YAML. Delete the alias autofill test. |
| 7 | **Name validation** (pure helper): trim → reject empty/whitespace → reject internal whitespace → reject non-ASCII → (server) dup-check case-insensitive primary key (block + notify + select existing). |
| 8 | **Add-compound rides existing observers.** Sequence: validate → dup-check → `pelsa_add_compound` → write → bump version → `updateSelectInput`. The resulting `input$pelsa_compound` change persists the selection and autofills the (empty) markers via the existing observers. No manual `set_ds`/`set_markers` in the add handler. |
| 9 | **Last-writer-wins.** No file-locking or re-read-before-write merge. |
| 10 | **Pure-helper tests only.** TDD on all new helpers; no `testServer` harness. |
| 11 | **Regenerate the committed YAML** in `yaml::write_yaml` output format (no comments, no `aliases:`), so the committed file is byte-identical to a future in-app save. `schema_version` stays `1`. Same three compounds/markers, minus aliases. |

## Components

### Pure helpers (`R/tab_pelsa_section1_helpers.R`)

```
.pelsa_resolve_compound_name(compound_markers, compound_name)
  -> primary key (case-insensitive match) or NA_character_. Alias loop removed.

pelsa_validate_compound_name(name)
  -> list(ok = TRUE, name = <trimmed>) | list(ok = FALSE, message = <chr>)
  Order: trim; empty/whitespace -> "Enter a compound name.";
  internal whitespace -> "Compound name cannot contain spaces.";
  non-ASCII (not ^[!-~]+$) -> "Compound name must be ASCII only."

pelsa_compound_exists(compound_markers, name)
  -> logical; case-insensitive match against primary keys only.

pelsa_add_compound(compound_markers, name)
  -> new parsed list with `name` added carrying markers = list() (empty).
  Errors if it already exists (caller checks first for the friendly message).
  Immutable.

pelsa_set_compound_markers(compound_markers, name, marker_rows)
  -> new parsed list where `name`'s $markers is fully replaced by rows
  (each row -> list(accession=, gene=) with gene omitted when NA). Resolves
  the name to its primary key and edits IN PLACE (preserves any other keys
  under that compound). Immutable.

pelsa_write_compound_markers(path, compound_markers)
  -> logical. Validates dirname(path) is a non-empty, writable directory;
  writes a tempfile in that directory then file.rename (atomic); serializes
  list(metadata=, compounds=) via yaml::write_yaml. Returns FALSE on any
  failure (never throws for a write error).
```

### UI (`pelsa_setup_box_ui` in `R/tab_pelsa_section1_helpers.R`)

- Under the compound `selectInput`: `textInput(ns("pelsa_new_compound"))` +
  `actionButton(ns("pelsa_add_compound_btn"), "Add compound")`.
- In the marker action row (with Remove/Clear):
  `actionButton(ns("pelsa_set_default_markers_btn"), "Set as default marker list for this compound")`.

### Server (`R/tab_pelsa_section1.R`)

- `compound_markers_version <- reactiveVal(0)`; `compound_markers()` depends on
  it and re-reads the YAML. `renderUI` reads choices via `isolate()`.
- **Add compound** (`observeEvent(input$pelsa_add_compound_btn)`):
  validate name → if dup, notify + `updateSelectInput(selected = existing)` →
  else `pelsa_add_compound` → `pelsa_write_compound_markers`:
  on success bump version, `updateSelectInput(choices = fresh keys, selected = new)`,
  clear text field; on failure show the read-only error notification.
- **Set as default** (`observeEvent(input$pelsa_set_default_markers_btn)`):
  require a selected compound (notify "select a compound first" otherwise) →
  `showModal(modalDialog(...))` naming the compound + row count, Confirm button
  id namespaced via `session$ns("pelsa_confirm_set_default")`, Cancel via
  `modalButton`. On confirm (observed bare): `pelsa_set_compound_markers` with the
  current table → `pelsa_write_compound_markers`: success bumps version, notifies,
  `removeModal`; failure shows error, keeps table, `removeModal`.
- **Reselect** (`observeEvent(input$pelsa_compound)`, line ~280): change merge to
  replace — non-empty compound → `set_markers(ome, pelsa_compound_marker_rows(...))`;
  "(none)"/empty → `set_markers(ome, pelsa_empty_marker_rows())`. Echo-guard kept.

### Data (`inst/pelsa/compound_markers.yaml`)

Regenerated via the writer, aliases stripped, comments gone, `schema_version: 1`.

## Error Handling

- Write failures: `showNotification(type="error")` — never swallowed. Message
  names the cause and includes the repo link.
- Name validation: structured `list(ok, message)` from the pure helper; server
  maps to a notification.
- A read returning `list(compounds = list())` (missing file) is tolerated as
  today.

## Testing

Extend `tests/testthat/test-pelsa-setup-controls.R` (and/or
`test-pelsa-marker.R`). TDD, pure helpers only:

- `pelsa_validate_compound_name`: trim, empty/whitespace, spaces, non-ASCII, ok.
- `pelsa_compound_exists`: exact, case-insensitive, miss.
- `.pelsa_resolve_compound_name`: case-insensitive primary key, no alias, NA miss.
- `pelsa_add_compound`: adds empty markers; errors on dup; immutability.
- `pelsa_set_compound_markers`: full replace, in-place (other keys kept),
  NA gene dropped, name resolved case-insensitively.
- `pelsa_write_compound_markers`: write to tempdir + re-read identically,
  `metadata` preserved, read-only/`""` dir → FALSE.
- Delete `pelsa_compound_marker_rows honors aliases`.

No `testServer` harness; observer wiring (add/set-default/modal/reselect/
`updateSelectInput`) is thin glue over tested helpers, verified by reasoning +
manual check.
