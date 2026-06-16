# Phase 5 implementation plan (START-03, START-04)

Worktree branch: `worktree-agent-a3124dfd4b4414145` (based on `main`, behind live feature branch).
Scope: START-04 and START-03 ONLY. dp-1a (deep_clone_gct / df_deep_copy) is DEFERRED and untouched.
Both edits kept surgical and localized so they can be re-applied onto the latest branch.

## START-04 — bound second `.gct` read to the header region

File: `R/sidebar_setup_helpers_GCT-processing.R`, function `read_gct_cdesc_as_character`
(~line 621), called by `parse_gctx_preserve_cdesc` (~line 685).

### Current behavior
`lines <- readLines(file_path, warn = FALSE)` reads the WHOLE file (entire matrix) a
second time, then only uses:
- line 1: `#1.3` (version, unused here)
- line 2: dims `nrow ncol nrhd nchd`
- line 3: header row (column id row)
- lines 4 .. (3 + nchd): the cdesc metadata rows

So the maximum line index ever touched is `3 + nchd`. Everything after that is the data
matrix and is read+discarded for nothing. For a 12k-row GCT this reads ~12k extra lines.

### Approach
Read only the needed header lines via a text connection:
1. Open `con <- file(file_path, open = "r")` (text mode), `on.exit(close(con))`.
2. Read the first 2 lines: `head2 <- readLines(con, n = 2L, warn = FALSE)`.
3. Validate `length(head2) >= 2`. Parse dims from `head2[2]` exactly as before to get
   `nchd` (and `ncmat`, `nrhd`).
4. Compute `needed_after_dims <- 1L + nchd` (the header row + nchd metadata rows, i.e.
   lines 3 .. 3+nchd). Read them: `rest <- readLines(con, n = needed_after_dims, warn = FALSE)`.
5. Reconstruct a `lines` vector identical (for indices 1..3+nchd) to the old full read:
   `lines <- c(head2, rest)`. All downstream indexing (`lines[2L]`, `lines[3L]`,
   `lines[meta_start + i - 1L]`) stays byte-identical because we preserved 1-based offsets.
6. Keep the existing `length(lines) < 3L`, dims-validation, and
   `length(lines) < meta_end` guards unchanged — they now fire on the bounded vector,
   preserving the same error messages for short/truncated files.

This is byte-identical: `readLines` line-splitting (LF/CRLF handling, final newline) is
identical whether reading the whole file or capping `n`; we only stop early. We never read
the data matrix.

### Edge cases
- v1.3 with `nchd = 0`: `needed_after_dims = 1` -> reads only the header row (line 3),
  then the early `nchd <= 0` branch returns the id-only cdesc. Matches old path.
- v1.2 (`#1.2`): GCT 1.2 has dims line `nrow ncol` (no rhd/nchd). Old code:
  `nchd <- if (length(dims) >= 4 && !is.na(dims[4])) dims[4] else 0`. With only 2 dims,
  nchd=0 -> same id-only path. New code reads `1 + 0 = 1` line after dims. Identical.
  (Note: `parse_gctx` itself only handles `.gct`/`.gctx`; v1.2 column-meta recovery was
  already a no-op in the old code, so we preserve that.)
- Header shorter than `n`: `readLines(con, n = k)` returns fewer than `k` lines if EOF is
  hit; the existing `length(lines) < meta_end` guard then `stop()`s with the same message
  as before. We must NOT pad — keep `c(head2, rest)` so a truncated file yields a short
  `lines` vector and trips the same guard.
- A file with < 2 lines: `head2` has length < 2 -> add an explicit guard that produces the
  SAME "Invalid .gct file (expected at least 3 lines)" error as before (old code's
  `length(lines) < 3L`). We check `length(head2) < 2L` OR final `length(lines) < 3L`.

## START-03 — fix observer accumulation on file add/remove

File: `R/sidebar_setup.R`, the `observe({...})` at ~line 396 inside `setupSidebarServer`.

### Current behavior
Every time `accumulated_files()` invalidates (add / remove / clear), the `observe` re-runs
and `lapply`s a fresh `observeEvent(input[[btn_id]], ...)` for every current file. Old
observers are never destroyed, so they stack. N add/remove cycles => O(N x files)
observers. A stale observer for a removed file would still fire on its (now-absent) button;
worse, duplicate live observers on the SAME button id can multi-fire.

### Approach (register-once via a tracking reactiveVal)
1. Add `registered_remove_btns <- reactiveVal(character(0))` next to the other reactiveVals
   (~line 207).
2. In the `observe`, after computing the current files, build the set of needed btn_ids
   the same way the UI does (`gsub("[^a-zA-Z0-9_]", "_", name)` -> `remove_file_<id>`).
3. Only register an `observeEvent` for btn_ids NOT already in `registered_remove_btns()`;
   append the newly registered ids to the tracker.
4. The per-button handler is unchanged in logic. Because it looks up the file BY NAME at
   click time (`isolate(accumulated_files())`) and no-ops if the name is gone, keeping a
   handler alive after its file is removed is safe. Re-adding the same filename reuses the
   same btn_id; since it is already registered we skip re-registration and the existing
   handler still works.
5. Clear-all: leave handlers registered (they no-op when their file is absent). Do NOT
   reset the tracker on clear, otherwise re-adding a previously-cleared filename would
   register a SECOND handler for the same id (double-fire). Keeping the tracker monotonic
   per session guarantees exactly one live handler per btn_id ever.

This preserves exact behavior: add accumulates; per-row remove removes exactly that file;
clear-all empties; re-add works; duplicate filenames de-duplicated by the upload handler
as before. The only change is that a given button id gets exactly ONE observeEvent for the
life of the session instead of one per invalidation.

### Edge cases
- Remove the middle file: handler matches by name, removes exactly it; remaining files keep
  their (already-registered) handlers.
- Remove then re-add same name: same btn_id, already registered, skip — existing handler
  fires correctly against the refreshed file list.
- Clear-all then re-add: btn_id already registered, skip — single handler.
- `ignoreInit = TRUE, ignoreNULL = TRUE` semantics preserved on each handler.
- Namespacing: `input[[btn_id]]` uses the bare (un-namespaced) id, matching the existing
  code and the `ns(...)` used in the UI. Unchanged.
- The `observe` still early-returns once `GCTs_and_params()` is set (post-setup) and on
  empty/NULL files — unchanged.

## Validation
- START-04: standalone script parses tiny (4x4, nchd=2), proteome (nchd=12), acetylome
  (nchd=11) fixtures plus brca proteome written to temp `.gct`, comparing OLD vs NEW
  `parse_gctx_preserve_cdesc` for byte-identical `@mat,@rid,@cid,@rdesc,@cdesc,@version`.
- START-03: `shiny::testServer` driving add -> add -> remove-middle -> re-add -> clear,
  asserting `accumulated_files()` at each step and that a registration counter does not
  grow unbounded.
- Permanent regression tests added under `tests/testthat/`.
