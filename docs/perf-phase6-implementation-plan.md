# Phase 6 implementation plan — EXP-4 + EXP-5 (EXP-2 DEFERRED)

Scope: `R/tab_export.R` ONLY, `downloadHandler` content function (~L129-277).
EXP-2 (`write.csv` -> `readr::write_csv`) is explicitly OUT of scope.

## EXP-4 — temp-dir cleanup
The per-export temp dir `exports_dir` (created at L137 via `dir.create(exports_dir, recursive = T)`)
is never removed -> leaks one dir per export in a long-running session.

Fix: immediately after `dir.create(exports_dir, recursive = T)` add
```r
on.exit(unlink(exports_dir, recursive = TRUE), add = TRUE)
```

Why this is safe re: the returned zip:
- `file` (the zip the handler returns) lives directly under `zip_dir == tempdir()`, NOT inside
  `exports_dir` (`exports_dir <- file.path(zip_dir, dir_name)`; `file` basename strips the extension
  to form `dir_name`, so `file` and `exports_dir` are SIBLINGS). `unlink(exports_dir)` therefore
  cannot delete `file`.
- `on.exit` runs at content-function exit, which is AFTER `zip::zip(file, ...)` has already written
  the archive. So the zip is fully built before the source dir is removed.
- Runs on BOTH success and error (default `on.exit` semantics) -> no leak even if an export throws
  before reaching the zip step.
- `add = TRUE` so it composes with any future `on.exit` (none currently). It is the only on.exit,
  so ordering is moot today; `add=TRUE` is forward-safe.
- `unlink(..., recursive=TRUE)` on a path is idempotent/safe if already gone (returns 0); no
  double-unlink hazard.

## EXP-5 — single-pass reactive snapshot
Currently each `exports[[tab_name]]` reactive is evaluated TWICE:
- pre-loop L177-188 (compute `total_exports`)
- write loop L195-201 (`exports_all_omes <- exports[[tab_name]]()` again)

Fix: snapshot each selected tab's export object ONCE before progress/writing.

```r
# snapshot each selected tab's export object exactly once
exports_snapshot <- lapply(selected_tabs, function(tab_name) {
  if (is.reactive(exports[[tab_name]])) exports[[tab_name]]() else exports[[tab_name]]
})
names(exports_snapshot) <- selected_tabs

# total from snapshot
total_exports <- 0
for (tab_name in selected_tabs) {
  exports_all_omes <- exports_snapshot[[tab_name]]
  for (ome in intersect(selected_omes, names(exports_all_omes))) {
    total_exports <- total_exports + length(exports_all_omes[[ome]])
  }
}
```
Write loop: replace the `if (is.reactive(...)) exports[[tab_name]]() else exports[[tab_name]]`
block with `exports_all_omes <- exports_snapshot[[tab_name]]`.

I keep the explicit `for` loop for `total_exports` rather than the doc's `vapply`/`lengths`
one-liner, to preserve EXACT current counting behavior (`length()` per ome over the
`intersect(selected_omes, names(...))` set) and avoid edge-case divergence on empty intersects.

### Behavior preserved
- Same files written: write loop iterates the identical structure, now read from the snapshot
  (same object the reactive would have returned — reactives are deterministic within one flush).
- Same names, same per-element `is.reactive(p)` handling inside the inner loop (UNCHANGED).
- Same `total_exports` / progress count: computed from the snapshot, which equals the second
  evaluation's value.
- Same reactive-vs-non-reactive handling at the TAB level: the snapshot's ternary is identical to
  the original two call sites.
- `selected_omes` / `selected_tabs` handling unchanged (`intersect`, NULL-safe).

### Edge cases
- Empty/NULL `selected_tabs`: `lapply(NULL, ...)` -> `list()`; `names(list()) <- character(0)` ok;
  `total_exports = 0`; write `lapply(NULL, ...)` no-ops. (Same as before.)
- Empty/NULL `selected_omes`: `intersect(NULL, names(e))` -> `character(0)`; total 0; no folders.
- A tab whose reactive errors: it would error at snapshot time instead of pre-loop time — same net
  effect (the whole handler aborts; on.exit still cleans up exports_dir). No behavior regression:
  previously it errored in the pre-loop at the same logical point (before any writing).
- Non-reactive tab entry: snapshot stores it directly; write loop reads it directly. Identical.

## Validation
1. Harness `dev/perf-phase6-harness.R`: replicate the content-function body for OLD and NEW with a
   synthetic `exports` list (mix reactive + non-reactive, multi-ome, multiple named closures).
   Instrument a per-tab call counter. Assert:
   (a) file set + contents identical OLD vs NEW,
   (b) total_exports identical,
   (c) each tab reactive evaluated exactly ONCE in NEW (twice in OLD),
   (d) exports_dir removed after handler returns, zip intact.
2. `devtools::test()` + existing export tests.
3. Permanent regression test `tests/testthat/test-export-hygiene.R` for call-once + cleanup.
