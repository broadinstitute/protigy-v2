# Performance Phase 4 - Package Attach Trim (START-01, START-02)

Goal: faster cold app open by (1) not attaching heavy Bioconductor/CRAN deps at
package load and calling them fully-qualified instead, and (2) removing dead deps.

> Note: the `docs/performance-implementation-phases.md` / `-review-v2.md` files do
> not exist in this worktree's git state. The two items (START-01, START-02) are
> nonetheless fully specified in the task brief, so this plan is derived directly
> from a grep audit of the codebase rather than from those docs.

## START-01 - Lazy-load heavy Bioconductor deps via `pkg::fn()`

All bare calls to the four target packages live in a SINGLE file:
`R/sidebar_setup_helpers_normalization.R`. Their `@importFrom` tags are inline in
that same file (NOT in `R/protigy-package.R`).

### Grep-verified conversion sites (BEFORE)

Bare calls (comments/roxygen excluded):

| Symbol                       | File:line | New form |
|------------------------------|-----------|----------|
| `normalize.quantiles(data)`  | normalization.R:133 | `preprocessCore::normalize.quantiles(data)` |
| `justvsn(data)`              | normalization.R:214 | `vsn::justvsn(data)` |
| `normalmixEM (...)`          | normalization.R:254 | `mixtools::normalmixEM(...)` |
| `normalmixEM (...)`          | normalization.R:255 | `mixtools::normalmixEM(...)` |
| `Mclust (...)`               | normalization.R:256 | `mclust::Mclust(...)` |
| `normalmixEM (...)`          | normalization.R:281 | `mixtools::normalmixEM(...)` |
| `normalmixEM (...)`          | normalization.R:282 | `mixtools::normalmixEM(...)` |

Counts: `normalize.quantiles` x1, `justvsn` x1, `normalmixEM` x4, `Mclust` x1,
`mclustBIC` x0 (only referenced via `@importFrom`, never called directly; `Mclust`
calls it internally - resolved through the package namespace once `mclust` is in
Imports, so no source change needed).

### roxygen `@importFrom` tags to REMOVE (file: normalization.R)

- line 91: `#' @importFrom preprocessCore normalize.quantiles`
- line 92: `#' @importFrom vsn justvsn`
- line 222: `#' @importFrom mixtools normalmixEM`
- line 223: `#' @importFrom mclust Mclust mclustBIC`

`R/protigy-package.R` has NO `@importFrom` for these four packages, so nothing to
remove there.

### DESCRIPTION

Keep `vsn`, `preprocessCore`, `mixtools`, `mclust` in Imports (still required for
`pkg::fn()` to resolve). No DESCRIPTION change for START-01.

### Namespace verification

- `vsn::justvsn` - confirmed via Context7 (Bioconductor vsn vignette: `justvsn(...)`).
- `preprocessCore::normalize.quantiles`, `mixtools::normalmixEM`,
  `mclust::Mclust`, `mclust::mclustBIC` - confirmed by the existing authoritative
  `@importFrom` tags in source (these tags are how the package currently resolves
  the symbols, i.e. ground-truth for symbol->package mapping).

## START-02 - Remove dead deps `furrr`, `future`, `WriteXLS`

### Grep-verified dead-dep audit (R/ + tests/, plus reflection patterns)

| Dep      | Only references found | Verdict |
|----------|-----------------------|---------|
| `furrr`  | `R/protigy-package.R:42` `@importFrom furrr future_map future_map2` only. No `future_map`/`future_map2` calls anywhere. | DEAD - remove |
| `future` | `R/protigy-package.R:41` `@importFrom future plan availableCores` only. Other "future" hits are English words in comments in `R/tab_stat_setup.R`. No `plan()`/`availableCores()` calls. | DEAD - remove |
| `WriteXLS`| `R/protigy-package.R:28` `@importFrom WriteXLS WriteXLS` only. No `WriteXLS(...)` calls. | DEAD - remove |

Also checked for `do.call`, `getExportedValue`, `requireNamespace`, `loadNamespace`,
`library()`, `require()` referencing any of these - none. No test file attaches them.

### Edits

`R/protigy-package.R` - remove three lines:
- `#' @importFrom WriteXLS WriteXLS` (line 28)
- `#' @importFrom future plan availableCores` (line 41)
- `#' @importFrom furrr future_map future_map2` (line 42)

`DESCRIPTION` Imports - remove `WriteXLS`, `future`, `furrr`.

## Self-review - weaknesses / risks

1. **`mclustBIC` not called directly.** `Mclust` calls `mclustBIC` internally; once
   `mclust` is an Imports dependency, R resolves it through the namespace. Removing
   the `@importFrom mclust ... mclustBIC` tag is therefore safe. Verified: no bare
   `mclustBIC` call exists in source.
2. **`pkg::fn` requires the package in Imports.** All four (vsn, preprocessCore,
   mixtools, mclust) remain in DESCRIPTION Imports - confirmed present. `::` will
   resolve.
3. **Reflection / dynamic dispatch grep can miss things.** Explicitly grepped
   `do.call|getExportedValue|requireNamespace|loadNamespace|library(|require(|::`
   intersected with all six package names - zero hits beyond the roxygen lines.
4. **Tests attaching packages.** Grepped `tests/` for `library(`/`require(` of these
   six - none.
5. **`@import` (full) vs `@importFrom`.** None of the six are full `@import`ed; only
   `@importFrom` (or, for the lazy ones, inline `@importFrom`). No risk of a
   wildcard import keeping them attached.
6. **NAMESPACE regeneration.** After edits, `devtools::document()` must regenerate
   NAMESPACE so the dropped importFrom lines disappear. Then `load_all` + `test`.
7. **`R CMD check` "Imports not imported from" note.** Because vsn/preprocessCore/
   mixtools/mclust are now used only via `::`, R CMD check is happy (it treats `::`
   as declared usage). They stay in Imports so no "unused Import" issue. furrr/
   future/WriteXLS are fully removed from both places so no dangling note.

## Validation plan (post-edit)

1. `devtools::document()` then `devtools::load_all(".")` - must succeed.
2. Re-grep: zero bare `justvsn|normalize.quantiles|normalmixEM|Mclust` calls;
   zero `furrr|future_map|WriteXLS` references; zero `@importFrom` for the six.
3. `devtools::test()` - report pass/fail.
4. `devtools::check()` if feasible; else defer to CI and report.
