# PELSA Compound & Marker Preset Management Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let users add compounds and set a compound's default marker-protein list from the PELSA Setup page (persisted to `compound_markers.yaml`), and make selecting a compound replace rather than append the marker table.

**Architecture:** All work is confined to the PELSA Setup module. New pure, immutable helpers in `R/tab_pelsa_section1_helpers.R` mutate and write the parsed compound-markers list; the server (`R/tab_pelsa_section1.R`) wires two new controls + a confirm modal, re-reads the YAML via a version-bump `reactiveVal`, drives the dropdown with a targeted `updateSelectInput` (no Setup-box re-render), and flips the compound-reselect observer from merge to replace.

**Tech Stack:** R, Shiny (`shinydashboardPlus`, `DT`), `yaml` package, `testthat` + `withr`. Design spec: `docs/superpowers/specs/2026-06-18-pelsa-compound-management-design.md`.

## Global Constraints

- **Reload before testing:** after editing `R/`, run `devtools::load_all(".")`. Tests exercise the loaded package, not source files.
- **ASCII-only R source:** no literal non-ASCII bytes in `R/` (use `\uXXXX` escapes if ever needed). The committed YAML must also be ASCII-only.
- **`ns()` rule:** namespace every `inputId`/`outputId` in module UI and inside server-created UI (`modalDialog`); do NOT namespace when referencing `input$`/`output$` or in `update*Input()`.
- **`%||%`** is imported from rlang in `R/protigy-package.R` — available; not base R before 4.4.
- **Immutability:** helpers return new objects; never mutate inputs.
- **Repo link** used in the read-only error message, verbatim: `https://github.com/broadinstitute/protigy-v2.git`.
- **No alias support:** aliases are removed everywhere in this change.
- **`schema_version` stays `1`** in the regenerated YAML.

---

### Task 1: Narrow `.pelsa_resolve_compound_name` to case-insensitive primary key (remove aliases)

**Files:**
- Modify: `R/tab_pelsa_section1_helpers.R:174-185` (the `.pelsa_resolve_compound_name` function)
- Modify: `tests/testthat/test-pelsa-setup-controls.R:145-153` (delete the alias test)
- Test: `tests/testthat/test-pelsa-setup-controls.R`

**Interfaces:**
- Produces: `.pelsa_resolve_compound_name(compound_markers, compound_name)` → primary key (case-insensitive match) or `NA_character_`. No alias matching.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-pelsa-setup-controls.R` (after the existing `pelsa_compound_marker_rows` tests):

```r
# ---- .pelsa_resolve_compound_name (case-insensitive, no aliases) -------------

test_that(".pelsa_resolve_compound_name matches case-insensitively on key", {
  cm <- list(compounds = list(Rapamycin = list(markers = list()),
                              AY9944    = list(markers = list())))
  expect_identical(.pelsa_resolve_compound_name(cm, "Rapamycin"), "Rapamycin")
  expect_identical(.pelsa_resolve_compound_name(cm, "rapamycin"), "Rapamycin")
  expect_identical(.pelsa_resolve_compound_name(cm, "RAPAMYCIN"), "Rapamycin")
})

test_that(".pelsa_resolve_compound_name ignores aliases and returns NA on miss", {
  cm <- list(compounds = list(
    Rapamycin = list(aliases = list("Sirolimus"), markers = list())
  ))
  expect_true(is.na(.pelsa_resolve_compound_name(cm, "Sirolimus")))
  expect_true(is.na(.pelsa_resolve_compound_name(cm, "Nonexistent")))
})

test_that(".pelsa_resolve_compound_name returns NA for empty compound set", {
  expect_true(is.na(.pelsa_resolve_compound_name(list(compounds = list()), "X")))
})
```

Delete the existing alias test at `tests/testthat/test-pelsa-setup-controls.R:145-153`:

```r
test_that("pelsa_compound_marker_rows honors aliases", {
  ...
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: the two case-insensitive assertions FAIL (current code matches exact key + alias, not lowercased), or the alias-ignore test FAILS (current code returns "Rapamycin" for "Sirolimus").

- [ ] **Step 3: Rewrite the function**

Replace `R/tab_pelsa_section1_helpers.R:174-185` with:

```r
# Resolve a compound name to its primary key in the parsed compound-marker list.
# Matching is by primary key, CASE-INSENSITIVE. Aliases are NOT consulted (the
# preset model is name-keyed only). Returns NA_character_ when nothing matches.
# @noRd
.pelsa_resolve_compound_name <- function(compound_markers, compound_name) {
  compounds <- compound_markers$compounds
  if (length(compounds) == 0L) return(NA_character_)
  keys <- names(compounds)
  hit <- which(tolower(keys) == tolower(compound_name))
  if (length(hit) == 0L) return(NA_character_)
  keys[[hit[[1]]]]
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: PASS (all resolve tests pass; the deleted alias test no longer runs).

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section1_helpers.R tests/testthat/test-pelsa-setup-controls.R
git commit -m "refactor(pelsa): drop aliases, make compound name resolution case-insensitive"
```

---

### Task 2: `pelsa_validate_compound_name` helper

**Files:**
- Modify: `R/tab_pelsa_section1_helpers.R` (add the helper after `.pelsa_resolve_compound_name`)
- Test: `tests/testthat/test-pelsa-setup-controls.R`

**Interfaces:**
- Produces: `pelsa_validate_compound_name(name)` → `list(ok = TRUE, name = <trimmed chr>)` on success, or `list(ok = FALSE, message = <chr>)` on failure.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-pelsa-setup-controls.R`:

```r
# ---- pelsa_validate_compound_name --------------------------------------------

test_that("pelsa_validate_compound_name trims and accepts a valid name", {
  res <- pelsa_validate_compound_name("  AY-9944  ")
  expect_true(res$ok)
  expect_identical(res$name, "AY-9944")
})

test_that("pelsa_validate_compound_name rejects empty / whitespace-only", {
  for (x in list("", "   ", NA_character_, NULL)) {
    res <- pelsa_validate_compound_name(x)
    expect_false(res$ok)
    expect_match(res$message, "Enter a compound name")
  }
})

test_that("pelsa_validate_compound_name rejects internal whitespace", {
  res <- pelsa_validate_compound_name("U 18666A")
  expect_false(res$ok)
  expect_match(res$message, "cannot contain spaces")
})

test_that("pelsa_validate_compound_name rejects non-ASCII", {
  res <- pelsa_validate_compound_name("Rapamycinµ")
  expect_false(res$ok)
  expect_match(res$message, "ASCII only")
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: FAIL with `could not find function "pelsa_validate_compound_name"`.

- [ ] **Step 3: Implement the helper**

Add to `R/tab_pelsa_section1_helpers.R` (after `.pelsa_resolve_compound_name`):

```r
# Validate a user-typed compound name. Returns a structured result so the server
# can map failures to a notification without embedding rules in the observer.
#
# Rules, in order: trim; reject empty/whitespace-only; reject internal
# whitespace; reject non-ASCII (only printable ASCII, no spaces, is allowed).
# Duplicate detection is NOT done here (it needs the live preset list and lives
# in the server, via pelsa_compound_exists()).
#
# @param name a length<=1 character (or NULL/NA) typed name.
# @return list(ok = TRUE, name = <trimmed>) | list(ok = FALSE, message = <chr>).
# @noRd
pelsa_validate_compound_name <- function(name) {
  if (is.null(name) || length(name) != 1L || is.na(name)) {
    return(list(ok = FALSE, message = "Enter a compound name."))
  }
  name <- trimws(as.character(name))
  if (!nzchar(name)) {
    return(list(ok = FALSE, message = "Enter a compound name."))
  }
  if (grepl("[[:space:]]", name)) {
    return(list(ok = FALSE, message = "Compound name cannot contain spaces."))
  }
  # Allowed: printable ASCII excluding space (0x21-0x7E).
  if (!grepl("^[!-~]+$", name)) {
    return(list(ok = FALSE, message = "Compound name must be ASCII only."))
  }
  list(ok = TRUE, name = name)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section1_helpers.R tests/testthat/test-pelsa-setup-controls.R
git commit -m "feat(pelsa): add pelsa_validate_compound_name helper"
```

---

### Task 3: `pelsa_compound_exists` + `pelsa_add_compound` helpers

**Files:**
- Modify: `R/tab_pelsa_section1_helpers.R` (add after `pelsa_validate_compound_name`)
- Test: `tests/testthat/test-pelsa-setup-controls.R`

**Interfaces:**
- Consumes: `.pelsa_resolve_compound_name` (Task 1).
- Produces:
  - `pelsa_compound_exists(compound_markers, name)` → logical (case-insensitive primary-key match).
  - `pelsa_add_compound(compound_markers, name)` → new parsed list with `name` added carrying `markers = list()`. Errors if it already exists.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-pelsa-setup-controls.R`:

```r
# ---- pelsa_compound_exists ---------------------------------------------------

test_that("pelsa_compound_exists matches case-insensitively, key only", {
  cm <- list(compounds = list(Rapamycin = list(markers = list())))
  expect_true(pelsa_compound_exists(cm, "Rapamycin"))
  expect_true(pelsa_compound_exists(cm, "rapamycin"))
  expect_false(pelsa_compound_exists(cm, "AY9944"))
  expect_false(pelsa_compound_exists(list(compounds = list()), "X"))
})

# ---- pelsa_add_compound ------------------------------------------------------

test_that("pelsa_add_compound adds a compound with empty markers", {
  cm  <- list(compounds = list())
  out <- pelsa_add_compound(cm, "NewCmpd")
  expect_true("NewCmpd" %in% names(out$compounds))
  expect_identical(out$compounds$NewCmpd$markers, list())
  # immutability: original untouched
  expect_length(cm$compounds, 0L)
})

test_that("pelsa_add_compound errors when the compound already exists", {
  cm <- list(compounds = list(Rapamycin = list(markers = list())))
  expect_error(pelsa_add_compound(cm, "rapamycin"), "already exists")
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: FAIL with `could not find function "pelsa_compound_exists"`.

- [ ] **Step 3: Implement the helpers**

Add to `R/tab_pelsa_section1_helpers.R`:

```r
# Does a compound named `name` already exist (case-insensitive, primary key)?
# @noRd
pelsa_compound_exists <- function(compound_markers, name) {
  !is.na(.pelsa_resolve_compound_name(compound_markers, name))
}

# Add a new compound carrying an empty markers list. Immutable: returns a new
# parsed list. Errors if the compound already exists (the caller is expected to
# check pelsa_compound_exists() first for a friendly message).
# @noRd
pelsa_add_compound <- function(compound_markers, name) {
  if (pelsa_compound_exists(compound_markers, name)) {
    stop(sprintf("pelsa_add_compound(): compound '%s' already exists.", name),
         call. = FALSE)
  }
  compounds <- compound_markers$compounds
  if (is.null(compounds)) compounds <- list()
  compounds[[name]] <- list(markers = list())
  out <- compound_markers
  out$compounds <- compounds
  out
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section1_helpers.R tests/testthat/test-pelsa-setup-controls.R
git commit -m "feat(pelsa): add pelsa_compound_exists + pelsa_add_compound helpers"
```

---

### Task 4: `pelsa_set_compound_markers` helper

**Files:**
- Modify: `R/tab_pelsa_section1_helpers.R` (add after `pelsa_add_compound`)
- Test: `tests/testthat/test-pelsa-setup-controls.R`

**Interfaces:**
- Consumes: `.pelsa_resolve_compound_name` (Task 1).
- Produces: `pelsa_set_compound_markers(compound_markers, name, marker_rows)` → new parsed list where the resolved compound's `$markers` is fully replaced by rows built from `marker_rows` (a `data.frame(accession, gene)`); each marker is `list(accession=)` plus `gene=` only when non-NA. Other keys under the compound are preserved. Errors if the compound is unknown.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-pelsa-setup-controls.R`:

```r
# ---- pelsa_set_compound_markers ----------------------------------------------

test_that("pelsa_set_compound_markers fully replaces markers in place", {
  cm <- list(compounds = list(
    Rapamycin = list(markers = list(list(accession = "OLD", gene = "OLDGENE")))
  ))
  rows <- data.frame(accession = c("P1", "P2"),
                     gene       = c("G1", NA_character_),
                     stringsAsFactors = FALSE)
  out <- pelsa_set_compound_markers(cm, "rapamycin", rows)  # case-insensitive

  mk <- out$compounds$Rapamycin$markers
  expect_length(mk, 2L)
  expect_identical(mk[[1]]$accession, "P1")
  expect_identical(mk[[1]]$gene, "G1")
  expect_identical(mk[[2]]$accession, "P2")
  expect_null(mk[[2]]$gene)  # NA gene is dropped, not written as NA
  # immutability
  expect_identical(cm$compounds$Rapamycin$markers[[1]]$accession, "OLD")
})

test_that("pelsa_set_compound_markers accepts an empty table (clears markers)", {
  cm  <- list(compounds = list(X = list(markers = list(list(accession = "P1")))))
  out <- pelsa_set_compound_markers(cm, "X", pelsa_empty_marker_rows())
  expect_identical(out$compounds$X$markers, list())
})

test_that("pelsa_set_compound_markers errors on unknown compound", {
  cm <- list(compounds = list())
  expect_error(
    pelsa_set_compound_markers(cm, "Ghost", pelsa_empty_marker_rows()),
    "unknown compound"
  )
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: FAIL with `could not find function "pelsa_set_compound_markers"`.

- [ ] **Step 3: Implement the helper**

Add to `R/tab_pelsa_section1_helpers.R`:

```r
# Replace a compound's marker preset list with `marker_rows` (a
# data.frame(accession, gene)). The compound is resolved to its primary key
# (case-insensitive) and edited IN PLACE, so any other keys it carries are
# preserved. An empty data.frame clears the preset (markers = list()). A marker
# with an NA/empty gene is written with accession only (no `gene` key).
# Immutable: returns a new parsed list. Errors if the compound is unknown.
# @noRd
pelsa_set_compound_markers <- function(compound_markers, name, marker_rows) {
  key <- .pelsa_resolve_compound_name(compound_markers, name)
  if (is.na(key)) {
    stop(sprintf("pelsa_set_compound_markers(): unknown compound '%s'.", name),
         call. = FALSE)
  }
  if (!is.data.frame(marker_rows) ||
      !all(c("accession", "gene") %in% names(marker_rows))) {
    stop("pelsa_set_compound_markers(): `marker_rows` must be a data.frame with ",
         "columns accession and gene.", call. = FALSE)
  }

  markers <- lapply(seq_len(nrow(marker_rows)), function(i) {
    acc  <- as.character(marker_rows$accession[[i]])
    gene <- marker_rows$gene[[i]]
    entry <- list(accession = acc)
    if (!is.null(gene) && !is.na(gene) && nzchar(as.character(gene))) {
      entry$gene <- as.character(gene)
    }
    entry
  })

  out <- compound_markers
  out$compounds[[key]]$markers <- markers
  out
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section1_helpers.R tests/testthat/test-pelsa-setup-controls.R
git commit -m "feat(pelsa): add pelsa_set_compound_markers helper"
```

---

### Task 5: `pelsa_write_compound_markers` atomic writer

**Files:**
- Modify: `R/tab_pelsa_section1_helpers.R` (add after `pelsa_set_compound_markers`)
- Test: `tests/testthat/test-pelsa-setup-controls.R`

**Interfaces:**
- Consumes: `pelsa_read_compound_markers` (existing), `pelsa_add_compound`/`pelsa_set_compound_markers` (Tasks 3-4) for round-trip tests.
- Produces: `pelsa_write_compound_markers(path, compound_markers)` → logical. Validates `dirname(path)` exists and is writable; serializes `list(metadata=, compounds=)` to a tempfile in that directory then `file.rename`s it onto `path`. Returns `FALSE` on any failure (never throws for a write error). Preserves the `metadata` block.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-pelsa-setup-controls.R`:

```r
# ---- pelsa_write_compound_markers --------------------------------------------

test_that("pelsa_write_compound_markers round-trips and preserves metadata", {
  tmp  <- withr::local_tempdir()
  path <- file.path(tmp, "compound_markers.yaml")
  cm <- list(
    metadata  = list(description = "test", schema_version = 1),
    compounds = list(Rapamycin = list(markers = list(
      list(accession = "P42345", gene = "MTOR")
    )))
  )
  expect_true(pelsa_write_compound_markers(path, cm))

  back <- pelsa_read_compound_markers(path)
  rows <- pelsa_compound_marker_rows(back, "Rapamycin")
  expect_identical(rows$accession, "P42345")
  expect_identical(rows$gene, "MTOR")

  raw <- yaml::read_yaml(path)
  expect_identical(raw$metadata$description, "test")
  expect_equal(raw$metadata$schema_version, 1)  # round-trips as numeric, not int
})

test_that("pelsa_write_compound_markers returns FALSE for a non-writable dir", {
  cm <- list(metadata = list(), compounds = list())
  # A path inside a directory that does not exist -> dirname not a real dir.
  bad <- file.path(tempfile(), "nope", "compound_markers.yaml")
  expect_false(pelsa_write_compound_markers(bad, cm))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: FAIL with `could not find function "pelsa_write_compound_markers"`.

- [ ] **Step 3: Implement the writer**

Add to `R/tab_pelsa_section1_helpers.R`:

```r
# Atomically write the parsed compound-marker list back to `path` as YAML.
#
# Mirrors pelsa_write_species_meta(): write a tempfile in the SAME directory as
# the target (so file.rename is atomic on one filesystem), then rename onto the
# target. The directory must exist and be writable; otherwise this returns FALSE
# without attempting a write (the caller surfaces a user-facing error). Plain
# yaml::write_yaml is used (comments are NOT preserved). The metadata block is
# written verbatim from the list. Returns TRUE on success, FALSE on any failure.
# @noRd
pelsa_write_compound_markers <- function(path, compound_markers) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(FALSE)
  }
  dir <- dirname(path)
  if (!dir.exists(dir) || file.access(dir, mode = 2L) != 0L) {
    return(FALSE)
  }
  payload <- list(
    metadata  = compound_markers$metadata %||% list(),
    compounds = compound_markers$compounds %||% list()
  )
  ok <- tryCatch({
    tmp <- tempfile(tmpdir = dir, fileext = ".yaml")
    yaml::write_yaml(payload, tmp)
    file.rename(tmp, path)
  }, error = function(e) FALSE)
  isTRUE(ok)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section1_helpers.R tests/testthat/test-pelsa-setup-controls.R
git commit -m "feat(pelsa): add pelsa_write_compound_markers atomic writer"
```

---

### Task 6: Regenerate `compound_markers.yaml` in writer format (strip aliases + comments)

**Files:**
- Modify: `inst/pelsa/compound_markers.yaml` (full replace)
- Test: `tests/testthat/test-pelsa-setup-controls.R` (existing `pelsa_compound_marker_rows` test still passes)

**Interfaces:**
- Consumes: `pelsa_write_compound_markers` (Task 5).

- [ ] **Step 1: Regenerate the file with the writer**

Run this one-off in R so the committed file is byte-identical to a future in-app save (do NOT hand-edit the YAML):

```r
R -q -e '
devtools::load_all(".")
cm <- list(
  metadata = list(
    description = "Preset PELSA marker proteins per treatment compound.",
    schema_version = 1
  ),
  compounds = list(
    Rapamycin = list(markers = list(
      list(accession = "P42345", gene = "MTOR"),
      list(accession = "P62942", gene = "FKBP1A"),
      list(accession = "Q13451", gene = "FKBP5")
    )),
    AY9944 = list(markers = list(
      list(accession = "Q9UBM7", gene = "DHCR7"),
      list(accession = "Q15125", gene = "EBP")
    )),
    "U-18666A" = list(markers = list(
      list(accession = "O15118", gene = "NPC1"),
      list(accession = "Q9UHC9", gene = "NPC1L1"),
      list(accession = "Q15125", gene = "EBP")
    ))
  )
)
stopifnot(pelsa_write_compound_markers("inst/pelsa/compound_markers.yaml", cm))
cat(readLines("inst/pelsa/compound_markers.yaml"), sep = "\n")
'
```

Expected: the printed file has no comments, no `aliases:` blocks, a `metadata:` block with `schema_version: 1`, and the three compounds with their markers.

- [ ] **Step 2: Verify reads + ASCII-only**

Run:

```r
R -q -e '
devtools::load_all(".")
cm <- pelsa_read_compound_markers("inst/pelsa/compound_markers.yaml")
stopifnot(setequal(names(cm$compounds), c("Rapamycin","AY9944","U-18666A")))
stopifnot(setequal(pelsa_compound_marker_rows(cm,"U-18666A")$accession,
                   c("O15118","Q9UHC9","Q15125")))
# ASCII-only guard
raw <- readBin("inst/pelsa/compound_markers.yaml", "raw", file.info("inst/pelsa/compound_markers.yaml")$size)
stopifnot(all(as.integer(raw) < 128))
cat("OK\n")
'
```

Expected: prints `OK`.

- [ ] **Step 3: Run the existing setup-controls tests**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: PASS (the existing `pelsa_compound_marker_rows returns accession+gene rows` test against the real file still passes).

- [ ] **Step 4: Commit**

```bash
git add inst/pelsa/compound_markers.yaml
git commit -m "chore(pelsa): regenerate compound_markers.yaml in writer format, drop aliases"
```

---

### Task 7: Add the two new controls to `pelsa_setup_box_ui`

**Files:**
- Modify: `R/tab_pelsa_section1_helpers.R:711-742` (the compound `selectInput` + marker section inside `pelsa_setup_box_ui`)
- Test: `tests/testthat/test-pelsa-setup-controls.R` (UI-presence check) and/or `tests/testthat/test-pelsa-app-ui.R`

**Interfaces:**
- Produces: the Setup box now contains inputs `pelsa_new_compound`, `pelsa_add_compound_btn`, and `pelsa_set_default_markers_btn` (all namespaced via `ns`).

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-pelsa-setup-controls.R`:

```r
# ---- pelsa_setup_box_ui new controls -----------------------------------------

test_that("pelsa_setup_box_ui exposes add-compound + set-default controls", {
  ui <- pelsa_setup_box_ui(
    species   = c("Human" = "9606"),
    compounds = c("Rapamycin"),
    ns        = function(x) paste0("PELSA-", x)
  )
  html <- as.character(ui)
  expect_match(html, "PELSA-pelsa_new_compound")
  expect_match(html, "PELSA-pelsa_add_compound_btn")
  expect_match(html, "PELSA-pelsa_set_default_markers_btn")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: FAIL (the three ids are not present in the markup).

- [ ] **Step 3: Add the controls**

In `R/tab_pelsa_section1_helpers.R`, modify the compound `selectInput` block (currently lines 713-718) to append the add-compound row immediately after it:

```r
    # 3. Treatment compound (presets from compound_markers.yaml).
    #    Selecting a compound REPLACES THIS dataset's marker table with its
    #    presets ("(none)" clears the table).
    shiny::selectInput(
      ns("pelsa_compound"),
      label   = "Treatment compound",
      choices = c("(none)" = "", compounds),
      selected = selected_compound
    ),

    # 3b. Add a new compound (empty preset) to compound_markers.yaml.
    shiny::tags$div(
      class = "pelsa-add-compound",
      shiny::textInput(
        ns("pelsa_new_compound"),
        label       = "Add a new compound",
        placeholder = "e.g. AY-9944 (no spaces, ASCII only)"
      ),
      shiny::actionButton(
        ns("pelsa_add_compound_btn"), "Add compound",
        icon = shiny::icon("plus")
      )
    ),
```

Then, in the marker action row (currently lines 737-741), add the set-default button after "Clear all":

```r
    shiny::div(
      style = "margin-top: 8px;",
      shiny::actionButton(ns("pelsa_remove_markers"), "Remove selected"),
      shiny::actionButton(ns("pelsa_clear_markers"), "Clear all"),
      shiny::actionButton(
        ns("pelsa_set_default_markers_btn"),
        "Set as default marker list for this compound",
        icon = shiny::icon("floppy-disk")
      )
    )
```

- [ ] **Step 4: Run test to verify it passes**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section1_helpers.R tests/testthat/test-pelsa-setup-controls.R
git commit -m "feat(pelsa): add 'Add compound' + 'Set as default markers' controls to Setup UI"
```

---

### Task 8: Version-bump re-read + isolate the dropdown choices in renderUI

**Files:**
- Modify: `R/tab_pelsa_section1.R:146-148` (the `compound_markers` reactive)
- Modify: `R/tab_pelsa_section1.R:211` (the `compounds =` arg in `pelsa_setup_box_ui`)

**Interfaces:**
- Produces: `compound_markers_version` (a `reactiveVal(0)`) and a `compound_markers()` reactive that re-reads the YAML whenever the version bumps; `renderUI` reads compound choices via `isolate(compound_markers())` so a write does NOT re-render the Setup box.

- [ ] **Step 1: Add the version reactiveVal + make compound_markers depend on it**

Replace `R/tab_pelsa_section1.R:144-148` with:

```r
    # Bumping this forces compound_markers() to re-read the YAML (after an in-app
    # add-compound / set-default write) without re-rendering the whole Setup box.
    compound_markers_version <- reactiveVal(0)

    # Re-read the compound presets on Setup entry, whenever the box renders, and
    # whenever a write bumps the version, so user edits show up without a restart.
    compound_markers <- reactive({
      compound_markers_version()
      pelsa_read_compound_markers(pelsa_compound_markers_path())
    })
```

- [ ] **Step 2: Isolate the choices read in renderUI**

In `R/tab_pelsa_section1.R`, change line 211 from:

```r
        compounds = names(compound_markers()$compounds),
```

to:

```r
        # isolate(): the dropdown is driven by a targeted updateSelectInput after
        # a write, so renderUI must NOT take a dependency on compound_markers()
        # (that would re-render the entire Setup box on every preset write).
        compounds = isolate(names(compound_markers()$compounds)),
```

- [ ] **Step 3: Reload + sanity-check the app builds**

Run: `R -q -e 'devtools::load_all("."); cat("loaded OK\n")'`
Expected: prints `loaded OK` with no error.

- [ ] **Step 4: Run the PELSA test suite (regression)**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section1.R
git commit -m "feat(pelsa): version-bump re-read of compound presets, isolate dropdown choices"
```

---

### Task 9: Flip the compound-reselect observer from merge to replace

**Files:**
- Modify: `R/tab_pelsa_section1.R:280-290` (the autofill `observeEvent(input$pelsa_compound)`)

**Interfaces:**
- Consumes: `pelsa_compound_marker_rows`, `pelsa_empty_marker_rows`, `cur_markers`, `set_markers`, `active_setup_ome`, `last_autofilled_compound` (all existing).

- [ ] **Step 1: Replace the merge body with replace semantics**

Replace `R/tab_pelsa_section1.R:280-290` (the `observeEvent(input$pelsa_compound, {...})` that currently merges) with:

```r
    observeEvent(input$pelsa_compound, {
      ome <- active_setup_ome(); req(ome)
      compound <- input$pelsa_compound
      tracker  <- last_autofilled_compound()
      # Echo guard: a box re-render (e.g. a setup-tab switch) re-emits the
      # persisted compound value. Skip when it is unchanged FOR THIS OME, so a
      # re-emit cannot clobber markers the user edited after autofill.
      if (identical(compound, tracker[[ome]])) return()

      if (is.null(compound) || !nzchar(compound)) {
        # "(none)" -> clear the table entirely.
        set_markers(ome, pelsa_empty_marker_rows())
      } else {
        # A genuine reselect REPLACES the table with this compound's presets
        # (a brand-new compound has none -> empty table).
        new_rows <- pelsa_compound_marker_rows(compound_markers(), compound)
        set_markers(ome, new_rows)
      }
      tracker[[ome]] <- compound
      last_autofilled_compound(tracker)
    })
```

- [ ] **Step 2: Reload + sanity-check**

Run: `R -q -e 'devtools::load_all("."); cat("loaded OK\n")'`
Expected: prints `loaded OK`.

- [ ] **Step 3: Verify replace logic at the helper level (already covered)**

Run: `R -q -e 'devtools::load_all("."); devtools::test_active_file("tests/testthat/test-pelsa-setup-controls.R")'`
Expected: PASS (the replace uses `pelsa_compound_marker_rows`, which is tested; the observer is thin glue).

- [ ] **Step 4: Commit**

```bash
git add R/tab_pelsa_section1.R
git commit -m "fix(pelsa): replace marker table on compound reselect instead of appending"
```

---

### Task 10: Add-compound server handler

**Files:**
- Modify: `R/tab_pelsa_section1.R` (add a new `observeEvent` in the marker section, after the paste-box Add handler at ~line 299)

**Interfaces:**
- Consumes: `pelsa_validate_compound_name`, `pelsa_compound_exists`, `pelsa_add_compound`, `pelsa_write_compound_markers`, `pelsa_compound_markers_path`, `compound_markers`, `compound_markers_version`, `active_setup_ome`.
- Produces: the read-only error message string (reused in Task 11) — define it once as `pelsa_readonly_save_msg` near the top of the marker section.

- [ ] **Step 1: Define the shared read-only error message**

In `R/tab_pelsa_section1.R`, just before the marker handlers (right after the `set_markers` definition near line 237), add:

```r
    # Shared message when a preset write fails (read-only package library). Note:
    # ASCII-only; the repo link lets the user clone + run from source to persist
    # presets. Reused by the add-compound and set-default handlers.
    pelsa_readonly_save_msg <- paste0(
      "Could not save the preset: the package library is not writable. ",
      "Run Protigy from the source tree (devtools::load_all) to manage presets. ",
      "Source: https://github.com/broadinstitute/protigy-v2.git"
    )
```

- [ ] **Step 2: Add the add-compound handler**

In `R/tab_pelsa_section1.R`, after the paste-box `observeEvent(input$pelsa_add_markers, ...)` (ends ~line 299), add:

```r
    # Add a brand-new compound (empty preset) and persist it to the YAML. On
    # success the dropdown is updated + the new compound selected (which the
    # existing pelsa_compound observers then persist + autofill-empty); on a
    # read-only library the write fails and we surface the actionable error.
    observeEvent(input$pelsa_add_compound_btn, {
      req(active_setup_ome())
      v <- pelsa_validate_compound_name(input$pelsa_new_compound)
      if (!isTRUE(v$ok)) {
        showNotification(v$message, type = "warning", duration = 5)
        return()
      }
      cm <- compound_markers()
      if (pelsa_compound_exists(cm, v$name)) {
        # Block + select the existing one (by its primary key) so the user can
        # edit its markers instead.
        existing <- .pelsa_resolve_compound_name(cm, v$name)
        showNotification(
          sprintf("Compound '%s' already exists.", existing),
          type = "warning", duration = 5
        )
        updateSelectInput(session, "pelsa_compound", selected = existing)
        return()
      }
      new_cm <- pelsa_add_compound(cm, v$name)
      ok <- pelsa_write_compound_markers(pelsa_compound_markers_path(), new_cm)
      if (!ok) {
        showNotification(pelsa_readonly_save_msg, type = "error", duration = 10)
        return()
      }
      # Re-read, refresh the dropdown choices, select the new compound (rides the
      # existing pelsa_compound observers), and clear the text field.
      compound_markers_version(compound_markers_version() + 1)
      choices <- c("(none)" = "", names(compound_markers()$compounds))
      updateSelectInput(session, "pelsa_compound",
                        choices = choices, selected = v$name)
      updateTextInput(session, "pelsa_new_compound", value = "")
      showNotification(sprintf("Added compound '%s'.", v$name),
                       type = "message", duration = 4)
    })
```

- [ ] **Step 3: Reload + sanity-check**

Run: `R -q -e 'devtools::load_all("."); cat("loaded OK\n")'`
Expected: prints `loaded OK`.

- [ ] **Step 4: Manual smoke (documented, not automated)**

Launch `Protigy::launchApp()`, upload data, open PELSA Setup. Type `TestCmpd` → Add compound → it appears selected, table empty, and `inst/pelsa/compound_markers.yaml` gains a `TestCmpd:` entry. Type `rapamycin` → Add → blocked with "already exists" and Rapamycin selected. (Revert the test edit to the YAML afterward.)

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section1.R
git commit -m "feat(pelsa): add-compound handler persists new compound to YAML"
```

---

### Task 11: Set-as-default handler with confirm modal

**Files:**
- Modify: `R/tab_pelsa_section1.R` (add handlers after the add-compound handler from Task 10)

**Interfaces:**
- Consumes: `pelsa_set_compound_markers`, `pelsa_write_compound_markers`, `pelsa_compound_markers_path`, `compound_markers`, `compound_markers_version`, `cur_markers`, `active_setup_ome`, `pelsa_readonly_save_msg`, `session$ns`.

- [ ] **Step 1: Add the trigger handler (opens the confirm modal)**

In `R/tab_pelsa_section1.R`, after the add-compound handler, add:

```r
    # "Set as default marker list for this compound": opens a confirm modal that
    # names the compound + the marker count, then (on confirm) rewrites that
    # compound's preset in the YAML to the table's CURRENT markers (full replace,
    # empty table allowed = clears the preset).
    observeEvent(input$pelsa_set_default_markers_btn, {
      ome <- active_setup_ome(); req(ome)
      compound <- input$pelsa_compound
      if (is.null(compound) || !nzchar(compound)) {
        showNotification("Select a compound first.", type = "warning",
                         duration = 5)
        return()
      }
      n <- nrow(cur_markers(ome))
      showModal(modalDialog(
        title = "Set default marker list",
        sprintf(
          paste0("This will replace the saved preset for '%s' with the %d ",
                 "marker(s) currently in the table. This rewrites ",
                 "compound_markers.yaml. Continue?"),
          compound, n
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("pelsa_confirm_set_default"), "Confirm",
                       class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })
```

- [ ] **Step 2: Add the confirm handler (bare input id)**

Immediately after, add:

```r
    # Confirm: write the current table as the selected compound's preset. The
    # confirm button id is namespaced via session$ns() in the modal markup, but
    # input$ references it BARE (the module ns() rule).
    observeEvent(input$pelsa_confirm_set_default, {
      ome <- active_setup_ome(); req(ome)
      compound <- input$pelsa_compound
      if (is.null(compound) || !nzchar(compound)) {
        removeModal()
        return()
      }
      new_cm <- pelsa_set_compound_markers(compound_markers(), compound,
                                           cur_markers(ome))
      ok <- pelsa_write_compound_markers(pelsa_compound_markers_path(), new_cm)
      removeModal()
      if (!ok) {
        showNotification(pelsa_readonly_save_msg, type = "error", duration = 10)
        return()
      }
      compound_markers_version(compound_markers_version() + 1)
      showNotification(
        sprintf("Saved %d marker(s) as the default for '%s'.",
                nrow(cur_markers(ome)), compound),
        type = "message", duration = 4
      )
    })
```

- [ ] **Step 3: Reload + sanity-check**

Run: `R -q -e 'devtools::load_all("."); cat("loaded OK\n")'`
Expected: prints `loaded OK`.

- [ ] **Step 4: Manual smoke (documented, not automated)**

Launch the app, open PELSA Setup, select `TestCmpd`, paste two accessions → Add markers → "Set as default…" → confirm. The YAML's `TestCmpd:` now lists those two markers. (Revert the YAML afterward.)

- [ ] **Step 5: Commit**

```bash
git add R/tab_pelsa_section1.R
git commit -m "feat(pelsa): set-as-default handler writes compound preset behind a confirm modal"
```

---

### Task 12: Full regression + documentation touch-up

**Files:**
- Modify: `R/tab_pelsa_section1_helpers.R` (docstring header listing of public helpers, lines 9-16, if present)
- Test: full PELSA suite

- [ ] **Step 1: Update the helper-file header docstring**

In `R/tab_pelsa_section1_helpers.R`, add the new helpers to the "Public helpers" list near the top (after `pelsa_empty_marker_rows()`):

```r
#   pelsa_validate_compound_name(name)          structured ok/message validation
#   pelsa_compound_exists(cm, name)             case-insensitive primary-key match
#   pelsa_add_compound(cm, name)                add compound w/ empty markers
#   pelsa_set_compound_markers(cm, name, rows)  replace a compound's preset markers
#   pelsa_write_compound_markers(path, cm)      atomic YAML write (read-only -> FALSE)
```

- [ ] **Step 2: Run the full PELSA test suite**

Run: `R -q -e 'devtools::load_all("."); devtools::test(filter = "pelsa")'`
Expected: PASS, no failures.

- [ ] **Step 3: Confirm no stray non-ASCII bytes were introduced**

Run:

```bash
grep -RnP "[^\x00-\x7F]" R/tab_pelsa_section1.R R/tab_pelsa_section1_helpers.R inst/pelsa/compound_markers.yaml || echo "ASCII OK"
```

Expected: prints `ASCII OK`.

- [ ] **Step 4: Commit**

```bash
git add R/tab_pelsa_section1_helpers.R
git commit -m "docs(pelsa): document new compound-management helpers"
```

---

## Self-Review Notes

- **Spec coverage:** Add-compound (Tasks 2,3,7,10), Set-as-default (Tasks 4,7,11), reselect replace (Task 9), writer + read-only error (Tasks 5,10,11), aliases removed (Tasks 1,6), no box re-render (Task 8), name validation (Task 2), regenerated YAML (Task 6), tests pure-only (Tasks 1-6). All covered.
- **Type consistency:** `pelsa_set_compound_markers(cm, name, marker_rows)` consumes `data.frame(accession, gene)` (same schema as `pelsa_empty_marker_rows()`/`cur_markers()`); `pelsa_write_compound_markers(path, cm)` returns logical, consumed by both server handlers; `pelsa_validate_compound_name` returns `list(ok, name|message)` consumed in Task 10. Confirm button `pelsa_confirm_set_default` namespaced in markup (Task 11 Step 1) and referenced bare (Step 2) — consistent.
- **Echo-guard:** Task 9 keeps `last_autofilled_compound` (declared at the existing line 279, untouched).
