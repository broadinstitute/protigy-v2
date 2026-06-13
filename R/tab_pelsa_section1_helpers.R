################################################################################
# PELSA Setup section (Section 1) — pure, testable helpers (Task 5A).
#
# These back the Setup tab's SHARED/app-wide controls + the reactive marker
# table. They are deliberately free of Shiny reactivity so they unit-test in
# isolation; the module server (tab_pelsa_section1.R) stays thin and calls into
# them.
#
# Public helpers (all @noRd):
#   pelsa_list_species(database_dir)            species subfolders under inst/database/
#   pelsa_read_compound_markers(path)           parse inst/pelsa/compound_markers.yaml
#   pelsa_compound_marker_rows(cm, compound)    marker rows (accession, gene) for a compound
#   pelsa_marker_rows_from_input(tokens, ...)   parsed paste-box tokens -> marker rows
#   pelsa_merge_marker_rows(existing, new)      de-duplicated union by accession
#   pelsa_empty_marker_rows()                   the canonical empty 2-col marker frame
#
# DEFERRED SEAMS (documented; NOT implemented in 5A):
#   - accession<->gene resolution (org.Hs.eg.db / org.Mm.eg.db; canonical /
#     reviewed flags; gene -> accession-choice prompt) is HEAVY and deferred to
#     5D. pelsa_marker_rows_from_input() takes a `resolver` arg as the seam:
#     when NULL (the 5A default) gene is left NA, to be filled by 5D's resolver.
#   - per-DATASET (per-ome) condition/replicate config + "apply to all" -> 5B.
#   - shinyjqui orderInput condition/replicate ordering widgets -> 5B.
#   - species UniProt-refresh button + progress -> 5C.
#   - driving the container's pelsa_analyzed_datasets from the datasets control
#     + the Start-Analysis compute pipeline -> 5D.
################################################################################

# The canonical empty marker-table frame: two character columns. Used as the
# starting state of the reactive marker table and the zero-row return of the
# row-building helpers, so the table schema is defined in exactly one place.
# @noRd
pelsa_empty_marker_rows <- function() {
  data.frame(
    accession = character(0),
    gene      = character(0),
    stringsAsFactors = FALSE
  )
}

# List the species subfolders under a PELSA database directory.
#
# Read LIVE (no caching) so a newly added species folder appears without an app
# restart. The directory is passed in as a PARAM (not resolved here) so the
# helper is testable and works for both the installed-package path
# (system.file("database", package = "Protigy")) and the dev/load_all inst path.
#
# @param database_dir character scalar path to the database directory.
# @return sorted character vector of subfolder (species) names; character(0)
#   when the directory is missing/empty or `database_dir` is "".
# @noRd
pelsa_list_species <- function(database_dir) {
  if (!is.character(database_dir) || length(database_dir) != 1L) {
    stop("pelsa_list_species(): `database_dir` must be a single string.",
         call. = FALSE)
  }
  if (is.na(database_dir) || !nzchar(database_dir) || !dir.exists(database_dir)) {
    return(character(0))
  }
  entries <- list.dirs(database_dir, full.names = FALSE, recursive = FALSE)
  entries <- entries[nzchar(entries)]
  sort(entries)
}

# Read + validate the PELSA compound-marker preset file (compound_markers.yaml).
#
# Structure contract (documented in the yaml header):
#   compounds:
#     <Compound Name>:
#       aliases:  [optional list of alternative names]
#       markers:  [ {accession: <chr REQUIRED>, gene: <chr optional>}, ... ]
#
# A MISSING file returns an empty result (list(compounds = list())) rather than
# erroring — the Setup dropdown then simply shows no presets. A MALFORMED file
# (unparseable YAML, or `compounds` not a named list) fails fast with a clear
# error, because that is a developer/config mistake worth surfacing loudly.
#
# @param path character scalar path to the yaml file.
# @return list(compounds = <named list of compound entries>). Each entry keeps
#   its `markers` (and `aliases` if present).
# @noRd
pelsa_read_compound_markers <- function(path) {
  if (!is.character(path) || length(path) != 1L) {
    stop("pelsa_read_compound_markers(): `path` must be a single string.",
         call. = FALSE)
  }
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(list(compounds = list()))
  }

  parsed <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      stop(sprintf(
        "pelsa_read_compound_markers(): failed to parse YAML at '%s': %s",
        path, conditionMessage(e)
      ), call. = FALSE)
    }
  )

  if (is.null(parsed) || is.null(parsed$compounds)) {
    return(list(compounds = list()))
  }
  compounds <- parsed$compounds
  if (!is.list(compounds) || is.null(names(compounds)) ||
      any(!nzchar(names(compounds)))) {
    stop("pelsa_read_compound_markers(): `compounds` must be a named list ",
         "keyed by compound name.", call. = FALSE)
  }

  # Validate each compound's markers carry an accession (gene is optional).
  for (cname in names(compounds)) {
    markers <- compounds[[cname]]$markers
    if (is.null(markers)) next  # a compound may legitimately have no presets
    if (!is.list(markers)) {
      stop(sprintf(
        "pelsa_read_compound_markers(): compound '%s' has a non-list `markers`.",
        cname
      ), call. = FALSE)
    }
    for (mk in markers) {
      if (!is.list(mk)) {
        stop(sprintf(
          "pelsa_read_compound_markers(): compound '%s' has a non-list marker entry (each marker must be a mapping with an `accession`).",
          cname
        ), call. = FALSE)
      }
      acc <- mk$accession
      if (is.null(acc) || !nzchar(as.character(acc)[[1]])) {
        stop(sprintf(
          "pelsa_read_compound_markers(): compound '%s' has a marker missing an `accession`.",
          cname
        ), call. = FALSE)
      }
    }
  }

  list(compounds = compounds)
}

# Resolve a (possibly aliased) compound name to its primary key in the parsed
# compound-marker list. Exact name match wins; otherwise an alias match (any
# compound whose `aliases` contains the name) is honored. Returns NA_character_
# when nothing matches.
# @noRd
.pelsa_resolve_compound_name <- function(compound_markers, compound_name) {
  compounds <- compound_markers$compounds
  if (length(compounds) == 0L) return(NA_character_)
  if (compound_name %in% names(compounds)) return(compound_name)
  for (cname in names(compounds)) {
    aliases <- compounds[[cname]]$aliases
    if (!is.null(aliases) && compound_name %in% as.character(aliases)) {
      return(cname)
    }
  }
  NA_character_
}

# Build the marker rows (accession, gene) for one compound's presets.
#
# Aliases are honored: `compound_name` may be the primary name OR any alias.
# When the compound is unknown or has no markers, returns the empty 2-col frame.
# A marker without a `gene` gets NA in the gene column.
#
# @param compound_markers parsed list from pelsa_read_compound_markers().
# @param compound_name    character scalar compound name (or alias).
# @return data.frame(accession, gene) — one row per preset marker.
# @noRd
pelsa_compound_marker_rows <- function(compound_markers, compound_name) {
  if (!is.list(compound_markers)) {
    stop("pelsa_compound_marker_rows(): `compound_markers` must be a list ",
         "(from pelsa_read_compound_markers()).", call. = FALSE)
  }
  if (!is.character(compound_name) || length(compound_name) != 1L) {
    stop("pelsa_compound_marker_rows(): `compound_name` must be a single string.",
         call. = FALSE)
  }

  key <- .pelsa_resolve_compound_name(compound_markers, compound_name)
  if (is.na(key)) return(pelsa_empty_marker_rows())

  markers <- compound_markers$compounds[[key]]$markers
  if (is.null(markers) || length(markers) == 0L) {
    return(pelsa_empty_marker_rows())
  }

  accession <- vapply(markers, function(mk) as.character(mk$accession)[[1]],
                      character(1))
  gene <- vapply(markers, function(mk) {
    g <- mk$gene
    if (is.null(g) || !nzchar(as.character(g)[[1]])) NA_character_
    else as.character(g)[[1]]
  }, character(1))

  data.frame(accession = accession, gene = gene, stringsAsFactors = FALSE)
}

# Turn parsed paste-box tokens into marker rows (accession, gene).
#
# 5A SEAM: accession -> gene resolution is deferred to 5D. When `resolver` is
# NULL (the default), every gene is NA. A non-NULL `resolver` must be a function
# taking the de-duplicated accession vector and returning a character vector of
# genes the SAME length (NA where unknown); this lets 5D plug in
# org.Hs.eg.db/org.Mm.eg.db (and the canonical/reviewed handling) without
# touching this signature.
#
# Tokens are de-duplicated preserving first-seen order; empty/NA tokens dropped.
# De-dup is accession-EXACT here (the table key) — isoform-base awareness is
# reserved for the volcano MATCHING rule (pelsa_match_markers), not the table.
#
# @param tokens   character vector of accession tokens (e.g. from pelsa_parse_markers()).
# @param resolver NULL (5A) or function(accessions) -> gene character vector.
# @return data.frame(accession, gene).
# @noRd
pelsa_marker_rows_from_input <- function(tokens, resolver = NULL) {
  if (is.null(tokens) || length(tokens) == 0L) {
    return(pelsa_empty_marker_rows())
  }
  if (!is.character(tokens)) {
    stop("pelsa_marker_rows_from_input(): `tokens` must be a character vector.",
         call. = FALSE)
  }
  if (!is.null(resolver) && !is.function(resolver)) {
    stop("pelsa_marker_rows_from_input(): `resolver` must be NULL or a function.",
         call. = FALSE)
  }

  tokens <- trimws(tokens)
  tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
  tokens <- unique(tokens)
  if (length(tokens) == 0L) {
    return(pelsa_empty_marker_rows())
  }

  if (is.null(resolver)) {
    gene <- rep(NA_character_, length(tokens))
  } else {
    gene <- as.character(resolver(tokens))
    if (length(gene) != length(tokens)) {
      stop("pelsa_marker_rows_from_input(): `resolver` must return one gene ",
           "per accession.", call. = FALSE)
    }
  }

  data.frame(accession = tokens, gene = gene, stringsAsFactors = FALSE)
}

# De-duplicated union of two marker-row frames, keyed by EXACT accession.
#
# Existing rows win on conflict (so a compound-autofill gene already in the
# table is not clobbered by a later paste that has no gene). New accessions are
# appended in their incoming order. Accession matching is exact (case- and
# isoform-sensitive) — this is the TABLE identity; the looser isoform-base,
# case-insensitive rule belongs to peptide MATCHING (pelsa_match_markers), and
# is intentionally NOT applied here.
#
# @param existing data.frame(accession, gene) — current table.
# @param new      data.frame(accession, gene) — rows to merge in.
# @return data.frame(accession, gene) — the de-duplicated union.
# @noRd
pelsa_merge_marker_rows <- function(existing, new) {
  .check <- function(x, nm) {
    if (!is.data.frame(x) ||
        !all(c("accession", "gene") %in% names(x))) {
      stop(sprintf(
        "pelsa_merge_marker_rows(): `%s` must be a data.frame with columns accession and gene.",
        nm
      ), call. = FALSE)
    }
  }
  .check(existing, "existing")
  .check(new, "new")

  if (nrow(new) == 0L) {
    return(existing[, c("accession", "gene"), drop = FALSE])
  }
  keep <- !(new$accession %in% existing$accession)
  combined <- rbind(
    existing[, c("accession", "gene"), drop = FALSE],
    new[keep, c("accession", "gene"), drop = FALSE]
  )
  rownames(combined) <- NULL
  combined
}
