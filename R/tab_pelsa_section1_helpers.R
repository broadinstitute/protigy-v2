################################################################################
# PELSA Setup section (Section 1) - pure, testable helpers (Task 5A).
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
# Section-1 ORDERING helpers (Task 5B) - also pure/testable:
#   pelsa_distinct_conditions(cdesc, col)          distinct condition values (occurrence order)
#   pelsa_samples_for_condition(cdesc, ...)         sample names of one condition, replicate-sorted
#   pelsa_default_replicate_order(cdesc, ...)        per-condition default replicate (sample) order
#   pelsa_merge_ordering(saved, available)          keep saved order, append new, drop removed
#   pelsa_build_sample_order(...)                    canonical ordered sample-name vector
#
# DEFERRED SEAMS (documented; NOT implemented in 5A):
#   - accession<->gene resolution (org.Hs.eg.db / org.Mm.eg.db; canonical /
#     reviewed flags; gene -> accession-choice prompt) is HEAVY and deferred to
#     5D. pelsa_marker_rows_from_input() takes a `resolver` arg as the seam:
#     when NULL (the 5A default) gene is left NA, to be filled by 5D's resolver.
#   - species UniProt-refresh button + progress -> 5C.
#   - driving the container's pelsa_analyzed_datasets from the datasets control
#     + the Start-Analysis compute pipeline -> 5D.
#
# IMPLEMENTED IN 5B (was a deferred 5A seam):
#   - per-DATASET (per-ome) condition/replicate config + "apply to all" checkbox.
#   - shinyjqui orderInput condition/replicate ordering widgets, backed by the
#     ordering helpers below (the widgets live in the module server; the merge /
#     sample-order LOGIC is here, pure and tested).
################################################################################

# ---- live path resolvers (shared with Section 3) -----------------------------

# Resolve the PELSA database directory live.
#   - installed package: system.file("database", package = "Protigy")
#   - dev/load_all:      the same call resolves to inst/database
# Returns "" when unavailable (pelsa_list_species() then yields character(0)).
# @noRd
pelsa_database_dir <- function() {
  system.file("database", package = "Protigy")
}

# Resolve the compound-marker preset yaml path live (same install/dev rule).
# @noRd
pelsa_compound_markers_path <- function() {
  system.file("pelsa", "compound_markers.yaml", package = "Protigy")
}

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
# erroring - the Setup dropdown then simply shows no presets. A MALFORMED file
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
# @return data.frame(accession, gene) - one row per preset marker.
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
# De-dup is accession-EXACT here (the table key) - isoform-base awareness is
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
# isoform-sensitive) - this is the TABLE identity; the looser isoform-base,
# case-insensitive rule belongs to peptide MATCHING (pelsa_match_markers), and
# is intentionally NOT applied here.
#
# @param existing data.frame(accession, gene) - current table.
# @param new      data.frame(accession, gene) - rows to merge in.
# @return data.frame(accession, gene) - the de-duplicated union.
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

################################################################################
# Section-1 ORDERING helpers (Task 5B)
#
# The Setup tab lets the user ORDER conditions and, within each condition, the
# replicate samples. The confirmed order becomes the canonical column order
# (sample_order) every downstream PELSA plot respects.
#
# All cdesc inputs below are a data.frame whose ROW NAMES are the sample names
# (this matches cmapR GCTs, where rownames(gct@cdesc) == gct@cid). The condition
# and replicate columns are columns of that data.frame.
################################################################################

# Coerce a cdesc column to a plain character vector (factors -> labels), so
# ordering compares on the displayed values, not factor codes.
# @noRd
.pelsa_col_chr <- function(cdesc, col) {
  if (!is.data.frame(cdesc)) {
    stop(".pelsa_col_chr(): `cdesc` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop(".pelsa_col_chr(): `col` must be a single non-empty string.",
         call. = FALSE)
  }
  if (!col %in% names(cdesc)) {
    stop(sprintf(".pelsa_col_chr(): column '%s' is not in cdesc.", col),
         call. = FALSE)
  }
  as.character(cdesc[[col]])
}

# Distinct condition values, in FIRST-SEEN (occurrence) order.
#
# This is the natural/default condition order: the order conditions first appear
# down the cdesc rows. NA values are dropped.
#
# @param cdesc data.frame (rownames = sample names).
# @param condition_col single column name.
# @return character vector of distinct conditions (occurrence order).
# @noRd
pelsa_distinct_conditions <- function(cdesc, condition_col) {
  vals <- .pelsa_col_chr(cdesc, condition_col)
  vals <- vals[!is.na(vals)]
  unique(vals)
}

# Sample names belonging to ONE condition, sorted by the replicate-identifier
# column (the default replicate order). Sample names are the cdesc rownames.
#
# Ties on the replicate column are broken by sample name for determinism.
#
# @param cdesc         data.frame (rownames = sample names).
# @param condition_col condition grouping column name.
# @param replicate_col replicate identifier column name.
# @param condition     the condition value to select.
# @return character vector of sample names for that condition, replicate-sorted.
# @noRd
pelsa_samples_for_condition <- function(cdesc, condition_col, replicate_col,
                                        condition) {
  cond_vals <- .pelsa_col_chr(cdesc, condition_col)
  rep_vals  <- .pelsa_col_chr(cdesc, replicate_col)
  samples   <- rownames(cdesc)
  if (is.null(samples)) samples <- as.character(seq_len(nrow(cdesc)))

  in_cond <- !is.na(cond_vals) & cond_vals == condition
  if (!any(in_cond)) return(character(0))

  ord <- order(rep_vals[in_cond], samples[in_cond], method = "radix")
  samples[in_cond][ord]
}

# Default per-condition replicate (sample) ordering: a NAMED LIST keyed by
# condition, each element the condition's samples sorted by replicate_col.
#
# Conditions are taken in their distinct/occurrence order.
#
# @return named list condition -> character vector of sample names.
# @noRd
pelsa_default_replicate_order <- function(cdesc, condition_col, replicate_col) {
  conds <- pelsa_distinct_conditions(cdesc, condition_col)
  out <- lapply(conds, function(cond) {
    pelsa_samples_for_condition(cdesc, condition_col, replicate_col, cond)
  })
  names(out) <- conds
  out
}

# Reconcile a SAVED order against the currently AVAILABLE items: keep saved
# items that are still available (in their saved order), then append any newly
# available items (in their available order). Mirrors updateDatasetOrdering()
# from the multi-ome heatmap (keep-saved / drop-removed / append-new).
#
# @param saved     character vector - a previously chosen order (may be NULL).
# @param available character vector - items that currently exist.
# @return character vector - the reconciled order (subset+superset of available).
# @noRd
pelsa_merge_ordering <- function(saved, available) {
  available <- as.character(available)
  available <- available[!is.na(available)]
  if (is.null(saved)) saved <- character(0)
  saved <- as.character(saved)

  kept <- saved[saved %in% available]
  kept <- unique(kept)
  appended <- setdiff(available, kept)
  c(kept, appended)
}

# Build the canonical SAMPLE order from a condition order + per-condition
# replicate order. This is the column order every downstream PELSA plot
# (Summary, Volcano) respects.
#
# Algorithm:
#   - Reconcile condition_order against the cdesc's distinct conditions
#     (drop conditions no longer present, append any missing in natural order).
#   - For each condition in that reconciled order, take its replicate order
#     (reconciled against the condition's actual samples, so removed samples are
#     dropped and any not-yet-ordered samples are appended in default order).
#   - Concatenate.
#
# Pure and deterministic: identical inputs always yield the identical vector.
#
# @param condition_order              character vector - chosen condition order.
# @param replicate_order_by_condition named list condition -> sample-name order.
# @param cdesc                        data.frame (rownames = sample names).
# @param condition_col                condition grouping column name.
# @param replicate_col                replicate identifier column name.
# @return character vector of sample names - the canonical column order.
# @noRd
pelsa_build_sample_order <- function(condition_order,
                                     replicate_order_by_condition,
                                     cdesc,
                                     condition_col,
                                     replicate_col) {
  if (!is.data.frame(cdesc)) {
    stop("pelsa_build_sample_order(): `cdesc` must be a data.frame.",
         call. = FALSE)
  }
  if (is.null(replicate_order_by_condition)) {
    replicate_order_by_condition <- list()
  }
  if (!is.list(replicate_order_by_condition)) {
    stop("pelsa_build_sample_order(): `replicate_order_by_condition` must be ",
         "a (named) list.", call. = FALSE)
  }

  available_conds <- pelsa_distinct_conditions(cdesc, condition_col)
  conds <- pelsa_merge_ordering(condition_order, available_conds)

  ordered <- lapply(conds, function(cond) {
    default_samples <- pelsa_samples_for_condition(
      cdesc, condition_col, replicate_col, cond
    )
    saved_rep <- replicate_order_by_condition[[cond]]
    pelsa_merge_ordering(saved_rep, default_samples)
  })

  out <- unlist(ordered, use.names = FALSE)
  if (is.null(out)) out <- character(0)
  out
}

################################################################################
# Section-1 per-dataset config UI builders (Task 5B) - pure tag constructors.
#
# These build the markup for the per-dataset config panel and the per-condition
# replicate card. They are pure functions of their args (an `ns` namespacer,
# pre-computed ids, and values), so the module server stays thin and the markup
# is testable without a running session. All inputIds are passed in ALREADY
# namespaced via ns() by the caller.
################################################################################

# One bordered, scroll-contained card for a single condition's replicate order.
#
# Single-replicate (<= 1 sample) conditions COLLAPSE to a static label (no drag
# widget). Multi-replicate conditions get a shinyjqui orderInput inside a
# scroll-capped card plus a Reset button.
#
# @param cond        condition name (card header).
# @param samples     character vector of this condition's sample names (default
#                    order); length<=1 collapses to a static label.
# @param order_id    ns()-namespaced inputId for the orderInput.
# @param reset_id    ns()-namespaced inputId for the Reset actionButton.
# @return a shiny tag (the card).
# @noRd
pelsa_replicate_card <- function(cond, samples, order_id, reset_id) {
  header <- shiny::tags$div(class = "pelsa-rep-card__head", cond)

  if (length(samples) <= 1L) {
    body <- shiny::tags$div(
      class = "pelsa-rep-card__single",
      if (length(samples) == 0L) "(no samples)" else samples[[1]]
    )
    return(shiny::tags$div(
      class = "pelsa-rep-card",
      header, body
    ))
  }

  shiny::tags$div(
    class = "pelsa-rep-card pelsa-rep-card--multi",
    header,
    orderInput(inputId = order_id, label = NULL, items = samples, width = "100%"),
    shiny::div(class = "pelsa-rep-card__reset",
               shiny::actionButton(reset_id, "Reset", class = "btn-xs"))
  )
}

# The per-dataset configuration panel: condition + replicate column selectors,
# a condition orderInput (+reset+keyboard-rank), and a placeholder uiOutput for
# the per-condition replicate cards.
#
# @param ome             dataset name (panel header).
# @param cols            character vector of cdesc column names (selector choices).
# @param sel_cond        currently-selected condition column.
# @param sel_rep         currently-selected replicate column.
# @param ids             named list of ns()-namespaced inputIds:
#                        condition_col, replicate_col, condition_order,
#                        condition_reset, replicate_cards.
# @param cond_order      character vector of conditions in their initial order;
#                        the condition orderInput is BORN with these items so the
#                        drag blocks render immediately (no post-render
#                        updateOrderInput message that can race the renderUI).
# @return a shiny tag (the panel).
# @noRd
pelsa_dataset_config_panel <- function(ome, cols, sel_cond, sel_rep, ids,
                                       cond_order = character(0)) {
  if (length(cols) == 0L) {
    return(shiny::div(
      class = "pelsa-ds-config",
      shiny::tags$strong(ome),
      shiny::helpText("This dataset has no sample-annotation columns to group by.")
    ))
  }

  shiny::div(
    class = "pelsa-ds-config",
    shiny::tags$strong(ome),
    shiny::selectInput(ids$condition_col, label = "Condition grouping column",
                       choices = cols, selected = sel_cond),
    shiny::selectInput(ids$replicate_col, label = "Replicate identifier column",
                       choices = cols, selected = sel_rep),

    shiny::tags$label("Condition order (drag to reorder)"),
    orderInput(inputId = ids$condition_order, label = NULL,
               items = as.list(cond_order), width = "100%"),
    shiny::div(
      style = "margin-top:6px;",
      shiny::actionButton(ids$condition_reset, "Reset to default order",
                          class = "btn-xs")
    ),

    shiny::tags$hr(),
    shiny::tags$label("Replicate order within each condition"),
    shiny::uiOutput(ids$replicate_cards)
  )
}

# Prune per-dataset setup_state lists down to the currently-checked datasets.
#
# Each field in `state_lists` is a named list keyed by dataset name; entries for
# datasets not in `checked` are dropped (immutable - a NEW list per field is
# returned, the input is never mutated). Fields absent / NULL are returned as an
# empty list so callers always get the full set of keys back.
#
# @param state_lists named list field -> (named list keyed by dataset).
# @param checked     character vector of datasets to KEEP.
# @return named list with the same field names, each pruned to `checked`.
# @noRd
pelsa_prune_perdataset_state <- function(state_lists, checked) {
  if (!is.list(state_lists)) {
    stop("pelsa_prune_perdataset_state(): `state_lists` must be a named list.",
         call. = FALSE)
  }
  checked <- as.character(checked)
  lapply(state_lists, function(field) {
    if (is.null(field) || length(field) == 0L) return(list())
    keep <- intersect(names(field), checked)
    if (length(keep) == 0L) return(list())
    field[keep]
  })
}

# A small uppercase eyebrow header with a leading icon for a Setup section.
# Color is supplied by the parent .pelsa-layer-* class (CSS var); pairing the
# icon + text label here means the layer is NEVER signalled by color alone.
#
# @param icon_name a Font Awesome icon name (shiny::icon).
# @param label     the section header text.
# @return a shiny tag (the section header).
# @noRd
pelsa_section_head <- function(icon_name, label) {
  shiny::tags$div(
    class = "pelsa-section-head",
    shiny::icon(icon_name), shiny::tags$span(label)
  )
}

# The PELSA Setup box markup (pure tag constructor).
#
# Builds the entire add_css_attributes(box(...)) for the Setup tab: datasets
# checklist, species + compound selectors, marker paste box + table placeholder,
# the per-dataset config placeholder (5B, rendered server-side), and the 5C
# maintenance refresh sub-section (species checklist + button). Kept pure (a
# function of its choice vectors + `ns`) so the module renderUI stays thin and
# the markup is testable without a running session. All inputIds are namespaced
# via the passed `ns`.
#
# @param datasets  character vector of dataset (ome) names (checkbox choices).
# @param species   character vector of species (live inst/database/ subfolders).
# @param compounds character vector of compound preset names.
# @param ns        the module namespacer (session$ns / NS(id)).
# @return a shiny tag (the Setup box).
# @noRd
pelsa_setup_box_ui <- function(datasets, species, compounds, ns) {
  # The Setup box is split into two equal columns. Each logical group is wrapped
  # in a .pelsa-section card whose LAYER class color-codes it so the user can
  # parse the form at a glance (see inst/custom.css "PELSA Setup"):
  #   LEFT  - the run configuration the user fills in top-to-bottom:
  #           data-input layer (datasets, species, compound, markers) then the
  #           ordering/config layer (condition / replicate config + reorder).
  #   RIGHT - the action layer (Start Analysis, made dominant) on top, then the
  #           clearly-secondary maintenance layer (UniProt refresh) below it.

  # 1 + 2 + 3 + 4 + 5. DATA-INPUT LAYER (blue): what to analyze + markers.
  data_section <- shiny::tags$div(
    class = "pelsa-section pelsa-layer-data",
    pelsa_section_head("table-list", "Data inputs"),

    # 1. Datasets to analyze (FIRST control).
    shiny::checkboxGroupInput(
      ns("pelsa_datasets"),
      label    = "Datasets to analyze",
      choices  = datasets,
      selected = datasets
    ),

    # 2. Species (live list of inst/database/ subfolders).
    shiny::selectInput(
      ns("pelsa_species"),
      label   = "Species",
      choices = species,
      selected = if (length(species)) species[[1]] else NULL
    ),

    # 3. Treatment compound (presets from compound_markers.yaml).
    #    Selecting a compound autofills the marker table (server observer).
    shiny::selectInput(
      ns("pelsa_compound"),
      label   = "Treatment compound",
      choices = c("(none)" = "", compounds)
    ),

    shiny::tags$hr(),

    # 4. Marker paste box + add button.
    shiny::tags$div(class = "pelsa-section-subhead", "Marker proteins"),
    shiny::textAreaInput(
      ns("pelsa_marker_input"),
      label       = "Add marker proteins (accessions)",
      placeholder = "P12345 Q99999 ... (space/comma/semicolon/newline)",
      rows        = 3
    ),
    shiny::actionButton(ns("pelsa_add_markers"), "Add markers"),

    # 5. Marker reactive table + remove/clear.
    shiny::tags$div(
      style = "margin-top: 10px;",
      DT::dataTableOutput(ns("pelsa_marker_table"))
    ),
    shiny::div(
      style = "margin-top: 8px;",
      shiny::actionButton(ns("pelsa_remove_markers"), "Remove selected"),
      shiny::actionButton(ns("pelsa_clear_markers"), "Clear all")
    )
  )

  # 6. ORDERING / CONFIG LAYER (purple): per-dataset condition/replicate (5B).
  #    "Apply to all" copies one dataset's column+order config to every checked
  #    dataset. The per-dataset panels are rendered server-side (they depend on
  #    the checked-dataset set and each dataset's cdesc).
  config_section <- shiny::tags$div(
    class = "pelsa-section pelsa-layer-config",
    pelsa_section_head("sliders", "Condition / replicate configuration"),
    shiny::checkboxInput(
      ns("pelsa_apply_all"),
      label = "Apply the same setup to all datasets",
      value = FALSE
    ),
    shiny::uiOutput(ns("pelsa_perdataset_config"))
  )

  left_col <- shiny::tagList(data_section, config_section)

  # 7. ACTION LAYER (green): START ANALYSIS (5D). The PRIMARY action - placed
  #    first/top of the right column and visually dominant. Gated by a pre-flight
  #    validation checklist; on success it runs the compute pipeline (staged
  #    withProgress), drives the container's analyzed-datasets seam, and redirects
  #    to the Summary tab. Validation errors render inline below the button.
  action_section <- shiny::tags$div(
    class = "pelsa-section pelsa-layer-action",
    pelsa_section_head("play", "Run analysis"),
    shiny::helpText(
      "Validate the setup above and compute every checked dataset, then jump ",
      "to the Summary tab."
    ),
    shiny::actionButton(
      ns("pelsa_start"), "Start Analysis",
      icon  = shiny::icon("play"),
      class = "btn-primary pelsa-start-btn"
    ),
    shiny::uiOutput(ns("pelsa_validation_msgs"))
  )

  # 7b. MAINTENANCE LAYER (slate, dashed = secondary): per-species UniProt
  #     refresh (5C). Below Start-Analysis and visually quieter - a
  #     setup-independent maintenance action. The species checklist is re-read
  #     LIVE each render (caller passes a fresh pelsa_list_species()). Clicking
  #     rebuilds the checked species' uniprot_features cache off the reactive
  #     path, with a progress bar (fetches take minutes) and a MERGE-over-cache +
  #     atomic write so a partial/flaky refresh never loses prior coverage.
  maint_section <- shiny::tags$div(
    class = "pelsa-section pelsa-layer-maint pelsa-refresh-section",
    pelsa_section_head("screwdriver-wrench", "Maintenance: UniProt library"),
    shiny::helpText(
      "Rebuild the per-species feature cache used for volcano feature ",
      "annotation. This fetches from UniProt and can take several ",
      "minutes per species. It is independent of Start Analysis."
    ),
    shiny::checkboxGroupInput(
      ns("pelsa_refresh_species"),
      label   = "Species to refresh",
      choices = species
    ),
    shiny::actionButton(
      ns("pelsa_refresh_btn"),
      "Refresh per-species UniProt annotation library",
      icon  = shiny::icon("sync"),
      class = "pelsa-refresh-btn"
    ),
    # Inline progress + result, rendered DIRECTLY under the button. Unlike a
    # showNotification() toast (which the user can dismiss / which auto-clears),
    # this status persists for the life of the fetch and stays put afterward, so
    # the live progress bar + the final summary can never be cleared off-screen.
    shiny::uiOutput(ns("pelsa_refresh_status"))
  )

  right_col <- shiny::tagList(action_section, maint_section)

  add_css_attributes(
    shinydashboardPlus::box(
      width = 12,
      title = "PELSA Setup",
      solidHeader = TRUE,
      status      = "primary",

      shiny::fluidRow(
        shiny::column(6, left_col),
        shiny::column(6, right_col)
      )
    ),
    classes = c("box-no-header", "box-with-tabs")
  )
}

# Positional input-id encoders for the per-dataset config controls. IDs are
# keyed by dataset index i (position in all_omes()) and condition index j, so
# arbitrary dataset/condition strings can never collide or produce illegal ids.
# These are the single source of truth for the bare (un-namespaced) ids; the
# module server ns()-wraps them for UI and uses them bare for update*Input().
# @noRd
pelsa_setup_ids <- function() {
  list(
    condition_col   = function(i)    sprintf("pelsa_condition_col_d%d", i),
    replicate_col   = function(i)    sprintf("pelsa_replicate_col_d%d", i),
    condition_order = function(i)    sprintf("pelsa_condition_order_d%d", i),
    condition_reset = function(i)    sprintf("pelsa_condition_reset_d%d", i),
    replicate_cards = function(i)    sprintf("pelsa_replicate_cards_d%d", i),
    replicate_order = function(i, j) sprintf("pelsa_replicate_order_d%d_c%d", i, j),
    replicate_reset = function(i, j) sprintf("pelsa_replicate_reset_d%d_c%d", i, j)
  )
}
