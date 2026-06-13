################################################################################
# PELSA marker matching + parsing (Task 2J) — the pure, testable matching/
# parsing core. The org.db / UniProt accession<->gene resolution UI (canonical
# / reviewed flags) lives in Phase 5; this file only owns the deterministic
# helpers it will call.
#
# Public helpers:
#   pelsa_isoform_base(accession)   strip UniProt isoform suffix "-<n>" (vectorized)
#   pelsa_parse_markers(raw)        marker paste-box string -> token char vector
#   pelsa_match_markers(acc, mk)    per-peptide logical: ANY token == ANY marker
#
# MATCHING RULE (pelsa_match_markers):
#   A peptide is a marker hit if ANY of its ;-delimited accession tokens,
#   normalized to isoform-BASE + lowercased, equals ANY marker's isoform-base +
#   lowercased. The rule is by ACCESSION (not gene), ANY-TOKEN, CASE-INSENSITIVE,
#   and SYMMETRIC in the isoform suffix: marker "P12345" matches a peptide on
#   "P12345-2", and marker "P12345-2" matches a peptide on "P12345" (both
#   normalize to base "p12345").
#
# pelsa_isoform_base consolidates the suffix-strip that tab_pelsa_coverage_helpers.R,
# tab_pelsa_annotation_helpers.R, and tab_pelsa_fasta_helpers.R each inlined
# (sub("-[0-9]+$", "", x)); those files are intentionally left unchanged — this
# is the shared entry point for future use (Phase 5/7).
################################################################################

# Strip the UniProt isoform suffix from accession(s): "P12345-2" -> "P12345".
# Vectorized over a character vector; NA -> NA. Only a TRAILING "-<digits>" is
# removed (so "A0A-B1" is unchanged; "Q9-10" -> "Q9", intended for the UniProt
# isoform-suffix convention).
# @noRd
pelsa_isoform_base <- function(accession) {
  if (is.null(accession) || length(accession) == 0L) {
    return(character(0))
  }
  if (!is.character(accession)) {
    stop("pelsa_isoform_base(): `accession` must be a character vector.",
         call. = FALSE)
  }
  sub("-[0-9]+$", "", accession)
}

# Parse a marker paste-box string into a character vector of tokens. Thin named
# wrapper over parse_protein_search_input (splits on space/comma/semicolon/
# newline, trims, drops empties). Returns character(0) on empty / NULL.
# @noRd
pelsa_parse_markers <- function(raw) {
  parse_protein_search_input(raw)
}

# Normalize a character vector of accessions to the matching key: isoform-base,
# trimmed, lowercased, with empties dropped.
# @noRd
pelsa_marker_key <- function(x) {
  x <- pelsa_isoform_base(trimws(x))
  x <- tolower(x)
  x[!is.na(x) & nzchar(x)]
}

# Per-peptide logical: does ANY of the peptide's accession tokens match ANY
# marker, by isoform-base + case-insensitive accession (see header for the full
# SYMMETRIC rule)?
#
# @param accession_tokens_list  EITHER a character vector of ";"-delimited
#   accession strings (one per peptide, e.g. "P12345;Q99999") OR an already-split
#   list of character vectors (one per peptide). The ";"-string form is primary
#   and is split internally on ";".
# @param marker_accessions      character vector of marker accessions as entered
#   (may themselves be isoform forms like "P12345-2").
# @return logical vector, one per peptide (TRUE = marker hit). Vectorized: no
#   per-peptide R loop.
#
# Edges: empty `marker_accessions` -> all FALSE; a peptide with NA/empty
# accessions -> FALSE; tokens are whitespace-trimmed.
# @noRd
pelsa_match_markers <- function(accession_tokens_list, marker_accessions) {
  # ---- Boundary validation (fail fast) ------------------------------------
  if (!is.character(marker_accessions)) {
    stop("pelsa_match_markers(): `marker_accessions` must be a character vector.",
         call. = FALSE)
  }
  is_list_form <- is.list(accession_tokens_list)
  if (!is_list_form && !is.character(accession_tokens_list)) {
    stop("pelsa_match_markers(): `accession_tokens_list` must be a character ",
         "vector of ;-delimited accessions or a list of character vectors.",
         call. = FALSE)
  }

  n <- length(accession_tokens_list)

  # ---- Normalize marker set once ------------------------------------------
  marker_keys <- unique(pelsa_marker_key(marker_accessions))
  if (n == 0L || length(marker_keys) == 0L) {
    return(rep(FALSE, n))
  }

  # ---- Build a (row_id, token) grid, then vectorize membership ------------
  if (is_list_form) {
    tokens_by_row <- accession_tokens_list
  } else {
    tokens_by_row <- strsplit(accession_tokens_list, ";", fixed = TRUE)
    # strsplit() on NA yields NA_character_; keep length 1 so row_id aligns.
  }

  lengths_per_row <- lengths(tokens_by_row)
  # Rows with zero tokens (e.g. "" -> character(0)) contribute nothing but must
  # still occupy a slot in the result; tapply handles absent row_ids via levels.
  row_id <- rep.int(seq_len(n), lengths_per_row)
  flat_tokens <- unlist(tokens_by_row, use.names = FALSE)

  if (length(flat_tokens) == 0L) {
    return(rep(FALSE, n))
  }

  flat_keys <- pelsa_isoform_base(trimws(flat_tokens))
  flat_keys <- tolower(flat_keys)
  is_hit <- !is.na(flat_keys) & nzchar(flat_keys) & flat_keys %in% marker_keys

  # any() per row across all its tokens, restoring all n rows in order.
  agg <- tapply(is_hit, factor(row_id, levels = seq_len(n)), FUN = any)
  out <- as.logical(agg)
  out[is.na(out)] <- FALSE
  out
}
