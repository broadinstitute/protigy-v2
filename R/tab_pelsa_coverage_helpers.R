################################################################################
# Module: PELSA per-protein sequence coverage (interval union).
#
# One pure (non-reactive) helper feeding the Summary "per-protein sequence
# coverage" metric:
#
#   pelsa_sequence_coverage(matched_cache, fasta_map, ...) -> data.frame
#
# For each protein accession it computes the fraction of the protein's FASTA
# sequence covered by its mapped peptides' residue spans. Overlapping spans are
# counted ONCE (UNION, not sum): spans [1,10] and [5,15] cover 15 residues, not
# 25. The input is the $matched cache from pelsa_map_peptide_positions() (2B) --
# FASTA-mapped peptides ONLY (no Spectronaut fallback): unmapped peptides are
# simply not present in the matched cache. A shared peptide that maps to A;B;C
# already exists as one row per (peptide, accession) post explode+map, so it
# automatically contributes its span to EVERY one of A/B/C.
#
# FASTA-length resolution uses the SAME isoform-base fallback as 2B: try the
# exact accession key, then strip a UniProt isoform suffix ("-<digits>") and try
# the base key. If neither key exists the protein length is unknown -> coverage
# NA (recorded, never crashes). (A shared pelsa_isoform_base() is formally
# created in Task 2J; the "-[0-9]+$" strip is inlined here and 2J consolidates.)
#
# VECTORIZED interval union across ALL accessions in ONE grouped data.table
# pass -- NO per-accession R loop and NO per-peptide loop. Classic sweep-line:
# sort by (accession, start, end); within each accession a span opens a NEW
# merged block when its start exceeds the running max-end of the spans seen so
# far (start > cummax(prev end)); blocks are id'd by cumsum() of that flag;
# covered = sum over merged blocks of (block_end - block_start + 1). INCLUSIVE
# 1-based spans: a single [s,e] covers e-s+1 residues; merely-adjacent spans
# ([1,5],[6,10]) do NOT overlap (6 > 5) so they stay disjoint, while touching
# spans ([1,5],[5,10]) overlap at residue 5 and merge. Keep free of Shiny
# reactivity (unit-testable).
################################################################################

# Per-protein sequence coverage from a FASTA-mapped matched cache.
#
# @param matched_cache  the $matched data.frame from
#                        pelsa_map_peptide_positions(): one row per
#                        (peptide, accession, occurrence) with integer
#                        pep_start / pep_end (1-based, inclusive).
# @param fasta_map       named list / character vector accession -> AA string
#                        (the SAME map 2B used).
# @param acc_col         accession column name (default "accession")
# @param start_col       span-start column name (default "pep_start")
# @param end_col         span-end column name (default "pep_end")
# @return data.frame, one row per DISTINCT accession in matched_cache, columns:
#   accession        character
#   covered_residues integer  (union length of all spans for the accession,
#                              CLAMPED to protein_length if it would exceed it)
#   protein_length   integer  (FASTA length; NA if the key is unresolved)
#   coverage         numeric  (covered_residues / protein_length; NA if the
#                              length is unknown or 0)
#   over_length_flag logical  (TRUE iff the raw union exceeded protein_length and
#                              was clamped; FALSE otherwise -- incl. unresolved /
#                              zero-length. Consumed by the Summary tab to badge
#                              anomalous accessions.)
# @noRd
pelsa_sequence_coverage <- function(matched_cache,
                                    fasta_map,
                                    acc_col = "accession",
                                    start_col = "pep_start",
                                    end_col = "pep_end") {
  # ---- Boundary validation (fail fast) ------------------------------------
  if (!is.data.frame(matched_cache)) {
    stop("pelsa_sequence_coverage: matched_cache must be a data.frame")
  }
  if (!is.list(fasta_map) && !is.character(fasta_map)) {
    stop("pelsa_sequence_coverage: fasta_map must be a named list or ",
         "character vector")
  }
  for (col in c(acc_col, start_col, end_col)) {
    if (!col %in% colnames(matched_cache)) {
      stop("pelsa_sequence_coverage: column '", col, "' not found in ",
           "matched_cache")
    }
  }

  out_cols <- c(
    "accession", "covered_residues", "protein_length", "coverage",
    "over_length_flag"
  )
  empty_out <- data.frame(
    accession = character(0),
    covered_residues = integer(0),
    protein_length = integer(0),
    coverage = numeric(0),
    over_length_flag = logical(0),
    stringsAsFactors = FALSE
  )

  acc <- as.character(matched_cache[[acc_col]])
  start <- matched_cache[[start_col]]
  end <- matched_cache[[end_col]]

  if (length(acc) == 0L) {
    return(empty_out)
  }

  # Spans must be integer-coercible. suppressWarnings keeps the message clean;
  # the NA check below is the real guard.
  start <- suppressWarnings(as.integer(start))
  end <- suppressWarnings(as.integer(end))
  if (anyNA(start) || anyNA(end)) {
    stop("pelsa_sequence_coverage: pep_start / pep_end must be ",
         "integer-coercible (got NA after coercion)")
  }
  # A span with start > end is a producer bug (2B always emits start <= end).
  if (any(start > end)) {
    stop("pelsa_sequence_coverage: found span(s) with start > end; ",
         "pep_start must be <= pep_end")
  }

  # ---- Vectorized interval union (single grouped data.table pass) ---------
  # cummax(shift(end)) gives, per row, the max end of PRIOR spans within the
  # accession. A row opens a new merged block when its start exceeds that
  # running max (start > prior-max-end). cumsum() of that flag ids blocks; the
  # FIRST row of each accession (no prior end -> NA) always opens a block.
  # Prefer the non-mutating grouped-aggregate form (over an in-place `:=`) for
  # clarity and immutability: a single grouped aggregate computes the union
  # length per accession in one pass.
  dt <- data.table::data.table(accession = acc, start = start, end = end)
  data.table::setorder(dt, accession, start, end)

  covered <- dt[, list(covered_residues = .pelsa_union_length(start, end)),
                by = "accession"]

  acc_vec <- covered$accession
  covered_residues <- covered$covered_residues

  # ---- FASTA length with isoform-base fallback ----------------------------
  protein_length <- .pelsa_resolve_fasta_length(acc_vec, fasta_map)

  # ---- Coverage: soft-fail-to-NA + clamp/warn/flag on over-length ---------
  # Spans came from substring matching the SAME FASTA, so covered <= length
  # always holds for a correctly-mapped protein. An over-length span would be a
  # 2B regression, but it must NOT abort the whole user-facing Summary metric
  # (consistent with this module's soft-fail posture: unresolved key -> NA,
  # zero-length -> NA, depth helper -> NA). So we CLAMP covered to the protein
  # length, emit ONE warning listing the offending accession(s) so a real
  # regression still surfaces in logs, and set over_length_flag = TRUE for those
  # rows so the Summary tab can badge them. A zero-length FASTA is NOT an
  # over-length anomaly -- it is excluded here and yields coverage NA below.
  resolved <- !is.na(protein_length)
  over <- resolved & protein_length > 0L & covered_residues > protein_length
  if (any(over)) {
    warning("pelsa_sequence_coverage: covered residues exceed protein length ",
            "for accession(s): ", paste(acc_vec[over], collapse = ", "),
            "; clamping to protein length")
    covered_residues[over] <- protein_length[over] # new value, no input mutation
  }

  coverage <- rep(NA_real_, length(acc_vec))
  ok <- resolved & protein_length > 0L
  coverage[ok] <- covered_residues[ok] / protein_length[ok]

  out <- data.frame(
    accession = acc_vec,
    covered_residues = covered_residues,
    protein_length = protein_length,
    coverage = coverage,
    over_length_flag = over,
    stringsAsFactors = FALSE
  )
  out[, out_cols]
}

# Union length of inclusive 1-based intervals, sorted by (start, end).
#
# Vectorized sweep-line (NO loop over intervals): cummax of the PRIOR end gives
# the running max-end of earlier spans; a span opens a NEW merged block when its
# start exceeds that running max (start > prior-max-end). cumsum() ids blocks;
# the union length is the sum over blocks of (block_end - block_start + 1).
# Touching spans ([1,5],[5,10]) merge (5 <= 5); merely-adjacent spans
# ([1,5],[6,10]) stay disjoint (6 > 5). Called once PER ACCESSION by the
# data.table grouped aggregate -- the per-call body is fully vectorized, so the
# only "loop" is data.table's C-level grouping, never an R peptide loop.
#
# @param start integer span starts (already sorted ascending within the group)
# @param end   integer span ends (paired with start)
# @return integer union length (covered residues)
# @noRd
.pelsa_union_length <- function(start, end) {
  prior_max_end <- cummax(c(-Inf, utils::head(end, -1L)))
  block_id <- cumsum(start > prior_max_end)
  block_start <- tapply(start, block_id, min)
  block_end <- tapply(end, block_id, max)
  as.integer(sum(block_end - block_start + 1L))
}

# Resolve each accession's FASTA length, falling back to the isoform base.
#
# Mirrors 2B's .pelsa_resolve_fasta_seq() resolution: exact key first, then the
# isoform base ("P12345-2" -> "P12345"). Name-indexed lookup (NOT
# unlist()[match()]) is used so ragged / zero-length map entries cannot shift
# indices. Returns nchar() of the resolved sequence, NA where no key matches.
#
# @param accession  character vector of accessions
# @param fasta_map  named list / character vector accession -> sequence
# @return integer vector of FASTA lengths (NA where unresolved)
# @noRd
.pelsa_resolve_fasta_length <- function(accession, fasta_map) {
  fasta_vec <- vapply(
    fasta_map,
    function(s) if (length(s) >= 1L) as.character(s)[[1L]] else NA_character_,
    character(1)
  )

  seq_exact <- unname(fasta_vec[accession])

  base_acc <- sub("-[0-9]+$", "", accession)
  needs_base <- is.na(seq_exact) & base_acc != accession
  if (any(needs_base)) {
    seq_exact[needs_base] <- unname(fasta_vec[base_acc[needs_base]])
  }

  as.integer(nchar(seq_exact)) # nchar(NA) -> NA_integer_ already
}
