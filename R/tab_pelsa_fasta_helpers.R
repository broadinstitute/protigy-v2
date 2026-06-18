################################################################################
# Module: PELSA FASTA reader + FASTA-substring peptide-position mapping.
#
# Two pure (non-reactive) helpers, the highest parity-risk piece of PELSA:
#
#   pelsa_read_fasta(path)           -> named list accession -> AA string
#   pelsa_map_peptide_positions(...) -> list(matched=, unmatched=)
#
# The matched coordinates (pep_start / pep_end) drive every aa<pos> label
# (intensity lines, volcano marker labels), sequence-coverage spans, and
# feature-overlap annotation. The unmatched table feeds the Summary
# "peptides failed to match FASTA" QC table; its reason taxonomy
# {accession_absent, sequence_not_found, bad_sequence_format} lets a
# wrong-FASTA/species be distinguished from benign formatting drops.
#
# Vectorized only (PELSA frames are 100k+ rows). The bulk path runs one regex
# over the sequence column, resolves all FASTA keys vectorized, and matches with
# a single stri_locate_all_fixed() call over the row vectors. The I->L retry is
# computed on the miss mask and (since it needs per-pair sequence rewriting)
# loops only over the small set of DISTINCT (sequence, accession) misses --
# never over all rows. Keep these free of Shiny reactivity (unit-testable).
################################################################################

# Read a FASTA file into a named list: accession -> amino-acid string.
#
# Two parse modes, chosen by the caller from the resolved species TYPE (never by
# header content):
#   "uniprot"      Parses UniProt-style headers (">sp|P12345|NAME ...",
#                  ">tr|A0A...|...") and bare headers (">ACC ..."). The accession
#                  key is the pipe-delimited second field for sp|/tr| headers,
#                  else the first whitespace-delimited token.
#   "self_curated" The accession key is ALWAYS the first whitespace-delimited
#                  token, even when the header contains a pipe (custom databases
#                  whose headers are not UniProt-formatted). Everything after the
#                  first field is the protein description and is ignored.
# Sequence blocks spanning multiple lines are concatenated into one upper-cased
# string.
#
# Vectorized: readLines() once, then header lines are located vectorized and
# sequence blocks grouped with cumsum(); no char-by-char scan.
#
# @param path  path to a FASTA file
# @param mode  "uniprot" (default; pipe-aware) or "self_curated" (first-token).
# @return named list accession -> upper-cased amino-acid string
# @noRd
pelsa_read_fasta <- function(path, mode = c("uniprot", "self_curated")) {
  mode <- match.arg(mode)
  if (length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("pelsa_read_fasta: FASTA file not found: ", path)
  }

  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))] # drop blank lines
  if (length(lines) == 0L) {
    stop("pelsa_read_fasta: FASTA file is empty: ", path)
  }

  is_header <- startsWith(lines, ">")
  header_idx <- which(is_header)
  if (length(header_idx) == 0L) {
    stop("pelsa_read_fasta: no FASTA header lines ('>...') found in: ", path)
  }

  # Each line belongs to the most recent header (group id via cumsum).
  group <- cumsum(is_header)

  # ---- Accession keys from header lines (vectorized) ----------------------
  headers <- sub("^>", "", lines[header_idx])
  # First whitespace-delimited token (e.g. "sp|P12345|NAME_HUMAN" or "BARE").
  first_tok <- sub("\\s.*$", "", headers)
  if (identical(mode, "self_curated")) {
    # Self-curated: the first whitespace token IS the accession, regardless of
    # any pipe in the header. Trailing fields (description) are ignored.
    keys <- first_tok
  } else {
    # UniProt: sp|P12345|... or tr|A0A...|... -> take the middle pipe field; else
    # the token.
    pipe_acc <- sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", first_tok)
    has_pipes <- grepl("\\|", first_tok)
    keys <- ifelse(has_pipes, pipe_acc, first_tok)
  }

  # ---- Sequence blocks per group (vectorized split, not char-by-char) -----
  seq_lines <- lines[!is_header]
  seq_groups <- group[!is_header]
  # Concatenate sequence lines per group, then upper-case.
  seq_by_group <- tapply(
    seq_lines, factor(seq_groups, levels = seq_along(header_idx)),
    FUN = function(x) toupper(paste0(x, collapse = "")), simplify = TRUE
  )
  seqs <- as.character(seq_by_group)
  seqs[is.na(seqs)] <- "" # header with no sequence lines -> empty string

  # ---- Duplicate-accession check ------------------------------------------
  # Records are keyed by accession; assigning names() keeps the first sequence
  # for any repeated key (first-wins). Both shipped FASTAs have zero duplicates,
  # but a custom/edited FASTA could, so warn (behavior otherwise unchanged).
  dups <- unique(keys[duplicated(keys)])
  if (length(dups)) {
    warning(
      "pelsa_read_fasta: ", length(dups),
      " duplicated accession(s); first sequence kept for each: ",
      paste(utils::head(dups, 20L), collapse = ", "),
      if (length(dups) > 20L) ", ..." else ""
    )
    # Make first-wins structural: keep only the first record per accession so
    # the returned map has one entry per unique key (length()/iteration safe).
    keep <- !duplicated(keys)
    keys <- keys[keep]
    seqs <- seqs[keep]
  }

  out <- as.list(seqs)
  names(out) <- keys
  out
}

# Resolve a FASTA sequence for an accession, falling back to the isoform base.
#
# Tries the exact key first; if absent and the accession carries a UniProt
# isoform suffix ("-<digits>"), strips it and tries the base key. Vectorized.
#
# @param accession  character vector of accessions
# @param fasta_map  named list accession -> sequence
# @return character vector of sequences (NA where neither key exists)
# @noRd
.pelsa_resolve_fasta_seq <- function(accession, fasta_map) {
  # Flatten the (possibly ragged) map to a length-1-per-entry NAMED vector.
  # NOTE: do NOT use unlist()[match()] -- unlist DROPS zero-length elements
  # (character(0)/NULL), shifting indices so a later entry silently gets a
  # neighbor's sequence (wrong pep_start) or NA (false accession_absent).
  # Name-indexed lookup is immune to ragged/empty entries.
  fasta_vec <- vapply(
    fasta_map,
    function(s) if (length(s) >= 1L) as.character(s)[[1L]] else NA_character_,
    character(1)
  )

  # Exact-key sequence by NAME (NA where the key is absent).
  seq_exact <- unname(fasta_vec[accession])

  # Base accession for isoform suffixes ("P12345-2" -> "P12345").
  base_acc <- sub("-[0-9]+$", "", accession)
  needs_base <- is.na(seq_exact) & base_acc != accession
  if (any(needs_base)) {
    seq_exact[needs_base] <- unname(fasta_vec[base_acc[needs_base]])
  }
  seq_exact
}

# Locate all (optionally overlapping) occurrence starts of peptides in sequences.
#
# One vectorized stri_locate_all_fixed() call over paired (sequence, peptide)
# vectors with overlap = TRUE so overlapping repeats (e.g. "AAA" in "AAAA") are
# all captured -- verified against the synthetic generator's overlap case.
#
# @param sequences  character vector of FASTA sequences
# @param peptides   character vector of peptide sequences (same length)
# @return list of integer vectors of 1-based start positions (length-0 = no hit)
# @noRd
.pelsa_locate_starts <- function(sequences, peptides) {
  if (length(sequences) == 0L) return(list())
  locs <- stringi::stri_locate_all_fixed(
    sequences, peptides,
    opts_fixed = stringi::stri_opts_fixed(overlap = TRUE)
  )
  lapply(locs, function(m) {
    starts <- m[, "start"]
    starts <- starts[!is.na(starts)]
    as.integer(starts)
  })
}

# Map exploded (peptide x accession) rows to FASTA substring positions.
#
# For each (peptide, accession) pair, substring-matches the peptide against the
# resolved FASTA sequence and emits ONE matched row PER OCCURRENCE. Unmatchable
# pairs are recorded in a side unmatched table with a reason.
#
# Algorithm (gold-standard logic):
#   1. Validate sequence: PEP.StrippedSequence is already pure [A-Z]; if a
#      sequence is not ^[A-Z]+$ -> unmatched reason "bad_sequence_format"
#      (never substring-searched).
#   2. Resolve FASTA key with isoform-base fallback; if neither key exists ->
#      unmatched reason "accession_absent".
#   3. Exact substring match with overlap = TRUE for all occurrences.
#   4. I->L isobaric retry on the miss subset: normalize I->L on BOTH peptide
#      and FASTA sequence, match again (positions remain valid -- same-length
#      substitution). Computed only over DISTINCT (sequence, accession) misses.
#   5. Pairs still unmatched after retry -> unmatched reason
#      "sequence_not_found".
#   6. Return list(matched=, unmatched=).
#
# @param exploded_df  long frame from pelsa_explode_accessions()
# @param fasta_map    named list accession -> sequence
# @param seq_col      peptide sequence column (default "PEP.StrippedSequence")
# @param acc_col      accession column (default "accession")
# @param gene_col     gene column (default "gene")
# @param pos_token_col Spectronaut position token column
#                      (default "pep_position_token")
# @return list(matched = data.frame, unmatched = data.frame)
# @noRd
pelsa_map_peptide_positions <- function(exploded_df,
                                        fasta_map,
                                        seq_col = "PEP.StrippedSequence",
                                        acc_col = "accession",
                                        gene_col = "gene",
                                        pos_token_col = "pep_position_token") {
  stopifnot(is.data.frame(exploded_df), is.list(fasta_map))
  for (col in c(seq_col, acc_col)) {
    if (!col %in% colnames(exploded_df)) {
      stop("pelsa_map_peptide_positions: column '", col, "' not found")
    }
  }

  matched_cols <- c(
    colnames(exploded_df),
    "pep_start", "pep_end", "pep_occurrence_idx", "n_occurrences"
  )
  empty_matched <- {
    out <- exploded_df[0L, , drop = FALSE]
    out$pep_start <- integer(0)
    out$pep_end <- integer(0)
    out$pep_occurrence_idx <- integer(0)
    out$n_occurrences <- integer(0)
    out
  }
  empty_unmatched <- data.frame(
    peptide_sequence = character(0), accession = character(0),
    gene = character(0), pep_position = character(0), reason = character(0),
    stringsAsFactors = FALSE
  )

  n <- nrow(exploded_df)
  if (n == 0L) {
    return(list(matched = empty_matched, unmatched = empty_unmatched))
  }

  seqs <- as.character(exploded_df[[seq_col]])
  accs <- as.character(exploded_df[[acc_col]])
  genes <- if (gene_col %in% colnames(exploded_df)) {
    as.character(exploded_df[[gene_col]])
  } else {
    rep(NA_character_, n)
  }
  pos_tokens <- if (pos_token_col %in% colnames(exploded_df)) {
    as.character(exploded_df[[pos_token_col]])
  } else {
    rep(NA_character_, n)
  }

  # ---- Classify rows (vectorized) -----------------------------------------
  # 1. Sequence format validation (one regex over the whole column). An NA
  #    peptide sequence is also treated as invalid -> bad_sequence_format
  #    (NA and malformed sequences are intentionally lumped under one reason).
  is_valid_seq <- !is.na(seqs) & grepl("^[A-Z]+$", seqs)
  # 2. Resolve FASTA sequence with isoform-base fallback.
  fasta_seq <- .pelsa_resolve_fasta_seq(accs, fasta_map)
  has_fasta <- !is.na(fasta_seq)

  reason <- rep(NA_character_, n)
  reason[!is_valid_seq] <- "bad_sequence_format"
  reason[is_valid_seq & !has_fasta] <- "accession_absent"

  # Candidate rows actually eligible for substring matching.
  candidate <- is_valid_seq & has_fasta

  # ---- Exact substring match (vectorized over candidates) -----------------
  starts_list <- vector("list", n)
  if (any(candidate)) {
    starts_list[candidate] <- .pelsa_locate_starts(
      fasta_seq[candidate], seqs[candidate]
    )
  }
  n_hits <- vapply(starts_list, length, integer(1))

  # ---- I->L isobaric retry on the miss subset -----------------------------
  miss <- candidate & n_hits == 0L
  if (any(miss)) {
    # Distinct (sequence, accession) misses only -> small loop, never 100k rows.
    miss_idx <- which(miss)
    key <- paste(seqs[miss_idx], accs[miss_idx], sep = "\r")
    uniq_first <- miss_idx[!duplicated(key)]

    il_pep <- gsub("I", "L", seqs[uniq_first], fixed = TRUE)
    il_seq <- gsub("I", "L", fasta_seq[uniq_first], fixed = TRUE)
    il_starts <- .pelsa_locate_starts(il_seq, il_pep)

    # Map the per-distinct results back to every miss row sharing that key.
    names(il_starts) <- key[!duplicated(key)]
    starts_list[miss_idx] <- il_starts[key]
    n_hits[miss_idx] <- vapply(starts_list[miss_idx], length, integer(1))
  }

  # Anything still unmatched after retry among candidates -> sequence_not_found.
  still_missing <- candidate & n_hits == 0L
  reason[still_missing] <- "sequence_not_found"

  # ---- Build matched rows (one per occurrence, vectorized expansion) ------
  matched_rows <- candidate & n_hits > 0L
  if (any(matched_rows)) {
    row_idx <- rep.int(which(matched_rows), n_hits[matched_rows])
    pep_start <- unlist(starts_list[matched_rows], use.names = FALSE)
    occ_idx <- sequence(n_hits[matched_rows])
    n_occ <- rep.int(n_hits[matched_rows], n_hits[matched_rows])
    pep_len <- nchar(seqs[row_idx])

    matched <- exploded_df[row_idx, , drop = FALSE]
    rownames(matched) <- NULL
    matched$pep_start <- as.integer(pep_start)
    matched$pep_end <- as.integer(pep_start + pep_len - 1L)
    matched$pep_occurrence_idx <- as.integer(occ_idx)
    matched$n_occurrences <- as.integer(n_occ)
  } else {
    matched <- empty_matched
  }

  # ---- Build unmatched rows -----------------------------------------------
  unmatched_mask <- !is.na(reason)
  if (any(unmatched_mask)) {
    unmatched <- data.frame(
      peptide_sequence = seqs[unmatched_mask],
      accession = accs[unmatched_mask],
      gene = genes[unmatched_mask],
      pep_position = pos_tokens[unmatched_mask],
      reason = reason[unmatched_mask],
      stringsAsFactors = FALSE
    )
    rownames(unmatched) <- NULL
  } else {
    unmatched <- empty_unmatched
  }

  list(matched = matched, unmatched = unmatched)
}
