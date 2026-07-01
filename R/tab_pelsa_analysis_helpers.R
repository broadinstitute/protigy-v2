################################################################################
# Module: PELSA explode helpers
#
# Pure (non-reactive) helper for exploding a peptide-level frame on its
# ;-delimited PG.ProteinAccessions into one row per (peptide, accession).
#
# This is the first of the PELSA pure-compute helpers. Its token-alignment rules
# (how ;-aligned PG.Genes / PEP.PeptidePosition tokens are matched to each
# accession) are the single source of truth for ;-handling across PELSA: the
# explode output feeds FASTA position mapping, sequence coverage, best-peptide
# rollup, and feature annotation.
#
# Vectorized only (PELSA frames are 100k+ rows): all rows are split once with
# strsplit(), then expanded with rep()/unlist(). Gene/position tokens are aligned
# to accessions with global index arithmetic (sequence()/cumsum()/integer
# indexing) -- NO R-level loop over peptide rows anywhere. Keep these free of
# Shiny reactivity so they remain unit-testable.
################################################################################

# Count the TRUE number of ;-delimited slots per row, including trailing-empty
# fields that strsplit() drops. "A;B" -> 2, "A;" -> 2, "A" -> 1, "" / NA -> 1.
# Used so the token-recycle rule keys off real slot count, not the strsplit
# length (which silently collapses "GENE1;" to a single token).
# @param x      character (or coercible) vector of ;-delimited strings
# @param n_row  expected length (for the all-NA / zero-length fallback)
# @return integer vector of length n_row
# @noRd
.pelsa_count_slots <- function(x, n_row) {
  x <- as.character(x)
  if (length(x) != n_row) x <- rep(x, length.out = n_row)
  # number of ";" in each string; +1 = slot count. A separator-free string is
  # 1 slot. An NA field is also 1 slot: gregexpr(";", NA) returns NA (NOT the -1
  # "no match" sentinel), so an `m[1] == -1L` test yields NA and breaks the
  # if() -- treat that NA the same as "no separators" (0).
  n_sep <- vapply(
    gregexpr(";", x, fixed = TRUE),
    function(m) if (length(m) == 1L && (is.na(m[1]) || m[1] == -1L)) 0L else length(m),
    integer(1)
  )
  n_sep + 1L
}

# Align a list of ;-split token vectors to per-row accession counts, returning a
# flat character vector ordered to match the exploded (per-accession) rows.
#
# FULLY VECTORIZED: no R-level loop over peptide rows. All work is O(total
# exploded rows) via sequence()/cumsum()/rep() and integer indexing into the
# flattened token vector.
#
# Alignment rule (per row):
#   - if the token count equals the accession count -> align 1:1 by index;
#   - else if there is exactly ONE token -> recycle it to all accessions;
#   - otherwise align by index up to the available tokens, filling missing
#     positions with NA (never error).
# Empty-string tokens (after trimming) become NA so downstream fallback logic
# can detect "no value".
#
# @param token_lists  list of character vectors (already strsplit on ";")
# @param n_acc        integer vector, accession count per ORIGINAL row
# @param row_idx      integer vector mapping each exploded row to its original
#                     row (length = sum(n_acc); = rep(seq_along(n_acc), n_acc))
# @param n_tok_true   optional integer vector of the TRUE per-row token count
#                     (slots), counting trailing-empty fields that strsplit()
#                     silently drops. When supplied, the recycle branch only
#                     fires for a genuinely separator-free single token; a row
#                     like "GENE1;" (1 split token but 2 true slots) is index-
#                     padded so the trailing slot becomes NA instead of being
#                     recycled. Defaults to lengths(token_lists) for callers
#                     that do not have the original strings.
# @return character vector of length sum(n_acc), aligned to exploded rows
# @noRd
.pelsa_align_tokens <- function(token_lists, n_acc, row_idx, n_tok_true = NULL) {
  total <- length(row_idx)
  if (total == 0L) return(character(0))

  # Per-original-row SPLIT token counts and the flattened, trimmed token vector.
  n_tok <- lengths(token_lists)
  flat_tok <- trimws(unlist(token_lists, use.names = FALSE))
  if (is.null(flat_tok)) flat_tok <- character(0)
  # Start offset (0-based) of each row's tokens within flat_tok.
  tok_offset <- cumsum(n_tok) - n_tok

  # The recycle decision must use the TRUE slot count (which counts trailing
  # empties), not the strsplit length: "GENE1;" splits to 1 token but is 2 slots
  # and must NOT recycle. Indexing into flat_tok still uses the split count.
  if (is.null(n_tok_true)) n_tok_true <- n_tok

  # Within-row accession index k = 1,2,..,n1,1,2,..,n2,... for the exploded rows.
  k <- sequence(n_acc)
  # Per-exploded-row view of the original row's split + true token counts.
  n_tok_row <- n_tok[row_idx]
  n_tok_true_row <- n_tok_true[row_idx]

  # Choose which within-row token each exploded row takes:
  #   true slot count == 1 AND a token exists -> token 1 (recycle a genuinely
  #       shared single value; gated on n_tok>=1 so an empty/NA field whose
  #       strsplit() yields zero tokens does NOT index into the next row)
  #   k <= split-token count -> token k (1:1 / index-pad to available tokens)
  #   otherwise             -> NA (missing/trailing-empty tail, or empty field)
  chosen <- ifelse(n_tok_true_row == 1L & n_tok_row >= 1L, 1L,
                   ifelse(k <= n_tok_row, k, NA_integer_))

  # Global index into flat_tok = per-row offset + chosen within-row index.
  global_idx <- tok_offset[row_idx] + chosen
  vals <- flat_tok[global_idx] # NA index -> NA value

  # Empty-string tokens are treated as "no value".
  vals[!is.na(vals) & !nzchar(vals)] <- NA_character_
  vals
}

# Explode a peptide-level frame on its ;-delimited protein accessions.
#
# Produces a long data.frame with one row per (original row, accession token).
# The ;-aligned gene and PEP.PeptidePosition tokens are aligned to each
# accession per .pelsa_align_tokens() above. Empty accession tokens are dropped
# (so "A;;B" yields A and B, not an empty middle). All non-exploded original
# columns are carried through unchanged so downstream joins keep the peptide
# sequence and intensity columns.
#
# @param df        data.frame of peptide rows (Spectronaut-style export)
# @param acc_col   column holding ;-delimited protein accessions
# @param gene_col  column holding ;-aligned gene tokens
# @param pos_col   column holding ;-aligned PEP.PeptidePosition tokens
# @param id_col    optional column name to use as the stable row identifier; if
#                  NULL, a .row_id (1-based original row index) is synthesized
#                  and included
# @return long data.frame with columns: the id (id_col or .row_id), accession,
#         gene, protein_name (;-aligned PG.ProteinNames token, NA when absent),
#         pep_position_token, plus all other original columns
# @noRd
pelsa_explode_accessions <- function(df,
                                     acc_col  = "PG.ProteinAccessions",
                                     gene_col = "PG.Genes",
                                     pos_col  = "PEP.PeptidePosition",
                                     id_col   = NULL,
                                     name_col = "PG.ProteinNames") {
  stopifnot(is.data.frame(df))
  if (!acc_col %in% colnames(df)) {
    stop("pelsa_explode_accessions: acc_col '", acc_col, "' not found in df")
  }

  n_row <- nrow(df)

  # Stable per-original-row identifier. Synthesize .row_id when none supplied.
  if (is.null(id_col)) {
    id_name <- ".row_id"
    if (id_name %in% colnames(df)) {
      stop("pelsa_explode_accessions: df already has a '.row_id' column; ",
           "pass id_col to use it as the identifier")
    }
    df[[id_name]] <- seq_len(n_row)
  } else {
    if (!id_col %in% colnames(df)) {
      stop("pelsa_explode_accessions: id_col '", id_col, "' not found in df")
    }
    id_name <- id_col
  }

  if (n_row == 0L) {
    out <- df[0L, , drop = FALSE]
    out$accession <- character(0)
    out$gene <- character(0)
    out$protein_name <- character(0)
    out$pep_position_token <- character(0)
    return(out)
  }

  # Split every row's accessions ONCE, then trim/drop empties on the FLATTENED
  # vector (one trimws() call total -- per-row trimws() is the 54x perf trap).
  acc_split <- strsplit(as.character(df[[acc_col]]), ";", fixed = TRUE)
  n_acc_raw <- lengths(acc_split)
  flat_acc <- trimws(unlist(acc_split, use.names = FALSE))
  if (is.null(flat_acc)) flat_acc <- character(0)
  # Strip a single leading FASTA header '>' that some Spectronaut exports carry
  # verbatim into PG.ProteinAccessions (per ;-token: ">WP_001.1;>WP_002.1").
  # FASTA keys never include the '>', so a '>'-prefixed token would fail every
  # downstream lookup (accession_absent). Trim first (above), then strip, so a
  # "> WP_001.1" with stray space is also handled. A bare accession is untouched.
  flat_acc <- sub("^>", "", flat_acc)
  # Map each flattened token back to its original row. Per-row RAW (pre-prune)
  # accession counts drive the gene/position alignment so each accession slot --
  # including empty ones -- gets its own gene/position token aligned by index.
  flat_row_raw <- rep.int(seq_len(n_row), n_acc_raw)
  # Drop empty AND NA accession tokens (strsplit(NA) -> NA, and nzchar(NA) is
  # TRUE, so NA must be excluded explicitly). A row whose accessions are all
  # empty/NA contributes zero exploded rows.
  keep <- !is.na(flat_acc) & nzchar(flat_acc)
  accession <- flat_acc[keep]
  # Per-row KEPT counts (post-prune) drive the carried-through column expansion.
  n_acc <- tabulate(flat_row_raw[keep], nbins = n_row)

  # Expand all carried-through columns by repeating each original row n_acc times.
  row_idx <- rep.int(seq_len(n_row), n_acc)
  out <- df[row_idx, , drop = FALSE]
  rownames(out) <- NULL

  # Align gene + position tokens to the exploded accession order.
  gene_lists <- if (gene_col %in% colnames(df)) {
    strsplit(as.character(df[[gene_col]]), ";", fixed = TRUE)
  } else {
    vector("list", n_row)
  }
  pos_lists <- if (pos_col %in% colnames(df)) {
    strsplit(as.character(df[[pos_col]]), ";", fixed = TRUE)
  } else {
    vector("list", n_row)
  }
  name_lists <- if (name_col %in% colnames(df)) {
    strsplit(as.character(df[[name_col]]), ";", fixed = TRUE)
  } else {
    vector("list", n_row)
  }

  # Align gene/position/protein_name against the RAW accession slots (including
  # empties), then apply the SAME keep-mask used for accessions. This keeps each
  # kept accession paired with its own gene/position/name even when an empty token
  # is interspersed (e.g. "A;;B" -> A keeps slot 1, B keeps slot 3, not dropped).
  flat_row_idx_raw <- rep.int(seq_len(n_row), n_acc_raw)
  # True per-row slot counts (count ";" separators + 1) so trailing-empty slots
  # that strsplit() drops are still counted. An NA / empty field is 1 slot. This
  # stops the recycle branch from firing on "GENE1;" (1 split token, 2 slots),
  # while a genuinely separator-free single token ("SHARED") still recycles.
  gene_slots <- .pelsa_count_slots(if (gene_col  %in% colnames(df)) df[[gene_col]]  else NA, n_row)
  pos_slots  <- .pelsa_count_slots(if (pos_col   %in% colnames(df)) df[[pos_col]]   else NA, n_row)
  name_slots <- .pelsa_count_slots(if (name_col  %in% colnames(df)) df[[name_col]]  else NA, n_row)
  gene_raw <- .pelsa_align_tokens(gene_lists,  n_acc_raw, flat_row_idx_raw, gene_slots)
  pos_raw  <- .pelsa_align_tokens(pos_lists,   n_acc_raw, flat_row_idx_raw, pos_slots)
  name_raw <- .pelsa_align_tokens(name_lists,  n_acc_raw, flat_row_idx_raw, name_slots)

  out$accession <- accession
  out$gene <- gene_raw[keep]
  out$protein_name <- name_raw[keep]
  out$pep_position_token <- pos_raw[keep]

  # Drop the original ;-delimited acc/gene/pos columns? No - carry originals
  # through unchanged per contract; the new columns are additive.
  out
}
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
# a single stri_locate_all_fixed() call over the row vectors. Sequences are
# trusted verbatim: an exact substring miss is a real miss (NO I->L / isobaric
# fuzzy retry). Keep these free of Shiny reactivity (unit-testable).
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

# Read ONLY the accession keys from a FASTA file (no sequence concatenation).
#
# A lightweight cousin of pelsa_read_fasta for callers that need the accession
# UNIVERSE but not the sequences -- e.g. the refresh universe + size estimate,
# which use only names(fasta_map). Parsing a proteome FASTA's full sequence map
# just to count headers is wasteful (a multi-second, memory-heavy concatenation
# on the Shiny event loop); this reads the header lines and applies the SAME key
# rule as pelsa_read_fasta, skipping the sequence work entirely.
#
# @param path  path to a FASTA file
# @param mode  "uniprot" (default; pipe-aware) or "self_curated" (first-token).
# @return character vector of unique accession keys (first-wins on duplicates).
# @noRd
pelsa_read_fasta_accessions <- function(path, mode = c("uniprot",
                                                       "self_curated")) {
  mode <- match.arg(mode)
  if (length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("pelsa_read_fasta_accessions: FASTA file not found: ", path)
  }
  lines <- readLines(path, warn = FALSE)
  headers <- sub("^>", "", lines[startsWith(lines, ">")])
  if (length(headers) == 0L) return(character(0))

  first_tok <- sub("\\s.*$", "", headers)
  keys <- if (identical(mode, "self_curated")) {
    first_tok
  } else {
    pipe_acc <- sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", first_tok)
    ifelse(grepl("\\|", first_tok), pipe_acc, first_tok)
  }
  keys <- keys[!is.na(keys) & nzchar(keys)]
  unique(keys)  # first-wins, matching pelsa_read_fasta's de-dup
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
#   3. Exact substring match with overlap = TRUE for all occurrences. Sequences
#      are trusted verbatim -- there is NO I->L (Leu/Ile) or other isobaric
#      fuzzy retry; an exact substring miss is a real miss.
#   4. Candidates that still did not match -> unmatched reason
#      "sequence_not_found".
#   5. Return list(matched=, unmatched=).
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

  # Candidates that did not exact-match are unmatched. We trust the FASTA and
  # peptide sequences verbatim: no I->L (Leu/Ile) or other isobaric retry.
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
################################################################################
# Module: PELSA within-condition CV helpers
#
# Pure (non-reactive) helpers for the single CV definition used everywhere CV
# appears in PELSA (per-condition KDE in Summary, per-sample companion CV).
# CV is ALWAYS computed on RAW (un-log-transformed, LINEAR), NON-normalized
# intensities, then sd/mean*100. These helpers take whatever matrix they are
# given and assume it is ALREADY LINEAR (the math here does NOT log/delinearize
# and does NOT normalize).
#
# IMPORTANT CALLER CONTRACT: Protigy's `GCTs_original` is the LOG-TRANSFORMED
# matrix (post perform_log_transformation), NOT raw linear. CV is NOT invariant
# under log. So the SOLE caller (pelsa_run_analysis_one in
# tab_pelsa_analysis_helpers.R) must DELINEARIZE that matrix by the dataset's
# declared log base (pelsa_delinearize: log2 => 2^x, log10 => 10^x, None/NA =>
# already-linear pass-through) BEFORE handing it to these helpers. Passing a
# log-space matrix in here yields a quantitatively WRONG CV.
#
# VECTORIZED ONLY. PELSA matrices are 100k+ rows. All per-row work uses
# matrixStats (rowMeans2 / rowSds) and rowSums over per-condition column blocks.
# The ONLY R-level loop allowed is over the handful of CONDITIONS
# (O(n_conditions)). There is NO apply()/loop over peptide ROWS anywhere --
# apply(x, 1, sd) is the documented ~54x performance trap and is never used.
#
# KDE / density curve rendering is NOT this helper's concern (Phase 6 renders the
# curve); these helpers only build the tidy CV table. Keep free of Shiny
# reactivity so they remain unit-testable.
################################################################################

# Compute per-peptide-row CV within each condition on the RAW (non-normalized) matrix.
#
# Pipeline: for each condition compute, per peptide row over that condition's
# RAW (non-normalized) replicate columns (NA ignored): cv_pct = sd / mean * 100,
# where sd is the SAMPLE sd (ddof = 1, matrixStats::rowSds default) and mean is
# rowMeans2(na.rm = TRUE). The caller delinearizes the matrix first; CV is
# computed on the non-normalized linear intensities (no sum-normalization).
#
# cv_status per (row, condition):
#   "insufficient_replicates"  if n_nonNA < min_nonNA  (cv_pct = NA)
#   "non_finite"               if n_nonNA >= min_nonNA but cv_pct is not finite
#                              (mean is 0 / NaN / Inf, or sd / result not finite)
#                              (cv_pct = NA)
#   "ok"                       otherwise (cv_pct finite)
# When status != "ok", cv_pct is set to NA.
#
# VECTORIZED with matrixStats: for each condition's column block use
# rowMeans2() / rowSds() (na.rm = TRUE) and rowSums(!is.na(block)) for n_nonNA.
# The only loop is over the handful of CONDITIONS. NEVER apply(x, 1, sd).
#
# CONTRACT: callers must supply NON-NEGATIVE raw linear intensities. A negative
# mean is not guarded -- it would yield a finite negative cv_pct flagged "ok" --
# so non-negativity is a precondition, not a runtime check.
#
# @param raw_mat        numeric matrix of RAW intensities (or data.frame block).
# @param condition_map  named/positional condition vector (see compute helpers).
# @param min_nonNA      minimum non-NA replicates for a finite CV (>= 1L).
# @return tidy long data.frame: columns row_id (1-based peptide row index into
#         raw_mat), condition, cv_pct, n_nonNA, cv_status; one row per
#         (peptide, condition).
# @noRd
pelsa_within_condition_cv <- function(raw_mat, condition_map, min_nonNA = 3L) {
  if (is.data.frame(raw_mat)) raw_mat <- as.matrix(raw_mat)

  stopifnot(
    "raw_mat must be a matrix" = is.matrix(raw_mat),
    "raw_mat must be numeric" = is.numeric(raw_mat),
    "min_nonNA must be a single value >= 1" =
      length(min_nonNA) == 1L && !is.na(min_nonNA) && min_nonNA >= 1L
  )
  min_nonNA <- as.integer(min_nonNA)

  cond <- .pelsa_resolve_condition_map(condition_map, raw_mat)

  n_row <- nrow(raw_mat)
  conditions <- unique(cond)
  row_ids <- seq_len(n_row)

  # Accumulate one per-condition block of length n_row, then rbind once.
  parts <- vector("list", length(conditions))
  for (i in seq_along(conditions)) {
    cnd <- conditions[i]
    cols <- which(cond == cnd)
    block <- raw_mat[, cols, drop = FALSE]

    n_nonNA <- rowSums(!is.na(block)) # vectorized, no per-row loop
    means <- matrixStats::rowMeans2(block, na.rm = TRUE)
    sds <- matrixStats::rowSds(block, na.rm = TRUE)
    cv_pct <- sds / means * 100

    status <- rep("ok", n_row)
    insufficient <- n_nonNA < min_nonNA
    status[insufficient] <- "insufficient_replicates"
    nonfinite <- !insufficient & !is.finite(cv_pct)
    status[nonfinite] <- "non_finite"
    cv_pct[status != "ok"] <- NA_real_

    parts[[i]] <- data.frame(
      row_id = row_ids,
      condition = rep(cnd, n_row),
      cv_pct = cv_pct,
      n_nonNA = as.integer(n_nonNA),
      cv_status = status,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

# Validate and resolve a condition map to a per-column character vector aligned
# to colnames(mat). Accepts a named vector (names must match columns) or a
# positionally-aligned vector. Fails fast on length/name mismatch.
#
# @param condition_map  named or positional character vector
# @param mat            the matrix whose columns it describes
# @return character vector of length ncol(mat), ordered to match the columns
# @noRd
.pelsa_resolve_condition_map <- function(condition_map, mat) {
  n_col <- ncol(mat)
  stopifnot(
    "condition_map length must match number of columns" =
      length(condition_map) == n_col
  )
  cn <- colnames(mat)
  nm <- names(condition_map)
  if (!is.null(nm) && !is.null(cn)) {
    stopifnot(
      "condition_map names must match matrix column names" =
        setequal(nm, cn)
    )
    condition_map <- condition_map[cn] # reorder to column order
  }
  as.character(unname(condition_map))
}
################################################################################
# Module: PELSA per-sample quantified-peptide depth helpers
#
# Pure (non-reactive) helpers feeding the Summary per-sample DEPTH bar plot (one
# bar per sample = number of quantified peptides) plus a companion table
# (mean / median / CV of those per-sample COUNTS).
#
# SOURCE matrix = the PROCESSED GCT (log2) matrix (GCTs_and_params()$GCTs[[ome]]),
# NOT the raw uploaded intensities. This DIFFERS from the within-condition CV
# the CV helper below in this file (R/tab_pelsa_analysis_helpers.R), which operates on RAW linear intensities.
#
# QUANTIFIED MASK -- the canonical pelsa_quantified_mask (finite AND non-zero,
# `is.finite(x) & x != 0`). This DEPARTS from the notebook's literal `> 0`:
# the notebook ran that mask on RAW LINEAR intensities (where `> 0` correctly
# means "detected"), but Protigy applies it to the PROCESSED matrix, which is
# log-transformed and median-normalized. On a log/median-centered matrix a real
# low-abundance peptide has a NEGATIVE value and median-centering pushes ~half
# of all finite values <= 0, so `> 0` silently dropped up to ~50% of genuine
# measurements. `!= 0` keeps negatives (real, low-abundance) while still
# excluding NA (upstream maps raw 0 -> NA before logging) and exact-zero; on a
# LINEAR matrix (never negative) it is identical to the old `> 0`.
#
# CV DISTINCTION: pelsa_depth_summary()'s cv_pct is the PLAIN linear CV of the
# per-sample COUNTS (sample sd / mean * 100, ddof = 1) -- it is the CV of the
# COUNT VECTOR, NOT the CV of intensities. This is consistent with the single CV
# definition used across PELSA (sd / mean * 100); it just happens to be applied
# to the integer depth counts here.
#
# VECTORIZED ONLY. PELSA matrices are 100k+ rows. The per-sample count is a
# single colSums() over a logical matrix -- there is NO per-row/per-column
# apply() loop. Summary stats are plain base-R aggregates over the count vector.
#
# ORDERING is the CALLER's job: bars/table are ordered by the user's sample_order
# in Phase 6. These helpers return counts keyed by column name in matrix-column
# order and do not re-order. Keep free of Shiny reactivity for unit-testability.
################################################################################

# Presence/absence mask for a PELSA intensity matrix: TRUE where a value is a
# GENUINE measurement. A value is "quantified" iff it is finite AND non-zero.
#
# This is the single, canonical definition of "quantified" shared by per-sample
# depth (pelsa_peptides_per_sample), fully-quantified counts, and per-condition
# membership, so the three never drift apart.
#
# Why `!= 0` and not the notebook's `> 0`:
#   - LINEAR matrix (log_transformation = "None"): raw intensities are never
#     negative; a literal 0 means "not detected" -> excluded. So `!= 0` is
#     IDENTICAL to the old `> 0` here -- no behavior change on linear data.
#   - LOG-transformed / normalized matrix: a real low-abundance peptide has a
#     NEGATIVE log value (raw intensity < 1), and median-centering pushes ~half
#     of all finite values <= 0 BY CONSTRUCTION. The old `> 0` silently dropped
#     those genuine measurements (under-counting depth / fully-quantified /
#     membership by up to ~50%). `!= 0` keeps them. Upstream maps raw 0 -> NA
#     before logging, so NA already encodes absence; the only value `!= 0`
#     excludes on log data is an exact log == 0 (raw intensity exactly 1), a
#     negligible measure-zero edge.
# NA / NaN / Inf / -Inf -> FALSE (not finite). 0 -> FALSE (absent).
# @noRd
pelsa_quantified_mask <- function(mat) {
  is.finite(mat) & mat != 0
}

# Count, per sample column, how many peptides are "quantified" in a PROCESSED
# matrix, using the canonical pelsa_quantified_mask (finite & non-zero).
#
# Vectorized: colSums() over the logical mask matrix -- one pass, no apply loop.
#
# @param processed_mat numeric matrix (rows = peptides, cols = samples) of
#                       PROCESSED values, OR a data.frame coerced to matrix
#                       (documented). Must have UNIQUE column (sample) names
#                       (duplicates make the named-integer return ambiguous for
#                       downstream counts[name] selection).
# @return NAMED integer vector; names = sample (column) names in column order,
#         values = number of quantified peptides per sample.
# @noRd
pelsa_peptides_per_sample <- function(processed_mat) {
  # Coerce a data.frame to a numeric matrix (documented).
  if (is.data.frame(processed_mat)) processed_mat <- as.matrix(processed_mat)

  stopifnot(
    "processed_mat must be a matrix" = is.matrix(processed_mat),
    "processed_mat must be numeric" = is.numeric(processed_mat),
    "processed_mat must have column (sample) names" =
      !is.null(colnames(processed_mat)),
    "processed_mat must have unique column (sample) names" =
      !anyDuplicated(colnames(processed_mat))
  )

  counts <- colSums(pelsa_quantified_mask(processed_mat))
  storage.mode(counts) <- "integer"
  counts
}

# Companion summary statistics over the per-sample quantified-count vector.
#
# cv_pct is the PLAIN linear CV of the COUNTS (sample sd ddof=1 / mean * 100) --
# the CV of the count vector, NOT of intensities (see banner). Edge cases:
#   - empty vector       -> all stats NA (mean/median/cv).
#   - single sample      -> cv_pct NA (sample sd of one value is undefined/NA).
#   - mean count == 0    -> cv_pct NA (avoid Inf/NaN; non-finite -> NA).
#   - NA elements        -> propagate to NA mean_n/median_n/cv_pct (no na.rm).
#                           The producer pelsa_peptides_per_sample() never
#                           yields NA (colSums over a logical mask is always a
#                           finite count), so an NA here signals a CALLER BUG;
#                           we propagate rather than silently masking it.
#
# @param n_quantified     named integer vector from pelsa_peptides_per_sample()
#                          (or any numeric vector).
# @param total_n_peptides optional total peptide count carried through (the
#                          notebook sets total_n_peptides = nrow(data_df), i.e.
#                          ALL GCT rows). Defaults to NA_integer_ when not given.
# @return one-row data.frame with columns mean_n, median_n, cv_pct,
#         total_n_peptides (export-friendly).
# @noRd
pelsa_depth_summary <- function(n_quantified, total_n_peptides = NULL) {
  stopifnot(
    "n_quantified must be numeric" =
      is.numeric(n_quantified) || length(n_quantified) == 0L,
    "total_n_peptides must be NULL or a single value" =
      is.null(total_n_peptides) || length(total_n_peptides) == 1L
  )

  # Coerce supplied total to integer so the output column type is stable
  # whether the caller passes 500 or 500L.
  total <- if (is.null(total_n_peptides)) {
    NA_integer_
  } else {
    as.integer(total_n_peptides)
  }

  if (length(n_quantified) == 0L) {
    return(data.frame(
      mean_n = NA_real_,
      median_n = NA_real_,
      cv_pct = NA_real_,
      total_n_peptides = total,
      stringsAsFactors = FALSE
    ))
  }

  mean_n <- mean(n_quantified)
  median_n <- stats::median(n_quantified)
  # Sample sd (ddof = 1); NA for a single element. Guard non-finite cv -> NA.
  sd_n <- stats::sd(n_quantified)
  cv_pct <- sd_n / mean_n * 100
  if (!is.finite(cv_pct)) cv_pct <- NA_real_

  data.frame(
    mean_n = mean_n,
    median_n = median_n,
    cv_pct = cv_pct,
    total_n_peptides = total,
    stringsAsFactors = FALSE
  )
}
################################################################################
# Module: PELSA peptide helpers
#
# Pure (non-reactive) per-peptide computations:
#   pelsa_missed_cleavages() - tryptic missed-cleavage count (notebook parity)
#   pelsa_peptide_length()   - residue count
#   pelsa_build_multilabel() - canonical ;-joined gene_aa<pos> label builder
#
# Missed-cleavage MUST match the analysis notebook exactly. The notebook rule is
#   core = peptide[:-1]; len(re.findall(r'[KR](?!P)', core))
# i.e. drop the C-terminal residue, then count internal K/R NOT immediately
# followed by P (the Keil trypsin rule: K-P and R-P are not cleaved). This count
# feeds the Summary missed-cleavage distribution and is parity-critical, so the
# implementation mirrors the regex on substr(seq, 1, nchar(seq) - 1L).
#
# pelsa_build_multilabel() is the single source of truth for volcano-point
# labels, reused by the best-peptide rollup (2G) and the volcano-df builders
# (3A) for both all-peptide and best-peptide panels.
#
# All functions are vectorized where the per-peptide contract allows (PELSA
# frames are 100k+ rows). They take no Shiny reactivity so they stay
# unit-testable.
################################################################################

# Count tryptic missed cleavages for each peptide sequence.
#
# Matches the notebook's exact rule: drop the C-terminal residue, then count
# internal K or R that is NOT immediately followed by P. stringr uses ICU regex,
# so the negative lookahead `[KR](?!P)` is supported. Peptides shorter than 2
# residues have no internal positions and yield 0. NA sequences yield NA.
#
# @param seq character vector of stripped peptide sequences
# @return integer vector of missed-cleavage counts, same length as `seq`
# @noRd
pelsa_missed_cleavages <- function(seq) {
  n <- length(seq)
  if (n == 0L) return(integer(0))

  # Coerce factor columns (e.g. from a TSV/join read) so nchar() never errors.
  seq <- as.character(seq)

  # Drop the C-terminal residue; sequences shorter than 2 have an empty core.
  len <- nchar(seq)
  core <- substr(seq, 1L, len - 1L)
  core[!is.na(len) & len < 2L] <- ""

  # Count internal K/R not immediately followed by P (Keil rule, K-P excluded).
  counts <- stringr::str_count(core, "[KR](?!P)")

  # Preserve NA sequences as NA; str_count already returns NA for NA input,
  # this guard makes the contract explicit and robust to backend differences.
  counts[is.na(seq)] <- NA_integer_
  as.integer(counts)
}

# Peptide residue count.
#
# Centralizes peptide-length logic so callers do not scatter nchar() calls.
# NA sequences yield NA.
#
# @param seq character vector of stripped peptide sequences
# @return integer vector of residue counts, same length as `seq`
# @noRd
pelsa_peptide_length <- function(seq) {
  # Coerce factor columns (e.g. from a TSV/join read) so nchar() never errors.
  as.integer(nchar(as.character(seq)))
}

# Resolve the label STEM (the text before "_aa<pos>") for a set of peptide
# mappings. Fallback order: gene -> protein_name -> accession. A stem is
# "missing" when it is NA or blank/whitespace after trimming (readr renders a
# blank report cell as NA, so both must count as missing). Self-curated species
# have no UniProt gene and no protein name, so they force the accession stem.
#
# Vectorized: every argument is a character vector (or a scalar recycled by the
# fifelse cascade). `protein_name` may be NULL when the caller's report has no
# PG.ProteinNames column, in which case the middle tier is skipped and the
# fallback degrades to the legacy gene -> accession behavior.
#
# @param gene            character vector of gene symbols (may be NA/"")
# @param protein_name    character vector of protein names (may be NA/""), or
#                        NULL when unavailable
# @param accession       character vector of accessions (the final fallback)
# @param is_self_curated single logical; TRUE forces the accession stem
# @return character vector of label stems, same length as `accession`
# @noRd
pelsa_resolve_label_stem <- function(gene, protein_name, accession,
                                     is_self_curated = FALSE) {
  accession <- as.character(accession)
  if (isTRUE(is_self_curated)) return(accession)

  gene <- as.character(gene)
  if (is.null(protein_name)) {
    protein_name <- rep(NA_character_, length(accession))
  } else {
    protein_name <- as.character(protein_name)
  }

  missing_gene <- is.na(gene) | !nzchar(trimws(gene))
  missing_name <- is.na(protein_name) | !nzchar(trimws(protein_name))

  # gene -> protein_name -> accession, applied as a two-step cascade so a blank
  # gene with a real protein name lands on the name, and both blank lands on the
  # accession.
  stem <- ifelse(missing_gene,
                 ifelse(missing_name, accession, protein_name),
                 gene)
  as.character(stem)
}

# Build one ;-joined multi-label string for a single peptide/dot.
#
# Given the per-mapping (gene, position, accession) vectors for ONE peptide
# (all the same length; these are the distinct accession x occurrence mappings
# in PG.ProteinAccessions token order), produce a single label string:
#   - each entry is "<gene>_aa<pos>", falling back to protein-name then
#     accession when the gene is empty/NA;
#   - fully-identical entries collapse to one;
#   - distinct entries (e.g. same gene at different positions) are kept;
#   - no cap; entries are joined with ";" in first-occurrence input order.
# Vectorization across many peptides is the caller's job (2G/3A): this builds
# exactly one label. Empty input returns NA_character_.
#
# An NA position renders literally as "<id>_aaNA": this is NOT guarded, by
# design, since callers pass FASTA-resolved positions (positions are expected to
# be resolved before this builder is reached).
#
# @param genes         character vector of gene symbols (may contain "" or NA)
# @param positions     integer or character vector of residue positions
# @param accessions    character vector of protein accessions (final fallback)
# @param is_self_curated single logical; TRUE forces accession stem
# @param protein_names character vector of PG.ProteinNames tokens (may be
#                      NA/""), or NULL when unavailable
# @return character scalar label for the peptide
# @noRd
pelsa_build_multilabel <- function(genes, positions, accessions,
                                   is_self_curated = FALSE,
                                   protein_names = NULL) {
  if (length(genes) == 0L) return(NA_character_)

  # Fail fast on length mismatch: a silent scalar recycle would emit a
  # plausible-but-wrong volcano label, which is worse than a loud failure.
  stopifnot(
    length(genes) == length(positions),
    length(genes) == length(accessions)
  )
  if (!is.null(protein_names)) {
    stopifnot(length(genes) == length(protein_names))
  }

  # Stem fallback gene -> protein_name -> accession (single source of truth).
  # Self-curated species have no UniProt gene/name: the resolver forces the
  # accession stem.
  label_id <- pelsa_resolve_label_stem(genes, protein_names, accessions,
                                       is_self_curated)

  entries <- paste0(label_id, "_aa", as.character(positions))

  # Collapse fully-identical entries, preserving first-occurrence order.
  entries <- entries[!duplicated(entries)]
  paste(entries, collapse = ";")
}
################################################################################
# Module: PELSA best-peptide-per-protein rollup helpers
#
# Pure (non-reactive) two-step rollup of an EXPLODED stat frame (one row per
# peptide x accession, already carrying per-contrast adj.P.Val / logFC for the
# contrast the caller selected) into one dot per distinct best-peptide.
#
#   Step 1 - per-accession best peptide (the notebook's _rollup_to_proteins):
#     STABLE sort on [adj.P.Val, logFC, peptide_seq, accession], then keep the
#     FIRST row per accession. The last two keys are a DETERMINISTIC total-order
#     tiebreak so the chosen "best" peptide is fully reproducible even on exact
#     (adj.P.Val, logFC) ties. data.table::setorder is a stable sort; the
#     four-key total order makes the head(1)-per-accession pick deterministic.
#
#   Step 2 - regroup winners by peptide (Protigy refinement):
#     A peptide has a SINGLE (adj.P.Val, logFC) coordinate, so a peptide that
#     won multiple accessions must be ONE dot, not several overlapping ones.
#     Group the step-1 winners by peptide_seq and emit one row per distinct
#     best-peptide, carrying a ;-joined multi-label (one <gene>_aa<pos> per won
#     accession) built via pelsa_build_multilabel() (the single source of truth
#     for labels - reused, not reimplemented).
#
# NA handling: rows with NA adj.P.Val sort LAST (na.last = TRUE) so a peptide
# with a real p-value is preferred over an NA one. An accession whose ONLY
# peptide has all-NA stats still keeps that peptide (its adj_p / logFC are NA).
#
# The down-only (logFC < 0) PELSA signature is applied via the most-negative-
# logFC tiebreak ONLY (ascending logFC), NOT as a filter - all peptides remain
# eligible. adj.P.Val / logFC are computed upstream (Statistics tab) and passed
# in by the caller; this module does not compute stats.
#
# Vectorized: setorder + per-accession .SD[1L], then a per-peptide group-by.
# pelsa_build_multilabel() is called ONCE PER DISTINCT BEST-PEPTIDE (the small
# winners set), never per exploded row. No per-row apply over the full frame.
################################################################################

# Roll an exploded per-(peptide, accession) stat frame up to one dot per
# distinct best-peptide.
#
# @param exploded_stat_df data.frame; one row per peptide x accession, carrying
#   the selected contrast's adj.P.Val / logFC plus accession / gene / pep_start.
# @param adjp_col   column name of the (selected-contrast) adjusted p-value
# @param logfc_col  column name of the (selected-contrast) log fold change
# @param pep_col    column name of the stripped peptide sequence
# @param acc_col    column name of the protein accession
# @param gene_col   column name of the gene symbol
# @param pos_col    column name of the FASTA-resolved peptide start position
# @return data.frame, one row per distinct best-peptide, with columns:
#   peptide_seq, adj_p, logFC, label, won_accessions, n_won
# @noRd
pelsa_best_peptide_rollup <- function(exploded_stat_df,
                                      adjp_col  = "adj.P.Val",
                                      logfc_col = "logFC",
                                      pep_col   = "PEP.StrippedSequence",
                                      acc_col   = "accession",
                                      gene_col  = "gene",
                                      pos_col   = "pep_start",
                                      is_self_curated = FALSE) {
  # ---- Boundary validation (fail fast) ------------------------------------
  if (!is.data.frame(exploded_stat_df)) {
    stop("pelsa_best_peptide_rollup: exploded_stat_df must be a data.frame")
  }
  required <- c(adjp_col, logfc_col, pep_col, acc_col, gene_col, pos_col)
  missing_cols <- setdiff(required, colnames(exploded_stat_df))
  if (length(missing_cols) > 0L) {
    stop("pelsa_best_peptide_rollup: missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }

  out_cols <- c("peptide_seq", "adj_p", "logFC", "label",
                "won_accessions", "n_won")
  empty_out <- data.frame(
    peptide_seq    = character(0),
    adj_p          = numeric(0),
    logFC          = numeric(0),
    label          = character(0),
    won_accessions = character(0),
    n_won          = integer(0),
    stringsAsFactors = FALSE
  )
  if (nrow(exploded_stat_df) == 0L) return(empty_out)

  # ---- Project to a minimal, stably-named data.table ----------------------
  dt <- data.table::data.table(
    peptide_seq  = as.character(exploded_stat_df[[pep_col]]),
    accession    = as.character(exploded_stat_df[[acc_col]]),
    gene         = as.character(exploded_stat_df[[gene_col]]),
    protein_name = if ("protein_name" %in% colnames(exploded_stat_df))
      as.character(exploded_stat_df[["protein_name"]]) else
      rep(NA_character_, nrow(exploded_stat_df)),
    pep_start    = exploded_stat_df[[pos_col]],
    adj_p        = as.numeric(exploded_stat_df[[adjp_col]]),
    logFC        = as.numeric(exploded_stat_df[[logfc_col]])
  )

  # ---- Step 1: stable total-order sort + first row per accession ----------
  # setorder is a STABLE sort; na.last = TRUE pushes NA adj_p / logFC to the
  # end so a real-stat peptide outranks an NA one. The peptide_seq / accession
  # keys give a deterministic total order so exact (adj_p, logFC) ties resolve
  # to the same row on every run.
  data.table::setorder(dt, adj_p, logFC, peptide_seq, accession,
                        na.last = TRUE)
  winners <- dt[, .SD[1L], by = "accession"]

  # ---- Step 2: regroup winners by peptide -> one dot per best-peptide -----
  # Per distinct best-peptide: its single (adj_p, logFC) coordinate, a ;-joined
  # multi-label over the accessions it won, the ;-joined won accessions, and the
  # win count. pelsa_build_multilabel runs once per peptide group (small set).
  out_dt <- winners[, {
    # The LABEL's ;-joined entries are ordered by (pep_start, accession) so the
    # best_peptide multi-label matches the all_peptide panel's entry order
    # (.pelsa_volcano_labels sorts by (pep_start, accession)); otherwise the two
    # panels could show the same peptide's ;-joined tokens in a different order.
    # NOTE: this reorders ONLY the display label. won_accessions stays in the
    # winners' stats-priority row order because its FIRST token is the
    # representative won accession (.pelsa_best_back_map drives the dot's
    # protein/gene/span/P.Value/is_marker from won_accessions[1]).
    lab_ord <- order(pep_start, accession)
    list(
      # adj_p[1L] / logFC[1L] rely on the per-peptide-single-coordinate
      # invariant: a peptide has ONE (adj_p, logFC), so any winner row for it
      # carries that same coordinate -- the first row is representative.
      adj_p          = adj_p[1L],
      logFC          = logFC[1L],
      label          = pelsa_build_multilabel(gene[lab_ord], pep_start[lab_ord],
                                               accession[lab_ord],
                                               is_self_curated,
                                               protein_names = protein_name[lab_ord]),
      won_accessions = paste(accession, collapse = ";"),
      n_won          = .N
    )
  }, by = "peptide_seq"]

  # Stable, deterministic row order for reproducible output.
  data.table::setorder(out_dt, adj_p, logFC, peptide_seq, na.last = TRUE)

  out <- as.data.frame(out_dt, stringsAsFactors = FALSE)
  out$n_won <- as.integer(out$n_won)
  out[, out_cols, drop = FALSE]
}
################################################################################
# PELSA marker matching + parsing (Task 2J) - the pure, testable matching/
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
# pelsa_isoform_base consolidates the suffix-strip that the coverage and fasta
# helpers (now in this file, R/tab_pelsa_analysis_helpers.R) and
# tab_pelsa_annotation_helpers.R each inlined
# (sub("-[0-9]+$", "", x)); those files are intentionally left unchanged - this
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
################################################################################
# Module: PELSA Start-Analysis - pure validation + compute-pipeline assembly
# (Task 5D).
#
# The Setup tab's "Start Analysis" button (tab_pelsa_section1.R) runs a one-shot
# compute pipeline that assembles the verified Phase-2 helpers into a per-dataset
# cache object the Summary (Phase 6) and Volcano (Phase 7) sections READ. The
# heavy, network-bound, and reactivity-bound wiring lives in the observer; the
# PURE logic lives here so it unit-tests with NO live network and NO Shiny.
#
# Public helpers (all @noRd):
#   pelsa_validate_setup(setup_snapshot, gcts, database_dir)
#       -> list(ok = TRUE/FALSE, errors = character()). Pre-flight checklist.
#   pelsa_setup_snapshot(setup_state)
#       -> a plain immutable list copy of the live reactiveValues, taken under
#          isolate() at click so mid-compute input edits cannot corrupt a run.
#   pelsa_dataset_peptide_frame(gct)
#       -> a peptide-level data.frame = cbind(rdesc, mat) for a cmapR GCT (or a
#          plain data.frame passed straight through; the test seam).
#   pelsa_condition_map_for(cdesc, sample_cols, condition_col)
#       -> named character vector sample -> condition, aligned to sample_cols
#          (the condition_map pelsa_within_condition_cv() consumes).
#   pelsa_run_analysis(gcts, gcts_original, setup_snapshot, fasta_map, feat_df,
#                      resolve_fasta = NULL, resolve_feat = NULL, ...)
#       -> named-by-dataset list of per-dataset cache objects (the integration
#          crux). NO network, NO Shiny: the observer reads each dataset's uploaded
#          FASTA + annotation file and passes them in; tests inject synthetic ones.
#
# ------------------------------------------------------------------------------
# DECISIONS (documented per the task spec)
# ------------------------------------------------------------------------------
# ANNOTATION-AS-UPLOADED. pelsa_run_analysis NEVER fetches from UniProt. It uses
#   the uploaded feature annotation (feat_df) as-is and records the dataset
#   accessions absent from it via pelsa_unannotated_accessions() (the "failed to
#   resolve annotation" set). This keeps the analysis path network-free, fast,
#   deterministic, and unit-testable. The cache records the unannotated set so the
#   Summary QC can flag poor annotation coverage.
#
# COMPUTE-ALL-AT-START: pelsa_run_analysis computes EVERY checked dataset's heavy
#   objects once per Start-Analysis (simpler, matches the "analyzed datasets"
#   semantics - the switcher then shows exactly the analyzed set). The planning
#   doc's switch-time freeing of INACTIVE rendered objects is a Phase 6/7 render
#   concern, not a compute concern. ALTERNATIVE (documented, not chosen): compute
#   lazily per-active-dataset to bound peak memory with many large datasets - the
#   seam is the per-dataset keying of the returned cache, so a lazy variant would
#   call pelsa_run_analysis_one() (below) on demand instead of looping here.
#
# CONDITION MAP / GCTs_original ALIGNMENT (the integration crux, documented):
#   - CV (2D) runs on RAW LINEAR intensities. We read them from
#     gcts_original[[ds]] - BUT note Protigy's `GCTs_original` is the
#     LOG-TRANSFORMED matrix (post perform_log_transformation), not raw linear.
#     CV is NOT invariant under log, so the pipeline DELINEARIZES that matrix by
#     the dataset's declared log base (params$log_transformation -> log2 => 2^x,
#     log10 => 10^x, None/NA => already-linear pass-through) via
#     pelsa_delinearize() BEFORE CV. Depth (2E) and the
#     intensity-line plot keep using the PROCESSED log2 matrix as-is. The raw
#     matrix columns are the sample
#     names; the condition map is built from THAT dataset's cdesc condition
#     column (setup$condition_col[[ds]]) keyed by sample name, so a named
#     condition_map aligns to columns regardless of column order
#     (.pelsa_resolve_condition_map reorders by name).
#   - Depth (2E) runs on the PROCESSED matrix from gcts[[ds]].
#   - The matched cache's .row_id is the 1-based ROW index into the per-dataset
#     PEPTIDE FRAME (rdesc rows == mat rows, same order), so CV's row_id (1-based
#     row index into the raw matrix) and the peptide frame rows refer to the same
#     peptide. We DO NOT join CV back onto the matched cache here (that is a
#     render-time concern for Summary); we just cache both keyed consistently.
################################################################################

# ---- inline validation message UI --------------------------------------------

# Build the inline validation feedback block (pure tag constructor).
#
# Given a pelsa_validate_setup() result, returns NULL when ok (no markup), else a
# red-bordered list of the specific errors so the user sees exactly what is
# missing. Pure (a function of its args) so it tests without a session.
#
# @param validation a list(ok=, errors=) from pelsa_validate_setup().
# @return NULL (ok) or a shiny tag (the error block).
# @noRd
pelsa_validation_msg_ui <- function(validation) {
  if (isTRUE(validation$ok) || length(validation$errors) == 0L) return(NULL)
  shiny::tags$div(
    class = "pelsa-validation-errors",
    style = paste0("border:1px solid #d9534f; border-radius:6px; ",
                   "padding:10px; margin-top:8px; background:#fdf3f2; ",
                   "color:#a94442;"),
    shiny::tags$strong(
      shiny::icon("circle-exclamation"), " Cannot start analysis:"
    ),
    shiny::tags$ul(
      style = "margin:6px 0 0 0;",
      lapply(validation$errors, function(e) shiny::tags$li(e))
    )
  )
}

# ---- snapshot ----------------------------------------------------------------

# Take a plain, immutable list snapshot of the live setup_state reactiveValues.
#
# Called under isolate() by the Start-Analysis observer so input edits made WHILE
# a run is computing cannot corrupt the in-flight run. Copies only the fields the
# pipeline + validation read. The per-dataset list fields are copied wholesale
# (they are themselves plain lists), so the snapshot shares no reference with the
# live reactiveValues.
#
# PER-DATASET FIELDS (all NAMED LISTS keyed by ome): species, compound,
# marker_rows, skip, condition_col, replicate_col, condition_order,
# replicate_order, sample_order. `datasets` is the NON-SKIPPED ome set (the Setup
# observer sets setup_state$datasets to the analyzed subset before snapshotting).
# Every per-ome field defaults to an empty list() so a fresh / partially-
# configured setup is safe to index with [[ome]].
#
# @param setup_state the live reactiveValues from PELSASection1_Tab_Server.
# @return a plain named list with the same field names.
# @noRd
pelsa_setup_snapshot <- function(setup_state) {
  list(
    datasets        = setup_state$datasets        %||% character(0),
    fasta_path      = setup_state$fasta_path      %||% list(),
    fasta_name      = setup_state$fasta_name      %||% list(),
    annotation_path = setup_state$annotation_path %||% list(),
    annotation_name = setup_state$annotation_name %||% list(),
    self_curated    = setup_state$self_curated    %||% list(),
    compound        = setup_state$compound        %||% list(),
    marker_rows     = setup_state$marker_rows     %||% list(),
    skip            = setup_state$skip            %||% list(),
    condition_col   = setup_state$condition_col   %||% list(),
    replicate_col   = setup_state$replicate_col   %||% list(),
    condition_order = setup_state$condition_order %||% list(),
    replicate_order = setup_state$replicate_order %||% list(),
    sample_order    = setup_state$sample_order    %||% list()
  )
}

# ---- pre-flight validation ---------------------------------------------------

# Pre-flight checklist for Start-Analysis (PURE, closed-form testable).
#
# `setup_snapshot$datasets` is the NON-SKIPPED (analyzed) ome set; the Setup
# observer derives it from the per-ome skip flags before snapshotting. ONLY these
# datasets are validated - a skipped dataset's (possibly incomplete) config is
# never checked.
#
# Checks, accumulating ALL failures (so the user sees every missing piece at
# once, not one-at-a-time):
#   1. >= 1 non-skipped dataset (else "all skipped").
#   2. Each non-skipped dataset has a condition column and replicate column
#      chosen. A value of "(none)" (the blank default) / "" / NA counts as NOT
#      chosen.
#   3. The chosen condition column exists in that dataset's cdesc.
#   4. Each non-skipped dataset has a confirmed (non-empty) condition order.
#   5. Each non-skipped dataset has an uploaded FASTA; and an uploaded annotation
#      file unless it is flagged self-curated.
#
# fasta_path/annotation_path/self_curated/condition_col/replicate_col are PER-OME
# named lists keyed by ome. database_dir is retained for signature stability but
# is no longer used (uploads supersede the on-disk database).
#
# An EMPTY marker table is VALID (markers are a volcano OVERLAY, not a
# prerequisite) - no marker check here.
#
# @param setup_snapshot a pelsa_setup_snapshot() list (or the live reactiveValues
#                        - both support $field access).
# @param gcts           named list of per-ome GCTs (for cdesc column existence).
# @param database_dir   the PELSA database dir (FASTA existence check).
# @return list(ok = logical scalar, errors = character()).
# @noRd
pelsa_validate_setup <- function(setup_snapshot, gcts, database_dir) {
  errors <- character(0)

  datasets <- setup_snapshot$datasets %||% character(0)
  datasets <- as.character(datasets)
  datasets <- datasets[!is.na(datasets) & nzchar(datasets)]

  # 1. >= 1 non-skipped dataset.
  if (length(datasets) == 0L) {
    errors <- c(errors,
                "Enable PELSA analysis for at least one dataset (all are skipped).")
  }

  fasta_path      <- setup_snapshot$fasta_path      %||% list()
  annotation_path <- setup_snapshot$annotation_path %||% list()
  self_curated    <- setup_snapshot$self_curated    %||% list()
  condition_col   <- setup_snapshot$condition_col   %||% list()
  replicate_col   <- setup_snapshot$replicate_col   %||% list()
  condition_order <- setup_snapshot$condition_order %||% list()

  for (ds in datasets) {
    # 2. Condition column (chosen + not the "(none)" default).
    col <- condition_col[[ds]]
    if (.pelsa_is_unset(col)) {
      errors <- c(errors, sprintf(
        "Dataset '%s': choose a condition grouping column.", ds))
    } else {
      # 3. Column must exist in that dataset's cdesc (when the GCT is available).
      gct <- if (is.list(gcts)) gcts[[ds]] else NULL
      cdesc <- .pelsa_gct_cdesc(gct)
      if (!is.null(cdesc) && !(col %in% names(cdesc))) {
        errors <- c(errors, sprintf(
          "Dataset '%s': condition column '%s' is not in its annotations.",
          ds, col))
      }
    }

    # 2. Replicate column (chosen + not the "(none)" default).
    if (.pelsa_is_unset(replicate_col[[ds]])) {
      errors <- c(errors, sprintf(
        "Dataset '%s': choose a replicate identifier column.", ds))
    }

    # 4. Confirmed condition order.
    order <- condition_order[[ds]]
    has_order <- !is.null(order) && length(order) >= 1L &&
      any(!is.na(order) & nzchar(as.character(order)))
    if (!has_order) {
      errors <- c(errors, sprintf(
        "Dataset '%s': confirm the condition order.", ds))
    }

    # 2 + 5. Per-dataset uploads: a FASTA is always required; an annotation file
    # is required unless this dataset is a self-curated database.
    fp <- fasta_path[[ds]]
    if (is.null(fp) || !nzchar(fp %||% "")) {
      errors <- c(errors, sprintf("Dataset '%s': upload a FASTA file.", ds))
    } else if (!file.exists(fp)) {
      errors <- c(errors, sprintf(
        paste0("Dataset '%s': the FASTA file is missing or was moved -- ",
               "re-upload it."), ds))
    }
    if (!isTRUE(self_curated[[ds]])) {
      ap <- annotation_path[[ds]]
      if (is.null(ap) || !nzchar(ap %||% "")) {
        errors <- c(errors, sprintf(
          paste0("Dataset '%s': upload a feature annotation file (or check ",
                 "'Self-curated database')."), ds))
      } else if (!file.exists(ap)) {
        errors <- c(errors, sprintf(
          paste0("Dataset '%s': the feature annotation file is missing or was ",
                 "moved -- re-upload it."), ds))
      }
    }
  }

  list(ok = length(errors) == 0L, errors = errors)
}

# TRUE when a per-ome scalar setting is "not chosen": NULL, non-scalar, NA,
# empty, or the blank "(none)" default the species/condition/replicate selectors
# start at. @noRd
.pelsa_is_unset <- function(v) {
  is.null(v) || length(v) != 1L || is.na(v) || !nzchar(v) ||
    identical(as.character(v), "(none)")
}

# cdesc of a cmapR GCT (or NULL when not a GCT / unavailable). @noRd
.pelsa_gct_cdesc <- function(gct) {
  if (methods::is(gct, "GCT")) {
    return(methods::slot(gct, "cdesc"))
  }
  NULL
}

# ---- delinearize (raw-intensity recovery for CV) -----------------------------

# Recover LINEAR (raw) intensities from a (possibly) log-transformed matrix.
#
# WHY THIS EXISTS: Protigy's `GCTs_original` is NOT the raw uploaded matrix in
# linear space -- it is the matrix AFTER `perform_log_transformation`
# (R/sidebar_setup_helpers_GCT-processing.R), i.e. the LOG-transformed values
# when log2/log10 was selected. The PELSA within-condition CV
# (pelsa_within_condition_cv) is defined on RAW LINEAR intensities (the notebook
# delinearizes before CV; CV is NOT invariant under log).
# So the analysis pipeline must DELINEARIZE `GCTs_original` by the dataset's
# declared log base BEFORE feeding it to the CV path.
#
# The declared base comes from that dataset's setup parameters
# `log_transformation` in {"None","log2","log10"} (perform_log_transformation:
# log2 = log(x,2), log10 = log(x,10); "None" = no transform, and the
# negative-values fallback ALSO sets the method to "None"). Therefore:
#   - "None" / NA / missing  -> the matrix is ALREADY LINEAR; pass it through
#                               UNCHANGED (do NOT exponentiate -- that would
#                               corrupt an already-linear matrix).
#   - "log2"                 -> 2 ^ mat
#   - "log10"                -> 10 ^ mat
# NA stays NA (2^NA == NA), so missingness is preserved.
#
# PURE + closed-form testable: a function of (mat, log_base) only.
#
# @param mat       numeric matrix (or data.frame intensity block, coerced).
# @param log_base  one of "None"/NA/NULL (linear pass-through), "log2", "log10".
# @return numeric matrix in LINEAR space, same shape/dimnames as mat.
# @noRd
pelsa_delinearize <- function(mat, log_base) {
  if (is.data.frame(mat)) mat <- as.matrix(mat)
  if (!is.matrix(mat) || !is.numeric(mat)) {
    stop("pelsa_delinearize: `mat` must be a numeric matrix or data.frame.",
         call. = FALSE)
  }

  base <- if (is.null(log_base) || length(log_base) == 0L) {
    NA_character_
  } else {
    as.character(log_base)[[1]]
  }

  # Already-linear: "None", NA, missing, or empty -> pass through unchanged.
  if (is.na(base) || !nzchar(base) || identical(base, "None")) {
    return(mat)
  }
  if (identical(base, "log2"))  return(2 ^ mat)
  if (identical(base, "log10")) return(10 ^ mat)

  stop(sprintf(
    "pelsa_delinearize: unknown log_base '%s' (expected None/log2/log10).",
    base), call. = FALSE)
}

# ---- GCT -> peptide frame ----------------------------------------------------

# Build a peptide-level data.frame from a cmapR GCT: cbind(@rdesc, @mat).
#
# rdesc carries the peptide annotation columns (PG.ProteinAccessions, PG.Genes,
# PEP.StrippedSequence, PEP.PeptidePosition, ...); mat carries the per-sample
# intensities (cols == sample names). They share row order (rdesc rownames ==
# mat rownames == rid), so cbind aligns peptides to intensities row-for-row. A
# plain data.frame is passed straight through (the test seam: the synthetic
# generator already yields a peptide frame).
#
# Peptide-result exports key on PEP.StrippedSequence, so a peptide GCT normally
# carries that column. Some PELSA peptide datasets, however, were uploaded with
# the stripped sequence AS the id column (the rid / rownames) and so have no
# PEP.StrippedSequence column. To keep position-mapping working for them, when
# PEP.StrippedSequence is absent we synthesize it from the dataset's id column
# (rid). This is additive: a real PEP.StrippedSequence column is always kept.
#
# @param gct a cmapR GCT, or a plain data.frame (returned unchanged).
# @return a peptide-level data.frame (guaranteed to have PEP.StrippedSequence
#         whenever a sequence-bearing id column is available).
# @noRd
pelsa_dataset_peptide_frame <- function(gct) {
  if (is.data.frame(gct)) {
    return(.pelsa_ensure_stripped_sequence(gct, id_values = rownames(gct)))
  }
  if (!methods::is(gct, "GCT")) {
    stop("pelsa_dataset_peptide_frame: expected a cmapR GCT or a data.frame.",
         call. = FALSE)
  }
  rdesc <- methods::slot(gct, "rdesc")
  mat   <- methods::slot(gct, "mat")
  rid   <- methods::slot(gct, "rid")
  mat_df <- as.data.frame(mat, check.names = FALSE, stringsAsFactors = FALSE)
  out <- cbind(rdesc, mat_df)
  out <- .pelsa_ensure_stripped_sequence(out, id_values = rid)
  rownames(out) <- NULL
  out
}

# Guarantee a PEP.StrippedSequence column on a peptide frame.
#
# Peptide results normally use PEP.StrippedSequence as their id column, so for
# PELSA datasets that column may be absent (the stripped sequence sits in the id
# column / rid instead). When PEP.StrippedSequence is missing we copy it from
# the supplied id values so downstream position-mapping has a sequence to match.
# A frame that already has PEP.StrippedSequence is returned unchanged.
#
# @param df         a peptide-level data.frame.
# @param id_values  character vector of per-row id values (the rid / rownames),
#                   used as the stripped sequence when the column is absent. May
#                   be NULL (then the frame is returned unchanged).
# @return df, with a PEP.StrippedSequence column added when it was missing and
#         id_values supplies one per row.
# @noRd
.pelsa_ensure_stripped_sequence <- function(df, id_values = NULL) {
  if ("PEP.StrippedSequence" %in% colnames(df)) return(df)
  if (is.null(id_values) || length(id_values) != nrow(df)) return(df)
  df[["PEP.StrippedSequence"]] <- as.character(id_values)
  df
}

# Numeric sample matrix from a GCT (or the intensity block of a data.frame).
#
# For a GCT: the @mat (rows = peptides, cols = samples). For a plain data.frame
# (test seam): the columns named in `sample_cols`, coerced to a numeric matrix.
#
# @param gct          a cmapR GCT or a peptide data.frame.
# @param sample_cols  sample column names (used for the data.frame seam).
# @return numeric matrix with colnames == sample names, in `sample_cols` order
#         where derivable.
# @noRd
pelsa_dataset_matrix <- function(gct, sample_cols) {
  if (methods::is(gct, "GCT")) {
    return(methods::slot(gct, "mat"))
  }
  if (is.data.frame(gct)) {
    cols <- intersect(sample_cols, colnames(gct))
    if (length(cols) == 0L) {
      stop("pelsa_dataset_matrix: none of `sample_cols` found in data.frame.",
           call. = FALSE)
    }
    m <- as.matrix(gct[, cols, drop = FALSE])
    storage.mode(m) <- "double"
    return(m)
  }
  stop("pelsa_dataset_matrix: expected a cmapR GCT or a data.frame.",
       call. = FALSE)
}

# rid (row id) vector from a GCT (@rid) or a data.frame (rownames). NULL when the
# object carries no usable id (so callers can no-op gracefully).
# @noRd
.pelsa_gct_rids <- function(g) {
  if (methods::is(g, "GCT")) return(methods::slot(g, "rid"))
  if (is.data.frame(g))      return(rownames(g))
  NULL
}

# cid (column/sample id) vector from a GCT (@cid) or a data.frame (colnames).
# NULL when the object carries no usable id (so callers can no-op gracefully).
# @noRd
.pelsa_gct_cids <- function(g) {
  if (methods::is(g, "GCT")) return(methods::slot(g, "cid"))
  if (is.data.frame(g))      return(colnames(g))
  NULL
}

# Restrict the CV-source (ORIGINAL) GCT to the PROCESSED peptide set, BY id, so
# within-condition CV describes exactly the analyzed peptides -- regardless of
# any rows the processing pipeline dropped (missing/SD filters) or reordered.
# (M8/M9)
#
# Both GCTs share one rid namespace: the processed GCT is built from the original
# by row-dropping filters that preserve rownames, so the processed rid set is a
# subset of the original's. The rid IS the identifier column chosen at setup, so
# aligning by rid keeps the CV row set in lock-step with the analysis row set.
# Both ROWS (peptides) and COLUMNS (samples) are restricted/reordered to the
# processed set: peptides by rid, samples by cid. Restricting samples means CV
# reflects exactly the analyzed samples even when setup filtered some out -- this
# matches the QC CV tab's qc_cv_align_source() behavior for consistency. The
# condition map (built from the original's cdesc, then intersected with the
# aligned matrix's columns by the caller) stays valid because it is keyed by name.
#
# We subset the @mat/@rdesc/@rid (and @cdesc/@cid) slots directly rather than via
# cmapR::subset_gct: subset_gct requires an "id" meta column that
# programmatically-built GCTs in this app don't guarantee. Direct slot replacement
# on the (copy-on-modify) local is robust to that and needs no GCT re-validation.
#
# @param gct_original   the unprocessed (CV-source) GCT, or a data.frame seam.
# @param gct_processed  the processed GCT whose rid/cid sets are the target.
# @return gct_original restricted + reordered to the processed rids and cids.
#         Inputs with no usable rid (non-GCT, non-data.frame) are returned
#         unchanged. Sample (cid) restriction is skipped when either side lacks
#         usable cids.
# @noRd
pelsa_align_original_to_processed <- function(gct_original, gct_processed) {
  proc_rids <- .pelsa_gct_rids(gct_processed)
  orig_rids <- .pelsa_gct_rids(gct_original)
  if (is.null(proc_rids) || is.null(orig_rids)) return(gct_original)
  if (anyDuplicated(orig_rids)) {
    stop("pelsa_align_original_to_processed: original GCT has duplicate ids; ",
         "cannot align the CV source by id.", call. = FALSE)
  }
  keep <- proc_rids[proc_rids %in% orig_rids]
  idx  <- match(keep, orig_rids)               # no NAs: keep is a subset of orig

  # Samples to keep, in processed order. NULL (skip) when either side lacks cids.
  proc_cids <- .pelsa_gct_cids(gct_processed)
  orig_cids <- .pelsa_gct_cids(gct_original)
  keep_cols <- if (!is.null(proc_cids) && !is.null(orig_cids)) {
    proc_cids[proc_cids %in% orig_cids]
  } else {
    NULL
  }

  if (methods::is(gct_original, "GCT")) {
    mat   <- methods::slot(gct_original, "mat")[idx, , drop = FALSE]
    rdesc <- methods::slot(gct_original, "rdesc")[idx, , drop = FALSE]
    if (!is.null(keep_cols)) {
      cidx  <- match(keep_cols, orig_cids)     # no NAs: keep_cols subset of orig
      mat   <- mat[, cidx, drop = FALSE]
      methods::slot(gct_original, "cdesc") <-
        methods::slot(gct_original, "cdesc")[cidx, , drop = FALSE]
      methods::slot(gct_original, "cid")   <- as.character(keep_cols)
    }
    methods::slot(gct_original, "mat")   <- mat
    methods::slot(gct_original, "rdesc") <- rdesc
    methods::slot(gct_original, "rid")   <- as.character(keep)
    return(gct_original)
  }
  # data.frame seam (rows = peptides, cols = samples)
  out <- gct_original[keep, , drop = FALSE]
  if (!is.null(keep_cols)) out <- out[, keep_cols, drop = FALSE]
  out
}

# ---- condition map -----------------------------------------------------------

# Build the named condition map (sample -> condition) the CV helper consumes.
#
# For each sample column, look up its condition from cdesc[[condition_col]]
# (cdesc rownames are sample names). The result is a NAMED character vector keyed
# by sample so .pelsa_resolve_condition_map() can reorder it to the matrix's
# column order regardless of how the columns are arranged.
#
# Samples absent from cdesc, or with an NA condition, are dropped (they cannot be
# assigned to a condition). The CALLER subsets the matrix to the returned names.
#
# @param cdesc         the dataset's cdesc (rownames = sample names).
# @param sample_cols   the sample (matrix) column names to map.
# @param condition_col the chosen condition grouping column.
# @return named character vector sample -> condition (a subset of sample_cols).
# @noRd
pelsa_condition_map_for <- function(cdesc, sample_cols, condition_col) {
  if (!is.data.frame(cdesc)) {
    stop("pelsa_condition_map_for: `cdesc` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(condition_col) || length(condition_col) != 1L ||
      is.na(condition_col) || !nzchar(condition_col) ||
      !(condition_col %in% names(cdesc))) {
    stop(sprintf(
      "pelsa_condition_map_for: condition column '%s' not in cdesc.",
      condition_col), call. = FALSE)
  }
  sample_cols <- as.character(sample_cols)
  cond_all <- as.character(cdesc[[condition_col]])
  names(cond_all) <- rownames(cdesc)

  cond <- cond_all[sample_cols]            # NA name -> NA value
  keep <- !is.na(cond) & nzchar(cond)
  out <- cond[keep]
  names(out) <- sample_cols[keep]
  out
}

# ---- failure discriminator ---------------------------------------------------

# Canonical predicate: did a per-dataset cache entry FAIL?
#
# The ONE place that defines the success-vs-failure rule for entries in the
# pelsa_run_analysis() return list. A successful entry is the 12-component cache
# (see the Cache contract below); a failed entry is list(error = <message>,
# stage = <stage label or NA>). Phase 6/7 MUST test with this predicate rather
# than inlining `!is.null(entry$error)`, so the rule lives in one place.
#
# @param entry one element of the pelsa_run_analysis() return list.
# @return TRUE iff the entry represents a failed dataset.
# @noRd
pelsa_analysis_failed <- function(entry) {
  is.list(entry) && !is.null(entry$error)
}

# Reconstruct a cache entry's full annotated frame: `matched` cbound with the 3
# stored feature columns (annotation_features). The cache stores only the 3
# feature columns (row-aligned to `matched`) rather than a full annotated
# duplicate of `matched` (~27MB/dataset saved); this rebuilds the frame consumers
# previously read as `entry$annotation`.
#
# @param entry a SUCCESSFUL per-dataset cache entry (list with $matched and
#              $annotation_features). Behaviour on a failed entry is undefined;
#              callers should gate with pelsa_analysis_failed() first.
# @return data.frame = matched + feature_class_primary/winning_accession/
#         winning_gene, or NULL when the entry lacks the required components.
# @noRd
pelsa_annotation_frame <- function(entry) {
  if (!is.list(entry)) return(NULL)
  matched <- entry$matched
  feats   <- entry$annotation_features
  if (!is.data.frame(matched) || !is.data.frame(feats)) return(NULL)
  if (nrow(matched) != nrow(feats)) {
    stop("pelsa_annotation_frame: matched and annotation_features row counts ",
         "disagree (", nrow(matched), " vs ", nrow(feats), ")", call. = FALSE)
  }
  out <- matched
  rownames(out) <- NULL
  for (col in PELSA_ANNOTATION_FEATURE_COLS) {
    out[[col]] <- feats[[col]]
  }
  out
}

# ---- per-condition membership + distributions (Summary toggle) ---------------

# Peptide -> condition membership over the PROCESSED matrix.
#
# A peptide BELONGS to a condition when it is quantified (the canonical
# pelsa_quantified_mask: finite AND non-zero) in AT LEAST ONE of that condition's
# samples. Many-to-many: a peptide quantified across several conditions appears
# once per condition. Pure (no Shiny, no network).
#
# @param proc_mat      peptides x samples numeric matrix (colnames = samples).
# @param condition_map NAMED vector sample -> condition (pelsa_condition_map_for).
# @return data.frame(row_id = integer 1-based peptide-frame row, condition =
#         character), one row per (peptide, condition) membership. Empty when
#         there are no samples / no mapped conditions.
# @noRd
pelsa_condition_membership <- function(proc_mat, condition_map) {
  if (is.data.frame(proc_mat)) proc_mat <- as.matrix(proc_mat)
  empty <- data.frame(row_id = integer(0), condition = character(0),
                      stringsAsFactors = FALSE)
  if (!is.matrix(proc_mat) || ncol(proc_mat) == 0L || nrow(proc_mat) == 0L) {
    return(empty)
  }
  cm <- condition_map
  if (is.null(cm) || length(cm) == 0L) return(empty)
  keep <- !is.na(cm) & nzchar(as.character(cm))
  cm <- cm[keep]
  samples <- intersect(names(cm), colnames(proc_mat))
  if (length(samples) == 0L) return(empty)

  mask <- pelsa_quantified_mask(proc_mat[, samples, drop = FALSE])
  conds <- unique(as.character(cm[samples]))
  parts <- lapply(conds, function(cond) {
    cols <- samples[as.character(cm[samples]) == cond]
    inc <- rowSums(mask[, cols, drop = FALSE]) > 0   # quantified in >= 1 sample
    rid <- which(inc)
    if (length(rid) == 0L) return(NULL)
    data.frame(row_id = as.integer(rid), condition = cond,
               stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) return(empty)
  do.call(rbind, parts)
}

# Long per-condition peptide-length frame for the Summary length toggle.
#
# @param membership      pelsa_condition_membership() output.
# @param peptide_metrics the cache peptide_metrics frame (row-aligned to the
#                        peptide frame == membership$row_id index space).
# @return data.frame(condition, peptide_length). Empty when no membership.
# @noRd
pelsa_length_by_condition <- function(membership, peptide_metrics) {
  empty <- data.frame(condition = character(0), peptide_length = numeric(0),
                      stringsAsFactors = FALSE)
  if (is.null(membership) || !is.data.frame(membership) ||
      nrow(membership) == 0L) {
    return(empty)
  }
  if (is.null(peptide_metrics) || !is.data.frame(peptide_metrics) ||
      !("peptide_length" %in% names(peptide_metrics))) {
    return(empty)
  }
  len <- suppressWarnings(as.numeric(peptide_metrics$peptide_length))
  rid <- membership$row_id
  ok <- !is.na(rid) & rid >= 1L & rid <= length(len)
  data.frame(condition = as.character(membership$condition[ok]),
             peptide_length = len[rid[ok]],
             stringsAsFactors = FALSE)
}

# Long per-condition sequence-coverage frame for the Summary coverage toggle.
#
# For each condition it subsets the matched cache to the peptides quantified in
# that condition and runs the SAME interval-union coverage as the experiment-wide
# metric, keeping the finite per-accession coverage fractions. @noRd
pelsa_coverage_by_condition <- function(membership, matched, fasta_map,
                                        acc_col = "accession",
                                        start_col = "pep_start",
                                        end_col = "pep_end",
                                        row_id_col = ".row_id") {
  empty <- data.frame(condition = character(0), coverage = numeric(0),
                      stringsAsFactors = FALSE)
  if (is.null(membership) || !is.data.frame(membership) ||
      nrow(membership) == 0L) {
    return(empty)
  }
  if (!is.data.frame(matched) || nrow(matched) == 0L ||
      !(row_id_col %in% names(matched))) {
    return(empty)
  }
  m_rid <- suppressWarnings(as.integer(matched[[row_id_col]]))
  conds <- unique(as.character(membership$condition))
  parts <- lapply(conds, function(cond) {
    rids <- membership$row_id[membership$condition == cond]
    sub <- matched[m_rid %in% rids, , drop = FALSE]
    if (nrow(sub) == 0L) return(NULL)
    cov <- suppressWarnings(
      pelsa_sequence_coverage(sub, fasta_map, acc_col = acc_col,
                              start_col = start_col, end_col = end_col))
    v <- suppressWarnings(as.numeric(cov$coverage))
    v <- v[is.finite(v)]
    if (length(v) == 0L) return(NULL)
    data.frame(condition = cond, coverage = v, stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) return(empty)
  do.call(rbind, parts)
}

# ---- per-dataset assembly ----------------------------------------------------

# Assemble one dataset's per-dataset analysis cache from the verified helpers.
#
# This is the stat-INDEPENDENT object set both Summary and Volcano reuse:
# FASTA-mapped matched/unmatched cache, within-condition CV, depth, sequence
# coverage, missed-cleavage + peptide-length, feature annotation, and mapping /
# annotation QC counts. The volcano's stat-DEPENDENT rollup is Phase 7's job and
# is NOT computed here.
#
# @section Cache contract:
# The returned named list is the load-bearing contract Phases 6 (Summary) and 7
# (Volcano) READ (never recompute). On SUCCESS it has exactly these 13
# components (EXACT names + shapes as implemented):
#   (NOTE: the former full-duplicate `annotation` frame is no longer stored; the
#   cache now carries `annotation_features` - just the 3 feature columns,
#   row-aligned to `matched` - and pelsa_annotation_frame(entry) reconstructs the
#   full annotated frame on demand. ~27MB/dataset saved.)
#   matched        data.frame, one row per (peptide, accession, occurrence) that
#                  FASTA-mapped. Key cols: accession, pep_start, pep_end (1-based
#                  inclusive), pep_occurrence_idx, n_occurrences,
#                  PEP.StrippedSequence, gene, .row_id (1-based row index into the
#                  peptide frame), plus all carried-through peptide-frame cols.
#   unmatched      data.frame, one row per peptide x accession that did NOT map.
#                  Cols: peptide_sequence, accession, gene, pep_position, reason
#                  (one of accession_absent / sequence_not_found /
#                  bad_sequence_format).
#   cv             data.frame OR NULL. One row per (peptide, condition). Cols:
#                  row_id (1-based row index into the raw matrix == peptide frame
#                  row), condition, cv_pct, n_nonNA, cv_status (ok /
#                  insufficient_replicates / non_finite). NULL when there is no
#                  raw GCT, or the condition column is absent / all-NA.
#   n_quantified   NAMED integer vector, one per sample (names = sample columns),
#                  count of quantified peptides per sample.
#   depth_summary  one-row data.frame. Cols: mean_n, median_n, cv_pct,
#                  total_n_peptides (== nrow(peptide frame)).
#   coverage       data.frame, one row per DISTINCT matched accession. Cols:
#                  accession, covered_residues, protein_length, coverage ([0,1] or
#                  NA), over_length_flag.
#   coverage_by_condition data.frame(condition, coverage), the per-protein
#                  coverage fraction split by condition (a peptide belongs to a
#                  condition when quantified in >= 1 of its samples). Empty frame
#                  when no usable processed condition column. Feeds the Summary
#                  coverage panel's per-condition toggle mode.
#   n_peptides_by_condition NAMED integer vector (condition -> count) of peptides
#                  QUANTIFIED (canonical finite & non-zero) in >= 1 sample of the
#                  condition -- the same membership coverage_by_condition uses.
#                  Empty when no usable processed condition column. Feeds the
#                  Summary condition table's n_peptides_quantified column.
#   peptide_metrics data.frame, one row per peptide-frame row. Cols:
#                  PEP.StrippedSequence, missed_cleavages, peptide_length.
#   length_by_condition data.frame(condition, peptide_length), peptide lengths
#                  split by condition (same membership rule as
#                  coverage_by_condition). Empty frame when no usable processed
#                  condition column. Feeds the Summary length panel's
#                  per-condition toggle mode.
#   annotation_features data.frame, row-aligned to `matched`, with exactly 3
#                  columns: feature_class_primary, winning_accession,
#                  winning_gene. The full annotated frame (matched + these 3) is
#                  reconstructed on demand via pelsa_annotation_frame(entry); the
#                  cache does NOT store the full duplicate.
#   unannotated    character vector of accessions present in the matched cache but
#                  ABSENT from feat_df (isoform-base fallback applied).
#   qc             list: n_peptides, n_fully_quantified (peptides quantified --
#                  finite & non-zero -- in ALL samples), n_exploded,
#                  n_matched_rows, n_unmatched_rows,
#                  unmatched_by_reason (named list reason -> count),
#                  n_unannotated_accessions,
#                  n_annotated_with_features (accessions with >=1 real feature
#                  row; isoform-base fallback applied),
#                  n_annotated_zero_feature (accessions present only as sentinel
#                  rows in feat_df).
#
# A FAILED dataset is instead list(error = <message>, stage = <last stage label
# reached, or NA>). Test with pelsa_analysis_failed(entry); the stage names the
# pipeline phase that threw (e.g. "Computing CV").
#
# GRACEFUL ZERO-MATCH: a dataset whose peptides do not FASTA-map at all is NOT an
# error - it returns a valid cache with qc$n_matched_rows == 0L (empty matched /
# coverage / annotation). Phase 6 should check qc$n_matched_rows > 0L before
# drawing coverage.
#
# @param gct           the PROCESSED GCT (or peptide data.frame) for this ds.
# @param gct_original  the GCT (or frame) Protigy stored as `GCTs_original` for
#                      this ds - the CV source. NOTE: this is the
#                      LOG-TRANSFORMED matrix (post perform_log_transformation),
#                      NOT raw linear, so the CV path DELINEARIZES it by
#                      `log_base` first (pelsa_delinearize) to recover the raw
#                      linear intensities CV is defined on. May be NULL (then CV
#                      is skipped, cv = NULL).
# @param fasta_map     named list accession -> sequence (read once by caller).
# @param feat_df       the species feature cache (read once by caller); used
#                      as-is (cache-as-is decision - no UniProt top-up).
# @param condition_col the chosen condition grouping column for this dataset.
# @param min_nonNA     min non-NA replicates for a finite CV (passed to 2D).
# @param log_base      this dataset's declared log transformation, one of
#                      "None"/NA (already linear), "log2", "log10". The CV input
#                      (gct_original's matrix) is delinearized by it BEFORE
#                      CV. The DEPTH metric and intensity-line
#                      use the PROCESSED log2 matrix as-is and are NOT affected.
# @param progress      NULL or a function(detail) advancing a sub-progress stage.
# @param stage_env     NULL or an environment whose $stage the assembly updates
#                      to the current stage label (so a caller's tryCatch can
#                      report WHICH stage failed). Internal seam.
# @return a named list (the per-dataset cache); see the Cache contract above.
# @noRd
pelsa_run_analysis_one <- function(gct,
                                   gct_original,
                                   fasta_map,
                                   feat_df,
                                   condition_col,
                                   min_nonNA = 3L,
                                   log_base = NA_character_,
                                   progress = NULL,
                                   stage_env = NULL) {
  .step <- function(detail) {
    if (is.environment(stage_env)) stage_env$stage <- detail
    if (is.function(progress)) progress(detail)
  }
  .step("Reading dataset")

  if (!is.list(fasta_map)) {
    stop("pelsa_run_analysis_one: `fasta_map` must be a (named) list.",
         call. = FALSE)
  }
  if (!is.data.frame(feat_df)) {
    stop("pelsa_run_analysis_one: `feat_df` must be a data.frame.",
         call. = FALSE)
  }

  # --- peptide frame + matrices --------------------------------------------
  peptides <- pelsa_dataset_peptide_frame(gct)

  # --- 2A explode -> 2B FASTA-map -------------------------------------------
  .step("Mapping peptide positions")
  exploded <- pelsa_explode_accessions(peptides)
  mapped   <- pelsa_map_peptide_positions(exploded, fasta_map)
  matched   <- mapped$matched
  unmatched <- mapped$unmatched

  # --- 2I feature annotation (cache-as-is) ----------------------------------
  .step("Annotating features")
  # Annotate the MATCHED cache (peptide x accession w/ pep_start/pep_end). The
  # annotated frame is `matched` PLUS exactly 3 feature columns
  # (feature_class_primary, winning_accession, winning_gene), row-aligned to
  # `matched`. We store ONLY those 3 columns (not the full annotated duplicate of
  # `matched`, which wasted ~27MB/dataset) and reconstruct the full frame on
  # demand via pelsa_annotation_frame(entry).
  annotation <- pelsa_annotate_features(matched, feat_df)
  annotation_features <- annotation[, PELSA_ANNOTATION_FEATURE_COLS, drop = FALSE]
  rownames(annotation_features) <- NULL
  unannotated <- pelsa_unannotated_accessions(matched, feat_df)
  annotation_status <- pelsa_annotation_status_counts(matched, feat_df)

  # --- 2D within-condition CV on the DELINEARIZED (raw linear) intensities ---
  # GCTs_original is LOG-transformed (Protigy stores the post-log matrix), so we
  # delinearize by this dataset's declared log base BEFORE CV.
  # CV is NOT invariant under log; the notebook delinearizes first. "None"/NA
  # means the matrix is already linear -> pelsa_delinearize passes it through.
  # CANONICAL condition annotation, shared by the CV panel (2D) AND the
  # per-condition membership (Summary toggle) so both describe the SAME
  # sample -> condition mapping. Prefer the ORIGINAL GCT's cdesc (CV's source of
  # truth); fall back to the processed GCT's cdesc for the data.frame seam or
  # when the original lacks the column. Each consumer intersects this map with
  # its own matrix's columns, so a sample filtered out of one matrix simply
  # drops from that panel without desyncing the condition labels.
  cdesc_cond <- if (!is.null(gct_original) && methods::is(gct_original, "GCT")) {
    methods::slot(gct_original, "cdesc")
  } else {
    NULL
  }
  # condition_col is NULL/absent for any dataset with no condition column set
  # (condition_cols[[ds]] -> NULL). `NULL %in% x` is logical(0), and `||`/`&&`
  # with a length-0 operand yields NA -> `if (NA)` crashes the whole dataset's
  # analysis. Guard the arg to a single non-empty string before the %in% test.
  cc_ok <- is.character(condition_col) && length(condition_col) == 1L &&
    !is.na(condition_col) && nzchar(condition_col)
  if (is.null(cdesc_cond) || !is.data.frame(cdesc_cond) || !cc_ok ||
      !(condition_col %in% names(cdesc_cond))) {
    cdesc_cond <- .pelsa_gct_cdesc(gct)
  }
  has_cond_col <- cc_ok && is.data.frame(cdesc_cond) &&
    condition_col %in% names(cdesc_cond)

  .step("Computing CV")
  cv <- NULL
  if (!is.null(gct_original)) {
    # M8/M9: restrict the CV source to the PROCESSED set BY id, so CV describes
    # exactly the analyzed peptides AND samples (processing may drop/reorder rows
    # and filter samples). cdesc_cond above remains valid -- it is keyed by sample
    # name and the cmap below intersects it with the aligned matrix's columns.
    gct_original <- pelsa_align_original_to_processed(gct_original, gct)
    log_mat <- pelsa_dataset_matrix(gct_original, colnames(peptides))
    raw_mat <- pelsa_delinearize(log_mat, log_base)
    if (has_cond_col) {
      cmap <- pelsa_condition_map_for(cdesc_cond, colnames(raw_mat),
                                      condition_col)
      if (length(cmap) > 0L) {
        sub <- raw_mat[, names(cmap), drop = FALSE]
        cv <- pelsa_within_condition_cv(sub, cmap, min_nonNA = min_nonNA)
      }
    }
  }

  # --- 2E peptides-per-sample depth on the PROCESSED matrix ------------------
  .step("Building coverage and depth")
  proc_mat <- pelsa_dataset_matrix(gct, colnames(peptides))
  n_quantified <- pelsa_peptides_per_sample(proc_mat)
  depth_summary <- pelsa_depth_summary(n_quantified,
                                       total_n_peptides = nrow(peptides))

  # Fully-quantified peptides: rows quantified (the canonical pelsa_quantified_
  # mask: finite & non-zero) in EVERY sample. 0 when there are no samples.
  n_fully_quantified <- if (ncol(proc_mat) == 0L) {
    0L
  } else {
    sum(rowSums(!pelsa_quantified_mask(proc_mat)) == 0L)
  }

  # --- 2F sequence coverage from the matched cache + fasta ------------------
  coverage <- pelsa_sequence_coverage(matched, fasta_map)

  # --- 2C missed cleavage + peptide length over the peptide universe --------
  seqs <- if ("PEP.StrippedSequence" %in% colnames(peptides)) {
    as.character(peptides[["PEP.StrippedSequence"]])
  } else {
    character(0)
  }
  peptide_metrics <- data.frame(
    PEP.StrippedSequence = seqs,
    missed_cleavages     = pelsa_missed_cleavages(seqs),
    peptide_length       = pelsa_peptide_length(seqs),
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )

  # --- per-condition length / coverage (Summary toggle) ---------------------
  # Membership over the PROCESSED matrix, keyed by the CANONICAL condition map
  # (cdesc_cond, shared with the CV panel) so the per-condition Summary panels
  # and the CV panel agree on which samples belong to each condition. A peptide
  # belongs to a condition when quantified in >= 1 of its samples. Empty frames
  # when there is no usable condition column.
  length_by_condition <- data.frame(condition = character(0),
                                    peptide_length = numeric(0),
                                    stringsAsFactors = FALSE)
  coverage_by_condition <- data.frame(condition = character(0),
                                      coverage = numeric(0),
                                      stringsAsFactors = FALSE)
  # Per-condition QUANTIFIED peptide count (canonical finite & non-zero in >= 1
  # sample), counted from the same membership the coverage/length panels use, so
  # the Summary "n_peptides_quantified" column means the same "quantified" as the
  # per-sample summary. NAMED integer vector (condition -> count); empty when no
  # usable condition column.
  n_peptides_by_condition <- integer(0)
  if (has_cond_col) {
    cmap_proc <- pelsa_condition_map_for(cdesc_cond, colnames(proc_mat),
                                         condition_col)
    if (length(cmap_proc) > 0L) {
      membership <- pelsa_condition_membership(proc_mat, cmap_proc)
      length_by_condition <- pelsa_length_by_condition(membership,
                                                       peptide_metrics)
      coverage_by_condition <- pelsa_coverage_by_condition(membership, matched,
                                                           fasta_map)
      if (is.data.frame(membership) && nrow(membership) > 0L) {
        n_peptides_by_condition <- table(as.character(membership$condition))
        n_peptides_by_condition <- stats::setNames(
          as.integer(n_peptides_by_condition), names(n_peptides_by_condition))
      }
    }
  }

  # --- mapping / annotation QC counts ---------------------------------------
  reasons <- if ("reason" %in% colnames(unmatched)) {
    as.character(unmatched$reason)
  } else {
    character(0)
  }
  qc <- list(
    n_peptides            = nrow(peptides),
    n_fully_quantified    = n_fully_quantified,
    n_exploded            = nrow(exploded),
    n_matched_rows        = nrow(matched),
    n_unmatched_rows      = nrow(unmatched),
    unmatched_by_reason   = as.list(c(table(reasons))),
    n_unannotated_accessions    = length(unannotated),
    n_annotated_with_features   = annotation_status$n_with_features,
    n_annotated_zero_feature    = annotation_status$n_zero_feature,
    # Disposition buckets from a self-describing annotation (0 unless the
    # uploaded annotation carries a `disposition` column). merged/demerged/
    # deleted accessions are "excluded for a reason", NOT failures -- so
    # n_failed is the true residual (0 when every accession is accounted).
    n_annotated_merged          = annotation_status$n_merged %||% 0L,
    n_annotated_demerged        = annotation_status$n_demerged %||% 0L,
    n_annotated_deleted         = annotation_status$n_deleted %||% 0L,
    n_annotation_failed         = annotation_status$n_failed %||% 0L
  )

  list(
    matched             = matched,
    unmatched           = unmatched,
    cv                  = cv,
    n_quantified        = n_quantified,
    depth_summary       = depth_summary,
    coverage            = coverage,
    coverage_by_condition = coverage_by_condition,
    n_peptides_by_condition = n_peptides_by_condition,
    peptide_metrics     = peptide_metrics,
    length_by_condition = length_by_condition,
    annotation_features = annotation_features,
    feat_raw            = feat_df,
    unannotated         = unannotated,
    qc                  = qc
  )
}

# Run the full compute pipeline for ALL checked datasets (the public entry the
# observer calls under withProgress). PURE-ish: NO Shiny, NO network. Each
# dataset supplies its OWN uploaded FASTA + annotation file, resolved by the
# dataset name (see resolve_fasta/resolve_feat), MEMOIZED per dataset.
#
# @param gcts           named list of PROCESSED GCTs (or frames), keyed by ds.
# @param gcts_original  named list of GCTs (or frames) Protigy stored as
#                       `GCTs_original`, keyed by ds (the CV source). These are
#                       LOG-TRANSFORMED (post perform_log_transformation), so the
#                       CV path DELINEARIZES each by `log_base_by_ds[[ds]]`
#                       (pelsa_delinearize) before CV. May be
#                       NULL / missing a ds (CV skipped).
# @param setup_snapshot pelsa_setup_snapshot() list (datasets + per-ds
#                       condition_col + per-ds uploads).
# @param fasta_map      LEGACY single-map fallback: a named list accession ->
#                       sequence used for EVERY dataset when resolve_fasta is NULL
#                       (a single-map run / the existing tests). Ignored when
#                       resolve_fasta is supplied.
# @param feat_df        LEGACY single-map fallback feature cache data.frame,
#                       used when resolve_feat is NULL. Ignored when resolve_feat
#                       is supplied.
# @param resolve_fasta  NULL or function(ds) -> fasta map for that dataset.
#                       When given, the FASTA is resolved PER DATASET (memoized
#                       per ds). The observer wraps the uploaded-file read; tests
#                       inject a map lookup.
# @param resolve_feat   NULL or function(ds) -> feature-cache data.frame for that
#                       dataset (same per-dataset memoization as resolve_fasta).
# @param min_nonNA      min non-NA replicates for a finite CV.
# @param log_base_by_ds named list/character keyed by ds giving each dataset's
#                       declared log transformation ("None"/NA/"log2"/"log10").
#                       Sourced from GCTs_and_params()$parameters[[ds]]$
#                       log_transformation. A ds absent here defaults to "None"
#                       (treated as already-linear). ONLY the CV input is
#                       delinearized; depth + intensity-line stay on the
#                       processed log2 matrix.
# @param set_progress   NULL or function(value, detail) advancing an overall
#                       0..1 progress bar; each dataset occupies an equal slice.
# @return named-by-dataset list of per-dataset cache objects (see the Cache
#         contract on pelsa_run_analysis_one), one entry per REQUESTED dataset in
#         request order. Datasets that error -- OR that were requested but are
#         absent from `gcts` -- are captured as list(error = <message>, stage =
#         <last stage label or NA>) so one failure never aborts the rest; test
#         entries with pelsa_analysis_failed(). Only a genuinely empty request
#         (no datasets) stops.
# @noRd
pelsa_run_analysis <- function(gcts,
                               gcts_original,
                               setup_snapshot,
                               fasta_map = NULL,
                               feat_df = NULL,
                               resolve_fasta = NULL,
                               resolve_feat = NULL,
                               min_nonNA = 3L,
                               log_base_by_ds = NULL,
                               set_progress = NULL) {
  datasets <- setup_snapshot$datasets %||% character(0)
  datasets <- as.character(datasets)
  datasets <- datasets[!is.na(datasets) & nzchar(datasets)]
  # Keep ALL requested datasets (do NOT silently drop ones absent from `gcts`).
  # An absent dataset is surfaced as a structured failure entry below, so the
  # Summary/Volcano sections can label it as failed rather than looking up a
  # NULL cache with no explanation (the caller advertises every requested
  # dataset to the switcher). `present` marks which can actually be analyzed.
  present <- datasets %in% names(gcts)

  if (length(datasets) == 0L) {
    stop("pelsa_run_analysis: no checked datasets to analyze.", call. = FALSE)
  }

  # PER-DATASET resolution. Each dataset supplies its OWN uploaded FASTA +
  # annotation file, resolved by the dataset name via the caller's
  # resolve_fasta(ds)/resolve_feat(ds) closures (the observer reads the uploaded
  # temp paths; tests inject maps). Results are MEMOIZED per ds. When no resolvers
  # are given, fall back to a single shared fasta_map/feat_df (the legacy
  # single-map path the existing tests + a single-dataset run use).
  fasta_cache <- new.env(parent = emptyenv())
  feat_cache  <- new.env(parent = emptyenv())
  resolve_one <- function(cache, resolver, shared, ds) {
    if (is.null(resolver)) return(shared)
    key <- as.character(ds)
    if (is.null(cache[[key]])) cache[[key]] <- list(value = resolver(ds))
    cache[[key]]$value
  }

  condition_cols <- setup_snapshot$condition_col %||% list()
  n <- length(datasets)
  out <- vector("list", n)
  names(out) <- datasets

  for (k in seq_along(datasets)) {
    ds <- datasets[[k]]

    # A requested dataset with no GCT in `gcts` cannot be analyzed; record a
    # structured failure entry (same shape as a compute failure) instead of
    # dropping it, so the Summary surfaces the gap.
    if (!present[[k]]) {
      out[[ds]] <- list(
        error = sprintf("dataset '%s' not found in processed GCTs", ds),
        stage = NA_character_
      )
      if (!is.null(set_progress)) set_progress(k / n, NULL)
      next
    }

    base_frac <- (k - 1L) / n
    sub_progress <- if (is.null(set_progress)) NULL else function(detail) {
      set_progress(base_frac, sprintf("(%d/%d) %s - %s", k, n, ds, detail))
    }

    # Track the last stage reached so a failure reports WHICH phase threw
    # (e.g. "dataset X failed during Computing CV"). The env is updated by
    # .step() inside pelsa_run_analysis_one.
    stage_env <- new.env(parent = emptyenv())
    stage_env$stage <- NA_character_

    ds_log_base <- if (is.null(log_base_by_ds)) NA_character_
                   else log_base_by_ds[[ds]] %||% NA_character_

    # Per-dataset uploads: the FASTA + annotation file are resolved by the
    # dataset name itself (resolve_fasta(ds)/resolve_feat(ds)), memoized per ds.
    # When no resolvers are given, fall back to the shared fasta_map/feat_df (the
    # legacy single-map path the older tests use).
    ds_fasta    <- resolve_one(fasta_cache, resolve_fasta, fasta_map, ds)
    ds_feat     <- resolve_one(feat_cache,  resolve_feat,  feat_df,   ds)

    out[[ds]] <- tryCatch(
      pelsa_run_analysis_one(
        gct          = gcts[[ds]],
        gct_original = if (is.list(gcts_original)) gcts_original[[ds]] else NULL,
        fasta_map    = ds_fasta,
        feat_df      = ds_feat,
        condition_col = condition_cols[[ds]],
        min_nonNA    = min_nonNA,
        log_base     = ds_log_base,
        progress     = sub_progress,
        stage_env    = stage_env
      ),
      error = function(e) list(error = conditionMessage(e),
                               stage = stage_env$stage)
    )

    if (!is.null(set_progress)) set_progress(k / n, NULL)
  }

  out
}
