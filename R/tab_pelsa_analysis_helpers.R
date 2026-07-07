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
    # UniProt: sp|P12345|... or tr|A0A...|... -> take the 2nd pipe field (works
    # for both the standard 3-field header and a malformed single-pipe header,
    # e.g. "foo|Q99999", where there is no 3rd field to anchor on); else the
    # token itself when there is no pipe at all.
    pipe_fields <- strsplit(first_tok, "\\|", fixed = FALSE)
    pipe_acc <- vapply(pipe_fields, function(f) {
      if (length(f) >= 2L) f[[2L]] else NA_character_
    }, character(1))
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
    pipe_fields <- strsplit(first_tok, "\\|", fixed = FALSE)
    pipe_acc <- vapply(pipe_fields, function(f) {
      if (length(f) >= 2L) f[[2L]] else NA_character_
    }, character(1))
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
