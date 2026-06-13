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
# @return character vector of length sum(n_acc), aligned to exploded rows
# @noRd
.pelsa_align_tokens <- function(token_lists, n_acc, row_idx) {
  total <- length(row_idx)
  if (total == 0L) return(character(0))

  # Per-original-row token counts and the flattened, trimmed token vector.
  n_tok <- lengths(token_lists)
  flat_tok <- trimws(unlist(token_lists, use.names = FALSE))
  if (is.null(flat_tok)) flat_tok <- character(0)
  # Start offset (0-based) of each row's tokens within flat_tok.
  tok_offset <- cumsum(n_tok) - n_tok

  # Within-row accession index k = 1,2,..,n1,1,2,..,n2,... for the exploded rows.
  k <- sequence(n_acc)
  # Per-exploded-row view of the original row's token count.
  n_tok_row <- n_tok[row_idx]

  # Choose which within-row token each exploded row takes:
  #   n_tok == 1            -> token 1 (recycle)
  #   k <= n_tok            -> token k (1:1 / index-pad)
  #   otherwise             -> NA (missing tail)
  chosen <- ifelse(n_tok_row == 1L, 1L, ifelse(k <= n_tok_row, k, NA_integer_))

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
#         gene, pep_position_token, plus all other original columns
# @noRd
pelsa_explode_accessions <- function(df,
                                     acc_col  = "PG.ProteinAccessions",
                                     gene_col = "PG.Genes",
                                     pos_col  = "PEP.PeptidePosition",
                                     id_col   = NULL) {
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
    out$pep_position_token <- character(0)
    return(out)
  }

  # Split every row's accessions ONCE, then trim/drop empties on the FLATTENED
  # vector (one trimws() call total -- per-row trimws() is the 54x perf trap).
  acc_split <- strsplit(as.character(df[[acc_col]]), ";", fixed = TRUE)
  n_acc_raw <- lengths(acc_split)
  flat_acc <- trimws(unlist(acc_split, use.names = FALSE))
  if (is.null(flat_acc)) flat_acc <- character(0)
  # Map each flattened token back to its original row, then keep only non-empty
  # tokens. Per-row kept counts via tabulate() (vectorized, no per-row loop).
  flat_row <- rep.int(seq_len(n_row), n_acc_raw)
  # Drop empty AND NA accession tokens (strsplit(NA) -> NA, and nzchar(NA) is
  # TRUE, so NA must be excluded explicitly). A row whose accessions are all
  # empty/NA contributes zero exploded rows.
  keep <- !is.na(flat_acc) & nzchar(flat_acc)
  accession <- flat_acc[keep]
  n_acc <- tabulate(flat_row[keep], nbins = n_row)

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

  out$accession <- accession
  out$gene <- .pelsa_align_tokens(gene_lists, n_acc, row_idx)
  out$pep_position_token <- .pelsa_align_tokens(pos_lists, n_acc, row_idx)

  # Drop the original ;-delimited acc/gene/pos columns? No — carry originals
  # through unchanged per contract; the new columns are additive.
  out
}
