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

# Build one ;-joined multi-label string for a single peptide/dot.
#
# Given the per-mapping (gene, position, accession) vectors for ONE peptide
# (all the same length; these are the distinct accession x occurrence mappings
# in PG.ProteinAccessions token order), produce a single label string:
#   - each entry is "<gene>_aa<pos>", falling back to "<accession>_aa<pos>"
#     when the gene is empty/NA;
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
# @param genes       character vector of gene symbols (may contain "" or NA)
# @param positions   integer or character vector of residue positions
# @param accessions  character vector of protein accessions (gene fallback)
# @return character scalar label for the peptide
# @noRd
pelsa_build_multilabel <- function(genes, positions, accessions) {
  if (length(genes) == 0L) return(NA_character_)

  # Fail fast on length mismatch: a silent scalar recycle would emit a
  # plausible-but-wrong volcano label, which is worse than a loud failure.
  stopifnot(
    length(genes) == length(positions),
    length(genes) == length(accessions)
  )

  # Gene -> accession fallback when the gene is missing/empty.
  genes <- as.character(genes)
  accessions <- as.character(accessions)
  empty_gene <- is.na(genes) | !nzchar(trimws(genes))
  label_id <- ifelse(empty_gene, accessions, genes)

  entries <- paste0(label_id, "_aa", as.character(positions))

  # Collapse fully-identical entries, preserving first-occurrence order.
  entries <- entries[!duplicated(entries)]
  paste(entries, collapse = ";")
}
