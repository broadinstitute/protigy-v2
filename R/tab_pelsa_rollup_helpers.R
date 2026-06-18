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
    peptide_seq = as.character(exploded_stat_df[[pep_col]]),
    accession   = as.character(exploded_stat_df[[acc_col]]),
    gene        = as.character(exploded_stat_df[[gene_col]]),
    pep_start   = exploded_stat_df[[pos_col]],
    adj_p       = as.numeric(exploded_stat_df[[adjp_col]]),
    logFC       = as.numeric(exploded_stat_df[[logfc_col]])
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
    # Within this by="peptide_seq" group the gene/pep_start/accession vectors
    # preserve the sorted `winners` row order (data.table by-groups keep the
    # parent table's order), so the label / won_accessions entry order is
    # deterministic under any input row permutation.
    list(
      # adj_p[1L] / logFC[1L] rely on the per-peptide-single-coordinate
      # invariant: a peptide has ONE (adj_p, logFC), so any winner row for it
      # carries that same coordinate -- the first row is representative.
      adj_p          = adj_p[1L],
      logFC          = logFC[1L],
      label          = pelsa_build_multilabel(gene, pep_start, accession,
                                               is_self_curated),
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
