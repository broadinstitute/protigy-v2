# Reusable direct-call fixture for the PELSA export bodies (pelsa_section3_export_*).
# Builds the MINIMAL, self-consistent inputs those functions require, with a
# controllable protein count so cap tests can exceed .PELSA_EXPORT_FIGURE_CAP.
# Each protein gets ONE peptide (1:1 accession:peptide) -> figure count == protein
# count, which is what the cap tests key on. Deterministic (no RNG).
#
# NOTE: `matched$pep_occurrence_idx` (all 1L here, since each accession has
# exactly one peptide occurrence) is REQUIRED by pelsa_intensity_line_data()'s
# boundary check (matched_required <- c("accession", "pep_start",
# "pep_occurrence_idx"), tab_pelsa_intensity_line_helpers.R:318) but was not
# listed in the original brief's column contract. Without it,
# pelsa_section3_export_intensity() silently emits ZERO PNGs per protein (the
# per-protein tryCatch in the export loop swallows the stop() and just skips
# to the next accession) -- confirmed via direct debugging against the
# pre-refactor export body.
.make_pelsa_export_fixture <- function(n_proteins = 8L, n_markers = 1L,
                                       frac_sig = 1.0) {
  ome <- "proteome"
  accs <- sprintf("ACC%03d", seq_len(n_proteins))
  peps <- sprintf("PEPTIDESEQ%03d", seq_len(n_proteins))
  genes <- sprintf("GENE%03d", seq_len(n_proteins))
  n_sig <- floor(frac_sig * n_proteins)
  adjp <- c(rep(0.001, n_sig), rep(0.5, n_proteins - n_sig))  # sig first
  samples <- c("s1", "s2", "s3", "s4")
  conds   <- c("A", "A", "B", "B")

  matched <- data.frame(
    accession = accs, .row_id = seq_len(n_proteins),
    PEP.StrippedSequence = peps, gene = genes,
    pep_start = seq_len(n_proteins), pep_end = seq_len(n_proteins) + 9L,
    pep_occurrence_idx = rep(1L, n_proteins),
    stringsAsFactors = FALSE)

  stat <- data.frame(
    .row_id = seq_len(n_proteins), PEP.StrippedSequence = peps,
    check.names = FALSE, stringsAsFactors = FALSE)
  stat[["logFC.A_over_B"]]     <- seq(-2, 2, length.out = n_proteins)
  stat[["adj.P.Val.A_over_B"]] <- adjp

  pm <- matrix(as.numeric(seq_len(n_proteins * 4L)),
               nrow = n_proteins, ncol = 4L,
               dimnames = list(peps, samples))

  coverage <- data.frame(accession = accs,
                         protein_length = rep(100L, n_proteins),
                         coverage = rep(0.5, n_proteins),
                         stringsAsFactors = FALSE)

  list(
    ome = ome,
    stat_results = setNames(list(stat), ome),
    cache_entry = list(matched = matched, coverage = coverage),
    feat_df = data.frame(accession = character(0), feature_type = character(0),
                         stringsAsFactors = FALSE),
    processed_mat = pm,
    condition_map = setNames(conds, samples),
    condition_order = c("A", "B"),
    marker_accessions = utils::head(accs, n_markers),
    contrast_choices = c("A vs B" = "A_over_B"))
}
