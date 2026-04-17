################################################################################
# Generate a synthetic test GCT for manual QA of the Linear Model module.
#
# Experimental design: 50 proteins × 40 samples
#   - treatment  : factor, 2 levels ("Vehicle", "Drug")  -- main effect
#   - timepoint  : factor, 2 levels ("T1", "T2")          -- enables interaction
#   - subject_id : factor, 10 subjects × 4 observations   -- blocking (repeated measures)
#   - batch      : factor, 2 batches                       -- covariate
#
# True signal: proteins PROT_001-PROT_010 respond to Drug×T2 interaction.
# All other proteins are pure noise.
#
# Usage:
#   source("inst/extdata/linear-model-test/generate_test_gct.R")
#   g <- cmapR::parse_gctx("inst/extdata/linear-model-test/lm_test_data.gct")
################################################################################

set.seed(42)

n_feat   <- 50
n_subj   <- 10
n_obs    <- 4   # each subject observed 4 times (2 treatments × 2 timepoints)
n_samp   <- n_subj * n_obs  # 40 total samples

# ── Sample metadata ──────────────────────────────────────────────────────────
# Fully balanced: subject 1-10 each appears in all 4 treatment×timepoint combos
treatment  <- rep(c("Vehicle", "Drug"),    times = n_samp / 2)
timepoint  <- rep(c("T1", "T2", "T1", "T2"), times = n_subj)
subject_id <- rep(paste0("S", sprintf("%02d", seq_len(n_subj))), each = n_obs)
batch      <- rep(c("Batch1", "Batch2"), times = n_samp / 2)

sample_ids <- paste0("samp_", sprintf("%02d", seq_len(n_samp)))

cdesc <- data.frame(
  id         = sample_ids,
  treatment  = factor(treatment,  levels = c("Vehicle", "Drug")),
  timepoint  = factor(timepoint,  levels = c("T1", "T2")),
  subject_id = factor(subject_id),
  batch      = factor(batch),
  row.names  = sample_ids,
  stringsAsFactors = FALSE
)

# ── Feature metadata ──────────────────────────────────────────────────────────
feat_ids   <- paste0("PROT_", sprintf("%03d", seq_len(n_feat)))
gene_syms  <- paste0("GENE_", sprintf("%03d", seq_len(n_feat)))

rdesc <- data.frame(
  id          = feat_ids,
  geneSymbol  = gene_syms,
  description = paste0("Synthetic protein ", seq_len(n_feat)),
  row.names   = feat_ids,
  stringsAsFactors = FALSE
)

# ── Expression matrix ─────────────────────────────────────────────────────────
mat <- matrix(rnorm(n_feat * n_samp, mean = 10, sd = 1),
              nrow = n_feat,
              dimnames = list(feat_ids, sample_ids))

# Add true signal: Drug × T2 interaction on first 10 proteins
is_drug_t2 <- cdesc$treatment == "Drug" & cdesc$timepoint == "T2"
for (i in seq_len(10)) {
  effect <- 1.5 + rnorm(1, sd = 0.2)   # ~1.5 log2 fold-change per protein
  mat[i, is_drug_t2] <- mat[i, is_drug_t2] + effect
}

# Add subject random effect (blocks)
for (subj in levels(cdesc$subject_id)) {
  subj_effect <- rnorm(1, sd = 0.4)
  subj_cols   <- cdesc$subject_id == subj
  mat[, subj_cols] <- mat[, subj_cols] + subj_effect
}

# Add batch effect
batch1_cols <- cdesc$batch == "Batch1"
mat[, batch1_cols] <- mat[, batch1_cols] + rnorm(1, sd = 0.3)

# ── Write GCT ─────────────────────────────────────────────────────────────────
gct <- methods::new(
  "GCT",
  mat   = mat,
  rdesc = rdesc,
  cdesc = cdesc,
  rid   = feat_ids,
  cid   = sample_ids
)

out_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "lm_test_data.gct")
# Fallback for interactive use
if (!nzchar(out_path) || !file.exists(dirname(out_path))) {
  out_path <- "inst/extdata/linear-model-test/lm_test_data.gct"
}
cmapR::write_gct(gct, ofile = out_path, appenddim = FALSE)
message("Written: ", out_path)
