################################################################################
# tests/lm-sandbox/manual/run_type1.R
#
# Run limma directly on the type1_rm_with_groups fixture and persist the
# canonical output frame as the golden RDS.
#
# Design: ~ group + time + group:time, blocked on subject via duplicateCorrelation.
# Reference levels: group=WT (the alphabetical first level after refactor), time=T1.
# We use protigy-v2-style configuration:
#   - intercept included
#   - robust = TRUE
#   - trend = FALSE (not intensity data)
#
# Outputs saved to tests/lm-sandbox/golden/type1_rm_with_groups.rds as a list:
#   - per_coef    : named list of topTable() frames (one per non-intercept coef)
#   - factor_F    : named list of multi-coef F-test frames (one per multi-level factor /
#                   interaction-bearing variable). Used by Phase 1.2 per-factor F-tests.
#   - consensus   : duplicateCorrelation$consensus.correlation
#   - design_cols : design matrix column names
################################################################################

suppressPackageStartupMessages({
  library(limma)
  library(statmod)
})

OUT <- "tests/lm-sandbox/golden/type1_rm_with_groups.rds"

fx <- readRDS("tests/lm-sandbox/data/type1_rm_with_groups.rds")
mat <- fx$mat
cdesc <- fx$cdesc

# Order factors with WT as reference (matches what we expect protigy-v2 to do
# after the Phase 1 reference-level picker; here we make it explicit so the
# golden is unambiguous).
cdesc$group <- factor(cdesc$group, levels = c("WT", "MUT"))
cdesc$time <- factor(cdesc$time, levels = c("T1", "T2", "T3"))

design <- model.matrix(~ group + time + group:time, data = cdesc)
stopifnot(qr(design)$rank == ncol(design))

block <- factor(cdesc$subject)
dupcor <- duplicateCorrelation(mat, design, block = block)
fit <- lmFit(mat, design, block = block, correlation = dupcor$consensus.correlation)
fit2 <- eBayes(fit, robust = TRUE, trend = FALSE)

non_intercept_coefs <- setdiff(colnames(fit2$coefficients), "(Intercept)")

per_coef <- lapply(non_intercept_coefs, function(cn) {
  tt <- topTable(fit2, coef = cn, number = Inf, sort.by = "none", adjust.method = "BH")
  tt[, c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B")]
})
names(per_coef) <- non_intercept_coefs

# Per-factor F-tests (Phase 1.2 contract): emit one F-test per variable that
# has more than one non-intercept coefficient.
group_coefs <- grep("^group", colnames(fit2$coefficients), value = TRUE)
group_coefs <- group_coefs[!grepl(":", group_coefs)]  # exclude interaction
time_coefs <- grep("^time", colnames(fit2$coefficients), value = TRUE)
time_coefs <- time_coefs[!grepl(":", time_coefs)]
interaction_coefs <- grep(":", colnames(fit2$coefficients), value = TRUE)

factor_F <- list()
# group has 1 coef (2 levels) — would NOT emit an F under option (c).
# time has 2 coefs — emit F.
if (length(time_coefs) >= 2) {
  factor_F$time <- topTable(fit2, coef = time_coefs, number = Inf,
                            sort.by = "none", adjust.method = "BH")
}
# Interaction has 2 coefs — emit F.
if (length(interaction_coefs) >= 2) {
  factor_F$`group:time` <- topTable(fit2, coef = interaction_coefs, number = Inf,
                                    sort.by = "none", adjust.method = "BH")
}

out <- list(
  per_coef = per_coef,
  factor_F = factor_F,
  consensus = dupcor$consensus.correlation,
  design_cols = colnames(design),
  limma_version = as.character(packageVersion("limma"))
)
saveRDS(out, OUT)
cat("wrote", OUT, "\n")
cat("  per-coef tables:", length(per_coef), "\n")
cat("  factor-F tables:", length(factor_F), "names:",
    paste(names(factor_F), collapse = ", "), "\n")
cat("  consensus.correlation:", out$consensus, "\n")
