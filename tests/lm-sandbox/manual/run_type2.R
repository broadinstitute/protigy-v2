################################################################################
# tests/lm-sandbox/manual/run_type2.R
#
# Upstream Type 2: repeated measures only (no groups).
# Design: ~ time, blocked on subject. (The reviewer §3.5 calls out that
# protigy-v2's current `~ 1` design is NOT the upstream Type 2 — to mirror
# upstream Type 2 in protigy-v2 the user selects `time` as fixed effect AND
# blocks on subject. We bake that mapping into the golden here.)
################################################################################

suppressPackageStartupMessages({
  library(limma)
  library(statmod)
})

OUT <- "tests/lm-sandbox/golden/type2_rm_only.rds"

fx <- readRDS("tests/lm-sandbox/data/type2_rm_only.rds")
mat <- fx$mat
cdesc <- fx$cdesc
cdesc$time <- factor(cdesc$time, levels = c("T1", "T2", "T3"))

design <- model.matrix(~ time, data = cdesc)
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

# time has >= 2 coefs -> per-factor F.
factor_F <- list(time = topTable(fit2, coef = non_intercept_coefs, number = Inf,
                                 sort.by = "none", adjust.method = "BH"))

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
cat("  factor-F tables:", length(factor_F), "\n")
cat("  consensus.correlation:", out$consensus, "\n")
