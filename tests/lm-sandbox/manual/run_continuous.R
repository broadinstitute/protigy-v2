################################################################################
# tests/lm-sandbox/manual/run_continuous.R
#
# Continuous covariate (age).
# Design: ~ age. No blocking.
################################################################################

suppressPackageStartupMessages({
  library(limma)
  library(statmod)
})

OUT <- "tests/lm-sandbox/golden/continuous_covariate.rds"

fx <- readRDS("tests/lm-sandbox/data/continuous_covariate.rds")
mat <- fx$mat
cdesc <- fx$cdesc

design <- model.matrix(~ age, data = cdesc)
fit <- lmFit(mat, design)
fit2 <- eBayes(fit, robust = TRUE, trend = FALSE)

non_intercept <- setdiff(colnames(fit2$coefficients), "(Intercept)")
per_coef <- lapply(non_intercept, function(cn) {
  tt <- topTable(fit2, coef = cn, number = Inf, sort.by = "none", adjust.method = "BH")
  tt[, c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B")]
})
names(per_coef) <- non_intercept

# age has 1 coef -> no F-test under option (c).
out <- list(
  per_coef = per_coef,
  factor_F = list(),
  design_cols = colnames(design),
  limma_version = as.character(packageVersion("limma"))
)
saveRDS(out, OUT)
cat("wrote", OUT, "\n")
cat("  coefs:", paste(non_intercept, collapse = ", "), "\n")
