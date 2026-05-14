################################################################################
# tests/lm-sandbox/manual/run_intensity_trend.R
#
# Intensity-style data with a mean-variance trend. Exercises eBayes(trend=TRUE).
# Design: ~ condition (no blocking, simple two-group).
# We save BOTH trend=TRUE and trend=FALSE outputs so the regression suite can
# verify (a) the implementation chooses trend=TRUE when intensity=TRUE, and
# (b) the two differ in a measurable way (proving the test is non-vacuous).
################################################################################

suppressPackageStartupMessages({
  library(limma)
  library(statmod)
})

OUT <- "tests/lm-sandbox/golden/intensity_trend.rds"

fx <- readRDS("tests/lm-sandbox/data/intensity_trend.rds")
mat <- fx$mat
cdesc <- fx$cdesc
cdesc$condition <- factor(cdesc$condition, levels = c("Ctrl", "Trt"))

design <- model.matrix(~ condition, data = cdesc)
fit <- lmFit(mat, design)
fit_trend  <- eBayes(fit, robust = TRUE, trend = TRUE)
fit_notrend <- eBayes(fit, robust = TRUE, trend = FALSE)

non_intercept <- setdiff(colnames(fit_trend$coefficients), "(Intercept)")
per_coef_trend <- lapply(non_intercept, function(cn) {
  tt <- topTable(fit_trend, coef = cn, number = Inf, sort.by = "none", adjust.method = "BH")
  tt[, c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B")]
})
names(per_coef_trend) <- non_intercept

per_coef_notrend <- lapply(non_intercept, function(cn) {
  tt <- topTable(fit_notrend, coef = cn, number = Inf, sort.by = "none", adjust.method = "BH")
  tt[, c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B")]
})
names(per_coef_notrend) <- non_intercept

# Sanity check that they actually differ (so the test for trend=TRUE proves
# something).
p_diff <- max(abs(per_coef_trend[[1]]$P.Value - per_coef_notrend[[1]]$P.Value))
cat("max |P.Value| diff trend vs no-trend:", p_diff, "\n")
if (p_diff < 1e-6) {
  stop("trend=TRUE and trend=FALSE produced identical outputs; fixture insufficient.")
}

out <- list(
  per_coef_trend = per_coef_trend,
  per_coef_notrend = per_coef_notrend,
  design_cols = colnames(design),
  max_pval_diff = p_diff,
  limma_version = as.character(packageVersion("limma"))
)
saveRDS(out, OUT)
cat("wrote", OUT, "\n")
