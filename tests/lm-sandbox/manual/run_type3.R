################################################################################
# tests/lm-sandbox/manual/run_type3.R
#
# Upstream Type 3: contrasts / no blocking.
# Cell-means design `~ 0 + condition`, two contrasts: B-A and C-A.
################################################################################

suppressPackageStartupMessages({
  library(limma)
  library(statmod)
})

OUT <- "tests/lm-sandbox/golden/type3_contrasts.rds"

fx <- readRDS("tests/lm-sandbox/data/type3_contrasts.rds")
mat <- fx$mat
cdesc <- fx$cdesc
cdesc$condition <- factor(cdesc$condition, levels = c("A", "B", "C", "D"))

design <- model.matrix(~ 0 + condition, data = cdesc)
colnames(design) <- levels(cdesc$condition)

fit <- lmFit(mat, design)
cont.matrix <- makeContrasts("B - A", "C - A", levels = design)
colnames(cont.matrix) <- c("B_vs_A", "C_vs_A")
fitc <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fitc, robust = TRUE, trend = FALSE)

contrast_names <- colnames(fit2$coefficients)
per_coef <- lapply(contrast_names, function(cn) {
  tt <- topTable(fit2, coef = cn, number = Inf, sort.by = "none", adjust.method = "BH")
  tt[, c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B")]
})
names(per_coef) <- contrast_names

# In a contrast-only model, "condition" the factor has 4 levels -> the joint
# F across all contrasts is informative as a global signal.
factor_F <- list(condition = topTable(fit2, coef = contrast_names, number = Inf,
                                      sort.by = "none", adjust.method = "BH"))

out <- list(
  per_coef = per_coef,
  factor_F = factor_F,
  design_cols = colnames(design),
  contrast_cols = colnames(cont.matrix),
  limma_version = as.character(packageVersion("limma"))
)
saveRDS(out, OUT)
cat("wrote", OUT, "\n")
cat("  contrasts:", paste(contrast_names, collapse = ", "), "\n")
