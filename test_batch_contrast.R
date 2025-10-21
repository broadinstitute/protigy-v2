# Test script to verify batch contrast processing produces same results
# This script compares the old (per-contrast) approach with the new (batch) approach

library(limma)
library(cmapR)

# Load test data
data(brca_retrospective_v5.0_proteome_gct)

# Create test GCT with sufficient samples
test_gct <- brca_retrospective_v5.0_proteome_gct

# Use first 100 genes and first 30 samples for faster testing
test_mat <- test_gct@mat[1:100, 1:30]
test_cdesc <- test_gct@cdesc[1:30, ]
test_rdesc <- test_gct@rdesc[1:100, ]

# Create test groups (3 groups for multiple contrasts)
test_cdesc$test_group <- rep(c("A", "B", "C"), c(10, 10, 10))

# Create test GCT
test_gct_small <- new("GCT",
  mat = test_mat,
  cdesc = test_cdesc,
  rdesc = test_rdesc,
  rid = rownames(test_mat),
  cid = colnames(test_mat)
)

gct_list <- list(proteome = test_gct_small)

# Define contrasts to test
contrasts_list <- list(
  c("A", "B"),
  c("A", "C"),
  c("B", "C")
)

cat("Testing batch contrast processing...\n")
cat("Number of genes:", nrow(test_mat), "\n")
cat("Number of samples:", ncol(test_mat), "\n")
cat("Number of contrasts:", length(contrasts_list), "\n\n")

#############################################################################
# OLD APPROACH - Loop through contrasts (original code)
#############################################################################
cat("Running OLD approach (per-contrast loops)...\n")
start_time_old <- Sys.time()

old_results <- list()
annotation_col <- "test_group"
ome_name <- "proteome"
intensity <- FALSE
use.adj.pvalue <- TRUE
p.value.alpha <- 0.05

ome_data <- gct_list[[ome_name]]@mat
rdesc <- gct_list[[ome_name]]@rdesc
cdesc <- gct_list[[ome_name]]@cdesc

for (contrast_pair in contrasts_list) {
  group1 <- contrast_pair[1]
  group2 <- contrast_pair[2]
  contrast_name <- paste0(group1, "_over_", group2)

  # Filter samples
  sample_names <- colnames(ome_data)
  all_groups <- cdesc[sample_names, annotation_col, drop = TRUE]
  keep_samples_logical <- all_groups %in% c(group1, group2)
  samples_to_keep <- sample_names[keep_samples_logical]
  groups <- all_groups[match(samples_to_keep, sample_names)]

  # Prepare data
  data <- ome_data[, samples_to_keep]
  groups <- factor(groups, levels = c(group2, group1))

  # OLD statistical approach
  design.mat <- cbind(ref = 1, comparison = as.numeric(groups))
  data.matrix <- data.frame(data, stringsAsFactors = FALSE)

  m <- lmFit(data.matrix, design.mat)
  m <- eBayes(m, robust = TRUE)
  sig <- topTable(m, coef = colnames(design.mat)[2], number = nrow(data), sort.by = 'none')

  sig$significant <- if (use.adj.pvalue) {
    sig$adj.P.Val <= p.value.alpha
  } else {
    sig$P.Value <= p.value.alpha
  }

  old_results[[contrast_name]] <- sig
}

end_time_old <- Sys.time()
time_old <- difftime(end_time_old, start_time_old, units = "secs")
cat("OLD approach completed in", round(as.numeric(time_old), 3), "seconds\n\n")

#############################################################################
# NEW APPROACH - Batch contrast processing
#############################################################################
cat("Running NEW approach (batch contrast processing)...\n")
start_time_new <- Sys.time()

new_results <- list()

# Extract all unique groups involved in any contrast
all_contrast_groups <- unique(unlist(contrasts_list))

# Filter samples to include only those belonging to groups in any contrast
sample_names <- colnames(ome_data)
all_groups <- cdesc[sample_names, annotation_col, drop = TRUE]
keep_samples_logical <- all_groups %in% all_contrast_groups
samples_to_keep <- sample_names[keep_samples_logical]

groups <- all_groups[match(samples_to_keep, sample_names)]
groups <- factor(groups, levels = all_contrast_groups)

# Prepare data
data <- ome_data[, samples_to_keep]
data.matrix <- data.frame(data, stringsAsFactors = FALSE)

# Create design matrix with all groups (no intercept)
design <- model.matrix(~ 0 + groups)
colnames(design) <- levels(groups)

# Build contrast matrix dynamically from contrasts_list
contrast_strings <- c()
contrast_names_vec <- c()
for (contrast_pair in contrasts_list) {
  group1 <- contrast_pair[1]
  group2 <- contrast_pair[2]
  # For contrast "A / B", user expects fold change = A - B
  contrast_strings <- c(contrast_strings, paste0("groups", group1, " - groups", group2))
  contrast_names_vec <- c(contrast_names_vec, paste0(group1, "_over_", group2))
}

# Create contrast matrix using makeContrasts
contrast_matrix <- eval(parse(text = paste0(
  "limma::makeContrasts(",
  paste(paste0(contrast_names_vec, " = ", contrast_strings), collapse = ", "),
  ", levels = design)"
)))

# Fit model once for all groups
fit <- lmFit(data.matrix, design)

# Fit all contrasts at once
fit2 <- contrasts.fit(fit, contrast_matrix)

# Apply eBayes once for all contrasts
fit2 <- eBayes(fit2, robust = TRUE)

# Extract results for each contrast
for (i in seq_along(contrast_names_vec)) {
  contrast_name <- contrast_names_vec[i]

  sig <- topTable(
    fit2,
    coef = i,
    number = nrow(data),
    sort.by = 'none'
  )

  sig$significant <- if (use.adj.pvalue) {
    sig$adj.P.Val <= p.value.alpha
  } else {
    sig$P.Value <= p.value.alpha
  }

  new_results[[contrast_name]] <- sig
}

end_time_new <- Sys.time()
time_new <- difftime(end_time_new, start_time_new, units = "secs")
cat("NEW approach completed in", round(as.numeric(time_new), 3), "seconds\n\n")

#############################################################################
# Compare results
#############################################################################
cat("========== PERFORMANCE COMPARISON ==========\n")
cat("OLD approach time:", round(as.numeric(time_old), 3), "seconds\n")
cat("NEW approach time:", round(as.numeric(time_new), 3), "seconds\n")
speedup <- as.numeric(time_old) / as.numeric(time_new)
cat("Speedup:", round(speedup, 2), "x\n\n")

cat("========== RESULTS COMPARISON ==========\n")
all_match <- TRUE

for (contrast_name in names(old_results)) {
  cat("Comparing contrast:", contrast_name, "\n")

  old_res <- old_results[[contrast_name]]
  new_res <- new_results[[contrast_name]]

  # Compare key columns
  logFC_match <- all.equal(old_res$logFC, new_res$logFC, tolerance = 1e-6)
  pval_match <- all.equal(old_res$P.Value, new_res$P.Value, tolerance = 1e-6)
  adjpval_match <- all.equal(old_res$adj.P.Val, new_res$adj.P.Val, tolerance = 1e-6)
  sig_match <- all.equal(old_res$significant, new_res$significant)

  cat("  logFC match:", isTRUE(logFC_match), "\n")
  cat("  P.Value match:", isTRUE(pval_match), "\n")
  cat("  adj.P.Val match:", isTRUE(adjpval_match), "\n")
  cat("  significant match:", isTRUE(sig_match), "\n")

  if (!isTRUE(logFC_match) || !isTRUE(pval_match) || !isTRUE(adjpval_match) || !isTRUE(sig_match)) {
    all_match <- FALSE
    cat("  WARNING: Results do not match!\n")
    if (!isTRUE(logFC_match)) cat("    logFC:", logFC_match, "\n")
    if (!isTRUE(pval_match)) cat("    P.Value:", pval_match, "\n")
    if (!isTRUE(adjpval_match)) cat("    adj.P.Val:", adjpval_match, "\n")
  }
  cat("\n")
}

if (all_match) {
  cat("SUCCESS: All results match between old and new approaches!\n")
  cat("The batch contrast processing optimization is working correctly.\n")
} else {
  cat("ERROR: Some results do not match!\n")
  cat("Please investigate the differences.\n")
}
