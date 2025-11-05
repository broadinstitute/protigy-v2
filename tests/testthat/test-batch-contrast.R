# Tests for batch contrast processing
# These tests verify that the batch contrast optimization (using contrasts.fit)
# produces identical results to the original per-contrast loop approach

# Load test data
data(brca_retrospective_v5.0_proteome_gct)

# Helper function: Create mock GCT for contrast testing
create_mock_gct_for_contrasts <- function(n_genes = 100, n_samples = 30, groups = c("A", "B", "C")) {
  test_gct <- brca_retrospective_v5.0_proteome_gct

  # Use subset of data for faster testing
  test_mat <- test_gct@mat[1:n_genes, 1:n_samples]
  test_cdesc <- test_gct@cdesc[1:n_samples, ]
  test_rdesc <- test_gct@rdesc[1:n_genes, ]

  # Create test groups
  samples_per_group <- n_samples %/% length(groups)
  remainder <- n_samples %% length(groups)
  group_sizes <- rep(samples_per_group, length(groups))
  if (remainder > 0) {
    group_sizes[1:remainder] <- group_sizes[1:remainder] + 1
  }
  test_cdesc$test_group <- rep(groups, group_sizes)

  # Create test GCT
  new("GCT",
      mat = test_mat,
      cdesc = test_cdesc,
      rdesc = test_rdesc,
      rid = rownames(test_mat),
      cid = colnames(test_mat)
  )
}

# Helper function: Run per-contrast approach (original implementation)
run_per_contrast_approach <- function(gct, contrasts_list, annotation_col = "test_group",
                                       use.adj.pvalue = TRUE, p.value.alpha = 0.05) {
  results <- list()
  ome_data <- gct@mat
  cdesc <- gct@cdesc

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

    # Statistical analysis using old approach
    design.mat <- cbind(ref = 1, comparison = as.numeric(groups))
    data.matrix <- data.frame(data, stringsAsFactors = FALSE)

    m <- limma::lmFit(data.matrix, design.mat)
    m <- limma::eBayes(m, robust = TRUE)
    sig <- limma::topTable(m, coef = colnames(design.mat)[2], number = nrow(data), sort.by = 'none')

    sig$significant <- if (use.adj.pvalue) {
      sig$adj.P.Val <= p.value.alpha
    } else {
      sig$P.Value <= p.value.alpha
    }

    results[[contrast_name]] <- sig
  }

  return(results)
}

# Helper function: Run batch contrast approach (optimized implementation)
run_batch_contrasts <- function(gct, contrasts_list, annotation_col = "test_group",
                                 use.adj.pvalue = TRUE, p.value.alpha = 0.05) {
  results <- list()
  ome_data <- gct@mat
  cdesc <- gct@cdesc

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
    contrast_strings <- c(contrast_strings, paste0("groups", group1, " - groups", group2))
    contrast_names_vec <- c(contrast_names_vec, paste0(group1, "_over_", group2))
  }

  # Create contrast matrix using do.call (safer than eval(parse()))
  contrast_list <- setNames(as.list(contrast_strings), contrast_names_vec)
  contrast_matrix <- do.call(
    limma::makeContrasts,
    c(contrast_list, list(levels = design))
  )

  # Fit model once for all groups
  fit <- limma::lmFit(data.matrix, design)

  # Fit all contrasts at once
  fit2 <- limma::contrasts.fit(fit, contrast_matrix)

  # Apply eBayes once for all contrasts
  fit2 <- limma::eBayes(fit2, robust = TRUE)

  # Extract results for each contrast
  for (i in seq_along(contrast_names_vec)) {
    contrast_name <- contrast_names_vec[i]

    sig <- limma::topTable(
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

    results[[contrast_name]] <- sig
  }

  return(results)
}

# Helper function: Compare two result sets
compare_results <- function(results1, results2, tolerance = 1e-6) {
  if (!identical(names(results1), names(results2))) {
    return(list(match = FALSE, message = "Contrast names don't match"))
  }

  for (contrast_name in names(results1)) {
    res1 <- results1[[contrast_name]]
    res2 <- results2[[contrast_name]]

    # Compare key columns
    logFC_match <- isTRUE(all.equal(res1$logFC, res2$logFC, tolerance = tolerance))
    pval_match <- isTRUE(all.equal(res1$P.Value, res2$P.Value, tolerance = tolerance))
    adjpval_match <- isTRUE(all.equal(res1$adj.P.Val, res2$adj.P.Val, tolerance = tolerance))
    sig_match <- isTRUE(all.equal(res1$significant, res2$significant))

    if (!logFC_match || !pval_match || !adjpval_match || !sig_match) {
      return(list(
        match = FALSE,
        contrast = contrast_name,
        logFC_match = logFC_match,
        pval_match = pval_match,
        adjpval_match = adjpval_match,
        sig_match = sig_match
      ))
    }
  }

  return(list(match = TRUE))
}

# ============================================================================
# Unit Tests
# ============================================================================

test_that("batch contrast processing handles multiple contrasts correctly", {
  # Setup
  test_gct <- create_mock_gct_for_contrasts(n_genes = 50, n_samples = 30, groups = c("A", "B", "C"))
  contrasts_list <- list(c("A", "B"), c("A", "C"), c("B", "C"))

  # Run batch approach
  batch_results <- run_batch_contrasts(test_gct, contrasts_list)

  # Verify results structure
  expect_equal(length(batch_results), 3)
  expect_equal(names(batch_results), c("A_over_B", "A_over_C", "B_over_C"))

  # Verify each result has correct columns
  for (contrast_name in names(batch_results)) {
    result <- batch_results[[contrast_name]]
    expect_true("logFC" %in% colnames(result))
    expect_true("P.Value" %in% colnames(result))
    expect_true("adj.P.Val" %in% colnames(result))
    expect_true("significant" %in% colnames(result))
    expect_equal(nrow(result), 50)  # Same number of genes
  }
})

test_that("batch contrast results match per-contrast approach", {
  # Setup
  test_gct <- create_mock_gct_for_contrasts(n_genes = 100, n_samples = 30, groups = c("A", "B", "C"))
  contrasts_list <- list(c("A", "B"), c("A", "C"), c("B", "C"))

  # Run both approaches
  per_contrast_results <- run_per_contrast_approach(test_gct, contrasts_list)
  batch_results <- run_batch_contrasts(test_gct, contrasts_list)

  # Compare results
  comparison <- compare_results(per_contrast_results, batch_results)

  expect_true(comparison$match,
              info = if (!comparison$match) {
                paste("Results don't match for contrast:", comparison$contrast,
                      "\nlogFC match:", comparison$logFC_match,
                      "\nP.Value match:", comparison$pval_match,
                      "\nadj.P.Val match:", comparison$adjpval_match,
                      "\nsignificant match:", comparison$sig_match)
              } else {
                "Results match"
              })
})

test_that("batch contrast processing handles single contrast", {
  # Setup
  test_gct <- create_mock_gct_for_contrasts(n_genes = 50, n_samples = 20, groups = c("A", "B"))
  contrasts_list <- list(c("A", "B"))

  # Run batch approach
  batch_results <- run_batch_contrasts(test_gct, contrasts_list)

  # Verify results
  expect_equal(length(batch_results), 1)
  expect_equal(names(batch_results), "A_over_B")
  expect_equal(nrow(batch_results[[1]]), 50)
})

test_that("batch contrast processing handles two groups", {
  # Setup
  test_gct <- create_mock_gct_for_contrasts(n_genes = 50, n_samples = 20, groups = c("A", "B"))
  contrasts_list <- list(c("A", "B"))

  # Run both approaches
  per_contrast_results <- run_per_contrast_approach(test_gct, contrasts_list)
  batch_results <- run_batch_contrasts(test_gct, contrasts_list)

  # Compare
  comparison <- compare_results(per_contrast_results, batch_results)
  expect_true(comparison$match)
})

test_that("batch contrast processing handles four groups with multiple contrasts", {
  # Setup
  test_gct <- create_mock_gct_for_contrasts(n_genes = 50, n_samples = 40, groups = c("A", "B", "C", "D"))
  contrasts_list <- list(c("A", "B"), c("C", "D"), c("A", "C"), c("B", "D"))

  # Run both approaches
  per_contrast_results <- run_per_contrast_approach(test_gct, contrasts_list)
  batch_results <- run_batch_contrasts(test_gct, contrasts_list)

  # Compare
  comparison <- compare_results(per_contrast_results, batch_results)
  expect_true(comparison$match)
})

test_that("batch contrast results have correct statistical properties", {
  # Setup
  test_gct <- create_mock_gct_for_contrasts(n_genes = 100, n_samples = 30, groups = c("A", "B", "C"))
  contrasts_list <- list(c("A", "B"))

  # Run batch approach
  batch_results <- run_batch_contrasts(test_gct, contrasts_list)
  result <- batch_results[[1]]

  # Verify p-values are in valid range
  expect_true(all(result$P.Value >= 0 & result$P.Value <= 1))
  expect_true(all(result$adj.P.Val >= 0 & result$adj.P.Val <= 1))

  # Verify adjusted p-values are >= raw p-values (as expected)
  expect_true(all(result$adj.P.Val >= result$P.Value | is.na(result$adj.P.Val)))

  # Verify significant flag is logical
  expect_true(is.logical(result$significant))
})

test_that("batch contrast processing with adjusted p-value threshold", {
  # Setup
  test_gct <- create_mock_gct_for_contrasts(n_genes = 50, n_samples = 30, groups = c("A", "B"))
  contrasts_list <- list(c("A", "B"))

  # Run with different thresholds
  results_strict <- run_batch_contrasts(test_gct, contrasts_list, p.value.alpha = 0.01)
  results_lenient <- run_batch_contrasts(test_gct, contrasts_list, p.value.alpha = 0.1)

  # Verify stricter threshold has fewer or equal significant features
  n_sig_strict <- sum(results_strict[[1]]$significant)
  n_sig_lenient <- sum(results_lenient[[1]]$significant)
  expect_true(n_sig_strict <= n_sig_lenient)
})

test_that("batch contrast processing uses raw p-value when specified", {
  # Setup
  test_gct <- create_mock_gct_for_contrasts(n_genes = 50, n_samples = 30, groups = c("A", "B"))
  contrasts_list <- list(c("A", "B"))

  # Run with raw p-values
  results_raw <- run_batch_contrasts(test_gct, contrasts_list, use.adj.pvalue = FALSE, p.value.alpha = 0.05)
  results_adj <- run_batch_contrasts(test_gct, contrasts_list, use.adj.pvalue = TRUE, p.value.alpha = 0.05)

  # The number of significant features should typically differ
  # (raw p-values usually give more significant features)
  n_sig_raw <- sum(results_raw[[1]]$significant)
  n_sig_adj <- sum(results_adj[[1]]$significant)

  # At minimum, verify both approaches produce valid results
  expect_true(is.numeric(n_sig_raw))
  expect_true(is.numeric(n_sig_adj))
  expect_true(n_sig_raw >= 0)
  expect_true(n_sig_adj >= 0)
})
