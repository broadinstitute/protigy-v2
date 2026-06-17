# Tests for Statistics module functions

# Load test data
data(brca_retrospective_v5.0_proteome_gct)

# Create mock GCT object for testing
create_mock_gct <- function() {
  # Use the loaded data as base
  gct <- brca_retrospective_v5.0_proteome_gct
  
  # Create a smaller subset for testing
  test_mat <- gct@mat[1:20, 1:10]  # 20 genes, 10 samples
  rownames(test_mat) <- paste0("gene_", 1:20)
  colnames(test_mat) <- paste0("sample_", 1:10)
  
  test_cdesc <- data.frame(
    group = rep(c("A", "B", "C"), c(4, 3, 3)),
    batch = rep(c("batch1", "batch2"), 5),
    row.names = paste0("sample_", 1:10)
  )
  
  test_rdesc <- data.frame(
    gene_name = paste0("gene_", 1:20),
    geneSymbol = paste0("GENE", 1:20),
    row.names = paste0("gene_", 1:20)
  )
  
  new("GCT",
      mat = test_mat,
      cdesc = test_cdesc,
      rdesc = test_rdesc,
      rid = paste0("gene_", 1:20),
      cid = paste0("sample_", 1:10)
  )
}

# Create mock statistical results for testing
create_mock_stat_results <- function() {
  # Create mock results for different test types
  list(
    proteome = data.frame(
      id = paste0("gene_", 1:20),
      gene_name = paste0("gene_", 1:20),
      geneSymbol = paste0("GENE", 1:20),
      logFC.A_over_B = rnorm(20, 0, 1),
      P.Value.A_over_B = runif(20, 0, 1),
      adj.P.Val.A_over_B = runif(20, 0, 1),
      Log.P.Value.A_over_B = -log10(runif(20, 0, 1)),
      significant.A_over_B = sample(c(TRUE, FALSE), 20, replace = TRUE),
      stringsAsFactors = FALSE
    )
  )
}

# Create mock statistical parameters for testing
create_mock_stat_params <- function() {
  list(
    proteome = list(
      test = "Two-sample Moderated T-test",
      cutoff = 0.05,
      stat = "adj.p.val"
    )
  )
}

# Mock shinyalert function for testing
mock_shinyalert <- function(message, type = "info", immediate = TRUE) {
  # Just return NULL for testing
  return(NULL)
}

# Assign mock function to global environment
assign('shinyalert', mock_shinyalert, envir = .GlobalEnv)

test_that("stat.testing handles 'None' test type", {
  mock_gct <- create_mock_gct()
  gct_list <- list(proteome = mock_gct)
  
  result <- stat.testing(
    test = "None",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = gct_list,
    chosen_groups = c("A", "B"),
    selected_contrasts = NULL,
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )
  
  expect_null(result)
})

test_that("stat.testing handles intensity parameter conversion", {
  mock_gct <- create_mock_gct()
  gct_list <- list(proteome = mock_gct)
  
  # Test string conversion
  result1 <- stat.testing(
    test = "None",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = gct_list,
    chosen_groups = c("A", "B"),
    selected_contrasts = NULL,
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = "Yes"
  )
  expect_null(result1)
  
  # Test numeric conversion
  result2 <- stat.testing(
    test = "None",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = gct_list,
    chosen_groups = c("A", "B"),
    selected_contrasts = NULL,
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = 1
  )
  expect_null(result2)
  
  # Test NULL handling
  result3 <- stat.testing(
    test = "None",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = gct_list,
    chosen_groups = c("A", "B"),
    selected_contrasts = NULL,
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = NULL
  )
  expect_null(result3)
})

# ---------------------------------------------------------------------------
# Real stat.testing driver + synthetic GCT builder
#
# These replace the previous test_moderated_f_test / test_one_sample_t_test /
# test_two_sample_t_test helpers, which re-implemented limma inside the test file
# (one even used a different model than production: cbind(ref = 1,
# as.numeric(groups)) instead of model.matrix(~ 0 + groups) + makeContrasts) and
# asserted on the copies, so stat.testing was never exercised. The tests below
# call the real stat.testing.
#
# stat.testing wraps work in shiny::withProgress, which needs a live session, so
# we drive it through a trivial testServer module.
# ---------------------------------------------------------------------------
run_stat_testing <- function(...) {
  if (!requireNamespace("limma", quietly = TRUE)) {
    skip("limma package not available")
  }
  args <- list(...)
  wrap <- function(id = "w") {
    shiny::moduleServer(id, function(input, output, session) {
      out <- shiny::reactiveVal(NULL)
      shiny::observe({ out(suppressMessages(do.call(stat.testing, args))) })
      out
    })
  }
  captured <- NULL
  shiny::testServer(wrap, {
    session$flushReact()
    captured <<- out()
  })
  captured
}

# Deterministic synthetic GCT with a known group structure in the `group` column.
make_stat_gct <- function(n_genes = 30, groups = c("A", "B", "C"),
                          per_group = 3, seed = 123, spike = NULL) {
  set.seed(seed)
  group_vec <- rep(groups, each = per_group)
  n_samples <- length(group_vec)
  samples <- paste0("sample_", seq_len(n_samples))
  genes <- paste0("gene_", seq_len(n_genes))

  mat <- matrix(rnorm(n_genes * n_samples), nrow = n_genes,
                dimnames = list(genes, samples))
  if (!is.null(spike)) {
    cols <- which(group_vec == spike$group)
    mat[spike$gene, cols] <- mat[spike$gene, cols] + spike$shift
  }

  cdesc <- data.frame(group = group_vec, row.names = samples,
                      stringsAsFactors = FALSE)
  rdesc <- data.frame(id = genes, geneSymbol = paste0("SYM_", seq_len(n_genes)),
                      row.names = genes, stringsAsFactors = FALSE)

  new("GCT", mat = mat, cdesc = cdesc, rdesc = rdesc,
      rid = genes, cid = samples)
}

# ---------------------------------------------------------------------------
# Moderated F test (real stat.testing, including the post-hoc contrast block)
# ---------------------------------------------------------------------------

test_that("stat.testing F-test produces omnibus columns", {
  gct <- make_stat_gct(n_genes = 20, groups = c("A", "B", "C"), per_group = 4)

  result <- run_stat_testing(
    test = "Moderated F test",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A", "B", "C"),
    selected_contrasts = NULL,
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  expect_equal(nrow(df), 20)
  # Omnibus F-test output columns.
  for (col in c("F", "P.Value", "adj.P.Val", "significant", "total.n",
                "Log.P.Value")) {
    expect_true(col %in% colnames(df), info = paste("missing", col))
  }
  expect_true(all(df$P.Value >= 0 & df$P.Value <= 1, na.rm = TRUE))
  expect_true(all(df$adj.P.Val >= df$P.Value - 1e-9, na.rm = TRUE))
  expect_true(is.logical(df$significant))
})

test_that("stat.testing F-test emits the post-hoc contrast block", {
  gct <- make_stat_gct(
    n_genes = 25, groups = c("A", "B", "C"), per_group = 4, seed = 9,
    spike = list(gene = "gene_1", group = "A", shift = 10)
  )

  result <- run_stat_testing(
    test = "Moderated F test",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A", "B", "C"),
    selected_contrasts = list(c("A", "B"), c("A", "C")),
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  # Post-hoc contrast columns (named group1_over_group2).
  for (cn in c("A_over_B", "A_over_C")) {
    for (stat in c("logFC", "P.Value", "adj.P.Val", "significant",
                   "Log.P.Value", "sign.logP")) {
      col <- paste0(stat, ".", cn)
      expect_true(col %in% colnames(df), info = paste("missing", col))
    }
  }

  # gene_1 was spiked up in A -> A_over_B post-hoc contrast strongly positive.
  spike_row <- df[df$id == "gene_1", ]
  expect_gt(spike_row$logFC.A_over_B, 5)
  expect_lt(spike_row$P.Value.A_over_B, 1e-3)
})

test_that("stat.testing F-test skips omes with insufficient groups", {
  # Only one of the chosen groups is present -> ome is skipped (message + next).
  gct <- make_stat_gct(n_genes = 10, groups = c("A"), per_group = 4)

  result <- run_stat_testing(
    test = "Moderated F test",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A", "B"),
    selected_contrasts = NULL,
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  # The ome had <2 usable groups, so no entry is accumulated for it.
  expect_null(result$proteome)
})

# ---------------------------------------------------------------------------
# One-sample Moderated T-test (real stat.testing)
# ---------------------------------------------------------------------------

test_that("stat.testing one-sample produces per-group columns", {
  gct <- make_stat_gct(n_genes = 20, groups = c("A", "B"), per_group = 4)

  result <- run_stat_testing(
    test = "One-sample Moderated T-test",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A"),
    selected_contrasts = NULL,
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  expect_equal(nrow(df), 20)
  for (stat in c("logFC", "P.Value", "adj.P.Val", "significant",
                 "Log.P.Value", "sign.logP")) {
    col <- paste0(stat, ".A")
    expect_true(col %in% colnames(df), info = paste("missing", col))
  }
  expect_true(all(df$P.Value.A >= 0 & df$P.Value.A <= 1, na.rm = TRUE))
  expect_true(all(df$adj.P.Val.A >= df$P.Value.A - 1e-9, na.rm = TRUE))
})

test_that("stat.testing one-sample handles multiple groups", {
  gct <- make_stat_gct(n_genes = 15, groups = c("A", "B"), per_group = 4)

  result <- run_stat_testing(
    test = "One-sample Moderated T-test",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A", "B"),
    selected_contrasts = NULL,
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  expect_true("logFC.A" %in% colnames(df))
  expect_true("logFC.B" %in% colnames(df))
})

# ---------------------------------------------------------------------------
# Two-sample Moderated T-test (real stat.testing)
# ---------------------------------------------------------------------------

test_that("stat.testing two-sample produces contrast columns", {
  gct <- make_stat_gct(n_genes = 20, groups = c("A", "B"), per_group = 4)

  result <- run_stat_testing(
    test = "Two-sample Moderated T-test",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A", "B"),
    selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  expect_equal(nrow(df), 20)
  for (stat in c("logFC", "P.Value", "adj.P.Val", "significant",
                 "Log.P.Value", "sign.logP")) {
    col <- paste0(stat, ".A_over_B")
    expect_true(col %in% colnames(df), info = paste("missing", col))
  }
  expect_true(is.logical(df$significant.A_over_B))
})

test_that("stat.testing two-sample logFC sign follows A - B convention", {
  # gene_1 is shifted up in group A; A_over_B = A - B should be positive.
  gct <- make_stat_gct(
    n_genes = 20, groups = c("A", "B"), per_group = 5, seed = 4,
    spike = list(gene = "gene_1", group = "A", shift = 8)
  )

  result <- run_stat_testing(
    test = "Two-sample Moderated T-test",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A", "B"),
    selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  spike_row <- df[df$id == "gene_1", ]
  expect_gt(spike_row$logFC.A_over_B, 4)
  expect_lt(spike_row$P.Value.A_over_B, 1e-3)
  expect_true(spike_row$significant.A_over_B)
})

# Helper function to extract core volcano plot logic
test_volcano_plot <- function(df, test_type, volcano_groups = NULL, volcano_contrasts = NULL, 
                              sig_cutoff = 0.05, sig_stat = "adj.p.val", 
                              sig.col = 'darkred', bg.col = 'gray', gene_symbol_col = "geneSymbol") {
  
  # Extract column names based on test type
  if (test_type == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    logfc_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "logFC.", ")")
    logP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "Log.P.Value.", ")")
    adjP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "adj.P.Val.", ")")
    pval_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "P.value.", ")")
  } else if (test_type == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_over_", groups[2])
    logfc_pattern <- paste0("logFC.*", contrast_name)
    logP_pattern <- paste0("Log\\.P\\.Value.*", contrast_name)
    adjP_pattern <- paste0("adj\\.P\\.Val.*", contrast_name)
    pval_pattern <- paste0("P\\.value.*", contrast_name)
  }
  
  logFC_col <- grep(logfc_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  logP_col <- grep(logP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  adjP_col <- grep(adjP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  pval_col <- grep(pval_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  id_col <- grep("id", colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  # Check columns exist
  required_cols <- c(logFC_col, logP_col, adjP_col, id_col)
  if(!all(required_cols %in% colnames(df))) {
    stop("Some required columns are missing in the result data.")
  }
  df <- df[complete.cases(df[, required_cols]), ]
  
  # Add columns for plotting
  df$id <- df[[id_col]]
  df$logFC <- df[[logFC_col]]
  df$adj.P.Val <- as.numeric(df[[adjP_col]])
  df$logP <- df[[logP_col]]
  df$P.Value <- as.numeric(df[[pval_col]])
  
  # Handle geneSymbol column
  geneSymbol_col <- tryCatch({
    grep(gene_symbol_col, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(geneSymbol_col) && !is.na(geneSymbol_col)) {
    df$geneSymbol <- df[[geneSymbol_col]]
  } else {
    df$geneSymbol <- df$id
  }
  
  # Compute threshold for dashed line
  if(sig_stat == "adj.p.val") {
    passing.id <- which(df$adj.P.Val < sig_cutoff)
    if(length(passing.id) > 0){
      y_cutoff <- -log10(max(df$P.Value[passing.id], na.rm = TRUE))
    } else {
      y_cutoff <- Inf
    }
  } else {
    y_cutoff <- -log10(sig_cutoff)
  }
  
  df$Significant <- df$logP > y_cutoff
  
  return(df)
}

test_that("Volcano plot core logic works for Two-sample test", {
  mock_stat_results <- create_mock_stat_results()
  
  result <- test_volcano_plot(
    df = mock_stat_results$proteome,
    test_type = "Two-sample Moderated T-test",
    volcano_contrasts = "A / B",
    sig_cutoff = 0.05,
    sig_stat = "adj.p.val",
    gene_symbol_col = "geneSymbol"
  )
  
  expect_true(is.data.frame(result))
  expect_true("logFC" %in% colnames(result))
  expect_true("logP" %in% colnames(result))
  expect_true("Significant" %in% colnames(result))
  expect_true("geneSymbol" %in% colnames(result))
  expect_true(all(result$Significant %in% c(TRUE, FALSE)))
})

test_that("Volcano plot core logic handles missing gene symbol", {
  mock_stat_results <- create_mock_stat_results()
  # Remove geneSymbol column
  mock_stat_results$proteome$geneSymbol <- NULL
  
  result <- test_volcano_plot(
    df = mock_stat_results$proteome,
    test_type = "Two-sample Moderated T-test",
    volcano_contrasts = "A / B",
    sig_cutoff = 0.05,
    sig_stat = "adj.p.val",
    gene_symbol_col = "geneSymbol"
  )
  
  expect_true(is.data.frame(result))
  expect_true("geneSymbol" %in% colnames(result))
  # Should use ID as fallback
  expect_equal(result$geneSymbol, result$id)
})

test_that("Volcano plot core logic works for One-sample test", {
  mock_stat_results <- create_mock_stat_results()
  
  result <- test_volcano_plot(
    df = mock_stat_results$proteome,
    test_type = "One-sample Moderated T-test",
    volcano_groups = "A",
    sig_cutoff = 0.05,
    sig_stat = "adj.p.val",
    gene_symbol_col = "geneSymbol"
  )
  
  expect_true(is.data.frame(result))
  expect_true("logFC" %in% colnames(result))
  expect_true("logP" %in% colnames(result))
  expect_true("Significant" %in% colnames(result))
})

# Note: plotVolcano missing gene symbol test removed due to reactive context dependencies

# Note: plotVolcano One-sample test removed due to reactive context dependencies

test_that("get_pvals extracts p-values correctly", {
  mock_stat_results <- create_mock_stat_results()
  mock_stat_params <- create_mock_stat_params()
  
  # Test Two-sample Moderated T-test
  pvals <- get_pvals(
    ome = "proteome",
    stat_params = mock_stat_params,
    stat_results = mock_stat_results,
    group = NULL,
    contrast = "A / B",
    pval_type = "P.Value"
  )
  
  expect_true(is.numeric(pvals))
  expect_true(length(pvals) > 0)
  expect_true(all(pvals >= 0 & pvals <= 1))
  
  # Test adj.P.Val
  adj_pvals <- get_pvals(
    ome = "proteome",
    stat_params = mock_stat_params,
    stat_results = mock_stat_results,
    group = NULL,
    contrast = "A / B",
    pval_type = "adj.P.Val"
  )
  
  expect_true(is.numeric(adj_pvals))
  expect_true(length(adj_pvals) > 0)
  expect_true(all(adj_pvals >= 0 & adj_pvals <= 1))
})

test_that("get_pvals handles One-sample Moderated T-test", {
  mock_stat_results <- create_mock_stat_results()
  mock_stat_params <- list(
    proteome = list(
      test = "One-sample Moderated T-test",
      cutoff = 0.05,
      stat = "adj.p.val"
    )
  )
  
  pvals <- get_pvals(
    ome = "proteome",
    stat_params = mock_stat_params,
    stat_results = mock_stat_results,
    group = "A",
    contrast = NULL,
    pval_type = "P.Value"
  )
  
  expect_true(is.numeric(pvals))
  expect_true(length(pvals) > 0)
})

test_that("plot_pval_histogram creates valid ggplot objects", {
  mock_stat_results <- create_mock_stat_results()
  mock_stat_params <- create_mock_stat_params()
  
  pvals <- runif(100, 0, 1)
  
  result <- plot_pval_histogram(
    pvals = pvals,
    title = "Test Histogram",
    xlabel = "P-values",
    stat_params = mock_stat_params,
    stat_results = mock_stat_results,
    ome = "proteome",
    group = NULL,
    contrast = "A / B",
    pval_type = "P.Value"
  )
  
  expect_s3_class(result, "ggplot")
  expect_true("pval" %in% names(result$data))
})

test_that("plot_pval_histogram handles different p-value types", {
  mock_stat_results <- create_mock_stat_results()
  mock_stat_params <- create_mock_stat_params()
  
  pvals <- runif(100, 0, 1)
  
  # Test with adj.P.Val
  result1 <- plot_pval_histogram(
    pvals = pvals,
    title = "Adjusted P-values",
    xlabel = "Adjusted P-values",
    stat_params = mock_stat_params,
    stat_results = mock_stat_results,
    ome = "proteome",
    group = NULL,
    contrast = "A / B",
    pval_type = "adj.P.Val"
  )
  
  expect_s3_class(result1, "ggplot")
  
  # Test with P.Value
  result2 <- plot_pval_histogram(
    pvals = pvals,
    title = "Nominal P-values",
    xlabel = "P-values",
    stat_params = mock_stat_params,
    stat_results = mock_stat_results,
    ome = "proteome",
    group = NULL,
    contrast = "A / B",
    pval_type = "P.Value"
  )
  
  expect_s3_class(result2, "ggplot")
})

test_that("helpButton creates valid HTML elements", {
  # Test with basic parameters
  result <- helpButton(
    el = "Test Element",
    title = "Test Title",
    content = "Test Content",
    placement = "right",
    trigger = "hover",
    offset = 0.5,
    col = 10
  )
  
  expect_s3_class(result, "shiny.tag")
  # Check that it's a div with row class
  expect_equal(result$name, "div")
  expect_equal(result$attribs$class, "row")
  # Check that it has children (the element and help button)
  expect_true(length(result$children) == 2)
})

test_that("helpButton handles different parameters", {
  # Test with different column width
  result1 <- helpButton(
    el = "Test Element",
    title = "Test Title",
    content = "Test Content",
    placement = "left",
    trigger = "click",
    offset = 1.0,
    col = 8
  )
  
  expect_s3_class(result1, "shiny.tag")
  
  # Test with minimal parameters
  result2 <- helpButton("Test Element")
  
  expect_s3_class(result2, "shiny.tag")
})

test_that("stat.testing handles minimal data for each test type", {
  # Two-sample with 2+1 samples across the contrast groups.
  gct_t2 <- make_stat_gct(n_genes = 6, groups = c("A", "B"), per_group = 3,
                          seed = 31)
  res_t2 <- run_stat_testing(
    test = "Two-sample Moderated T-test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct_t2),
    chosen_groups = c("A", "B"), selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )
  expect_equal(nrow(res_t2$proteome), 6)
  expect_true("significant.A_over_B" %in% colnames(res_t2$proteome))

  # F-test with three small groups.
  gct_f <- make_stat_gct(n_genes = 6, groups = c("A", "B", "C"), per_group = 3,
                         seed = 32)
  res_f <- run_stat_testing(
    test = "Moderated F test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct_f),
    chosen_groups = c("A", "B", "C"), selected_contrasts = NULL,
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )
  expect_equal(nrow(res_f$proteome), 6)
  expect_true("significant" %in% colnames(res_f$proteome))
})

test_that("stat.testing handles NA values in the matrix", {
  gct <- make_stat_gct(n_genes = 8, groups = c("A", "B", "C"), per_group = 3,
                       seed = 41)
  # Introduce a couple of NAs without zeroing out any feature entirely.
  gct@mat[1, 1] <- NA
  gct@mat[2, 5] <- NA

  res_f <- run_stat_testing(
    test = "Moderated F test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct),
    chosen_groups = c("A", "B", "C"), selected_contrasts = NULL,
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )
  df <- res_f$proteome
  expect_equal(nrow(df), 8)
  expect_true("total.n" %in% colnames(df))
  # total.n counts finite observations and must be finite for every feature.
  expect_true(all(is.finite(df$total.n)))
})

test_that("stat.testing alpha and adjustment monotonicity (F-test)", {
  gct <- make_stat_gct(n_genes = 30, groups = c("A", "B", "C"), per_group = 4,
                       seed = 51,
                       spike = list(gene = "gene_1", group = "A", shift = 8))

  run <- function(alpha, use_adj) {
    res <- run_stat_testing(
      test = "Moderated F test", annotation_col = "group",
      chosen_omes = "proteome", gct = list(proteome = gct),
      chosen_groups = c("A", "B", "C"), selected_contrasts = NULL,
      p.value.alpha = alpha, use.adj.pvalue = use_adj, apply.log = FALSE,
      intensity = FALSE
    )
    sum(res$proteome$significant, na.rm = TRUE)
  }

  # Stricter alpha never flags more features.
  expect_lte(run(0.01, TRUE), run(0.10, TRUE))
  # Adjusted p-values are never less conservative than nominal at the same alpha.
  expect_lte(run(0.05, TRUE), run(0.05, FALSE))
})

test_that("Volcano plot handles edge cases", {
  # Create test data with edge cases
  edge_case_data <- data.frame(
    id = paste0("gene_", 1:5),
    gene_name = paste0("gene_", 1:5),
    geneSymbol = paste0("GENE", 1:5),
    logFC.A_over_B = c(0, 1, -1, 10, -10),  # Including extreme values
    P.Value.A_over_B = c(0.001, 0.01, 0.1, 0.5, 0.9),
    adj.P.Val.A_over_B = c(0.01, 0.05, 0.2, 0.6, 0.95),
    Log.P.Value.A_over_B = c(3, 2, 1, 0.3, 0.05),
    significant.A_over_B = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  
  # Test with extreme logFC values
  result <- test_volcano_plot(
    df = edge_case_data,
    test_type = "Two-sample Moderated T-test",
    volcano_contrasts = "A / B",
    sig_cutoff = 0.05,
    sig_stat = "adj.p.val",
    gene_symbol_col = "geneSymbol"
  )
  
  expect_true(is.data.frame(result))
  expect_true("logFC" %in% colnames(result))
  expect_true("logP" %in% colnames(result))
  expect_true("Significant" %in% colnames(result))
  expect_true(all(is.finite(result$logFC)))
  expect_true(all(is.finite(result$logP)))
})

test_that("Volcano plot handles missing data gracefully", {
  # Create test data with missing values in non-required columns
  missing_data <- data.frame(
    id = paste0("gene_", 1:5),
    gene_name = paste0("gene_", 1:5),
    geneSymbol = paste0("GENE", 1:5),
    logFC.A_over_B = c(1, 0.5, -1, 2, -2),
    P.Value.A_over_B = c(0.01, 0.05, 0.1, 0.1, 0.2),
    adj.P.Val.A_over_B = c(0.05, 0.1, 0.1, 0.15, 0.25),
    Log.P.Value.A_over_B = c(2, 1.3, 1, 1, 0.7),
    significant.A_over_B = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  
  # Should handle missing data gracefully by filtering out incomplete cases
  result <- test_volcano_plot(
    df = missing_data,
    test_type = "Two-sample Moderated T-test",
    volcano_contrasts = "A / B",
    sig_cutoff = 0.05,
    sig_stat = "adj.p.val",
    gene_symbol_col = "geneSymbol"
  )
  
  expect_true(is.data.frame(result))
  expect_true("logFC" %in% colnames(result))
  expect_true("logP" %in% colnames(result))
  expect_true("Significant" %in% colnames(result))
})

test_that("stat.testing handles a single-feature matrix", {
  # Two-sample with a single feature.
  gct_t2 <- make_stat_gct(n_genes = 1, groups = c("A", "B"), per_group = 3,
                          seed = 61)
  res_t2 <- run_stat_testing(
    test = "Two-sample Moderated T-test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct_t2),
    chosen_groups = c("A", "B"), selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )
  expect_equal(nrow(res_t2$proteome), 1)
  expect_true("significant.A_over_B" %in% colnames(res_t2$proteome))

  # One-sample with a single feature.
  res_t1 <- run_stat_testing(
    test = "One-sample Moderated T-test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct_t2),
    chosen_groups = c("A"), selected_contrasts = NULL,
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )
  expect_equal(nrow(res_t1$proteome), 1)
  expect_true("significant.A" %in% colnames(res_t1$proteome))
})

test_that("Statistics functions handle column duplication fix", {
  # Test that the fix for "Input columns in x must be unique" error works
  # This test simulates the scenario where rdesc already has an "id" column
  
  # Create mock GCT with rdesc that already has an "id" column
  mock_mat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  rownames(mock_mat) <- paste0("gene_", 1:10)
  colnames(mock_mat) <- paste0("sample_", 1:6)
  
  mock_cdesc <- data.frame(
    group = rep(c("A", "B"), each = 3),
    row.names = paste0("sample_", 1:6)
  )
  
  # Create rdesc with an existing "id" column (this was causing the error)
  mock_rdesc <- data.frame(
    id = paste0("gene_", 1:10),  # This column already exists
    gene_name = paste0("gene_", 1:10),
    geneSymbol = paste0("GENE", 1:10),
    row.names = paste0("gene_", 1:10)
  )
  
  mock_gct <- new("GCT",
    mat = mock_mat,
    cdesc = mock_cdesc,
    rdesc = mock_rdesc,
    rid = paste0("gene_", 1:10),
    cid = paste0("sample_", 1:6)
  )
  
  # Test that the statistical functions work without column duplication errors
  # This simulates the core logic from stat.testing without the Shiny wrapper
  
  # Test Moderated F-test logic
  groups <- rep(c("A", "B"), each = 3)
  f <- factor(groups)
  design <- model.matrix(~ 0 + f)
  data.rownorm <- sweep(mock_mat, MARGIN = 1, STATS = apply(mock_mat, 1, mean, na.rm = TRUE))
  
  if (requireNamespace("limma", quietly = TRUE)) {
    fit <- limma::lmFit(data.rownorm, design)
    fit <- limma::eBayes(fit, robust = TRUE)
    sig <- limma::topTable(fit, number = nrow(mock_mat), sort.by = 'none')
    
    # Test the column joining logic that was fixed
    rdesc_df <- as.data.frame(mock_rdesc)
    
    # This should NOT create a duplicate "id" column
    if (!"id" %in% colnames(rdesc_df)) {
      id.col <- names(Filter(function(col) !is.numeric(col), mock_rdesc))[1]
      rdesc_df[[id.col]] <- rownames(rdesc_df)
      colnames(rdesc_df)[colnames(rdesc_df) == id.col] <- "id"
    }
    
    # Create final results
    final.results <- data.frame(
      id = rownames(mock_mat),
      sig,
      stringsAsFactors = FALSE
    )
    
    # Test the dplyr::right_join that was failing
    expect_no_error({
      combined_results <- dplyr::right_join(rdesc_df, final.results, by = "id")
    })
    
    expect_true(is.data.frame(combined_results))
    expect_equal(nrow(combined_results), nrow(mock_mat))
    expect_true("id" %in% colnames(combined_results))
    expect_false(any(duplicated(colnames(combined_results))))
  }
  
  # Test Two-sample T-test logic
  groups <- rep(c("A", "B"), each = 3)
  groups <- factor(groups, levels = c("A", "B"))
  design.mat <- cbind(ref = 1, comparison = as.numeric(groups))
  data.matrix <- data.frame(mock_mat, stringsAsFactors = FALSE)
  
  if (requireNamespace("limma", quietly = TRUE)) {
    m <- limma::lmFit(data.matrix, design.mat)
    m <- limma::eBayes(m, robust = TRUE)
    sig <- limma::topTable(m, coef = colnames(design.mat)[2], number = nrow(mock_mat), sort.by = "none")
    
    # Test the column joining logic for two-sample test
    rdesc_df <- as.data.frame(mock_rdesc)
    
    # This should NOT create a duplicate "id" column
    if (!"id" %in% colnames(rdesc_df)) {
      id.col <- names(Filter(function(col) !is.numeric(col), mock_rdesc))[1]
      colnames(rdesc_df)[colnames(rdesc_df) == id.col] <- "id"
    }
    
    final.results <- data.frame(
      id = rownames(mock_mat),
      sig,
      stringsAsFactors = FALSE
    )
    
    # Test the dplyr::right_join that was failing
    expect_no_error({
      combined_results <- dplyr::right_join(rdesc_df, final.results, by = "id")
    })
    
    expect_true(is.data.frame(combined_results))
    expect_equal(nrow(combined_results), nrow(mock_mat))
    expect_true("id" %in% colnames(combined_results))
    expect_false(any(duplicated(colnames(combined_results))))
  }
})

test_that("Normalized values join preserves all rows in statistics results", {
  # Test that when normalized values are added to statistics results,
  # all rows are preserved and IDs match correctly
  
  # Create mock data with specific IDs to test the join
  set.seed(123)
  n_features <- 25
  n_samples <- 6
  
  # Create matrix with some IDs that might cause issues (e.g., potential duplicates)
  mock_mat <- matrix(rnorm(n_features * n_samples), nrow = n_features, ncol = n_samples)
  # Use IDs that could potentially have issues if rownames are used incorrectly
  feature_ids <- c(paste0("A0A0", sprintf("%06d", 1:10)), 
                   paste0("E9P", sprintf("%04d", 1:5)),
                   paste0("H0Y", sprintf("%04d", 1:5)),
                   paste0("Q9", sprintf("%06d", 1:5)))
  rownames(mock_mat) <- feature_ids
  colnames(mock_mat) <- paste0("sample_", 1:n_samples)
  
  # Create rdesc with id column (matching the actual GCT structure)
  mock_rdesc <- data.frame(
    id = feature_ids,  # Same IDs as rownames
    gene_name = paste0("gene_", 1:n_features),
    geneSymbol = paste0("GENE", 1:n_features),
    row.names = feature_ids
  )
  
  mock_cdesc <- data.frame(
    group = rep(c("A", "B"), each = 3),
    row.names = paste0("sample_", 1:n_samples)
  )
  
  mock_gct <- new("GCT",
    mat = mock_mat,
    cdesc = mock_cdesc,
    rdesc = mock_rdesc,
    rid = feature_ids,
    cid = paste0("sample_", 1:n_samples)
  )
  
  # Simulate the statistics calculation and normalized values join
  # This mimics the logic from tab_stat_setup_helpers.R
  
  # Step 1: Get data (as in the actual function)
  ome_data <- mock_gct@mat
  rdesc <- mock_gct@rdesc
  
  # Step 2: Create statistics results (simplified)
  # In real code, this would come from limma, but for testing we'll create mock results
  final_results <- data.frame(
    id = feature_ids,
    logFC = rnorm(n_features),
    P.Value = runif(n_features, 0, 1),
    adj.P.Val = runif(n_features, 0, 1),
    stringsAsFactors = FALSE
  )
  
  # Step 3: Join rdesc to results (as in actual code)
  rdesc_df <- as.data.frame(rdesc)
  if (!"id" %in% colnames(rdesc_df)) {
    rdesc_df$id <- rownames(rdesc_df)
  }
  
  combined_results <- dplyr::left_join(rdesc_df, final_results, by = "id")
  
  # Verify initial join preserved all rows
  expect_equal(nrow(combined_results), n_features)
  expect_true(all(feature_ids %in% combined_results$id))
  
  # Step 4: Add normalized values (the part we fixed)
  normalized_df <- as.data.frame(ome_data)
  # Use the same ID source as used for statistics (from rdesc)
  if ("id" %in% colnames(rdesc)) {
    normalized_df$id <- rdesc[["id"]]
  } else {
    normalized_df$id <- rownames(rdesc)
  }
  
  # Verify normalized_df has correct IDs
  expect_equal(nrow(normalized_df), n_features)
  expect_true(all(feature_ids %in% normalized_df$id))
  
  # Step 5: Join normalized values
  combined_results_with_intensities <- dplyr::left_join(combined_results, normalized_df, by = "id")
  
  # THE KEY TEST: All rows should be preserved
  expect_equal(nrow(combined_results_with_intensities), n_features, 
               info = "All rows must be preserved when adding normalized values")
  expect_true(all(feature_ids %in% combined_results_with_intensities$id),
              info = "All original IDs must be present after join")
  
  # Verify that normalized values were added (should have sample columns)
  expect_true(all(paste0("sample_", 1:n_samples) %in% colnames(combined_results_with_intensities)),
              info = "Normalized intensity columns should be present")
  
  # Verify no duplicate IDs in the result
  expect_equal(length(unique(combined_results_with_intensities$id)), n_features,
               info = "No duplicate IDs should be created")
})

test_that("Normalized values join works with IDs from rdesc (not rownames)", {
  # Test that the join uses rdesc[["id"]] correctly, not rownames
  # This tests the specific fix we made
  
  set.seed(456)
  n_features <- 15
  n_samples <- 4
  
  # Create matrix where rownames might differ from rdesc$id (edge case)
  mock_mat <- matrix(rnorm(n_features * n_samples), nrow = n_features, ncol = n_samples)
  rownames(mock_mat) <- paste0("row_", 1:n_features)
  colnames(mock_mat) <- paste0("sample_", 1:n_samples)
  
  # Create rdesc with id column that's different from rownames
  # This simulates a case where rownames might be modified but rdesc$id is correct
  feature_ids <- paste0("ID_", 1:n_features)
  mock_rdesc <- data.frame(
    id = feature_ids,  # IDs in rdesc (the correct ones to use)
    gene_name = paste0("gene_", 1:n_features),
    row.names = rownames(mock_mat)  # Rownames match matrix, but IDs are different
  )
  
  mock_cdesc <- data.frame(
    group = rep(c("A", "B"), each = 2),
    row.names = paste0("sample_", 1:n_samples)
  )
  
  mock_gct <- new("GCT",
    mat = mock_mat,
    cdesc = mock_cdesc,
    rdesc = mock_rdesc,
    rid = rownames(mock_mat),
    cid = paste0("sample_", 1:n_samples)
  )
  
  # Simulate statistics results using rdesc IDs (as in actual code)
  ome_data <- mock_gct@mat
  rdesc <- mock_gct@rdesc
  
  # Create final results with IDs from rdesc (as in actual code)
  final_results <- data.frame(
    id = rdesc[["id"]],  # Using rdesc IDs, not rownames
    logFC = rnorm(n_features),
    P.Value = runif(n_features, 0, 1),
    stringsAsFactors = FALSE
  )
  
  # Join rdesc to results
  rdesc_df <- as.data.frame(rdesc)
  if (!"id" %in% colnames(rdesc_df)) {
    rdesc_df$id <- rownames(rdesc_df)
  }
  
  combined_results <- dplyr::left_join(rdesc_df, final_results, by = "id")
  expect_equal(nrow(combined_results), n_features)
  
  # Add normalized values using the FIXED approach (rdesc[["id"]], not rownames)
  normalized_df <- as.data.frame(ome_data)
  if ("id" %in% colnames(rdesc)) {
    normalized_df$id <- rdesc[["id"]]  # Use rdesc IDs, not rownames
  } else {
    normalized_df$id <- rownames(rdesc)
  }
  
  # Join normalized values
  combined_results_with_intensities <- dplyr::left_join(combined_results, normalized_df, by = "id")
  
  # All rows should be preserved
  expect_equal(nrow(combined_results_with_intensities), n_features,
               info = "All rows must be preserved when using rdesc IDs")
  expect_true(all(feature_ids %in% combined_results_with_intensities$id),
              info = "All rdesc IDs must be present")
  
  # Verify IDs match between results and normalized values
  expect_setequal(combined_results_with_intensities$id, feature_ids)
})

# NOTE: The p-value/volcano adjusted-cutoff tautology tests that used to live
# here (they recomputed the expected cutoff with the same expression they
# asserted) were removed. The real cutoff logic is exercised by build_volcano_df
# / volcano pipeline tests in test-volcano-labeling.R.

test_that("stat.testing two-sample handles hyphens in group names", {
  # Group names with hyphens (e.g. "Non-inflamed") must round-trip through the
  # make.names()-based contrast naming without error. Real stat.testing only.
  gct <- make_stat_gct(n_genes = 12, groups = c("Inflamed", "Non-inflamed"),
                       per_group = 4, seed = 71)

  result <- run_stat_testing(
    test = "Two-sample Moderated T-test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct),
    chosen_groups = c("Inflamed", "Non-inflamed"),
    selected_contrasts = list(c("Inflamed", "Non-inflamed")),
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  expect_equal(nrow(df), 12)
  # Contrast name keeps the original group labels (Inflamed_over_Non-inflamed).
  expect_true("logFC.Inflamed_over_Non-inflamed" %in% colnames(df))
  expect_true("P.Value.Inflamed_over_Non-inflamed" %in% colnames(df))
})

test_that("stat.testing one-sample handles hyphens in group names", {
  gct <- make_stat_gct(n_genes = 10, groups = c("Non-inflamed", "Other"),
                       per_group = 4, seed = 72)

  result <- run_stat_testing(
    test = "One-sample Moderated T-test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct),
    chosen_groups = c("Non-inflamed"), selected_contrasts = NULL,
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  expect_equal(nrow(df), 10)
  # One-sample columns are suffixed with the make.names()-valid group label.
  expect_true("logFC.Non.inflamed" %in% colnames(df))
  expect_true("significant.Non.inflamed" %in% colnames(df))
})

test_that("stat.testing F-test handles hyphens in group names", {
  gct <- make_stat_gct(
    n_genes = 12,
    groups = c("Inflamed", "Non-inflamed", "Pre-inflamed"),
    per_group = 4, seed = 73
  )

  expect_no_error({
    result <- run_stat_testing(
      test = "Moderated F test", annotation_col = "group",
      chosen_omes = "proteome", gct = list(proteome = gct),
      chosen_groups = c("Inflamed", "Non-inflamed", "Pre-inflamed"),
      selected_contrasts = NULL,
      p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
      intensity = FALSE
    )
  })
  expect_equal(nrow(result$proteome), 12)
  expect_true("F" %in% colnames(result$proteome))
})

################################################################################
# Test Annotation Column Suitability for Statistical Testing
#
# These drive the REAL annotation_suitable_for_testing() reactive in
# statSetup_Tab_Server via shiny::testServer. The previous versions re-inlined a
# copy of the predicate body and asserted on the copy, so the production reactive
# was never exercised.
################################################################################

# Build a GCT whose cdesc has a single annotation column `annot` with the given
# values, and wire it through GCTs_and_params + globals for testServer.
suitability_args <- function(annot_values) {
  n <- length(annot_values)
  samples <- paste0("sample_", seq_len(n))
  mat <- matrix(seq_len(n * 2), nrow = 2, ncol = n,
                dimnames = list(c("g1", "g2"), samples))
  cdesc <- data.frame(annot = annot_values, row.names = samples,
                      stringsAsFactors = FALSE)
  rdesc <- data.frame(id = c("g1", "g2"), row.names = c("g1", "g2"),
                      stringsAsFactors = FALSE)
  gct <- new("GCT", mat = mat, cdesc = cdesc, rdesc = rdesc,
             rid = c("g1", "g2"), cid = samples)

  list(
    GCTs_and_params = shiny::reactiveVal(list(
      GCTs = list(ome1 = gct),
      parameters = list(ome1 = list(annotation_column = "annot"))
    )),
    globals = shiny::reactiveValues(default_ome = "ome1", colors = list())
  )
}

# Evaluate annotation_suitable_for_testing() against the real reactive.
check_suitable <- function(annot_values) {
  a <- suitability_args(annot_values)
  result <- NULL
  shiny::testServer(
    statSetup_Tab_Server,
    args = list(GCTs_and_params = a$GCTs_and_params, globals = a$globals),
    {
      session$setInputs(selected_omes = "ome1")
      result <<- annotation_suitable_for_testing()
    }
  )
  result
}

test_that("annotation_suitable_for_testing: TRUE for >=2 categories", {
  expect_true(check_suitable(c("A", "A", "B", "B")))
})

test_that("annotation_suitable_for_testing: TRUE for 3 categories", {
  expect_true(check_suitable(c("A", "A", "B", "B", "C", "C")))
})

test_that("annotation_suitable_for_testing: FALSE for a single category", {
  expect_false(check_suitable(c("A", "A", "A", "A")))
})

test_that("annotation_suitable_for_testing: FALSE for an ID column (all unique)", {
  expect_false(check_suitable(c("S1", "S2", "S3", "S4")))
})

test_that("annotation_suitable_for_testing: NA values are ignored", {
  # Two real categories after dropping NAs -> suitable.
  expect_true(check_suitable(c("A", "A", NA, "B", "B", NA)))
})

test_that("annotation_suitable_for_testing: single category after dropping NAs is FALSE", {
  expect_false(check_suitable(c("A", NA, NA, NA)))
})

test_that("annotation_suitable_for_testing: repeated categories are not an ID column", {
  # S1 repeats -> not all-unique -> not an ID column -> suitable (>=2 cats).
  expect_true(check_suitable(c("S1", "S2", "S1", "S3")))
})

test_that("annotation_suitable_for_testing: ID column with NAs is still FALSE", {
  # All non-NA values unique and character -> ID column -> not suitable.
  expect_false(check_suitable(c("S1", "S2", NA, "S3", "S4")))
})

test_that("annotation_suitable_for_testing: all-NA column is FALSE", {
  expect_false(check_suitable(c(NA, NA, NA, NA)))
})

test_that("annotation_suitable_for_testing: unique numeric column is suitable", {
  # Numeric columns are never treated as ID columns (the guard requires
  # is.character), so a unique numeric column with >=2 values is suitable.
  expect_true(check_suitable(c(1, 2, 3, 4)))
})
