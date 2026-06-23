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

test_that("F-test per-group AveExpr columns are key-matched, not positionally misaligned", {
  # Regression: the design's AveExpr.<group> columns follow factor level order
  # (order of APPEARANCE in the group column), but aggregate() sorts its output
  # ALPHABETICALLY. A positional assignment then writes each group's mean into
  # the wrong AveExpr column whenever appearance order != alphabetical order.
  #
  # Lay out group "B" first then "A" (appearance order B, A; alphabetical A, B)
  # and spike a gene's intensity hugely in group A. AveExpr.A must carry the
  # high mean; the bug would put A's mean under AveExpr.B.
  gct <- make_stat_gct(
    n_genes = 12, groups = c("B", "A"), per_group = 4, seed = 7,
    spike = list(gene = "gene_1", group = "A", shift = 100)
  )

  result <- run_stat_testing(
    test = "Moderated F test",
    annotation_col = "group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("B", "A"),
    selected_contrasts = NULL,
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  expect_true(all(c("AveExpr.A", "AveExpr.B") %in% colnames(df)))
  row <- df[df$id == "gene_1", ]
  # The spike (+100) was in group A, so AveExpr.A must be the large one.
  expect_gt(row$AveExpr.A, 50)
  expect_lt(row$AveExpr.B, 50)
  expect_gt(row$AveExpr.A, row$AveExpr.B)
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

test_that("stat.testing one-sample uses the 'id' rdesc column even when it is not first", {
  # Regression: cmapR's parse_gctx orders rdesc with annotation columns FIRST
  # and `id` LAST. The one-sample branch picked the first non-numeric rdesc
  # column as the id, so it grabbed geneSymbol (gene values), and the join on
  # "id" produced all-NA stat columns. Build a GCT with geneSymbol first, id
  # last (the GCT-upload shape) and assert the stats actually populate.
  gct <- make_stat_gct(n_genes = 12, groups = c("A", "B"), per_group = 4, seed = 3)
  # Reorder rdesc so `id` is the LAST column (geneSymbol first), as parse_gctx does.
  gct@rdesc <- gct@rdesc[, c("geneSymbol", "id"), drop = FALSE]

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
  expect_equal(nrow(df), 12)
  # The join must succeed: stat columns are populated, not all-NA.
  expect_true("logFC.A" %in% colnames(df))
  expect_false(all(is.na(df$logFC.A)),
               info = "logFC.A all-NA means the id-column join failed (the bug)")
  expect_false(all(is.na(df$P.Value.A)))
  # The id column must carry the true feature ids (gene_*), not gene symbols.
  expect_true(any(grepl("^gene_", as.character(df$id))))
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

# NOTE: The test_volcano_plot helper and its tests (which re-implemented volcano
# column-extraction logic inline and asserted on the copy) were removed.
# The real cutoff / volcano logic is exercised in test-volcano-labeling.R.

test_that("stat.testing intensity=TRUE exercises trend eBayes path", {
  # Previously this test paired intensity conversion with test='None', which
  # returns NULL before intensity is ever read, so no observable effect.
  # Now we run a real Two-sample test with intensity=TRUE and verify the result
  # is structurally identical to the non-intensity run -- the eBayes(trend=TRUE)
  # path must complete without error and the output columns must be present.
  gct <- make_stat_gct(n_genes = 15, groups = c("A", "B"), per_group = 4, seed = 5)

  res_no <- run_stat_testing(
    test = "Two-sample Moderated T-test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct),
    chosen_groups = c("A", "B"), selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )
  res_yes <- run_stat_testing(
    test = "Two-sample Moderated T-test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct),
    chosen_groups = c("A", "B"), selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = TRUE
  )

  # Both must complete and produce the same row count and contrast columns.
  expect_equal(nrow(res_no$proteome), 15)
  expect_equal(nrow(res_yes$proteome), 15)
  for (col in c("logFC.A_over_B", "P.Value.A_over_B", "adj.P.Val.A_over_B",
                "significant.A_over_B")) {
    expect_true(col %in% colnames(res_no$proteome),  info = paste("no-intensity missing", col))
    expect_true(col %in% colnames(res_yes$proteome), info = paste("intensity missing", col))
  }
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

# NOTE: "Volcano plot handles edge cases" and "Volcano plot handles missing data
# gracefully" were removed -- both used the deleted test_volcano_plot helper and
# never called production code. Volcano logic is covered in test-volcano-labeling.R.

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

test_that("stat.testing produces no duplicate column names when rdesc has an id column", {
  # Regression: when rdesc already carries an "id" column the left_join used to
  # create a duplicate id.x / id.y pair, causing downstream failures.  Run the
  # real stat.testing with a GCT whose rdesc already contains "id" and assert
  # that all output column names are unique.
  gct <- make_stat_gct(n_genes = 10, groups = c("A", "B"), per_group = 3, seed = 81)
  # make_stat_gct already puts an "id" column in rdesc, which is the scenario.
  expect_true("id" %in% colnames(gct@rdesc))

  # Moderated F-test path
  res_f <- run_stat_testing(
    test = "Moderated F test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct),
    chosen_groups = c("A", "B"), selected_contrasts = NULL,
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )
  df_f <- res_f$proteome
  expect_false(any(duplicated(colnames(df_f))),
               info = "F-test result must have unique column names")
  expect_equal(nrow(df_f), 10)

  # Two-sample T-test path
  res_t <- run_stat_testing(
    test = "Two-sample Moderated T-test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct),
    chosen_groups = c("A", "B"), selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )
  df_t <- res_t$proteome
  expect_false(any(duplicated(colnames(df_t))),
               info = "Two-sample result must have unique column names")
  expect_equal(nrow(df_t), 10)
})

test_that("stat.testing output preserves all input rows and includes intensity columns", {
  # Regression: normalized-values join must not drop rows or produce duplicate IDs.
  # Use real stat.testing (not an inline simulation) on a GCT with UniProt-style IDs.
  n_genes <- 25
  n_samples <- 6
  feature_ids <- c(paste0("A0A0", sprintf("%06d", 1:10)),
                   paste0("E9P", sprintf("%04d", 1:5)),
                   paste0("H0Y", sprintf("%04d", 1:5)),
                   paste0("Q9", sprintf("%06d", 1:5)))
  samples <- paste0("sample_", seq_len(n_samples))
  set.seed(99)
  mat <- matrix(rnorm(n_genes * n_samples), nrow = n_genes,
                dimnames = list(feature_ids, samples))
  cdesc <- data.frame(group = rep(c("A", "B"), each = 3), row.names = samples,
                      stringsAsFactors = FALSE)
  rdesc <- data.frame(id = feature_ids,
                      geneSymbol = paste0("SYM_", seq_len(n_genes)),
                      row.names = feature_ids, stringsAsFactors = FALSE)
  gct <- new("GCT", mat = mat, cdesc = cdesc, rdesc = rdesc,
             rid = feature_ids, cid = samples)

  res <- run_stat_testing(
    test = "Two-sample Moderated T-test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct),
    chosen_groups = c("A", "B"), selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )
  df <- res$proteome

  expect_equal(nrow(df), n_genes,
               info = "All input features must be present in output")
  expect_true(all(feature_ids %in% df$id),
              info = "All original IDs must appear in output")
  expect_equal(length(unique(df$id)), n_genes,
               info = "No duplicate IDs")
  # Intensity columns (the raw normalized values appended last) must be present.
  expect_true(all(samples %in% colnames(df)),
              info = "Normalized intensity columns must be present in output")
})

test_that("stat.testing uses rdesc id column (not matrix rownames) for output IDs", {
  # When rdesc$id differs from matrix rownames the output $id must come from
  # rdesc, not from rownames().  Use real stat.testing.
  n_features <- 15
  n_samples <- 4
  set.seed(456)
  mat_rownames <- paste0("row_", seq_len(n_features))
  feature_ids  <- paste0("ID_", seq_len(n_features))
  samples      <- paste0("sample_", seq_len(n_samples))

  mat <- matrix(rnorm(n_features * n_samples), nrow = n_features,
                dimnames = list(mat_rownames, samples))
  cdesc <- data.frame(group = rep(c("A", "B"), each = 2), row.names = samples,
                      stringsAsFactors = FALSE)
  # rdesc rownames match the matrix rownames; rdesc$id carries different values.
  rdesc <- data.frame(id = feature_ids,
                      gene_name = paste0("gene_", seq_len(n_features)),
                      row.names = mat_rownames, stringsAsFactors = FALSE)
  gct <- new("GCT", mat = mat, cdesc = cdesc, rdesc = rdesc,
             rid = mat_rownames, cid = samples)

  res <- run_stat_testing(
    test = "Two-sample Moderated T-test", annotation_col = "group",
    chosen_omes = "proteome", gct = list(proteome = gct),
    chosen_groups = c("A", "B"), selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
    intensity = FALSE
  )
  df <- res$proteome

  expect_equal(nrow(df), n_features)
  # IDs in the output must come from rdesc$id, not from matrix rownames.
  expect_setequal(df$id, feature_ids)
  expect_false(any(mat_rownames %in% df$id),
               info = "Matrix rownames must not appear as ids when rdesc carries its own id column")
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

test_that("stat.testing F-test post-hoc contrasts work with hyphenated group names", {
  # BUG: the F-test post-hoc block used paste0("`f", group1, ...") with the
  # ORIGINAL group name, but design columns are named "f" + make.names(group).
  # For "Non-inflamed" this generated "`fNon-inflamed`" which makeContrasts
  # could not resolve (it expected "`fNon.inflamed`").  Fixed by applying
  # make.names() to the group name in the contrast string.
  gct <- make_stat_gct(
    n_genes = 15,
    groups = c("Inflamed", "Non-inflamed", "Pre-inflamed"),
    per_group = 4, seed = 74,
    spike = list(gene = "gene_1", group = "Inflamed", shift = 10)
  )

  # This must not error -- before the fix it threw
  #   "object 'fNon-inflamed' not found"
  expect_no_error({
    result <- run_stat_testing(
      test = "Moderated F test", annotation_col = "group",
      chosen_omes = "proteome", gct = list(proteome = gct),
      chosen_groups = c("Inflamed", "Non-inflamed", "Pre-inflamed"),
      selected_contrasts = list(
        c("Inflamed", "Non-inflamed"),
        c("Inflamed", "Pre-inflamed")
      ),
      p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log = FALSE,
      intensity = FALSE
    )
  })

  df <- result$proteome
  expect_equal(nrow(df), 15)

  # Post-hoc contrast columns must appear with the ORIGINAL group labels.
  for (cn in c("Inflamed_over_Non-inflamed", "Inflamed_over_Pre-inflamed")) {
    for (stat in c("logFC", "P.Value", "adj.P.Val", "significant",
                   "Log.P.Value", "sign.logP")) {
      col <- paste0(stat, ".", cn)
      expect_true(col %in% colnames(df), info = paste("missing column:", col))
    }
  }

  # gene_1 spiked in Inflamed -> Inflamed_over_Non-inflamed should be positive.
  spike_row <- df[df$id == "gene_1", ]
  expect_gt(spike_row[["logFC.Inflamed_over_Non-inflamed"]], 3)
  expect_lt(spike_row[["P.Value.Inflamed_over_Non-inflamed"]], 0.05)
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
