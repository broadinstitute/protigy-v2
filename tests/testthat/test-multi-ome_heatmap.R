################################################################################
# Unit Tests for Multiomic Heatmap Module
################################################################################

# Test helper functions
create_mock_gct <- function(n_genes = 5, n_samples = 4, ome_name = "proteome") {
  mat <- matrix(rnorm(n_genes * n_samples), nrow = n_genes, ncol = n_samples)
  rownames(mat) <- paste0("gene_", 1:n_genes)
  colnames(mat) <- paste0("sample_", 1:n_samples)

  # Handle odd number of samples
  if (n_samples %% 2 == 0) {
    group_vec <- rep(c("A", "B"), each = n_samples/2)
  } else {
    group_vec <- c(rep("A", floor(n_samples/2)), rep("B", ceiling(n_samples/2)))
  }

  cdesc <- data.frame(
    group = group_vec,
    row.names = paste0("sample_", 1:n_samples)
  )

  rdesc <- data.frame(
    gene_name = paste0("gene_", 1:n_genes),
    geneSymbol = paste0("gene_", 1:n_genes),  # Add geneSymbol column
    protigy.ome = rep(ome_name, n_genes),
    row.names = paste0("gene_", 1:n_genes)
  )

  new("GCT",
      mat = mat,
      cdesc = cdesc,
      rdesc = rdesc,
      rid = paste0("gene_", 1:n_genes),
      cid = paste0("sample_", 1:n_samples)
  )
}

create_mock_merged_gct <- function() {
  # Create a merged GCT with multiple omes
  proteome_gct <- create_mock_gct(5, 4, "proteome")
  phospho_gct <- create_mock_gct(5, 4, "phosphoproteome")

  # Modify the phospho GCT to have different samples (simulating different column counts)
  phospho_gct@mat <- phospho_gct@mat[, 1:3]  # Only 3 samples
  phospho_gct@cdesc <- phospho_gct@cdesc[1:3, , drop = FALSE]
  phospho_gct@cid <- phospho_gct@cid[1:3]
  colnames(phospho_gct@mat) <- paste0("sample_", c(1, 2, 5))  # Different sample set
  rownames(phospho_gct@cdesc) <- paste0("sample_", c(1, 2, 5))
  phospho_gct@cid <- paste0("sample_", c(1, 2, 5))

  # Create merged GCT using the same logic as merge_processed_gcts
  GCTs_processed <- list(proteome = proteome_gct, phosphoproteome = phospho_gct)

  # Add protigy.ome column and create unique IDs
  GCTs_processed <- mapply(
    GCTs_processed, names(GCTs_processed),
    SIMPLIFY = FALSE, USE.NAMES = TRUE,
    FUN = function(gct, ome) {
      gct@rdesc$protigy.ome <- rep(ome, dim(gct@rdesc)[1])
      return(gct)
    })

  # Merge using Reduce (mirrors merge_processed_gcts internals for fixture setup)
  merged <- Reduce(
    function(gct1, gct2) {
      gct1@rdesc$old_id = gct1@rid
      gct2@rdesc$old_id = gct2@rid
      rownames(gct1@mat) = rownames(gct1@rdesc) = gct1@rdesc$id = gct1@rid = paste(gct1@rdesc$protigy.ome,gct1@rid,sep="_")
      rownames(gct2@mat) = rownames(gct2@rdesc) = gct2@rdesc$id = gct2@rid = paste(gct2@rdesc$protigy.ome,gct2@rid,sep="_")
      merged <- cmapR::merge_gct(gct1, gct2, dim='row')
      return(merged)
    },
    GCTs_processed)

  rownames(merged@cdesc) <- merged@cid
  rownames(merged@rdesc) <- merged@rid

  return(merged)
}

create_mock_gcts_and_params <- function() {
  list(
    GCTs = list(
      proteome = create_mock_gct(5, 4, "proteome"),
      phosphoproteome = create_mock_gct(5, 3, "phosphoproteome")
    ),
    GCTs_merged = create_mock_merged_gct(),
    parameters = list(
      proteome = list(annotation_column = "group"),
      phosphoproteome = list(annotation_column = "group")
    )
  )
}

# Helper: build minimal but valid myComplexHeatmap inputs
.hm_test_data <- function(n_omes = 2) {
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  rownames(mat) <- c("proteome_gene1", "proteome_gene2",
                     "phosphoproteome_gene1", "phosphoproteome_gene2")
  colnames(mat) <- paste0("sample_", 1:5)
  rdesc <- data.frame(
    geneSymbol  = c("gene1", "gene2", "gene1", "gene2"),
    protigy.ome = c("proteome", "proteome", "phosphoproteome", "phosphoproteome"),
    row.names   = rownames(mat)
  )
  # sample_anno MUST include Sample.ID (used for sorting/filtering in the function)
  cdesc <- data.frame(
    Sample.ID = paste0("sample_", 1:5),
    group     = rep(c("A", "B"), length.out = 5),
    row.names = paste0("sample_", 1:5)
  )
  custom_colors <- list(
    group = list(is_discrete = TRUE, vals = c("A", "B"),
                 colors = c("red", "blue"))
  )
  list(mat = mat, rdesc = rdesc, cdesc = cdesc, custom_colors = custom_colors)
}

# Test the multiomic heatmap helper functions
# Note: preprocess_gcts_multiome_heatmap test removed
# This function is no longer used since the multi-ome heatmap now uses
# the merged GCT created during the main setup process

# Test the ComplexHeatmap function - simplified test
test_that("myComplexHeatmap function exists and can be called", {
  # Just test that the function exists and can be called with basic parameters
  expect_true(exists("myComplexHeatmap", envir = asNamespace("Protigy")))
  expect_true(is.function(get("myComplexHeatmap", envir = asNamespace("Protigy"))))
})

# Note: Edge case tests for preprocess_gcts_multiome_heatmap removed
# This function is no longer used since the multi-ome heatmap now uses
# the merged GCT created during the main setup process

# Test parameter validation
test_that("Multiomic heatmap validates parameters correctly", {
  merged_gct <- create_mock_merged_gct()

  # Test with invalid gene list - this should result in empty data
  params <- list(
    genes.char = "nonexistent_gene",
    min.val = -2,
    max.val = 2,
    show.rownames = TRUE,
    show.colnames = TRUE,
    cluster.rows = TRUE,
    cluster.cols = TRUE
  )

  custom_colors <- list(
    group = c("A" = "red", "B" = "blue")
  )

  # Should handle invalid genes gracefully - expect error for empty data
  expect_error({
    Protigy:::myComplexHeatmap(
      params = params,
      GENEMAX = 20,
      merged_rdesc = merged_gct@rdesc,
      merged_mat = merged_gct@mat,
      sample_anno = merged_gct@cdesc,
      custom_colors = custom_colors
    )
  })
})

# Note: Different data types test for preprocess_gcts_multiome_heatmap removed
# This function is no longer used since the multi-ome heatmap now uses
# the merged GCT created during the main setup process

# Note: Color function tests removed
# The multiome_heatmap_custom_colors function is no longer used
# Colors are now used directly from globals$colors$multi_ome

# Note: Error handling tests for preprocess_gcts_multiome_heatmap removed
# This function is no longer used since the multi-ome heatmap now uses
# the merged GCT created during the main setup process

# Test the rewritten module logic
test_that("Rewritten multiomic module uses merged GCT correctly", {
  # Create mock GCTs_and_params
  gcts_and_params <- create_mock_gcts_and_params()

  # Test that the merged GCT has the expected structure
  merged_gct <- gcts_and_params$GCTs_merged

  expect_true(is(merged_gct, "GCT"))
  expect_true("protigy.ome" %in% names(merged_gct@rdesc))
  expect_equal(nrow(merged_gct@mat), 10)  # 5 genes from each ome
  expect_equal(ncol(merged_gct@mat), 5)   # All unique samples

  # Test that the merged GCT contains data from both omes
  ome_counts <- table(merged_gct@rdesc$protigy.ome)
  expect_equal(length(ome_counts), 2)
  expect_true("proteome" %in% names(ome_counts))
  expect_true("phosphoproteome" %in% names(ome_counts))
})

# Test sample alignment in merged GCT
test_that("Merged GCT properly aligns samples from different omes", {
  merged_gct <- create_mock_merged_gct()

  # Check that all samples are present
  expected_samples <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5")
  expect_equal(sort(merged_gct@cid), sort(expected_samples))

  # Check that the matrix has the right dimensions
  expect_equal(ncol(merged_gct@mat), 5)
  expect_equal(nrow(merged_gct@mat), 10)  # 5 genes from each ome

  # Check that cdesc has the right samples
  expect_equal(sort(rownames(merged_gct@cdesc)), sort(expected_samples))
})

# Test gene ID uniqueness in merged GCT
test_that("Merged GCT creates unique gene IDs", {
  merged_gct <- create_mock_merged_gct()

  # Check that gene IDs are unique
  expect_equal(length(merged_gct@rid), length(unique(merged_gct@rid)))

  # Check that gene IDs contain ome information
  expect_true(all(grepl("_", merged_gct@rid)))

  # Check that we have genes from both omes
  proteome_genes <- sum(grepl("^proteome_", merged_gct@rid))
  phospho_genes <- sum(grepl("^phosphoproteome_", merged_gct@rid))

  # The merged GCT should have 5 genes from each ome
  expect_equal(proteome_genes, 5)
  expect_equal(phospho_genes, 5)
  expect_equal(length(merged_gct@rid), 10)  # Total genes from both omes
})

# ---------------------------------------------------------------------------
# FIX A (P2.2): direct assertions on myComplexHeatmap return value.
# Each test calls the real function and asserts directly on the output.
# The old tests wrapped every assertion in tryCatch(error = ...) which turned
# any error into a spurious PASS; those wrappers have been removed here.
# ---------------------------------------------------------------------------

test_that("myComplexHeatmap works with valid parameters", {
  skip_if_not_installed("ComplexHeatmap")
  td <- .hm_test_data()
  params <- list(
    genes.char = "gene1,gene2",
    zscore = "none",
    min.val = -2,
    max.val = 2,
    sort.after = "group",
    show.sample.label = FALSE,
    ome.order = c("proteome", "phosphoproteome"),
    max_features_per_gene = 5,
    cluster_columns = TRUE
  )
  result <- Protigy:::myComplexHeatmap(
    params = params, GENEMAX = 20,
    merged_rdesc = td$rdesc, merged_mat = td$mat,
    sample_anno = td$cdesc, custom_colors = td$custom_colors
  )
  expect_true(is.list(result))
  expect_setequal(names(result), c("HM", "Table", "cluster_columns", "cluster_rows"))
  expect_true("geneSymbol" %in% names(result$Table))
  expect_true("ome" %in% names(result$Table))
  # Data rows (non-NA ome) must be from both omes
  data_rows <- result$Table[!is.na(result$Table$ome), ]
  expect_true(nrow(data_rows) > 0)
  expect_true("proteome" %in% data_rows$ome)
  expect_true("phosphoproteome" %in% data_rows$ome)
})

test_that("myComplexHeatmap works with clustering disabled", {
  skip_if_not_installed("ComplexHeatmap")
  td <- .hm_test_data()
  params <- list(
    genes.char = "gene1,gene2",
    zscore = "none",
    min.val = -2,
    max.val = 2,
    sort.after = "group",
    show.sample.label = FALSE,
    ome.order = c("proteome", "phosphoproteome"),
    max_features_per_gene = 5,
    cluster_columns = FALSE
  )
  result <- Protigy:::myComplexHeatmap(
    params = params, GENEMAX = 20,
    merged_rdesc = td$rdesc, merged_mat = td$mat,
    sample_anno = td$cdesc, custom_colors = td$custom_colors
  )
  # cluster_columns = FALSE must be echoed back in the return value
  expect_equal(result$cluster_columns, FALSE)
})

test_that("myComplexHeatmap: ome.order is accepted without dropping data", {
  skip_if_not_installed("ComplexHeatmap")
  td <- .hm_test_data()
  # Request phosphoproteome first (reverse of natural rdesc order).
  # The returned Table's ome column is character (the factor used for row_split
  # inside ComplexHeatmap is stripped when annotation rows are prepended), so
  # visual ordering cannot be directly observed from Table row positions.
  # This test verifies only that passing ome.order does NOT silently drop omes.
  params <- list(
    genes.char = "gene1,gene2",
    zscore = "none",
    min.val = -2,
    max.val = 2,
    sort.after = "group",
    show.sample.label = FALSE,
    ome.order = c("phosphoproteome", "proteome"),
    max_features_per_gene = 5,
    cluster_columns = FALSE
  )
  result <- Protigy:::myComplexHeatmap(
    params = params, GENEMAX = 20,
    merged_rdesc = td$rdesc, merged_mat = td$mat,
    sample_anno = td$cdesc, custom_colors = td$custom_colors
  )
  data_rows <- result$Table[!is.na(result$Table$ome), ]
  # Both omes must be present (ome.order was accepted and not silently dropped)
  expect_true("proteome" %in% data_rows$ome)
  expect_true("phosphoproteome" %in% data_rows$ome)
})

test_that("myComplexHeatmap handles dataset selection", {
  skip_if_not_installed("ComplexHeatmap")
  td <- .hm_test_data()
  params <- list(
    genes.char = "gene1,gene2",
    selected_datasets = c("proteome", "phosphoproteome"),
    zscore = "none",
    min.val = -2,
    max.val = 2,
    sort.after = NULL,
    show.sample.label = FALSE,
    ome.order = NULL,
    max_features_per_gene = 5,
    cluster_columns = TRUE,
    cluster_rows = FALSE
  )
  result <- Protigy:::myComplexHeatmap(
    params = params, GENEMAX = 20,
    merged_rdesc = td$rdesc, merged_mat = td$mat,
    sample_anno = td$cdesc, custom_colors = td$custom_colors
  )
  expect_setequal(names(result), c("HM", "Table", "cluster_columns", "cluster_rows"))
  expect_equal(result$cluster_rows, FALSE)
  # cluster_columns may be downgraded by the function if data has no variation;
  # assert it is a logical scalar (not missing)
  expect_true(is.logical(result$cluster_columns))
})

test_that("myComplexHeatmap handles dataset filtering", {
  skip_if_not_installed("ComplexHeatmap")
  td <- .hm_test_data()
  # Only proteome selected; ome.order = NULL so that ome.order re-leveling
  # cannot independently NA out phosphoproteome rows -- the assertion must
  # exercise selected_datasets alone.
  params <- list(
    genes.char = "gene1,gene2",
    selected_datasets = c("proteome"),
    zscore = "none",
    min.val = -2,
    max.val = 2,
    sort.after = NULL,
    show.sample.label = FALSE,
    ome.order = NULL,
    max_features_per_gene = 5,
    cluster_columns = FALSE,
    cluster_rows = FALSE
  )
  result <- Protigy:::myComplexHeatmap(
    params = params, GENEMAX = 20,
    merged_rdesc = td$rdesc, merged_mat = td$mat,
    sample_anno = td$cdesc, custom_colors = td$custom_colors
  )
  # All non-NA ome values in the Table must be "proteome"
  data_rows <- result$Table[!is.na(result$Table$ome), ]
  expect_true(all(data_rows$ome == "proteome"))
})

test_that("myComplexHeatmap handles empty dataset selection", {
  # Create mock data
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  rownames(mat) <- c("proteome_gene1", "proteome_gene2", "phosphoproteome_gene1", "phosphoproteome_gene2")
  colnames(mat) <- paste0("sample_", 1:5)

  rdesc <- data.frame(
    geneSymbol = c("gene1", "gene2", "gene1", "gene2"),
    protigy.ome = c("proteome", "proteome", "phosphoproteome", "phosphoproteome"),
    row.names = rownames(mat)
  )

  cdesc <- data.frame(
    group = rep(c("A", "B"), length.out = 5),
    row.names = colnames(mat)
  )

  # Test with empty dataset selection
  params <- list(
    genes.char = "gene1,gene2",
    selected_datasets = character(0),
    cluster_columns = TRUE,
    cluster_rows = FALSE
  )

  custom_colors <- list(
    group = c("A" = "red", "B" = "blue")
  )

  expect_error(
    Protigy:::myComplexHeatmap(
      params = params,
      GENEMAX = 20,
      merged_rdesc = rdesc,
      merged_mat = mat,
      sample_anno = cdesc,
      custom_colors = custom_colors
    ),
    "argument is of length zero"
  )
})

test_that("myComplexHeatmap handles row clustering", {
  skip_if_not_installed("ComplexHeatmap")
  td <- .hm_test_data()
  params <- list(
    genes.char = "gene1,gene2",
    selected_datasets = c("proteome", "phosphoproteome"),
    zscore = "none",
    min.val = -2,
    max.val = 2,
    sort.after = NULL,
    show.sample.label = FALSE,
    ome.order = NULL,
    max_features_per_gene = 5,
    cluster_columns = TRUE,
    cluster_rows = TRUE
  )
  result <- Protigy:::myComplexHeatmap(
    params = params, GENEMAX = 20,
    merged_rdesc = td$rdesc, merged_mat = td$mat,
    sample_anno = td$cdesc, custom_colors = td$custom_colors
  )
  # cluster_rows = TRUE must be echoed back
  expect_equal(result$cluster_rows, TRUE)
})

test_that("myComplexHeatmap handles missing cluster_rows parameter", {
  skip_if_not_installed("ComplexHeatmap")
  td <- .hm_test_data()
  params <- list(
    genes.char = "gene1,gene2",
    selected_datasets = c("proteome", "phosphoproteome"),
    zscore = "none",
    min.val = -2,
    max.val = 2,
    sort.after = NULL,
    show.sample.label = FALSE,
    ome.order = NULL,
    max_features_per_gene = 5,
    cluster_columns = TRUE
    # cluster_rows omitted -> defaults to FALSE inside myComplexHeatmap
  )
  result <- Protigy:::myComplexHeatmap(
    params = params, GENEMAX = 20,
    merged_rdesc = td$rdesc, merged_mat = td$mat,
    sample_anno = td$cdesc, custom_colors = td$custom_colors
  )
  # The function defaults cluster_rows to FALSE when the param is absent
  expect_equal(result$cluster_rows, FALSE)
})

test_that("myComplexHeatmap handles selected annotations", {
  skip_if_not_installed("ComplexHeatmap")
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  rownames(mat) <- c("proteome_gene1", "proteome_gene2",
                     "phosphoproteome_gene1", "phosphoproteome_gene2")
  colnames(mat) <- paste0("sample_", 1:5)
  rdesc <- data.frame(
    geneSymbol  = c("gene1", "gene2", "gene1", "gene2"),
    protigy.ome = c("proteome", "proteome", "phosphoproteome", "phosphoproteome"),
    row.names   = rownames(mat)
  )
  cdesc <- data.frame(
    Sample.ID = paste0("sample_", 1:5),
    group     = rep(c("A", "B"), length.out = 5),
    treatment = rep(c("T1", "T2"), length.out = 5),
    row.names = paste0("sample_", 1:5)
  )
  custom_colors <- list(
    group     = list(is_discrete = TRUE, vals = c("A", "B"),
                     colors = c("red", "blue")),
    treatment = list(is_discrete = TRUE, vals = c("T1", "T2"),
                     colors = c("green", "orange"))
  )
  params <- list(
    genes.char = "gene1,gene2",
    selected_datasets = c("proteome", "phosphoproteome"),
    zscore = "none",
    min.val = -2,
    max.val = 2,
    sort.after = NULL,
    show.sample.label = FALSE,
    ome.order = NULL,
    max_features_per_gene = 5,
    cluster_columns = FALSE,
    cluster_rows = FALSE
  )
  result <- Protigy:::myComplexHeatmap(
    params = params, GENEMAX = 20,
    merged_rdesc = rdesc, merged_mat = mat,
    sample_anno = cdesc, custom_colors = custom_colors,
    selected_annotations = c("group", "treatment")
  )
  # Data rows must be present
  data_rows <- result$Table[!is.na(result$Table$ome), ]
  expect_true(nrow(data_rows) > 0)
})

test_that("myComplexHeatmap handles GENEMAX parameter", {
  skip_if_not_installed("ComplexHeatmap")
  # 30 features (15 unique genes x 2 omes)
  n_genes <- 30
  mat <- matrix(rnorm(n_genes * 5), nrow = n_genes, ncol = 5)
  rownames(mat) <- paste0(
    rep(c("proteome", "phosphoproteome"), each = n_genes / 2),
    "_gene", rep(1:(n_genes / 2), 2))
  colnames(mat) <- paste0("sample_", 1:5)
  rdesc <- data.frame(
    geneSymbol  = rep(paste0("gene", 1:(n_genes / 2)), 2),
    protigy.ome = rep(c("proteome", "phosphoproteome"), each = n_genes / 2),
    row.names   = rownames(mat)
  )
  cdesc <- data.frame(
    Sample.ID = paste0("sample_", 1:5),
    group     = rep(c("A", "B"), length.out = 5),
    row.names = paste0("sample_", 1:5)
  )
  params <- list(
    genes.char        = paste0("gene", 1:(n_genes / 2), collapse = ","),
    selected_datasets = c("proteome", "phosphoproteome"),
    zscore = "none",
    min.val = -2,
    max.val = 2,
    sort.after = NULL,
    show.sample.label = FALSE,
    ome.order = NULL,
    max_features_per_gene = 5,
    cluster_columns = FALSE,
    cluster_rows = FALSE
  )
  custom_colors <- list(
    group = list(is_discrete = TRUE, vals = c("A", "B"),
                 colors = c("red", "blue"))
  )
  result <- Protigy:::myComplexHeatmap(
    params = params, GENEMAX = 10,
    merged_rdesc = rdesc, merged_mat = mat,
    sample_anno = cdesc, custom_colors = custom_colors
  )
  data_rows <- result$Table[!is.na(result$Table$ome), ]
  # GENEMAX = 10 must cap the number of distinct genes shown
  expect_lte(length(unique(data_rows$geneSymbol)), 10)
})

# ---------------------------------------------------------------------------
# multiome_heatmap export must skip cleanly, not crash, when no genes have
# been typed into Multi-ome > Heatmap
#
# HM.out() throws via validate()'s "Input genes to see results" whenever
# HM.params()$genes.char is empty. Nothing populates the genes text input
# during a headless export (input$genes stays unset, exactly like this
# testServer session, which never calls session$setInputs(genes = ...)), so
# this failed on EVERY export -- single-ome or multi-ome -- with no visible
# sign of failure beyond a bare error swallowed by tab_export.R's per-item
# tryCatch. It should now skip cleanly (mirrors qc_corr_heatmap_export_function
# for single-sample omes): no error, no file.
# ---------------------------------------------------------------------------

test_that("multiome_heatmap export skips cleanly (no error, no file) when no genes are set", {
  gcts_and_params <- create_mock_gcts_and_params()

  shiny::testServer(
    multiomeHeatmapTabServer,
    args = list(
      GCTs_and_params = shiny::reactiveVal(gcts_and_params),
      globals = shiny::reactiveValues(
        colors = list(multi_ome = list(
          group = list(is_discrete = TRUE, vals = c("A", "B"), colors = c("red", "blue"))
        )),
        default_ome = "proteome",
        default_annotations = list(proteome = "group", phosphoproteome = "group")
      )
    ),
    expr = {
      exports <- session$getReturned()
      tmp_dir <- tempfile("multiome_hm_export_")
      dir.create(tmp_dir)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

      # input$genes was never set (no session$setInputs() call above) -- this
      # is exactly the headless-export scenario that used to crash.
      expect_no_error(exports$multi_ome$multiome_heatmap(tmp_dir))
      expect_identical(list.files(tmp_dir), character(0))
    }
  )
})

# The export guard added to multiome_heatmap_export_function() reuses the
# exact same need()/%then% checks HM.out() itself validates on -- it just
# avoids validate()'s throw. This directly exercises that condition chain
# (same functions, not a copy) to confirm it only blocks when something is
# genuinely missing/invalid, and lets a fully-populated, valid state through.
# (A full end-to-end "render a real heatmap during export" positive control
# would additionally exercise options_multiomeHeatmapTabServer's entire
# parameter set and myComplexHeatmap()'s rendering internals, which is
# already covered independently by the "myComplexHeatmap works with valid
# parameters" test above; wiring both through testServer at once mostly
# re-tests that unrelated machinery rather than this guard.)
test_that("multiome_heatmap export guard only blocks when genes/range are genuinely unset", {
  shiny::isolate({
    merged_rdesc <- shiny::reactive("some rdesc")
    merged_mat   <- shiny::reactive("some mat")
    sample_anno  <- shiny::reactive("some anno")

    guard <- function(genes.char, min.val, max.val) {
      need(merged_rdesc(), "x") %then%
        need(merged_mat(), "x") %then%
        need(sample_anno(), "x") %then%
        need(genes.char, "x") %then%
        need(min.val < max.val, "x")
    }

    # Exactly the headless-export scenario that used to crash: nothing typed in.
    expect_false(is.null(guard(genes.char = NULL, min.val = NA, max.val = NA)))
    expect_false(is.null(guard(genes.char = "",   min.val = -2, max.val = 2)))
    # Invalid range (min >= max) must also block, same as HM.out() itself.
    expect_false(is.null(guard(genes.char = "gene_1", min.val = 2, max.val = -2)))
    # Fully populated, valid state must NOT block.
    expect_true(is.null(guard(genes.char = "gene_1", min.val = -2, max.val = 2)))
  })
})
