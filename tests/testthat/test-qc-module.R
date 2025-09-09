# Tests for QC module functions

# Load test data
data(brca_retrospective_v5.0_proteome_gct)

# Create mock GCT object for testing
create_mock_gct <- function() {
  # Use the loaded data as base
  gct <- brca_retrospective_v5.0_proteome_gct
  
  # Create a smaller subset for testing
  test_mat <- gct@mat[1:10, 1:8]  # 10 genes, 8 samples
  rownames(test_mat) <- paste0("gene_", 1:10)
  colnames(test_mat) <- paste0("sample_", 1:8)
  
  test_cdesc <- data.frame(
    group = rep(c("A", "B"), each = 4),
    batch = rep(c("batch1", "batch2"), 4),
    row.names = paste0("sample_", 1:8)
  )
  
  test_rdesc <- data.frame(
    gene_name = paste0("gene_", 1:10),
    row.names = paste0("gene_", 1:10)
  )
  
  new("GCT",
      mat = test_mat,
      cdesc = test_cdesc,
      rdesc = test_rdesc,
      rid = paste0("gene_", 1:10),
      cid = paste0("sample_", 1:8)
  )
}

# Create mock parameters
create_mock_parameters <- function() {
  list(
    data_normalization = "Median",
    max_missing = 50,
    data_filter = "None"
  )
}

# Create mock color map
create_mock_color_map <- function() {
  list(
    is_discrete = TRUE,
    colors = list(group = c("red", "blue")),
    vals = c("A", "B")
  )
}

test_that("create_boxplot creates valid ggplot objects", {
  mock_gct <- create_mock_gct()
  mock_params <- create_mock_parameters()
  mock_colors <- create_mock_color_map()
  
  # Test original boxplot
  result_org <- create_boxplot(mock_gct, "group", "test_ome", mock_colors, mock_params, "org")
  expect_s3_class(result_org, "ggplot")
  
  # Test normalized boxplot
  result_norm <- create_boxplot(mock_gct, "group", "test_ome", mock_colors, mock_params, "norm")
  expect_s3_class(result_norm, "ggplot")
  
  # Test with no normalization
  mock_params_no_norm <- list(
    data_normalization = "None",
    max_missing = 100,
    data_filter = "None"
  )
  result_no_norm <- create_boxplot(mock_gct, "group", "test_ome", mock_colors, mock_params_no_norm, "norm")
  expect_s3_class(result_no_norm, "ggplot")
})

test_that("create_boxplot handles different color map types", {
  mock_gct <- create_mock_gct()
  mock_params <- create_mock_parameters()
  
  # Test with discrete color map
  discrete_colors <- list(
    is_discrete = TRUE,
    colors = list(group = c("red", "blue")),
    vals = c("A", "B")
  )
  result_discrete <- create_boxplot(mock_gct, "group", "test_ome", discrete_colors, mock_params, "org")
  expect_s3_class(result_discrete, "ggplot")
  
  # Skip continuous color map test for boxplots - complex to test properly
  # The function expects specific color map structure that's difficult to mock
  
  # Test with NULL color map
  result_null <- create_boxplot(mock_gct, "group", "test_ome", NULL, mock_params, "org")
  expect_s3_class(result_null, "ggplot")
})

test_that("create_profile_plot creates valid ggplot objects", {
  mock_gct <- create_mock_gct()
  mock_params <- create_mock_parameters()
  mock_colors <- create_mock_color_map()
  
  # Test original profile plot
  result_org <- create_profile_plot(mock_gct, "group", "test_ome", mock_colors, mock_params, "org")
  expect_s3_class(result_org, "ggplot")
  
  # Test normalized profile plot
  result_norm <- create_profile_plot(mock_gct, "group", "test_ome", mock_colors, mock_params, "norm")
  expect_s3_class(result_norm, "ggplot")
  
  # Test with no normalization
  mock_params_no_norm <- list(
    data_normalization = "None",
    max_missing = 100,
    data_filter = "None"
  )
  result_no_norm <- create_profile_plot(mock_gct, "group", "test_ome", mock_colors, mock_params_no_norm, "norm")
  expect_s3_class(result_no_norm, "ggplot")
})

test_that("create_profile_plot handles different color map types", {
  mock_gct <- create_mock_gct()
  mock_params <- create_mock_parameters()
  
  # Test with discrete color map
  discrete_colors <- list(
    is_discrete = TRUE,
    colors = list(group = c("red", "blue")),
    vals = c("A", "B")
  )
  result_discrete <- create_profile_plot(mock_gct, "group", "test_ome", discrete_colors, mock_params, "org")
  expect_s3_class(result_discrete, "ggplot")
  
  # Skip continuous color map test for profile plots - complex to test properly
  # The function expects specific color map structure that's difficult to mock
  
  # Test with NULL color map
  result_null <- create_profile_plot(mock_gct, "group", "test_ome", NULL, mock_params, "org")
  expect_s3_class(result_null, "ggplot")
})

test_that("create_corr_heatmap creates valid heatmap objects", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Test with pearson correlation
  result_pearson <- create_corr_heatmap(mock_gct, "group", "test_ome", mock_colors, "pearson")
  expect_true(is.list(result_pearson))
  expect_true("HM" %in% names(result_pearson))
  expect_true("Table" %in% names(result_pearson))
  expect_true(is.matrix(result_pearson$Table))
  
  # Test with spearman correlation
  result_spearman <- create_corr_heatmap(mock_gct, "group", "test_ome", mock_colors, "spearman")
  expect_true(is.list(result_spearman))
  expect_true("HM" %in% names(result_spearman))
  expect_true("Table" %in% names(result_spearman))
  expect_true(is.matrix(result_spearman$Table))
})

test_that("create_corr_heatmap handles different color map types", {
  mock_gct <- create_mock_gct()
  
  # Test with discrete color map
  discrete_colors <- list(
    is_discrete = TRUE,
    colors = list(group = c("red", "blue")),
    vals = c("A", "B")
  )
  result_discrete <- create_corr_heatmap(mock_gct, "group", "test_ome", discrete_colors, "pearson")
  expect_true(is.list(result_discrete))
  
  # Test with continuous color map - correlation heatmap only supports discrete colors
  # Skip continuous color test for correlation heatmap as it only handles discrete colors
  # The function expects discrete color maps for annotation
})

test_that("create_corr_boxplot creates valid ggplot objects", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Calculate correlation matrix for testing
  cor_matrix <- cor(mock_gct@mat, use = "pairwise.complete.obs", method = "pearson")
  
  # Test with pearson correlation
  result_pearson <- create_corr_boxplot(mock_gct, "group", "test_ome", mock_colors, "pearson", cor_matrix)
  expect_s3_class(result_pearson, "ggplot")
  
  # Test with spearman correlation
  result_spearman <- create_corr_boxplot(mock_gct, "group", "test_ome", mock_colors, "spearman", cor_matrix)
  expect_s3_class(result_spearman, "ggplot")
})

test_that("create_corr_boxplot handles different color map types", {
  mock_gct <- create_mock_gct()
  cor_matrix <- cor(mock_gct@mat, use = "pairwise.complete.obs", method = "pearson")
  
  # Test with discrete color map
  discrete_colors <- list(
    is_discrete = TRUE,
    colors = list(group = c("red", "blue")),
    vals = c("A", "B")
  )
  result_discrete <- create_corr_boxplot(mock_gct, "group", "test_ome", discrete_colors, "pearson", cor_matrix)
  expect_s3_class(result_discrete, "ggplot")
  
  # Skip continuous color map test for correlation boxplots - complex to test properly
  # The function expects specific color map structure that's difficult to mock
  
  # Test with NULL color map
  result_null <- create_corr_boxplot(mock_gct, "group", "test_ome", NULL, "pearson", cor_matrix)
  expect_s3_class(result_null, "ggplot")
})

test_that("create_PCA_plot creates valid ggplot objects", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Test basic PCA plot
  result_basic <- create_PCA_plot(mock_gct, "group", "test_ome", mock_colors, 1, 2)
  expect_s3_class(result_basic, "ggplot")
  
  # Test PCA plot with second variable
  result_dual <- create_PCA_plot(mock_gct, "group", "test_ome", mock_colors, 1, 2, 
                                 "batch", "color", "shape")
  expect_s3_class(result_dual, "ggplot")
  
  # Test PCA plot with different PC components
  result_different_pcs <- create_PCA_plot(mock_gct, "group", "test_ome", mock_colors, 2, 3)
  expect_s3_class(result_different_pcs, "ggplot")
})

test_that("create_PCA_plot handles different color map types", {
  mock_gct <- create_mock_gct()
  
  # Test with discrete color map
  discrete_colors <- list(
    is_discrete = TRUE,
    colors = list(group = c("red", "blue")),
    vals = c("A", "B")
  )
  result_discrete <- create_PCA_plot(mock_gct, "group", "test_ome", discrete_colors, 1, 2)
  expect_s3_class(result_discrete, "ggplot")
  
  # Skip continuous color map test for PCA plots - complex to test properly
  # The function expects specific color map structure that's difficult to mock
  
  # Test with NULL color map
  result_null <- create_PCA_plot(mock_gct, "group", "test_ome", NULL, 1, 2)
  expect_s3_class(result_null, "ggplot")
})

test_that("create_PCA_plot validates input parameters", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Test invalid PC inputs
  expect_error(
    create_PCA_plot(mock_gct, "group", "test_ome", mock_colors, NULL, 2),
    "PC1 and PC2 must be valid and non-empty"
  )
  
  expect_error(
    create_PCA_plot(mock_gct, "group", "test_ome", mock_colors, 1, NULL),
    "PC1 and PC2 must be valid and non-empty"
  )
  
  # Test equal PC1 and PC2
  expect_error(
    create_PCA_plot(mock_gct, "group", "test_ome", mock_colors, 1, 1),
    "PC1 and PC2 are equal"
  )
  
  # Test same display method for both variables
  expect_error(
    create_PCA_plot(mock_gct, "group", "test_ome", mock_colors, 1, 2, 
                   "batch", "color", "color"),
    "Both variables cannot use the same display method"
  )
  
  # Test same variable for both annotations
  expect_error(
    create_PCA_plot(mock_gct, "group", "test_ome", mock_colors, 1, 2, 
                   "group", "color", "shape"),
    "Second variable must be different from the first variable"
  )
})

test_that("pca_variance_explained calculates variance correctly", {
  mock_gct <- create_mock_gct()
  
  # Calculate PCA
  mat <- mock_gct@mat
  data.norm <- mat %>% data.frame() %>% drop_na() %>% t()
  data.norm <- data.norm[,apply(data.norm, 2, var, na.rm=TRUE) != 0]
  my_pca <- prcomp(data.norm, center=TRUE, scale=TRUE)
  
  # Test variance explained calculation
  result <- pca_variance_explained(my_pca, mock_gct@cdesc, components = 1:3)
  
  expect_true(is.list(result))
  expect_true("plot" %in% names(result))
  expect_true("table" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
  expect_true(is.data.frame(result$table))
})

test_that("create_PCA_reg creates valid ggplot objects", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Test PCA regression plot
  result <- create_PCA_reg(mock_gct, "group", "test_ome", mock_colors, components.max = 5)
  expect_s3_class(result, "ggplot")
})

test_that("dynamicHeightHMCorr calculates height correctly", {
  # Test with different numbers of entries
  height_10 <- dynamicHeightHMCorr(10)
  height_50 <- dynamicHeightHMCorr(50)
  height_100 <- dynamicHeightHMCorr(100)
  
  expect_true(is.numeric(height_10))
  expect_true(is.numeric(height_50))
  expect_true(is.numeric(height_100))
  expect_true(height_50 > height_10)
  expect_true(height_100 > height_50)
})

test_that("draw_corr_HM draws heatmap without errors", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Create heatmap
  hm_result <- create_corr_heatmap(mock_gct, "group", "test_ome", mock_colors, "pearson")
  
  # Test drawing function - should not throw errors
  expect_no_error(draw_corr_HM(hm_result$HM))
})

test_that("QC functions handle edge cases", {
  # Test with minimal data
  minimal_mat <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  rownames(minimal_mat) <- c("gene1", "gene2")
  colnames(minimal_mat) <- c("sample1", "sample2")
  
  minimal_cdesc <- data.frame(
    group = c("A", "B"),
    row.names = c("sample1", "sample2")
  )
  
  minimal_rdesc <- data.frame(
    gene_name = c("gene1", "gene2"),
    row.names = c("gene1", "gene2")
  )
  
  minimal_gct <- new("GCT",
                     mat = minimal_mat,
                     cdesc = minimal_cdesc,
                     rdesc = minimal_rdesc,
                     rid = c("gene1", "gene2"),
                     cid = c("sample1", "sample2")
  )
  
  mock_params <- create_mock_parameters()
  mock_colors <- create_mock_color_map()
  
  # Test that functions handle minimal data
  expect_s3_class(create_boxplot(minimal_gct, "group", "test_ome", mock_colors, mock_params, "org"), "ggplot")
  expect_s3_class(create_profile_plot(minimal_gct, "group", "test_ome", mock_colors, mock_params, "org"), "ggplot")
  
  # Test correlation functions with minimal data
  cor_result <- create_corr_heatmap(minimal_gct, "group", "test_ome", mock_colors, "pearson")
  expect_true(is.list(cor_result))
  
  # For correlation boxplot, we need at least 2 samples per group
  # Create a dataset with 2 samples per group for correlation testing
  corr_mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  rownames(corr_mat) <- c("gene1", "gene2")
  colnames(corr_mat) <- c("sample1", "sample2", "sample3")
  
  corr_cdesc <- data.frame(
    group = c("A", "A", "B"),  # A has 2 samples, B has 1
    row.names = c("sample1", "sample2", "sample3")
  )
  
  corr_rdesc <- data.frame(
    gene_name = c("gene1", "gene2"),
    row.names = c("gene1", "gene2")
  )
  
  corr_gct <- new("GCT",
                  mat = corr_mat,
                  cdesc = corr_cdesc,
                  rdesc = corr_rdesc,
                  rid = c("gene1", "gene2"),
                  cid = c("sample1", "sample2", "sample3")
  )
  
  cor_matrix <- cor(corr_gct@mat, use = "pairwise.complete.obs", method = "pearson")
  expect_warning(
    result <- create_corr_boxplot(corr_gct, "group", "test_ome", mock_colors, "pearson", cor_matrix),
    "Groups with only one sample cannot be correlated and will be excluded"
  )
  expect_s3_class(result, "ggplot")
  
  # Test PCA with minimal data
  expect_s3_class(create_PCA_plot(minimal_gct, "group", "test_ome", mock_colors, 1, 2), "ggplot")
})

test_that("create_corr_boxplot handles single-sample groups", {
  # Test with mixed groups (some single-sample, some multi-sample)
  mixed_mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  rownames(mixed_mat) <- paste0("gene_", 1:4)
  colnames(mixed_mat) <- paste0("sample_", 1:5)
  
  mixed_cdesc <- data.frame(
    group = c("A", "A", "B", "C", "D"),  # A has 2 samples, B/C/D have 1 each
    row.names = paste0("sample_", 1:5)
  )
  
  mixed_rdesc <- data.frame(
    gene_name = paste0("gene_", 1:4),
    row.names = paste0("gene_", 1:4)
  )
  
  mixed_gct <- new("GCT",
                   mat = mixed_mat,
                   cdesc = mixed_cdesc,
                   rdesc = mixed_rdesc,
                   rid = paste0("gene_", 1:4),
                   cid = paste0("sample_", 1:5)
  )
  
  cor_matrix <- cor(mixed_gct@mat, use = "pairwise.complete.obs", method = "pearson")
  
  # Should work with warning about excluded groups
  expect_warning(
    result <- create_corr_boxplot(mixed_gct, "group", "test_ome", NULL, "pearson", cor_matrix),
    "Groups with only one sample cannot be correlated and will be excluded"
  )
  expect_s3_class(result, "ggplot")
  
  # Test with all single-sample groups
  single_mat <- matrix(rnorm(12), nrow = 3, ncol = 4)
  rownames(single_mat) <- paste0("gene_", 1:3)
  colnames(single_mat) <- paste0("sample_", 1:4)
  
  single_cdesc <- data.frame(
    group = c("A", "B", "C", "D"),  # All groups have only 1 sample
    row.names = paste0("sample_", 1:4)
  )
  
  single_rdesc <- data.frame(
    gene_name = paste0("gene_", 1:3),
    row.names = paste0("gene_", 1:3)
  )
  
  single_gct <- new("GCT",
                    mat = single_mat,
                    cdesc = single_cdesc,
                    rdesc = single_rdesc,
                    rid = paste0("gene_", 1:3),
                    cid = paste0("sample_", 1:4)
  )
  
  single_cor_matrix <- cor(single_gct@mat, use = "pairwise.complete.obs", method = "pearson")
  
  # Should stop with error
  expect_error(
    create_corr_boxplot(single_gct, "group", "test_ome", NULL, "pearson", single_cor_matrix),
    "No groups have more than one sample. Cannot calculate intra-group correlations."
  )
})
