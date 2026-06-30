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
    id = paste0("gene_", 1:10),
    geneSymbol = paste0("SYMBOL_", 1:10),
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
  
  # Test with continuous color map
  # Use a variable with many unique values (>20) to ensure it's treated as continuous
  continuous_colors <- list(
    is_discrete = FALSE,
    colors = NULL  # Will use default colors
  )
  # Create continuous annotation with many unique values (30 values > 20 cutoff)
  mock_gct_continuous <- create_mock_gct()
  # Expand to 30 samples for continuous variable
  mock_mat_continuous <- matrix(rnorm(300), nrow = 10, ncol = 30)
  rownames(mock_mat_continuous) <- paste0("gene_", 1:10)
  colnames(mock_mat_continuous) <- paste0("sample_", 1:30)
  mock_cdesc_continuous <- data.frame(
    group = rep(c("A", "B"), each = 15),
    expression = seq(1, 30, by = 1),  # 30 unique values - will be continuous
    row.names = paste0("sample_", 1:30)
  )
  mock_gct_continuous@mat <- mock_mat_continuous
  mock_gct_continuous@cdesc <- mock_cdesc_continuous
  mock_gct_continuous@cid <- paste0("sample_", 1:30)
  
  result_continuous <- create_boxplot(mock_gct_continuous, "expression", "test_ome", continuous_colors, mock_params, "org")
  expect_s3_class(result_continuous, "ggplot")
  
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
  
  # Test with continuous color map
  # Use a variable with many unique values (>20) to ensure it's treated as continuous
  continuous_colors <- list(
    is_discrete = FALSE,
    colors = NULL  # Will use default colors
  )
  # Create continuous annotation with many unique values (30 values > 20 cutoff)
  mock_gct_continuous <- create_mock_gct()
  # Expand to 30 samples for continuous variable
  mock_mat_continuous <- matrix(rnorm(300), nrow = 10, ncol = 30)
  rownames(mock_mat_continuous) <- paste0("gene_", 1:10)
  colnames(mock_mat_continuous) <- paste0("sample_", 1:30)
  mock_cdesc_continuous <- data.frame(
    group = rep(c("A", "B"), each = 15),
    expression = seq(1, 30, by = 1),  # 30 unique values - will be continuous
    row.names = paste0("sample_", 1:30)
  )
  mock_gct_continuous@mat <- mock_mat_continuous
  mock_gct_continuous@cdesc <- mock_cdesc_continuous
  mock_gct_continuous@cid <- paste0("sample_", 1:30)
  
  result_continuous <- create_profile_plot(mock_gct_continuous, "expression", "test_ome", continuous_colors, mock_params, "org")
  expect_s3_class(result_continuous, "ggplot")
  
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
  expect_true("tooltip" %in% names(result$plot$data))
  expect_match(result$plot$data$tooltip[1], "^PC: PC")
  expect_match(result$plot$data$tooltip[1], "% variance explained:")
})

test_that("create_PCA_reg creates valid ggplot objects", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Test PCA regression plot
  result <- create_PCA_reg(mock_gct, "group", "test_ome", mock_colors, components.max = 5)
  expect_s3_class(result, "ggplot")
})

test_that("create_PCA_reg handles fewer than 10 PCs correctly", {
  # Create a GCT with only 3 samples (max 3 PCs possible)
  small_mat <- matrix(rnorm(15), nrow = 5, ncol = 3)
  rownames(small_mat) <- paste0("gene_", 1:5)
  colnames(small_mat) <- paste0("sample_", 1:3)
  
  small_cdesc <- data.frame(
    group = c("A", "B", "A"),
    row.names = paste0("sample_", 1:3)
  )
  
  small_rdesc <- data.frame(
    gene_name = paste0("gene_", 1:5),
    row.names = paste0("gene_", 1:5)
  )
  
  small_gct <- new("GCT",
                   mat = small_mat,
                   cdesc = small_cdesc,
                   rdesc = small_rdesc,
                   rid = paste0("gene_", 1:5),
                   cid = paste0("sample_", 1:3)
  )
  
  mock_colors <- create_mock_color_map()
  
  # Should work with default components.max=10, but only use available PCs (3)
  result <- create_PCA_reg(small_gct, "group", "test_ome", mock_colors)
  expect_s3_class(result, "ggplot")
  
  # Should also work with explicit components.max > available PCs
  result2 <- create_PCA_reg(small_gct, "group", "test_ome", mock_colors, components.max = 10)
  expect_s3_class(result2, "ggplot")
  
  # Should work with components.max less than available PCs
  result3 <- create_PCA_reg(small_gct, "group", "test_ome", mock_colors, components.max = 2)
  expect_s3_class(result3, "ggplot")
})

test_that("create_PCA_reg handles edge case with minimal PCs", {
  # Create a GCT with only 2 samples (max 2 PCs possible, but typically only 1 is meaningful)
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
  
  mock_colors <- create_mock_color_map()
  
  # Should work even with only 1-2 PCs available
  result <- create_PCA_reg(minimal_gct, "group", "test_ome", mock_colors)
  expect_s3_class(result, "ggplot")
})

test_that("pca_variance_explained handles fewer than 10 PCs correctly", {
  # Create a PCA with only 3 samples (max 3 PCs)
  small_mat <- matrix(rnorm(15), nrow = 5, ncol = 3)
  rownames(small_mat) <- paste0("gene_", 1:5)
  colnames(small_mat) <- paste0("sample_", 1:3)
  
  # Calculate PCA
  data.norm <- small_mat %>% data.frame() %>% drop_na() %>% t()
  data.norm <- data.norm[,apply(data.norm, 2, var, na.rm=TRUE) != 0]
  my_pca <- prcomp(data.norm, center=TRUE, scale=TRUE)
  
  # Create cdesc
  small_cdesc <- data.frame(
    group = c("A", "B", "A"),
    row.names = paste0("sample_", 1:3)
  )
  
  # Test with default components (1:10) - should automatically adjust to available PCs
  result_default <- pca_variance_explained(my_pca, small_cdesc)
  expect_true(is.list(result_default))
  expect_true("plot" %in% names(result_default))
  expect_true("table" %in% names(result_default))
  expect_s3_class(result_default$plot, "ggplot")
  
  # Test with explicit components that exceed available PCs
  result_explicit <- pca_variance_explained(my_pca, small_cdesc, components = 1:10)
  expect_true(is.list(result_explicit))
  expect_s3_class(result_explicit$plot, "ggplot")
  
  # Test with components that are within available PCs
  result_within <- pca_variance_explained(my_pca, small_cdesc, components = 1:2)
  expect_true(is.list(result_within))
  expect_s3_class(result_within$plot, "ggplot")
})

test_that("pca_variance_explained handles edge case with minimal PCs", {
  # Create a PCA with only 2 samples (max 2 PCs, but typically only 1 meaningful)
  minimal_mat <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  rownames(minimal_mat) <- c("gene1", "gene2")
  colnames(minimal_mat) <- c("sample1", "sample2")
  
  # Calculate PCA
  data.norm <- minimal_mat %>% data.frame() %>% drop_na() %>% t()
  data.norm <- data.norm[,apply(data.norm, 2, var, na.rm=TRUE) != 0]
  my_pca <- prcomp(data.norm, center=TRUE, scale=TRUE)
  
  # Create cdesc
  minimal_cdesc <- data.frame(
    group = c("A", "B"),
    row.names = c("sample1", "sample2")
  )
  
  # Should work with default components, adjusting to available PCs
  result <- pca_variance_explained(my_pca, minimal_cdesc)
  expect_true(is.list(result))
  expect_s3_class(result$plot, "ggplot")
  
  # Should work with explicit components within range
  result2 <- pca_variance_explained(my_pca, minimal_cdesc, components = 1)
  expect_true(is.list(result2))
  expect_s3_class(result2$plot, "ggplot")
})

test_that("create_PCA_reg handles datasets with 10 or more PCs", {
  # Create a GCT with 15 samples (max 15 PCs possible)
  large_mat <- matrix(rnorm(150), nrow = 10, ncol = 15)
  rownames(large_mat) <- paste0("gene_", 1:10)
  colnames(large_mat) <- paste0("sample_", 1:15)
  
  large_cdesc <- data.frame(
    group = rep(c("A", "B", "C"), each = 5),
    row.names = paste0("sample_", 1:15)
  )
  
  large_rdesc <- data.frame(
    gene_name = paste0("gene_", 1:10),
    row.names = paste0("gene_", 1:10)
  )
  
  large_gct <- new("GCT",
                   mat = large_mat,
                   cdesc = large_cdesc,
                   rdesc = large_rdesc,
                   rid = paste0("gene_", 1:10),
                   cid = paste0("sample_", 1:15)
  )
  
  mock_colors <- create_mock_color_map()
  
  # Should work with default components.max=10 (using first 10 of 15 available)
  result <- create_PCA_reg(large_gct, "group", "test_ome", mock_colors)
  expect_s3_class(result, "ggplot")
  
  # Should work with components.max less than available
  result2 <- create_PCA_reg(large_gct, "group", "test_ome", mock_colors, components.max = 5)
  expect_s3_class(result2, "ggplot")
})

test_that("calculate_PCA function works correctly", {
  mock_gct <- create_mock_gct()
  
  # Test that calculate_PCA returns expected structure
  pca_result <- calculate_PCA(mock_gct)
  
  expect_true(is.list(pca_result))
  expect_true("pca" %in% names(pca_result))
  expect_true("data_norm" %in% names(pca_result))
  expect_true("original_colnames" %in% names(pca_result))
  expect_true("n_features" %in% names(pca_result))
  expect_true("n_features_total" %in% names(pca_result))
  expect_equal(pca_result$n_features, ncol(pca_result$data_norm))
  expect_equal(pca_result$n_features_total, nrow(mock_gct@mat))
  
  # Test that PCA object is valid
  expect_true(inherits(pca_result$pca, "prcomp"))
  expect_true(is.matrix(pca_result$data_norm))
  expect_true(is.character(pca_result$original_colnames))
  
  # Test that PCA has expected components
  expect_true("x" %in% names(pca_result$pca))
  expect_true("sdev" %in% names(pca_result$pca))
  expect_true(nrow(pca_result$pca$x) == length(pca_result$original_colnames))
})

test_that("ggplotly_with_gg_subtitle preserves ggplot subtitle", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  pca_result <- calculate_PCA(mock_gct)

  gg <- create_PCA_plot(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    comp.x = 1,
    comp.y = 2,
    pca_result = pca_result
  )

  ply <- ggplotly_with_gg_subtitle(gg, tooltip = "text")
  expect_true(grepl("features used", ply$x$layout$title$text, fixed = TRUE))
  expect_true(grepl(gg$labels$title, ply$x$layout$title$text, fixed = TRUE))
})

test_that("create_PCA_plot shows feature count as subtitle", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  pca_result <- calculate_PCA(mock_gct)

  result <- create_PCA_plot(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    comp.x = 1,
    comp.y = 2,
    pca_result = pca_result
  )

  expect_equal(
    result$labels$subtitle,
    paste0(
      format(pca_result$n_features, big.mark = ","), "/",
      format(pca_result$n_features_total, big.mark = ","),
      " features used"
    )
  )
})

test_that("PCA loadings helpers return expected structure", {
  mock_gct <- create_mock_gct()
  pca_result <- calculate_PCA(mock_gct)

  loadings_df <- get_pca_loadings_df(pca_result, gct = mock_gct)
  expect_equal(nrow(loadings_df), pca_result$n_features)
  expect_true("feature" %in% names(loadings_df))
  expect_true(all(paste0("PC", seq_len(min(10L, ncol(pca_result$pca$rotation)))) %in% names(loadings_df)))
  expect_equal(loadings_df$id, paste0("gene_", 1:10))
  expect_equal(loadings_df$geneSymbol, paste0("SYMBOL_", 1:10))

  n_rank_pcs <- min(10L, ncol(pca_result$pca$rotation))
  cum_col <- paste0("cumulative_loading_PC1_", n_rank_pcs)
  export_df <- get_pca_loadings_df(pca_result, gct = mock_gct, for_export = TRUE)
  expect_equal(
    names(export_df)[1:4],
    c("rank", cum_col, "id", "geneSymbol")
  )
  expect_false("feature" %in% names(export_df))
  expect_equal(export_df$rank, seq_len(nrow(export_df)))
  expect_true(all(diff(export_df[[cum_col]]) <= 1e-10))
  pc_cols <- grep("^PC\\d+$", names(export_df), value = TRUE)
  expect_length(pc_cols, ncol(pca_result$pca$rotation))

  top_global <- top_pca_loading_features(loadings_df, topn = 5, max_pcs = 10L)
  expect_length(top_global, 5)
  expect_equal(top_global, export_df$id[seq_len(5)])

  cumplot <- create_PCA_loadings_cumulative(pca_result, ome = "test_ome", gct = mock_gct)
  expect_s3_class(cumplot, "ggplot")
  expect_equal(length(unique(cumplot$data$feature)), 10)
  legend_lvls <- levels(cumplot$data$legend_label)
  expect_length(legend_lvls, 10)
  expect_equal(as.integer(sub(":.*$", "", legend_lvls)), seq_len(10))
  plot_display <- sub("^\\d{2}: ", "", legend_lvls)
  expected_display <- ifelse(
    !is.na(export_df$geneSymbol[seq_len(10)]) & nzchar(export_df$geneSymbol[seq_len(10)]),
    export_df$geneSymbol[seq_len(10)],
    export_df$id[seq_len(10)]
  )
  expect_equal(plot_display, expected_display)
  n_pc_plot <- min(10L, ncol(pca_result$pca$rotation))
  top_features <- top_pca_loading_features(loadings_df, topn = 10L, max_pcs = n_pc_plot)
  plot_cum_at_pc10 <- cumplot$data[
    cumplot$data$PC == n_pc_plot & cumplot$data$feature %in% top_features,
    c("feature", "cumulative")
  ]
  plot_cum_at_pc10 <- plot_cum_at_pc10[match(top_features, plot_cum_at_pc10$feature), ]
  expect_equal(
    plot_cum_at_pc10$cumulative,
    export_df[[cum_col]][seq_len(10)],
    tolerance = 1e-10
  )

  minimal_mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  rownames(minimal_mat) <- c("gene1", "gene2", "gene3")
  colnames(minimal_mat) <- c("sample1", "sample2")
  minimal_gct <- new("GCT",
    mat = minimal_mat,
    cdesc = data.frame(group = c("A", "B"), row.names = colnames(minimal_mat)),
    rdesc = data.frame(id = rownames(minimal_mat), row.names = rownames(minimal_mat)),
    rid = rownames(minimal_mat),
    cid = colnames(minimal_mat)
  )
  minimal_pca <- calculate_PCA(minimal_gct)
  minimal_export <- get_pca_loadings_df(minimal_pca, gct = minimal_gct, for_export = TRUE)
  n_min_pcs <- ncol(minimal_pca$pca$rotation)
  expect_equal(names(minimal_export)[2], paste0("cumulative_loading_PC1_", n_min_pcs))
  expect_true(all(grepl("^\\d{2}: ", legend_lvls)))
  expect_true(all(cumplot$data$cumulative >= 0))
  n_pc_all <- ncol(pca_result$pca$rotation)
  n_pc_plot <- min(10L, n_pc_all)
  final_vals <- cumplot$data[cumplot$data$PC == n_pc_plot, "cumulative"]
  if (n_pc_all <= n_pc_plot) {
    expect_true(all(abs(final_vals - 1) < 1e-10))
  } else {
    expect_true(all(final_vals < 1))
  }

})

test_that("create_PCA_plot works with pre-calculated PCA", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Pre-calculate PCA
  pca_result <- calculate_PCA(mock_gct)
  
  # Test that plot works with pre-calculated PCA
  result_with_pca <- create_PCA_plot(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    comp.x = 1,
    comp.y = 2,
    pca_result = pca_result
  )
  
  expect_s3_class(result_with_pca, "ggplot")
  
  # Test that plot works without pre-calculated PCA (backward compatibility)
  result_without_pca <- create_PCA_plot(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    comp.x = 1,
    comp.y = 2
  )
  
  expect_s3_class(result_without_pca, "ggplot")
  
  # Test that both produce the same PCA coordinates (data should be identical)
  # Extract PCA coordinates from both plots
  plot_data_with <- result_with_pca$data
  plot_data_without <- result_without_pca$data
  
  # PCA coordinates should be the same (allowing for small numerical differences)
  expect_equal(plot_data_with$PC1, plot_data_without$PC1, tolerance = 1e-10)
  expect_equal(plot_data_with$PC2, plot_data_without$PC2, tolerance = 1e-10)
})

test_that("create_PCA_reg works with pre-calculated PCA", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Pre-calculate PCA
  pca_result <- calculate_PCA(mock_gct)
  
  # Test that regression plot works with pre-calculated PCA
  result_with_pca <- create_PCA_reg(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    pca_result = pca_result
  )
  
  expect_s3_class(result_with_pca, "ggplot")
  
  # Test that regression plot works without pre-calculated PCA (backward compatibility)
  result_without_pca <- create_PCA_reg(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors
  )
  
  expect_s3_class(result_without_pca, "ggplot")
  
  # Both should produce valid plots (we can't easily compare exact values due to 
  # variance explained calculations, but both should work)
  expect_true(inherits(result_with_pca, "ggplot"))
  expect_true(inherits(result_without_pca, "ggplot"))
})

test_that("PCA caching produces consistent results across multiple calls", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Pre-calculate PCA once
  pca_result <- calculate_PCA(mock_gct)
  
  # Create multiple plots with different visualization parameters using same PCA
  plot1 <- create_PCA_plot(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    comp.x = 1,
    comp.y = 2,
    pca_result = pca_result
  )
  
  plot2 <- create_PCA_plot(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    comp.x = 2,
    comp.y = 3,
    pca_result = pca_result
  )
  
  # Both should use the same underlying PCA coordinates
  plot1_data <- plot1$data
  plot2_data <- plot2$data
  
  # PC1 and PC2 should be identical in both plots (same PCA, different visualization)
  expect_equal(plot1_data$PC1, plot2_data$PC1, tolerance = 1e-10)
  expect_equal(plot1_data$PC2, plot2_data$PC2, tolerance = 1e-10)
  
  # PC3 should be available in plot2
  expect_true("PC3" %in% names(plot2_data))
})

test_that("calculate_PCA handles edge cases correctly", {
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
  
  # Should work with minimal data
  pca_result <- calculate_PCA(minimal_gct)
  expect_true(is.list(pca_result))
  expect_true(inherits(pca_result$pca, "prcomp"))
  
  # Test error handling with invalid data (all NA)
  na_mat <- matrix(NA, nrow = 2, ncol = 2)
  rownames(na_mat) <- c("gene1", "gene2")
  colnames(na_mat) <- c("sample1", "sample2")
  
  na_cdesc <- data.frame(
    group = c("A", "B"),
    row.names = c("sample1", "sample2")
  )
  
  na_rdesc <- data.frame(
    gene_name = c("gene1", "gene2"),
    row.names = c("gene1", "gene2")
  )
  
  na_gct <- new("GCT",
                mat = na_mat,
                cdesc = na_cdesc,
                rdesc = na_rdesc,
                rid = c("gene1", "gene2"),
                cid = c("sample1", "sample2")
  )
  
  # Should error appropriately: all-NA features leave 0 usable features, which
  # trips the <2-features guard added by the PCA-guard fix.
  expect_error(calculate_PCA(na_gct), "requires at least 2 features")

  # Single-sample dataset: PCA is undefined. Must fail with a clear message,
  # NOT the cryptic "argument is of length zero" from a dropped matrix dimension.
  single_mat <- matrix(c(1, 2, 3, 4, 5), nrow = 5, ncol = 1)
  rownames(single_mat) <- paste0("gene", 1:5)
  colnames(single_mat) <- "sample1"

  single_gct <- new("GCT",
                     mat = single_mat,
                     cdesc = data.frame(group = "A", row.names = "sample1"),
                     rdesc = data.frame(gene_name = paste0("gene", 1:5),
                                        row.names = paste0("gene", 1:5)),
                     rid = paste0("gene", 1:5),
                     cid = "sample1"
  )

  expect_error(calculate_PCA(single_gct), "at least 2 samples")
})

test_that("create_PCA_plot accepts fill_shapes parameter", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Add a second annotation column for shape testing
  mock_gct@cdesc$batch <- rep(c("batch1", "batch2"), 4)
  
  # Test with fill_shapes = FALSE (default, hollow shapes)
  plot_hollow <- create_PCA_plot(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    comp.x = 1,
    comp.y = 2,
    second_col_of_interest = "batch",
    var1_display = "color",
    var2_display = "shape",
    fill_shapes = FALSE
  )
  
  expect_s3_class(plot_hollow, "ggplot")
  
  # Test with fill_shapes = TRUE (filled shapes)
  plot_filled <- create_PCA_plot(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    comp.x = 1,
    comp.y = 2,
    second_col_of_interest = "batch",
    var1_display = "color",
    var2_display = "shape",
    fill_shapes = TRUE
  )
  
  expect_s3_class(plot_filled, "ggplot")
  
  # Both plots should be valid ggplot objects
  expect_true(inherits(plot_hollow, "ggplot"))
  expect_true(inherits(plot_filled, "ggplot"))
})

test_that("create_PCA_plot fill_shapes works with multiple shape categories", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  # Create GCT with 6 different batch categories to test shape cycling
  mock_gct@cdesc$batch <- rep(c("batch1", "batch2", "batch3", "batch4", "batch5", "batch6"), length.out = 8)
  
  # Test with fill_shapes = TRUE and 6 categories
  plot_filled <- create_PCA_plot(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    comp.x = 1,
    comp.y = 2,
    second_col_of_interest = "batch",
    var1_display = "color",
    var2_display = "shape",
    fill_shapes = TRUE
  )
  
  expect_s3_class(plot_filled, "ggplot")
  
  # Plot should be created successfully with filled shapes for 6 categories
  expect_true(inherits(plot_filled, "ggplot"))
})

test_that("create_PCA_plot fill_shapes defaults to FALSE", {
  mock_gct <- create_mock_gct()
  mock_colors <- create_mock_color_map()
  
  mock_gct@cdesc$batch <- rep(c("batch1", "batch2"), 4)
  
  # Test without specifying fill_shapes (should default to FALSE)
  plot_default <- create_PCA_plot(
    gct = mock_gct,
    col_of_interest = "group",
    ome = "test_ome",
    custom_color_map = mock_colors,
    comp.x = 1,
    comp.y = 2,
    second_col_of_interest = "batch",
    var1_display = "color",
    var2_display = "shape"
  )
  
  expect_s3_class(plot_default, "ggplot")
  
  # Plot should be created successfully with default (hollow) shapes
  expect_true(inherits(plot_default, "ggplot"))
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

test_that("create_PCA_plot handles hyphens in group names", {
  # Test PCA plot with group names containing hyphens (e.g., "Non-inflamed")
  # This tests the fix for preserving original column names and annotation values
  mock_mat <- matrix(rnorm(40), nrow = 5, ncol = 8)
  rownames(mock_mat) <- paste0("gene_", 1:5)
  colnames(mock_mat) <- paste0("sample_", 1:8)
  
  mock_cdesc <- data.frame(
    group = c(rep("Inflamed", 4), rep("Non-inflamed", 4)),  # Group with hyphen
    row.names = paste0("sample_", 1:8)
  )
  
  mock_rdesc <- data.frame(
    gene_name = paste0("gene_", 1:5),
    row.names = paste0("gene_", 1:5)
  )
  
  mock_gct <- new("GCT",
                  mat = mock_mat,
                  cdesc = mock_cdesc,
                  rdesc = mock_rdesc,
                  rid = paste0("gene_", 1:5),
                  cid = paste0("sample_", 1:8)
  )
  
  mock_colors <- list(
    is_discrete = TRUE,
    colors = list(group = c("red", "blue")),
    vals = c("Inflamed", "Non-inflamed")
  )
  
  # Should not error and should preserve hyphen in group name
  result <- create_PCA_plot(mock_gct, "group", "test_ome", mock_colors, 1, 2)
  expect_s3_class(result, "ggplot")
  
  # Check that the plot data contains the hyphenated group name
  plot_data <- result$data
  # The column is named after col_of_interest ("group")
  annot_col <- plot_data$group
  expect_true("Non-inflamed" %in% as.character(annot_col) || "Non-inflamed" %in% levels(annot_col))
})

test_that("create_PCA_plot uses alphabetical legend levels for discrete annotations", {
  mock_gct <- create_mock_gct()
  # Deliberately non-alphabetical order in data; legend should still be alphabetical.
  mock_gct@cdesc$group <- c("zeta", "beta", "alpha", "beta", "zeta", "alpha", "beta", "alpha")

  mock_colors <- list(
    is_discrete = TRUE,
    colors = list(group = c("red", "green", "blue")),
    vals = c("alpha", "beta", "zeta")
  )

  result <- create_PCA_plot(mock_gct, "group", "test_ome", mock_colors, 1, 2)
  expect_s3_class(result, "ggplot")
  expect_identical(levels(result$data$group), c("alpha", "beta", "zeta"))
})

test_that("create_PCA_plot alphabetizes second variable legend levels", {
  mock_gct <- create_mock_gct()
  mock_gct@cdesc$group <- c("beta", "alpha", "beta", "alpha", "beta", "alpha", "beta", "alpha")
  mock_gct@cdesc$batch <- c("z2", "z1", "z3", "z1", "z2", "z3", "z1", "z2")

  mock_colors <- list(
    is_discrete = TRUE,
    colors = list(group = c("red", "blue")),
    vals = c("alpha", "beta")
  )

  result <- create_PCA_plot(
    mock_gct, "group", "test_ome", mock_colors, 1, 2,
    second_col_of_interest = "batch", var1_display = "color", var2_display = "shape"
  )
  expect_s3_class(result, "ggplot")
  expect_identical(levels(result$data$group), c("alpha", "beta"))
  expect_identical(levels(result$data$batch), c("z1", "z2", "z3"))
})

test_that("create_PCA_plot handles continuous color mapping", {
  # Test PCA plot with continuous color mapping
  # Use a variable with many unique values (>20) to ensure it's treated as continuous
  mock_mat <- matrix(rnorm(300), nrow = 10, ncol = 30)
  rownames(mock_mat) <- paste0("gene_", 1:10)
  colnames(mock_mat) <- paste0("sample_", 1:30)
  
  # Create continuous annotation with many unique values (e.g., expression level, intensity)
  # Using 30 unique values ensures it's classified as continuous (cutoff is 20)
  mock_cdesc <- data.frame(
    expression = seq(1, 30, by = 1),  # 30 unique values - will be continuous
    row.names = paste0("sample_", 1:30)
  )
  
  mock_rdesc <- data.frame(
    gene_name = paste0("gene_", 1:10),
    row.names = paste0("gene_", 1:10)
  )
  
  mock_gct <- new("GCT",
                  mat = mock_mat,
                  cdesc = mock_cdesc,
                  rdesc = mock_rdesc,
                  rid = paste0("gene_", 1:10),
                  cid = paste0("sample_", 1:30)
  )
  
  # Continuous color map with default colors (when colors is NULL)
  continuous_colors <- list(
    is_discrete = FALSE,
    colors = NULL  # Will use default colors
  )
  
  # Should not error and should keep expression as numeric (not convert to factor)
  result <- create_PCA_plot(mock_gct, "expression", "test_ome", continuous_colors, 1, 2)
  expect_s3_class(result, "ggplot")
  
  # Check that the plot data contains numeric expression values
  plot_data <- result$data
  expect_true(is.numeric(plot_data$expression))
  
  # Test with explicit continuous color map
  continuous_colors_explicit <- list(
    is_discrete = FALSE,
    colors = c("blue", "white", "red", "gray50"),
    vals = c("low", "mid", "high", "na_color")
  )
  
  result2 <- create_PCA_plot(mock_gct, "expression", "test_ome", continuous_colors_explicit, 1, 2)
  expect_s3_class(result2, "ggplot")
  expect_true(is.numeric(result2$data$expression))
})
