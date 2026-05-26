################################################################################
# Tests for PCA loadings helpers in R/tab_qc_PCA_helpers.R
################################################################################

make_loadings_df <- function() {
  data.frame(
    PC1 = c(0.9, 0.1, 0.5),
    PC2 = c(0.1, 0.9, 0.5),
    PC3 = c(0.0, 0.0, 0.5),
    feature = c("f1", "f2", "f3"),
    id = c("id_b", "id_a", "id_c"),
    geneSymbol = c("SYM_B", "SYM_A", NA_character_),
    stringsAsFactors = FALSE
  )
}

make_pca_result <- function(rotation, n_features_total = NULL) {
  if (is.null(n_features_total)) {
    n_features_total <- nrow(rotation)
  }
  list(
    pca = structure(
      list(rotation = rotation),
      class = "prcomp"
    ),
    n_features = nrow(rotation),
    n_features_total = n_features_total
  )
}

# --------------------------------------------------------------------------- #
# Small utilities                                                              #
# --------------------------------------------------------------------------- #

test_that("pca_rank_pcs_used caps at available PCs and max_pcs", {
  expect_equal(pca_rank_pcs_used(15L), 10L)
  expect_equal(pca_rank_pcs_used(5L), 5L)
  expect_equal(pca_rank_pcs_used(15L, max_pcs = 3L), 3L)
})

test_that("pca_cumulative_loading_column_name reflects PC count", {
  expect_equal(pca_cumulative_loading_column_name(10L), "cumulative_loading_PC1_10")
  expect_equal(pca_cumulative_loading_column_name(2L), "cumulative_loading_PC1_2")
})

test_that("pca_feature_display_label prefers geneSymbol when present", {
  expect_equal(pca_feature_display_label("id1", "GENE1"), "GENE1")
  expect_equal(pca_feature_display_label("id1", NA_character_), "id1")
  expect_equal(pca_feature_display_label("id1", ""), "id1")
})

# --------------------------------------------------------------------------- #
# Cumulative loading fraction (hand-computed)                                    #
# --------------------------------------------------------------------------- #

test_that("pca_cumulative_loading_fraction matches hand-computed values", {
  df <- make_loadings_df()
  # f1: sq = 0.81 + 0.01 + 0 = 0.82; through PC1 = 0.81/0.82
  expect_equal(
    pca_cumulative_loading_fraction(df, through_pc = 1L)[1],
    0.81 / 0.82,
    tolerance = 1e-10
  )
  expect_equal(
    pca_cumulative_loading_fraction(df, through_pc = 2L)[1],
    1,
    tolerance = 1e-10
  )
  # f3: equal loadings on 3 PCs -> 1/3 through PC1, 1 through PC3
  expect_equal(
    pca_cumulative_loading_fraction(df, through_pc = 1L)[3],
    1 / 3,
    tolerance = 1e-10
  )
  expect_equal(
    pca_cumulative_loading_fraction(df, through_pc = 3L)[3],
    1,
    tolerance = 1e-10
  )
})

test_that("pca_loading_sq_matrix squares PC columns only", {
  df <- make_loadings_df()
  sq <- pca_loading_sq_matrix(df)
  expect_equal(ncol(sq), 3L)
  expect_equal(nrow(sq), 3L)
  expect_equal(colnames(sq), c("PC1", "PC2", "PC3"))
  expect_equal(unname(sq[1L, "PC1"]), 0.81, tolerance = 1e-10)
})

# --------------------------------------------------------------------------- #
# Ranking                                                                      #
# --------------------------------------------------------------------------- #

test_that("pca_rank_features_by_pc_loading orders by cumulative fraction", {
  df <- make_loadings_df()
  ranked <- pca_rank_features_by_pc_loading(df, through_pc = 1L)
  expect_equal(ranked$feature, c("f1", "f3", "f2"))
  expect_true(all(diff(ranked$cumulative_loading_PC1_1) <= 1e-10))
})

test_that("pca_rank_features_by_pc_loading breaks ties by max |loading| then id", {
  tie_df <- data.frame(
    PC1 = c(0.8, 0.6),
    PC2 = c(0.2, 0.6),
    feature = c("fa", "fb"),
    id = c("id_z", "id_y"),
    stringsAsFactors = FALSE
  )
  ranked <- pca_rank_features_by_pc_loading(tie_df, through_pc = 2L)
  expect_equal(ranked$feature, c("fa", "fb"))
  expect_equal(ranked$cumulative_loading_PC1_2, c(1, 1))
})

test_that("top_pca_loading_features returns requested top N in rank order", {
  df <- make_loadings_df()
  top2 <- top_pca_loading_features(df, topn = 2L, max_pcs = 1L)
  expect_length(top2, 2L)
  expect_equal(top2, c("f1", "f3"))
})

# --------------------------------------------------------------------------- #
# get_pca_loadings_df                                                          #
# --------------------------------------------------------------------------- #

test_that("get_pca_loadings_df non-export keeps feature and respects max_pcs", {
  rotation <- matrix(
    c(0.7, 0.1, 0.2, 0.3),
    nrow = 2L,
    ncol = 2L,
    dimnames = list(c("feat1", "feat2"), c("PC1", "PC2"))
  )
  pca_result <- make_pca_result(rotation)
  gct <- new("GCT",
    mat = matrix(1:4, nrow = 2L, ncol = 2L),
    cdesc = data.frame(group = c("A", "B"), row.names = c("s1", "s2")),
    rdesc = data.frame(
      id = c("ID1", "ID2"),
      geneSymbol = c("G1", "G2"),
      row.names = c("feat1", "feat2")
    ),
    rid = c("feat1", "feat2"),
    cid = c("s1", "s2")
  )

  out <- get_pca_loadings_df(pca_result, gct = gct, max_pcs = 1L)
  expect_true("feature" %in% names(out))
  expect_equal(names(out)[grep("^PC", names(out))], "PC1")
  expect_equal(out$id, c("ID1", "ID2"))
  expect_equal(out$geneSymbol, c("G1", "G2"))
})

test_that("get_pca_loadings_df export is sorted and uses dynamic cumulative column", {
  rotation <- matrix(
    c(0.9, 0.1, 0.1, 0.9, 0.5, 0.5),
    nrow = 3L,
    ncol = 2L,
    dimnames = list(c("f1", "f2", "f3"), c("PC1", "PC2"))
  )
  pca_result <- make_pca_result(rotation)
  gct <- new("GCT",
    mat = matrix(1:6, nrow = 3L, ncol = 2L),
    cdesc = data.frame(group = c("A", "B"), row.names = c("s1", "s2")),
    rdesc = data.frame(id = c("id1", "id2", "id3"), row.names = c("f1", "f2", "f3")),
    rid = c("f1", "f2", "f3"),
    cid = c("s1", "s2")
  )

  export_df <- get_pca_loadings_df(pca_result, gct = gct, for_export = TRUE)
  expect_equal(names(export_df)[1:2], c("rank", "cumulative_loading_PC1_2"))
  expect_equal(export_df$rank, seq_len(nrow(export_df)))
  expect_true(all(diff(export_df$cumulative_loading_PC1_2) <= 1e-10))
  expect_false("feature" %in% names(export_df))
  expect_equal(ncol(export_df), 2L + 2L + ncol(rotation)) # rank, cum, id, geneSymbol, PCs
})

# --------------------------------------------------------------------------- #
# Cumulative loadings plot                                                     #
# --------------------------------------------------------------------------- #

test_that("create_PCA_loadings_cumulative legend uses geneSymbol with id fallback", {
  mat <- matrix(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    nrow = 5L,
    ncol = 3L
  )
  rownames(mat) <- paste0("gene_", 1:5)
  colnames(mat) <- paste0("sample_", 1:3)
  gct <- new("GCT",
    mat = mat,
    cdesc = data.frame(group = c("A", "B", "A"), row.names = colnames(mat)),
    rdesc = data.frame(
      id = rownames(mat),
      geneSymbol = c("G1", "G2", NA, "", "G5"),
      row.names = rownames(mat)
    ),
    rid = rownames(mat),
    cid = colnames(mat)
  )
  pca_result <- calculate_PCA(gct)
  loadings_df <- get_pca_loadings_df(pca_result, gct = gct, max_pcs = NULL)
  n_plot <- pca_rank_pcs_used(ncol(pca_result$pca$rotation))
  top_features <- top_pca_loading_features(loadings_df, topn = 3L, max_pcs = n_plot)

  cumplot <- create_PCA_loadings_cumulative(pca_result, ome = "test", gct = gct)
  expect_s3_class(cumplot, "ggplot")

  legend_lvls <- levels(cumplot$data$legend_label)
  plot_labels <- sub("^\\d{2}: ", "", legend_lvls)
  meta <- loadings_df[match(top_features, loadings_df$feature), , drop = FALSE]
  expected <- vapply(seq_len(nrow(meta)), function(i) {
    pca_feature_display_label(meta$id[i], meta$geneSymbol[i])
  }, character(1))
  expect_equal(plot_labels[seq_along(expected)], expected)
})

test_that("create_PCA_loadings_cumulative cumulative curve ends at export metric", {
  df <- make_loadings_df()
  rotation <- as.matrix(df[, c("PC1", "PC2", "PC3")])
  rownames(rotation) <- df$feature
  pca_result <- make_pca_result(rotation)
  gct <- new("GCT",
    mat = matrix(1:6, nrow = 3L, ncol = 2L),
    cdesc = data.frame(group = c("A", "B"), row.names = c("s1", "s2")),
    rdesc = data.frame(
      id = df$id,
      geneSymbol = df$geneSymbol,
      row.names = df$feature
    ),
    rid = df$feature,
    cid = c("s1", "s2")
  )

  export_df <- get_pca_loadings_df(pca_result, gct = gct, for_export = TRUE)
  cum_col <- names(export_df)[2]
  cumplot <- create_PCA_loadings_cumulative(pca_result, gct = gct)
  n_plot <- pca_rank_pcs_used(ncol(rotation))

  top_features <- top_pca_loading_features(df, topn = 3L, max_pcs = n_plot)
  plot_end <- cumplot$data[
    cumplot$data$PC == n_plot & cumplot$data$feature %in% top_features,
    c("feature", "cumulative")
  ]
  plot_end <- plot_end[match(top_features, plot_end$feature), ]
  export_vals <- export_df[[cum_col]][seq_along(top_features)]
  expect_equal(plot_end$cumulative, export_vals, tolerance = 1e-10)
})
