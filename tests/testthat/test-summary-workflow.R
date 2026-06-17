# Phase 3 (P3.2) -- summary_workflow branch coverage (R/tab_summary_helpers.R).
#
# summary_workflow(params) builds the per-dataset "Workflow" table: a base set of
# rows plus conditional rows for id-mapping, sample/row filters, group
# normalization, and the StdDev percentile. It also computes
# id_mapping_n_unmapped as "mapped / total" = (total - bad) / total.
#
# This file tests one branch at a time, asserting row presence AND order. The
# source is tested AS-IS (no edits to R/).

# Minimal params with every field the unconditional branches read, so the base
# call never errors. data_filter/group_normalization are read unguarded.
base_params <- function(...) {
  modifyList(list(
    gct_file_name = "data.gct",
    annotation_column = "group",
    gene_symbol_column = "geneSymbol",
    intensity_data = FALSE,
    log_transformation = "none",
    data_normalization = "none",
    group_normalization = FALSE,
    data_filter = "None",
    max_missing = 50,
    convert_ids_to_gene_symbol = FALSE
  ), list(...))
}

# Convenience: the ordered vector of row labels for a built table.
row_labels <- function(df) rownames(df)

# ---------------------------------------------------------------------------
# Base table -- no conditional rows
# ---------------------------------------------------------------------------

test_that("base workflow has only the unconditional rows in order", {
  df <- summary_workflow(base_params())
  expect_equal(
    row_labels(df),
    c("File name", "Annotation column", "Gene symbol column",
      "Intensity data", "Log transformation", "Data normalization",
      "Normalized by group", "Data filter", "Max missing %")
  )
  expect_equal(df["File name", "Value"], "data.gct")
})

# ---------------------------------------------------------------------------
# ID-mapping branch (requires convert_ids_to_gene_symbol TRUE + gene col "None")
# ---------------------------------------------------------------------------

test_that("id-mapping rows are inserted after Gene symbol column", {
  df <- summary_workflow(base_params(
    convert_ids_to_gene_symbol = TRUE,
    gene_symbol_column = "None",
    id_source_column = "id",
    id_mapping_species = "human",
    id_mapping_keytype = "UNIPROT",
    id_mapping_n_total = 100,
    id_mapping_n_unmapped = 10
  ))
  labs <- row_labels(df)
  expect_true(all(c("Map IDs to symbols", "ID column for mapping",
                    "ID mapping species", "Detected ID keytype",
                    "Gene symbols mapped") %in% labs))
  # inserted directly after "Gene symbol column"
  gsc <- which(labs == "Gene symbol column")
  expect_equal(labs[gsc + 1], "Map IDs to symbols")
  # the Yes/No formatting branch
  expect_equal(df["Map IDs to symbols", "Value"], "Yes")
})

test_that("id_mapping_n_unmapped renders as mapped/total = (total - bad)/total", {
  df <- summary_workflow(base_params(
    convert_ids_to_gene_symbol = TRUE,
    gene_symbol_column = "None",
    id_mapping_n_total = 100,
    id_mapping_n_unmapped = 10
  ))
  # 100 total, 10 unmapped -> 90 mapped
  expect_equal(df["Gene symbols mapped", "Value"], "90 / 100")
})

test_that("id_mapping_n_unmapped renders empty when counts are missing", {
  df <- summary_workflow(base_params(
    convert_ids_to_gene_symbol = TRUE,
    gene_symbol_column = "None"
    # no id_mapping_n_total / id_mapping_n_unmapped
  ))
  expect_equal(df["Gene symbols mapped", "Value"], "")
})

test_that("id-mapping rows are absent when gene_symbol_column is not 'None'", {
  df <- summary_workflow(base_params(
    convert_ids_to_gene_symbol = TRUE,
    gene_symbol_column = "geneSymbol"
  ))
  expect_false("Map IDs to symbols" %in% row_labels(df))
})

# ---------------------------------------------------------------------------
# Sample (column) filter branch
# ---------------------------------------------------------------------------

test_that("sample-filter rows are inserted after Annotation column", {
  df <- summary_workflow(base_params(
    sample_filter_enabled = TRUE,
    sample_filter_column = "tissue",
    sample_filter_values = c("tumor", "normal")
  ))
  labs <- row_labels(df)
  ac <- which(labs == "Annotation column")
  expect_equal(labs[ac + 1], "Column filter column")
  expect_equal(labs[ac + 2], "Column filter values")
  # multi-value formatting collapses with ", "
  expect_equal(df["Column filter values", "Value"], "tumor, normal")
})

# ---------------------------------------------------------------------------
# Row filter branch -- insert position depends on sample-filter presence
# ---------------------------------------------------------------------------

test_that("row-filter rows insert after Annotation column when no sample filter", {
  df <- summary_workflow(base_params(
    row_filter_enabled = TRUE,
    row_filter_column = "type",
    row_filter_values = "keep"
  ))
  labs <- row_labels(df)
  ac <- which(labs == "Annotation column")
  expect_equal(labs[ac + 1], "Row filter column")
  expect_equal(labs[ac + 2], "Row filter values")
})

test_that("row-filter rows insert after sample-filter values when both enabled", {
  df <- summary_workflow(base_params(
    sample_filter_enabled = TRUE,
    sample_filter_column = "tissue",
    sample_filter_values = "tumor",
    row_filter_enabled = TRUE,
    row_filter_column = "type",
    row_filter_values = "keep"
  ))
  labs <- row_labels(df)
  sfv <- which(labs == "Column filter values")
  expect_equal(labs[sfv + 1], "Row filter column")
  expect_equal(labs[sfv + 2], "Row filter values")
})

# ---------------------------------------------------------------------------
# Group normalization branch
# ---------------------------------------------------------------------------

test_that("group-normalization column row inserts after Normalized by group", {
  df <- summary_workflow(base_params(
    group_normalization = TRUE,
    group_normalization_column = "batch"
  ))
  labs <- row_labels(df)
  gn <- which(labs == "Normalized by group")
  expect_equal(labs[gn + 1], "Group normalization col.")
  expect_equal(df["Group normalization col.", "Value"], "batch")
})

# ---------------------------------------------------------------------------
# StdDev percentile branch
# ---------------------------------------------------------------------------

test_that("StdDev percentile row inserts after Data filter when filter is StdDev", {
  df <- summary_workflow(base_params(
    data_filter = "StdDev",
    data_filter_sd_pct = 25
  ))
  labs <- row_labels(df)
  dfp <- which(labs == "Data filter")
  expect_equal(labs[dfp + 1], "Std. Dev. filter percentile")
  expect_equal(df["Std. Dev. filter percentile", "Value"], "25")
})

test_that("StdDev percentile row absent for non-StdDev filters", {
  df <- summary_workflow(base_params(data_filter = "None"))
  expect_false("Std. Dev. filter percentile" %in% row_labels(df))
})

# ---------------------------------------------------------------------------
# Combined: all conditional branches together keep a sane ordering
# ---------------------------------------------------------------------------

test_that("all conditional branches coexist with correct relative order", {
  df <- summary_workflow(base_params(
    convert_ids_to_gene_symbol = TRUE,
    gene_symbol_column = "None",
    id_mapping_n_total = 50, id_mapping_n_unmapped = 5,
    sample_filter_enabled = TRUE,
    sample_filter_column = "tissue", sample_filter_values = "tumor",
    row_filter_enabled = TRUE,
    row_filter_column = "type", row_filter_values = "keep",
    group_normalization = TRUE, group_normalization_column = "batch",
    data_filter = "StdDev", data_filter_sd_pct = 10
  ))
  labs <- row_labels(df)
  # spot-check key orderings
  expect_lt(which(labs == "Column filter column"), which(labs == "Row filter column"))
  expect_lt(which(labs == "Normalized by group"), which(labs == "Group normalization col."))
  expect_lt(which(labs == "Data filter"), which(labs == "Std. Dev. filter percentile"))
  expect_equal(df["Gene symbols mapped", "Value"], "45 / 50")
})
