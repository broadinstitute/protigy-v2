################################################################################
# Tests for Spectronaut preprocessing helper functions
################################################################################

test_that("extract_protigy_id: normal case splits on semicolon and takes first token", {
  data <- data.frame(
    PG.ProteinGroups = c("PROT1;PROT2;PROT3", "PROT4;PROT5"),
    Value = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )
  result <- extract_protigy_id(data, "PG.ProteinGroups", ";")
  expect_equal(result$protigy_id, c("PROT1", "PROT4"))
  expect_equal(names(result)[1], "protigy_id")
})

test_that("extract_protigy_id: empty first token falls through to next non-empty", {
  data <- data.frame(
    PG.ProteinGroups = c(";PROT2;PROT3", "PROT4"),
    stringsAsFactors = FALSE
  )
  result <- extract_protigy_id(data, "PG.ProteinGroups", ";")
  expect_equal(result$protigy_id, c("PROT2", "PROT4"))
})

test_that("extract_protigy_id: all-empty tokens returns NA", {
  data <- data.frame(
    PG.ProteinGroups = c(";;;", "PROT1"),
    stringsAsFactors = FALSE
  )
  result <- extract_protigy_id(data, "PG.ProteinGroups", ";")
  expect_true(is.na(result$protigy_id[1]))
  expect_equal(result$protigy_id[2], "PROT1")
})

test_that("extract_protigy_id: NA input returns NA", {
  data <- data.frame(
    PG.ProteinGroups = c(NA_character_, "PROT1"),
    stringsAsFactors = FALSE
  )
  result <- extract_protigy_id(data, "PG.ProteinGroups", ";")
  expect_true(is.na(result$protigy_id[1]))
  expect_equal(result$protigy_id[2], "PROT1")
})

test_that("detect_quant_suffixes: detects two suffixes correctly", {
  data_columns <- c(
    "PG.ProteinGroups",
    "run1.PG.IBAQ", "run1.PG.Quantity",
    "run2.PG.IBAQ", "run2.PG.Quantity"
  )
  run_labels <- c("run1", "run2")
  result <- detect_quant_suffixes(data_columns, run_labels)
  expect_equal(result, c(".PG.IBAQ", ".PG.Quantity"))
})

test_that("detect_quant_suffixes: handles run labels with dots and underscores", {
  data_columns <- c(
    "S1_rep1.PG.Quantity", "S1_rep1.PG.IBAQ",
    "S2_rep1.PG.Quantity", "S2_rep1.PG.IBAQ"
  )
  run_labels <- c("S1_rep1", "S2_rep1")
  result <- detect_quant_suffixes(data_columns, run_labels)
  expect_equal(result, c(".PG.IBAQ", ".PG.Quantity"))
})

test_that("apply_spectronaut_condition_setup: renames and drops non-selected columns", {
  data <- data.frame(
    PG.ProteinGroups = c("P1", "P2"),
    "run1.PG.Quantity" = c(10, 20),
    "run1.PG.IBAQ"     = c(100, 200),
    "run2.PG.Quantity" = c(30, 40),
    "run2.PG.IBAQ"     = c(300, 400),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  condition_setup <- data.frame(
    "Run Label"  = c("run1", "run2"),
    "Condition"  = c("Control", "Treatment"),
    "Replicate"  = c(1, 1),
    check.names  = FALSE,
    stringsAsFactors = FALSE
  )
  result <- apply_spectronaut_condition_setup(data, condition_setup, ".PG.Quantity",
                                              merge_condition_replicate = FALSE)
  expect_true("Control" %in% names(result))
  expect_true("Treatment" %in% names(result))
  expect_false("run1.PG.IBAQ" %in% names(result))
  expect_false("run2.PG.IBAQ" %in% names(result))
  expect_false("run1.PG.Quantity" %in% names(result))
})

test_that("apply_spectronaut_condition_setup: merge_condition_replicate produces correct names", {
  data <- data.frame(
    PG.ProteinGroups = c("P1"),
    "run1.PG.Quantity" = c(10),
    "run2.PG.Quantity" = c(30),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  condition_setup <- data.frame(
    "Run Label"  = c("run1", "run2"),
    "Condition"  = c("Control", "Control"),
    "Replicate"  = c(1, 2),
    check.names  = FALSE,
    stringsAsFactors = FALSE
  )
  result <- apply_spectronaut_condition_setup(data, condition_setup, ".PG.Quantity",
                                              merge_condition_replicate = TRUE)
  expect_true("Control_R1" %in% names(result))
  expect_true("Control_R2" %in% names(result))
})

test_that("split_gene_symbol_column: normal case takes first token", {
  rdesc <- data.frame(
    geneSymbol = c("GENE1;GENE2;GENE3", "GENE4"),
    stringsAsFactors = FALSE
  )
  result <- split_gene_symbol_column(rdesc, "geneSymbol", ";")
  expect_equal(result$geneSymbol, c("GENE1", "GENE4"))
})

test_that("split_gene_symbol_column: leading empties (;;;gene1) skips to first non-empty", {
  rdesc <- data.frame(
    geneSymbol = c(";;;GENE1", "GENE2"),
    stringsAsFactors = FALSE
  )
  result <- split_gene_symbol_column(rdesc, "geneSymbol", ";")
  expect_equal(result$geneSymbol, c("GENE1", "GENE2"))
})

test_that("split_gene_symbol_column: all empty returns NA", {
  rdesc <- data.frame(
    geneSymbol = c(";;;", "GENE1"),
    stringsAsFactors = FALSE
  )
  result <- split_gene_symbol_column(rdesc, "geneSymbol", ";")
  expect_true(is.na(result$geneSymbol[1]))
  expect_equal(result$geneSymbol[2], "GENE1")
})
