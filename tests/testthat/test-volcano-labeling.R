# Tests for volcano plot protein search & labeling helpers
# Covers: get_clicked_feature_id, parse_protein_search_input, add_volcano_labels

## get_clicked_feature_id ####################################################

test_that("get_clicked_feature_id returns exact match", {
  df <- data.frame(
    id    = c("prot_A", "prot_B", "prot_C"),
    logFC = c(1.0, -2.0, 0.5),
    logP  = c(3.0,  2.5, 1.0),
    stringsAsFactors = FALSE
  )
  click <- list(x = 1.0, y = 3.0)
  result <- get_clicked_feature_id(click, df)
  expect_equal(result, "prot_A")
})

test_that("get_clicked_feature_id returns nearest within tolerance", {
  df <- data.frame(
    id    = c("prot_A", "prot_B"),
    logFC = c(1.0, -2.0),
    logP  = c(3.0,  2.5),
    stringsAsFactors = FALSE
  )
  # Click slightly off from prot_A (within default tolerance 0.01)
  click <- list(x = 1.005, y = 2.998)
  result <- get_clicked_feature_id(click, df)
  expect_equal(result, "prot_A")
})

test_that("get_clicked_feature_id returns NA when no point within tolerance", {
  df <- data.frame(
    id    = c("prot_A", "prot_B"),
    logFC = c(1.0, -2.0),
    logP  = c(3.0,  2.5),
    stringsAsFactors = FALSE
  )
  click <- list(x = 5.0, y = 10.0)  # far from all points
  result <- get_clicked_feature_id(click, df)
  expect_true(is.na(result))
})

test_that("get_clicked_feature_id handles empty data frame", {
  df <- data.frame(id = character(0), logFC = numeric(0), logP = numeric(0))
  click <- list(x = 1.0, y = 2.0)
  result <- get_clicked_feature_id(click, df)
  expect_true(is.na(result))
})

test_that("get_clicked_feature_id handles NULL click$x gracefully", {
  df <- data.frame(id = "A", logFC = 1.0, logP = 3.0, stringsAsFactors = FALSE)
  click <- list(x = NULL, y = 3.0)
  result <- get_clicked_feature_id(click, df)
  expect_true(is.na(result))
})

## parse_protein_search_input #################################################

test_that("parse_protein_search_input splits on spaces", {
  result <- parse_protein_search_input("ProtA ProtB ProtC")
  expect_equal(result, c("ProtA", "ProtB", "ProtC"))
})

test_that("parse_protein_search_input splits on commas", {
  result <- parse_protein_search_input("ProtA,ProtB,ProtC")
  expect_equal(result, c("ProtA", "ProtB", "ProtC"))
})

test_that("parse_protein_search_input splits on semicolons", {
  result <- parse_protein_search_input("ProtA;ProtB;ProtC")
  expect_equal(result, c("ProtA", "ProtB", "ProtC"))
})

test_that("parse_protein_search_input handles mixed delimiters", {
  result <- parse_protein_search_input("ProtA ProtB,ProtC;ProtD")
  expect_equal(result, c("ProtA", "ProtB", "ProtC", "ProtD"))
})

test_that("parse_protein_search_input drops empty tokens", {
  result <- parse_protein_search_input("  ProtA  ,,  ProtB  ")
  expect_equal(result, c("ProtA", "ProtB"))
})

test_that("parse_protein_search_input returns empty vector for blank input", {
  expect_equal(parse_protein_search_input(""), character(0))
  expect_equal(parse_protein_search_input("   "), character(0))
  expect_equal(parse_protein_search_input(NULL), character(0))
})

test_that("parse_protein_search_input handles newlines as delimiters", {
  result <- parse_protein_search_input("ProtA\nProtB\nProtC")
  expect_equal(result, c("ProtA", "ProtB", "ProtC"))
})

## add_volcano_labels ##########################################################

# Helper: create minimal plotly scatter for testing
make_test_plotly <- function(df) {
  plotly::plot_ly(df, x = ~logFC, y = ~logP, type = "scatter", mode = "markers",
                  key = ~id, source = "test_source")
}

# Helper: mock reactiveVal (stores value in plain environment)
mock_rv <- function(init = 0L) {
  e <- new.env(parent = emptyenv())
  e$val <- init
  function(x) {
    if (missing(x)) e$val else { e$val <- x; invisible(x) }
  }
}

test_that("add_volcano_labels returns a plotly object", {
  skip_if_not_installed("plotly")
  df <- data.frame(
    id = c("A", "B", "C"), logFC = c(1, -1, 0.1),
    logP = c(4, 3, 1), Significant = c(TRUE, TRUE, FALSE),
    geneSymbol = c("GENE1", "GENE2", "GENE3"), stringsAsFactors = FALSE
  )
  p <- make_test_plotly(df)
  rv <- mock_rv()
  result <- add_volcano_labels(p, df, poi = "A", label_mode = "poi",
                                y_cutoff = 2, hidden_count_rv = rv)
  expect_s3_class(result, "plotly")
})

test_that("add_volcano_labels with empty poi and no mode returns plotly unchanged", {
  skip_if_not_installed("plotly")
  df <- data.frame(
    id = c("A", "B"), logFC = c(1, -1), logP = c(4, 3),
    Significant = c(TRUE, FALSE), geneSymbol = c("G1", "G2"),
    stringsAsFactors = FALSE
  )
  p <- make_test_plotly(df)
  rv <- mock_rv()
  result <- add_volcano_labels(p, df, poi = character(0), label_mode = character(0),
                                y_cutoff = 2, hidden_count_rv = rv)
  expect_s3_class(result, "plotly")
  expect_equal(rv(), 0L)
})

test_that("add_volcano_labels sets hidden_count_rv to 0 when all labels fit", {
  skip_if_not_installed("plotly")
  df <- data.frame(
    id = c("A", "B"), logFC = c(1, -1), logP = c(4, 3),
    Significant = c(TRUE, FALSE), geneSymbol = c("G1", "G2"),
    stringsAsFactors = FALSE
  )
  p <- make_test_plotly(df)
  rv <- mock_rv()
  result <- add_volcano_labels(p, df, poi = c("A", "B"), label_mode = "poi",
                                y_cutoff = 2, hidden_count_rv = rv)
  expect_equal(rv(), 0L)
})

test_that("add_volcano_labels hidden_count_rv reflects dropped overlapping labels", {
  skip_if_not_installed("plotly")
  # 10 points at identical (x, y) — all but the first must be hidden
  n <- 10
  df <- data.frame(
    id          = paste0("P", 1:n),
    logFC       = rep(1.0, n),   # all at exactly the same x
    logP        = rep(3.0, n),   # all at exactly the same y
    Significant = rep(FALSE, n),
    geneSymbol  = paste0("G", 1:n),
    stringsAsFactors = FALSE
  )
  p <- make_test_plotly(df)
  rv <- mock_rv()
  result <- add_volcano_labels(p, df, poi = df$id, label_mode = "poi",
                                y_cutoff = 2, hidden_count_rv = rv)
  # 9 of 10 labels should be hidden (all at same position)
  expect_equal(rv(), 9L)
})

test_that("add_volcano_labels handles NA in Significant column gracefully", {
  skip_if_not_installed("plotly")
  df <- data.frame(
    id = c("A", "B", "C"), logFC = c(1, -1, 0.1),
    logP = c(4, 3, 1), Significant = c(TRUE, NA, FALSE),
    geneSymbol = c("GENE1", "GENE2", "GENE3"), stringsAsFactors = FALSE
  )
  p <- make_test_plotly(df)
  rv <- mock_rv()
  expect_no_error(
    add_volcano_labels(p, df, poi = character(0), label_mode = "significant",
                       y_cutoff = 2, hidden_count_rv = rv)
  )
})
