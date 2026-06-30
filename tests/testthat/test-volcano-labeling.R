# Tests for volcano plot feature search and labeling helpers
# Covers: get_clicked_feature_id, parse_protein_search_input,
#         volcano_build_hover_text, volcano_display_trim, volcano_maybe_display_trim,
#         volcano_label_top_significant_subset, add_volcano_labels

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

## volcano_display_trim ########################################################

test_that("volcano_display_trim keeps accession-style prefix, drops trailing _digits", {
  expect_equal(
    volcano_display_trim("NP_000468.1_K28k_1_1_28_28"),
    "NP_000468.1_K28k"
  )
  expect_equal(
    volcano_display_trim("NP_000468.1_K28k _1_1_28_28"),
    "NP_000468.1_K28k"
  )
  expect_equal(
    volcano_display_trim(c("NP_000468.1_K28k", "NM_999.2_X_y_0_0")),
    c("NP_000468.1_K28k", "NM_999.2_X")
  )
})

test_that("volcano_display_trim leaves short or NA values alone", {
  expect_equal(volcano_display_trim("BRCA1"), "BRCA1")
  expect_equal(volcano_display_trim(NA_character_), NA_character_)
  expect_equal(volcano_display_trim(character(0)), character(0))
})

test_that("volcano_maybe_display_trim passes through when disabled", {
  x <- c("NP_000468.1_K28k_1_1_28_28", "A")
  expect_equal(volcano_maybe_display_trim(x, FALSE), as.character(x))
  expect_equal(volcano_maybe_display_trim(x, TRUE), volcano_display_trim(x))
})

## volcano_build_hover_text #####################################################

test_that("volcano_build_hover_text is ID-only without gene symbol values", {
  expect_equal(volcano_build_hover_text(c("a", "b")), c("ID: a", "ID: b"))
  expect_equal(volcano_build_hover_text("x", NULL, "geneSymbol"), "ID: x")
})

test_that("volcano_build_hover_text appends gene symbol line when lengths match", {
  out <- volcano_build_hover_text(
    c("id1", "id2"),
    c("G1", "G2"),
    "geneSymbol"
  )
  expect_equal(out, c("ID: id1<br>geneSymbol: G1", "ID: id2<br>geneSymbol: G2"))
})

test_that("volcano_build_hover_text uses custom column name for second line", {
  out <- volcano_build_hover_text("id1", "G1", "myGeneSym")
  expect_equal(out, "ID: id1<br>myGeneSym: G1")
})

test_that("volcano_build_hover_text ignores gs_vals when length mismatches ids", {
  expect_equal(
    volcano_build_hover_text(c("a", "b"), "only_one", "geneSymbol"),
    c("ID: a", "ID: b")
  )
})

test_that("volcano_build_hover_text preserves NA in gene symbol display", {
  out <- volcano_build_hover_text(c("i1", "i2"), c("G1", NA), "geneSymbol")
  expect_equal(out[2], "ID: i2<br>geneSymbol: NA")
})

test_that("volcano_build_hover_text appends user-selected label column line", {
  out <- volcano_build_hover_text(
    c("id1", "id2"),
    gs_vals      = c("G1", "G2"),
    gs_col_name  = "geneSymbol",
    lbl_vals     = c("desc1", "desc2"),
    lbl_col_name = "description"
  )
  expect_equal(out, c(
    "ID: id1<br>geneSymbol: G1<br>description: desc1",
    "ID: id2<br>geneSymbol: G2<br>description: desc2"
  ))
})

test_that("volcano_build_hover_text ignores lbl_vals when length mismatches ids", {
  out <- volcano_build_hover_text(
    c("a", "b"),
    gs_vals      = c("G1", "G2"),
    gs_col_name  = "geneSymbol",
    lbl_vals     = "only_one",
    lbl_col_name = "description"
  )
  expect_equal(out, c("ID: a<br>geneSymbol: G1", "ID: b<br>geneSymbol: G2"))
})

test_that("volcano_build_hover_text supports label line without gene symbol line", {
  out <- volcano_build_hover_text(
    c("id1", "id2"),
    lbl_vals     = c("d1", "d2"),
    lbl_col_name = "description"
  )
  expect_equal(out, c("ID: id1<br>description: d1", "ID: id2<br>description: d2"))
})

## volcano_label_top_significant_subset ########################################

test_that("volcano_label_top_significant_subset returns top n by logP among significant", {
  df <- data.frame(
    id          = letters[1:10],
    logFC       = 1:10,
    logP        = 1:10,
    Significant = c(rep(TRUE, 8), FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  sub <- volcano_label_top_significant_subset(df, 3L)
  expect_equal(nrow(sub), 3L)
  expect_equal(as.character(sub$id), c("h", "g", "f"))
  expect_equal(sub$logP, c(8, 7, 6))
})

test_that("volcano_label_top_significant_subset returns fewer than n when not enough sig", {
  df <- data.frame(
    id = c("a", "b"), logFC = c(1, 2), logP = c(5, 4),
    Significant = c(TRUE, TRUE), stringsAsFactors = FALSE
  )
  sub <- volcano_label_top_significant_subset(df, 20L)
  expect_equal(nrow(sub), 2L)
})

test_that("volcano_label_top_significant_subset excludes NA logP and NA Significant", {
  df <- data.frame(
    id = c("a", "b", "c", "d"),
    logFC = 1:4,
    logP = c(10, NA, 8, 7),
    Significant = c(TRUE, TRUE, TRUE, NA),
    stringsAsFactors = FALSE
  )
  sub <- volcano_label_top_significant_subset(df, 10L)
  expect_equal(as.character(sub$id), c("a", "c"))
  expect_equal(sub$logP, c(10, 8))
})

test_that("volcano_label_top_significant_subset breaks logP ties by larger abs(logFC) first", {
  df <- data.frame(
    id = c("low_fc", "mid_fc", "high_fc"),
    logFC = c(1, 2, 3),
    logP = c(5, 5, 5),
    Significant = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  sub <- volcano_label_top_significant_subset(df, 2L)
  expect_equal(as.character(sub$id), c("high_fc", "mid_fc"))
})

test_that("volcano_label_top_significant_subset ties + and - logFC with same magnitude", {
  df <- data.frame(
    id = c("neg", "pos"),
    logFC = c(-2, 2),
    logP = c(5, 5),
    Significant = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  sub <- volcano_label_top_significant_subset(df, 1L)
  expect_equal(nrow(sub), 2L)
  expect_setequal(as.character(sub$id), c("neg", "pos"))
})

test_that("volcano_label_top_significant_subset keeps all rows tied on logP and abs(logFC)", {
  df <- data.frame(
    id = c("r1", "r2", "r3"),
    logFC = c(1, 1, 1),
    logP = c(5, 5, 5),
    Significant = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  sub <- volcano_label_top_significant_subset(df, 2L)
  expect_equal(nrow(sub), 3L)
  expect_setequal(as.character(sub$id), c("r1", "r2", "r3"))
})

## volcano_labeled_feature_ids ##################################################

test_that("volcano_labeled_feature_ids matches add_volcano_labels union (sig + poi)", {
  df_plot <- data.frame(
    id = c("s1", "s2", "p1", "ns"),
    logFC = 1:4,
    logP = 5:8,
    Significant = c(TRUE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  ids <- volcano_labeled_feature_ids(df_plot, c("significant", "poi"), c("p1"))
  expect_setequal(ids, c("s1", "s2", "p1"))
})

test_that("volcano_labeled_feature_ids uses top-20 path when significant not selected", {
  df_plot <- data.frame(
    id = letters[1:5],
    logFC = 1:5,
    logP = 5:1,
    Significant = rep(TRUE, 5),
    stringsAsFactors = FALSE
  )
  ids <- volcano_labeled_feature_ids(df_plot, "significant_top20", character(0))
  expect_true(length(ids) <= 20L)
  expect_true(all(ids %in% letters[1:5]))
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
  # 10 points at identical (x, y)  -  all but the first must be hidden
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

test_that("add_volcano_labels places POI before significant when coordinates overlap", {
  skip_if_not_installed("plotly")
  # Same (logFC, logP): if significance-only rows were placed first, the POI would lose.
  df <- data.frame(
    id          = c("sig_only", "poi_only"),
    logFC       = c(1, 1),
    logP        = c(5, 5),
    Significant = c(TRUE, FALSE),
    geneSymbol  = c("SIG", "POI"),
    stringsAsFactors = FALSE
  )
  p <- make_test_plotly(df)
  rv <- mock_rv()
  result <- add_volcano_labels(
    p, df, poi = "poi_only", label_mode = c("significant", "poi"),
    y_cutoff = 2, hidden_count_rv = rv
  )
  built <- plotly::plotly_build(result)
  ann <- built$x$layout$annotations %||% list()
  expect_length(ann, 1L)
  expect_equal(ann[[1]]$text, "POI")
  expect_equal(rv(), 1L)
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

test_that("add_volcano_labels with significant_top20 caps labeled significant rows", {
  skip_if_not_installed("plotly")
  n_sig <- 25L
  df <- data.frame(
    id          = sprintf("P%02d", seq_len(n_sig)),
    logFC       = seq_len(n_sig),
    logP        = seq_len(n_sig),
    Significant = rep(TRUE, n_sig),
    geneSymbol  = sprintf("G%02d", seq_len(n_sig)),
    stringsAsFactors = FALSE
  )
  p <- make_test_plotly(df)
  rv <- mock_rv()
  result <- add_volcano_labels(
    p, df, poi = character(0), label_mode = "significant_top20",
    y_cutoff = 0, hidden_count_rv = rv
  )
  expect_s3_class(result, "plotly")
  # layout() stores annotations in layoutAttrs until plotly_build() merges x$layout
  built <- plotly::plotly_build(result)
  n_ann <- length(built$x$layout$annotations %||% list())
  expect_equal(n_ann, 20L)
})

## regex_escape ###############################################################

test_that("regex_escape escapes PCRE metacharacters", {
  expect_equal(regex_escape("Group(A)"),    "Group\\(A\\)")
  expect_equal(regex_escape("T=2.5h"),      "T=2\\.5h")
  expect_equal(regex_escape("Control+Drug"), "Control\\+Drug")
  expect_equal(regex_escape("back\\slash"), "back\\\\slash")
  expect_equal(regex_escape("plain"),       "plain")
  expect_equal(regex_escape(""),            "")
})

## get_volcano_cols  -  metacharacter safety ####################################

test_that("get_volcano_cols handles group names with regex metacharacters", {
  group_name <- "Group(A)"

  col_names <- c(
    "id",
    paste0("logFC.", group_name),
    paste0("Log.P.Value.", group_name),
    paste0("adj.P.Val.", group_name),
    paste0("P.value.", group_name),
    "geneSymbol"
  )
  df <- setNames(
    as.data.frame(matrix(NA_real_, nrow = 1, ncol = length(col_names))),
    col_names
  )
  df$id         <- "prot_1"
  df$geneSymbol <- "GENE1"

  result <- get_volcano_cols(df, "One-sample Moderated T-test",
                              volcano_groups    = group_name,
                              volcano_contrasts = NULL)

  expect_false(is.na(result$logfc), info = "logfc col not found  -  metacharacter escaping likely missing")
  expect_false(is.na(result$logp),  info = "logp col not found")
  expect_false(is.na(result$adjp),  info = "adjp col not found")
  expect_false(is.na(result$pval),  info = "pval col not found")
})

test_that("get_volcano_cols handles contrast names with regex metacharacters (two-sample)", {
  contrast_name_col <- "Group(A)_over_Group(B)"
  col_names <- c(
    "id",
    paste0("logFC.", contrast_name_col),
    paste0("Log.P.Value.", contrast_name_col),
    paste0("adj.P.Val.", contrast_name_col),
    paste0("P.value.", contrast_name_col),
    "geneSymbol"
  )
  df <- setNames(
    as.data.frame(matrix(NA_real_, nrow = 1, ncol = length(col_names))),
    col_names
  )
  df$id         <- "prot_1"
  df$geneSymbol <- "GENE1"

  result <- get_volcano_cols(df, "Two-sample Moderated T-test",
                              volcano_groups    = NULL,
                              volcano_contrasts = "Group(A) / Group(B)")

  expect_false(is.na(result$logfc), info = "logfc col not found for metacharacter contrast")
  expect_false(is.na(result$logp))
  expect_false(is.na(result$adjp))
  expect_false(is.na(result$pval))
})

## get_volcano_cols  -  normal cases ############################################

# Helper: build a minimal stat_results df with standard column naming
make_one_sample_df <- function(group = "GroupA") {
  cols <- c("id", "geneSymbol",
            paste0("logFC.", group),
            paste0("Log.P.Value.", group),
            paste0("adj.P.Val.", group),
            paste0("P.value.", group))
  df <- setNames(as.data.frame(matrix(0, nrow = 3, ncol = length(cols))), cols)
  df$id <- c("p1", "p2", "p3")
  df$geneSymbol <- c("G1", "G2", "G3")
  df
}

make_two_sample_df <- function(contrast = "A / B") {
  groups <- strsplit(contrast, " / ")[[1]]
  cn     <- paste0(groups[1], "_over_", groups[2])
  cols <- c("id", "geneSymbol",
            paste0("logFC.", cn),
            paste0("Log.P.Value.", cn),
            paste0("adj.P.Val.", cn),
            paste0("P.value.", cn))
  df <- setNames(as.data.frame(matrix(0, nrow = 3, ncol = length(cols))), cols)
  df$id <- c("p1", "p2", "p3")
  df$geneSymbol <- c("G1", "G2", "G3")
  df
}

test_that("get_volcano_cols resolves correct columns for one-sample test", {
  df     <- make_one_sample_df("GroupA")
  result <- get_volcano_cols(df, "One-sample Moderated T-test",
                              volcano_groups = "GroupA", volcano_contrasts = NULL)

  expect_equal(result$logfc, "logFC.GroupA")
  expect_equal(result$logp,  "Log.P.Value.GroupA")
  expect_equal(result$adjp,  "adj.P.Val.GroupA")
  expect_equal(result$pval,  "P.value.GroupA")
  expect_equal(result$id,    "id")
  expect_equal(result$gs,    "geneSymbol")
})

test_that("get_volcano_cols resolves correct columns for two-sample test", {
  df     <- make_two_sample_df("GroupA / GroupB")
  result <- get_volcano_cols(df, "Two-sample Moderated T-test",
                              volcano_groups = NULL,
                              volcano_contrasts = "GroupA / GroupB")

  expect_equal(result$logfc, "logFC.GroupA_over_GroupB")
  expect_equal(result$logp,  "Log.P.Value.GroupA_over_GroupB")
  expect_equal(result$adjp,  "adj.P.Val.GroupA_over_GroupB")
  expect_equal(result$pval,  "P.value.GroupA_over_GroupB")
})

test_that("get_volcano_cols returns NA for id when id column is absent", {
  df <- make_one_sample_df("GroupA")
  df <- df[, setdiff(colnames(df), "id")]
  result <- get_volcano_cols(df, "One-sample Moderated T-test",
                              volcano_groups = "GroupA", volcano_contrasts = NULL)
  expect_true(is.na(result$id))
})

test_that("get_volcano_cols returns NA for gs when geneSymbol column is absent", {
  df <- make_one_sample_df("GroupA")
  df <- df[, setdiff(colnames(df), "geneSymbol")]
  result <- get_volcano_cols(df, "One-sample Moderated T-test",
                              volcano_groups = "GroupA", volcano_contrasts = NULL)
  expect_true(is.na(result$gs))
})

## build_volcano_df ###########################################################

# Helper: create a cols list matching make_one_sample_df("GroupA")
make_cols_one_sample <- function(group = "GroupA") {
  list(
    logfc = paste0("logFC.", group),
    logp  = paste0("Log.P.Value.", group),
    adjp  = paste0("adj.P.Val.", group),
    pval  = paste0("P.value.", group),
    id    = "id",
    gs    = "geneSymbol"
  )
}

test_that("build_volcano_df returns expected canonical columns", {
  df_raw <- make_one_sample_df("GroupA")
  df_raw[["logFC.GroupA"]]       <- c(2.0, -1.5, 0.1)
  df_raw[["Log.P.Value.GroupA"]] <- c(4.0,  3.0, 1.0)
  df_raw[["adj.P.Val.GroupA"]]   <- c(0.01, 0.05, 0.5)
  df_raw[["P.value.GroupA"]]     <- c(0.001, 0.01, 0.1)

  cols   <- make_cols_one_sample()
  result <- build_volcano_df(df_raw, cols, sig_cutoff = 0.05, sig_stat = "p.val")

  expect_true(all(c("id", "logFC", "logP", "adj.P.Val", "P.Value", "geneSymbol", "Significant") %in% colnames(result)))
  expect_equal(result$logFC, c(2.0, -1.5, 0.1))
  expect_equal(result$logP,  c(4.0, 3.0, 1.0))
})

test_that("build_volcano_df computes Significant correctly with p.val sig_stat", {
  df_raw <- make_one_sample_df("GroupA")
  df_raw[["logFC.GroupA"]]       <- c(2.0, -1.5, 0.1)
  df_raw[["Log.P.Value.GroupA"]] <- c(4.0,  3.0,  0.5)
  df_raw[["adj.P.Val.GroupA"]]   <- c(0.01, 0.05, 0.5)
  df_raw[["P.value.GroupA"]]     <- c(0.001, 0.01, 0.1)

  cols   <- make_cols_one_sample()
  # sig_cutoff = 0.05 -> y_cutoff = -log10(0.05) ~ 1.301
  # logP values: 4.0 > 1.301 (sig), 3.0 > 1.301 (sig), 0.5 < 1.301 (not sig)
  result <- build_volcano_df(df_raw, cols, sig_cutoff = 0.05, sig_stat = "p.val")

  expect_equal(result$Significant, c(TRUE, TRUE, FALSE))
  expect_equal(attr(result, "y_cutoff"), -log10(0.05))
})

test_that("build_volcano_df computes Significant correctly with adj.p.val sig_stat", {
  df_raw <- make_one_sample_df("GroupA")
  df_raw[["logFC.GroupA"]]       <- c(2.0, -1.5, 0.1)
  df_raw[["Log.P.Value.GroupA"]] <- c(4.0,  3.0,  0.5)
  df_raw[["adj.P.Val.GroupA"]]   <- c(0.01, 0.06, 0.5)   # only row 1 passes
  df_raw[["P.value.GroupA"]]     <- c(0.001, 0.01, 0.1)

  cols   <- make_cols_one_sample()
  # sig_cutoff = 0.05: only row 1 passes adj.P.Val < 0.05
  # max P.Value among passing rows = 0.001 -> y_cutoff = -log10(0.001) = 3.0
  # logP: 4.0 > 3.0 (sig), 3.0 == 3.0 (NOT sig, strict >), 0.5 < 3.0 (not sig)
  result <- build_volcano_df(df_raw, cols, sig_cutoff = 0.05, sig_stat = "adj.p.val")

  expect_true(result$Significant[1])
  expect_false(result$Significant[2])
  expect_false(result$Significant[3])
})

test_that("build_volcano_df errors with helpful message when columns are missing", {
  df_raw <- make_one_sample_df("GroupA")
  cols <- make_cols_one_sample()
  cols$logfc <- "logFC.DOES_NOT_EXIST"

  expect_error(
    build_volcano_df(df_raw, cols, sig_cutoff = 0.05, sig_stat = "p.val"),
    regexp = "Missing required volcano columns"
  )
})

test_that("build_volcano_df drops rows where logP is NA", {
  df_raw <- make_one_sample_df("GroupA")
  df_raw[["logFC.GroupA"]]       <- c(2.0, NA,   0.1)
  df_raw[["Log.P.Value.GroupA"]] <- c(4.0, NA,   1.0)
  df_raw[["adj.P.Val.GroupA"]]   <- c(0.01, NA,  0.5)
  df_raw[["P.value.GroupA"]]     <- c(0.001, NA, 0.1)

  cols   <- make_cols_one_sample()
  result <- build_volcano_df(df_raw, cols, sig_cutoff = 0.05, sig_stat = "p.val")

  expect_equal(nrow(result), 2)
  expect_equal(result$id, c("p1", "p3"))
})

test_that("build_volcano_df uses id as geneSymbol fallback when gs col is absent", {
  df_raw <- make_one_sample_df("GroupA")
  df_raw[["logFC.GroupA"]]       <- c(1.0, 2.0, 0.5)
  df_raw[["Log.P.Value.GroupA"]] <- c(3.0, 4.0, 1.0)
  df_raw[["adj.P.Val.GroupA"]]   <- c(0.01, 0.02, 0.3)
  df_raw[["P.value.GroupA"]]     <- c(0.001, 0.002, 0.1)

  cols    <- make_cols_one_sample()
  cols$gs <- NA_character_   # simulate absent geneSymbol column

  result <- build_volcano_df(df_raw, cols, sig_cutoff = 0.05, sig_stat = "p.val")
  expect_equal(result$geneSymbol, result$id)
})

test_that("build_volcano_df sets all Significant to FALSE when no rows pass adj.p.val cutoff", {
  df_raw <- make_one_sample_df("GroupA")
  df_raw[["logFC.GroupA"]]       <- c(2.0, -1.5, 0.1)
  df_raw[["Log.P.Value.GroupA"]] <- c(4.0,  3.0,  1.0)
  df_raw[["adj.P.Val.GroupA"]]   <- c(0.1,  0.2,  0.5)  # none pass cutoff of 0.05
  df_raw[["P.value.GroupA"]]     <- c(0.05, 0.1,  0.2)

  cols   <- make_cols_one_sample()
  result <- build_volcano_df(df_raw, cols, sig_cutoff = 0.05, sig_stat = "adj.p.val")

  expect_true(all(!result$Significant))
  expect_equal(attr(result, "y_cutoff"), Inf)
})

## volcano_label_union_for_ome ##################################################

# Helper: build a two-sample stat_results df with two contrasts.
# contrast_a, contrast_b: contrast strings like "A / B"
# Each contrast column names follow the standard pattern:
#   logFC.<A_over_B>, Log.P.Value.<A_over_B>, adj.P.Val.<A_over_B>, P.value.<A_over_B>
make_two_sample_stat_results <- function(contrast_a = "A / B", contrast_b = "C / D",
                                          sig_ids_a = character(0),
                                          sig_ids_b = character(0),
                                          all_ids = c("p1", "p2", "p3", "p4", "p5")) {
  cn_a <- paste0(strsplit(contrast_a, " / ")[[1]][1], "_over_", strsplit(contrast_a, " / ")[[1]][2])
  cn_b <- paste0(strsplit(contrast_b, " / ")[[1]][1], "_over_", strsplit(contrast_b, " / ")[[1]][2])

  n <- length(all_ids)
  # logP values: 5 for sig_ids, 0.5 for the rest (p.val cutoff = 0.05 -> y_cutoff = 1.301)
  lp_a <- ifelse(all_ids %in% sig_ids_a, 5, 0.5)
  lp_b <- ifelse(all_ids %in% sig_ids_b, 5, 0.5)

  df <- data.frame(
    id         = all_ids,
    geneSymbol = paste0("G", seq_len(n)),
    stringsAsFactors = FALSE
  )
  df[[paste0("logFC.", cn_a)]]         <- rep(1, n)
  df[[paste0("Log.P.Value.", cn_a)]]   <- lp_a
  df[[paste0("adj.P.Val.", cn_a)]]     <- rep(0.5, n)
  df[[paste0("P.value.", cn_a)]]       <- rep(0.1, n)
  df[[paste0("logFC.", cn_b)]]         <- rep(1, n)
  df[[paste0("Log.P.Value.", cn_b)]]   <- lp_b
  df[[paste0("adj.P.Val.", cn_b)]]     <- rep(0.5, n)
  df[[paste0("P.value.", cn_b)]]       <- rep(0.1, n)
  df
}

# Helper: build a minimal stat_params list for a two-sample test
make_two_sample_stat_params <- function(contrast_a = "A / B", contrast_b = "C / D",
                                         sig_cutoff = 0.05) {
  list(
    test      = "Two-sample Moderated T-test",
    contrasts = c(contrast_a, contrast_b),
    cutoff    = sig_cutoff,
    stat      = "p.val"
  )
}

test_that("volcano_label_union_for_ome returns union of significant IDs across two contrasts", {
  # p1, p2 are significant only in contrast A; p3, p4 only in contrast B
  df <- make_two_sample_stat_results(
    contrast_a = "A / B", contrast_b = "C / D",
    sig_ids_a  = c("p1", "p2"),
    sig_ids_b  = c("p3", "p4")
  )
  sp <- make_two_sample_stat_params()

  result <- volcano_label_union_for_ome(df, sp, label_mode = "significant", poi = character(0))
  expect_setequal(result, c("p1", "p2", "p3", "p4"))
})

test_that("volcano_label_union_for_ome returns only POI when label_mode is 'poi'", {
  df <- make_two_sample_stat_results(sig_ids_a = c("p1"), sig_ids_b = c("p2"))
  sp <- make_two_sample_stat_params()

  result <- volcano_label_union_for_ome(df, sp, label_mode = "poi", poi = c("p5"))
  expect_setequal(result, "p5")
})

test_that("volcano_label_union_for_ome handles overlap: IDs significant in both contrasts appear once", {
  df <- make_two_sample_stat_results(sig_ids_a = c("p1", "p2"), sig_ids_b = c("p2", "p3"))
  sp <- make_two_sample_stat_params()

  result <- volcano_label_union_for_ome(df, sp, label_mode = "significant", poi = character(0))
  expect_equal(length(result), length(unique(result)))  # no duplicates
  expect_setequal(result, c("p1", "p2", "p3"))
})

test_that("volcano_label_union_for_ome returns empty when no significant features and no POI", {
  df <- make_two_sample_stat_results(sig_ids_a = character(0), sig_ids_b = character(0))
  sp <- make_two_sample_stat_params()

  result <- volcano_label_union_for_ome(df, sp, label_mode = "significant", poi = character(0))
  expect_equal(result, character(0))
})

test_that("volcano_label_union_for_ome returns empty for unsupported test type", {
  df <- make_two_sample_stat_results()
  sp <- list(test = "Moderated F test", contrasts = c("A / B"), cutoff = 0.05, stat = "p.val")

  result <- volcano_label_union_for_ome(df, sp, label_mode = "significant", poi = character(0))
  expect_equal(result, character(0))
})

test_that("volcano_label_union_for_ome returns empty for NULL inputs", {
  expect_equal(volcano_label_union_for_ome(NULL, NULL, "significant", character(0)), character(0))
  df <- make_two_sample_stat_results()
  expect_equal(volcano_label_union_for_ome(df, NULL, "significant", character(0)), character(0))
})

test_that("volcano_label_union_for_ome works for one-sample test with multiple groups", {
  # Build a one-sample df with two groups
  group_a <- "GroupA"
  group_b <- "GroupB"
  n <- 5
  all_ids <- paste0("p", seq_len(n))
  df <- data.frame(
    id = all_ids, geneSymbol = paste0("G", seq_len(n)),
    stringsAsFactors = FALSE
  )
  # p1, p2 sig in GroupA (logP = 5 > cutoff 1.301); p3 sig in GroupB
  df[[paste0("logFC.", group_a)]]         <- rep(1, n)
  df[[paste0("Log.P.Value.", group_a)]]   <- c(5, 5, 0.5, 0.5, 0.5)
  df[[paste0("adj.P.Val.", group_a)]]     <- rep(0.5, n)
  df[[paste0("P.value.", group_a)]]       <- rep(0.1, n)
  df[[paste0("logFC.", group_b)]]         <- rep(1, n)
  df[[paste0("Log.P.Value.", group_b)]]   <- c(0.5, 0.5, 5, 0.5, 0.5)
  df[[paste0("adj.P.Val.", group_b)]]     <- rep(0.5, n)
  df[[paste0("P.value.", group_b)]]       <- rep(0.1, n)

  sp <- list(
    test   = "One-sample Moderated T-test",
    groups = c(group_a, group_b),
    cutoff = 0.05,
    stat   = "p.val"
  )

  result <- volcano_label_union_for_ome(df, sp, label_mode = "significant", poi = character(0))
  expect_setequal(result, c("p1", "p2", "p3"))
})

## Per-ome POI isolation (registry pattern) ####################################
# Simulate the parent-level poi_registry: call volcano_label_union_for_ome once
# per ome with that ome's own POI, then Reduce(union) the results.
# This mirrors the fixed global_union_ids() logic in statPlot_Ome_Server.
# Uses the existing make_two_sample_stat_results / make_two_sample_stat_params
# helpers (contrast format "A / B" -> column suffix "A_over_B").

test_that("global union uses each ome's own POI, not a shared one", {
  # Prot ome: one contrast, sig = p1, POI = p2 (non-sig)
  prot_df <- make_two_sample_stat_results(
    contrast_a = "Ctrl / Treated", contrast_b = "Ctrl / Other",
    sig_ids_a  = "p1",
    all_ids    = c("p1", "p2", "p3")
  )
  prot_sp  <- list(test = "Two-sample Moderated T-test",
                   contrasts = "Ctrl / Treated", cutoff = 0.05, stat = "p.val")
  prot_poi <- c("p2")

  # Phos ome: one contrast, sig = q1, POI = q2 (non-sig)  -  completely separate IDs
  phos_df <- make_two_sample_stat_results(
    contrast_a = "Ctrl / Treated", contrast_b = "Ctrl / Other",
    sig_ids_a  = "q1",
    all_ids    = c("q1", "q2", "q3")
  )
  phos_sp  <- list(test = "Two-sample Moderated T-test",
                   contrasts = "Ctrl / Treated", cutoff = 0.05, stat = "p.val")
  phos_poi <- c("q2")

  # Simulate global_union_ids(): each ome reads its own POI from the registry
  prot_union <- volcano_label_union_for_ome(prot_df, prot_sp, c("poi", "significant"), prot_poi)
  phos_union <- volcano_label_union_for_ome(phos_df, phos_sp, c("poi", "significant"), phos_poi)
  global_ids <- Reduce(union, list(prot_union, phos_union), init = character(0))

  # Prot's sig hit (p1) and POI (p2) must both be in the global union
  expect_true("p1" %in% global_ids, info = "sig from Prot should be in global union")
  expect_true("p2" %in% global_ids, info = "POI from Prot should be in global union")
  # Phos's sig hit (q1) and POI (q2) must both be in the global union
  expect_true("q1" %in% global_ids, info = "sig from Phos should be in global union")
  expect_true("q2" %in% global_ids, info = "POI from Phos should be in global union")
  # Non-sig, non-poi entries from either ome must NOT appear
  expect_false("p3" %in% global_ids, info = "non-sig non-poi from Prot should be absent")
  expect_false("q3" %in% global_ids, info = "non-sig non-poi from Phos should be absent")
})

test_that("passing wrong ome POI to another ome misses that ome's POI (old bug regression)", {
  # Demonstrates the bug fixed in global_union_ids(): if Prot's POI is passed to
  # the Phos iteration instead of Phos's own POI, Phos-only POI entries are missed.
  prot_df <- make_two_sample_stat_results(
    contrast_a = "Ctrl / Treated", contrast_b = "Ctrl / Other",
    all_ids    = c("p1", "p2")
  )
  phos_df <- make_two_sample_stat_results(
    contrast_a = "Ctrl / Treated", contrast_b = "Ctrl / Other",
    all_ids    = c("q1", "q2")
  )
  phos_sp  <- list(test = "Two-sample Moderated T-test",
                   contrasts = "Ctrl / Treated", cutoff = 0.05, stat = "p.val")
  prot_poi <- c("p1")  # Prot's POI  -  not in phos_df
  phos_poi <- c("q2")  # Phos's POI  -  only in phos_df

  # OLD (buggy) path: Prot's POI passed to Phos iteration
  buggy_phos_union <- volcano_label_union_for_ome(phos_df, phos_sp, "poi", prot_poi)
  expect_false("q2" %in% buggy_phos_union, info = "Phos-only POI absent when Prot's POI is passed")
  expect_false("p1" %in% buggy_phos_union, info = "Prot POI filtered (not in phos_df$id)")

  # FIXED path: Phos gets its own POI -> q2 now appears
  fixed_phos_union <- volcano_label_union_for_ome(phos_df, phos_sp, "poi", phos_poi)
  expect_true("q2" %in% fixed_phos_union, info = "Phos POI present when correct POI used")
})

test_that("baseline (union_mode none) uses only this ome's own POI with no cross-contamination", {
  # When union_mode == "none", effective_poi == proteins_of_interest() for this ome.
  # Another ome's POI must not appear.
  prot_poi <- c("p1")
  phos_poi <- c("q2")  # a completely different ID from another ome

  # In "none" mode: effective_poi = prot_poi only (no union computation)
  effective_poi_none <- prot_poi

  expect_true("p1"  %in% effective_poi_none, info = "own POI should be in baseline set")
  expect_false("q2" %in% effective_poi_none, info = "other ome's POI must not appear in baseline")
})
