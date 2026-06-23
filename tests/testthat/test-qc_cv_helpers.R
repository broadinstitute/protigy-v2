################################################################################
# Tests for R/tab_qc_cv_helpers.R
################################################################################

# --------------------------------------------------------------------------- #
# combine_cdesc_cols                                                           #
# --------------------------------------------------------------------------- #

test_that("combine_cdesc_cols single column returns as.character of that column", {
  df <- data.frame(A = c("ctrl", "treat", "ctrl"), stringsAsFactors = FALSE)
  expect_identical(combine_cdesc_cols(df, "A"), c("ctrl", "treat", "ctrl"))
})

test_that("combine_cdesc_cols two columns pastes with default underscore separator", {
  df <- data.frame(A = c("ctrl", "treat"), B = c("T0", "T6"), stringsAsFactors = FALSE)
  expect_identical(combine_cdesc_cols(df, c("A", "B")), c("ctrl_T0", "treat_T6"))
})

test_that("combine_cdesc_cols replaces NA with literal string 'NA'", {
  df <- data.frame(A = c("ctrl", NA, "treat"), stringsAsFactors = FALSE)
  result <- combine_cdesc_cols(df, "A")
  expect_identical(result, c("ctrl", "NA", "treat"))
})

test_that("combine_cdesc_cols honors custom sep", {
  df <- data.frame(A = c("a", "b"), B = c("x", "y"), stringsAsFactors = FALSE)
  expect_identical(combine_cdesc_cols(df, c("A", "B"), sep = "|"), c("a|x", "b|y"))
})

test_that("combine_cdesc_cols errors when a requested column is missing", {
  df <- data.frame(A = c("a", "b"), stringsAsFactors = FALSE)
  expect_error(combine_cdesc_cols(df, c("A", "NOTHERE")))
})

# --------------------------------------------------------------------------- #
# compute_cv_table                                                             #
# --------------------------------------------------------------------------- #

# Hand-computed expected values:
# Group A samples: col 1 (1, 3), col 2 (3, 5)
#   row 1: mu=2, sd=sqrt(2), cv=sqrt(2)/2 ~ 0.7071
#   row 2: mu=4, sd=sqrt(2), cv=sqrt(2)/4 ~ 0.3536
# Group B samples: col 3 (2, 6), col 4 (4, 8)
#   row 1: mu=3, sd=sqrt(2), cv=sqrt(2)/3 ~ 0.4714
#   row 2: mu=7, sd=sqrt(2), cv=sqrt(2)/7 ~ 0.2020

test_that("compute_cv_table basic 2x4 matrix with 2 groups matches hand-computed CV", {
  mat <- matrix(c(1, 3, 3, 5, 2, 6, 4, 8), nrow = 2,
                dimnames = list(c("f1", "f2"), NULL))
  grouping <- c("A", "A", "B", "B")
  result <- compute_cv_table(mat, grouping)
  expect_equal(nrow(result), 2L)
  expect_true("id" %in% names(result))
  expect_true("CV_A" %in% names(result))
  expect_true("CV_B" %in% names(result))
  expect_equal(result$CV_A[1], sd(c(1, 3)) / mean(c(1, 3)), tolerance = 1e-6)
  expect_equal(result$CV_A[2], sd(c(3, 5)) / mean(c(3, 5)), tolerance = 1e-6)
  expect_equal(result$CV_B[1], sd(c(2, 4)) / mean(c(2, 4)), tolerance = 1e-6)
  expect_equal(result$CV_B[2], sd(c(6, 8)) / mean(c(6, 8)), tolerance = 1e-6)
})

test_that("compute_cv_table default log_base = 'None' is identity (linear, unchanged)", {
  mat <- matrix(c(1, 3, 3, 5, 2, 6, 4, 8), nrow = 2,
                dimnames = list(c("f1", "f2"), NULL))
  grouping <- c("A", "A", "B", "B")
  # explicit "None" must match the default (no delinearization)
  expect_equal(compute_cv_table(mat, grouping),
               compute_cv_table(mat, grouping, log_base = "None"))
})

test_that("compute_cv_table delinearizes a log2 matrix before computing CV", {
  # CV is NOT invariant under log: it must be computed on raw (linear)
  # intensities. With log_base = 'log2' the matrix must be 2^x first.
  # matrix() fills column-major: f1 = (2,16,4,32), f2 = (8,64,16,128).
  lin <- matrix(c(2, 8, 16, 64, 4, 16, 32, 128), nrow = 2,
                dimnames = list(c("f1", "f2"), NULL))
  log2_mat <- log2(lin)
  grouping <- c("A", "A", "B", "B")

  result <- compute_cv_table(log2_mat, grouping, log_base = "log2")

  # Expected CV is computed on the LINEAR values.
  # f1: A = (2,16), B = (4,32); f2: A = (8,64), B = (16,128).
  expect_equal(result$CV_A[1], sd(c(2, 16)) / mean(c(2, 16)), tolerance = 1e-6)
  expect_equal(result$CV_B[1], sd(c(4, 32)) / mean(c(4, 32)), tolerance = 1e-6)
  expect_equal(result$CV_A[2], sd(c(8, 64)) / mean(c(8, 64)), tolerance = 1e-6)
  # Computing CV on the log values directly (the bug) would give a different,
  # smaller number, so this differs from the wrong result.
  expect_false(isTRUE(all.equal(
    result$CV_A[1], sd(log2(c(2, 16))) / mean(log2(c(2, 16))), tolerance = 1e-6
  )))
})

test_that("compute_cv_table delinearizes a log10 matrix before computing CV", {
  lin <- matrix(c(10, 100, 1000, 10000), nrow = 1,
                dimnames = list("f1", NULL))
  log10_mat <- log10(lin)
  grouping <- c("A", "A", "B", "B")
  result <- compute_cv_table(log10_mat, grouping, log_base = "log10")
  expect_equal(result$CV_A, sd(c(10, 100)) / mean(c(10, 100)), tolerance = 1e-6)
  expect_equal(result$CV_B, sd(c(1000, 10000)) / mean(c(1000, 10000)),
               tolerance = 1e-6)
})

test_that("compute_cv_table handles NA values with na.rm = TRUE", {
  mat <- matrix(c(1, NA, 3, 5), nrow = 1,
                dimnames = list("f1", NULL))
  grouping <- c("A", "A", "B", "B")
  result <- compute_cv_table(mat, grouping)
  # Group A: only col 1 (value 1); sd of a single non-NA = NA, so CV = NA
  expect_true(is.na(result$CV_A))
  # Group B: (3, 5); cv = sd(3,5)/mean(3,5)
  expect_equal(result$CV_B, sd(c(3, 5)) / mean(c(3, 5)), tolerance = 1e-6)
})

test_that("compute_cv_table single-sample group yields NA CV without crashing", {
  # 2 features, 3 samples: group A has 1 sample, group B has 2 samples
  mat <- matrix(c(5, 10, 3, 6, 4, 8), nrow = 2,
                dimnames = list(c("f1", "f2"), NULL))
  grouping <- c("A", "B", "B")
  result <- compute_cv_table(mat, grouping)
  # Group A has only 1 sample, sd = NA
  expect_true(all(is.na(result$CV_A)))
  # Group B has 2 samples, CV should be finite
  expect_true(all(!is.na(result$CV_B)))
})

test_that("compute_cv_table errors when grouping length != ncol(mat)", {
  mat <- matrix(1:6, nrow = 2, dimnames = list(c("f1", "f2"), NULL))
  expect_error(compute_cv_table(mat, c("A", "B")))  # length 2, ncol = 3
})

test_that("compute_cv_table errors on non-numeric matrix", {
  mat <- matrix(c("a", "b", "c", "d"), nrow = 2)
  expect_error(compute_cv_table(mat, c("A", "B")))
})

test_that("compute_cv_table preserves feature IDs in the id column", {
  mat <- matrix(c(1, 2, 3, 4), nrow = 2,
                dimnames = list(c("prot_A", "prot_B"), NULL))
  grouping <- c("G1", "G1")
  result <- compute_cv_table(mat, grouping)
  expect_identical(result$id, c("prot_A", "prot_B"))
})

# --------------------------------------------------------------------------- #
# filter_cv_table                                                              #
# --------------------------------------------------------------------------- #

test_that("filter_cv_table min_groups='one' keeps rows where at least one group's CV is below cutoff", {
  cv_df <- data.frame(
    id   = c("f1", "f2", "f3"),
    CV_A = c(0.1,  0.5,  0.6),
    CV_B = c(0.5,  0.1,  0.7),
    stringsAsFactors = FALSE
  )
  result <- filter_cv_table(cv_df, cutoff = 0.2, min_groups = "one")
  expect_identical(result$id, c("f1", "f2"))  # f3: both >= 0.2
})

test_that("filter_cv_table min_groups='all' keeps only rows where every group's CV is below cutoff", {
  cv_df <- data.frame(
    id   = c("f1", "f2", "f3"),
    CV_A = c(0.1,  0.5,  0.1),
    CV_B = c(0.1,  0.1,  0.5),
    stringsAsFactors = FALSE
  )
  result <- filter_cv_table(cv_df, cutoff = 0.2, min_groups = "all")
  expect_identical(result$id, "f1")  # f2: CV_A >= 0.2; f3: CV_B >= 0.2
})

test_that("filter_cv_table treats NA CVs as not satisfying the cutoff", {
  cv_df <- data.frame(
    id   = c("f1", "f2"),
    CV_A = c(NA,   0.1),
    CV_B = c(0.1,  0.1),
    stringsAsFactors = FALSE
  )
  # min_groups = "all": f1 has NA in CV_A -> NA is not < cutoff -> excluded
  result_all <- filter_cv_table(cv_df, cutoff = 0.2, min_groups = "all")
  expect_identical(result_all$id, "f2")

  # min_groups = "one": f1 has CV_B = 0.1 < 0.2 -> included
  result_one <- filter_cv_table(cv_df, cutoff = 0.2, min_groups = "one")
  expect_identical(result_one$id, c("f1", "f2"))
})

test_that("filter_cv_table invalid min_groups value errors via match.arg", {
  cv_df <- data.frame(id = "f1", CV_A = 0.1, stringsAsFactors = FALSE)
  expect_error(filter_cv_table(cv_df, cutoff = 0.2, min_groups = "both"))
})

# --------------------------------------------------------------------------- #
# create_cv_violin_plot                                                       #
# --------------------------------------------------------------------------- #

test_that("create_cv_violin_plot returns ggplot with linear CV y-axis label", {
  cv_df <- data.frame(
    id = c("f1", "f2", "f3"),
    CV_A = c(0.10, 0.20, 0.30),
    CV_B = c(0.15, 0.25, 0.35),
    stringsAsFactors = FALSE
  )
  palette <- c(A = "#1b9e77", B = "#d95f02")

  p <- create_cv_violin_plot(cv_df, palette = palette, log_scale = FALSE)

  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$y, "CV")
  y_scale <- p$scales$get_scales("y")
  expect_true(grepl("identity", y_scale$trans$name, fixed = TRUE))
})

test_that("create_cv_violin_plot uses log10 y-axis label and transform in log mode", {
  cv_df <- data.frame(
    id = c("f1", "f2", "f3"),
    CV_A = c(0.10, 0.20, 0.30),
    CV_B = c(0.15, 0.25, 0.35),
    stringsAsFactors = FALSE
  )
  palette <- c(A = "#1b9e77", B = "#d95f02")

  p <- create_cv_violin_plot(cv_df, palette = palette, log_scale = TRUE)

  expect_identical(p$labels$y, "log10(CV)")
  y_scale <- p$scales$get_scales("y")
  expect_true(grepl("log-10", y_scale$trans$name, fixed = TRUE))
})

test_that("create_cv_violin_plot keeps box fill transparent and supports y_range zoom", {
  cv_df <- data.frame(
    id = c("f1", "f2", "f3"),
    CV_A = c(0.10, 0.20, 0.30),
    CV_B = c(0.15, 0.25, 0.35),
    stringsAsFactors = FALSE
  )
  palette <- c(A = "#1b9e77", B = "#d95f02")

  p <- create_cv_violin_plot(cv_df, palette = palette, y_range = c(0.05, 0.40))

  # Layer order in helper: violin first, boxplot second
  expect_true(length(p$layers) >= 2L)
  expect_identical(p$layers[[2]]$aes_params$fill, NA)
  expect_s3_class(p$coordinates, "CoordCartesian")
  expect_equal(p$coordinates$limits$y, c(0.05, 0.40))
})

# ---------------------------------------------------------------------------
# CV intensity-source wording (source-level guard)
#
# compute_cv_table receives GCT_processed()@mat, which is log-transformed AND
# normalized. pelsa_delinearize reverses only the log base, NOT normalization,
# so the CV is computed on delinearized-but-still-normalized intensities -- NOT
# strictly "raw" intensities. The UI note (tab_qc_cv.R) and the helper comment
# (tab_qc_cv_helpers.R) must not over-claim "raw", and must acknowledge that
# normalization stays applied. Reverting either to the "raw (linear)" wording
# fails this test.
# ---------------------------------------------------------------------------

test_that("CV note + comment do not over-claim 'raw' intensities", {
  note_path   <- testthat::test_path("..", "..", "R", "tab_qc_cv.R")
  helper_path <- testthat::test_path("..", "..", "R", "tab_qc_cv_helpers.R")
  # R/ source is absent under R CMD check (installed package); skip like the
  # other source-level guard tests rather than erroring on a missing file.
  skip_if_not(file.exists(note_path) && file.exists(helper_path),
              "tab_qc_cv.R / tab_qc_cv_helpers.R source not found")

  note   <- paste(readLines(note_path, warn = FALSE), collapse = "\n")
  helper <- paste(readLines(helper_path, warn = FALSE), collapse = "\n")

  # The misleading "raw (linear)" claim must be gone from the user-facing note.
  expect_false(grepl("raw \\(linear\\)", note),
               info = "tab_qc_cv.R UI note must not claim CV is on raw (linear) intensities")
  # The note must acknowledge that normalization remains applied.
  expect_true(grepl("normaliz", note, ignore.case = TRUE),
              info = "tab_qc_cv.R UI note must mention normalization still applies")
  # The helper comment must not assert the matrix is RAW LINEAR.
  expect_false(grepl("RAW\\s*\\n?\\s*#?\\s*LINEAR|raw linear", helper, ignore.case = TRUE),
               info = "compute_cv_table comment must not claim raw linear intensities")
})
