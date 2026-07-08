test_that("volcano export subtitle capitalizes the word after the pipe", {
  # Minimal df with the columns .pelsa_export_ggplot consumes.
  df <- data.frame(
    logFC                = c(-1, 0, 1),
    logP                 = c(1, 0.2, 1.5),
    sig_direction        = c("down", "ns", "up"),
    feature_class_primary = c("none", "none", "none"),
    is_marker            = c(FALSE, FALSE, FALSE),
    label                = c("", "", ""),
    stringsAsFactors     = FALSE
  )
  attr(df, "y_cutoff") <- 1.0
  g <- .pelsa_export_ggplot(df, full_df = df, color_mode = "significance",
                            volcano_label = "All-peptide volcano")
  expect_equal(g$labels$subtitle, "All-peptide volcano | Significance coloring")
})

test_that("volcano export subtitle capitalizes feature coloring mode", {
  df <- data.frame(
    logFC                = c(-1, 0, 1),
    logP                 = c(1, 0.2, 1.5),
    sig_direction        = c("down", "ns", "up"),
    feature_class_primary = c("none", "none", "none"),
    is_marker            = c(FALSE, FALSE, FALSE),
    label                = c("", "", ""),
    stringsAsFactors     = FALSE
  )
  attr(df, "y_cutoff") <- 1.0
  g <- .pelsa_export_ggplot(df, full_df = df, color_mode = "feature",
                            volcano_label = "All-peptide volcano")
  expect_equal(g$labels$subtitle, "All-peptide volcano | Feature coloring")
})

test_that("intensity export subtitle shows peptide count and coverage", {
  ld <- data.frame(
    condition = factor(rep(c("A", "B"), each = 2), levels = c("A", "B")),
    mean_log2 = c(10, 11, 12, 13),
    peptide_seq = rep(c("PEPA", "PEPB"), times = 2),
    pep_occurrence_idx = rep(1L, 4),
    aa_label = rep(c("aa10", "aa20"), times = 2),
    panel = rep("Significant", 4),
    pep_start = rep(c(10L, 20L), times = 2),
    pep_end = rep(c(15L, 25L), times = 2),
    stringsAsFactors = FALSE
  )
  p <- pelsa_intensity_export_ggplot(ld, "GENE", "P12345", log_base = 2,
                                     coverage_frac = 0.4213)
  expect_equal(p$labels$subtitle, "2 peptides | 42.1% sequence coverage")
})

test_that("intensity export subtitle shows singular peptide when coverage is NA", {
  ld <- data.frame(
    condition = factor(c("A", "B"), levels = c("A", "B")),
    mean_log2 = c(10, 11),
    peptide_seq = c("PEPA", "PEPA"),
    pep_occurrence_idx = c(1L, 1L),
    aa_label = c("aa10", "aa10"),
    panel = c("Significant", "Significant"),
    pep_start = c(10L, 10L),
    pep_end = c(15L, 15L),
    stringsAsFactors = FALSE
  )
  p <- pelsa_intensity_export_ggplot(ld, "GENE", "P12345", log_base = 2)
  expect_equal(p$labels$subtitle, "1 peptide")
})

test_that("intensity export drops the x-axis title and bolds+blackens x text", {
  ld <- data.frame(
    condition = factor(rep(c("C1", "C2"), each = 2), levels = c("C1", "C2")),
    mean_log2 = c(10, 11, 9, 12),
    peptide_seq = rep(c("PEPA", "PEPB"), times = 2),
    pep_occurrence_idx = rep(1L, 4),
    panel = rep("Significant", 4),
    aa_label = rep(c("PEPA_aa1", "PEPB_aa2"), times = 2),
    pep_start = rep(c(10L, 20L), times = 2),
    pep_end = rep(c(15L, 25L), times = 2),
    stringsAsFactors = FALSE
  )
  g <- pelsa_intensity_export_ggplot(ld, gene = "GENE", accession = "P00001")
  expect_null(g$labels$x)
  expect_equal(g$theme$axis.text.x$colour, "black")
  expect_equal(g$theme$axis.text.x$face, "bold")
})

test_that("volcano export legend text is small (8pt) with a tight key", {
  # Reuse the existing fixture from the subtitle tests above.
  df <- data.frame(
    logFC                = c(-1, 0, 1),
    logP                 = c(1, 0.2, 1.5),
    sig_direction        = c("down", "ns", "up"),
    feature_class_primary = c("none", "none", "none"),
    is_marker            = c(FALSE, FALSE, FALSE),
    label                = c("", "", ""),
    stringsAsFactors     = FALSE
  )
  attr(df, "y_cutoff") <- 1.0
  g <- .pelsa_export_ggplot(df, full_df = df, color_mode = "significance",
                            label_mode = character(0))
  expect_lte(g$theme$legend.text$size, 8)
  expect_lte(as.numeric(g$theme$legend.key.size), 9)
})

test_that("pelsa_bar_export_width holds the floor for small bar counts", {
  expect_equal(pelsa_bar_export_width(1), 5.6)
  expect_equal(pelsa_bar_export_width(3), 5.6)
  expect_equal(pelsa_bar_export_width(9), 5.6)  # 0.6*9 = 5.4 < floor
})

test_that("pelsa_bar_export_width grows 0.6in per bar past the floor", {
  expect_equal(pelsa_bar_export_width(15), 9.0)   # 0.6*15
  expect_equal(pelsa_bar_export_width(30), 18.0)  # 0.6*30
})

test_that("pelsa_bar_export_width clamps to the 30in ceiling", {
  expect_equal(pelsa_bar_export_width(50), 30)
  expect_equal(pelsa_bar_export_width(200), 30)
})

test_that("pelsa_bar_export_width degrades non-positive / NA input to the floor", {
  expect_equal(pelsa_bar_export_width(0), 5.6)
  expect_equal(pelsa_bar_export_width(-5), 5.6)
  expect_equal(pelsa_bar_export_width(NA_integer_), 5.6)
})
