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
