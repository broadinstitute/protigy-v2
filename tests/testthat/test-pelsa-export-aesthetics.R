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
