################################################################################
# Phase 5: GCT export for LM stat results.
#
# Mirrors the Statistics module's dual-export pattern (CSV + GCT for SSGSEA).
# The GCT @mat slot holds the signed-log p-value columns (one per coefficient),
# matching how `tab_stat_summary.R:700-707` constructs its export.
################################################################################

library(testthat)


# Test the pure builder (no shiny). We extract a helper from the module so
# the assembly logic can be unit-tested.

test_that("build_lm_stat_gct produces an SSGSEA-ready GCT", {
  # Mock a small LM result frame: id + per-coef logFC/P.Value/adj.P.Val/logSignP
  # plus a few sample columns.
  set.seed(701)
  n <- 8
  df <- data.frame(
    id = paste0("f", 1:n),
    geneSymbol = paste0("G", 1:n),
    logFC.cond_B = rnorm(n),
    P.Value.cond_B = runif(n),
    adj.P.Val.cond_B = runif(n),
    logSignP.cond_B = rnorm(n),
    logFC.cond_C = rnorm(n),
    P.Value.cond_C = runif(n),
    adj.P.Val.cond_C = runif(n),
    logSignP.cond_C = rnorm(n),
    s1 = rnorm(n),
    s2 = rnorm(n),
    s3 = rnorm(n),
    stringsAsFactors = FALSE
  )

  gct <- build_lm_stat_gct(df)
  expect_s4_class(gct, "GCT")
  # @mat columns are the logSignP columns (one per coefficient).
  expect_setequal(colnames(gct@mat), c("logSignP.cond_B", "logSignP.cond_C"))
  expect_equal(nrow(gct@mat), n)
  # All feature ids are preserved as rid.
  expect_equal(gct@rid, df$id)
  # rdesc carries everything that isn't a logSignP column (including the raw
  # sample columns and the other per-coef stats — those are NOT in @mat).
  expect_true("geneSymbol" %in% colnames(gct@rdesc))
  expect_true("logFC.cond_B" %in% colnames(gct@rdesc))
  expect_false(any(startsWith(colnames(gct@rdesc), "logSignP.")))
})


test_that("build_lm_stat_gct returns NULL when no logSignP columns exist", {
  df <- data.frame(id = paste0("f", 1:3), x = 1:3)
  expect_null(build_lm_stat_gct(df))
})


test_that("lm_results_gct_export writes a parseable .gct file", {
  skip_if_not_installed("cmapR")
  set.seed(702)
  n <- 6
  df <- data.frame(
    id = paste0("f", 1:n),
    logSignP.A_vs_B = rnorm(n),
    s1 = rnorm(n),
    s2 = rnorm(n),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = "")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  write_lm_stat_gct(df, dir_name = tmp, ome = "protein")
  out_path <- file.path(tmp, "lm_stat_results_for_ssGSEA_protein.gct")
  expect_true(file.exists(out_path))
  parsed <- cmapR::parse_gctx(out_path)
  expect_equal(nrow(parsed@mat), n)
  expect_true("logSignP.A_vs_B" %in% colnames(parsed@mat))
})
