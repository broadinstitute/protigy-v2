# Tests for batch contrast processing through the REAL stat.testing entry point.
#
# Previously this file defined its own copies of the limma pipeline
# (run_batch_contrasts / run_per_contrast_approach) and asserted on the copies,
# so stat.testing itself was never exercised. One copy even used a different model
# (cbind(ref = 1, as.numeric(groups))) than production
# (model.matrix(~ 0 + groups) + makeContrasts). Those tests reported coverage they
# did not provide. They are replaced here with tests that call stat.testing
# directly and assert its real multi-contrast output.

# stat.testing wraps its work in shiny::withProgress, which needs a live session.
# Drive it inside a trivial testServer module so withProgress/incProgress resolve.
run_stat_testing <- function(...) {
  args <- list(...)
  wrap <- function(id = "w") {
    shiny::moduleServer(id, function(input, output, session) {
      out <- shiny::reactiveVal(NULL)
      shiny::observe({ out(suppressMessages(do.call(stat.testing, args))) })
      out
    })
  }
  captured <- NULL
  shiny::testServer(wrap, {
    session$flushReact()
    captured <<- out()
  })
  captured
}

# Build a deterministic synthetic GCT with a known group structure.
make_contrast_gct <- function(n_genes = 40, groups = c("A", "B", "C"),
                              per_group = 4, seed = 101,
                              spike = NULL) {
  set.seed(seed)
  group_vec <- rep(groups, each = per_group)
  n_samples <- length(group_vec)
  samples <- paste0("sample_", seq_len(n_samples))
  genes <- paste0("gene_", seq_len(n_genes))

  mat <- matrix(rnorm(n_genes * n_samples), nrow = n_genes,
                dimnames = list(genes, samples))

  # Optionally make a feature strongly differential between two groups.
  if (!is.null(spike)) {
    target_cols <- which(group_vec == spike$group)
    mat[spike$gene, target_cols] <- mat[spike$gene, target_cols] + spike$shift
  }

  cdesc <- data.frame(test_group = group_vec, row.names = samples,
                      stringsAsFactors = FALSE)
  rdesc <- data.frame(id = genes, geneSymbol = paste0("SYM_", seq_len(n_genes)),
                      row.names = genes, stringsAsFactors = FALSE)

  new("GCT", mat = mat, cdesc = cdesc, rdesc = rdesc,
      rid = genes, cid = samples)
}

# Expected per-contrast columns produced by the two-sample branch.
contrast_cols <- function(contrast_name) {
  paste0(c("logFC", "P.Value", "adj.P.Val", "Log.P.Value",
           "sign.logP", "significant"), ".", contrast_name)
}

# ============================================================================
# Multiple contrasts through stat.testing
# ============================================================================

test_that("stat.testing produces correct columns for multiple contrasts", {
  gct <- make_contrast_gct(n_genes = 40, groups = c("A", "B", "C"), per_group = 4)
  contrasts_list <- list(c("A", "B"), c("A", "C"), c("B", "C"))

  result <- run_stat_testing(
    test = "Two-sample Moderated T-test",
    annotation_col = "test_group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A", "B", "C"),
    selected_contrasts = contrasts_list,
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  expect_true(is.list(result))
  expect_named(result, "proteome")
  df <- result$proteome
  expect_equal(nrow(df), 40)

  for (cn in c("A_over_B", "A_over_C", "B_over_C")) {
    for (col in contrast_cols(cn)) {
      expect_true(col %in% colnames(df),
                  info = paste("missing column:", col))
    }
  }
})

test_that("stat.testing two-sample p-values and adj.P.Val are sane", {
  gct <- make_contrast_gct(n_genes = 60, groups = c("A", "B"), per_group = 5)

  result <- run_stat_testing(
    test = "Two-sample Moderated T-test",
    annotation_col = "test_group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A", "B"),
    selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  p <- df$P.Value.A_over_B
  adj <- df$adj.P.Val.A_over_B

  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
  expect_true(all(adj >= 0 & adj <= 1, na.rm = TRUE))
  # BH-adjusted p-values are never smaller than the nominal p-value.
  expect_true(all(adj >= p - 1e-9, na.rm = TRUE))
  expect_true(is.logical(df$significant.A_over_B))
})

test_that("stat.testing flags a constructed strongly-differential feature", {
  # gene_1 is shifted by +10 in group A only -> large |logFC| and tiny p-value.
  gct <- make_contrast_gct(
    n_genes = 40, groups = c("A", "B", "C"), per_group = 4, seed = 7,
    spike = list(gene = "gene_1", group = "A", shift = 10)
  )

  result <- run_stat_testing(
    test = "Two-sample Moderated T-test",
    annotation_col = "test_group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A", "B", "C"),
    selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  spike_row <- df[df$id == "gene_1", ]

  # A is shifted up vs B; contrast A_over_B = A - B should be strongly positive.
  expect_gt(spike_row$logFC.A_over_B, 5)
  expect_lt(spike_row$P.Value.A_over_B, 1e-4)
  expect_true(spike_row$significant.A_over_B)
})

test_that("stat.testing single contrast yields one contrast block", {
  gct <- make_contrast_gct(n_genes = 30, groups = c("A", "B"), per_group = 4)

  result <- run_stat_testing(
    test = "Two-sample Moderated T-test",
    annotation_col = "test_group",
    chosen_omes = "proteome",
    gct = list(proteome = gct),
    chosen_groups = c("A", "B"),
    selected_contrasts = list(c("A", "B")),
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  df <- result$proteome
  expect_equal(nrow(df), 30)
  expect_true("logFC.A_over_B" %in% colnames(df))
  # No second contrast leaked in.
  expect_length(grep("^logFC\\.", colnames(df)), 1)
})

test_that("stricter alpha never marks more features significant", {
  gct <- make_contrast_gct(n_genes = 50, groups = c("A", "B"), per_group = 5,
                           seed = 21)

  run <- function(alpha) {
    res <- run_stat_testing(
      test = "Two-sample Moderated T-test",
      annotation_col = "test_group",
      chosen_omes = "proteome",
      gct = list(proteome = gct),
      chosen_groups = c("A", "B"),
      selected_contrasts = list(c("A", "B")),
      p.value.alpha = alpha,
      use.adj.pvalue = TRUE,
      apply.log = FALSE,
      intensity = FALSE
    )
    sum(res$proteome$significant.A_over_B, na.rm = TRUE)
  }

  expect_lte(run(0.01), run(0.10))
})

# ============================================================================
# Multi-ome regression guard (STAT-07 accumulator fix)
# ============================================================================

test_that("stat.testing returns results for EVERY ome (multi-ome multi-contrast)", {
  # Regression guard: a former bug reused `results_list` as the per-contrast
  # accumulator, clobbering the per-ome accumulator so only the last ome
  # survived. Assert every ome is present with every contrast.
  gct_a <- make_contrast_gct(n_genes = 30, groups = c("A", "B", "C"),
                             per_group = 4, seed = 1)
  gct_b <- make_contrast_gct(n_genes = 35, groups = c("A", "B", "C"),
                             per_group = 4, seed = 2)

  result <- run_stat_testing(
    test = "Two-sample Moderated T-test",
    annotation_col = "test_group",
    chosen_omes = c("proteome", "phospho"),
    gct = list(proteome = gct_a, phospho = gct_b),
    chosen_groups = c("A", "B", "C"),
    selected_contrasts = list(c("A", "B"), c("A", "C")),
    p.value.alpha = 0.05,
    use.adj.pvalue = TRUE,
    apply.log = FALSE,
    intensity = FALSE
  )

  expect_named(result, c("proteome", "phospho"), ignore.order = TRUE)
  expect_equal(nrow(result$proteome), 30)
  expect_equal(nrow(result$phospho), 35)

  for (ome in c("proteome", "phospho")) {
    for (cn in c("A_over_B", "A_over_C")) {
      expect_true(paste0("logFC.", cn) %in% colnames(result[[ome]]),
                  info = paste(ome, "missing", cn))
      expect_true(paste0("adj.P.Val.", cn) %in% colnames(result[[ome]]),
                  info = paste(ome, "missing adj.P.Val for", cn))
    }
  }
})
