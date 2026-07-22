# Tests for the "Apply suggested alpha" wiring in the LM Results module.
#
# The alpha suggestion (suggest_alpha_level) is derived from the NOMINAL
# p-value distribution. Applying it must therefore also move the active
# statistic onto the nominal scale, otherwise a nominal-derived cutoff is
# silently applied against the adjusted-p scale (a category error).

# Build an lm_results list whose nominal p-values yield a real alpha
# suggestion: a near-uniform tail (so the KS-uniformity test passes) plus a
# handful of tiny p-values standing in for true signal.
make_lm_results <- function(coef = "groupB") {
  set.seed(42)
  nom_p <- c(seq(0.11, 0.99, length.out = 60), rep(0.001, 5))
  adj_p <- pmin(nom_p * 2, 1)
  df <- data.frame(
    id = paste0("feat_", seq_along(nom_p)),
    geneSymbol = paste0("GENE", seq_along(nom_p)),
    stringsAsFactors = FALSE
  )
  df[[paste0("logFC.", coef)]]    <- rnorm(length(nom_p))
  df[[paste0("P.Value.", coef)]]  <- nom_p
  df[[paste0("adj.P.Val.", coef)]] <- adj_p
  list(proteome = df)
}

# Drive lmResults_Ome_Server through shiny::testServer, fire the apply button,
# and return the resulting lm_params for the ome.
apply_alpha_and_read_params <- function(coef = "groupB",
                                        starting_stat = "adj.p.val") {
  lm_results_rv <- shiny::reactiveVal(make_lm_results(coef))
  lm_params_rv  <- shiny::reactiveVal(list(
    proteome = list(stat = starting_stat, cutoff = 0.05)
  ))

  captured <- NULL
  shiny::testServer(
    lmResults_Ome_Server,
    args = list(
      id = "proteome",
      ome = "proteome",
      GCT_processed = shiny::reactive(NULL),
      parameters = shiny::reactive(NULL),
      default_annotation_column = shiny::reactive(NULL),
      color_map = shiny::reactive(NULL),
      lm_params = lm_params_rv,
      lm_results = lm_results_rv
    ),
    {
      session$setInputs(pval_coefficient = coef)
      session$setInputs(apply_alpha_suggestion = 1)
      captured <<- lm_params_rv()[["proteome"]]
    }
  )
  captured
}

test_that("suggest_alpha_level yields a real alpha for the test fixture", {
  # Guards the fixture: if this returns NA the apply-button tests are vacuous.
  nom_p <- c(seq(0.11, 0.99, length.out = 60), rep(0.001, 5))
  suggestion <- suggest_alpha_level(nom_p)
  expect_false(is.na(suggestion$alpha))
})

test_that("Apply suggested alpha co-sets stat to nom.p.val", {
  params <- apply_alpha_and_read_params(starting_stat = "adj.p.val")
  expect_equal(params$stat, "nom.p.val")
})

test_that("Apply suggested alpha also sets the cutoff to the suggested value", {
  nom_p <- c(seq(0.11, 0.99, length.out = 60), rep(0.001, 5))
  expected_alpha <- suggest_alpha_level(nom_p)$alpha

  params <- apply_alpha_and_read_params(starting_stat = "adj.p.val")
  expect_equal(params$cutoff, expected_alpha)
})
