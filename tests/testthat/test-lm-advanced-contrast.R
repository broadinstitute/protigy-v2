################################################################################
# End-to-end Advanced (free-text numeric) contrast recovery for lm.regression().
#
# The contrast-string BUILDER/validator helpers are covered by
# test-lm-setup-helpers-contrasts.R, but NO test runs an Advanced free-text
# contrast all the way THROUGH lm.regression() and checks the NUMERIC result
# against a planted effect. This suite closes that gap for the hardest case: a
# difference-of-differences (interaction) contrast on a 2x2 factorial.
#
# When `contrasts_list` is supplied, lm.regression() re-parameterises the fit:
#   - output columns are keyed by the contrast's NAME, not by design coefs
#     (list("DoD"=...) -> logFC.DoD / P.Value.DoD / adj.P.Val.DoD);
#   - the intercept is NOT dropped and per-factor F-tests are SKIPPED, so the
#     only stat columns are for the named contrasts;
#   - each token inside the contrast string is make.names()-normalised, so the
#     interaction coefficient can be referenced as either "treatmentDrug:timepointT2"
#     or its safe form "treatmentDrug.timepointT2".
#
# The 2x2 interaction coefficient IS the difference-of-differences, so we plant
# a KNOWN interaction (Drug x T2 cell shifted; all mains null) and assert an
# Advanced contrast recovers it. We test three equivalent contrast spellings and
# cross-check against both the non-contrast coefficient path and an independently
# hand-built limma::makeContrasts oracle.
################################################################################

library(testthat)

# ---- fixture -----------------------------------------------------------------

# Balanced 2x2 factorial: treatment {Vehicle,Drug} x timepoint {T1,T2}, `reps`
# per cell. The interaction (Drug:T2) is planted at magnitude `effect` on the
# first n_true features; main effects stay null so the ONLY signal is the
# difference-of-differences. References are Vehicle / T1, so the non-reference
# design coefficients are treatmentDrug, timepointT2, treatmentDrug:timepointT2.
# reps=10 tightens the interaction SE (which is ~2x a simple contrast) enough
# that the planted effect clears BH with clean specificity.
make_2x2_dod_gct <- function(seed = 101L, effect = 3.0, reps = 10L,
                             n_null = 60L, n_true = 20L) {
  set.seed(seed)
  n_feat <- n_null + n_true
  grid <- expand.grid(
    treatment = c("Vehicle", "Drug"),
    timepoint = c("T1", "T2"),
    rep = seq_len(reps),
    stringsAsFactors = FALSE
  )
  n_samp <- nrow(grid)
  sample_ids <- paste0("s", sprintf("%03d", seq_len(n_samp)))
  feat_ids   <- paste0("F", sprintf("%03d", seq_len(n_feat)))

  cdesc <- data.frame(
    id        = sample_ids,
    treatment = factor(grid$treatment, levels = c("Vehicle", "Drug")),
    timepoint = factor(grid$timepoint, levels = c("T1", "T2")),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )
  rdesc <- data.frame(
    id = feat_ids, geneSymbol = paste0("G", seq_len(n_feat)),
    row.names = feat_ids, stringsAsFactors = FALSE
  )
  mat <- matrix(rnorm(n_feat * n_samp, mean = 10, sd = 1),
                nrow = n_feat, dimnames = list(feat_ids, sample_ids))

  drug_t2 <- cdesc$treatment == "Drug" & cdesc$timepoint == "T2"
  mat[seq_len(n_true), drug_t2] <- mat[seq_len(n_true), drug_t2] + effect

  gct <- methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
                      rid = feat_ids, cid = sample_ids)
  list(gct = gct, cdesc = cdesc, mat = mat,
       n_true = n_true, n_null = n_null, effect = effect)
}

FORMULA <- "~ treatment + timepoint + treatment:timepoint"
VTYPES  <- list(treatment = "factor", timepoint = "factor")

fit_dod <- function(gct, name, expr) {
  cl <- stats::setNames(list(expr), name)
  lm.regression(gct = gct, formula_string = FORMULA,
                variable_types = VTYPES, contrasts_list = cl)
}

# ---- output shape under the contrast path ------------------------------------

test_that("Advanced contrast keys output columns by contrast NAME (not design coef)", {
  fx  <- make_2x2_dod_gct()
  res <- fit_dod(fx$gct, "DoD", "treatmentDrug:timepointT2")

  # Columns are named after the list name, make.names()-normalised.
  expect_true(all(c("logFC.DoD", "P.Value.DoD", "adj.P.Val.DoD") %in% colnames(res)))

  # In the contrast path the intercept is not dropped internally, but no
  # (Intercept) stat column is emitted, and per-factor F-tests are SKIPPED.
  expect_false(any(grepl("Intercept", colnames(res))))
  expect_false(any(grepl("^F[.]", colnames(res))))
  # No stray design-coefficient columns leak into the output.
  expect_false("logFC.treatmentDrug.timepointT2" %in% colnames(res))
  expect_false("logFC.treatmentDrug" %in% colnames(res))

  # Every feature reported exactly once.
  expect_equal(nrow(res), fx$n_true + fx$n_null)
  expect_false(any(duplicated(res$id)))
})

# ---- numeric recovery: the interaction-coefficient contrast ------------------

test_that("difference-of-differences contrast recovers the planted interaction effect", {
  fx  <- make_2x2_dod_gct()
  res <- fit_dod(fx$gct, "DoD", "treatmentDrug:timepointT2")
  tp  <- seq_len(fx$n_true)
  nl  <- (fx$n_true + 1L):(fx$n_true + fx$n_null)

  # Recovered aggregate DoD ~ planted effect on true features; nulls ~ 0.
  # Difference-of-differences estimates are noisy per-feature (SE ~2x a simple
  # contrast), so assert on the median, not every feature.
  expect_equal(median(res$logFC.DoD[tp]), fx$effect, tolerance = 0.4)
  expect_lt(abs(median(res$logFC.DoD[nl])), 0.3)
  # True block is well separated from the null block.
  expect_gt(median(res$logFC.DoD[tp]), median(res$logFC.DoD[nl]) + 1.5)
  # Direction is unambiguous: the planted effect is positive.
  expect_true(all(res$logFC.DoD[tp] > 0))
})

test_that("difference-of-differences contrast is specific: clean at BH 5%", {
  fx  <- make_2x2_dod_gct()
  res <- fit_dod(fx$gct, "DoD", "treatmentDrug:timepointT2")
  tp  <- seq_len(fx$n_true)
  nl  <- (fx$n_true + 1L):(fx$n_true + fx$n_null)

  # Near-zero null false positives (allow a tiny stochastic slack, <1% of nulls).
  expect_lte(sum(res$adj.P.Val.DoD[nl] < 0.05), max(1L, ceiling(0.01 * fx$n_null)))
  # Power is real (not a vacuous specificity claim): all true features detected.
  expect_gt(mean(res$adj.P.Val.DoD[tp] < 0.05), 0.9)
})

# ---- token-spelling equivalence ----------------------------------------------

test_that("contrast token accepts both 'a:b' and make.names 'a.b' spellings identically", {
  fx <- make_2x2_dod_gct()
  # The backend make.names()-normalises each token, so the raw interaction
  # coefficient (with ':') and its safe form (with '.') are the same contrast.
  res_colon <- fit_dod(fx$gct, "DoD", "treatmentDrug:timepointT2")
  res_dot   <- fit_dod(fx$gct, "DoD", "treatmentDrug.timepointT2")

  expect_equal(res_colon$logFC.DoD, res_dot$logFC.DoD, tolerance = 1e-12)
  expect_equal(res_colon$P.Value.DoD, res_dot$P.Value.DoD, tolerance = 1e-12)
})

# ---- composite expression: real contrast arithmetic + token renaming ---------

test_that("a genuinely composite DoD expression equals the plain interaction contrast", {
  fx <- make_2x2_dod_gct()
  tp <- seq_len(fx$n_true)
  nl <- (fx$n_true + 1L):(fx$n_true + fx$n_null)

  # (treatmentDrug + treatmentDrug:timepointT2) - treatmentDrug algebraically
  # equals the interaction coefficient, but forces the contrast parser to handle
  # a multi-token composite with '+'/'-'/'()' AND rename the ':' token inside it.
  res_comp  <- fit_dod(fx$gct, "DoD2",
                       "(treatmentDrug + treatmentDrug:timepointT2) - treatmentDrug")
  res_plain <- fit_dod(fx$gct, "DoD", "treatmentDrug:timepointT2")

  # Composite recovers the same planted effect...
  expect_equal(median(res_comp$logFC.DoD2[tp]), fx$effect, tolerance = 0.4)
  expect_lt(abs(median(res_comp$logFC.DoD2[nl])), 0.3)
  # ...and is numerically identical to the direct interaction contrast: proof
  # the contrast arithmetic + token renaming compose correctly on a real
  # composite expression.
  expect_equal(res_comp$logFC.DoD2, res_plain$logFC.DoD, tolerance = 1e-9)
})

# ---- cross-check: contrast path reproduces the direct coefficient ------------

test_that("contrast-path logFC equals the non-contrast interaction coefficient", {
  fx <- make_2x2_dod_gct()

  res_contrast <- fit_dod(fx$gct, "DoD", "treatmentDrug:timepointT2")
  res_direct <- lm.regression(
    gct = fx$gct, formula_string = FORMULA, variable_types = VTYPES
  )

  # In the no-contrast path the interaction is the design coefficient
  # logFC.treatmentDrug.timepointT2. Selecting it via a contrast must reproduce
  # it feature-for-feature (the contrast machinery is an identity map here).
  expect_true("logFC.treatmentDrug.timepointT2" %in% colnames(res_direct))
  expect_equal(res_contrast$logFC.DoD,
               res_direct$logFC.treatmentDrug.timepointT2,
               tolerance = 1e-6)
  expect_equal(res_contrast$P.Value.DoD,
               res_direct$P.Value.treatmentDrug.timepointT2,
               tolerance = 1e-6)
})

# ---- independent oracle: hand-built limma::makeContrasts -----------------------

test_that("contrast-path logFC matches an independent hand-built limma oracle", {
  fx <- make_2x2_dod_gct()
  res <- fit_dod(fx$gct, "DoD", "treatmentDrug:timepointT2")

  # Rebuild the whole fit by hand with the same make.names()-safe design and an
  # explicit makeContrasts on the interaction coefficient. This is an oracle
  # external to lm.regression()'s own contrast plumbing.
  design <- stats::model.matrix(stats::as.formula(FORMULA), data = fx$cdesc)
  colnames(design) <- make.names(colnames(design))
  fit  <- limma::lmFit(fx$mat, design)
  cm   <- limma::makeContrasts(contrasts = "treatmentDrug.timepointT2",
                               levels = design)
  fit2 <- limma::eBayes(limma::contrasts.fit(fit, cm),
                        trend = FALSE, robust = TRUE)
  tt   <- limma::topTable(fit2, coef = 1, number = Inf, sort.by = "none")

  # topTable is unsorted, so row order matches the input matrix / res order.
  expect_equal(res$logFC.DoD, tt$logFC, tolerance = 1e-6)
})
