################################################################################
# Phase 1: eBayes robust=TRUE -> robust=FALSE fallback.
#
# Tiny designs can yield degenerate prior estimation under robust=TRUE; the
# existing protigy code catches trend=TRUE failures but does NOT catch
# robust=TRUE failures. Add a parallel tryCatch.
#
# Because robust=TRUE only emits a warning ("Estimation of var.prior failed")
# on typical degenerate fixtures, we mock `limma::eBayes` to force the error
# path and verify the fallback gives a finite result.
################################################################################

library(testthat)

test_that("eBayes robust=TRUE failure falls back to robust=FALSE with a warning", {
  set.seed(202)
  n_samples <- 6
  cdesc <- data.frame(
    id = paste0("s", 1:n_samples),
    group = factor(rep(c("A", "B"), length.out = n_samples)),
    row.names = paste0("s", 1:n_samples),
    stringsAsFactors = FALSE
  )
  mat <- matrix(rnorm(20 * n_samples), nrow = 20,
                dimnames = list(paste0("f", 1:20), rownames(cdesc)))
  rdesc <- data.frame(id = rownames(mat), row.names = rownames(mat))
  gct <- methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
                       rid = rownames(mat), cid = colnames(mat))

  # Capture the original limma::eBayes BEFORE rebinding it (otherwise the
  # mock call recurses into itself).
  real_ebayes <- get("eBayes", envir = asNamespace("limma"))
  call_log <- list()
  fake_ebayes <- function(fit, robust = FALSE, trend = FALSE, ...) {
    call_log[[length(call_log) + 1]] <<- list(robust = robust, trend = trend)
    if (isTRUE(robust)) stop("simulated robust failure")
    real_ebayes(fit, robust = FALSE, trend = trend, ...)
  }
  expect_warning(
    res <- with_mocked_bindings(
      lm.regression(
        gct = gct,
        formula_string = "~ group",
        variable_types = list(group = "factor"),
        intensity = FALSE
      ),
      eBayes = fake_ebayes,
      .package = "limma"
    ),
    regexp = "eBayes with robust=TRUE failed"
  )
  expect_true(is.data.frame(res))
  expect_true(any(grepl("^logFC\\.", colnames(res))))
  # Should have been called twice: first with robust=TRUE (failed), then with robust=FALSE.
  expect_gte(length(call_log), 2)
  expect_true(call_log[[1]]$robust)
  expect_false(call_log[[length(call_log)]]$robust)
})
