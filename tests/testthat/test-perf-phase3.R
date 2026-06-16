# Phase 3 (matrixStats numerics) regression suite. Locks the vectorized
# implementations to the original stats-based behavior and pins the dp-1b
# single-row bugfix. See docs/perf-phase3-implementation-plan.md.

test_that("dp-1b: perform_missing_filter keeps a matrix when exactly one row survives", {
  m <- matrix(rnorm(20), nrow = 5, ncol = 4,
              dimnames = list(paste0("r", 1:5), paste0("c", 1:4)))
  m[1, ] <- NA; m[2, ] <- NA; m[3, ] <- NA; m[4, 2:4] <- NA  # only r5 fully complete
  out <- perform_missing_filter(m, 0)
  expect_true(is.matrix(out))          # old code collapsed to a numeric vector -> crash
  expect_identical(nrow(out), 1L)
  expect_identical(rownames(out), "r5")
})

test_that("dp-1b: perform_missing_filter returns a 0-row matrix (not an error) when none survive", {
  m <- matrix(NA_real_, nrow = 4, ncol = 3,
              dimnames = list(paste0("r", 1:4), paste0("c", 1:3)))
  out <- perform_missing_filter(m, 0)
  expect_true(is.matrix(out))
  expect_identical(nrow(out), 0L)
  expect_identical(ncol(out), 3L)
})

test_that("dp-1b: missing fraction matches the original per-row computation", {
  set.seed(11L)
  m <- matrix(rnorm(200), 25, 8); m[sample(200, 60)] <- NA
  rownames(m) <- paste0("r", 1:25); colnames(m) <- paste0("c", 1:8)
  for (mm in c(0, 25, 50, 100)) {
    new <- perform_missing_filter(m, mm)
    old_frac <- apply(m, 1, function(x) sum(is.na(x)) / length(x))
    rn <- rownames(new); if (is.null(rn)) rn <- character(0)  # 0-row subset -> NULL
    expect_identical(rn, rownames(m)[old_frac <= mm / 100])
  }
})

test_that("dp-norm: colMedians/colMads branches equal the stats::median/stats::mad result", {
  set.seed(22L)
  m <- matrix(rnorm(404), 101, 4); m[sample(404, 40)] <- NA
  rownames(m) <- paste0("r", 1:101); colnames(m) <- paste0("c", 1:4)
  oracle <- function(d, method) {
    if (method == "Median") dn <- apply(d, 2, function(x) x - median(x, na.rm = TRUE))
    else if (method == "Median-MAD") dn <- apply(d, 2, function(x) (x - median(x, na.rm = TRUE)) / mad(x, na.rm = TRUE))
    safe_set_colnames(dn, d)
  }
  for (m_ in c("Median", "Median-MAD")) {
    expect_identical(normalize.data.helper(m, method = m_), oracle(m, m_))
  }
})

test_that("dp-norm: per-group median shift is preserved", {
  set.seed(33L)
  m <- matrix(rnorm(60), 15, 4, dimnames = list(paste0("r", 1:15), paste0("c", 1:4)))
  # per_group = TRUE adds back median of column medians
  live <- normalize.data.helper(m, method = "Median", per_group = TRUE)
  med  <- apply(m, 2, median, na.rm = TRUE)
  ref  <- safe_set_colnames(apply(m, 2, function(x) x - median(x, na.rm = TRUE)), m) + median(med, na.rm = TRUE)
  expect_equal(live, ref)
})

test_that("dp-sd: sd.filter selects the same row set as the original apply(sd) computation", {
  set.seed(44L)
  n <- 60
  tab <- data.frame(
    id = paste0("p", 1:n),
    matrix(rnorm(n * 6), n, 6, dimnames = list(NULL, paste0("s", 1:6))),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  grp.vec <- setNames(rep("g1", 6), paste0("s", 1:6))
  res <- sd.filter(tab, grp.vec, "id", 25)

  mat <- data.matrix(tab[, names(grp.vec)])
  sd_old <- apply(mat, 1, sd, na.rm = TRUE)
  thr <- quantile(sd_old, 0.25, na.rm = TRUE)
  expect_identical(unname(res$values.filtered$g1), unname(which(sd_old < thr)))
  # rows below threshold are NA'd in the returned table
  na_rows <- which(is.na(res$table[[2]]))
  expect_identical(unname(na_rows), unname(which(sd_old < thr)))
})
