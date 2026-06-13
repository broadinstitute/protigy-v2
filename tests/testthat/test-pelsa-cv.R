################################################################################
# Tests for the PELSA within-condition CV helpers
# (R/tab_pelsa_cv_helpers.R): pelsa_sum_normalize() and
# pelsa_within_condition_cv().
#
# CLOSED-FORM GROUND TRUTH (these comments ARE the reference):
#
# Tiny RAW (linear, un-logged) matrix, 4 peptide rows x 6 sample cols.
# Two conditions, 3 replicates each:
#   A = {A1, A2, A3}, B = {B1, B2, B3}
#
#        A1   A2   A3    B1   B2   B3
#   r1: 100  200  300    10   20   60
#   r2:  50   NA  100    30   40   20      (A2 is NA -> r2 not complete-case in A)
#   r3:   0    0    0    20   10   10      (A all-zero -> normalized mean 0 in A)
#   r4:  60  100  200    40   30   10
#
# --- Sum-normalization (per-condition, complete-case basis, scale="mean") ---
# Condition A complete-case rows (non-NA across A1,A2,A3) = {r1, r3, r4}
#   cc column sums: A1 = 100+0+60 = 160
#                   A2 = 200+0+100 = 300
#                   A3 = 300+0+200 = 500
#   mean of cc column sums = (160 + 300 + 500) / 3 = 960 / 3 = 320
#   factor_j = mean / colSum_cc[j]:
#     fA1 = 320/160 = 2
#     fA2 = 320/300 = 16/15
#     fA3 = 320/500 = 0.64
#
# Condition B complete-case rows = {r1, r2, r3, r4} (no NAs in B)
#   cc column sums: B1 = 10+30+20+40 = 100
#                   B2 = 20+40+10+30 = 100
#                   B3 = 60+20+10+10 = 100
#   mean = 100  ->  fB1 = fB2 = fB3 = 1  (normalized B == raw B)
#
# Normalized values (norm_col_j = raw_col_j * factor_j); NA stays NA:
#        A1   A2(=*16/15)  A3(=*0.64)   B1  B2  B3
#   r1: 200   213.333...   192          10  20  60
#   r2: 100    NA          64           30  40  20
#   r3:   0     0            0          20  10  10
#   r4: 120   106.666...   128          40  30  10
#
# --- CV per (row, condition) on the NORMALIZED matrix: cv = sd/mean*100 ---
#   sd is the SAMPLE sd (ddof = 1), NA ignored.
#
#   r2, condition B (normalized = raw = {30, 40, 20}):
#     mean = 30, sample sd = 10  ->  cv = 10/30*100 = 33.333...%  (CLEAN closed form)
#   r1, condition B (= {10, 20, 60}): mean = 30, sd = sqrt(1400/2) = sqrt(700)
#     cv = sqrt(700)/30*100
#   r2, condition A: only A1, A3 non-NA -> n_nonNA = 2 < 3 ->
#     cv_status = "insufficient_replicates", cv_pct = NA
#   r3, condition A: normalized = {0,0,0} -> mean = 0 -> cv_status = "non_finite",
#     cv_pct = NA  (n_nonNA = 3 >= min_nonNA, but result not finite)
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# Build the closed-form tiny RAW matrix + condition map used across tests.
.cv_tiny_inputs <- function() {
  mat <- matrix(
    c(
      100, 200, 300,  10, 20, 60,
      50,  NA,  100,  30, 40, 20,
      0,   0,   0,    20, 10, 10,
      60,  100, 200,  40, 30, 10
    ),
    nrow = 4, byrow = TRUE,
    dimnames = list(NULL, c("A1", "A2", "A3", "B1", "B2", "B3"))
  )
  cond <- c(A1 = "A", A2 = "A", A3 = "A", B1 = "B", B2 = "B", B3 = "B")
  list(mat = mat, cond = cond)
}

# --------------------------------------------------------------------------
# pelsa_sum_normalize
# --------------------------------------------------------------------------

test_that("pelsa_sum_normalize matches hand-computed per-condition complete-case rescale", {
  io <- .cv_tiny_inputs()
  got <- pelsa_sum_normalize(io$mat, io$cond)

  # Build the exact expected matrix explicitly (avoid helper typo risk).
  fA <- c(320 / 160, 320 / 300, 320 / 500) # 2, 16/15, 0.64
  exp <- io$mat
  exp[, "A1"] <- io$mat[, "A1"] * fA[1]
  exp[, "A2"] <- io$mat[, "A2"] * fA[2]
  exp[, "A3"] <- io$mat[, "A3"] * fA[3]
  # B factors are all 1 -> normalized B == raw B.

  expect_equal(dim(got), dim(io$mat))
  expect_equal(colnames(got), colnames(io$mat))
  expect_equal(got, exp, tolerance = 1e-8)
})

test_that("pelsa_sum_normalize leaves NA positions as NA", {
  io <- .cv_tiny_inputs()
  got <- pelsa_sum_normalize(io$mat, io$cond)
  # exact NA cell: row 2, column A2 (index 2,2)
  expect_true(is.na(got[2, 2]))
  # everything else non-NA
  expect_equal(sum(is.na(got)), 1L)
})

test_that("pelsa_sum_normalize accepts an unnamed condition vector aligned to columns", {
  io <- .cv_tiny_inputs()
  cond_unnamed <- unname(io$cond)
  got_named <- pelsa_sum_normalize(io$mat, io$cond)
  got_unnamed <- pelsa_sum_normalize(io$mat, cond_unnamed)
  expect_equal(got_unnamed, got_named, tolerance = 1e-8)
})

test_that("pelsa_sum_normalize coerces a data.frame block to matrix", {
  io <- .cv_tiny_inputs()
  df <- as.data.frame(io$mat, check.names = FALSE)
  got <- pelsa_sum_normalize(df, io$cond)
  expect_true(is.matrix(got))
  expect_equal(got, pelsa_sum_normalize(io$mat, io$cond), tolerance = 1e-8)
})

test_that("pelsa_sum_normalize validates inputs and fails fast", {
  io <- .cv_tiny_inputs()
  # character matrix -> not numeric
  bad <- matrix(as.character(io$mat), nrow = 4,
                dimnames = dimnames(io$mat))
  expect_error(pelsa_sum_normalize(bad, io$cond))
  # length mismatch
  expect_error(pelsa_sum_normalize(io$mat, io$cond[1:3]))
  # named condition vector whose names don't match columns
  bad_names <- io$cond
  names(bad_names) <- paste0("X", seq_along(bad_names))
  expect_error(pelsa_sum_normalize(io$mat, bad_names))
})

# --------------------------------------------------------------------------
# pelsa_within_condition_cv
# --------------------------------------------------------------------------

test_that("within-condition CV: clean closed-form row (r2, condition B) == 33.333...%", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)

  r2B <- res[res$row_id == 2L & res$condition == "B", , drop = FALSE]
  expect_equal(nrow(r2B), 1L)
  expect_equal(r2B$cv_status, "ok")
  expect_equal(r2B$n_nonNA, 3L)
  # normalized B == raw B == {30,40,20}: mean 30, sample sd 10 -> 33.3333...%
  expect_equal(r2B$cv_pct, 10 / 30 * 100, tolerance = 1e-8)
})

test_that("within-condition CV: r1 condition B matches closed form sqrt(700)/30*100", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  r1B <- res[res$row_id == 1L & res$condition == "B", , drop = FALSE]
  expect_equal(r1B$cv_status, "ok")
  expect_equal(r1B$cv_pct, sqrt(700) / 30 * 100, tolerance = 1e-8)
})

test_that("within-condition CV: insufficient replicates -> status + NA cv", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  # r2 in condition A has only A1, A3 non-NA -> n_nonNA = 2 < 3
  r2A <- res[res$row_id == 2L & res$condition == "A", , drop = FALSE]
  expect_equal(nrow(r2A), 1L)
  expect_equal(r2A$n_nonNA, 2L)
  expect_equal(r2A$cv_status, "insufficient_replicates")
  expect_true(is.na(r2A$cv_pct))
})

test_that("within-condition CV: zero/non-finite mean -> non_finite + NA cv", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  # r3 in condition A: normalized {0,0,0} -> mean 0 -> non_finite (n_nonNA = 3)
  r3A <- res[res$row_id == 3L & res$condition == "A", , drop = FALSE]
  expect_equal(nrow(r3A), 1L)
  expect_equal(r3A$n_nonNA, 3L)
  expect_equal(r3A$cv_status, "non_finite")
  expect_true(is.na(r3A$cv_pct))
})

test_that("within-condition CV: n_nonNA counts non-NA replicates incl. NA holes", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  # All B cells non-NA -> n_nonNA == 3 for every row in B.
  expect_true(all(res$n_nonNA[res$condition == "B"] == 3L))
  # Condition A: r2 has one NA -> n_nonNA 2; others 3.
  nA <- res$n_nonNA[res$condition == "A"][order(res$row_id[res$condition == "A"])]
  expect_equal(nA, c(3L, 2L, 3L, 3L))
})

test_that("within-condition CV: tidy long shape is one row per (peptide, condition)", {
  io <- .cv_tiny_inputs()
  res <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 3L)
  expect_s3_class(res, "data.frame")
  expect_setequal(colnames(res),
                  c("row_id", "condition", "cv_pct", "n_nonNA", "cv_status"))
  # 4 peptide rows x 2 conditions = 8 long rows.
  expect_equal(nrow(res), 4L * 2L)
  expect_setequal(unique(res$row_id), 1:4)
  expect_setequal(unique(res$condition), c("A", "B"))
})

test_that("within-condition CV: min_nonNA boundary and validation", {
  io <- .cv_tiny_inputs()
  # min_nonNA = 2 -> r2 condition A (n=2) now becomes ok (mean of {100,50} finite).
  res2 <- pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 2L)
  r2A <- res2[res2$row_id == 2L & res2$condition == "A", , drop = FALSE]
  expect_equal(r2A$cv_status, "ok")
  expect_false(is.na(r2A$cv_pct))

  expect_error(pelsa_within_condition_cv(io$mat, io$cond, min_nonNA = 0L))
})

test_that("empty complete-case basis -> block unchanged + raw-basis CVs (fallback)", {
  # Condition A: EVERY row carries an NA hole somewhere in A1/A2/A3, so A's
  # complete-case set is empty -> pelsa_sum_normalize hits the factor=1 fallback
  # and returns A unchanged. The CVs for A must therefore equal the raw-basis
  # CVs (sd/mean*100 on the un-normalized values), NOT an error and NOT NA.
  mat <- matrix(
    c(
      # A1  A2  A3    B1  B2  B3
      NA,  20, 30,   10, 20, 60,
      40,  NA, 60,   30, 40, 20,
      70,  80, NA,   20, 10, 10
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(NULL, c("A1", "A2", "A3", "B1", "B2", "B3"))
  )
  cond <- c(A1 = "A", A2 = "A", A3 = "A", B1 = "B", B2 = "B", B3 = "B")

  norm <- pelsa_sum_normalize(mat, cond)
  # (a) condition A block returned UNCHANGED (NA preserved, values identical).
  expect_equal(norm[, c("A1", "A2", "A3")], mat[, c("A1", "A2", "A3")])

  # (b) CVs for A equal raw-basis CVs (sd/mean*100 on un-normalized values).
  res <- pelsa_within_condition_cv(mat, cond, min_nonNA = 2L)
  resA <- res[res$condition == "A", , drop = FALSE]
  resA <- resA[order(resA$row_id), , drop = FALSE]
  raw_cv <- function(x) {
    x <- x[!is.na(x)]
    stats::sd(x) / mean(x) * 100
  }
  expected <- vapply(seq_len(nrow(mat)), function(r) {
    raw_cv(mat[r, c("A1", "A2", "A3")])
  }, numeric(1))
  expect_equal(resA$cv_status, rep("ok", 3L))
  expect_false(anyNA(resA$cv_pct))
  expect_equal(resA$cv_pct, expected, tolerance = 1e-8)
})

test_that("single-replicate condition -> sd undefined -> non_finite + NA cv", {
  # A real upload can have a singleton condition (one replicate column).
  mat <- matrix(
    c(
      100,  10, 20,
      200,  30, 40
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(NULL, c("S1", "B1", "B2"))
  )
  cond <- c(S1 = "S", B1 = "B", B2 = "B")
  res <- pelsa_within_condition_cv(mat, cond, min_nonNA = 1L)

  resS <- res[res$condition == "S", , drop = FALSE]
  expect_equal(nrow(resS), 2L)
  expect_equal(resS$n_nonNA, c(1L, 1L))      # one replicate each
  expect_equal(resS$cv_status, c("non_finite", "non_finite")) # sd undefined
  expect_true(all(is.na(resS$cv_pct)))
})

# --------------------------------------------------------------------------
# Smoke / shape test against the shared synthetic generator
# --------------------------------------------------------------------------

test_that("within-condition CV runs on synthetic frame with correct shape", {
  syn <- pelsa_make_synthetic(seed = 1)
  raw <- as.matrix(syn$peptides[, syn$sample_cols])
  res <- pelsa_within_condition_cv(raw, syn$condition_map, min_nonNA = 3L)

  n_cond <- length(unique(syn$condition_map))
  expect_equal(nrow(res), nrow(raw) * n_cond)
  expect_setequal(unique(res$condition), unique(unname(syn$condition_map)))

  # The generator forces the FIRST data row to have <3 non-NA in LowN.
  low_n_row1 <- res[res$row_id == 1L & res$condition == syn$low_n_condition, ,
                    drop = FALSE]
  expect_equal(nrow(low_n_row1), 1L)
  expect_equal(low_n_row1$cv_status, "insufficient_replicates")
  expect_true(is.na(low_n_row1$cv_pct))
})
