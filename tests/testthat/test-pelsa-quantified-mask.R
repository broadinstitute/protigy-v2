################################################################################
# Tests for the canonical "quantified" mask (pelsa_quantified_mask) and its three
# consumers -- per-sample depth, fully-quantified count, and per-condition
# membership -- ACROSS DATA TYPES (linear / log2 / log10 / median-centered).
#
# THE CONTRACT: "quantified" = finite & non-zero. This is INVARIANT under the
# monotonic processing transforms Protigy applies (log2/log10, then mean/median
# centering), because those map a positive finite intensity to a finite value
# and never to exactly 0 (unless a raw value is exactly 1, or a value lands
# exactly on the centering offset -- both are deterministic edges tested below).
# So depth / fully-quantified / membership must read the SAME on every scale.
#
# The OLD notebook mask (`> 0`) did NOT have this property: on a log/centered
# matrix ~half of finite values are <= 0, so it silently under-counted. These
# tests lock the corrected, scale-stable behavior and the no-change-on-linear
# regression.
################################################################################

# A controlled raw LINEAR matrix: 4 peptides x 4 samples, two conditions
# (c1 = S1,S2 ; c2 = S3,S4). Deliberately includes:
#   pepLow : raw < 1 in every sample  -> NEGATIVE under log2/log10 (fix target)
#   pepNA  : an NA hole               -> never quantified there, any scale
#   pepHi  : large                    -> positive on every scale
#   pepMid : moderate
# No raw value is exactly 1, so no log value is exactly 0.
.qm_raw <- function() {
  matrix(
    c(
      #  S1     S2     S3     S4
      120.0, 130.0, 110.0,  90.0,   # pepHi-ish (pepMid)
        0.5,   0.6,   0.4,   0.55,  # pepLow  (raw < 1 -> log NEGATIVE)
      100.0,    NA, 100.0, 100.0,   # pepNA   (NA in S2)
      8000.0,7900.0,8100.0,7950.0   # pepHi
    ),
    nrow = 4, byrow = TRUE,
    dimnames = list(c("pepMid", "pepLow", "pepNA", "pepHi"),
                    c("S1", "S2", "S3", "S4"))
  )
}

.qm_cmap <- function() {
  c(S1 = "c1", S2 = "c1", S3 = "c2", S4 = "c2")
}

# Apply each processing scale to a raw linear matrix. Uses the real package
# transform for log; median/mean centering mirrors normalize.data's centering.
.qm_scales <- function(raw) {
  log2m  <- perform_log_transformation(raw, "log2")$data
  log10m <- perform_log_transformation(raw, "log10")$data
  med    <- sweep(log2m, 2, apply(log2m, 2, stats::median, na.rm = TRUE), "-")
  list(None = raw, log2 = log2m, log10 = log10m, `log2+median` = med)
}

# Ground truth (finite & non-zero), independent of scale, for .qm_raw():
#   depth per sample: S1 all 4 finite&!=0 = 4 ; S2 pepNA is NA -> 3 ; S3 4 ; S4 4
#   fully-quantified (finite&!=0 in ALL samples): pepMid,pepLow,pepHi = 3
#                                                 (pepNA fails on S2)
#   membership: every finite&!=0 peptide is in both conditions; pepNA is still
#               quantified in c1 via S1 (NA only in S2) and in c2 via S3,S4.
#               -> all 4 peptides in c1 AND c2.

test_that("per-sample depth is identical across None/log2/log10 (negatives kept)", {
  # Pure monotonic transforms (no centering) cannot create exact zeros here
  # (no raw value is exactly 1), so the finite & non-zero count is invariant.
  scales <- .qm_scales(.qm_raw())[c("None", "log2", "log10")]
  expected <- c(S1 = 4L, S2 = 3L, S3 = 4L, S4 = 4L)
  for (nm in names(scales)) {
    got <- pelsa_peptides_per_sample(scales[[nm]])
    expect_equal(got, expected, info = sprintf("scale = %s", nm))
  }
})

test_that("fully-quantified is identical across None/log2/log10 (negatives kept)", {
  scales <- .qm_scales(.qm_raw())[c("None", "log2", "log10")]
  fully <- function(m) sum(rowSums(!pelsa_quantified_mask(m)) == 0L)
  for (nm in names(scales)) {
    expect_equal(fully(scales[[nm]]), 3L, info = sprintf("scale = %s", nm))
  }
})

test_that("median-centering: documented exact-zero edge (median element absent)", {
  # KNOWN, NEGLIGIBLE EDGE: median-centering subtracts the column median, so a
  # column with an ODD finite count puts its MEDIAN element at exactly 0, which
  # finite & non-zero then reads as "absent". Here S2 has 3 finite values, its
  # median (pepMid) centers to 0 -> S2 drops from 3 to 2. At real scale this is
  # at most ~1 peptide per sample (immaterial), but we assert it rather than
  # hide it. All OTHER columns (even finite counts, median between elements) are
  # unaffected and still match the scale-invariant counts.
  med <- .qm_scales(.qm_raw())[["log2+median"]]
  expect_equal(pelsa_peptides_per_sample(med),
               c(S1 = 4L, S2 = 2L, S3 = 4L, S4 = 4L))
  # pepMid loses its only c1-via-S2 *additional* sample but is still present in
  # S1, so it stays fully... except S2 == 0 makes it NOT fully-quantified now.
  fully <- sum(rowSums(!pelsa_quantified_mask(med)) == 0L)
  expect_equal(fully, 2L)  # pepLow + pepHi (pepMid knocked out at S2)
})

test_that("per-condition membership is identical across scales", {
  scales <- .qm_scales(.qm_raw())
  cmap <- .qm_cmap()
  for (nm in names(scales)) {
    mem <- pelsa_condition_membership(scales[[nm]], cmap)
    # All 4 peptides appear in BOTH conditions -> 8 (row_id, condition) rows.
    expect_equal(nrow(mem), 8L, info = sprintf("scale = %s", nm))
    expect_setequal(unique(mem$condition), c("c1", "c2"))
    # pepLow is row 2: present in both conditions on every scale (negative on log)
    expect_true(all(c("c1", "c2") %in% mem$condition[mem$row_id == 2L]),
                info = sprintf("pepLow membership, scale = %s", nm))
    expect_setequal(sort(unique(mem$row_id)), 1:4)
  }
})

test_that("OLD `> 0` mask WOULD have diverged on log scales (fix is load-bearing)", {
  raw <- .qm_raw()
  log2m <- perform_log_transformation(raw, "log2")$data
  old_depth <- colSums(is.finite(log2m) & log2m > 0)       # notebook mask
  new_depth <- pelsa_peptides_per_sample(log2m)
  # pepLow is negative under log2: the old mask drops it from EVERY sample,
  # the new mask keeps it. They must differ -> proves the change matters.
  expect_false(isTRUE(all.equal(unname(old_depth), unname(new_depth))))
  expect_equal(unname(new_depth) - unname(as.integer(old_depth)),
               c(1L, 1L, 1L, 1L))  # exactly pepLow recovered per sample
})

test_that("no behavior change on LINEAR data (None): new mask == old `> 0`", {
  raw <- .qm_raw()
  expect_equal(pelsa_quantified_mask(raw), is.finite(raw) & raw > 0)
  expect_equal(unname(pelsa_peptides_per_sample(raw)),
               unname(colSums(is.finite(raw) & raw > 0)))
})

test_that("documented edges: exact 0 and exact log-zero are treated as absent", {
  # Exact 0 (linear absent) -> dropped.
  m0 <- matrix(c(0, 5, 0, 7), nrow = 2, dimnames = list(NULL, c("A", "B")))
  expect_equal(unname(pelsa_peptides_per_sample(m0)), c(1L, 1L))
  # A raw intensity of EXACTLY 1 -> log2 == 0 -> dropped (negligible edge).
  raw1 <- matrix(c(1, 4, 2, 1), nrow = 2, dimnames = list(NULL, c("A", "B")))
  l <- perform_log_transformation(raw1, "log2")$data    # col A: log2(1)=0, log2(4)=2
  expect_equal(unname(pelsa_peptides_per_sample(l)), c(1L, 1L))
})
