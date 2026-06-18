################################################################################
# Tests for min_samples_message (R/utilities.R)
#
# Single-sample omes cannot support analyses that require >= 2 samples
# (PCA, Correlation, CV). The shared helper returns NULL when the gate is
# satisfied and an interpolated message string when it is not, so each QC
# section can grey out via validate(need(...)) with one consistent message.
################################################################################

make_gct_n_samples <- function(n_samples, n_features = 5) {
  mat <- matrix(
    seq_len(n_features * max(n_samples, 1)),
    nrow = n_features,
    ncol = max(n_samples, 1)
  )
  rownames(mat) <- paste0("gene", seq_len(n_features))
  cids <- paste0("sample", seq_len(max(n_samples, 1)))
  colnames(mat) <- cids
  if (n_samples == 0) {
    mat <- mat[, integer(0), drop = FALSE]
    cids <- character(0)
  }
  new("GCT",
      mat = mat,
      cdesc = data.frame(group = rep("A", length(cids)), row.names = cids),
      rdesc = data.frame(gene_name = rownames(mat), row.names = rownames(mat)),
      rid = rownames(mat),
      cid = cids)
}

test_that("min_samples_message returns NULL when sample count meets threshold", {
  expect_null(min_samples_message(make_gct_n_samples(2), n = 2, analysis = "PCA"))
  expect_null(min_samples_message(make_gct_n_samples(10), n = 2, analysis = "PCA"))
})

test_that("min_samples_message returns an interpolated message below threshold", {
  msg <- min_samples_message(make_gct_n_samples(1), n = 2, analysis = "PCA")
  expect_type(msg, "character")
  expect_match(msg, "PCA", fixed = TRUE)
  expect_match(msg, "at least 2 samples", fixed = TRUE)
  # reports the actual count (singular)
  expect_match(msg, "1 sample", fixed = TRUE)
})

test_that("min_samples_message uses the analysis label provided", {
  expect_match(
    min_samples_message(make_gct_n_samples(1), n = 2, analysis = "Correlation"),
    "Correlation", fixed = TRUE
  )
  expect_match(
    min_samples_message(make_gct_n_samples(1), n = 2, analysis = "CV"),
    "CV", fixed = TRUE
  )
})

test_that("min_samples_message handles zero samples defensively", {
  msg <- min_samples_message(make_gct_n_samples(0), n = 2, analysis = "PCA")
  expect_type(msg, "character")
  expect_match(msg, "0 samples", fixed = TRUE)
})

test_that("min_samples_message message is ASCII only", {
  msg <- min_samples_message(make_gct_n_samples(1), n = 2, analysis = "PCA")
  # project rule: ASCII-only strings in source
  expect_false(grepl("[^[:print:]]", msg))
})

# A single-sample ome cannot run PCA. The on-screen panels grey out via
# validate(need(is.null(pca_min_samples_msg()), ...)). The export bundle must
# SKIP cleanly (like the CV tab's cv_export_available guard) rather than let the
# inner reactives raise a shiny.silent.error inside ggsave()/write.csv(), which
# tab_export.R would record as a misleading "Could not save: .../qc_PCA" failure.
test_that("PCA export bundle skips single-sample omes without erroring or writing", {
  single <- make_gct_n_samples(1)
  shiny::testServer(
    QCPCA_Ome_Server,
    args = list(
      id = "QCPCATab",
      ome = "test_ome",
      GCT_processed = shiny::reactive(single),
      parameters = shiny::reactive(list()),
      default_annotation_column = shiny::reactive("group"),
      color_map = shiny::reactive(NULL)
    ),
    {
      dir <- withr::local_tempdir()
      exports <- session$returned
      expect_true(is.function(exports$qc_PCA))
      # Must not raise (no shiny.silent.error escaping the bundle) ...
      expect_silent(res <- exports$qc_PCA(dir))
      # ... and must write nothing for an unrunnable single-sample ome.
      expect_length(list.files(dir), 0L)
    }
  )
})

test_that("PCA export bundle skips a failed-PCA ome (>=2 samples, no variance) cleanly", {
  # Two samples but every feature has zero variance -> calculate_PCA() stops, so
  # cached_pca_result()$error is set while pca_min_samples_msg() is NULL. The
  # second guard must skip the export rather than let the $error validate(need)
  # escape as a shiny.silent.error.
  mat <- matrix(rep(c(1, 1), each = 3), nrow = 3, ncol = 2)  # both columns identical
  rownames(mat) <- paste0("gene", 1:3)
  colnames(mat) <- c("sample1", "sample2")
  degenerate <- new("GCT",
                    mat = mat,
                    cdesc = data.frame(group = c("A", "A"), row.names = colnames(mat)),
                    rdesc = data.frame(gene_name = rownames(mat), row.names = rownames(mat)),
                    rid = rownames(mat),
                    cid = colnames(mat))
  shiny::testServer(
    QCPCA_Ome_Server,
    args = list(
      id = "QCPCATab",
      ome = "test_ome",
      GCT_processed = shiny::reactive(degenerate),
      parameters = shiny::reactive(list()),
      default_annotation_column = shiny::reactive("group"),
      color_map = shiny::reactive(NULL)
    ),
    {
      dir <- withr::local_tempdir()
      expect_null(pca_min_samples_msg())            # passes the sample-count gate
      expect_false(is.null(cached_pca_result()$error))  # but PCA itself errored
      exports <- session$returned
      expect_silent(exports$qc_PCA(dir))
      expect_length(list.files(dir), 0L)
    }
  )
})
