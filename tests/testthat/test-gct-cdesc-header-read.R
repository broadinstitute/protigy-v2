# Regression tests for START-04: read_gct_cdesc_as_character / parse_gctx_preserve_cdesc
# must read only the GCT header region (line 1, dims, column-id row, nchd cdesc rows)
# instead of the whole file, while producing byte-identical cdesc/GCT output.

gct_fixture <- function(name) {
  fp <- system.file("extdata", name, package = "Protigy")
  if (!nzchar(fp) || !file.exists(fp)) {
    testthat::skip(paste0("Fixture not available: ", name))
  }
  fp
}

test_that("read_gct_cdesc_as_character preserves leading-zero string values", {
  fp <- gct_fixture("tiny-leading-zero-cdesc.gct")
  cdesc <- read_gct_cdesc_as_character(fp)
  expect_s3_class(cdesc, "data.frame")
  expect_equal(nrow(cdesc), 4L)             # 4 samples
  expect_true("id" %in% names(cdesc))
  # All cdesc values stay character (no numeric coercion of e.g. "001").
  non_id <- cdesc[, setdiff(names(cdesc), "id"), drop = FALSE]
  expect_true(all(vapply(non_id, is.character, logical(1))))
})

test_that("parse_gctx_preserve_cdesc matches a full-file cdesc parse byte-for-byte", {
  # Independent reference parse that reads the WHOLE file, then compares.
  reference_cdesc_full <- function(file_path) {
    lines <- readLines(file_path, warn = FALSE)
    dims <- suppressWarnings(as.integer(strsplit(lines[2L], "\t", fixed = TRUE)[[1]]))
    ncmat <- dims[2L]
    nrhd <- if (length(dims) >= 3L && !is.na(dims[3L])) dims[3L] else 0L
    nchd <- if (length(dims) >= 4L && !is.na(dims[4L])) dims[4L] else 0L
    header <- strsplit(lines[3L], "\t", fixed = TRUE)[[1]]
    if (nrhd > 0L) {
      cid <- header[(nrhd + 2L):length(header)]
    } else {
      has_desc <- any(grepl("description", header, ignore.case = TRUE))
      off <- if (has_desc) 2L else 1L
      cid <- header[(off + 1L):length(header)]
    }
    cid <- as.character(cid)
    if (nchd <= 0L) {
      out <- data.frame(id = cid, stringsAsFactors = FALSE)
      rownames(out) <- cid
      return(out)
    }
    vals <- vector("list", nchd); nms <- character(nchd)
    for (i in seq_len(nchd)) {
      f <- strsplit(lines[3L + i], "\t", fixed = TRUE)[[1]]
      vs <- nrhd + 2L; ve <- vs + length(cid) - 1L
      if (length(f) < ve) f <- c(f, rep(NA_character_, ve - length(f)))
      nms[i] <- as.character(f[1L]); vals[[i]] <- as.character(f[vs:ve])
    }
    out <- as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- make.unique(nms); rownames(out) <- cid; out$id <- rownames(out)
    out
  }

  fixtures <- c(
    "tiny-leading-zero-cdesc.gct",
    "mb-proteome-leadingzero-cdesc.gct_n54x12307.gct",
    "mb-acetylome-ratio-norm-NArm.gct"
  )
  for (name in fixtures) {
    fp <- gct_fixture(name)
    bounded <- read_gct_cdesc_as_character(fp)
    full <- reference_cdesc_full(fp)
    expect_equal(bounded, full, info = paste("cdesc mismatch for", name))

    # Full GCT object is preserved end-to-end.
    g <- parse_gctx_preserve_cdesc(fp)
    expect_s4_class(g, "GCT")
    expect_setequal(rownames(g@cdesc), g@cid)
  }
})

test_that("read_gct_cdesc_as_character errors on a truncated header", {
  tmp <- tempfile(fileext = ".gct")
  # Declares 2 cdesc rows but only provides 1 -> must trip the missing-rows guard.
  writeLines(c("#1.3", "2\t2\t0\t2", "id\tS1\tS2", "metaA\tx\ty"), tmp)
  expect_error(read_gct_cdesc_as_character(tmp), "missing cdesc header rows")
})

test_that("read_gct_cdesc_as_character errors on too-few lines", {
  tmp <- tempfile(fileext = ".gct")
  writeLines(c("#1.3"), tmp)
  expect_error(read_gct_cdesc_as_character(tmp), "expected at least 3 lines")
})
