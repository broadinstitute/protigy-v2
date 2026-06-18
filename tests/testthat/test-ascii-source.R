# Repo hygiene: R source must be ASCII-only.
#
# CLAUDE.md: "ASCII-only R source: no literal Unicode in R/; use \uXXXX escapes
# (e.g. "●" for a filled bullet). Enforced in practice -- non-ASCII bytes
# break R CMD check." This guard fails if any R/*.R file contains a byte outside
# the 0x00-0x7F range, naming the offending file:line so it is easy to fix.

test_that("no R/ source file contains non-ASCII bytes", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ source dir not found")
  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  skip_if(length(files) == 0L, "no R source files found")

  offenders <- character(0)
  for (f in files) {
    # Read as raw bytes so the check is encoding-independent.
    bytes <- readBin(f, what = "raw", n = file.info(f)$size)
    if (!any(bytes > as.raw(0x7F))) next
    # Locate offending lines for a useful failure message.
    lines <- readLines(f, warn = FALSE, encoding = "bytes")
    bad <- which(vapply(lines, function(L)
      any(charToRaw(L) > as.raw(0x7F)), logical(1)))
    offenders <- c(offenders,
                   sprintf("%s:%d", basename(f), bad))
  }

  expect_identical(
    offenders, character(0),
    info = paste0("non-ASCII bytes in R/ source (use ASCII or \\uXXXX escapes):\n",
                  paste(offenders, collapse = "\n"))
  )
})
