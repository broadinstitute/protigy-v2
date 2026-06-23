# Repo hygiene: R source must be ASCII-only.
#
# CLAUDE.md: "ASCII-only R source: no literal Unicode in R/; use \uXXXX escapes
# (e.g. "\u25cf" for a filled bullet). Enforced in practice -- non-ASCII bytes
# break R CMD check." This guard fails if any R/*.R or tests/testthat/*.R file
# contains a byte outside the 0x00-0x7F range, naming the offending file:line so
# it is easy to fix. (Intentional Unicode in test data must be a \uXXXX escape,
# which is itself ASCII -- e.g. "unicode\u00e9.gct".)

# Scan one directory's *.R files for non-ASCII bytes; return "file:line" offenders.
.ascii_offenders <- function(dir) {
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
  offenders <- character(0)
  for (f in files) {
    bytes <- readBin(f, what = "raw", n = file.info(f)$size)
    if (!any(bytes > as.raw(0x7F))) next
    lines <- readLines(f, warn = FALSE, encoding = "bytes")
    bad <- which(vapply(lines, function(L)
      any(charToRaw(L) > as.raw(0x7F)), logical(1)))
    offenders <- c(offenders, sprintf("%s:%d", basename(f), bad))
  }
  offenders
}

test_that("no R/ source file contains non-ASCII bytes", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ source dir not found")
  offenders <- .ascii_offenders(r_dir)
  expect_identical(
    offenders, character(0),
    info = paste0("non-ASCII bytes in R/ (use ASCII or \\uXXXX escapes):\n",
                  paste(offenders, collapse = "\n"))
  )
})

test_that("no tests/testthat/ source file contains non-ASCII bytes", {
  t_dir <- testthat::test_path(".")
  skip_if_not(dir.exists(t_dir), "tests/testthat/ dir not found")
  offenders <- .ascii_offenders(t_dir)
  expect_identical(
    offenders, character(0),
    info = paste0("non-ASCII bytes in tests/testthat/ (use ASCII or \\uXXXX ",
                  "escapes):\n", paste(offenders, collapse = "\n"))
  )
})
