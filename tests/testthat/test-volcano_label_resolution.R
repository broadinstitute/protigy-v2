test_that("resolve_volcano_label_text passes values through when split is disabled", {
  vals <- c("A", "B;C", NA, "")
  expect_identical(resolve_volcano_label_text(vals, split_enabled = FALSE), as.character(vals))
})

test_that("resolve_volcano_label_text splits and returns first non-empty token", {
  expect_equal(resolve_volcano_label_text(";;;G1;G2;G3;;;", split_enabled = TRUE, separator = ";"), "G1")
})

test_that("resolve_volcano_label_text skips whitespace-only tokens", {
  expect_equal(resolve_volcano_label_text(";  ;G1", split_enabled = TRUE, separator = ";"), "G1")
})

test_that("resolve_volcano_label_text returns NA when all tokens are empty", {
  expect_identical(resolve_volcano_label_text(";;;", split_enabled = TRUE, separator = ";"), NA_character_)
})

test_that("resolve_volcano_label_text works with a custom multi-char separator", {
  expect_equal(resolve_volcano_label_text("||A||B", split_enabled = TRUE, separator = "||"), "A")
})

test_that("resolve_volcano_label_text treats empty separator as pass-through", {
  val <- "a;b;c"
  expect_equal(resolve_volcano_label_text(val, split_enabled = TRUE, separator = ""), val)
})

test_that("resolve_volcano_label_text is vectorized correctly", {
  vals <- c(";;;G1;G2", "X", NA, ";;")
  result <- resolve_volcano_label_text(vals, split_enabled = TRUE, separator = ";")
  expect_equal(result, c("G1", "X", NA_character_, NA_character_))
  expect_length(result, 4L)
})

test_that("resolve_volcano_label_text returns NA_character_ for NA input", {
  expect_identical(resolve_volcano_label_text(NA_character_, split_enabled = TRUE, separator = ";"), NA_character_)
})

test_that("resolve_volcano_label_text preserves NA when split disabled", {
  expect_identical(resolve_volcano_label_text(NA_character_, split_enabled = FALSE), NA_character_)
})
