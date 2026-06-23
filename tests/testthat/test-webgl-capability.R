test_that("webgl_capability defaults to TRUE when unknown", {
  expect_true(webgl_capability(NULL))
  expect_true(webgl_capability(NA))
  expect_true(webgl_capability(logical(0)))
})

test_that("webgl_capability returns FALSE only for explicit FALSE", {
  expect_false(webgl_capability(FALSE))
})

test_that("webgl_capability returns TRUE for explicit TRUE", {
  expect_true(webgl_capability(TRUE))
})

test_that("webgl_capability is defensive against odd inputs", {
  expect_true(webgl_capability("yes"))
  expect_true(webgl_capability(1L))
  expect_true(webgl_capability(c(FALSE, TRUE)))  # not length-1 -> default TRUE
})
