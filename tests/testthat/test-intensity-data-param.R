# intensity_data_param_is_yes() -- R/sidebar_setup_helpers_shiny.R (not exported)

test_that("intensity_data_param_is_yes: canonical strings and logical checkbox values", {
  f <- Protigy:::intensity_data_param_is_yes
  expect_false(f(NULL))
  expect_false(f(character(0)))
  expect_true(f("Yes"))
  expect_false(f("No"))
  expect_true(f(TRUE))
  expect_false(f(FALSE))
  expect_false(f("raw"))
  expect_false(f(NA_character_))
  expect_false(f(NA))
})

test_that("intensity_data_param_is_yes: first element only for length > 1", {
  f <- Protigy:::intensity_data_param_is_yes
  expect_true(f(c("Yes", "No")))
  expect_false(f(c("No", "Yes")))
})

test_that("intensity_data_param_is_yes: case-insensitive yes string (trimmed)", {
  f <- Protigy:::intensity_data_param_is_yes
  expect_true(f("yes"))
  expect_true(f("YES"))
  expect_true(f(" Yes "))
})
