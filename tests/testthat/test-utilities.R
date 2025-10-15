
test_that("smart_trim works on example labels", {
  # test labels
  labels = c('xxxx_stuffandthings_xasf',
             'xxxx_stuffandthings_ejwt',
             'xxxx_stuffandthings_fdsh')
  
  labels_reverse = c('fsax_sgnihtdnaffuts_xxxx',
                     'twje_sgnihtdnaffuts_xxxx',
                     'hsdf_sgnihtdnaffuts_xxxx')
  
  labels_equal_difference = c('xxxx_stuffandthings_yyyy',
                              'xxxx_thingsandstuff_yyyy',
                              'xxxx_stuffandthings_yyyy')
  
  
  expect_equal(suppressWarnings(smart_trim(labels)), 
               c("hings_xasf", "hings_ejwt", "hings_fdsh"))
  expect_equal(suppressWarnings(smart_trim(labels_reverse)),
               c("fsax_sgnih", "twje_sgnih", "hsdf_sgnih"))
  expect_equal(suppressWarnings(smart_trim(labels_equal_difference)),
               c("xxxx_stuff", "xxxx_thing", "xxxx_stuff"))
})

test_that("scale_font_size returns appropriate font sizes", {
  # test different parameters
  expect_equal(suppressWarnings(scale_font_size(dimension=50,max.size=14,scale.factor=50)), 14)
  expect_equal(suppressWarnings(scale_font_size(dimension=100,max.size=14,scale.factor=50)), 7)
  expect_equal(suppressWarnings(scale_font_size(dimension=75,max.size=12,scale.factor=50)), 8)
  expect_equal(suppressWarnings(scale_font_size(dimension=75,max.size=12,scale.factor=100)), 12)
})

test_that("smart_trim handles various label scenarios", {
  # Test basic functionality
  labels <- c('sample_001_condition_A', 'sample_002_condition_A', 'sample_003_condition_B')
  result <- suppressWarnings(smart_trim(labels, trim_length = 10))
  expect_equal(length(result), 3)
  expect_equal(length(unique(result)), 3) # All should be unique
  
  # Test with equal uniqueness at beginning and end
  labels_equal <- c('prefix_suffix', 'prefix_suffix', 'prefix_suffix')
  expect_warning(smart_trim(labels_equal, trim_length = 5), 
                 "Labels appear to be equally unique")
  
  # Test with single label
  single_label <- c('single_label')
  result_single <- suppressWarnings(smart_trim(single_label, trim_length = 5))
  expect_equal(result_single, "singl")
  
  # Test with empty labels - this actually works but returns empty result
  empty_labels <- character(0)
  result_empty <- smart_trim(empty_labels)
  expect_equal(length(result_empty), 0)
  
  # Test default_trim parameter
  labels_test <- c('same_start_end', 'same_start_end', 'same_start_end')
  result_beginning <- suppressWarnings(smart_trim(labels_test, trim_length = 5, default_trim = "beginning"))
  result_end <- suppressWarnings(smart_trim(labels_test, trim_length = 5, default_trim = "end"))
  expect_equal(result_beginning, c("same_", "same_", "same_"))
  expect_equal(result_end, c("", "", "")) # Empty when equally unique
})

test_that("scale_font_size calculates correct font sizes", {
  # Test basic scaling
  expect_equal(scale_font_size(dimension = 50, max.size = 14, scale.factor = 50), 14)
  expect_equal(scale_font_size(dimension = 100, max.size = 14, scale.factor = 50), 7)
  expect_equal(scale_font_size(dimension = 25, max.size = 12, scale.factor = 50), 12)
  
  # Test edge cases
  expect_equal(scale_font_size(dimension = 1, max.size = 14, scale.factor = 50), 14)
  expect_equal(scale_font_size(dimension = 1000, max.size = 14, scale.factor = 50), 0)
  
  # Test with different parameters
  expect_equal(scale_font_size(dimension = 75, max.size = 12, scale.factor = 100), 12)
  expect_equal(scale_font_size(dimension = 150, max.size = 20, scale.factor = 75), 10)
})

test_that("add_css_attributes adds classes and styles correctly", {
  # Create a test element
  test_element <- div(id = "test")
  
  # Test adding classes only
  result_classes <- add_css_attributes(test_element, classes = "test-class")
  expect_equal(result_classes$attribs$class, " test-class") # Note: leading space
  
  # Test adding styles only
  result_styles <- add_css_attributes(test_element, styles = c("color: red", "font-size: 12px"))
  expect_equal(result_styles$attribs$style, " color: red; font-size: 12px") # Note: leading space
  
  # Test adding both classes and styles
  result_both <- add_css_attributes(test_element, classes = "test-class", styles = "color: blue")
  expect_equal(result_both$attribs$class, " test-class") # Note: leading space
  expect_equal(result_both$attribs$style, " color: blue") # Note: leading space
  
  # Test with existing attributes
  test_element_with_attrs <- div(id = "test", class = "existing-class", style = "margin: 10px")
  result_existing <- add_css_attributes(test_element_with_attrs, classes = "new-class", styles = "padding: 5px")
  expect_equal(result_existing$attribs$class, "existing-class new-class")
  expect_equal(result_existing$attribs$style, "margin: 10px padding: 5px") # Note: no semicolon
})

test_that("%then% operator works correctly", {
  # Test with NULL first argument
  result1 <- NULL %then% "fallback"
  expect_equal(result1, "fallback")
  
  # Test with non-NULL first argument
  result2 <- "value" %then% "fallback"
  expect_equal(result2, "value")
  
  # Test with complex expressions
  result3 <- if (TRUE) "true" else NULL %then% "false"
  expect_equal(result3, "true")
  
  result4 <- if (FALSE) "true" else NULL %then% "false"
  expect_equal(result4, "false")
})

test_that("my_shinyalert_tryCatch handles warnings and errors", {
  # Test successful execution
  result_success <- my_shinyalert_tryCatch(
    expr = 2 + 2,
    show.warning = FALSE,
    show.error = FALSE
  )
  expect_equal(result_success, 4)
  
  # Test warning handling
  expect_warning(
    my_shinyalert_tryCatch(
      expr = warning("Test warning"),
      show.warning = FALSE,
      show.error = FALSE
    )
  )
  
  # Test error handling with return value
  result_error <- my_shinyalert_tryCatch(
    expr = stop("Test error"),
    show.warning = FALSE,
    show.error = FALSE,
    return.error = "error_occurred"
  )
  expect_equal(result_error, "error_occurred")
  
  # Test error handling without return value
  result_error_null <- my_shinyalert_tryCatch(
    expr = stop("Test error"),
    show.warning = FALSE,
    show.error = FALSE
  )
  expect_null(result_error_null)
})

test_that("my_shinyalert_tryCatch handles custom messages", {
  # Test custom warning message
  expect_warning(
    my_shinyalert_tryCatch(
      expr = warning("Original warning"),
      text.warning = "Custom warning message",
      show.warning = FALSE,
      show.error = FALSE
    )
  )
  
  # Test custom error message
  result_error <- my_shinyalert_tryCatch(
    expr = stop("Original error"),
    text.error = "Custom error message",
    show.warning = FALSE,
    show.error = FALSE,
    return.error = "error_occurred"
  )
  expect_equal(result_error, "error_occurred")
  
  # Test append functionality
  expect_warning(
    my_shinyalert_tryCatch(
      expr = warning("Original warning"),
      text.warning = "Custom prefix: ",
      append.warning = TRUE,
      show.warning = FALSE,
      show.error = FALSE
    )
  )
})

# Tests for is.discrete and is.continuous utility functions

test_that("is.discrete correctly identifies discrete data", {
  # Test discrete character data
  expect_true(is.discrete(c("A", "B", "A", "C")))
  expect_true(is.discrete(c("Treatment", "Control", "Treatment")))
  
  # Test discrete factor data
  expect_true(is.discrete(factor(c("A", "B", "A", "C"))))
  
  # Test discrete numeric data with few unique values
  expect_true(is.discrete(c(1, 2, 1, 3, 2)))
  expect_true(is.discrete(c(0, 1, 0, 1, 0)))
})

test_that("is.discrete correctly identifies continuous data", {
  # Test continuous numeric data with many unique values
  continuous_data <- seq(1, 100, by = 0.1)
  expect_false(is.discrete(continuous_data))
  
  # Test continuous numeric data with many unique values (above cutoff)
  continuous_data_25 <- seq(1, 25, by = 1)
  expect_false(is.discrete(continuous_data_25, nfactor_cutoff = 20))
  
  # Test numeric data that looks continuous
  expect_false(is.discrete(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0, 3.1)))
})

test_that("is.discrete handles NA values correctly", {
  # Test with NA values
  expect_true(is.discrete(c("A", "B", NA, "A")))
  expect_true(is.discrete(c(1, 2, NA, 1)))
  
  # Test with common NA patterns
  expect_true(is.discrete(c("A", "B", "NA", "A")))
  expect_true(is.discrete(c("A", "B", "n/a", "A")))
  expect_true(is.discrete(c("A", "B", "unknown", "A")))
  expect_true(is.discrete(c("A", "B", "", "A")))
})

test_that("is.discrete respects nfactor_cutoff parameter", {
  # Test with default cutoff (20)
  data_15_unique <- rep(1:15, each = 2)
  expect_true(is.discrete(data_15_unique))
  
  # Test with custom cutoff
  expect_false(is.discrete(data_15_unique, nfactor_cutoff = 10))
  expect_true(is.discrete(data_15_unique, nfactor_cutoff = 20))
})

test_that("is.continuous correctly identifies continuous data", {
  # Test continuous numeric data
  continuous_data <- seq(1, 100, by = 0.1)
  expect_true(is.continuous(continuous_data))
  
  # Test continuous numeric data with many unique values
  continuous_data_25 <- seq(1, 25, by = 1)
  expect_true(is.continuous(continuous_data_25, nfactor_cutoff = 20))
})

test_that("is.continuous correctly identifies discrete data", {
  # Test discrete character data
  expect_false(is.continuous(c("A", "B", "A", "C")))
  expect_false(is.continuous(c("Treatment", "Control", "Treatment")))
  
  # Test discrete factor data
  expect_false(is.continuous(factor(c("A", "B", "A", "C"))))
  
  # Test discrete numeric data with few unique values
  expect_false(is.continuous(c(1, 2, 1, 3, 2)))
  expect_false(is.continuous(c(0, 1, 0, 1, 0)))
})

test_that("is.continuous handles NA values correctly", {
  # Test with NA values
  expect_false(is.continuous(c("A", "B", NA, "A")))
  expect_false(is.continuous(c(1, 2, NA, 1)))
  
  # Test with common NA patterns
  expect_false(is.continuous(c("A", "B", "NA", "A")))
  expect_false(is.continuous(c("A", "B", "n/a", "A")))
  expect_false(is.continuous(c("A", "B", "unknown", "A")))
  expect_false(is.continuous(c("A", "B", "", "A")))
})

test_that("is.continuous respects nfactor_cutoff parameter", {
  # Test with default cutoff (10)
  data_15_unique <- rep(1:15, each = 2)
  expect_true(is.continuous(data_15_unique))
  
  # Test with custom cutoff
  expect_false(is.continuous(data_15_unique, nfactor_cutoff = 20))
  expect_true(is.continuous(data_15_unique, nfactor_cutoff = 10))
})

test_that("is.discrete and is.continuous are complementary", {
  # Test that they return opposite results for the same data
  test_data <- c("A", "B", "A", "C")
  expect_equal(is.discrete(test_data), !is.continuous(test_data))
  
  test_data_numeric <- c(1, 2, 1, 3, 2)
  expect_equal(is.discrete(test_data_numeric), !is.continuous(test_data_numeric))
  
  test_data_continuous <- seq(1, 100, by = 0.1)
  expect_equal(is.discrete(test_data_continuous), !is.continuous(test_data_continuous))
})

test_that("is.discrete handles edge cases", {
  # Test empty vector
  expect_true(is.discrete(character(0)))
  expect_true(is.discrete(numeric(0)))
  
  # Test single value
  expect_true(is.discrete("A"))
  expect_true(is.discrete(1))
  
  # Test all NA values
  expect_true(is.discrete(c(NA, NA, NA)))
  expect_true(is.discrete(c("NA", "NA", "NA")))
  
  # Test mixed numeric and character (should be treated as discrete)
  expect_true(is.discrete(c("1", "2", "1", "3")))
})

test_that("is.continuous handles edge cases", {
  # Test empty vector
  expect_false(is.continuous(character(0)))
  expect_false(is.continuous(numeric(0)))
  
  # Test single value
  expect_false(is.continuous("A"))
  expect_false(is.continuous(1))
  
  # Test all NA values
  expect_false(is.continuous(c(NA, NA, NA)))
  expect_false(is.continuous(c("NA", "NA", "NA")))
  
  # Test mixed numeric and character (should be treated as discrete)
  expect_false(is.continuous(c("1", "2", "1", "3")))
})