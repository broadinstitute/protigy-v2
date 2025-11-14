# Tests for contrast selection and generation helper functions

test_that("generate_all_pairwise generates correct contrasts", {
  groups <- c("A", "B", "C")
  
  # Test bidirectional = TRUE
  contrasts_bidirectional <- generate_all_pairwise(groups, bidirectional = TRUE)
  expect_equal(length(contrasts_bidirectional), 6)  # 3 choose 2 * 2 directions
  expect_true("A / B" %in% contrasts_bidirectional)
  expect_true("B / A" %in% contrasts_bidirectional)
  expect_true("A / C" %in% contrasts_bidirectional)
  expect_true("C / A" %in% contrasts_bidirectional)
  expect_true("B / C" %in% contrasts_bidirectional)
  expect_true("C / B" %in% contrasts_bidirectional)
  
  # Test bidirectional = FALSE
  contrasts_unidirectional <- generate_all_pairwise(groups, bidirectional = FALSE)
  expect_equal(length(contrasts_unidirectional), 3)  # 3 choose 2
  expect_true("A / B" %in% contrasts_unidirectional)
  expect_true("A / C" %in% contrasts_unidirectional)
  expect_true("B / C" %in% contrasts_unidirectional)
  expect_false("B / A" %in% contrasts_unidirectional)
})

test_that("generate_all_pairwise handles edge cases", {
  # Single group
  expect_equal(generate_all_pairwise("A"), character(0))
  
  # Two groups
  expect_equal(generate_all_pairwise(c("A", "B"), bidirectional = FALSE), "A / B")
  expect_equal(length(generate_all_pairwise(c("A", "B"), bidirectional = TRUE)), 2)
  
  # Empty groups
  expect_equal(generate_all_pairwise(character(0)), character(0))
})

test_that("generate_all_vs_reference generates correct contrasts", {
  groups <- c("Control", "Treatment1", "Treatment2", "Treatment3")
  reference <- "Control"
  
  # Test bidirectional = FALSE (default)
  contrasts <- generate_all_vs_reference(groups, reference, bidirectional = FALSE)
  expect_equal(length(contrasts), 3)
  expect_true("Treatment1 / Control" %in% contrasts)
  expect_true("Treatment2 / Control" %in% contrasts)
  expect_true("Treatment3 / Control" %in% contrasts)
  expect_false("Control / Treatment1" %in% contrasts)
  
  # Test bidirectional = TRUE
  contrasts_bidirectional <- generate_all_vs_reference(groups, reference, bidirectional = TRUE)
  expect_equal(length(contrasts_bidirectional), 6)  # 3 forward + 3 reverse
  expect_true("Treatment1 / Control" %in% contrasts_bidirectional)
  expect_true("Control / Treatment1" %in% contrasts_bidirectional)
})

test_that("generate_all_vs_reference handles edge cases", {
  # Reference not in groups
  expect_equal(generate_all_vs_reference(c("A", "B"), "C"), character(0))
  
  # Only two groups (returns named vector from sapply, so compare as.character)
  result <- generate_all_vs_reference(c("A", "B"), "A", bidirectional = FALSE)
  expect_equal(as.character(result), "B / A")
  
  # Reference is only group
  expect_equal(generate_all_vs_reference("A", "A"), character(0))
})

test_that("generate_all_vs_multiple_references generates correct contrasts", {
  groups <- c("Control1", "Control2", "Treatment1", "Treatment2")
  reference_groups <- c("Control1", "Control2")
  
  # Test bidirectional = FALSE (default)
  contrasts <- generate_all_vs_multiple_references(groups, reference_groups, bidirectional = FALSE)
  expect_equal(length(contrasts), 4)  # 2 treatments * 2 controls
  expect_true("Treatment1 / Control1" %in% contrasts)
  expect_true("Treatment1 / Control2" %in% contrasts)
  expect_true("Treatment2 / Control1" %in% contrasts)
  expect_true("Treatment2 / Control2" %in% contrasts)
  
  # Test bidirectional = TRUE
  contrasts_bidirectional <- generate_all_vs_multiple_references(
    groups, reference_groups, bidirectional = TRUE
  )
  expect_equal(length(contrasts_bidirectional), 8)  # 4 forward + 4 reverse
})

test_that("generate_all_vs_multiple_references handles edge cases", {
  # No reference groups
  expect_equal(generate_all_vs_multiple_references(c("A", "B"), character(0)), character(0))
  
  # All groups are references
  expect_equal(generate_all_vs_multiple_references(c("A", "B"), c("A", "B")), character(0))
  
  # Reference groups not in groups
  expect_equal(generate_all_vs_multiple_references(c("A", "B"), "C"), character(0))
})

test_that("generate_sequential_pairs generates correct contrasts", {
  groups <- c("Time_1", "Time_2", "Time_3", "Time_4")
  
  # Test bidirectional = FALSE (default - later/earlier)
  contrasts <- generate_sequential_pairs(groups, bidirectional = FALSE)
  expect_equal(length(contrasts), 3)
  expect_equal(contrasts, c("Time_2 / Time_1", "Time_3 / Time_2", "Time_4 / Time_3"))
  
  # Test bidirectional = TRUE
  contrasts_bidirectional <- generate_sequential_pairs(groups, bidirectional = TRUE)
  expect_equal(length(contrasts_bidirectional), 6)  # 3 forward + 3 reverse
  expect_true("Time_2 / Time_1" %in% contrasts_bidirectional)
  expect_true("Time_1 / Time_2" %in% contrasts_bidirectional)
})

test_that("generate_sequential_pairs handles edge cases", {
  # Single group
  expect_equal(generate_sequential_pairs("A"), character(0))
  
  # Two groups
  expect_equal(generate_sequential_pairs(c("A", "B"), bidirectional = FALSE), "B / A")
  expect_equal(length(generate_sequential_pairs(c("A", "B"), bidirectional = TRUE)), 2)
  
  # Empty groups
  expect_equal(generate_sequential_pairs(character(0)), character(0))
})

test_that("parse_contrast_label correctly parses contrast strings", {
  # Valid contrast
  parsed <- parse_contrast_label("GroupA / GroupB")
  expect_equal(parsed$numerator, "GroupA")
  expect_equal(parsed$denominator, "GroupB")
  
  # Contrast with spaces
  parsed2 <- parse_contrast_label("Treatment 1 / Control")
  expect_equal(parsed2$numerator, "Treatment 1")
  expect_equal(parsed2$denominator, "Control")
  
  # Contrast with special characters
  parsed3 <- parse_contrast_label("Group-A / Group_B")
  expect_equal(parsed3$numerator, "Group-A")
  expect_equal(parsed3$denominator, "Group_B")
})

test_that("parse_contrast_label handles invalid formats", {
  # Missing separator
  expect_error(parse_contrast_label("GroupA GroupB"), "Invalid contrast label format")
  
  # Too many parts
  expect_error(parse_contrast_label("A / B / C"), "Invalid contrast label format")
  
  # Empty string
  expect_error(parse_contrast_label(""), "Invalid contrast label format")
})

test_that("contrast_labels_to_list converts correctly", {
  labels <- c("A / B", "C / D", "E / F")
  result <- contrast_labels_to_list(labels)
  
  expect_equal(length(result), 3)
  expect_equal(result[[1]], c("A", "B"))
  expect_equal(result[[2]], c("C", "D"))
  expect_equal(result[[3]], c("E", "F"))
})

test_that("is_valid_contrast correctly validates contrasts", {
  groups <- c("A", "B", "C", "D")
  
  # Valid contrast
  expect_true(is_valid_contrast("A / B", groups))
  expect_true(is_valid_contrast("C / D", groups))
  
  # Invalid - group not in groups
  expect_false(is_valid_contrast("A / E", groups))
  expect_false(is_valid_contrast("X / Y", groups))
  
  # Invalid format
  expect_false(is_valid_contrast("A B", groups))
  expect_false(is_valid_contrast("", groups))
})

test_that("detect_control_group detects common control names", {
  # Exact match
  expect_equal(detect_control_group(c("Treatment", "Control")), "Control")
  expect_equal(detect_control_group(c("A", "ctrl", "B")), "ctrl")
  expect_equal(detect_control_group(c("WT", "mutant")), "WT")
  
  # Contains match
  expect_equal(detect_control_group(c("Treatment", "Control_group")), "Control_group")
  expect_equal(detect_control_group(c("untreated_sample", "treated")), "untreated_sample")
  
  # Case insensitive
  expect_equal(detect_control_group(c("TREATMENT", "CONTROL")), "CONTROL")
  expect_equal(detect_control_group(c("Ctrl", "Treatment")), "Ctrl")
  
  # No control found - returns first alphabetically
  expect_equal(detect_control_group(c("Treatment1", "Treatment2")), "Treatment1")
  expect_equal(detect_control_group(c("Z", "A", "B")), "A")
})

test_that("detect_control_group handles edge cases", {
  # Single group
  expect_equal(detect_control_group("Control"), "Control")
  
  # Empty
  expect_null(detect_control_group(character(0)))
})

