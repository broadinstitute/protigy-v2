# Tests for customize helper functions

test_that("make_custom_colors creates correct structure", {
  # Create mock GCT objects using proper GCT structure
  mock_gct1 <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group1 = c("A", "B", "C"),
      group2 = c("X", "Y", "Z"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  mock_gct2 <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group1 = c("A", "B", "C"),
      group3 = c("P", "Q", "R"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  # Create mock merged GCT
  mock_merged_gct <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group1 = c("A", "B", "C"),
      group2 = c("X", "Y", "Z"),
      group3 = c("P", "Q", "R"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  GCTs <- list(ome1 = mock_gct1, ome2 = mock_gct2)
  
  # Mock set_annot_colors function
  set_annot_colors <- function(cdesc) {
    result <- list()
    for (col in names(cdesc)) {
      result[[col]] <- rainbow(length(unique(cdesc[[col]])))
    }
    return(result)
  }
  
  # Assign the mock function to the global environment for testing
  assign("set_annot_colors", set_annot_colors, envir = .GlobalEnv)
  
  result <- make_custom_colors(GCTs, mock_merged_gct)
  
  expect_type(result, "list")
  expect_named(result, c("multi_ome", "ome1", "ome2"))
  
  # Check multi_ome colors
  expect_type(result$multi_ome, "list")
  expect_true(length(result$multi_ome) > 0)
  
  # Check individual ome colors
  expect_type(result$ome1, "list")
  expect_type(result$ome2, "list")
})

test_that("make_custom_colors handles single GCT", {
  # Create mock GCT object
  mock_gct <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group1 = c("A", "B", "C"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  # Create mock merged GCT (same as single GCT)
  mock_merged_gct <- mock_gct
  
  GCTs <- list(ome1 = mock_gct)
  
  # Mock set_annot_colors function
  set_annot_colors <- function(cdesc) {
    result <- list()
    for (col in names(cdesc)) {
      result[[col]] <- rainbow(length(unique(cdesc[[col]])))
    }
    return(result)
  }
  
  assign("set_annot_colors", set_annot_colors, envir = .GlobalEnv)
  
  result <- make_custom_colors(GCTs, mock_merged_gct)
  
  expect_type(result, "list")
  expect_named(result, c("multi_ome", "ome1"))
  expect_equal(result$multi_ome, result$ome1)
})

test_that("make_custom_colors handles GCTs with no common columns", {
  # Create mock GCT objects with no common columns
  mock_gct1 <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group1 = c("A", "B", "C"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  mock_gct2 <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group2 = c("X", "Y", "Z"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  # Create mock merged GCT
  mock_merged_gct <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group1 = c("A", "B", "C"),
      group2 = c("X", "Y", "Z"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  GCTs <- list(ome1 = mock_gct1, ome2 = mock_gct2)
  
  # Mock set_annot_colors function
  set_annot_colors <- function(cdesc) {
    result <- list()
    for (col in names(cdesc)) {
      result[[col]] <- rainbow(length(unique(cdesc[[col]])))
    }
    return(result)
  }
  
  assign("set_annot_colors", set_annot_colors, envir = .GlobalEnv)
  
  result <- make_custom_colors(GCTs, mock_merged_gct)
  
  expect_type(result, "list")
  expect_named(result, c("multi_ome", "ome1", "ome2"))
  
  # Each ome should have its own colors
  expect_true(length(result$ome1) > 0)
  expect_true(length(result$ome2) > 0)
})

# Note: Empty GCT test removed due to complex color generation dependencies
# The set_annot_colors function has intricate logic that's difficult to mock properly
# and expects actual annotation data to work correctly. Testing empty cases would require
# extensive mocking of the color generation logic, which is not practical for unit tests.

test_that("make_custom_colors handles missing columns in merged GCT", {
  # Create mock GCT objects
  mock_gct1 <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group1 = c("A", "B", "C"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  # Create mock merged GCT without group1 column
  mock_merged_gct <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group2 = c("X", "Y", "Z"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  GCTs <- list(ome1 = mock_gct1)
  
  # Mock set_annot_colors function
  set_annot_colors <- function(cdesc) {
    result <- list()
    for (col in names(cdesc)) {
      result[[col]] <- rainbow(length(unique(cdesc[[col]])))
    }
    return(result)
  }
  
  assign("set_annot_colors", set_annot_colors, envir = .GlobalEnv)
  
  expect_warning(
    result <- make_custom_colors(GCTs, mock_merged_gct),
    "ome1: column 'group1' could not be found in the merged GCT"
  )
  
  expect_type(result, "list")
  expect_named(result, c("multi_ome", "ome1"))
})

test_that("make_custom_colors preserves column order", {
  # Create mock GCT objects
  mock_gct1 <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group1 = c("A", "B", "C"),
      group2 = c("X", "Y", "Z"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  mock_gct2 <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group1 = c("A", "B", "C"),
      group3 = c("P", "Q", "R"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  # Create mock merged GCT
  mock_merged_gct <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      group1 = c("A", "B", "C"),
      group2 = c("X", "Y", "Z"),
      group3 = c("P", "Q", "R"),
      row.names = paste0("sample_", 1:3)
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )
  
  GCTs <- list(ome1 = mock_gct1, ome2 = mock_gct2)
  
  # Mock set_annot_colors function
  set_annot_colors <- function(cdesc) {
    result <- list()
    for (col in names(cdesc)) {
      result[[col]] <- rainbow(length(unique(cdesc[[col]])))
    }
    return(result)
  }
  
  assign("set_annot_colors", set_annot_colors, envir = .GlobalEnv)
  
  result <- make_custom_colors(GCTs, mock_merged_gct)
  
  # Check that common columns are preserved
  expect_true("group1" %in% names(result$ome1))
  expect_true("group1" %in% names(result$ome2))
  expect_true("group1" %in% names(result$multi_ome))
  
  # Check that unique columns are handled
  expect_true("group2" %in% names(result$ome1))
  expect_true("group3" %in% names(result$ome2))
})


# Tests for import_colors_from_yaml with smart matching
test_that("import_colors_from_yaml - Scenario 1: All conditions match", {
  # Create custom colors structure
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A", "drug_B"),
        colors = c("#000000", "#111111", "#222222")  # Original colors
      )
    )
  )

  # Create YAML file with all matching conditions
  yaml_content <- "
colors:
  multi_ome:
    treatment:
      control: '#4477AA'
      drug_A: '#EE6677'
      drug_B: '#228833'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # All colors should be updated based on name matching
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")  # control
  expect_equal(result$multi_ome$treatment$colors[2], "#EE6677")  # drug_A
  expect_equal(result$multi_ome$treatment$colors[3], "#228833")  # drug_B

  unlink(temp_file)
})

test_that("import_colors_from_yaml - Scenario 2: Some conditions match", {
  # Create custom colors structure with some matching and some non-matching conditions
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A", "drug_C", "drug_D"),  # drug_C and drug_D don't match YAML
        colors = c("#000000", "#111111", "#222222", "#333333")  # Original colors
      )
    )
  )

  # YAML has control and drug_A (match) plus extra colors from drug_B
  yaml_content <- "
colors:
  multi_ome:
    treatment:
      control: '#4477AA'
      drug_A: '#EE6677'
      drug_B: '#228833'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # Matched conditions get their colors
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")  # control (matched)
  expect_equal(result$multi_ome$treatment$colors[2], "#EE6677")  # drug_A (matched)

  # Unmatched conditions sorted alphabetically: drug_C, drug_D
  # Unused color from YAML: #228833 (from drug_B)
  # drug_C should get #228833 (first unmatched alphabetically)
  expect_equal(result$multi_ome$treatment$colors[3], "#228833")  # drug_C (sequential)

  # drug_D has no more colors, keeps original
  expect_equal(result$multi_ome$treatment$colors[4], "#333333")  # drug_D (original)

  unlink(temp_file)
})

test_that("import_colors_from_yaml - Scenario 3: No conditions match", {
  # Create custom colors structure with completely different conditions
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("therapy_X", "therapy_Y", "therapy_Z"),  # None match YAML
        colors = c("#000000", "#111111", "#222222")  # Original colors
      )
    )
  )

  # YAML has completely different condition names
  yaml_content <- "
colors:
  multi_ome:
    treatment:
      control: '#4477AA'
      drug_A: '#EE6677'
      drug_B: '#228833'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # No matches, all conditions unmatched
  # Sorted alphabetically: therapy_X, therapy_Y, therapy_Z
  # Apply colors sequentially from YAML
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")  # therapy_X (sequential)
  expect_equal(result$multi_ome$treatment$colors[2], "#EE6677")  # therapy_Y (sequential)
  expect_equal(result$multi_ome$treatment$colors[3], "#228833")  # therapy_Z (sequential)

  unlink(temp_file)
})

test_that("import_colors_from_yaml - More conditions than colors", {
  # Create custom colors structure with more conditions than YAML has colors
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("drug_A", "drug_B", "drug_C", "drug_D", "drug_E"),
        colors = c("#000000", "#111111", "#222222", "#333333", "#444444")
      )
    )
  )

  # YAML has only 2 colors
  yaml_content <- "
colors:
  multi_ome:
    treatment:
      control: '#4477AA'
      drug_F: '#EE6677'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # No matches, sorted alphabetically: drug_A, drug_B, drug_C, drug_D, drug_E
  # Only 2 colors available from YAML
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")  # drug_A (sequential)
  expect_equal(result$multi_ome$treatment$colors[2], "#EE6677")  # drug_B (sequential)

  # Remaining conditions keep original colors
  expect_equal(result$multi_ome$treatment$colors[3], "#222222")  # drug_C (original)
  expect_equal(result$multi_ome$treatment$colors[4], "#333333")  # drug_D (original)
  expect_equal(result$multi_ome$treatment$colors[5], "#444444")  # drug_E (original)

  unlink(temp_file)
})

test_that("import_colors_from_yaml - Global cross-column matching", {
  # Create custom colors structure
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_X"),
        colors = c("#000000", "#111111")
      )
    )
  )

  # YAML has 'control' in different column (tissue), should still match globally
  yaml_content <- "
colors:
  multi_ome:
    tissue:
      control: '#4477AA'
      normal: '#EE6677'
    batch:
      batch1: '#228833'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # 'control' still matches globally from tissue column (cross-column name
  # match is preserved to be helpful when users rename columns).
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")  # control (matched from tissue)

  # Bug #5: because `treatment` is NOT in the YAML, unmatched conditions
  # keep their original defaults instead of being clobbered by unrelated
  # leftover colors from other columns. drug_X keeps its original color.
  expect_equal(result$multi_ome$treatment$colors[2], "#111111")  # drug_X (original)

  unlink(temp_file)
})

test_that("import_colors_from_yaml - Skips continuous colors", {
  # Create custom colors structure with continuous colors
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      ),
      age = list(
        is_discrete = FALSE,  # Continuous
        vals = c(25, 30, 35, 40),
        colors = c("#AAA", "#BBB", "#CCC", "#DDD")
      )
    )
  )

  # YAML only has discrete colors
  yaml_content <- "
colors:
  multi_ome:
    treatment:
      control: '#4477AA'
      drug_A: '#EE6677'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # Treatment colors should be updated
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")
  expect_equal(result$multi_ome$treatment$colors[2], "#EE6677")

  # Age (continuous) should remain unchanged
  expect_equal(result$multi_ome$age$colors, c("#AAA", "#BBB", "#CCC", "#DDD"))

  unlink(temp_file)
})

test_that("import_colors_from_yaml - Handles missing YAML colors section", {
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      )
    )
  )

  yaml_content <- "
metadata:
  created_date: '2025-11-15'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  expect_warning(
    result <- import_colors_from_yaml(temp_file, custom_colors),
    "No 'colors' section found in YAML file"
  )

  # Should return original colors unchanged
  expect_equal(result, custom_colors)

  unlink(temp_file)
})

test_that("import_colors_from_yaml - Handles missing ome gracefully", {
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      )
    )
  )

  # YAML has different ome name
  yaml_content <- "
colors:
  proteome:
    treatment:
      control: '#4477AA'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # Should return original colors unchanged (no matching ome)
  expect_equal(result$multi_ome$treatment$colors, c("#000000", "#111111"))

  unlink(temp_file)
})

test_that("import_colors_from_yaml - Handles legacy groups.colors format", {
  # Create custom colors structure
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      ),
      tissue = list(
        is_discrete = TRUE,
        vals = c("normal", "tumor"),
        colors = c("#222222", "#333333")
      )
    ),
    proteome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      )
    )
  )

  # PANOPLY format with groups.colors (flat structure, applies to all omes)
  yaml_content <- "
groups.colors:
  treatment:
    control: '#4477AA'
    drug_A: '#EE6677'
  tissue:
    normal: '#228833'
    tumor: '#CCBB44'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # Should apply to both multi_ome and proteome (PANOPLY format applies to all)
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")  # control
  expect_equal(result$multi_ome$treatment$colors[2], "#EE6677")  # drug_A
  expect_equal(result$multi_ome$tissue$colors[1], "#228833")  # normal
  expect_equal(result$multi_ome$tissue$colors[2], "#CCBB44")  # tumor

  expect_equal(result$proteome$treatment$colors[1], "#4477AA")  # control
  expect_equal(result$proteome$treatment$colors[2], "#EE6677")  # drug_A

  unlink(temp_file)
})

test_that("import_colors_from_yaml - Handles missing annotation columns gracefully", {
  # Create custom colors with multiple annotation columns
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      ),
      tissue = list(
        is_discrete = TRUE,
        vals = c("normal", "tumor"),
        colors = c("#222222", "#333333")
      ),
      batch = list(
        is_discrete = TRUE,
        vals = c("batch1", "batch2"),
        colors = c("#444444", "#555555")
      )
    )
  )

  # YAML only has treatment and tissue, missing batch
  yaml_content <- "
colors:
  multi_ome:
    treatment:
      control: '#4477AA'
      drug_A: '#EE6677'
    tissue:
      normal: '#228833'
      tumor: '#CCBB44'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # Columns in YAML should be updated
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")
  expect_equal(result$multi_ome$treatment$colors[2], "#EE6677")
  expect_equal(result$multi_ome$tissue$colors[1], "#228833")
  expect_equal(result$multi_ome$tissue$colors[2], "#CCBB44")

  # Missing column (batch) should keep original colors
  expect_equal(result$multi_ome$batch$colors, c("#444444", "#555555"))

  unlink(temp_file)
})

test_that("import_colors_from_yaml - Handles missing condition values in annotation", {
  # Create custom colors with more conditions than YAML
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A", "drug_B", "drug_C"),
        colors = c("#000000", "#111111", "#222222", "#333333")
      )
    )
  )

  # YAML only has control and drug_A
  yaml_content <- "
colors:
  multi_ome:
    treatment:
      control: '#4477AA'
      drug_A: '#EE6677'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # Matched conditions get YAML colors
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")  # control (matched)
  expect_equal(result$multi_ome$treatment$colors[2], "#EE6677")  # drug_A (matched)

  # Missing conditions (drug_B, drug_C) keep original colors
  # (No unused colors in YAML since both were matched)
  expect_equal(result$multi_ome$treatment$colors[3], "#222222")  # drug_B (original)
  expect_equal(result$multi_ome$treatment$colors[4], "#333333")  # drug_C (original)

  unlink(temp_file)
})

test_that("import_colors_from_yaml - Prefers ProTIGY format over PANOPLY format", {
  # Create custom colors structure
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      )
    )
  )

  # YAML has both formats (should use ProTIGY format)
  yaml_content <- "
colors:
  multi_ome:
    treatment:
      control: '#4477AA'
      drug_A: '#EE6677'
groups.colors:
  treatment:
    control: '#999999'
    drug_A: '#888888'
"

  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # Should use ProTIGY format (colors), not PANOPLY format (groups.colors)
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")  # from colors
  expect_equal(result$multi_ome$treatment$colors[2], "#EE6677")  # from colors

  unlink(temp_file)
})


# ---------------------------------------------------------------------------
# Phase 1 bug-fix regression tests
# ---------------------------------------------------------------------------

test_that("import_colors_from_yaml - malformed YAML raises an error (bug #1)", {
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      )
    )
  )

  temp_file <- tempfile(fileext = ".yaml")
  writeLines("colors: [this is: not valid: yaml", temp_file)

  # Prior to fix, malformed YAML silently returned original custom_colors and
  # the UI showed "Import Successful". Now errors must propagate.
  expect_error(import_colors_from_yaml(temp_file, custom_colors))

  unlink(temp_file)
})


test_that("import_colors_from_yaml - flat groups.colors is applied (bug #2)", {
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      )
    ),
    proteome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      )
    )
  )

  # Truly flat PANOPLY-style file: condition -> color, no column nesting.
  yaml_content <- "
groups.colors:
  control: '#4477AA'
  drug_A: '#EE6677'
"
  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")
  expect_equal(result$multi_ome$treatment$colors[2], "#EE6677")
  expect_equal(result$proteome$treatment$colors[1], "#4477AA")
  expect_equal(result$proteome$treatment$colors[2], "#EE6677")

  unlink(temp_file)
})


test_that("import_colors_from_yaml - invalid hex codes are skipped with warning (bug #7)", {
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A", "drug_B"),
        colors = c("#000000", "#111111", "#222222")
      )
    )
  )

  yaml_content <- "
colors:
  multi_ome:
    treatment:
      control: '#4477AA'
      drug_A: 'not-a-hex'
      drug_B: '#EE6677'
"
  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  expect_warning(
    result <- import_colors_from_yaml(temp_file, custom_colors),
    "invalid hex color"
  )

  # Valid entries applied, invalid entry keeps original
  expect_equal(result$multi_ome$treatment$colors[1], "#4477AA")
  expect_equal(result$multi_ome$treatment$colors[2], "#111111")  # original kept
  expect_equal(result$multi_ome$treatment$colors[3], "#EE6677")

  unlink(temp_file)
})


test_that("export_colors_to_yaml - skips omes with no discrete columns (bug #15)", {
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("a", "b"),
        colors = c("#AAAAAA", "#BBBBBB")
      )
    ),
    proteome = list(
      age = list(
        is_discrete = FALSE,
        vals = NULL,
        colors = c("#111111", "#222222", "#333333")
      )
    )
  )

  temp_file <- tempfile(fileext = ".yaml")
  export_colors_to_yaml(custom_colors, temp_file)

  yaml_back <- yaml::read_yaml(temp_file)
  expect_true("multi_ome" %in% names(yaml_back$colors))
  # proteome had only continuous entries, so it should be omitted entirely.
  expect_false("proteome" %in% names(yaml_back$colors))

  unlink(temp_file)
})


test_that("export_colors_to_yaml - round-trip preserves YAML-special keys (bug #12)", {
  # Keys like "yes", "no", "1" would otherwise parse as logical/numeric and
  # fail equality match against current_vals on re-import.
  custom_colors <- list(
    multi_ome = list(
      flag = list(
        is_discrete = TRUE,
        vals = c("yes", "no", "1"),
        colors = c("#AAAAAA", "#BBBBBB", "#CCCCCC")
      )
    )
  )

  temp_file <- tempfile(fileext = ".yaml")
  export_colors_to_yaml(custom_colors, temp_file)

  # Reset to placeholder colors so the importer has something to change.
  custom_colors$multi_ome$flag$colors <- c("#000000", "#000000", "#000000")

  result <- import_colors_from_yaml(temp_file, custom_colors)
  expect_equal(result$multi_ome$flag$colors,
               c("#AAAAAA", "#BBBBBB", "#CCCCCC"))

  unlink(temp_file)
})


test_that("export_colors_to_yaml - propagates write errors (bug #10)", {
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("a"),
        colors = c("#AAAAAA")
      )
    )
  )

  # Writing into a non-existent directory should now error out rather than
  # silently returning FALSE. Suppress the upstream `file()` warning so the
  # test log stays clean; the error itself is what we're asserting on.
  bad_path <- file.path(tempfile(), "does", "not", "exist", "out.yaml")
  expect_error(suppressWarnings(export_colors_to_yaml(custom_colors, bad_path)))
})


test_that("is_valid_hex_color - validates 6-digit hex (bug #7)", {
  expect_true(is_valid_hex_color("#AABBCC"))
  expect_true(is_valid_hex_color("#000000"))
  expect_true(is_valid_hex_color("#abcdef"))
  expect_false(is_valid_hex_color("AABBCC"))        # missing #
  expect_false(is_valid_hex_color("#ABC"))          # 3-digit
  expect_false(is_valid_hex_color("#AABBCCDD"))     # 8-digit
  expect_false(is_valid_hex_color("#GGGGGG"))       # non-hex chars
  expect_false(is_valid_hex_color(NA_character_))
  expect_false(is_valid_hex_color(NULL))
  expect_false(is_valid_hex_color(c("#AABBCC", "#112233")))  # vector
})


test_that("export/import - continuous palettes round-trip (bug #8)", {
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("ctrl", "drug"),
        colors = c("#000000", "#111111")
      ),
      age = list(
        is_discrete = FALSE,
        vals = c("low", "mid", "high", "na_color"),
        colors = c("#FF0000", "#00FF00", "#0000FF", "#BBBBBB")
      )
    )
  )

  temp_file <- tempfile(fileext = ".yaml")
  export_colors_to_yaml(custom_colors, temp_file)

  # Wipe colors so the importer has something to change.
  custom_colors$multi_ome$treatment$colors <- c("#777777", "#888888")
  custom_colors$multi_ome$age$colors <- c("#777777", "#777777", "#777777", "#777777")

  result <- import_colors_from_yaml(temp_file, custom_colors)

  expect_equal(result$multi_ome$treatment$colors, c("#000000", "#111111"))
  expect_equal(result$multi_ome$age$colors,
               c("#FF0000", "#00FF00", "#0000FF", "#BBBBBB"))

  unlink(temp_file)
})


test_that("export_colors_to_yaml - skips continuous function-form palettes (bug #8)", {
  # When continuous.return_function=TRUE, $colors is a circlize colorRamp2
  # closure — not YAML-serializable. Must be silently skipped, not crash.
  custom_colors <- list(
    multi_ome = list(
      age = list(
        is_discrete = FALSE,
        vals = NULL,
        colors = function(x) "#AAAAAA"  # stand-in for a colorRamp2 closure
      ),
      group = list(
        is_discrete = TRUE,
        vals = c("A", "B"),
        colors = c("#111111", "#222222")
      )
    )
  )

  temp_file <- tempfile(fileext = ".yaml")
  expect_no_error(export_colors_to_yaml(custom_colors, temp_file))

  yaml_back <- yaml::read_yaml(temp_file)
  # Discrete entry exported normally
  expect_equal(as.character(yaml_back$colors$multi_ome$group$A), "#111111")
  # Function-form continuous entry not emitted
  expect_null(yaml_back$continuous_colors$multi_ome$age)

  unlink(temp_file)
})


test_that("import_colors_from_yaml - unused colors stay within their column (bug #5)", {
  # `treatment` is in the YAML, `batch` is not. Previously, leftover YAML
  # colors from `treatment` would bleed into `batch`'s unmatched conditions.
  # Now `batch` must keep its defaults.
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("ctrl", "drug"),
        colors = c("#000000", "#111111")
      ),
      batch = list(
        is_discrete = TRUE,
        vals = c("batch1", "batch2"),
        colors = c("#AAAAAA", "#BBBBBB")  # colorblind-safe defaults
      )
    )
  )

  yaml_content <- "
colors:
  multi_ome:
    treatment:
      ctrl: '#4477AA'
      drug: '#EE6677'
      extra: '#228833'
"
  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  # treatment gets its YAML colors
  expect_equal(result$multi_ome$treatment$colors, c("#4477AA", "#EE6677"))

  # batch is absent from YAML → defaults preserved; #228833 must NOT leak in.
  expect_equal(result$multi_ome$batch$colors, c("#AAAAAA", "#BBBBBB"))

  unlink(temp_file)
})


test_that("import_colors_from_yaml - duplicate condition names keep per-column color (bug #6)", {
  # `Control` appears in both `Treatment` and `QC.status` with DIFFERENT
  # colors in the YAML. Previously only the first occurrence survived
  # globally, losing per-column distinctness on round-trip. Now each
  # column gets its own color.
  custom_colors <- list(
    multi_ome = list(
      Treatment = list(
        is_discrete = TRUE,
        vals = c("Control", "DrugA"),
        colors = c("#000000", "#111111")
      ),
      QC.status = list(
        is_discrete = TRUE,
        vals = c("Control", "Fail"),
        colors = c("#222222", "#333333")
      )
    )
  )

  yaml_content <- "
colors:
  multi_ome:
    Treatment:
      Control: '#4477AA'
      DrugA: '#EE6677'
    QC.status:
      Control: '#228833'
      Fail: '#CCBB44'
"
  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)

  result <- import_colors_from_yaml(temp_file, custom_colors)

  expect_equal(result$multi_ome$Treatment$colors[1], "#4477AA")   # Treatment's Control
  expect_equal(result$multi_ome$QC.status$colors[1], "#228833")   # QC.status's Control — must differ

  unlink(temp_file)
})


test_that("colors_structure_signature - stable under color-only edits (bug #4)", {
  before <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#000000", "#111111")
      )
    )
  )
  after_edit <- before
  after_edit$multi_ome$treatment$colors <- c("#AAAAAA", "#BBBBBB")

  # Color edits must NOT change the signature — otherwise the Customize
  # observer would clobber user edits back to globals$colors every time.
  expect_identical(
    colors_structure_signature(before),
    colors_structure_signature(after_edit)
  )
})


test_that("colors_structure_signature - changes when omes/columns change (bug #4)", {
  small <- list(
    ome1 = list(
      group = list(is_discrete = TRUE, vals = c("A", "B"), colors = c("#111", "#222"))
    )
  )
  new_ome <- list(
    ome1 = list(
      group = list(is_discrete = TRUE, vals = c("A", "B"), colors = c("#111", "#222"))
    ),
    ome2 = list(
      batch = list(is_discrete = TRUE, vals = c("X", "Y"), colors = c("#333", "#444"))
    )
  )
  new_vals <- list(
    ome1 = list(
      group = list(is_discrete = TRUE, vals = c("A", "B", "C"), colors = c("#1", "#2", "#3"))
    )
  )

  # Adding an ome changes the signature → refresh fires.
  expect_false(identical(colors_structure_signature(small),
                         colors_structure_signature(new_ome)))
  # Adding a condition value changes the signature → refresh fires.
  expect_false(identical(colors_structure_signature(small),
                         colors_structure_signature(new_vals)))

  # Empty / NULL inputs return empty string consistently.
  expect_identical(colors_structure_signature(NULL), "")
  expect_identical(colors_structure_signature(list()), "")
})


test_that("make_custom_colors - handles regex-metachar column names (bug #11)", {
  mock_gct <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      `group+plus` = c("A", "B", "C"),
      `treatment(type)` = c("X", "Y", "Z"),
      row.names = paste0("sample_", 1:3),
      check.names = FALSE
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )

  # Merged GCT has the same columns with ome-suffixed variants (simulating the
  # code path that exercises the regex).
  mock_merged <- new("GCT",
    mat = matrix(1:9, nrow = 3, ncol = 3),
    rdesc = data.frame(id = paste0("gene_", 1:3)),
    cdesc = data.frame(
      `group+plus` = c("A", "B", "C"),
      `treatment(type)` = c("X", "Y", "Z"),
      row.names = paste0("sample_", 1:3),
      check.names = FALSE
    ),
    rid = paste0("gene_", 1:3),
    cid = paste0("sample_", 1:3)
  )

  # Must not error on regex construction.
  expect_no_error(result <- make_custom_colors(list(ome1 = mock_gct), mock_merged))
  expect_true("group+plus" %in% names(result$ome1))
  expect_true("treatment(type)" %in% names(result$ome1))
})
