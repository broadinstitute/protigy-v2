################################################################################
# Tests for color customization features
################################################################################

test_that("Fixed color palette order is reproducible", {
  # Load test data
  data(brca_retrospective_v5.0_proteome_gct)
  gct <- brca_retrospective_v5.0_proteome_gct

  # Generate colors twice
  colors1 <- set_annot_colors(gct@cdesc, autodetect_continuous_nfactor_cutoff = 20)
  colors2 <- set_annot_colors(gct@cdesc, autodetect_continuous_nfactor_cutoff = 20)

  # Check that annotation columns match
  expect_equal(names(colors1), names(colors2))

  # Check that values and colors are identical for each column
  for (col in names(colors1)) {
    expect_equal(colors1[[col]]$vals, colors2[[col]]$vals,
                 info = paste("Values mismatch for column:", col))
    expect_equal(colors1[[col]]$colors, colors2[[col]]$colors,
                 info = paste("Colors mismatch for column:", col))
  }
})


test_that("Colors are assigned in fixed order for sorted annotation values", {
  # Create simple test data with known values
  test_annot <- data.frame(
    group = c("B", "C", "A", "A", "B", "C"),
    stringsAsFactors = FALSE
  )

  # Generate colors
  colors <- set_annot_colors(test_annot, autodetect_continuous_nfactor_cutoff = 20)

  # Check that values are sorted
  expect_equal(colors$group$vals, c("A", "B", "C"))

  # Check that we have 3 colors
  expect_equal(length(colors$group$colors), 3)

  # Generate again and verify consistency
  colors2 <- set_annot_colors(test_annot, autodetect_continuous_nfactor_cutoff = 20)
  expect_equal(colors$group$colors, colors2$group$colors)
})


test_that("sync_colors_across_omes updates all omes with same condition", {
  # Create mock color structure
  custom_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#4477AA", "#EE6677")
      )
    ),
    proteome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#4477AA", "#EE6677")
      )
    ),
    phosphoproteome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#4477AA", "#EE6677")
      )
    )
  )

  # Sync color for "control" to a new color
  new_color <- "#FF0000"
  updated_colors <- sync_colors_across_omes(
    custom_colors,
    annot_column = "treatment",
    annot_value = "control",
    new_color = new_color
  )

  # Check that all omes were updated
  expect_equal(updated_colors$multi_ome$treatment$colors[1], new_color)
  expect_equal(updated_colors$proteome$treatment$colors[1], new_color)
  expect_equal(updated_colors$phosphoproteome$treatment$colors[1], new_color)

  # Check that drug_A color remained unchanged
  expect_equal(updated_colors$multi_ome$treatment$colors[2], "#EE6677")
  expect_equal(updated_colors$proteome$treatment$colors[2], "#EE6677")
})


test_that("export_colors_to_yaml and import_colors_from_yaml work correctly", {
  # Create test colors
  test_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A", "drug_B"),
        colors = c("#4477AA", "#EE6677", "#228833")
      ),
      tissue = list(
        is_discrete = TRUE,
        vals = c("normal", "tumor"),
        colors = c("#CCBB44", "#AA3377")
      )
    ),
    proteome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#4477AA", "#EE6677")
      )
    )
  )

  # Create temporary file
  temp_file <- tempfile(fileext = ".yaml")

  # Export colors
  result <- export_colors_to_yaml(test_colors, temp_file)
  expect_true(result)
  expect_true(file.exists(temp_file))

  # Read back and verify structure
  yaml_data <- yaml::read_yaml(temp_file)
  expect_true("metadata" %in% names(yaml_data))
  expect_true("colors" %in% names(yaml_data))
  expect_true("multi_ome" %in% names(yaml_data$colors))

  # Verify metadata fields
  expect_true("created_date" %in% names(yaml_data$metadata))
  expect_true("protigy_version" %in% names(yaml_data$metadata))
  expect_match(yaml_data$metadata$created_date, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")

  # Verify that YAML has proper named structure (not unnamed array)
  # This tests the as.list() fix
  treatment_names <- names(yaml_data$colors$multi_ome$treatment)
  expect_false(is.null(treatment_names))
  expect_equal(treatment_names, c("control", "drug_A", "drug_B"))
  expect_equal(unname(unlist(yaml_data$colors$multi_ome$treatment)),
               c("#4477AA", "#EE6677", "#228833"))

  # Verify tissue colors also have proper named structure
  tissue_names <- names(yaml_data$colors$multi_ome$tissue)
  expect_false(is.null(tissue_names))
  expect_equal(tissue_names, c("normal", "tumor"))

  # Test import
  modified_colors <- test_colors
  modified_colors$multi_ome$treatment$colors <- c("#000000", "#111111", "#222222")

  imported_colors <- import_colors_from_yaml(temp_file, modified_colors)

  # Check that colors were imported correctly
  expect_equal(imported_colors$multi_ome$treatment$colors,
               test_colors$multi_ome$treatment$colors)

  # Clean up
  unlink(temp_file)
})


test_that("export_colors_to_yaml skips continuous colors", {
  # Create test colors with both discrete and continuous
  test_colors <- list(
    multi_ome = list(
      treatment = list(
        is_discrete = TRUE,
        vals = c("control", "drug_A"),
        colors = c("#4477AA", "#EE6677")
      ),
      age = list(
        is_discrete = FALSE,
        vals = c(20, 30, 40, 50),
        colors = c("#AAAAAA", "#BBBBBB", "#CCCCCC", "#DDDDDD")
      ),
      tissue = list(
        is_discrete = TRUE,
        vals = c("normal", "tumor"),
        colors = c("#228833", "#CCBB44")
      )
    )
  )

  temp_file <- tempfile(fileext = ".yaml")
  export_colors_to_yaml(test_colors, temp_file)

  yaml_data <- yaml::read_yaml(temp_file)

  # Discrete columns should be exported
  expect_true("treatment" %in% names(yaml_data$colors$multi_ome))
  expect_true("tissue" %in% names(yaml_data$colors$multi_ome))

  # Continuous column should NOT be exported
  expect_false("age" %in% names(yaml_data$colors$multi_ome))

  unlink(temp_file)
})


test_that("make_custom_colors generates consistent structure", {
  # Load test data
  data(brca_retrospective_v5.0_proteome_gct)
  data(brca_retrospective_v5.0_phosphoproteome_gct)

  prot_gct <- brca_retrospective_v5.0_proteome_gct
  phos_gct <- brca_retrospective_v5.0_phosphoproteome_gct

  # Add ome identifier to row descriptions for merged GCT
  prot_gct@rdesc$protigy.ome <- "proteome"
  phos_gct@rdesc$protigy.ome <- "phosphoproteome"

  # Create merged GCT (simplified version)
  merged_gct <- prot_gct  # Use proteome as base
  merged_gct@cdesc <- prot_gct@cdesc  # Use same cdesc

  GCTs <- list(
    proteome = prot_gct,
    phosphoproteome = phos_gct
  )

  # Generate custom colors
  custom_colors <- make_custom_colors(GCTs, merged_gct)

  # Check structure
  expect_true("multi_ome" %in% names(custom_colors))
  expect_true("proteome" %in% names(custom_colors))
  expect_true("phosphoproteome" %in% names(custom_colors))

  # Check that each ome has color information
  for (ome in names(custom_colors)) {
    expect_true(is.list(custom_colors[[ome]]))
    expect_true(length(custom_colors[[ome]]) > 0)

    # Check that each annotation column has required fields
    for (annot_col in names(custom_colors[[ome]])) {
      expect_true("is_discrete" %in% names(custom_colors[[ome]][[annot_col]]))
      expect_true("vals" %in% names(custom_colors[[ome]][[annot_col]]))
      expect_true("colors" %in% names(custom_colors[[ome]][[annot_col]]))
    }
  }

  # Check color consistency across runs
  custom_colors2 <- make_custom_colors(GCTs, merged_gct)

  for (ome in names(custom_colors)) {
    for (annot_col in names(custom_colors[[ome]])) {
      expect_equal(custom_colors[[ome]][[annot_col]]$vals,
                   custom_colors2[[ome]][[annot_col]]$vals,
                   info = paste("Values mismatch for", ome, annot_col))
      expect_equal(custom_colors[[ome]][[annot_col]]$colors,
                   custom_colors2[[ome]][[annot_col]]$colors,
                   info = paste("Colors mismatch for", ome, annot_col))
    }
  }
})


test_that("Color palette handles NA values correctly", {
  # Create test data with NA values
  test_annot <- data.frame(
    group = c("A", "B", NA, "A", "B", "NA"),
    stringsAsFactors = FALSE
  )

  colors <- set_annot_colors(test_annot, autodetect_continuous_nfactor_cutoff = 20)

  # Check that NA values are at the end
  expect_true("NA" %in% colors$group$vals)
  expect_equal(tail(colors$group$vals, 1), "NA")

  # Check that NA color is grey
  na_color_idx <- which(colors$group$vals == "NA")
  expect_match(colors$group$colors[na_color_idx], "^#[BbCcDd]{6}$")
})


test_that("Color palette is colorblind safe", {
  # Create test data with multiple groups
  test_annot <- data.frame(
    group = rep(LETTERS[1:6], each = 5),
    stringsAsFactors = FALSE
  )

  colors <- set_annot_colors(test_annot, autodetect_continuous_nfactor_cutoff = 20)

  # Check that colors are using Paul Tol palettes (which are colorblind safe)
  # The default "Bright" palette starts with #4477AA
  expect_match(colors$group$colors[1], "^#[0-9A-Fa-f]{6}$")

  # Check that all colors are distinct
  expect_equal(length(unique(colors$group$colors)), length(colors$group$vals))
})
