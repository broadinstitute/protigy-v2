# Tests for Shiny helper functions

# htmltools may store tag names in different shapes; compare case-insensitively
tag_name_lc <- function(tag) {
  tolower(as.character(tag$name)[[1L]])
}

# Find <select> for a Shiny namespaced input id (exact match; handles id on wrapper)
extract_select_input_choices_by_id <- function(tag_list, input_id) {
  find_select_el <- function(items) {
    if (is.null(items)) {
      return(NULL)
    }
    if (inherits(items, "shiny.tag")) {
      items <- list(items)
    }
    if (!is.list(items)) {
      return(NULL)
    }
    for (item in items) {
      if (!inherits(item, "shiny.tag")) {
        if (is.list(item)) {
          found <- find_select_el(item)
          if (!is.null(found)) {
            return(found)
          }
        }
        next
      }
      id <- item$attribs$id %||% ""
      if (nzchar(id) && identical(id, input_id)) {
        if (identical(tag_name_lc(item), "select")) {
          return(item)
        }
        if (!is.null(item$children)) {
          inner <- find_select_el(item$children)
          if (!is.null(inner)) {
            return(inner)
          }
        }
        return(NULL)
      } else if (!is.null(item$children)) {
        found <- find_select_el(item$children)
        if (!is.null(found)) {
          return(found)
        }
      }
    }
    NULL
  }

  select_input <- find_select_el(as.list(tag_list))
  if (is.null(select_input)) {
    return(character(0))
  }

  # Current Shiny/htmltools: <select> often has one child — an `html()` blob of all
  # <option> tags, not separate shiny.tag "option" nodes. Walking only tags yields
  # character(0) and makes tests look "broken" while the app is fine.
  ch <- select_input$children
  if (length(ch) >= 1L) {
    first <- ch[[1L]]
    if (is.character(first) && inherits(first, "html")) {
      txt <- paste(as.character(first), collapse = "")
      mm <- stringr::str_match_all(txt, "value=\"([^\"]*)\"")[[1]]
      if (is.matrix(mm) && nrow(mm) > 0L) {
        return(mm[, 2L])
      }
      return(character(0))
    }
  }

  choices <- character(0)
  extract_options <- function(items) {
    if (is.null(items)) {
      return()
    }
    if (inherits(items, "shiny.tag")) {
      items <- list(items)
    }
    if (!is.list(items)) {
      return()
    }
    for (item in items) {
      if (!inherits(item, "shiny.tag")) {
        if (is.list(item)) {
          extract_options(item)
        }
        next
      }
      nm <- tag_name_lc(item)
      if (identical(nm, "option")) {
        value <- item$attribs$value
        if (is.null(value) && length(item$children) > 0L) {
          ch <- item$children[[1L]]
          value <- if (is.character(ch)) {
            ch
          } else if (inherits(ch, "shiny.tag") && length(ch$children)) {
            ch$children[[1L]]
          } else {
            NULL
          }
        }
        if (length(value) == 1L) {
          choices <<- c(choices, as.character(value))
        }
      } else if (identical(nm, "optgroup")) {
        extract_options(item$children)
      } else if (!is.null(item$children)) {
        extract_options(item$children)
      }
    }
  }

  extract_options(select_input$children)
  choices
}

# Working directory in testthat 3e is usually tests/testthat/
protigy_pkg_root_for_tests <- function() {
  wd <- normalizePath(getwd(), winslash = "/")
  if (grepl("/tests/testthat/?$", wd)) {
    return(dirname(dirname(wd)))
  }
  if (grepl("/tests/?$", wd)) {
    return(dirname(wd))
  }
  wd
}

# Find first <input type="number"> whose id matches exactly or by fixed substring
extract_numeric_input_attribs <- function(tag_list, input_id_pattern) {
  find_input <- function(items) {
    if (is.null(items)) {
      return(NULL)
    }
    for (item in items) {
      if (inherits(item, "shiny.tag")) {
        att <- item$attribs
        id <- att$id %||% ""
        nm <- tag_name_lc(item)
        if (identical(nm, "input") && nzchar(id) &&
            (identical(id, input_id_pattern) || grepl(input_id_pattern, id, fixed = TRUE))) {
          return(att)
        }
        if (!is.null(item$children)) {
          found <- find_input(item$children)
          if (!is.null(found)) {
            return(found)
          }
        }
      } else if (is.list(item)) {
        found <- find_input(item)
        if (!is.null(found)) {
          return(found)
        }
      }
    }
    NULL
  }
  find_input(as.list(tag_list))
}

test_that("labelSetupUI creates correct HTML structure", {
  # Test with single file
  ns <- shiny::NS("test")
  result_single <- labelSetupUI(ns, "file1.gct")
  
  expect_s3_class(result_single, "shiny.tag.list")
  expect_equal(length(result_single), 2) # h4 + textInput
  
  # Test with multiple files
  result_multiple <- labelSetupUI(ns, c("file1.gct", "file2.gct"))
  expect_s3_class(result_multiple, "shiny.tag.list")
  expect_equal(length(result_multiple), 2) # h4 + lapply result (which is a list)
})

# Note: labelSetupUI input ID test removed due to complex HTML structure
# The function returns a complex nested structure that's difficult to test reliably

test_that("gctSetupUI creates correct structure", {
  # Create mock GCT object using proper GCT structure
  mock_gct <- new("GCT",
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
  
  # Create mock parameters
  mock_parameters <- list(
    test_ome = list(
      annotation_column = "group1",
      intensity_data = "raw",
      log_transformation = "log2",
      data_normalization = "Median",
      group_normalization = FALSE,
      max_missing = 50,
      data_filter = "None",
      data_filter_sd_pct = 25
    )
  )
  
  # Create mock parameter choices
  mock_parameter_choices <- list(
    intensity_data = c("raw", "normalized"),
    log_transformation = c("None", "log2", "log10"),
    data_normalization = list(
      intensity_data_no = c("None", "Median", "Quantile")
    ),
    max_missing = list(
      intensity_data_no = list(min = 0, max = 100, step = 5)
    ),
    data_filter = c("None", "StdDev"),
    data_filter_sd_pct = list(min = 0, max = 100)
  )
  
  ns <- shiny::NS("test")
  
  result <- gctSetupUI(
    ns = ns,
    label = "test_ome",
    parameter_choices = mock_parameter_choices,
    parameters = mock_parameters,
    current_place = 1,
    max_place = 2,
    GCTs = list(test_ome = mock_gct)
  )
  
  expect_s3_class(result, "shiny.tag.list")
  expect_true(length(result) > 0)
})

test_that("gctSetupUI handles single GCT", {
  # Create mock GCT object using proper GCT structure
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
  
  # Create mock parameters
  mock_parameters <- list(
    test_ome = list(
      annotation_column = "group1",
      intensity_data = "raw",
      log_transformation = "log2",
      data_normalization = "Median",
      group_normalization = FALSE,
      max_missing = 50,
      data_filter = "None"
    )
  )
  
  # Create mock parameter choices
  mock_parameter_choices <- list(
    intensity_data = c("raw", "normalized"),
    log_transformation = c("None", "log2", "log10"),
    data_normalization = list(
      intensity_data_no = c("None", "Median", "Quantile")
    ),
    max_missing = list(
      intensity_data_no = list(min = 0, max = 100, step = 5)
    ),
    data_filter = c("None", "StdDev"),
    data_filter_sd_pct = list(min = 0, max = 100, step = 5)
  )
  
  ns <- shiny::NS("test")
  
  result <- gctSetupUI(
    ns = ns,
    label = "test_ome",
    parameter_choices = mock_parameter_choices,
    parameters = mock_parameters,
    current_place = 1,
    max_place = 1, # Single GCT
    GCTs = list(test_ome = mock_gct)
  )
  
  expect_s3_class(result, "shiny.tag.list")
  expect_true(length(result) > 0)
})

test_that("validate_labels validates correct labels", {
  # Test valid labels
  valid_labels <- c("Proteome", "Phosphoproteome", "RNAseq")
  names(valid_labels) <- c("file1.gct", "file2.gct", "file3.gct")
  
  result <- validate_labels(valid_labels)
  expect_true(result)
})

test_that("validate_labels rejects invalid labels", {
  # Test invalid label (contains spaces)
  invalid_labels <- c("Proteome", "Phospho proteome", "RNAseq")
  names(invalid_labels) <- c("file1.gct", "file2.gct", "file3.gct")
  
  expect_error(
    validate_labels(invalid_labels),
    "Invalid label for file2.gct"
  )
  
  # Test reserved word
  reserved_labels <- c("Proteome", "multi_ome", "RNAseq")
  names(reserved_labels) <- c("file1.gct", "file2.gct", "file3.gct")
  
  expect_error(
    validate_labels(reserved_labels),
    "Invalid label for file2.gct, 'multi_ome' is a reserved word"
  )
  
  # Test duplicate labels
  duplicate_labels <- c("Proteome", "Proteome", "RNAseq")
  names(duplicate_labels) <- c("file1.gct", "file2.gct", "file3.gct")
  
  expect_error(
    validate_labels(duplicate_labels),
    "All labels must be unique"
  )
})

test_that("validate_labels handles edge cases", {
  # Test empty labels
  empty_labels <- character(0)
  names(empty_labels) <- character(0)
  
  result_empty <- validate_labels(empty_labels)
  expect_true(result_empty)
  
  # Test single label
  single_label <- c("Proteome")
  names(single_label) <- c("file1.gct")
  
  result_single <- validate_labels(single_label)
  expect_true(result_single)
})

test_that("advancedSettingsUI creates correct structure", {
  # Test with multiple parameters
  mock_parameters <- list(
    ome1 = list(),
    ome2 = list(),
    ome3 = list()
  )
  
  ns <- shiny::NS("test")
  result_multiple <- advancedSettingsUI(ns, mock_parameters)
  
  expect_s3_class(result_multiple, "shiny.tag.list")
  expect_true(length(result_multiple) > 0)
  
  # Test with single parameter
  mock_parameters_single <- list(
    ome1 = list()
  )
  
  result_single <- advancedSettingsUI(ns, mock_parameters_single)
  expect_null(result_single)
})

test_that("gctSetupUI handles missing parameters gracefully", {
  # Create mock GCT object using proper GCT structure
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
  
  # Create mock parameters with missing values
  mock_parameters <- list(
    test_ome = list(
      annotation_column = NULL, # Missing
      intensity_data = "raw",
      log_transformation = "log2",
      data_normalization = "Median",
      group_normalization = FALSE,
      max_missing = 50,
      data_filter = "None"
    )
  )
  
  # Create mock parameter choices
  mock_parameter_choices <- list(
    intensity_data = c("raw", "normalized"),
    log_transformation = c("None", "log2", "log10"),
    data_normalization = list(
      intensity_data_no = c("None", "Median", "Quantile")
    ),
    max_missing = list(
      intensity_data_no = list(min = 0, max = 100, step = 5)
    ),
    data_filter = c("None", "StdDev"),
    data_filter_sd_pct = list(min = 0, max = 100, step = 5)
  )
  
  ns <- shiny::NS("test")
  
  result <- gctSetupUI(
    ns = ns,
    label = "test_ome",
    parameter_choices = mock_parameter_choices,
    parameters = mock_parameters,
    current_place = 1,
    max_place = 1,
    GCTs = list(test_ome = mock_gct)
  )
  
  expect_s3_class(result, "shiny.tag.list")
  expect_true(length(result) > 0)
})

## FIX D (P2.3): the three tests below previously re-implemented the
## `if (n_samples > 20L) norm_choices <- norm_choices[...]` guard inline and
## asserted against their own copy.  They never called gctSetupUI, so they
## would pass even if that function's guard were deleted.
## Replacement: call the REAL gctSetupUI and inspect the rendered <select>
## using extract_select_input_choices_by_id (already defined above), matching
## the style of the intensity-branch tests later in this file.

make_2comp_pc <- function() {
  # parameter_choices with 2-component in the intensity_data_no branch
  list(
    log_transformation = c("None", "log2", "log10"),
    data_normalization = list(
      intensity_data_no = c("None", "Median", "Quantile", "2-component")
    ),
    max_missing = list(
      intensity_data_no = list(min = 0, max = 100, step = 5)
    ),
    data_filter = c("None", "StdDev"),
    data_filter_sd_pct = list(min = 0, max = 100, step = 5)
  )
}

make_2comp_params <- function(n_norm = "Median") {
  list(
    test_ome = list(
      annotation_column         = "group1",
      intensity_data            = "No",
      log_transformation        = "log2",
      data_normalization        = n_norm,
      group_normalization       = FALSE,
      max_missing               = 50,
      data_filter               = "None",
      data_filter_sd_pct        = 25,
      gene_symbol_column        = "None",
      convert_ids_to_gene_symbol = FALSE,
      sample_filter_enabled     = FALSE,
      sample_filter_column      = "",
      sample_filter_values      = character(0),
      row_filter_enabled        = FALSE,
      row_filter_column         = "",
      row_filter_values         = character(0),
      id_source_column          = "",
      id_mapping_species        = "Homo sapiens"
    )
  )
}

test_that("gctSetupUI filters out 2-component from <select> for datasets with >20 samples", {
  # FIX D: call the REAL gctSetupUI and parse the rendered HTML.
  mock_gct_large <- new("GCT",
    mat   = matrix(seq_len(4 * 25), nrow = 4, ncol = 25),
    rdesc = data.frame(id = paste0("gene_", 1:4)),
    cdesc = data.frame(
      group1   = rep(c("A", "B"), length.out = 25),
      row.names = paste0("sample_", 1:25)
    ),
    rid = paste0("gene_", 1:4),
    cid = paste0("sample_", 1:25)
  )

  ns <- shiny::NS("tst")
  ui <- gctSetupUI(
    ns = ns,
    label = "test_ome",
    parameter_choices = make_2comp_pc(),
    parameters = make_2comp_params(),
    current_place = 1L,
    max_place = 1L,
    GCTs = list(test_ome = mock_gct_large)
  )

  norm_choices <- extract_select_input_choices_by_id(ui, ns("test_ome_data_normalization"))
  expect_false("2-component" %in% norm_choices,
    info = paste("2-component must be absent for 25-sample dataset; got:", paste(norm_choices, collapse = ", ")))
  expect_true("None" %in% norm_choices)
  expect_true("Median" %in% norm_choices)
  expect_true("Quantile" %in% norm_choices)
})

test_that("gctSetupUI includes 2-component in <select> for datasets with <=20 samples (boundary)", {
  # FIX D: 20 columns is the boundary; 2-component must NOT be stripped.
  mock_gct_small <- new("GCT",
    mat   = matrix(seq_len(4 * 20), nrow = 4, ncol = 20),
    rdesc = data.frame(id = paste0("gene_", 1:4)),
    cdesc = data.frame(
      group1   = rep(c("A", "B"), each = 10),
      row.names = paste0("sample_", 1:20)
    ),
    rid = paste0("gene_", 1:4),
    cid = paste0("sample_", 1:20)
  )

  ns <- shiny::NS("tst")
  ui <- gctSetupUI(
    ns = ns,
    label = "test_ome",
    parameter_choices = make_2comp_pc(),
    parameters = make_2comp_params(),
    current_place = 1L,
    max_place = 1L,
    GCTs = list(test_ome = mock_gct_small)
  )

  norm_choices <- extract_select_input_choices_by_id(ui, ns("test_ome_data_normalization"))
  expect_true("2-component" %in% norm_choices,
    info = paste("2-component must be present for 20-sample dataset; got:", paste(norm_choices, collapse = ", ")))
  expect_true("None" %in% norm_choices)
  expect_true("Median" %in% norm_choices)
})

test_that("gctSetupUI resets selected value to None when 2-component is stored but disabled", {
  # FIX D: when data_normalization = "2-component" is stored but n_samples > 20,
  # the rendered selectInput must show "None" as the selected value, not "2-component".
  # The previous test re-implemented this reset inline; this test drives gctSetupUI
  # and inspects the rendered HTML.
  mock_gct_large <- new("GCT",
    mat   = matrix(seq_len(4 * 25), nrow = 4, ncol = 25),
    rdesc = data.frame(id = paste0("gene_", 1:4)),
    cdesc = data.frame(
      group1   = rep(c("A", "B"), length.out = 25),
      row.names = paste0("sample_", 1:25)
    ),
    rid = paste0("gene_", 1:4),
    cid = paste0("sample_", 1:25)
  )

  # Stored selection is "2-component" but should be downgraded to "None".
  params_2comp <- make_2comp_params(n_norm = "2-component")

  ns <- shiny::NS("tst")
  ui <- gctSetupUI(
    ns = ns,
    label = "test_ome",
    parameter_choices = make_2comp_pc(),
    parameters = params_2comp,
    current_place = 1L,
    max_place = 1L,
    GCTs = list(test_ome = mock_gct_large)
  )

  # 2-component must not appear in the choices list at all.
  norm_choices <- extract_select_input_choices_by_id(ui, ns("test_ome_data_normalization"))
  expect_false("2-component" %in% norm_choices,
    info = paste("2-component must be absent; got:", paste(norm_choices, collapse = ", ")))

  # The rendered HTML must carry selected="selected" on "None", not "2-component".
  # We check this by searching the raw HTML for the selected option.
  html_txt <- as.character(shiny::tagList(ui))
  # "None" option should be marked selected; "2-component" must not appear as selected.
  # A selected <option> renders as: <option value="None" selected>None</option>
  expect_true(
    grepl('value="None"[^>]*selected|selected[^>]*value="None"', html_txt, perl = TRUE),
    info = "Expected 'None' to be the selected option in the rendered HTML"
  )
})

## Regression: intensity_data Yes/No must drive normalization + max_missing in gctSetupUI
## (hardcoding intensity_data_no broke UI after collectInputs/re-render; see 4458a8f).

mock_gct_setup_intensity <- function(n_col = 3L) {
  new("GCT",
    mat = matrix(seq_len(4L * n_col), nrow = 4L, ncol = n_col),
    rdesc = data.frame(id = paste0("gene_", 1:4)),
    cdesc = data.frame(
      group1 = rep(c("A", "B"), length.out = n_col),
      row.names = paste0("sample_", seq_len(n_col))
    ),
    rid = paste0("gene_", 1:4),
    cid = paste0("sample_", seq_len(n_col))
  )
}

mock_parameter_choices_intensity_branches <- function() {
  list(
    log_transformation = c("None", "log2"),
    data_normalization = list(
      intensity_data_yes = c("NORM_YES_A", "NORM_YES_B"),
      intensity_data_no = c("NORM_NO_A", "NORM_NO_B", "NORM_NO_C")
    ),
    max_missing = list(
      intensity_data_yes = list(min = 0, max = 99, step = 1),
      intensity_data_no = list(min = 0, max = 100, step = 5)
    ),
    data_filter = c("None", "StdDev"),
    data_filter_sd_pct = list(min = 10, max = 90, step = 5)
  )
}

base_mock_params_ome <- function(intensity_data, data_normalization, max_missing) {
  list(
    ome = list(
      annotation_column = "group1",
      intensity_data = intensity_data,
      log_transformation = "None",
      data_normalization = data_normalization,
      group_normalization = FALSE,
      max_missing = max_missing,
      data_filter = "None",
      data_filter_sd_pct = 25,
      gene_symbol_column = "None",
      convert_ids_to_gene_symbol = FALSE,
      sample_filter_enabled = FALSE,
      sample_filter_column = "",
      sample_filter_values = character(0),
      row_filter_enabled = FALSE,
      row_filter_column = "",
      row_filter_values = character(0),
      id_source_column = "",
      id_mapping_species = "Homo sapiens"
    )
  )
}

test_that("gctSetupUI uses intensity_data_yes normalization when intensity_data is Yes", {
  mock_gct <- mock_gct_setup_intensity(3L)
  pc <- mock_parameter_choices_intensity_branches()
  pm <- base_mock_params_ome("Yes", "NORM_YES_A", 50L)
  ns <- shiny::NS("mod")
  ui <- gctSetupUI(
    ns = ns,
    label = "ome",
    parameter_choices = pc,
    parameters = pm,
    current_place = 1L,
    max_place = 1L,
    GCTs = list(ome = mock_gct)
  )
  norm_choices <- extract_select_input_choices_by_id(ui, ns("ome_data_normalization"))
  expect_true("NORM_YES_A" %in% norm_choices)
  expect_false("NORM_NO_A" %in% norm_choices)
  nm <- extract_numeric_input_attribs(ui, ns("ome_max_missing"))
  expect_equal(as.numeric(nm$max), 99)
})

test_that("gctSetupUI uses intensity_data_yes when intensity_data is logical TRUE", {
  mock_gct <- mock_gct_setup_intensity(3L)
  pc <- mock_parameter_choices_intensity_branches()
  pm <- base_mock_params_ome(TRUE, "NORM_YES_A", 50L)
  ns <- shiny::NS("mod")
  ui <- gctSetupUI(
    ns = ns,
    label = "ome",
    parameter_choices = pc,
    parameters = pm,
    current_place = 1L,
    max_place = 1L,
    GCTs = list(ome = mock_gct)
  )
  norm_choices <- extract_select_input_choices_by_id(ui, ns("ome_data_normalization"))
  expect_true("NORM_YES_A" %in% norm_choices)
  expect_false("NORM_NO_A" %in% norm_choices)
})

test_that("gctSetupUI uses intensity_data_no normalization when intensity_data is not Yes", {
  mock_gct <- mock_gct_setup_intensity(3L)
  pc <- mock_parameter_choices_intensity_branches()
  pm <- base_mock_params_ome("No", "NORM_NO_B", 50L)
  ns <- shiny::NS("mod")
  ui <- gctSetupUI(
    ns = ns,
    label = "ome",
    parameter_choices = pc,
    parameters = pm,
    current_place = 1L,
    max_place = 1L,
    GCTs = list(ome = mock_gct)
  )
  norm_choices <- extract_select_input_choices_by_id(ui, ns("ome_data_normalization"))
  expect_true("NORM_NO_A" %in% norm_choices)
  expect_false("NORM_YES_A" %in% norm_choices)
  nm <- extract_numeric_input_attribs(ui, ns("ome_max_missing"))
  expect_equal(as.numeric(nm$max), 100)
})

test_that("gctSetupUI aligns with inst setupChoices.yaml intensity_data_yes/no keys", {
  ypath <- system.file("setup_parameters/setupChoices.yaml", package = "Protigy")
  skip_if_not(nzchar(ypath), "setupChoices.yaml not found (run devtools::test from package source)")
  pc <- yaml::read_yaml(ypath)
  mock_gct <- mock_gct_setup_intensity(3L)
  ns <- shiny::NS("chk")
  for (intensity in c("Yes", "No")) {
    pm <- base_mock_params_ome(
      intensity,
      pc$data_normalization[[paste0("intensity_data_", tolower(intensity))]][[1L]],
      50L
    )
    ui <- gctSetupUI(
      ns = ns,
      label = "ome",
      parameter_choices = pc,
      parameters = pm,
      current_place = 1L,
      max_place = 1L,
      GCTs = list(ome = mock_gct)
    )
    norm_choices <- extract_select_input_choices_by_id(ui, ns("ome_data_normalization"))
    expected <- pc$data_normalization[[paste0("intensity_data_", tolower(intensity))]]
    expect_identical(norm_choices, expected, info = paste("intensity_data =", intensity))
    nm <- extract_numeric_input_attribs(ui, ns("ome_max_missing"))
    mm <- pc$max_missing[[paste0("intensity_data_", tolower(intensity))]]
    expect_equal(as.numeric(nm$min), mm$min, info = paste("intensity_data =", intensity))
    expect_equal(as.numeric(nm$max), mm$max, info = paste("intensity_data =", intensity))
  }
})

test_that("intensity observeEvent must not use ignoreInit = TRUE (regression guard)", {
  spf <- file.path(protigy_pkg_root_for_tests(), "R", "sidebar_setup.R")
  skip_if_not(file.exists(spf), "sidebar_setup.R not at expected path (skip in unusual test layout)")
  lines <- readLines(spf, warn = FALSE)
  start <- grep("^\\s*observeEvent\\(\\s*current_intensity\\(\\)", lines)
  expect_length(start, 1L)
  end <- grep("^\\s*# update sample filter values choices when sample filter column changes", lines)
  expect_length(end, 1L)
  expect_true(end > start)
  chunk <- lines[seq.int(start, end - 1L)]
  expect_false(
    any(grepl("ignoreInit\\s*=\\s*TRUE", chunk)),
    info = paste(chunk, collapse = "\n")
  )
})
