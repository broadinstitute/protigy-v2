################################################################################
# Tests for the "fresh start on variable change" reset in the LM setup module.
#
# Bug: after configuring a 2-variable model (reference levels, contrasts), the
# user unchecks one variable. The stale contrast rows reference design
# coefficients that no longer exist, and the stale interaction_terms selection
# used to crash formula_string(). Expected behavior (user-stated): changing the
# set of selected variables clears the contrast configuration for a fresh start
# against the new variable list.
#
# These tests drive lmSetup_Tab_Server via shiny::testServer and assert that
# mutating input$selected_variables reseeds contrast_rows to a single empty row
# and does not error.
################################################################################

library(testthat)

# Minimal GCTs_and_params fixture backed by the packaged brca proteome GCT so
# cdesc()/GCTs() resolve inside the setup server.
make_lm_setup_gcap <- function() {
  e <- new.env()
  utils::data("brca_retrospective_v5.0_proteome_gct", package = "Protigy",
              envir = e)
  g <- e[["brca_retrospective_v5.0_proteome_gct"]]
  shiny::reactiveVal(list(
    GCTs = list(proteome = g),
    parameters = list(proteome = list(annotation_column = "geneSymbol"))
  ))
}

make_globals <- function() {
  shiny::reactiveValues(default_ome = "proteome", colors = list())
}

# A non-empty contrast-row list standing in for user-configured contrasts.
# Uses PAM50 coefficient names (real factor levels in the brca proteome cdesc).
configured_rows <- function() {
  list(list(id = "row_1", type = "simple",
            num = "PAM50LumA", den = "PAM50Basal",
            advanced_expr = "", label = "LumA-Basal", label_user_edited = FALSE),
       list(id = "row_2", type = "simple",
            num = "PAM50LumB", den = "PAM50Basal",
            advanced_expr = "", label = "LumB-Basal", label_user_edited = FALSE))
}


test_that("changing selected_variables reseeds contrast_rows to a single empty row", {
  shiny::testServer(
    lmSetup_Tab_Server,
    args = list(
      id = "lm",
      GCTs_and_params = make_lm_setup_gcap(),
      globals = make_globals()
    ),
    {
      session$setInputs(selected_ome = "proteome")
      # User has configured two contrasts against a 2-variable model.
      session$setInputs(selected_variables = c("PAM50", "ER.Status"))
      contrast_rows(configured_rows())
      expect_length(contrast_rows(), 2L)

      # User unchecks one variable -> fresh start.
      session$setInputs(selected_variables = "PAM50")

      rows <- contrast_rows()
      expect_length(rows, 1L)
      expect_identical(rows[[1]]$type, "simple")
      expect_identical(rows[[1]]$num, "")
      expect_identical(rows[[1]]$den, "")
    }
  )
})


test_that("unchecking a variable after an interaction was chosen does not error", {
  # The original crash: interaction_terms persists stale (non-NULL) after a
  # variable is removed, and formula_string()'s combn() blew up on length 1.
  shiny::testServer(
    lmSetup_Tab_Server,
    args = list(
      id = "lm",
      GCTs_and_params = make_lm_setup_gcap(),
      globals = make_globals()
    ),
    {
      session$setInputs(selected_ome = "proteome")
      session$setInputs(selected_variables = c("PAM50", "ER.Status"))
      session$setInputs(interaction_terms = "PAM50 : ER.Status")

      # Drop to one variable; interaction_terms is still non-NULL (stale).
      expect_error(
        session$setInputs(selected_variables = "PAM50"),
        NA  # NA => assert NO error is thrown
      )
      # formula_string must still resolve to a valid single-variable formula.
      expect_match(formula_string(), "PAM50")
    }
  )
})
