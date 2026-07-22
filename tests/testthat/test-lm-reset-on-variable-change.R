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
            num2 = "", den2 = "", label = "LumA-Basal", label_user_edited = FALSE),
       list(id = "row_2", type = "simple",
            num = "PAM50LumB", den = "PAM50Basal",
            num2 = "", den2 = "", label = "LumB-Basal", label_user_edited = FALSE))
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


test_that("flipping a card's mode Single->Multi clears num/den (spec #5)", {
  # Regression test: the persist observe used to clear num/den/num2/den2 on a
  # type toggle, then call contrast_rows(new_rows), which re-invalidated the
  # SAME observe within the same flush. On the re-run, type_val already
  # equalled the updated r$type, so the toggle branch was skipped and the
  # slot-persist branch read the (stale, pre-toggle) num_/den_ inputs back
  # into the row -- silently defeating the clear. Fixed by reseeding the row
  # id on toggle so the stale slot inputs are orphaned (read NULL).
  shiny::testServer(
    lmSetup_Tab_Server,
    args = list(
      id = "lm",
      GCTs_and_params = make_lm_setup_gcap(),
      globals = make_globals()
    ),
    {
      session$setInputs(selected_ome = "proteome")
      session$setInputs(selected_variables = "PAM50")

      # Seed a single Single-mode row with real coefficients set. Input ids
      # are keyed by the row's internal id (num_<id>/den_<id>/type_<id>), so
      # build the setInputs call dynamically via do.call.
      row_id <- contrast_rows()[[1]]$id
      do.call(session$setInputs, setNames(
        list("PAM50LumA", "PAM50Basal"),
        c(paste0("num_", row_id), paste0("den_", row_id))
      ))

      row <- contrast_rows()[[1]]
      expect_identical(row$num, "PAM50LumA")
      expect_identical(row$den, "PAM50Basal")
      expect_identical(row$type, "simple")

      # Flip the mode from Single to Multi using the CURRENT row id.
      do.call(session$setInputs, setNames(
        list("multi"), paste0("type_", row_id)
      ))

      # The fix reassigns the row's id, so read by POSITION, not old id.
      rows <- contrast_rows()
      expect_length(rows, 1L)
      row <- rows[[1]]
      expect_identical(row$type, "multi")
      expect_identical(row$num, "")
      expect_identical(row$den, "")
      expect_identical(row$num2, "")
      expect_identical(row$den2, "")
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


test_that("Multi contrast with net-zero pairs is dropped by contrast_specs", {
  # Regression test: the contrast builder's net-zero guard drops Multi contrasts
  # when both pairs are identical (A-B)-(A-B)=0 or individually zero (A-A)-(B-B)=0.
  # This test verifies the guard is wired and fires correctly.
  shiny::testServer(
    lmSetup_Tab_Server,
    args = list(
      id = "lm",
      GCTs_and_params = make_lm_setup_gcap(),
      globals = make_globals()
    ),
    {
      session$setInputs(selected_ome = "proteome")
      session$setInputs(selected_variables = "PAM50")

      # Seed a single Simple-mode row with real coefficients.
      row_id <- contrast_rows()[[1]]$id
      do.call(session$setInputs, setNames(
        list("PAM50LumA", "PAM50Basal"),
        c(paste0("num_", row_id), paste0("den_", row_id))
      ))

      # Flip to Multi mode. The toggle reassigns the row id.
      do.call(session$setInputs, setNames(
        list("multi"), paste0("type_", row_id)
      ))

      # Re-read the row id AFTER toggle (it changed).
      rows <- contrast_rows()
      expect_length(rows, 1L)
      new_row_id <- rows[[1]]$id

      # Set the four slots to an IDENTICAL-PAIRS net-zero config:
      # num=PAM50LumA, den=PAM50Basal, num2=PAM50LumA, den2=PAM50Basal
      # -> (LumA-Basal)-(LumA-Basal) = 0, should be dropped.
      do.call(session$setInputs, setNames(
        list("PAM50LumA", "PAM50Basal", "PAM50LumA", "PAM50Basal"),
        c(paste0("num_", new_row_id), paste0("den_", new_row_id),
          paste0("num2_", new_row_id), paste0("den2_", new_row_id))
      ))

      # Flush to trigger reactive updates.
      session$flushReact()

      # Assert that the net-zero row is DROPPED by contrast_specs.
      specs <- contrast_specs()
      expect_length(specs, 0L)
    }
  )
})


test_that("Multi contrast with valid (non-net-zero) pairs is kept by contrast_specs", {
  # Positive control: ensure the guard does not over-reject valid contrasts.
  # This test verifies a difference-of-differences contrast with distinct pairs
  # is NOT dropped.
  shiny::testServer(
    lmSetup_Tab_Server,
    args = list(
      id = "lm",
      GCTs_and_params = make_lm_setup_gcap(),
      globals = make_globals()
    ),
    {
      session$setInputs(selected_ome = "proteome")
      session$setInputs(selected_variables = "PAM50")

      # Seed a single Simple-mode row.
      row_id <- contrast_rows()[[1]]$id
      do.call(session$setInputs, setNames(
        list("PAM50LumA", "PAM50Basal"),
        c(paste0("num_", row_id), paste0("den_", row_id))
      ))

      # Flip to Multi mode.
      do.call(session$setInputs, setNames(
        list("multi"), paste0("type_", row_id)
      ))

      # Re-read the row id.
      rows <- contrast_rows()
      new_row_id <- rows[[1]]$id

      # Set VALID (non-zero) difference-of-differences:
      # num=PAM50LumA, den=PAM50Basal, num2=PAM50LumB, den2=PAM50Basal
      # -> (LumA-Basal)-(LumB-Basal) != 0, should NOT be dropped.
      do.call(session$setInputs, setNames(
        list("PAM50LumA", "PAM50Basal", "PAM50LumB", "PAM50Basal"),
        c(paste0("num_", new_row_id), paste0("den_", new_row_id),
          paste0("num2_", new_row_id), paste0("den2_", new_row_id))
      ))

      # Flush to trigger reactive updates.
      session$flushReact()

      # Assert that the valid contrast is KEPT by contrast_specs.
      specs <- contrast_specs()
      expect_length(specs, 1L)
      expect_identical(specs[[1]]$type, "multi")
    }
  )
})
