################################################################################
# Tests for the PELSA Section 2 self-curated annotation value box.
#
# Covers Task 1: the failed_annotation_count valueBox must return a neutral
# (non-orange) card for self-curated datasets and retain its orange failure
# state for non-self-curated datasets with unannotated accessions.
################################################################################

library(testthat)

# Shared minimal reactives helper (mirrors pattern in test-pelsa-summary.R).
.s2_min_reactives <- function(entry, self_curated_flag) {
  GCTs_and_params   <- shiny::reactiveVal(list(GCTs = list(ds1 = NULL),
                                               parameters = list(ds1 = list())))
  globals           <- shiny::reactiveValues(default_ome = "ds1", colors = list())
  GCTs_original     <- shiny::reactiveVal(list(ds1 = NULL))
  active_dataset    <- shiny::reactive("ds1")
  pelsa_analysis    <- shiny::reactiveVal(list(ds1 = entry))
  pelsa_setup_state <- shiny::reactive(
    list(self_curated = list(ds1 = self_curated_flag))
  )
  list(
    GCTs_and_params   = GCTs_and_params,
    globals           = globals,
    GCTs_original     = GCTs_original,
    active_dataset    = active_dataset,
    pelsa_analysis    = pelsa_analysis,
    pelsa_setup_state = pelsa_setup_state
  )
}

# A synthetic cache entry that would normally trigger an orange failure box:
# 3 unannotated accessions -> n_unannotated_accessions = 3.
.s2_failed_entry <- function() {
  list(
    qc           = list(n_unannotated_accessions = 3L),
    unannotated  = c("A", "B", "C"),
    stage        = "done"
  )
}

# ---- self-curated: neutral card (not orange) ---------------------------------

test_that("failed_annotation_count is neutral for a self-curated dataset", {
  entry <- .s2_failed_entry()
  args  <- .s2_min_reactives(entry, self_curated_flag = TRUE)

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = args,
    {
      vb   <- output$failed_annotation_count
      html <- as.character(vb$html %||% vb)
      expect_false(grepl("orange", html, ignore.case = TRUE),
                   info = paste("Expected no 'orange' in valueBox HTML; got:", html))
      expect_true(grepl("self-curated", html, ignore.case = TRUE),
                  info = paste("Expected 'self-curated' in valueBox HTML; got:", html))
    }
  )
})

# ---- non-self-curated: orange failure card stays ----------------------------

test_that("failed_annotation_count stays orange for a non-self-curated dataset with failures", {
  entry <- .s2_failed_entry()
  args  <- .s2_min_reactives(entry, self_curated_flag = FALSE)

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = args,
    {
      vb   <- output$failed_annotation_count
      html <- as.character(vb$html %||% vb)
      expect_true(grepl("orange", html, ignore.case = TRUE),
                  info = paste("Expected 'orange' in valueBox HTML; got:", html))
    }
  )
})
