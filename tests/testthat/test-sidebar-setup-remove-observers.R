# Regression tests for START-03: remove-button observers must be registered exactly
# once per button id across add / remove / re-add / clear cycles (no accumulation).
#
# We drive the module's `accumulated_files` reactiveVal directly (rather than the full
# file-upload pipeline) so the test isolates the observer-registration logic that the
# `observe({...})` block in setupSidebarServer is responsible for. The tracker
# `registered_remove_btns` must grow monotonically and contain each id only once.

make_files <- function(names) {
  data.frame(
    name = names,
    size = rep(100L, length(names)),
    type = rep("", length(names)),
    datapath = paste0("/tmp/", names),
    stringsAsFactors = FALSE
  )
}

expected_btn_ids <- function(names) {
  paste0("remove_file_", gsub("[^a-zA-Z0-9_]", "_", names))
}

test_that("remove-button observers register once per id and do not accumulate", {
  shiny::testServer(
    setupSidebarServer,
    args = list(id = "setupSidebar", parent = NULL),
    {
      # No files yet -> no registrations.
      session$flushReact()
      expect_equal(registered_remove_btns(), character(0))

      # Add two files.
      accumulated_files(make_files(c("a.gct", "b.gct")))
      session$flushReact()
      ids_ab <- expected_btn_ids(c("a.gct", "b.gct"))
      expect_setequal(registered_remove_btns(), ids_ab)
      expect_equal(length(registered_remove_btns()), 2L)

      # Add a third (simulate accumulate). Existing ids must NOT re-register.
      accumulated_files(make_files(c("a.gct", "b.gct", "c.gct")))
      session$flushReact()
      ids_abc <- expected_btn_ids(c("a.gct", "b.gct", "c.gct"))
      expect_setequal(registered_remove_btns(), ids_abc)
      expect_equal(length(registered_remove_btns()), 3L)
      # No duplicates.
      expect_equal(length(unique(registered_remove_btns())), 3L)

      # Remove the MIDDLE file (b). Tracker stays monotonic (stale handler is a safe
      # no-op), so it must still contain all three ids and not shrink or grow.
      accumulated_files(make_files(c("a.gct", "c.gct")))
      session$flushReact()
      expect_setequal(registered_remove_btns(), ids_abc)
      expect_equal(length(registered_remove_btns()), 3L)

      # Re-add the same middle filename (b). Its id is already registered -> no new entry.
      accumulated_files(make_files(c("a.gct", "c.gct", "b.gct")))
      session$flushReact()
      expect_setequal(registered_remove_btns(), ids_abc)
      expect_equal(length(registered_remove_btns()), 3L)

      # Several no-op invalidations (same file set) must not grow the tracker.
      for (k in 1:5) {
        accumulated_files(make_files(c("a.gct", "c.gct", "b.gct")))
        session$flushReact()
      }
      expect_equal(length(registered_remove_btns()), 3L)

      # Clear all (NULL) then re-add a previously seen filename: still single handler.
      accumulated_files(NULL)
      session$flushReact()
      expect_equal(length(registered_remove_btns()), 3L)
      accumulated_files(make_files(c("a.gct")))
      session$flushReact()
      expect_equal(length(registered_remove_btns()), 3L)

      # Brand-new filename after clear adds exactly one new id.
      accumulated_files(make_files(c("a.gct", "d.gct")))
      session$flushReact()
      expect_true("remove_file_d_gct" %in% registered_remove_btns())
      expect_equal(length(registered_remove_btns()), 4L)
    }
  )
})

test_that("remove handler removes exactly the targeted file by name", {
  shiny::testServer(
    setupSidebarServer,
    args = list(id = "setupSidebar", parent = NULL),
    {
      accumulated_files(make_files(c("x.gct", "y.gct", "z.gct")))
      session$flushReact()

      # Fire the middle remove button; only y.gct should be removed.
      session$setInputs(remove_file_y_gct = 1L)
      session$flushReact()
      remaining <- accumulated_files()
      expect_equal(remaining$name, c("x.gct", "z.gct"))

      # Removing the last two leaves NULL (full reset path).
      session$setInputs(remove_file_x_gct = 1L)
      session$flushReact()
      session$setInputs(remove_file_z_gct = 1L)
      session$flushReact()
      expect_null(accumulated_files())
    }
  )
})
