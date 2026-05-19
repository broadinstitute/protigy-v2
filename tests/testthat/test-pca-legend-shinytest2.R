################################################################################
# Integration test: PCA legend ordering and cdesc value preservation for GCT upload
################################################################################

library(testthat)

test_that("uploaded GCT preserves leading-zero cdesc and PCA legend order", {
  skip_if_no_shinytest2()

  app <- make_app_driver(testthat::test_path("apps/full-app"))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(
    `setupSidebar-dataFiles` = get_test_file("mb-proteome-leadingzero-cdesc.gct_n54x12307.gct")
  )
  app$wait_for_idle(duration = 1000, timeout = 25000)

  # Step 1: labels
  app$set_inputs(
    `setupSidebar-Label_mb-proteome-leadingzero-cdesc.gct_n54x12307.gct` = "Proteome",
    wait_ = FALSE
  )
  app$wait_for_idle(duration = 400)
  wait_for_input_bound(app, "setupSidebar-submitLabelsButton")
  app$click(input = "setupSidebar-submitLabelsButton")
  app$wait_for_idle(duration = 1200, timeout = 25000)

  # Step 2: use a replicated grouping variable so setup processing succeeds.
  app$set_inputs(`setupSidebar-Proteome_annotation_column` = "condition", wait_ = FALSE)
  app$wait_for_idle(duration = 400)
  wait_for_input_bound(app, "setupSidebar-submitGCTButton")
  app$click(input = "setupSidebar-submitGCTButton")
  app$wait_for_idle(duration = 3000, timeout = 45000)

  # Pull processed data from the running app to verify upload -> parse -> process path.
  gcts_and_params <- NULL
  for (i in seq_len(30)) {
    gcts_and_params <- app$get_value(export = "GCTs_and_params")
    if (!is.null(gcts_and_params)) break
    Sys.sleep(1)
  }
  expect_false(is.null(gcts_and_params))
  expect_true("Proteome" %in% names(gcts_and_params$GCTs))

  uploaded_gct <- gcts_and_params$GCTs$Proteome
  expect_true("barcode" %in% names(uploaded_gct@cdesc))
  expect_setequal(unique(as.character(uploaded_gct@cdesc$barcode)), c("001", "002", "010"))

  # Build PCA plot from app-produced GCT and validate legend level ordering.
  p <- create_PCA_plot(
    gct = uploaded_gct,
    col_of_interest = "barcode",
    ome = "Proteome",
    custom_color_map = NULL,
    comp.x = 1,
    comp.y = 2
  )
  expect_s3_class(p, "ggplot")
  expect_identical(levels(p$data$barcode), c("001", "002", "010"))
})
