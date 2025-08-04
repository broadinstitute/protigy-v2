# test_that("labelSetupUI creates expected HTML output", {
#   expect_snapshot(labelSetupUI(ns = shiny::NS("test"), gctFileNames = "file-01.gct"))
# })
# 
# test_that("labelSetupUI creates expected HTML output for multiple inputs", {
#   expect_snapshot(labelSetupUI(ns = shiny::NS("test"), 
#                                gctFileNames = c("file-01.gct", "file-02.gct")))
# })