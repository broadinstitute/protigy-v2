# The Statistics volcano renders via ggplotly then optionally converts the SVG
# scatter traces to WebGL scattergl via stat_volcano_apply_webgl(). A plain
# plot_ly scatter is a faithful stand-in for the SVG plot the render produces:
# toWebGL must convert scatter -> scattergl, and the helper must leave it as
# SVG scatter when use_webgl is FALSE.

trace_type_of <- function(p) {
  b <- plotly::plotly_build(p)
  b$x$data[[1L]]$type
}

base_svg_plot <- function() {
  plotly::plot_ly(x = 1:3, y = c(2, 1, 3), type = "scatter", mode = "markers")
}

test_that("stat_volcano_apply_webgl converts to scattergl when capable", {
  p <- stat_volcano_apply_webgl(base_svg_plot(), use_webgl = TRUE)
  expect_equal(trace_type_of(p), "scattergl")
})

test_that("stat_volcano_apply_webgl leaves SVG scatter when not capable", {
  p <- stat_volcano_apply_webgl(base_svg_plot(), use_webgl = FALSE)
  expect_equal(trace_type_of(p), "scatter")
})

test_that("stat_volcano_apply_webgl defaults to WebGL", {
  p <- stat_volcano_apply_webgl(base_svg_plot())
  expect_equal(trace_type_of(p), "scattergl")
})

test_that("stat_volcano_apply_webgl falls back to the input plot on toWebGL error", {
  # Force the conversion to throw and confirm our tryCatch returns the original
  # SVG plot unchanged (the render must never crash on a toWebGL failure).
  p <- base_svg_plot()
  out <- testthat::with_mocked_bindings(
    suppressMessages(stat_volcano_apply_webgl(p, use_webgl = TRUE)),
    toWebGL = function(p) stop("boom"),
    .package = "plotly"
  )
  expect_identical(out, p)
})
