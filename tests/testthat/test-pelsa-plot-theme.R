test_that("pelsa_plot_theme encodes the typography contract", {
  th <- pelsa_plot_theme()
  expect_s3_class(th, "theme")

  # Title: size 14, bold, centered
  expect_equal(th$plot.title$size, 14)
  expect_equal(th$plot.title$face, "bold")
  expect_equal(th$plot.title$hjust, 0.5)
  expect_equal(th$plot.title.position, "plot")

  # Subtitle: size 12, centered
  expect_equal(th$plot.subtitle$size, 12)
  expect_equal(th$plot.subtitle$hjust, 0.5)

  # Axis titles: size 12, bold; axis text: size 10
  expect_equal(th$axis.title$size, 12)
  expect_equal(th$axis.title$face, "bold")
  expect_equal(th$axis.text$size, 10)

  # Legend: title 12 bold, text 11
  expect_equal(th$legend.title$size, 12)
  expect_equal(th$legend.title$face, "bold")
  expect_equal(th$legend.text$size, 11)

  # Gridlines removed by default
  expect_s3_class(th$panel.grid.major, "element_blank")
  expect_s3_class(th$panel.grid.minor, "element_blank")
})

test_that("pelsa_plot_theme(gridlines = TRUE) keeps gridlines", {
  th <- pelsa_plot_theme(gridlines = TRUE)
  # When gridlines are kept, we do NOT blank them (inherit theme_bw defaults)
  expect_false(inherits(th$panel.grid.major, "element_blank"))
})

test_that("pelsa_plot_theme composes onto a plot without error", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point() +
    pelsa_plot_theme()
  built <- ggplot2::ggplot_build(p)
  expect_s3_class(built, "ggplot_built")
})
