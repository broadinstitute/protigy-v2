# Minimal but COMPLETE volcano frame: enough columns that marker_split,
# color_column (significance + feature), and pelsa_volcano_tip all run without
# error. Two rows (one marker, one background) so both base traces are non-empty.
make_webgl_vdf <- function() {
  df <- data.frame(
    id                   = c("p1", "p2"),
    logFC                = c(1.5, -2.0),
    logP                 = c(3.0, 4.0),
    adj.P.Val            = c(0.01, 0.001),
    is_marker            = c(FALSE, TRUE),
    sig_color            = c("grey70", "red"),
    feature_color        = c("grey70", "blue"),
    pep_start            = c(10L, 20L),
    pep_end              = c(15L, 25L),
    winning_gene         = c("GENEA", "GENEB"),
    PG.Genes             = c("GENEA", "GENEB"),
    winning_accession    = c("A1", "A2"),
    PG.ProteinAccessions = c("A1", "A2"),
    label                = c("GENEA", "GENEB"),
    stringsAsFactors     = FALSE
  )
  attr(df, "y_cutoff") <- 2.0
  df
}

# Pull the trace `type` of every data trace from a built plotly object.
built_trace_types <- function(p) {
  b <- plotly::plotly_build(p)
  vapply(b$x$data, function(tr) tr$type %||% NA_character_, character(1))
}

test_that("build_plot defaults to scattergl (WebGL) trace types", {
  p <- pelsa_volcano_build_plot(make_webgl_vdf())
  types <- built_trace_types(p)
  expect_true(length(types) >= 2L)
  expect_true(all(types == "scattergl"))
})

test_that("build_plot with use_webgl=FALSE emits SVG scatter trace types", {
  p <- pelsa_volcano_build_plot(make_webgl_vdf(), use_webgl = FALSE)
  types <- built_trace_types(p)
  expect_true(length(types) >= 2L)
  expect_true(all(types == "scatter"))
})

test_that("use_webgl does not change trace count or meta tags", {
  pg <- plotly::plotly_build(pelsa_volcano_build_plot(make_webgl_vdf()))
  ps <- plotly::plotly_build(
    pelsa_volcano_build_plot(make_webgl_vdf(), use_webgl = FALSE))
  expect_equal(length(pg$x$data), length(ps$x$data))
  expect_identical(pg$x$data[[1L]]$meta, "pelsa_bg")
  expect_identical(ps$x$data[[1L]]$meta, "pelsa_bg")
  expect_identical(pg$x$data[[2L]]$meta, "pelsa_mk")
  expect_identical(ps$x$data[[2L]]$meta, "pelsa_mk")
})

test_that("gold overlay trace honors use_webgl", {
  df <- make_webgl_vdf()
  sel <- list(origin = "volcano", accession = "A2", peptide_seq = NA,
              row = 2L)
  tr_gl  <- pelsa_volcano_gold_trace(df, selection = sel, use_webgl = TRUE)
  tr_svg <- pelsa_volcano_gold_trace(df, selection = sel, use_webgl = FALSE)
  expect_equal(tr_gl$type,  "scattergl")
  expect_equal(tr_svg$type, "scatter")
  expect_identical(tr_gl$meta, "pelsa_gold")
})

test_that("clicked-point overlay trace honors use_webgl", {
  df <- make_webgl_vdf()
  sel <- list(origin = "volcano", accession = "A2", peptide_seq = NA,
              row = 2L)
  tr_gl  <- pelsa_volcano_clicked_point_trace(df, selection = sel,
                                              use_webgl = TRUE)
  tr_svg <- pelsa_volcano_clicked_point_trace(df, selection = sel,
                                              use_webgl = FALSE)
  expect_equal(tr_gl$type,  "scattergl")
  expect_equal(tr_svg$type, "scatter")
  expect_identical(tr_gl$meta, "pelsa_gold_click")
})

# A volcano frame with NA-logFC rows interleaved among plottable ones. plotly
# prunes NA-x from a trace's coordinate arrays but NOT from the parallel
# marker.color / text vectors, so if the build passes the full-length color/tip
# vectors alongside x=logFC, the colors shift off their points and significant
# peptides render with the wrong (gray) color. This fixture reproduces that: the
# significant p3/p5 (rows AFTER the NA-logFC p2/p4) must keep their sig colors.
make_na_logfc_vdf <- function() {
  df <- data.frame(
    id                   = c("p1", "p2", "p3", "p4", "p5"),
    logFC                = c(0.5, NA, -1.2, NA, 1.4),   # p2/p4 NA -> pruned from x
    logP                 = c(1.0, 4.0, 6.0, 4.0, 6.5),
    adj.P.Val            = c(0.5, 0.001, 0.001, 0.001, 0.001),
    P.Value              = c(0.3, 1e-4, 1e-6, 1e-4, 1e-6),
    is_marker            = rep(FALSE, 5),
    sig_color            = c("gray", "gray", "#1f4e9c", "gray", "darkred"),
    feature_color        = rep("grey70", 5),
    pep_start            = rep(10L, 5),
    pep_end              = rep(15L, 5),
    winning_gene         = paste0("G", 1:5),
    PG.Genes             = paste0("G", 1:5),
    winning_accession    = paste0("A", 1:5),
    PG.ProteinAccessions = paste0("A", 1:5),
    label                = paste0("G", 1:5),
    stringsAsFactors     = FALSE
  )
  attr(df, "y_cutoff") <- 5.0
  df
}

test_that("build_plot keeps color/text aligned with x when NA-logFC rows exist", {
  # NA-logFC rows must not desync marker.color/text from the pruned x/y arrays.
  for (uw in c(TRUE, FALSE)) {
    p <- suppressWarnings(
      pelsa_volcano_build_plot(make_na_logfc_vdf(), use_webgl = uw))
    b <- suppressWarnings(plotly::plotly_build(p))
    bg <- b$x$data[[1L]]   # background trace (index 0)
    nx <- length(bg$x)
    expect_equal(length(bg$y), nx)
    expect_equal(length(bg$marker$color), nx,
                 info = paste("marker.color length must equal x length; use_webgl =", uw))
    expect_equal(length(bg$text), nx,
                 info = paste("text length must equal x length; use_webgl =", uw))
    # The plotted colors must be exactly the sig colors of the plottable
    # (non-NA-logFC) rows, IN ORDER: p1 gray, p3 blue, p5 red.
    expect_identical(as.character(bg$marker$color),
                     c("gray", "#1f4e9c", "darkred"))
  }
})
