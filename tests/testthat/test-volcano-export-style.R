################################################################################
# Tests: the visual contract of the exported volcano plot (plotVolcano()).
#
# These pin the figure spec agreed for the Statistics export: page size, type
# sizes and faces, the direction palette, the dashed cutoff line, the gold POI
# marker, and the rule that a feature's label takes the colour of the point it
# labels.
#
# WHY THIS FILE EXISTS AS ASSERTIONS AND NOT AS EYEBALLING: every value here was
# chosen by looking at a rendered PDF. Nothing about "16 pt vs 14 pt" or
# "gray80 vs gray" fails loudly at runtime -- a drifted value produces a plot
# that still renders, still exports, and is simply wrong. Only an assertion
# catches that.
#
# Every assertion below was mutation-tested: the production line it guards was
# reverted and the test confirmed to fail, then restored.
#
# Theme values are read through plot_theme()/calc_element() rather than off
# gg$theme directly, because that is what resolves inheritance from theme_bw()
# -- reading gg$theme alone would miss a value the theme supplies and report a
# NULL that the rendered plot never sees.
################################################################################

library(testthat)

# ---------------------------------------------------------------------------
# Fixture: a six-feature two-sample result covering every colour case.
#
#   id  logFC   P.Value  adj.P.Val  logP    expected
#   A    2.0    1e-5     0.001      5.000   significant, up      -> red
#   F    3.0    1e-6     0.0005     6.000   significant, up      -> red
#   B   -2.0    1e-4     0.002      4.000   significant, down    -> blue
#   C    0.1    0.5      0.9        0.301   insignificant        -> grey
#   E   -0.5    0.2      0.6        0.699   insignificant        -> grey
#   D    1.5    1e-3     0.01       3.000   boundary             -> grey
#
# D is deliberate. With stat = "adj.p.val" the y cutoff is the raw p of the
# largest still-passing adj.P.Val -- D's -- so the line lands exactly at
# logP 3, and plotVolcano's `logP > y_cutoff` (strict) puts D on the grey side
# of its own boundary. Every adj-p fixture has one such feature by construction;
# naming it here stops it looking like a miscalculation later.
#
# COLUMN ORDER MATTERS. plotVolcano resolves the raw-p column with a
# case-insensitive grep for "P\\.value.*<contrast>", and "Log.P.Value.<contrast>"
# also matches that pattern -- so whichever comes first in colnames() wins.
# Real stat.testing() output emits P.Value before Log.P.Value, and this fixture
# mirrors that order. Reversing it silently makes every feature significant.
# ---------------------------------------------------------------------------
make_style_fixture <- function(stat = "adj.p.val", cutoff = 0.05) {
  df <- data.frame(
    id         = c("A", "B", "C", "D", "E", "F"),
    geneSymbol = c("GA", "GB", "GC", "GD", "GE", "GF"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  df[["logFC.X_over_Y"]]       <- c(2, -2, 0.1, 1.5, -0.5, 3)
  df[["P.Value.X_over_Y"]]     <- c(1e-5, 1e-4, 0.5, 1e-3, 0.2, 1e-6)
  df[["adj.P.Val.X_over_Y"]]   <- c(0.001, 0.002, 0.9, 0.01, 0.6, 0.0005)
  df[["Log.P.Value.X_over_Y"]] <- -log10(df[["P.Value.X_over_Y"]])
  sp <- list(myome = list(test = "Two-sample Moderated T-test", cutoff = cutoff,
                          stat = stat, contrasts = "X / Y", groups = NULL))
  list(df = df, statp = function() sp, statr = function() list(myome = df))
}

# NOT wrapped in suppressWarnings(): the export path is supposed to be silent,
# and suppressing here would hide a reintroduced warning from every test in the
# file. See "the export path builds without warnings" below.
build_style_plot <- function(fx, ...) {
  plotVolcano("myome", NULL, "X / Y", fx$df, fx$statp, fx$statr, ...)
}

resolved <- function(gg, element) {
  ggplot2::calc_element(element, ggplot2:::plot_theme(gg))
}

# The single point layer carrying a given colour, or NULL. Category layers are
# built one per category, so colour identifies the layer.
layer_data_for_color <- function(gg, hex) {
  for (ly in gg$layers) {
    d <- ly$data
    if (inherits(ly$geom, "GeomPoint") && is.data.frame(d) &&
        "point_color" %in% names(d) && nrow(d) > 0 &&
        all(d$point_color == hex)) {
      return(d)
    }
  }
  NULL
}

repel_layer <- function(gg) {
  hits <- Filter(function(ly) inherits(ly$geom, "GeomLabelRepel"), gg$layers)
  if (length(hits) == 0L) NULL else hits[[1]]
}

poi_layer <- function(gg) {
  hits <- Filter(
    function(ly) inherits(ly$geom, "GeomPoint") &&
                 identical(ly$aes_params$shape, 21),
    gg$layers
  )
  if (length(hits) == 0L) NULL else hits[[1]]
}

## Warnings ####################################################################

test_that("the export path builds without warnings", {
  # `text` is a plotly convention (ggplotly(tooltip = "text")), not a ggplot2
  # aesthetic, so mapping it makes layer() warn once PER LAYER. The export never
  # reads it and builds one plot per contrast, so mapping it there produced a
  # wall of "Ignoring unknown aesthetics: text" for no benefit.
  fx <- make_style_fixture()
  gg <- expect_no_warning(
    plotVolcano("myome", NULL, "X / Y", fx$df, fx$statp, fx$statr,
                label_proteins = "A", label_mode = c("significant", "poi"))
  )
  # ... and the hover strings behind that aesthetic are not built either. The
  # export loops every contrast over the whole ome, so this is real work
  # (~10k pasted strings per contrast) with no reader.
  expect_null(gg$data$.hover_text)
})

test_that("the interactive path still carries the plotly hover aesthetic", {
  # The export must be silent, but not by breaking the tooltip: interactive =
  # TRUE has to keep mapping `text`, warning and all.
  fx <- make_style_fixture()
  gg <- suppressWarnings(
    plotVolcano("myome", NULL, "X / Y", fx$df, fx$statp, fx$statr,
                interactive = TRUE)
  )
  mapped <- unique(unlist(lapply(gg$layers, function(ly) names(ly$mapping))))
  expect_true("text" %in% c(names(gg$mapping), mapped))
  expect_true(all(nzchar(gg$data$.hover_text)))
})

test_that("volcano_muffle_unknown_aes drops only the aesthetics warning", {
  # A blanket suppressWarnings() at the interactive call site would also swallow
  # real problems, so the muffler matches on the message. This is the assertion
  # that stops it being widened later.
  expect_no_warning(volcano_muffle_unknown_aes(warning("Ignoring unknown aesthetics: text")))
  expect_warning(volcano_muffle_unknown_aes(warning("a genuine problem")),
                 "a genuine problem")
})

## Page size ###################################################################

test_that("the volcano export page is 6 x 5 inches", {
  d <- get_pdf_params("volcano")
  expect_equal(d$width, 6)
  expect_equal(d$height, 5)
  expect_equal(d$units, "in")
})

test_that("the volcano page size does not change the default used by other exports", {
  # get_plot_export_dimensions() is shared by every other plot export
  # (tab_stat_summary.R, the QC tabs). Resizing the volcano by editing the
  # DEFAULT keys instead of the volcano keys would silently resize all of them,
  # and nothing else in the suite would notice.
  d <- get_pdf_params()
  expect_equal(d$width, 8)
  expect_equal(d$height, 6)
})

## Type: sizes and faces #######################################################

test_that("plot title is 14 pt bold and centred", {
  gg <- build_style_plot(make_style_fixture())
  el <- resolved(gg, "plot.title")
  expect_equal(el$size, 14)
  expect_equal(el$face, "bold")
  expect_equal(el$hjust, 0.5)
})

test_that("subtitle is 12 pt, unbolded, black, and centred under the title", {
  gg <- build_style_plot(make_style_fixture())
  el <- resolved(gg, "plot.subtitle")
  expect_equal(el$size, 12)
  expect_equal(el$face, "plain")
  expect_equal(el$colour, "black")
  # theme_bw() left-aligns the subtitle by default; under a centred title that
  # reads as a misalignment, so hjust is set explicitly.
  expect_equal(el$hjust, 0.5)
})

test_that("both axis titles are 12 pt bold and axis text is 8 pt", {
  gg <- build_style_plot(make_style_fixture())
  for (el_name in c("axis.title.x", "axis.title.y")) {
    el <- resolved(gg, el_name)
    expect_equal(el$size, 12, info = el_name)
    expect_equal(el$face, "bold", info = el_name)
  }
  expect_equal(resolved(gg, "axis.text.x")$size, 8)
  expect_equal(resolved(gg, "axis.text.y")$size, 8)
})

test_that("group annotations render at 12 pt, matching the axis titles", {
  # annotate() sizes in MILLIMETRES while element_text() sizes in POINTS, so a
  # literal size = 12 here would draw at ~34 pt. The stored value must be the
  # mm equivalent of 12 pt.
  gg <- build_style_plot(make_style_fixture())
  annots <- Filter(function(ly) inherits(ly$geom, "GeomText"), gg$layers)
  expect_gt(length(annots), 0)
  for (ly in annots) {
    expect_equal(ly$aes_params$size * ggplot2::.pt, 12)
    expect_equal(ly$aes_params$fontface, "bold")
    expect_equal(ly$aes_params$colour, "red")
    expect_equal(ly$aes_params$alpha, 0.6)
  }
})

test_that("gridlines are removed", {
  gg <- build_style_plot(make_style_fixture())
  expect_s3_class(resolved(gg, "panel.grid.major"), "element_blank")
  expect_s3_class(resolved(gg, "panel.grid.minor"), "element_blank")
})

## Title and subtitle text #####################################################

test_that("the title names the contrast with 'vs' and carries no cutoff clause", {
  gg <- build_style_plot(make_style_fixture())
  expect_equal(gg$labels$title, "Volcano plot for myome: X vs Y")
  # The contrast is stored as "X / Y"; the slash is a storage detail, not a
  # thing to show a reader. The cutoff belongs to the subtitle now.
  expect_false(grepl("/", gg$labels$title, fixed = TRUE))
  expect_false(grepl("cutoff", gg$labels$title, fixed = TRUE))
})

test_that("the subtitle carries the cutoff without parentheses", {
  gg <- build_style_plot(make_style_fixture())
  expect_equal(gg$labels$subtitle, "Adj. P cutoff: 0.05")
})

test_that("the subtitle names nominal p when that is the stat the cutoff uses", {
  # The stat is user-selectable in Statistics > Summary. Hardcoding "Adj. P"
  # would print a false label on every nominal-p run.
  gg <- build_style_plot(make_style_fixture(stat = "nom.p.val"))
  expect_equal(gg$labels$subtitle, "Nom. P cutoff: 0.05")
})

test_that("the subtitle tracks the shared cutoff instead of naming a fixed one", {
  # THE POINT OF THIS TEST: the two assertions above use a fixture whose cutoff
  # happens to be 0.05, so they pass just as well against a hardcoded "0.05".
  # Only varying the cutoff distinguishes "read from stat_params()[[ome]]$cutoff"
  # from "printed a literal". That setting is shared with the colouring and the
  # cutoff line (Statistics > Summary), so a subtitle that disagreed with it
  # would be describing a plot other than the one on the page.
  for (cut in c(0.01, 0.1, 0.25)) {
    gg <- build_style_plot(make_style_fixture(cutoff = cut))
    expect_equal(gg$labels$subtitle, paste0("Adj. P cutoff: ", cut),
                 info = paste("cutoff =", cut))
  }
})

test_that("a non-default cutoff moves the line and the colours with the subtitle", {
  # The subtitle is only honest if the rest of the figure moved too. At 0.01,
  # D (adj.P.Val = 0.01, not < 0.01) drops out of the passing set, which raises
  # the y cutoff and turns B grey.
  gg <- build_style_plot(make_style_fixture(cutoff = 0.01))
  expect_equal(gg$labels$subtitle, "Adj. P cutoff: 0.01")
  expect_setequal(layer_data_for_color(gg, "red")$id, c("A", "F"))
  expect_true("B" %in% layer_data_for_color(gg, "gray80")$id)
})

test_that("axis titles name the transform applied to each axis", {
  gg <- build_style_plot(make_style_fixture())
  expect_equal(gg$labels$x, "log2(Fold Change)")
  expect_equal(gg$labels$y, "-log10(Nom. P)")
})

## Cutoff line #################################################################

test_that("the significance cutoff line is dashed", {
  gg <- build_style_plot(make_style_fixture())
  hl <- Filter(function(ly) inherits(ly$geom, "GeomHline"), gg$layers)
  expect_length(hl, 1L)
  expect_equal(hl[[1]]$aes_params$linetype, "dashed")
})

## Direction palette ###########################################################

test_that("significant points are coloured by direction and the rest stay grey", {
  gg <- build_style_plot(make_style_fixture())

  up   <- layer_data_for_color(gg, "red")
  down <- layer_data_for_color(gg, "blue")
  grey <- layer_data_for_color(gg, "gray80")

  expect_setequal(up$id,   c("A", "F"))        # significant, logFC > 0
  expect_setequal(down$id, "B")                # significant, logFC < 0
  expect_setequal(grey$id, c("C", "D", "E"))   # do not clear the cutoff
})

test_that("direction alone never colours a point -- significance gates it", {
  # C has a positive logFC but does not clear the cutoff, so it must be grey
  # rather than red. This is the property that keeps the colours consistent
  # with the cutoff line: everything coloured sits above it.
  gg <- build_style_plot(make_style_fixture())
  grey <- layer_data_for_color(gg, "gray80")
  # C and D both have logFC > 0, same sign as the red points, and are still grey.
  expect_true(all(c("C", "D") %in% grey$id))
  expect_true(all(grey$logFC[grey$id %in% c("C", "D")] > 0))
  expect_false(any(grey$id %in% layer_data_for_color(gg, "red")$id))
})

test_that("the grey cloud is drawn beneath the significant points", {
  # Layer order is what keeps hits visible under heavy overplotting; on a real
  # ome the grey cloud is ~10k points and would bury them if drawn last.
  gg <- build_style_plot(make_style_fixture())
  colour_of <- vapply(gg$layers, function(ly) {
    d <- ly$data
    if (inherits(ly$geom, "GeomPoint") && is.data.frame(d) &&
        "point_color" %in% names(d) && nrow(d) > 0) d$point_color[1] else NA_character_
  }, character(1))
  idx <- function(hex) which(colour_of == hex)[1]
  expect_lt(idx("gray80"), idx("blue"))
  expect_lt(idx("gray80"), idx("red"))
})

## POI marker ##################################################################

test_that("POI points are gold with a black outline, larger than the other points", {
  gg <- build_style_plot(make_style_fixture(), label_proteins = "A")
  ly <- poi_layer(gg)
  expect_false(is.null(ly))
  # shape 21 is required, not cosmetic: only shapes 21-25 read `fill`, which is
  # what lets the gold interior and the black ring be two separate colours.
  expect_equal(ly$aes_params$shape, 21)
  # Literals, NOT .volcano_poi_fill / .volcano_poi_outline: asserting a constant
  # against itself is a tautology that passes whatever the constant is changed
  # to. Mutation-tested -- repainting the constant magenta must fail here.
  expect_equal(ly$aes_params$fill, "gold")
  expect_equal(ly$aes_params$colour, "black")
  # Bigger than the size-1 category points on purpose -- at size 1 the outline
  # ring swallows the fill and the POI is invisible against the cloud.
  expect_equal(ly$aes_params$size, 1.5)
  expect_gt(ly$aes_params$size, 1)
})

## Labels ######################################################################

test_that("features are labelled with boxed labels, not bare text", {
  gg <- build_style_plot(make_style_fixture(), label_mode = "significant")
  expect_false(is.null(repel_layer(gg)))
  # geom_text_repel would satisfy "a label exists" but not the white-box
  # treatment that keeps a red or blue label readable over the point cloud.
  expect_false(any(vapply(gg$layers,
                          function(ly) inherits(ly$geom, "GeomTextRepel"),
                          logical(1))))
})

test_that("every requested label is drawn rather than silently discarded", {
  # max.overlaps has a finite default (10). Under it, asking for N labels and
  # getting fewer is silent -- the plot renders, just without the labels the
  # user asked for.
  gg <- build_style_plot(make_style_fixture(), label_mode = "significant")
  expect_identical(repel_layer(gg)$geom_params$max.overlaps, Inf)
})

test_that("feature labels are plain, while the group annotations stay bold", {
  # Two different text layers with two different answers, which is exactly how
  # they get confused: the reference script's geom_label_repel parameters bold
  # the labels (its own are 1.7 mm on a small figure), the group annotations are
  # bold on purpose. Pinning both here keeps a change to one from drifting the
  # other.
  gg <- build_style_plot(make_style_fixture(), label_mode = "significant")
  expect_equal(repel_layer(gg)$aes_params$fontface, "plain")

  annots <- Filter(function(ly) inherits(ly$geom, "GeomText"), gg$layers)
  expect_gt(length(annots), 0)
  for (ly in annots) expect_equal(ly$aes_params$fontface, "bold")
})

test_that("a label takes the colour of the point it labels", {
  gg <- build_style_plot(make_style_fixture(), label_mode = "significant")
  d <- repel_layer(gg)$data
  expect_equal(d$label_col[d$id == "A"], "red")    # significant, up
  expect_equal(d$label_col[d$id == "B"], "blue")   # significant, down
})

test_that("POI labels use darkgoldenrod, not the gold of the POI point", {
  # geom_label_repel draws on a white fill and gold text on white is unreadable,
  # so the label colour is deliberately darker than the marker it belongs to.
  gg <- build_style_plot(make_style_fixture(),
                         label_proteins = "A", label_mode = "poi")
  d <- repel_layer(gg)$data
  # The literal value, for the same reason as the POI marker colours above.
  expect_equal(d$label_col[d$id == "A"], "#B8860B")
  # ... and the property that motivates it: the label must not be painted in
  # the marker's own gold, which is unreadable on the white label fill.
  expect_false(identical(.volcano_poi_label_color, .volcano_poi_fill))
})

test_that("a POI label overrides the direction label for the same feature", {
  # A is both significant-up (red) and a POI. It must be labelled once, in the
  # POI colour -- two overlapping labels on one point would be a layout bug.
  gg <- build_style_plot(make_style_fixture(), label_proteins = "A",
                         label_mode = c("significant", "poi"))
  d <- repel_layer(gg)$data
  expect_equal(sum(d$id == "A"), 1L)
  expect_equal(d$label_col[d$id == "A"], "#B8860B")
})
