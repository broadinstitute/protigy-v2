################################################################################
# Tests: volcano plot UI controls
#
# Both tests use shiny::testServer() with injected mock data — no browser,
# no real statistics run, fully deterministic.
#
# Test 1 — sidebar controls
#   Verifies the new controls introduced for this feature are present in the
#   rendered sidebar HTML.
#
# Test 2 — POI list layout
#   Triggers the feature search, then inspects output$poi_list_ui HTML to
#   confirm the scroll container and Clear-all button are structured correctly.
################################################################################

library(testthat)

# ---------------------------------------------------------------------------
# Helpers shared by both tests
# ---------------------------------------------------------------------------

make_mock_stat_params <- function() {
  list(
    Proteome = list(
      test      = "Two-sample Moderated T-test",
      groups    = c("A", "B"),
      contrasts = "A / B",
      stat      = "adj.p.val",
      cutoff    = 0.05
    )
  )
}

make_mock_stat_results <- function() {
  list(
    Proteome = data.frame(
      id                   = c("p1", "p2"),
      geneSymbol           = c("G1", "G2"),
      logFC.A_over_B       = c(1.0, -0.5),
      P.Value.A_over_B     = c(0.001, 0.6),
      adj.P.Val.A_over_B   = c(0.01, 0.9),
      Log.P.Value.A_over_B = c(3.0, 0.22),
      significant.A_over_B = c(TRUE, FALSE),
      stringsAsFactors     = FALSE
    )
  )
}

make_mock_gct <- function() {
  mat   <- matrix(c(1, 2, 3, 4), nrow = 2,
                  dimnames = list(c("p1", "p2"), c("s1", "s2")))
  cdesc <- data.frame(group = c("A", "B"), row.names = c("s1", "s2"),
                      stringsAsFactors = FALSE)
  rdesc <- data.frame(geneSymbol = c("G1", "G2"), row.names = c("p1", "p2"),
                      stringsAsFactors = FALSE)
  new("GCT", mat = mat, cdesc = cdesc, rdesc = rdesc,
      rid = c("p1", "p2"), cid = c("s1", "s2"))
}

# Shared testServer args used by both tests.
# poi_registry / top_n_registry / label_mode_registry are normally created by
# the parent module (statPlot_Server) as reactiveVal(list()) and passed down so
# state is shared across omes. statPlot_Ome_Server defaults them to NULL, but
# proteins_of_interest() and friends invoke them as reactives, which crashes
# when NULL. Provide empty reactiveVal()s here so the per-contrast state path
# resolves cleanly.
make_server_args <- function() {
  list(
    ome                       = "Proteome",
    GCT_processed             = shiny::reactive(make_mock_gct()),
    parameters                = shiny::reactive(list(annotation_column = "group")),
    default_annotation_column = shiny::reactive("group"),
    color_map                 = shiny::reactive(NULL),
    stat_params               = shiny::reactive(make_mock_stat_params()),
    stat_results              = shiny::reactive(make_mock_stat_results()),
    poi_registry              = shiny::reactiveVal(list()),
    top_n_registry            = shiny::reactiveVal(list()),
    label_mode_registry       = shiny::reactiveVal(list())
  )
}

# ---------------------------------------------------------------------------
# Test 1: sidebar HTML contains the new controls
# ---------------------------------------------------------------------------

test_that("volcano sidebar HTML contains new controls (testServer)", {
  shiny::testServer(statPlot_Ome_Server, args = make_server_args(), {
    # as.character() returns a 2-element vector (HTML + dependency info);
    # collapse to a single string so all expect_match calls work.
    # suppressWarnings: testServer has no browser, so plotly click-event
    # registration produces a benign warning we don't need to see.
    html <- suppressWarnings(
      paste(as.character(output$volcano_sidebar_contents), collapse = "\n")
    )
    expect_match(html, "Shorten long labels on plot", fixed = TRUE)
    expect_match(html, "Search Features:",            fixed = TRUE)
    expect_match(html, "Feature(s) of Interest:",     fixed = TRUE)
    expect_match(html, 'value="poi"',                 fixed = TRUE)
  })
})

# ---------------------------------------------------------------------------
# Test 2: POI list layout — scroll container + Clear-all placement
# ---------------------------------------------------------------------------

test_that("volcano POI list has scrollable container with Clear all outside it (testServer)", {
  shiny::testServer(statPlot_Ome_Server, args = make_server_args(), {
    # Trigger the feature search.  The observer requires stat_results()
    # (injected above), protein_search, search_metadata_col, and search_btn.
    # Setting all four together means req() passes on the first flush.
    # volcano_contrasts is needed because output$poi_list_ui resolves
    # proteins_of_interest() -> current_contrast_key() -> req(input$volcano_contrasts);
    # in a real session the sidebar's selectInput populates it, but testServer
    # only renders outputs we touch, so we must set it explicitly here.
    # suppressWarnings: same benign plotly event-registration warning as Test 1.
    suppressWarnings(session$setInputs(
      volcano_contrasts   = "A / B",
      protein_search      = "p1 p2",
      search_metadata_col = "id",
      search_btn          = 1
    ))

    html <- suppressWarnings(
      paste(as.character(output$poi_list_ui), collapse = "\n")
    )

    # Scroll container is present with the required style properties.
    expect_match(html, "max-height: 220px", fixed = TRUE)
    expect_match(html, "overflow-y: auto",  fixed = TRUE)

    # Clear-all button is present.
    expect_match(html, "clear_all_poi", fixed = TRUE)

    # Clear-all must sit OUTSIDE (after) the scroll container.
    # Strategy: from the scroll-div opening to the position of clear_all_poi,
    # count <div opens vs </div> closes.  If closes >= opens, the nesting has
    # returned to the level of the scroll div's parent, meaning the button is
    # a sibling (outside), not a child (inside).
    scroll_open <- regexpr("max-height: 220px", html, fixed = TRUE)[[1]]
    clear_pos   <- regexpr("clear_all_poi",     html, fixed = TRUE)[[1]]
    expect_true(scroll_open > 0 && clear_pos > scroll_open)

    between  <- substr(html, scroll_open, clear_pos - 1L)
    n_open   <- lengths(regmatches(between, gregexpr("<div",   between, fixed = TRUE)))
    n_close  <- lengths(regmatches(between, gregexpr("</div>", between, fixed = TRUE)))
    expect_true(
      n_close >= n_open,
      info = "clear_all_poi must appear outside (not nested inside) the scroll div"
    )
  })
})
