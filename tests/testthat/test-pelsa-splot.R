################################################################################
# Tests for the PELSA Summary intensity-rank (S-plot) panel.
# PURE helpers (no Shiny) + light testServer. No network.
################################################################################

library(testthat)

test_that("S-plot constants have the expected fixed values", {
  expect_identical(.PELSA_TRYPSIN_ACCESSIONS, c("Q29463", "P00760", "P00761"))
  expect_identical(.PELSA_SPLOT_TOP_N, 3L)
  expect_match(.PELSA_SPLOT_TRYPSIN_COLOR, "^#[0-9A-Fa-f]{6}$")
  expect_identical(.PELSA_SPLOT_SUBDIR, "intensity_rank")
})

test_that("axis title reflects log base and normalization, forcing log2 for None", {
  expect_equal(
    pelsa_splot_axis_title(list(log_transformation = "log2",
                                data_normalization = "Median (non-zero)")),
    "log2(intensity), Median (non-zero) normalized")
  expect_equal(
    pelsa_splot_axis_title(list(log_transformation = "log10",
                                data_normalization = "Quantile")),
    "log10(intensity), Quantile normalized")
  # None log -> forced log2; None normalization -> clause dropped
  expect_equal(
    pelsa_splot_axis_title(list(log_transformation = "None",
                                data_normalization = "None")),
    "log2(intensity)")
  # Missing fields are tolerated
  expect_equal(pelsa_splot_axis_title(list()), "log2(intensity)")
})

test_that("display_intensity: already-log as-is; None forces log2 and drops non-positive", {
  expect_equal(pelsa_splot_display_intensity(c(10, 8), "log2"), c(10, 8))
  expect_equal(pelsa_splot_display_intensity(c(4, 2), "None"), c(2, 1))   # log2
  v <- pelsa_splot_display_intensity(c(8, 0, -3, NA), "None")
  expect_equal(v[1], 3)                       # log2(8)
  expect_true(all(is.na(v[2:4])))             # 0, negative, NA -> NA
})

test_that("rank_frame drops NA, ranks desc, keeps row_id = matrix row index", {
  mat <- matrix(c(5, NA, 9, 1), nrow = 4,
                dimnames = list(c("p1","p2","p3","p4"), "S1"))
  pf <- data.frame(
    PEP.StrippedSequence = c("AAA","BBB","CCC","DDD"),
    PG.ProteinAccessions = c("P1","P2","P3","P4"),
    PG.Genes             = c("GA","GB","GC","GD"),
    stringsAsFactors = FALSE)
  rf <- pelsa_splot_rank_frame(mat, "S1", pf, "log2")
  expect_equal(nrow(rf), 3L)                      # p2 (NA) dropped
  expect_equal(rf$rank, 1:3)
  expect_equal(rf$row_id, c(3L, 1L, 4L))          # 9, 5, 1 -> rows 3,1,4
  expect_equal(rf$display_intensity, c(9, 5, 1))
  expect_equal(rf$sequence[1], "CCC")
})

test_that("rank_frame returns an empty typed frame when no finite values", {
  mat <- matrix(c(NA_real_, NA_real_), nrow = 2, dimnames = list(NULL, "S1"))
  pf <- data.frame(PEP.StrippedSequence = c("A","B"),
                   PG.ProteinAccessions = c("P","Q"),
                   PG.Genes = c("g","h"), stringsAsFactors = FALSE)
  rf <- pelsa_splot_rank_frame(mat, "S1", pf, "log2")
  expect_equal(nrow(rf), 0L)
  expect_true(all(c("row_id","sequence","accessions","genes",
                    "display_intensity","rank") %in% names(rf)))
})

test_that("marker_topn caps at N per accession and is marker-scoped", {
  rf <- data.frame(
    row_id = 1:5, sequence = paste0("S", 1:5),
    accessions = "x", genes = "x",
    display_intensity = c(50, 40, 30, 20, 10), rank = 1:5,
    stringsAsFactors = FALSE)
  matched <- data.frame(
    .row_id   = c(1L, 2L, 3L, 4L, 5L),
    accession = c("M1","M1","M1","M1","M2"),
    gene      = c("GA","GA","GA","GA","GB"),
    pep_start = c(10L, 20L, 30L, 40L, 5L),
    PEP.StrippedSequence = paste0("S", 1:5),
    check.names = FALSE, stringsAsFactors = FALSE)
  res <- pelsa_splot_marker_topn(matched, c("M1","M2"), rf, n = 3L)
  expect_setequal(res$highlight, 1:5)             # all 5 highlighted
  expect_setequal(res$labels$row_id, c(1L, 2L, 3L, 5L))  # M1 top3 (1,2,3) + M2 (5)
  expect_equal(res$labels$label[res$labels$row_id == 1L], "GA_aa10")
  expect_equal(res$labels$label[res$labels$row_id == 5L], "GB_aa5")
})

test_that("marker_topn shares a peptide across markers with a ;-joined label", {
  rf <- data.frame(row_id = 1:2, sequence = c("S1","S2"),
                   accessions = "x", genes = "x",
                   display_intensity = c(9, 8), rank = 1:2,
                   stringsAsFactors = FALSE)
  matched <- data.frame(
    .row_id   = c(1L, 1L, 2L),
    accession = c("M1","M2","M2"),
    gene      = c("GA","GB","GB"),
    pep_start = c(11L, 22L, 33L),
    PEP.StrippedSequence = c("S1","S1","S2"),
    check.names = FALSE, stringsAsFactors = FALSE)
  res <- pelsa_splot_marker_topn(matched, c("M1","M2"), rf, n = 3L)
  expect_equal(res$labels$label[res$labels$row_id == 1L], "GA_aa11;GB_aa22")
})

test_that("marker_topn drops NA-in-sample peptides and falls back to accession label", {
  rf <- data.frame(row_id = 1L, sequence = "S1", accessions = "x", genes = "x",
                   display_intensity = 5, rank = 1L, stringsAsFactors = FALSE)
  matched <- data.frame(
    .row_id = c(1L, 2L), accession = c("M1","M1"), gene = c("", ""),
    pep_start = c(7L, 8L),
    PEP.StrippedSequence = c("S1","S2"),
    check.names = FALSE, stringsAsFactors = FALSE)
  res <- pelsa_splot_marker_topn(matched, "M1", rf, n = 3L)
  expect_equal(res$highlight, 1L)                 # row 2 absent from rf (NA)
  expect_equal(res$labels$label, "M1_aa7")        # blank gene -> accession
})

test_that("marker_topn returns empty on no markers / no matches", {
  rf <- data.frame(row_id = 1L, sequence = "S1", accessions = "x", genes = "x",
                   display_intensity = 5, rank = 1L, stringsAsFactors = FALSE)
  empty_m <- pelsa_splot_marker_topn(data.frame(), "M1", rf)
  expect_length(empty_m$highlight, 0L)
  expect_equal(nrow(empty_m$labels), 0L)
})

test_that("tooltip lists accessions, bolds matched keys, caps with always-show", {
  rf <- data.frame(
    row_id = 1L, rank = 1L, display_intensity = 12.345,
    sequence = "PEPTIDE",
    accessions = "P1;Q2;R3",
    genes = "GA;GB;",
    stringsAsFactors = FALSE)
  tip <- pelsa_splot_tooltip(rf, bold_keys = "q2", cap = 8L)
  expect_match(tip, "Rank: #1")
  expect_match(tip, "Intensity: 12.35")            # 2 decimals
  expect_match(tip, "Sequence: PEPTIDE")
  expect_match(tip, "P1 \\(GA\\)")
  expect_match(tip, "<b>Q2 \\(GB\\)</b>")          # bolded
  expect_match(tip, "R3", fixed = FALSE)           # blank gene -> bare accession
})

test_that("tooltip cap keeps bolded accession beyond the cap", {
  accs <- paste0("A", 1:10)
  rf <- data.frame(row_id = 1L, rank = 1L, display_intensity = 1,
                   sequence = "S",
                   accessions = paste(accs, collapse = ";"),
                   genes = paste(rep("g", 10), collapse = ";"),
                   stringsAsFactors = FALSE)
  tip <- pelsa_splot_tooltip(rf, bold_keys = "a10", cap = 3L)
  expect_match(tip, "<b>A10 \\(g\\)</b>")          # bold shown despite cap 3
  expect_match(tip, "\\(\\+[0-9]+ more\\)")
})

test_that("tooltip handles unmapped peptides", {
  rf <- data.frame(row_id = 1L, rank = 2L, display_intensity = 3,
                   sequence = "S", accessions = "", genes = "",
                   stringsAsFactors = FALSE)
  expect_match(pelsa_splot_tooltip(rf, character(0)), "Maps to: \\(unmapped\\)")
})

test_that("prepare bundles background, marker overlay, labels, and title", {
  mat <- matrix(c(50, 40, 30, 20, 10), nrow = 5,
                dimnames = list(paste0("p", 1:5), "S1"))
  pf <- data.frame(
    PEP.StrippedSequence = paste0("S", 1:5),
    PG.ProteinAccessions = c("M1","M1","M1","M2","P9"),
    PG.Genes             = c("GA","GA","GA","GB","G9"),
    stringsAsFactors = FALSE)
  matched <- data.frame(
    .row_id = 1:4, accession = c("M1","M1","M1","M2"),
    gene = c("GA","GA","GA","GB"), pep_start = c(10L,20L,30L,5L),
    PEP.StrippedSequence = paste0("S", 1:4),
    check.names = FALSE, stringsAsFactors = FALSE)
  prep <- pelsa_splot_prepare(
    mat, "S1", pf, matched,
    selected_markers = c("M1","M2"), trypsin_accs = .PELSA_TRYPSIN_ACCESSIONS,
    label_trypsin = FALSE,
    params = list(log_transformation = "log2",
                  data_normalization = "Median (non-zero)"))
  expect_equal(nrow(prep$background), 5L)
  expect_setequal(prep$marker_pts$rank, 1:4)            # M1+M2 peptides
  expect_equal(nrow(prep$trypsin_pts), 0L)
  expect_setequal(prep$marker_labels$label,
                  c("GA_aa10","GA_aa20","GA_aa30","GB_aa5"))
  expect_false(prep$show_trypsin)
  expect_equal(prep$y_title, "log2(intensity), Median (non-zero) normalized")
  # background hovertext bolds the selected markers
  expect_true(any(grepl("<b>M1", prep$background$hovertext)))
})

test_that("prepare yields an empty-but-typed bundle when no finite peptides", {
  mat <- matrix(NA_real_, nrow = 2, dimnames = list(NULL, "S1"))
  pf <- data.frame(PEP.StrippedSequence = c("A","B"),
                   PG.ProteinAccessions = c("M1","M2"),
                   PG.Genes = c("a","b"), stringsAsFactors = FALSE)
  prep <- pelsa_splot_prepare(mat, "S1", pf, data.frame(),
                              "M1", .PELSA_TRYPSIN_ACCESSIONS, FALSE,
                              list(log_transformation = "log2"))
  expect_equal(nrow(prep$background), 0L)
  expect_equal(nrow(prep$marker_labels), 0L)
})

test_that("build_plotly returns a plotly with 2 traces (trypsin off) and webgl switch", {
  prep <- list(
    background = data.frame(rank = 1:3, y = c(9,8,7),
                            hovertext = c("a","b","c"), stringsAsFactors = FALSE),
    marker_pts = data.frame(rank = 1L, y = 9, hovertext = "a",
                            stringsAsFactors = FALSE),
    trypsin_pts = data.frame(rank = integer(0), y = numeric(0),
                             hovertext = character(0)),
    marker_labels = data.frame(rank = 1L, y = 9, label = "GA_aa10",
                               stringsAsFactors = FALSE),
    trypsin_labels = data.frame(rank = integer(0), y = numeric(0),
                                label = character(0)),
    y_title = "log2(intensity)", show_trypsin = FALSE)
  p <- pelsa_splot_build_plotly(prep, use_webgl = TRUE)
  expect_s3_class(p, "plotly")
  b <- plotly::plotly_build(p)
  expect_length(b$x$data, 2L)                       # background + marker
  expect_equal(b$x$data[[1]]$type, "scattergl")
  expect_true(length(b$x$layout$annotations) >= 1L) # baked label
  p_svg <- pelsa_splot_build_plotly(prep, use_webgl = FALSE)
  expect_equal(plotly::plotly_build(p_svg)$x$data[[1]]$type, "scatter")
})

test_that("build_plotly adds a trypsin trace only when show_trypsin", {
  prep <- list(
    background = data.frame(rank = 1:2, y = c(9,8), hovertext = c("a","b")),
    marker_pts = data.frame(rank = integer(0), y = numeric(0), hovertext = character(0)),
    trypsin_pts = data.frame(rank = 2L, y = 8, hovertext = "b"),
    marker_labels = data.frame(rank = integer(0), y = numeric(0), label = character(0)),
    trypsin_labels = data.frame(rank = 2L, y = 8, label = "PRSS1_aa5"),
    y_title = "log2(intensity)", show_trypsin = TRUE)
  b <- plotly::plotly_build(pelsa_splot_build_plotly(prep))
  expect_length(b$x$data, 3L)                       # bg + marker + trypsin
})

test_that("build_ggplot returns a ggplot with point + label layers", {
  prep <- list(
    background = data.frame(rank = 1:3, y = c(9,8,7)),
    marker_pts = data.frame(rank = 1L, y = 9),
    trypsin_pts = data.frame(rank = integer(0), y = numeric(0)),
    marker_labels = data.frame(rank = 1L, y = 9, label = "GA_aa10",
                               stringsAsFactors = FALSE),
    trypsin_labels = data.frame(rank = integer(0), y = numeric(0),
                                label = character(0)),
    y_title = "log2(intensity)", show_trypsin = FALSE)
  g <- pelsa_splot_build_ggplot(prep)
  expect_s3_class(g, "ggplot")
  expect_gte(length(g$layers), 3L)                  # bg + marker + repel label
  expect_equal(g$labels$y, "log2(intensity)")
})

# ---- export styling -----------------------------------------------------------

test_that("pelsa_splot_build_ggplot applies the new export styling", {
  prep <- list(
    background = data.frame(rank = 1:3, y = c(9, 8, 7)),
    marker_pts = data.frame(rank = 1L, y = 9),
    trypsin_pts = data.frame(rank = integer(0), y = numeric(0)),
    marker_labels = data.frame(rank = 1L, y = 9, label = "GA_aa10",
                               stringsAsFactors = FALSE),
    trypsin_labels = data.frame(rank = integer(0), y = numeric(0),
                                label = character(0)),
    y_title = "log2(intensity)", show_trypsin = FALSE)
  g <- pelsa_splot_build_ggplot(prep)
  expect_equal(g$theme$plot.title$size, 11)
  expect_equal(g$theme$plot.subtitle$size, 11)
  expect_equal(g$theme$axis.title$size, 11)
  expect_equal(g$theme$axis.text$size, 7)
  expect_equal(g$theme$axis.text$colour, "black")
  expect_equal(g$theme$plot.title.position, "panel")  # ggplot2 default (unset -> falls back)
  expect_equal(g$theme$legend.background$colour, "black")
  expect_equal(g$theme$legend.background$fill, NA)
  repel_layer <- Filter(function(l) inherits(l$geom, "GeomLabelRepel"), g$layers)[[1]]
  expect_equal(repel_layer$aes_params$size, 3)
  expect_equal(repel_layer$aes_params$fontface, "bold")
})

test_that("export writes one PNG per sample with customization applied", {
  mat <- matrix(c(50,40,30, 45,35,25), nrow = 3,
                dimnames = list(c("p1","p2","p3"), c("S1","S2")))
  rdesc <- data.frame(
    PEP.StrippedSequence = c("AAA","BBB","CCC"),
    PG.ProteinAccessions = c("M1","M1","P9"),
    PG.Genes             = c("GA","GA","G9"),
    row.names = c("p1","p2","p3"), stringsAsFactors = FALSE)
  cdesc <- data.frame(condition = c("c","c"),
                      row.names = c("S1","S2"), stringsAsFactors = FALSE)
  g <- cmapR::GCT(mat = mat, rdesc = rdesc, cdesc = cdesc)
  matched <- data.frame(
    .row_id = 1:2, accession = c("M1","M1"), gene = c("GA","GA"),
    pep_start = c(10L, 20L), PEP.StrippedSequence = c("AAA","BBB"),
    check.names = FALSE, stringsAsFactors = FALSE)

  tmp <- file.path(tempdir(), paste0("splotexp_", as.integer(runif(1, 1, 1e6))))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  pelsa_splot_export_for(
    tmp, g, matched, marker_accs = c("M1","P9"),
    params = list(log_transformation = "log2",
                  data_normalization = "Median (non-zero)"),
    custom = list(selected_markers = "M1", label_trypsin = FALSE))

  out <- file.path(tmp, "02_qc", "intensity_rank")
  expect_true(file.exists(file.path(out, "intensity_rank_S1.png")))
  expect_true(file.exists(file.path(out, "intensity_rank_S2.png")))
  unlink(tmp, recursive = TRUE)
})

test_that("export is a no-op writing no PNGs when gct is NULL", {
  tmp <- file.path(tempdir(), paste0("splotnull_", as.integer(runif(1, 1, 1e6))))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  out <- pelsa_splot_export_for(tmp, NULL, data.frame(), character(0),
                                params = list(), custom = NULL)
  expect_true(dir.exists(out))
  expect_length(list.files(out, pattern = "\\.png$"), 0L)
  unlink(tmp, recursive = TRUE)
})

test_that("export defaults to all markers and returns the subdir when custom is NULL", {
  mat <- matrix(c(50,40,30, 45,35,25), nrow = 3,
                dimnames = list(c("p1","p2","p3"), c("S1","S2")))
  rdesc <- data.frame(
    PEP.StrippedSequence = c("AAA","BBB","CCC"),
    PG.ProteinAccessions = c("M1","M1","P9"),
    PG.Genes             = c("GA","GA","G9"),
    row.names = c("p1","p2","p3"), stringsAsFactors = FALSE)
  cdesc <- data.frame(condition = c("c","c"),
                      row.names = c("S1","S2"), stringsAsFactors = FALSE)
  g <- cmapR::GCT(mat = mat, rdesc = rdesc, cdesc = cdesc)
  matched <- data.frame(
    .row_id = 1:2, accession = c("M1","M1"), gene = c("GA","GA"),
    pep_start = c(10L, 20L), PEP.StrippedSequence = c("AAA","BBB"),
    check.names = FALSE, stringsAsFactors = FALSE)

  tmp <- file.path(tempdir(), paste0("splotcustom_", as.integer(runif(1, 1, 1e6))))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  out <- pelsa_splot_export_for(
    tmp, g, matched, marker_accs = c("M1","P9"),
    params = list(log_transformation = "log2",
                  data_normalization = "Median (non-zero)"),
    custom = NULL)
  expect_equal(out, file.path(tmp, "02_qc", "intensity_rank"))
  expect_true(file.exists(file.path(out, "intensity_rank_S1.png")))
  expect_true(file.exists(file.path(out, "intensity_rank_S2.png")))
  unlink(tmp, recursive = TRUE)
})

test_that("dashboard UI includes the intensity-rank panel outputs", {
  ui <- pelsa_section2_dashboard_ui(shiny::NS("PELSASection2Tab"), "ds1")
  html <- as.character(ui)
  expect_match(html, "PELSASection2Tab-splot_plot")
  expect_match(html, "PELSASection2Tab-splot_sample")
  expect_match(html, "PELSASection2Tab-splot_markers")
  expect_match(html, "PELSASection2Tab-splot_trypsin")
  expect_match(html, "Intensity rank")
})

test_that("server seeds per-ome S-plot state and renders the plot", {
  source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 12)
  rids <- paste0("pep", seq_len(nrow(syn$peptides)))
  mat <- as.matrix(syn$peptides[, syn$sample_cols]); rownames(mat) <- rids
  rdesc <- syn$peptides[, setdiff(colnames(syn$peptides), syn$sample_cols),
                        drop = FALSE]; rownames(rdesc) <- rids
  cdesc <- data.frame(condition = sub("_R[0-9]+$", "", syn$sample_cols),
                      row.names = syn$sample_cols, stringsAsFactors = FALSE)
  g <- cmapR::GCT(mat = mat, rdesc = rdesc, cdesc = cdesc)
  cache <- pelsa_run_analysis(
    gcts = list(ds1 = g), gcts_original = list(ds1 = g),
    setup_snapshot = list(datasets = "ds1", species = "human",
                          condition_col = list(ds1 = "condition")),
    fasta_map = syn$fasta, feat_df = data.frame(
      accession = "SHARED1", start = 1L, end = 50L,
      feature_class = "domain", stringsAsFactors = FALSE))
  marker_acc <- as.character(cache$ds1$matched$accession[1])

  GCTs_and_params <- shiny::reactiveVal(list(
    GCTs = list(ds1 = g),
    parameters = list(ds1 = list(log_transformation = "log2",
                                 data_normalization = "Median (non-zero)"))))
  globals <- shiny::reactiveValues(webgl_supported = TRUE)
  setup_state <- shiny::reactive(list(
    sample_order = list(ds1 = syn$sample_cols),
    marker_rows = list(ds1 = data.frame(accession = marker_acc, gene = "GA",
                                        stringsAsFactors = FALSE))))

  shiny::testServer(
    PELSASection2_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = shiny::reactiveVal(NULL),
                active_dataset = shiny::reactive("ds1"),
                pelsa_analysis = shiny::reactive(cache),
                pelsa_setup_state = setup_state),
    {
      session$flushReact()
      expect_false(is.null(splot_state[["ds1"]]))
      expect_true(marker_acc %in% splot_state[["ds1"]]$selected_markers)
      prep <- splot_prep()
      expect_gt(nrow(prep$background), 0L)
      expect_s3_class(pelsa_splot_build_plotly(prep, use_webgl()), "plotly")
      expect_false(is.null(output$splot_plot))        # render produced output

      # write-back: deselect-all arrives as a non-NULL empty value and updates
      # the store to character(0) WITHOUT deleting the field (the ignoreNULL=TRUE
      # contract the marker observer relies on).
      session$setInputs(splot_markers = character(0))
      expect_true("selected_markers" %in% names(splot_state[["ds1"]]))
      expect_length(splot_state[["ds1"]]$selected_markers, 0L)
      expect_gt(nrow(splot_prep()$background), 0L)   # plot still renders w/o markers

      # write-back: trypsin toggle persists into the per-ome store
      session$setInputs(splot_trypsin = TRUE)
      expect_true(isTRUE(splot_state[["ds1"]]$label_trypsin))

      # write-back: sample selection persists into the per-ome store
      session$setInputs(splot_sample = syn$sample_cols[[2]])
      expect_equal(splot_state[["ds1"]]$sample, syn$sample_cols[[2]])
    })
})

test_that("marker_topn ;-joins multiple positions of the same accession (repeat motif)", {
  rf <- data.frame(row_id = 1L, sequence = "S1", accessions = "x", genes = "x",
                   display_intensity = 9, rank = 1L, stringsAsFactors = FALSE)
  matched <- data.frame(
    .row_id = c(1L, 1L), accession = c("M1", "M1"), gene = c("GA", "GA"),
    pep_start = c(20L, 10L),
    PEP.StrippedSequence = c("S1", "S1"),
    check.names = FALSE, stringsAsFactors = FALSE)
  res <- pelsa_splot_marker_topn(matched, "M1", rf, n = 3L)
  expect_equal(res$highlight, 1L)
  expect_equal(res$labels$label, "GA_aa10;GA_aa20")
})

test_that("splot marker labels use protein-name fallback when gene missing", {
  matched <- data.frame(
    .row_id      = c(1L, 2L),
    PEP.StrippedSequence = c("PEPK", "TIDEK"),
    accession    = c("P1", "P2"),
    gene         = c("", NA),
    protein_name = c("NameA", "NameB"),
    pep_start    = c(10L, 20L),
    pep_occurrence_idx = c(1L, 1L),
    stringsAsFactors = FALSE
  )
  rank_frame <- data.frame(
    row_id            = c(1L, 2L),
    sequence          = c("PEPK", "TIDEK"),
    display_intensity = c(100, 90),
    stringsAsFactors  = FALSE
  )
  out <- pelsa_splot_marker_topn(matched, accessions = c("P1", "P2"),
                                 rank_frame, n = 3L)
  labs <- out$labels$label[order(out$labels$row_id)]
  expect_equal(labs, c("NameA_aa10", "NameB_aa20"))
})

test_that("S-plot centers its title and subtitle over the whole figure", {
  prep <- list(
    background = data.frame(rank = 1:5, y = c(5, 4, 3, 2, 1),
                            stringsAsFactors = FALSE),
    marker_pts = data.frame(rank = integer(0), y = numeric(0),
                            stringsAsFactors = FALSE),
    trypsin_pts = data.frame(rank = integer(0), y = numeric(0),
                             stringsAsFactors = FALSE),
    marker_labels = data.frame(rank = integer(0), y = numeric(0),
                               label = character(0), stringsAsFactors = FALSE),
    trypsin_labels = data.frame(rank = integer(0), y = numeric(0),
                                label = character(0), stringsAsFactors = FALSE),
    y_title = "log2(intensity)", show_trypsin = FALSE)
  g <- pelsa_splot_build_ggplot(prep, title = "Intensity rank (S-plot)",
                                subtitle = "sample_1")
  # plot.title.position is no longer set explicitly; ggplot2 falls back to
  # its own "panel" default (see Task 7: title/subtitle stay centered over
  # the panel via hjust, not over the whole figure via plot.title.position).
  expect_equal(g$theme$plot.title.position, "panel")
  expect_equal(g$theme$plot.title$hjust, 0.5)
  expect_equal(g$theme$plot.subtitle$hjust, 0.5)
})

test_that("S-plot repel labels dodge in both directions", {
  prep <- list(
    background = data.frame(rank = 1:50, y = rnorm(50)),
    marker_pts = data.frame(rank = c(3, 8), y = c(2, -2)),
    marker_labels = data.frame(rank = c(3, 8), y = c(2, -2),
                               label = c("A", "B")),
    trypsin_pts = data.frame(rank = integer(0), y = numeric(0)),
    trypsin_labels = data.frame(rank = integer(0), y = numeric(0),
                                label = character(0)),
    show_trypsin = FALSE, y_title = "y"
  )
  g <- pelsa_splot_build_ggplot(prep)
  repel <- Filter(function(l) inherits(l$geom, "GeomLabelRepel"), g$layers)
  dirs <- vapply(repel, function(l) l$geom_params$direction %||% "", character(1))
  expect_true(all(dirs == "both"))
})
