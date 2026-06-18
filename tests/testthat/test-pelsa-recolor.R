library(testthat)

# Minimal volcano-df-shaped frame: 2 proteins, one a marker. Columns the recolor
# reads: id, winning_accession, is_marker, sig_color, feature_color.
.mk_df <- function() {
  data.frame(
    id                = c("PEPA1", "PEPA2", "PEPB1", "PEPMK"),
    winning_accession = c("ACCA", "ACCA", "ACCB", "ACCMK"),
    is_marker         = c(FALSE, FALSE, FALSE, TRUE),
    sig_color         = c("#1f4e9c", "darkred", "gray70", "gray70"),
    feature_color     = c("#111111", "#222222", "#333333", "#444444"),
    stringsAsFactors  = FALSE
  )
}

test_that("highlight_mask: selected peptide + same-protein + find, uniform", {
  df <- .mk_df()
  # selection on ACCA peptide PEPA1 -> PEPA1 + sibling PEPA2 highlighted.
  sel <- list(accession = "ACCA", peptide_seq = "PEPA1")
  m <- pelsa_volcano_highlight_mask(df, selection = sel, find_mask = NULL)
  expect_equal(which(m), c(1L, 2L))
  # NULL selection + NULL find -> nothing.
  expect_false(any(pelsa_volcano_highlight_mask(df, NULL, NULL)))
  # find mask alone (ACCB) -> row 3.
  fm <- df$winning_accession == "ACCB"
  expect_equal(which(pelsa_volcano_highlight_mask(df, NULL, fm)), 3L)
  # selection with NA peptide_seq -> all same-accession rows (accession only).
  sel2 <- list(accession = "ACCA", peptide_seq = NA_character_)
  expect_equal(which(pelsa_volcano_highlight_mask(df, sel2, NULL)), c(1L, 2L))
})

test_that("recolor: NULL selection + no find -> base fills, no rings", {
  df <- .mk_df()
  out <- pelsa_volcano_recolor(df, selection = NULL, find_mask = NULL,
                               color_mode = "significance")
  split <- pelsa_volcano_marker_split(df)
  expect_length(out$background$color, nrow(split$background))
  expect_length(out$markers$color,   nrow(split$markers))
  expect_setequal(out$background$color, c("#1f4e9c", "darkred", "gray70"))
  expect_true(all(out$background$line.width == 0))
  expect_true(all(out$markers$line.width == 0))
})

test_that("recolor: click selection -> gold fill + dark ring on the clicked peptide", {
  df <- .mk_df()
  sel <- list(origin = "click", accession = "ACCA", peptide_seq = "PEPA1")
  out <- pelsa_volcano_recolor(df, sel, NULL, "significance")
  split <- pelsa_volcano_marker_split(df)
  bg_id <- as.character(split$background$id)
  expect_equal(out$background$color[bg_id == "PEPA1"], .PELSA_GOLD)
  expect_equal(out$background$line.color[bg_id == "PEPA1"], .PELSA_SEL_DARK_RING)
  expect_equal(out$background$color[bg_id == "PEPA2"], "darkred")
  expect_equal(out$background$line.color[bg_id == "PEPA2"], .PELSA_GOLD)
  expect_gt(out$background$line.width[bg_id == "PEPA2"], 0)
  expect_equal(out$background$color[bg_id == "PEPB1"], "gray70")
  expect_equal(out$background$line.width[bg_id == "PEPB1"], 0)
})

test_that("recolor: a clicked MARKER goes gold in the marker trace (gold wins)", {
  df <- .mk_df()
  sel <- list(origin = "click", accession = "ACCMK", peptide_seq = "PEPMK")
  out <- pelsa_volcano_recolor(df, sel, NULL, "significance")
  expect_equal(out$markers$color, .PELSA_GOLD)
})

test_that("recolor: multi-find mask -> uniform gold fill, no dark ring", {
  df <- .mk_df()
  mask <- df$winning_accession == "ACCA"
  out <- pelsa_volcano_recolor(df, selection = NULL, find_mask = mask,
                               color_mode = "significance")
  split <- pelsa_volcano_marker_split(df)
  bg_id <- as.character(split$background$id)
  expect_equal(out$background$color[bg_id %in% c("PEPA1", "PEPA2")],
               c(.PELSA_GOLD, .PELSA_GOLD))
  expect_true(all(out$background$line.color[bg_id %in% c("PEPA1","PEPA2")]
                  != .PELSA_SEL_DARK_RING))
})

test_that("recolor: NA peptide_seq -> all same-accession rows get gold ring, none gold fill", {
  df <- .mk_df()
  sel <- list(origin = "click", accession = "ACCA", peptide_seq = NA_character_)
  out <- pelsa_volcano_recolor(df, sel, NULL, "significance")
  split <- pelsa_volcano_marker_split(df)
  bg_id <- as.character(split$background$id)
  expect_equal(out$background$line.color[bg_id %in% c("PEPA1","PEPA2")],
               c(.PELSA_GOLD, .PELSA_GOLD))
  expect_false(.PELSA_GOLD %in% out$background$color)  # no gold FILL when no peptide id
})

test_that("recolor: feature color mode uses feature_color as the base", {
  df <- .mk_df()
  out <- pelsa_volcano_recolor(df, NULL, NULL, "feature")
  expect_true("#111111" %in% out$background$color)
})

test_that("trace_index: finds the meta-stamped bg/marker traces", {
  # build_plot needs a full volcano-df shape; extend the minimal frame.
  df <- data.frame(
    id = c("PEPA1","PEPA2","PEPB1","PEPMK"),
    winning_accession = c("ACCA","ACCA","ACCB","ACCMK"),
    PG.ProteinAccessions = c("ACCA","ACCA","ACCB","ACCMK"),
    winning_gene = c("GA","GA","GB","GM"), PG.Genes = c("GA","GA","GB","GM"),
    is_marker = c(FALSE,FALSE,FALSE,TRUE),
    sig_color = c("#1f4e9c","darkred","gray70","gray70"),
    feature_color = c("#111","#222","#333","#444"),
    logFC = c(-1,1,2,0.5), logP = c(1,2,3,1.5),
    adj.P.Val = c(0.2,0.01,0.001,0.3), P.Value = c(0.1,0.005,0.0005,0.2),
    Significant = c(FALSE,TRUE,TRUE,FALSE), feature_class_primary = "none",
    pep_start = c(1L,5L,2L,9L), pep_end = c(4L,9L,8L,15L),
    label = c("GA_aa1","GA_aa5","GB_aa2","GM_aa9"),
    stringsAsFactors = FALSE, check.names = FALSE)
  p <- pelsa_volcano_build_plot(df, full_df = df, color_mode = "significance",
                                label_mode = "none", source_id = "s")
  # Resolve on the RAW build object AND on plotly_build(p) - the production path
  # (apply_highlight) wraps in plotly_build, so the meta tag must survive it.
  idx <- .pelsa_volcano_trace_index(p)
  expect_equal(idx$background, 0L)
  expect_equal(idx$markers, 1L)
  idx_built <- .pelsa_volcano_trace_index(plotly::plotly_build(p))
  expect_equal(idx_built$background, 0L)
  expect_equal(idx_built$markers, 1L)
})

test_that("recolor find_mask: duplicate ids across protein groups stay row-aligned", {
  # Two rows share the stripped sequence "DUP" but different winning_accession.
  # A find on ACCA must gold ONLY the ACCA row, not the ACCB row that shares id.
  df <- data.frame(
    id                = c("DUP", "DUP", "PEPB1"),
    winning_accession = c("ACCA", "ACCB", "ACCB"),
    is_marker         = c(FALSE, FALSE, FALSE),
    sig_color         = c("gray70", "gray70", "gray70"),
    feature_color     = c("#111", "#222", "#333"),
    stringsAsFactors  = FALSE)
  mask <- df$winning_accession == "ACCA"          # only row 1
  out <- pelsa_volcano_recolor(df, selection = NULL, find_mask = mask,
                               color_mode = "significance")
  split <- pelsa_volcano_marker_split(df)
  # background row order == df row order here (no markers): row1 gold, row2 not.
  expect_equal(out$background$color[1], .PELSA_GOLD)
  expect_equal(out$background$color[2], "gray70")
})

# ---- gold OVERLAY trace (Stage B: proxy addTraces highlight) ----------------

# Full volcano-df shape the gold trace + hover read (logFC/logP/pep_*/gene/acc).
.mk_full_df <- function() {
  data.frame(
    id                   = c("PEPA1", "PEPA2", "PEPB1", "PEPMK"),
    winning_accession    = c("ACCA", "ACCA", "ACCB", "ACCMK"),
    PG.ProteinAccessions = c("ACCA", "ACCA", "ACCB", "ACCMK"),
    winning_gene         = c("GA", "GA", "GB", "GM"),
    PG.Genes             = c("GA", "GA", "GB", "GM"),
    is_marker            = c(FALSE, FALSE, FALSE, TRUE),
    sig_color            = c("#1f4e9c", "darkred", "gray70", "gray70"),
    feature_color        = c("#111", "#222", "#333", "#444"),
    logFC                = c(-1, 1, 2, 0.5),
    logP                 = c(1, 2, 3, 1.5),
    adj.P.Val            = c(0.2, 0.01, 0.001, 0.3),
    P.Value              = c(0.1, 0.005, 0.0005, 0.2),
    pep_start            = c(1L, 5L, 2L, 9L),
    pep_end              = c(4L, 9L, 8L, 15L),
    label                = c("GA_aa1", "GA_aa5", "GB_aa2", "GM_aa9"),
    stringsAsFactors     = FALSE, check.names = FALSE)
}

test_that("gold_trace: NULL when nothing is highlighted", {
  df <- .mk_full_df()
  expect_null(pelsa_volcano_gold_trace(df, selection = NULL, find_mask = NULL))
  expect_null(pelsa_volcano_gold_trace(df[0, , drop = FALSE],
                                       selection = list(accession = "ACCA")))
  expect_null(pelsa_volcano_gold_trace("not a df"))
})

test_that("gold_trace: selection -> gold scattergl trace over the right points", {
  df <- .mk_full_df()
  sel <- list(accession = "ACCA", peptide_seq = "PEPA1")  # PEPA1 + sibling PEPA2
  tr <- pelsa_volcano_gold_trace(df, selection = sel, find_mask = NULL)
  expect_false(is.null(tr))
  expect_equal(tr$type, "scattergl")
  expect_equal(tr$mode, "markers")
  expect_identical(tr$meta, "pelsa_gold")
  expect_identical(tr$marker$color, .PELSA_GOLD)
  expect_identical(tr$marker$line$color, .PELSA_VOLCANO_MARKER_EDGE)
  # Two highlighted points (PEPA1 + PEPA2), at their (logFC, logP). x/y/text are
  # as.list()-wrapped so even a single point serializes to a JSON array (the
  # proxy auto_unbox scalar-collapse bug); unlist before the value checks.
  expect_equal(unlist(tr$x), c(-1, 1))
  expect_equal(unlist(tr$y), c(1, 2))
  # 6-line hover, one per highlighted point.
  expect_length(tr$text, 2L)
  txt <- unlist(tr$text)
  expect_true(all(grepl("Peptide: ", txt, fixed = TRUE)))
  expect_equal(lengths(regmatches(txt, gregexpr("<br>", txt))),
               c(5L, 5L))  # 6 lines => 5 <br> separators
})

test_that("gold_trace: find_mask alone highlights the matched rows", {
  df <- .mk_full_df()
  fm <- df$winning_accession == "ACCB"   # row 3 only
  tr <- pelsa_volcano_gold_trace(df, selection = NULL, find_mask = fm)
  expect_false(is.null(tr))
  # Single matched row: as.list keeps x/y as a length-1 list (-> JSON array).
  expect_equal(unlist(tr$x), 2)     # PEPB1 logFC
  expect_equal(unlist(tr$y), 3)     # PEPB1 logP
  expect_length(tr$text, 1L)
  # Regression guard: a length-1 coord must serialize as an ARRAY, not a scalar.
  expect_equal(as.character(jsonlite::toJSON(tr$x, auto_unbox = TRUE)), "[2]")
})

test_that("gold_trace size matches the build's gold/marker px (7)", {
  df <- .mk_full_df()
  tr <- pelsa_volcano_gold_trace(df, selection = list(accession = "ACCA"))
  expect_equal(tr$marker$size, 7)
})

test_that("volcano_tip: empty in -> empty out; 6 lines per row otherwise", {
  df <- .mk_full_df()
  expect_length(pelsa_volcano_tip(df[0, , drop = FALSE]), 0L)
  tips <- pelsa_volcano_tip(df)
  expect_length(tips, nrow(df))
  expect_true(grepl("Accession: ACCA", tips[1], fixed = TRUE))
  expect_true(grepl("logFC: -1.00", tips[1], fixed = TRUE))
})
