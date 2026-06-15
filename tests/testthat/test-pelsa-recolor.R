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
