# Phase 3 (P3.7) -- assorted untested pure helpers:
#   * color utils: color_mod / color_dist / color_range
#   * export-dimension utils: get_plot_export_dimensions / get_ggsave_params /
#     get_pdf_params
#   * heatmap helpers: extractGenes / getHMTable / dynamicHeightHM
#   * PELSA edge cases: pelsa_safe_name, pelsa_depth_summary(c(0,0,0)),
#     malformed-JSON pelsa_parse_uniprot_json, no-header pelsa_read_fasta stop
#
# All pure / deterministic; no Shiny, no network.

# ---------------------------------------------------------------------------
# color_mod / color_dist / color_range
# ---------------------------------------------------------------------------

test_that("color_mod brightens by the default modifier and clamps at 255", {
  # default modifier is 30 when nothing else is supplied
  expect_equal(color_mod("#000000"), "#1E1E1E")          # 0 + 30 = 30 = 0x1E
  # clamps at FF (255): 0xFF + 30 stays 0xFF
  expect_equal(color_mod("#FFFFFF"), "#FFFFFF")
})

test_that("color_mod honors per-channel modifiers and clamps at 0", {
  # only R specified -> G/B unmodified (modifier becomes 0)
  expect_equal(color_mod("#102030", mod_R = 16), "#202030")  # 0x10+16=0x20
  # negative push clamps at 0
  expect_equal(color_mod("#0A0A0A", mod_R = -100, mod_G = -100, mod_B = -100),
               "#000000")
})

test_that("color_mod accepts hex with or without leading hash", {
  expect_equal(color_mod("000000", modifier = 0), "#000000")
})

test_that("color_mod rejects invalid hex codes", {
  expect_error(color_mod("not-a-hex"), "Invalid hex")
  expect_error(color_mod("#12345"), "Invalid hex")   # 5 digits
})

test_that("color_dist returns the per-channel signed distance", {
  d <- color_dist("#000000", "#0A1400")  # R:0->10, G:0->20, B:0->0
  expect_equal(as.numeric(d), c(10, 20, 0))
})

test_that("color_range returns a palette of the requested length", {
  pal <- color_range("#000000", "#FFFFFF", 5)
  expect_length(pal, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
  # endpoints anchor the ramp
  expect_equal(toupper(pal[1]), "#000000")
})

test_that("color_range with <= 1 color returns the start color", {
  expect_equal(color_range("#123456", "#abcdef", 1), "#123456")
})

# ---------------------------------------------------------------------------
# export-dimension utilities (read defaults from setupDefaults.yaml)
# ---------------------------------------------------------------------------

test_that("get_plot_export_dimensions returns width/height/units for default", {
  d <- get_plot_export_dimensions("default")
  expect_named(d, c("width", "height", "units"))
  expect_true(is.numeric(d$width) && d$width > 0)
  expect_true(is.numeric(d$height) && d$height > 0)
  expect_true(is.character(d$units) && nzchar(d$units))
})

test_that("get_plot_export_dimensions uses the multiome_heatmap profile", {
  dflt <- get_plot_export_dimensions("default")
  mh   <- get_plot_export_dimensions("multiome_heatmap")
  expect_named(mh, c("width", "height", "units"))
  # the two profiles are distinct value sets (different yaml keys)
  expect_false(identical(c(dflt$width, dflt$height), c(mh$width, mh$height)))
})

test_that("get_ggsave_params and get_pdf_params mirror the dimension profile", {
  d  <- get_plot_export_dimensions("default")
  gg <- get_ggsave_params("default")
  pf <- get_pdf_params("default")
  expect_equal(gg[c("width", "height", "units")], d[c("width", "height", "units")])
  expect_equal(pf[c("width", "height", "units")], d[c("width", "height", "units")])
})

# ---------------------------------------------------------------------------
# heatmap helpers
# ---------------------------------------------------------------------------

test_that("dynamicHeightHM is the documented linear height in pixels", {
  # 0.3*(n+12)+3 inch, * 48 px/inch
  expect_equal(dynamicHeightHM(0), (0.3 * 12 + 3) * 48)
  expect_equal(dynamicHeightHM(10), (0.3 * 22 + 3) * 48)
  expect_gt(dynamicHeightHM(100), dynamicHeightHM(10))  # monotone increasing
})

test_that("extractGenes splits on comma/space/semicolon and uniquifies", {
  tbl <- data.frame(geneSymbol = c("A", "B", "C"), stringsAsFactors = FALSE)
  # extractGenes names its returned vector via sapply(); compare unnamed values.
  expect_equal(unname(extractGenes("A,B,A", tbl, GENEMAX = 20)$genes.vec),
               c("A", "B"))                 # comma + de-dup
  expect_equal(unname(extractGenes("A B C", tbl, GENEMAX = 20)$genes.vec),
               c("A", "B", "C"))            # space delimiter
  expect_equal(unname(extractGenes("A;B", tbl, GENEMAX = 20)$genes.vec),
               c("A", "B"))                 # semicolon delimiter
})

test_that("extractGenes reports genes not present in the table", {
  tbl <- data.frame(geneSymbol = c("A", "B"), stringsAsFactors = FALSE)
  expect_warning(res <- extractGenes("A,Z", tbl, GENEMAX = 20), "not found")
  expect_equal(unname(res$genes.vec), "A")
  expect_equal(unname(res$genes.notInTable), "Z")
})

test_that("extractGenes caps at GENEMAX with a warning", {
  tbl <- data.frame(geneSymbol = paste0("G", 1:30), stringsAsFactors = FALSE)
  genes <- paste(paste0("G", 1:30), collapse = ",")
  expect_warning(res <- extractGenes(genes, tbl, GENEMAX = 20), "more than 20")
  expect_length(res$genes.vec, 20L)
})

test_that("extractGenes returns NULL on NULL / empty input", {
  expect_null(extractGenes(NULL, data.frame(geneSymbol = "A"), 20))
  expect_null(extractGenes(character(0), data.frame(geneSymbol = "A"), 20))
})

test_that("getHMTable prepends geneSymbol/ome/row_label and keeps row ids", {
  vals <- data.frame(s1 = c(1, 2), s2 = c(3, 4))
  row.anno <- data.frame(
    geneSymbol = c("GA", "GB"),
    DataType = c("proteome", "phospho"),
    row.names = c("feat1", "feat2"),
    stringsAsFactors = FALSE
  )
  out <- getHMTable(vals, row.anno, params = list())
  expect_equal(names(out)[1:3], c("geneSymbol", "ome", "row_label"))
  expect_equal(rownames(out), c("feat1", "feat2"))
  expect_equal(as.character(out$ome), c("proteome", "phospho"))
  expect_true(all(c("s1", "s2") %in% names(out)))
})

test_that("getHMTable falls back to protigy.ome, then 'Unknown'", {
  vals <- data.frame(s1 = c(1, 2))
  # no DataType -> protigy.ome
  ra1 <- data.frame(geneSymbol = c("GA", "GB"),
                    protigy.ome = c("rna", "rna"),
                    row.names = c("f1", "f2"), stringsAsFactors = FALSE)
  expect_equal(as.character(getHMTable(vals, ra1, list())$ome), c("rna", "rna"))

  # neither column -> "Unknown"
  ra2 <- data.frame(geneSymbol = c("GA", "GB"),
                    row.names = c("f1", "f2"), stringsAsFactors = FALSE)
  expect_equal(as.character(getHMTable(vals, ra2, list())$ome),
               c("Unknown", "Unknown"))
})

# ---------------------------------------------------------------------------
# PELSA edge cases
# ---------------------------------------------------------------------------

test_that("pelsa_safe_name sanitizes unsafe characters and collapses runs", {
  expect_equal(pelsa_safe_name("A B/C:D"), "A_B_C_D")
  expect_equal(pelsa_safe_name("keep.dot-dash_underscore"),
               "keep.dot-dash_underscore")
  expect_equal(pelsa_safe_name("  weird  spaces  "), "weird_spaces")
  # leading/trailing separators are trimmed
  expect_equal(pelsa_safe_name("__edge__"), "edge")
})

test_that("pelsa_safe_name maps NA / empty to 'unknown'", {
  expect_equal(pelsa_safe_name(NA), "unknown")
  expect_equal(pelsa_safe_name(""), "unknown")
  expect_equal(pelsa_safe_name(c("ok", NA, "")), c("ok", "unknown", "unknown"))
})

test_that("pelsa_depth_summary(c(0,0,0)) guards a non-finite CV to NA", {
  # mean = 0 -> cv = sd/0 = NaN -> guarded to NA_real_
  res <- pelsa_depth_summary(c(0, 0, 0))
  expect_equal(res$mean_n, 0)
  expect_equal(res$median_n, 0)
  expect_true(is.na(res$cv_pct))
})

test_that("pelsa_parse_uniprot_json returns the empty frame for malformed input", {
  empty <- pelsa_empty_feature_frame()
  expect_equal(pelsa_parse_uniprot_json(NULL), empty)
  expect_equal(pelsa_parse_uniprot_json("not a list"), empty)
  expect_equal(pelsa_parse_uniprot_json(list()), empty)
  # a well-formed entry with NO features also yields the empty frame
  expect_equal(
    pelsa_parse_uniprot_json(list(primaryAccession = "P1", features = list())),
    empty
  )
})

test_that("pelsa_read_fasta stops when no header line is present", {
  f <- tempfile(fileext = ".fasta")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("ACDEFGHIK", "LMNPQRST"), f)   # sequence-only, no '>' header
  expect_error(pelsa_read_fasta(f), "no FASTA header")
})

test_that("pelsa_explode_accessions returns a 0-row frame with its added columns", {
  empty <- data.frame(
    PG.ProteinAccessions = character(0),
    PG.Genes = character(0),
    PEP.PeptidePosition = character(0),
    stringsAsFactors = FALSE
  )
  out <- pelsa_explode_accessions(empty)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_true(all(c("accession", "gene", "pep_position_token") %in% names(out)))
})

test_that("pelsa_thin_background passes through a 0-row volcano frame", {
  empty <- data.frame(
    Significant = logical(0), logFC = numeric(0),
    logP = numeric(0), is_marker = logical(0),
    stringsAsFactors = FALSE
  )
  out <- pelsa_thin_background(empty)
  expect_equal(out$n_total, 0L)
  expect_equal(out$n_shown, 0L)
  expect_equal(out$n_thinnable, 0L)
  expect_equal(nrow(out$df), 0L)
})
