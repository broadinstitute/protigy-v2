# Phase 3 (P3.1) -- export EXECUTION path via the real downloadHandler.
#
# test-export-hygiene.R asserts against a hand-copied content-function body. This
# file drives the ACTUAL exportTabServer download handler through shiny::testServer,
# unzips the result, and asserts the data-leak + correctness invariants on the
# real output:
#   * per-ome `<ome>_parameters.yaml` exists and contains NO gct_file_path
#   * customization/color_scheme.yaml round-trips through the real exporter/importer
#   * folder layout is ome/tab/file
#   * multi_ome gets a folder but NO parameters.yaml
#
# It also covers the pure .pelsa_pack_lanes lane allocator used by the PELSA
# Woods static export.

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Drive a downloadHandler registered inside a testServer block.
#
# testServer's MockShinySession does not surface downloads via getOutput(); the
# handler lives in private$outs as "<proxyN>-<id>". We fetch its render func,
# evaluate it to get list(filename, content), then run content() into a temp file.
# ---------------------------------------------------------------------------
drive_download <- function(session, output_id, fileext = ".zip") {
  priv <- session$.__enclos_env__$private
  keys <- ls(priv$outs)
  key <- keys[endsWith(keys, paste0("-", output_id))]
  if (length(key) != 1L) {
    stop("could not locate a single download output for id '", output_id,
         "' (found: ", paste(keys, collapse = ", "), ")")
  }
  render_func <- priv$outs[[key]]$func
  res <- render_func(shinysession = session, name = key)
  out_file <- tempfile(fileext = fileext)
  res$content(out_file)
  out_file
}

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# Export-fn factory: writes a single named file into dir_name.
make_export_fn <- function(fname, content) {
  force(fname); force(content)
  function(dir_name) writeLines(content, file.path(dir_name, fname))
}

make_all_exports <- function() {
  list(
    omes = shiny::reactive(c("proteome", "phospho", "multi_ome")),
    exports = list(
      summary_exports = list(
        proteome = list(s1 = make_export_fn("summary1.txt", "p-sum")),
        phospho  = list(s1 = make_export_fn("summary1.txt", "ph-sum"))
      ),
      # a reactive tab (mirrors statSummary_exports etc.)
      statPlot_exports = shiny::reactive(list(
        proteome = list(v1 = make_export_fn("volcano1.txt", "p-volc")),
        multi_ome = list(mv = make_export_fn("mv.txt", "multi-volc"))
      ))
    )
  )
}

make_gcts_and_params <- function() {
  cdesc <- data.frame(group = c("A", "B"), row.names = c("s1", "s2"))
  rdesc <- data.frame(id = c("g1", "g2"), row.names = c("g1", "g2"))
  mat <- matrix(1:4, 2, 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
             rid = c("g1", "g2"), cid = c("s1", "s2"))
  list(
    GCTs = list(proteome = gct, phospho = gct),
    GCTs_merged = gct,
    parameters = list(
      # gct_file_path MUST be stripped from the exported YAML (path-leak guard).
      proteome = list(dataset_label = "proteome",
                      gct_file_path = "/secret/abs/path/proteome.gct",
                      normalization = "median"),
      phospho = list(dataset_label = "phospho",
                     gct_file_path = "/secret/abs/path/phospho.gct",
                     log_transform = TRUE)
    )
  )
}

make_globals_with_colors <- function() {
  shiny::reactiveValues(
    colors = list(
      proteome = list(
        group = list(is_discrete = TRUE,
                     vals = c("A", "B"),
                     colors = c("#112233", "#445566"))
      )
    )
  )
}

unzip_export <- function(zip_path) {
  ex <- tempfile("unzipped_"); dir.create(ex)
  utils::unzip(zip_path, exdir = ex)
  # the handler nests everything under the basename of the zip
  top <- list.dirs(ex, recursive = FALSE)
  if (length(top) == 1L) top else ex
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("P3.1: download handler produces a zip with ome/tab/file layout", {
  all_exports <- make_all_exports()
  gp <- shiny::reactiveVal(make_gcts_and_params())
  globals <- make_globals_with_colors()

  shiny::testServer(
    exportTabServer,
    args = list(all_exports = all_exports, GCTs_and_params = gp, globals = globals),
    {
      session$setInputs(
        omesForExport = c("proteome", "phospho", "multi_ome"),
        tabsForExport = c("summary_exports", "statPlot_exports")
      )
      zip_path <- drive_download(session, "download")
      expect_true(file.exists(zip_path))
      expect_gt(file.info(zip_path)$size, 0)

      root <- unzip_export(zip_path)

      # ome/tab/file layout for a plain tab
      expect_true(file.exists(file.path(root, "proteome", "summary_exports", "summary1.txt")))
      expect_true(file.exists(file.path(root, "phospho", "summary_exports", "summary1.txt")))
      # reactive tab + multi_ome ome
      expect_true(file.exists(file.path(root, "proteome", "statPlot_exports", "volcano1.txt")))
      expect_true(file.exists(file.path(root, "multi_ome", "statPlot_exports", "mv.txt")))
    }
  )
})

test_that("P3.1: per-ome parameters.yaml exists and strips gct_file_path", {
  all_exports <- make_all_exports()
  gp <- shiny::reactiveVal(make_gcts_and_params())
  globals <- make_globals_with_colors()

  shiny::testServer(
    exportTabServer,
    args = list(all_exports = all_exports, GCTs_and_params = gp, globals = globals),
    {
      session$setInputs(
        omesForExport = c("proteome", "phospho", "multi_ome"),
        tabsForExport = c("summary_exports")
      )
      zip_path <- drive_download(session, "download")
      root <- unzip_export(zip_path)

      for (ome in c("proteome", "phospho")) {
        yml_path <- file.path(root, ome, paste0(ome, "_parameters.yaml"))
        expect_true(file.exists(yml_path))
        params <- yaml::read_yaml(yml_path)
        expect_false("gct_file_path" %in% names(params))
        expect_equal(params$dataset_label, ome)
      }

      # multi_ome must NOT get a parameters.yaml (excluded via setdiff)
      expect_false(file.exists(file.path(root, "multi_ome", "multi_ome_parameters.yaml")))
    }
  )
})

test_that("P3.1: color_scheme.yaml round-trips through the real exporter", {
  all_exports <- make_all_exports()
  gp <- shiny::reactiveVal(make_gcts_and_params())
  globals <- make_globals_with_colors()

  shiny::testServer(
    exportTabServer,
    args = list(all_exports = all_exports, GCTs_and_params = gp, globals = globals),
    {
      session$setInputs(
        omesForExport = c("proteome"),
        tabsForExport = c("summary_exports")
      )
      zip_path <- drive_download(session, "download")
      root <- unzip_export(zip_path)

      color_yaml <- file.path(root, "customization", "color_scheme.yaml")
      expect_true(file.exists(color_yaml))

      # Re-import via the real importer and assert the color survives.
      imported <- import_colors_from_yaml_full(
        color_yaml,
        custom_colors = isolate(globals$colors)
      )
      expect_equal(
        imported$colors$proteome$group$colors,
        c("#112233", "#445566")
      )
    }
  )
})

test_that("P3.1: handler tolerates NULL colors (no customization write failure)", {
  all_exports <- make_all_exports()
  gp <- shiny::reactiveVal(make_gcts_and_params())
  globals <- shiny::reactiveValues(colors = NULL)

  shiny::testServer(
    exportTabServer,
    args = list(all_exports = all_exports, GCTs_and_params = gp, globals = globals),
    {
      session$setInputs(
        omesForExport = c("proteome"),
        tabsForExport = c("summary_exports")
      )
      zip_path <- drive_download(session, "download")
      root <- unzip_export(zip_path)
      # customization folder still created, but no color_scheme.yaml written
      expect_false(file.exists(file.path(root, "customization", "color_scheme.yaml")))
      # the actual export still landed
      expect_true(file.exists(file.path(root, "proteome", "summary_exports", "summary1.txt")))
    }
  )
})

# ---------------------------------------------------------------------------
# .pelsa_pack_lanes -- pure lane allocator used by the Woods static export
# ---------------------------------------------------------------------------

test_that("P3.1: .pelsa_pack_lanes returns integer(0) for empty input", {
  expect_identical(.pelsa_pack_lanes(integer(0), integer(0)), integer(0))
})

test_that("P3.1: .pelsa_pack_lanes packs non-overlapping intervals on one lane", {
  # [1,5] then [6,10] do not overlap -> both lane 1
  lanes <- .pelsa_pack_lanes(c(1, 6), c(5, 10))
  expect_equal(lanes, c(1L, 1L))
})

test_that("P3.1: .pelsa_pack_lanes pushes overlapping intervals to new lanes", {
  # [1,10] and [5,15] overlap -> lanes 1 and 2
  lanes <- .pelsa_pack_lanes(c(1, 5), c(10, 15))
  expect_equal(sort(unique(lanes)), c(1L, 2L))
  expect_equal(length(lanes), 2L)
})

test_that("P3.1: .pelsa_pack_lanes reuses a freed lane after an interval ends", {
  # [1,3], [5,7] (fits lane1 after [1,3]), [2,6] overlaps both -> lane2
  lanes <- .pelsa_pack_lanes(c(1, 5, 2), c(3, 7, 6))
  # first two go on lane 1 (non-overlapping), the [2,6] needs lane 2
  expect_equal(max(lanes), 2L)
})
