# Phase 3 (P3.6) -- setup-pipeline safety nets.
#
# Exercises the anti-mutation deep-copy helpers, the export-hygiene rdesc
# repackaging / gene-symbol strip, and the real merge_processed_gcts conflict
# rename + missing-column NA-fill branches. These were previously untested.
#
# merge_processed_gcts wraps its body in shiny::withProgress, which needs a
# reactive domain. We supply a MockShinySession via withReactiveDomain so we can
# call the REAL function (not a hand-copied merge body).

# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

make_small_gct <- function(rids, cids, cdesc, rdesc = NULL) {
  mat <- matrix(seq_len(length(rids) * length(cids)),
                nrow = length(rids), ncol = length(cids),
                dimnames = list(rids, cids))
  if (is.null(rdesc)) {
    rdesc <- data.frame(id = rids, stringsAsFactors = FALSE)
  }
  rownames(rdesc) <- rids
  rownames(cdesc) <- cids
  new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc, rid = rids, cid = cids)
}

with_progress_domain <- function(expr) {
  sess <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(sess, force(expr))
}

# ---------------------------------------------------------------------------
# df_deep_copy / deep_clone_gct -- mutate the clone, source unchanged
# ---------------------------------------------------------------------------

test_that("df_deep_copy returns an independent copy (NULL passthrough)", {
  expect_null(df_deep_copy(NULL))

  orig <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  cp <- df_deep_copy(orig)
  expect_equal(cp, orig)

  cp$a[1] <- 999L
  cp$b[2] <- "ZZZ"
  # Source must be untouched.
  expect_equal(orig$a, 1:3)
  expect_equal(orig$b, letters[1:3])
})

test_that("df_deep_copy coerces non-data.frame input to a data.frame", {
  m <- matrix(1:4, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  cp <- df_deep_copy(m)
  expect_s3_class(cp, "data.frame")
})

test_that("deep_clone_gct yields a fully independent GCT (mat + rdesc + cdesc)", {
  cdesc <- data.frame(group = c("A", "B"), stringsAsFactors = FALSE)
  rdesc <- data.frame(id = c("g1", "g2"), geneSymbol = c("GENE1", "GENE2"),
                      stringsAsFactors = FALSE)
  gct <- make_small_gct(c("g1", "g2"), c("s1", "s2"), cdesc, rdesc)

  clone <- deep_clone_gct(gct)

  # Equal values up front.
  expect_equal(clone@mat, gct@mat)
  expect_equal(clone@rdesc$geneSymbol, gct@rdesc$geneSymbol)
  expect_equal(clone@cdesc$group, gct@cdesc$group)
  expect_identical(clone@rid, gct@rid)
  expect_identical(clone@cid, gct@cid)

  # Mutate clone in every slot; source must remain identical to its originals.
  clone@mat[1, 1] <- -100
  clone@rdesc$geneSymbol[1] <- "MUTATED"
  clone@cdesc$group[1] <- "MUTATED"

  expect_equal(gct@mat[1, 1], 1)            # original first cell
  expect_equal(gct@rdesc$geneSymbol[1], "GENE1")
  expect_equal(gct@cdesc$group[1], "A")
})

# ---------------------------------------------------------------------------
# strip_gene_symbol_mapping_columns
# ---------------------------------------------------------------------------

test_that("strip_gene_symbol_mapping_columns drops geneSymbol_original* only", {
  rdesc <- data.frame(
    id = c("g1", "g2"),
    geneSymbol = c("A", "B"),
    geneSymbol_original = c("a", "b"),
    geneSymbol_original_2 = c("a2", "b2"),
    other = c(1, 2),
    stringsAsFactors = FALSE
  )
  out <- strip_gene_symbol_mapping_columns(rdesc)
  expect_false(any(grepl("^geneSymbol_original", names(out))))
  expect_true(all(c("id", "geneSymbol", "other") %in% names(out)))
})

test_that("strip_gene_symbol_mapping_columns is a no-op when nothing matches", {
  rdesc <- data.frame(id = "g1", geneSymbol = "A", stringsAsFactors = FALSE)
  expect_equal(strip_gene_symbol_mapping_columns(rdesc), rdesc)
  expect_null(strip_gene_symbol_mapping_columns(NULL))
  # Non-data.frame returns unchanged.
  expect_equal(strip_gene_symbol_mapping_columns(1:3), 1:3)
})

# ---------------------------------------------------------------------------
# repackage_transformed_gct_with_upload_rdesc
# ---------------------------------------------------------------------------

test_that("repackage uses upload rdesc (no geneSymbol_original* leak) when rids align", {
  upload_rdesc <- data.frame(id = c("g1", "g2"),
                             geneSymbol = c("A", "B"),
                             stringsAsFactors = FALSE)
  gct_upload <- make_small_gct(c("g1", "g2"), c("s1", "s2"),
                               data.frame(group = c("X", "Y")), upload_rdesc)

  # transformed has the same rids but with a pipeline-only backup column
  trans_rdesc <- data.frame(id = c("g1", "g2"),
                            geneSymbol = c("A2", "B2"),
                            geneSymbol_original = c("A", "B"),
                            stringsAsFactors = FALSE)
  gct_trans <- make_small_gct(c("g1", "g2"), c("s1", "s2"),
                              data.frame(group = c("X", "Y")), trans_rdesc)

  out <- repackage_transformed_gct_with_upload_rdesc(gct_trans, gct_upload)
  expect_false("geneSymbol_original" %in% names(out@rdesc))
  # rdesc comes from the upload object (original geneSymbol values)
  expect_equal(out@rdesc$geneSymbol, c("A", "B"))
})

test_that("repackage falls back to strip when rids do not align with upload", {
  upload_rdesc <- data.frame(id = c("g1", "g2"), stringsAsFactors = FALSE)
  gct_upload <- make_small_gct(c("g1", "g2"), c("s1", "s2"),
                               data.frame(group = c("X", "Y")), upload_rdesc)

  # transformed has DIFFERENT rids -> cannot reuse upload rdesc by name
  trans_rdesc <- data.frame(id = c("gX", "gY"),
                            geneSymbol_original = c("a", "b"),
                            stringsAsFactors = FALSE)
  gct_trans <- make_small_gct(c("gX", "gY"), c("s1", "s2"),
                              data.frame(group = c("X", "Y")), trans_rdesc)

  out <- repackage_transformed_gct_with_upload_rdesc(gct_trans, gct_upload)
  expect_false(any(grepl("^geneSymbol_original", names(out@rdesc))))
})

test_that("repackage passes through NULL inputs", {
  expect_null(repackage_transformed_gct_with_upload_rdesc(NULL, NULL))
})

# ---------------------------------------------------------------------------
# merge_processed_gcts -- conflict rename + missing-column NA-fill
# ---------------------------------------------------------------------------

test_that("merge_processed_gcts renames conflicting cdesc columns per ome", {
  # Two omes share sample ids but disagree on the `batch` column -> conflict.
  cdesc_a <- data.frame(batch = c("b1", "b2"), shared = c("z", "z"),
                        stringsAsFactors = FALSE)
  cdesc_b <- data.frame(batch = c("DIFF1", "DIFF2"), shared = c("z", "z"),
                        stringsAsFactors = FALSE)

  gct_a <- make_small_gct(c("g1", "g2"), c("s1", "s2"), cdesc_a)
  gct_b <- make_small_gct(c("h1", "h2"), c("s1", "s2"), cdesc_b)

  GCTs <- list(omeA = gct_a, omeB = gct_b)
  params <- list(omeA = list(dataset_label = "omeA"),
                 omeB = list(dataset_label = "omeB"))

  merged <- with_progress_domain(merge_processed_gcts(GCTs, params))

  cols <- names(merged@cdesc)
  # The conflicting `batch` column is duplicated into per-ome variants so no
  # ome's values are lost (the core conflict-rename invariant).
  expect_true("batch.omeA" %in% cols)
  expect_true("batch.omeB" %in% cols)
  expect_equal(merged@cdesc[c("s1", "s2"), "batch.omeA"], c("b1", "b2"))
  expect_equal(merged@cdesc[c("s1", "s2"), "batch.omeB"], c("DIFF1", "DIFF2"))
  # Non-conflicting shared column survives untouched.
  expect_true("shared" %in% cols)
  # The original ambiguous `batch` column must NOT be resurrected. Previously the
  # missing-column NA-fill loop re-added it populated with only one ome's values
  # (last-writer-wins), silently dropping the other ome's data. Only the three
  # intended columns (batch.omeA, batch.omeB, shared) should remain.
  expect_false("batch" %in% cols)
  expect_equal(length(cols), 3L)
})

test_that("merge_processed_gcts fills missing one-ome-only columns with NA", {
  # `extra` exists only in omeA; merged should carry it with NA for omeB samples.
  cdesc_a <- data.frame(grp = c("p", "q"), extra = c("E1", "E2"),
                        stringsAsFactors = FALSE)
  cdesc_b <- data.frame(grp = c("p", "q"),
                        stringsAsFactors = FALSE)

  gct_a <- make_small_gct(c("g1", "g2"), c("sA1", "sA2"), cdesc_a)
  gct_b <- make_small_gct(c("h1", "h2"), c("sB1", "sB2"), cdesc_b)

  GCTs <- list(omeA = gct_a, omeB = gct_b)
  params <- list(omeA = list(dataset_label = "omeA"),
                 omeB = list(dataset_label = "omeB"))

  merged <- with_progress_domain(merge_processed_gcts(GCTs, params))

  expect_true("extra" %in% names(merged@cdesc))
  # omeA samples keep their values; omeB samples are NA.
  expect_equal(merged@cdesc[c("sA1", "sA2"), "extra"], c("E1", "E2"))
  expect_true(all(is.na(merged@cdesc[c("sB1", "sB2"), "extra"])))

  # NOTE on the `x` column: when the two omes have DISJOINT sample ids,
  # cmapR::merge_gct introduces a spurious all-NA `x` column in the merged
  # cdesc. This was verified to originate inside cmapR itself (it is present in
  # the raw cmapR::merge_gct output before any of merge_processed_gcts' own
  # post-processing runs), not from our merge logic. It carries no data, so we
  # assert the load-bearing invariant -- the real annotation columns are present
  # and no values were lost -- rather than fighting the cmapR internal.
  expect_true(all(c("grp", "extra") %in% names(merged@cdesc)))
  # grp came from both omes and is intact for every sample (no data lost).
  expect_equal(merged@cdesc[c("sA1", "sA2", "sB1", "sB2"), "grp"],
               c("p", "q", "p", "q"))
})

test_that("merge_processed_gcts adds protigy.ome from dataset_label and prefixes rids", {
  cdesc_a <- data.frame(grp = c("p", "q"), stringsAsFactors = FALSE)
  cdesc_b <- data.frame(grp = c("p", "q"), stringsAsFactors = FALSE)
  gct_a <- make_small_gct(c("g1", "g2"), c("sA1", "sA2"), cdesc_a)
  gct_b <- make_small_gct(c("h1", "h2"), c("sB1", "sB2"), cdesc_b)

  GCTs <- list(omeA = gct_a, omeB = gct_b)
  params <- list(omeA = list(dataset_label = "labelA"),
                 omeB = list(dataset_label = "labelB"))

  merged <- with_progress_domain(merge_processed_gcts(GCTs, params))

  expect_equal(nrow(merged@mat), 4)
  expect_true("protigy.ome" %in% names(merged@rdesc))
  expect_setequal(unique(merged@rdesc$protigy.ome), c("labelA", "labelB"))
  # rids prefixed by the dataset label.
  expect_true(any(startsWith(merged@rid, "labelA_")))
  expect_true(any(startsWith(merged@rid, "labelB_")))
})
