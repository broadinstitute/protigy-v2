################################################################################
# INT-2: discrete-column cache for the setup panel.
#
# These tests use SYNTHETIC data with HAND-DERIVED ground truth (not just
# "new matches old") so they validate correctness independently of the original
# implementation. They also lock in the safety property the cache must preserve:
# the original recompute-every-render design could never show stale choices, so
# the cache must always reflect the CURRENT annotation table after any
# upload / removal / reprocess.
#
# is.discrete() rule used to derive ground truth (nfactor_cutoff = 20):
#   a column is CONTINUOUS iff it has > 20 unique values AND they are all numeric
#   (after NA-pattern handling); otherwise it is DISCRETE.
################################################################################

# Build a minimal valid GCT from rdesc/cdesc (mat content is irrelevant here).
mk_test_gct <- function(rdesc, cdesc) {
  mat <- matrix(
    0, nrow = nrow(rdesc), ncol = nrow(cdesc),
    dimnames = list(rdesc$id, cdesc$id)
  )
  methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
}

test_that("compute_discrete_columns matches hand-derived ground truth (rdesc)", {
  rdesc <- data.frame(
    id            = paste0("g", 1:40),                       # non-numeric strings -> DISCRETE
    geneSymbol    = rep(c("EGFR", "KRAS", "TP53", "BRCA1"), 10), # 4 unique -> DISCRETE
    cluster       = rep(c("A", "B"), 20),                    # 2 unique -> DISCRETE
    score_cont    = as.character(1:40),                      # 40 unique numeric -> CONTINUOUS
    small_numeric = as.character(rep(1:5, 8)),               # 5 unique numeric -> DISCRETE
    exactly20num  = as.character(rep(1:20, 2)),              # exactly 20 -> DISCRETE
    over20num     = as.character(rep(1:21, length.out = 40)),# 21 unique numeric -> CONTINUOUS
    allNA         = rep(NA_character_, 40),                  # all NA -> DISCRETE
    na_patterns   = rep(c("NA", "unknown", "n/a", ""), 10),  # NA patterns -> DISCRETE
    stringsAsFactors = FALSE
  )
  expected <- c("id", "geneSymbol", "cluster", "small_numeric",
                "exactly20num", "allNA", "na_patterns")
  expect_identical(compute_discrete_columns(rdesc), expected)
})

test_that("compute_discrete_columns matches hand-derived ground truth (cdesc)", {
  cdesc <- data.frame(
    id        = paste0("s", 1:25),                    # non-numeric strings -> DISCRETE
    Condition = rep(c("ctrl", "treat"), length.out = 25), # 2 unique -> DISCRETE
    Batch     = as.character(rep(1:3, length.out = 25)),  # 3 unique numeric -> DISCRETE
    ContScore = as.character(1:25),                   # 25 unique numeric -> CONTINUOUS
    stringsAsFactors = FALSE
  )
  expect_identical(compute_discrete_columns(cdesc),
                   c("id", "Condition", "Batch"))
})

test_that("build_discrete_columns_map + resolve match ground truth for both upload-path shapes", {
  # GCT-upload shape: rich rdesc annotations, cdesc from gct header
  gct_path <- mk_test_gct(
    rdesc = data.frame(id = paste0("p", 1:50),
                       geneSymbol = rep(c("A", "B", "C", "D", "E"), 10), # 5 unique -> DISCRETE
                       accession  = paste0("ACC", 1:50),                 # 50 unique non-numeric -> DISCRETE
                       stringsAsFactors = FALSE),
    cdesc = data.frame(id = paste0("s", 1:8), Sample.ID = paste0("s", 1:8),
                       Condition = rep(c("A", "B"), 4), stringsAsFactors = FALSE)
  )
  # CSV+design shape: minimal rdesc (id only), cdesc from experimental design
  csv_path <- mk_test_gct(
    rdesc = data.frame(id = paste0("prot", 1:50), stringsAsFactors = FALSE),
    cdesc = data.frame(id = paste0("s", 1:8), Sample.ID = paste0("s", 1:8),
                       Treatment = rep(c("ctrl", "drug"), 4),
                       Dose = as.character(c(0, 10, 0, 10, 0, 10, 0, 10)), # 2 unique numeric -> DISCRETE
                       stringsAsFactors = FALSE)
  )
  GCTs <- list(proteome = gct_path, csv_ome = csv_path)
  map <- build_discrete_columns_map(GCTs)

  # GCT-path ground truth
  expect_identical(resolve_discrete_columns(map, "proteome", "rdesc", gct_path@rdesc),
                   c("id", "geneSymbol", "accession"))
  expect_identical(resolve_discrete_columns(map, "proteome", "cdesc", gct_path@cdesc),
                   c("id", "Sample.ID", "Condition"))
  # CSV-path ground truth
  expect_identical(resolve_discrete_columns(map, "csv_ome", "rdesc", csv_path@rdesc),
                   "id")
  expect_identical(resolve_discrete_columns(map, "csv_ome", "cdesc", csv_path@cdesc),
                   c("id", "Sample.ID", "Treatment", "Dose"))
})

test_that("SAFETY: re-upload with different columns produces no stale carryover", {
  # Same label, version 1 then version 2 with different rdesc columns.
  rdesc_v1 <- data.frame(id = paste0("g", 1:5),
                         oldcol = c("a", "b", "a", "c", "b"), stringsAsFactors = FALSE)
  rdesc_v2 <- data.frame(id = paste0("g", 1:5),
                         newcol = c("p", "q", "p", "r", "q"),
                         extra  = c("m", "m", "n", "n", "o"), stringsAsFactors = FALSE)
  cdesc <- data.frame(id = paste0("s", 1:4), G = rep(c("x", "y"), 2), stringsAsFactors = FALSE)

  map_v1 <- build_discrete_columns_map(list(prot = mk_test_gct(rdesc_v1, cdesc)))
  map_v2 <- build_discrete_columns_map(list(prot = mk_test_gct(rdesc_v2, cdesc)))

  expect_identical(map_v1$prot$rdesc, c("id", "oldcol"))
  expect_identical(map_v2$prot$rdesc, c("id", "newcol", "extra"))
  expect_false(identical(map_v1$prot$rdesc, map_v2$prot$rdesc))
})

test_that("SAFETY: value change that flips the verdict is reflected (same column name)", {
  cdesc <- data.frame(id = "s1", G = "x", stringsAsFactors = FALSE)
  # 'v' as 5 unique numeric -> DISCRETE
  rdesc_disc <- data.frame(id = paste0("g", 1:5),
                           v = c("1", "1", "2", "2", "3"), stringsAsFactors = FALSE)
  # 'v' as 30 unique numeric -> CONTINUOUS
  rdesc_cont <- data.frame(id = paste0("g", 1:30),
                           v = as.character(1:30), stringsAsFactors = FALSE)

  m_disc <- build_discrete_columns_map(list(o = mk_test_gct(rdesc_disc, cdesc)))
  m_cont <- build_discrete_columns_map(list(o = mk_test_gct(rdesc_cont, cdesc)))

  expect_true("v" %in% m_disc$o$rdesc)   # discrete -> included
  expect_false("v" %in% m_cont$o$rdesc)  # continuous -> excluded
})

test_that("SAFETY: missing label / NULL map fall back to a LIVE scan (never stale/empty)", {
  fresh <- data.frame(id = paste0("x", 1:6),
                      cat = c("a", "a", "b", "b", "c", "c"), stringsAsFactors = FALSE)
  stale_map <- build_discrete_columns_map(
    list(old_ome = mk_test_gct(data.frame(id = "g1", k = "a", stringsAsFactors = FALSE),
                               data.frame(id = "s1", G = "x", stringsAsFactors = FALSE)))
  )
  # label not in map -> must compute from live df, returning the correct ground truth
  expect_identical(resolve_discrete_columns(stale_map, "new_ome", "rdesc", fresh),
                   c("id", "cat"))
  # NULL map (tests / pre-cache) -> live scan
  expect_identical(resolve_discrete_columns(NULL, "any", "rdesc", fresh),
                   c("id", "cat"))
})

test_that("SAFETY: removed ome is absent from the recomputed map", {
  cdesc <- data.frame(id = "s1", G = "x", stringsAsFactors = FALSE)
  ga <- mk_test_gct(data.frame(id = "g1", k = "a", stringsAsFactors = FALSE), cdesc)
  gb <- mk_test_gct(data.frame(id = "g2", k = "b", stringsAsFactors = FALSE), cdesc)
  map_two <- build_discrete_columns_map(list(a = ga, b = gb))
  map_one <- build_discrete_columns_map(list(a = ga))  # 'b' removed
  expect_true("b" %in% names(map_two))
  expect_false("b" %in% names(map_one))
})

test_that("edge cases: empty / NULL inputs", {
  expect_identical(build_discrete_columns_map(NULL), list())
  expect_identical(build_discrete_columns_map(list()), list())
  expect_identical(compute_discrete_columns(data.frame()), character(0))
  # df with columns but zero rows: names present, is.discrete handles empty cols
  empty_rows <- data.frame(id = character(0), x = character(0), stringsAsFactors = FALSE)
  expect_identical(compute_discrete_columns(empty_rows), c("id", "x"))
})
