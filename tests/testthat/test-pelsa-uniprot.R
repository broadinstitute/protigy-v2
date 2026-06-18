################################################################################
# Tests for the PELSA UniProt feature fetch + JSON parser/classifier (Task 2H).
#
#   pelsa_feature_to_class(ftype, desc)      -> feature_class (vectorized)
#   pelsa_parse_uniprot_json(entry)          -> 8-col per-feature data.frame
#   pelsa_parse_uniprot_json_batch(entries)  -> rbind of per-entry frames
#   pelsa_fetch_uniprot(accessions, ...)     -> list(features=, unresolved=)
#
# The classifier + parser are PARITY-CRITICAL: a fresh fetch (new species or
# cache miss) must classify features IDENTICALLY to the notebook's
# uniprot_features.py::feature_to_class + _parse_json_features
# (classifier_version "fixed_v1"). class_score comes from SCORES, which must
# equal inst/database/human/uniprot_features/schema.json::feature_class_scores.
#
# Ground truth is hand-set in the canned UniProt-shaped JSON fixtures under
# fixtures/pelsa/uniprot_*.json. NO live network is exercised in the default
# run; the one optional smoke test is skip_if_offline()/skip_on_ci()-guarded.
################################################################################

# Read a canned UniProt-shaped fixture as the parsed list shape that
# httr2::resp_body_json() / jsonlite::fromJSON(simplifyVector = FALSE) returns.
read_uniprot_fixture <- function(name) {
  path <- testthat::test_path("fixtures", "pelsa", name)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

# ---- pelsa_feature_to_class: closed-form parity / order-sensitivity ----------

test_that("pelsa_feature_to_class maps the gold cases (incl. the fix-* cases)", {
  expect_equal(pelsa_feature_to_class("Active site", "Proton acceptor"),
               "active_or_binding_site")
  expect_equal(pelsa_feature_to_class("Binding site", "ATP"),
               "active_or_binding_site")
  expect_equal(pelsa_feature_to_class("Metal binding", ""),
               "active_or_binding_site")
  # the fix: DNA binding -> active_or_binding_site
  expect_equal(pelsa_feature_to_class("DNA binding", "Homeobox"),
               "active_or_binding_site")

  expect_equal(pelsa_feature_to_class("Transmembrane", "Helical"),
               "transmembrane_or_signal")
  # the fix: Signal -> transmembrane_or_signal
  expect_equal(pelsa_feature_to_class("Signal", ""),
               "transmembrane_or_signal")
  expect_equal(pelsa_feature_to_class("Signal peptide", ""),
               "transmembrane_or_signal")

  expect_equal(pelsa_feature_to_class("Compositional bias", "Polar residues"),
               "low_complexity_or_disorder")

  expect_equal(pelsa_feature_to_class("Repeat", "WD 1"),
               "repeat_or_coiled_coil")
  # the fix: Coiled coil -> repeat_or_coiled_coil
  expect_equal(pelsa_feature_to_class("Coiled coil", ""),
               "repeat_or_coiled_coil")
  expect_equal(pelsa_feature_to_class("Coiled-coil", ""),
               "repeat_or_coiled_coil")

  expect_equal(pelsa_feature_to_class("Domain", "Protein kinase domain"),
               "catalytic_domain")
  expect_equal(pelsa_feature_to_class("Domain", "PH"),
               "folded_domain")

  expect_equal(pelsa_feature_to_class("Region", "Interaction with substrate"),
               "region_or_motif")
  expect_equal(pelsa_feature_to_class("Motif", "Nuclear localization signal"),
               "region_or_motif")

  expect_equal(pelsa_feature_to_class("Cross-link", "Glycyl lysine isopeptide"),
               "other")
})

test_that("pelsa_feature_to_class respects ORDER-sensitive checks", {
  # disordered-region: the desc-keyword disorder check BEATS region_or_motif
  expect_equal(pelsa_feature_to_class("Region", "Disordered"),
               "low_complexity_or_disorder")
  expect_equal(pelsa_feature_to_class("Region", "Low complexity"),
               "low_complexity_or_disorder")
  expect_equal(pelsa_feature_to_class("Motif", "Compositionally biased"),
               "low_complexity_or_disorder")

  # compositional bias short-circuits first regardless of desc keywords
  expect_equal(pelsa_feature_to_class("Compositional bias", "kinase"),
               "low_complexity_or_disorder")

  # domain catalytic-by-keyword over folded
  for (kw in c("kinase", "methyltransferase", "transferase", "atpase",
               "helicase", "protease", "dehydrogenase")) {
    expect_equal(
      pelsa_feature_to_class("Domain", paste("Some", kw, "thing")),
      "catalytic_domain",
      info = kw
    )
  }

  # HARDEST case: the desc-disorder check (step 4) must BEAT the domain branch
  # (step 6). A Domain whose description is disordered classifies as
  # low_complexity_or_disorder, NOT folded/catalytic. Guards against a future
  # reorder that moves the disorder block below the domain block.
  expect_equal(pelsa_feature_to_class("Domain", "Disordered"),
               "low_complexity_or_disorder")
  # ...even when a catalytic keyword is ALSO present: disorder still wins over
  # the domain catalytic branch.
  expect_equal(pelsa_feature_to_class("Domain", "Disordered kinase domain"),
               "low_complexity_or_disorder")
})

test_that("pelsa_feature_to_class is vectorized and case/space-insensitive", {
  ftypes <- c("  ACTIVE SITE ", "domain", "Region", NA, "")
  descs  <- c("x", "kinase activity", "Disordered", "y", "z")
  expect_equal(
    pelsa_feature_to_class(ftypes, descs),
    c("active_or_binding_site", "catalytic_domain",
      "low_complexity_or_disorder", "other", "other")
  )
})

# ---- SCORES must equal schema.json::feature_class_scores ---------------------

test_that("SCORES equals the cache schema feature_class_scores", {
  scores <- pelsa_feature_class_scores()
  schema_path <- testthat::test_path(
    "..", "..", "inst", "database", "9606", "uniprot_features", "schema.json"
  )
  skip_if_not(file.exists(schema_path), "schema.json not found")
  schema <- jsonlite::fromJSON(schema_path, simplifyVector = TRUE)
  schema_scores <- unlist(schema$feature_class_scores)

  expect_setequal(names(scores), names(schema_scores))
  expect_equal(scores[names(schema_scores)], schema_scores[names(schema_scores)],
               ignore_attr = TRUE)
  # spot-check the exact values from the task spec
  expect_equal(unname(scores["active_or_binding_site"]), 5L)
  expect_equal(unname(scores["catalytic_domain"]), 3L)
  expect_equal(unname(scores["folded_domain"]), 2L)
  expect_equal(unname(scores["region_or_motif"]), 1L)
  expect_equal(unname(scores["repeat_or_coiled_coil"]), -1L)
  expect_equal(unname(scores["transmembrane_or_signal"]), 0L)
  expect_equal(unname(scores["low_complexity_or_disorder"]), -3L)
  expect_equal(unname(scores["other"]), 0L)
})

# ---- pelsa_parse_uniprot_json: per-entry parse ------------------------------

test_that("pelsa_parse_uniprot_json classifies every class with correct schema", {
  entry <- read_uniprot_fixture("uniprot_entry_all_classes.json")
  df <- pelsa_parse_uniprot_json(entry)

  # 17 features, 2 skipped (null start, null end) -> 15 rows
  expect_equal(nrow(df), 15L)
  expect_equal(
    names(df),
    c("accession", "feature_type", "start", "end", "description",
      "feature_class", "class_score", "coord_quality")
  )
  expect_true(is.integer(df$start))
  expect_true(is.integer(df$end))
  expect_true(is.integer(df$class_score))
  expect_true(is.character(df$accession))
  expect_true(all(df$accession == "P11111"))

  # index rows by feature_type/description to assert class + score
  cls <- function(type, start) df$feature_class[df$feature_type == type & df$start == start]
  scr <- function(type, start) df$class_score[df$feature_type == type & df$start == start]

  expect_equal(cls("Active site", 100), "active_or_binding_site")
  expect_equal(scr("Active site", 100), 5L)
  expect_equal(cls("Domain", 200), "catalytic_domain")      # "Protein kinase domain"
  expect_equal(scr("Domain", 200), 3L)
  expect_equal(cls("Domain", 500), "folded_domain")          # "PH"
  expect_equal(scr("Domain", 500), 2L)
  expect_equal(cls("Transmembrane", 700), "transmembrane_or_signal")
  expect_equal(scr("Transmembrane", 700), 0L)
  expect_equal(cls("Compositional bias", 800), "low_complexity_or_disorder")
  expect_equal(scr("Compositional bias", 800), -3L)
  expect_equal(cls("Region", 900), "low_complexity_or_disorder") # "Disordered" beats region
  expect_equal(scr("Region", 900), -3L)
  expect_equal(cls("Region", 1000), "region_or_motif")
  expect_equal(scr("Region", 1000), 1L)
  expect_equal(cls("Repeat", 1100), "repeat_or_coiled_coil")
  expect_equal(scr("Repeat", 1100), -1L)
  expect_equal(cls("Cross-link", 1200), "other")
  expect_equal(scr("Cross-link", 1200), 0L)
  expect_equal(cls("DNA binding", 1300), "active_or_binding_site")
  expect_equal(cls("Signal", 1), "transmembrane_or_signal")
  expect_equal(cls("Coiled coil", 1400), "repeat_or_coiled_coil")

  # parsed parity for the HARDEST order-case: a Domain with a disordered
  # description -> low_complexity_or_disorder (-3), NOT folded/catalytic. This
  # proves step-4 (desc-disorder) beats step-6 (domain) through the full parser.
  expect_equal(cls("Domain", 1650), "low_complexity_or_disorder")
  expect_equal(scr("Domain", 1650), -3L)
})

test_that("pelsa_parse_uniprot_json: coord_quality exact vs fuzzy", {
  entry <- read_uniprot_fixture("uniprot_entry_all_classes.json")
  df <- pelsa_parse_uniprot_json(entry)

  # both EXACT -> "exact"
  expect_equal(df$coord_quality[df$feature_type == "Active site"], "exact")
  # start modifier "OUTSIDE" -> "fuzzy"
  expect_equal(df$coord_quality[df$feature_type == "Region" & df$start == 1500], "fuzzy")
})

test_that("pelsa_parse_uniprot_json: description falls back to ligand name", {
  entry <- read_uniprot_fixture("uniprot_entry_all_classes.json")
  df <- pelsa_parse_uniprot_json(entry)
  # Binding site with empty description + ligand name "ATP"
  bind <- df[df$feature_type == "Binding site" & df$start == 1600, , drop = FALSE]
  expect_equal(nrow(bind), 1L)
  expect_equal(bind$description, "ATP")
  expect_equal(bind$feature_class, "active_or_binding_site")
})

test_that("pelsa_parse_uniprot_json: NULL start or end features are skipped", {
  entry <- read_uniprot_fixture("uniprot_entry_all_classes.json")
  df <- pelsa_parse_uniprot_json(entry)
  # the two Domain features with start=1700/end=null and start=null/end=1800
  expect_equal(sum(df$start == 1700, na.rm = TRUE), 0L)
  expect_equal(sum(df$end == 1800, na.rm = TRUE), 0L)
})

test_that("pelsa_parse_uniprot_json: empty/NULL features -> 0-row typed frame", {
  empty <- read_uniprot_fixture("uniprot_entry_empty.json")
  df <- pelsa_parse_uniprot_json(empty)
  expect_equal(nrow(df), 0L)
  expect_equal(
    names(df),
    c("accession", "feature_type", "start", "end", "description",
      "feature_class", "class_score", "coord_quality")
  )
  expect_true(is.integer(df$start))
  expect_true(is.integer(df$end))
  expect_true(is.integer(df$class_score))
  expect_true(is.character(df$accession))

  # features key entirely absent
  df2 <- pelsa_parse_uniprot_json(list(primaryAccession = "X99999"))
  expect_equal(nrow(df2), 0L)
})

test_that("pelsa_parse_uniprot_json: single-feature entry -> 1-row frame", {
  # Locks the "features is an unnamed list of objects" contract against a
  # single-object simplify quirk (one feature must still iterate as a list).
  single <- read_uniprot_fixture("uniprot_entry_single.json")
  df <- pelsa_parse_uniprot_json(single)
  expect_equal(nrow(df), 1L)
  expect_equal(df$accession, "S11111")
  expect_equal(df$feature_type, "Active site")
  expect_equal(df$start, 17L)
  expect_equal(df$end, 17L)
  expect_equal(df$feature_class, "active_or_binding_site")
  expect_equal(df$class_score, 5L)
})

# ---- pelsa_parse_uniprot_json_batch -----------------------------------------

test_that("pelsa_parse_uniprot_json_batch rbinds per-entry frames", {
  e1 <- read_uniprot_fixture("uniprot_entry_simple.json")  # Q22222, 2 features
  e2 <- read_uniprot_fixture("uniprot_entry_empty.json")   # E00000, 0 features
  df <- pelsa_parse_uniprot_json_batch(list(e1, e2))

  expect_equal(nrow(df), 2L)
  expect_true(all(df$accession == "Q22222"))
  expect_setequal(df$feature_type, c("Active site", "Repeat"))
  expect_equal(
    names(df),
    c("accession", "feature_type", "start", "end", "description",
      "feature_class", "class_score", "coord_quality")
  )

  # empty batch
  df0 <- pelsa_parse_uniprot_json_batch(list())
  expect_equal(nrow(df0), 0L)
  expect_equal(length(names(df0)), 8L)
})

# ---- pelsa_fetch_uniprot: input validation (no network) ----------------------

test_that("pelsa_fetch_uniprot validates accessions input", {
  expect_error(pelsa_fetch_uniprot(123))
  expect_error(pelsa_fetch_uniprot(list("P12345")))
})

test_that("pelsa_fetch_uniprot returns empty result for empty input (no network)", {
  res <- pelsa_fetch_uniprot(character(0))
  expect_named(res, c("features", "unresolved", "transient_unresolved",
                      "canceled"))
  expect_equal(nrow(res$features), 0L)
  expect_equal(length(res$unresolved), 0L)
  expect_equal(length(res$transient_unresolved), 0L)
  expect_false(res$canceled)
})

test_that("pelsa_fetch_uniprot cancels before the first batch (no network)", {
  # should_cancel TRUE -> the loop breaks at the first boundary before any
  # request, so NO network is touched. All accessions are unresolved, canceled.
  res <- pelsa_fetch_uniprot(c("P00001", "P00002", "P00003"),
                             should_cancel = function() TRUE)
  expect_true(res$canceled)
  expect_equal(nrow(res$features), 0L)
  expect_setequal(res$unresolved, c("P00001", "P00002", "P00003"))
})

# ---- batched /search parse parity (no network) ------------------------------
# The batched fetcher pulls a {"results": [ <entry>, ... ]} page and parses the
# `results` array with pelsa_parse_uniprot_json_batch. This must yield EXACTLY
# the same 8-col rows as parsing each entry alone (the per-accession design's
# output) - batching is a transport change, never a parsing change.

test_that("a /search results array parses identically to per-accession entries", {
  mk_entry <- function(acc, type, desc, s, e) list(
    primaryAccession = acc,
    features = list(list(
      type = type, description = desc,
      location = list(start = list(value = s, modifier = "EXACT"),
                      end   = list(value = e, modifier = "EXACT"))
    ))
  )
  entries <- list(
    mk_entry("P00001", "Active site", "Charge relay system", 48L, 48L),
    mk_entry("P00002", "Domain", "Protein kinase domain", 10L, 260L),
    mk_entry("P00003", "Transmembrane", "Helical", 35L, 55L)
  )
  # The /search page shape the batched fetcher consumes.
  search_page <- list(results = entries)

  per_accession <- do.call(rbind, lapply(entries, pelsa_parse_uniprot_json))
  rownames(per_accession) <- NULL
  batched <- pelsa_parse_uniprot_json_batch(search_page$results)

  expect_identical(batched, per_accession)
  # Spot-check the classifier on the batched rows (content, not just shape).
  expect_identical(
    batched$feature_class,
    c("active_or_binding_site", "catalytic_domain", "transmembrane_or_signal")
  )
  expect_identical(batched$class_score, c(5L, 3L, 0L))
})

# ---- optional live smoke test (guarded) -------------------------------------

test_that("pelsa_fetch_uniprot live smoke test (batched)", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_offline()

  res <- pelsa_fetch_uniprot("P04637")  # TP53
  expect_named(res, c("features", "unresolved", "transient_unresolved", "canceled"))
  expect_gt(nrow(res$features), 0L)
  expect_true(all(res$features$accession == "P04637"))
  expect_true(all(
    res$features$feature_class %in% names(pelsa_feature_class_scores())
  ))
})

# ---- live correctness: batched fetch == manual ground truth -----------------
# Verifies the BATCHED fetcher returns the SAME annotations as fetching each
# accession alone (the independent per-accession .json path = ground truth), AND
# that specific, hand-verified features match the real UniProt entry. Guarded:
# only runs with a live network, off CI/CRAN.

test_that("batched fetch matches per-accession ground truth for known accessions", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_offline()

  accs <- c("P04637", "P00533", "P38398", "P00761")  # TP53, EGFR, BRCA1, Trypsin

  # (A) batched (one /search query, cursor-paginated). 4 accessions fit one
  # batch under UniProt's 100-OR cap; use the default batch size.
  batched <- pelsa_fetch_uniprot(accs)$features

  # (B) ground truth: per-accession .json, parsed by the same pure parser
  manual_one <- function(acc) {
    parsed <- jsonlite::fromJSON(
      sprintf("https://rest.uniprot.org/uniprotkb/%s.json", acc),
      simplifyVector = FALSE)
    pelsa_parse_uniprot_json(parsed)
  }
  manual <- do.call(rbind, lapply(accs, manual_one))
  rownames(manual) <- NULL

  ord <- function(df) {
    df[order(df$accession, df$feature_type, df$start, df$end, df$description),
       , drop = FALSE]
  }
  b <- ord(batched); rownames(b) <- NULL
  m <- ord(manual);  rownames(m) <- NULL

  expect_equal(b, m)                                   # byte-identical content
  expect_setequal(unique(batched$accession), accs)     # all 4 resolved
})

test_that("batched fetch returns hand-verified P00761 (trypsin) features", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_offline()

  # Ground truth read MANUALLY off the UniProt P00761 entry:
  #   Active site   48,  92, 185  ("Charge relay system")
  #   Binding site  60,  62,  65, 70  (Ca2+ ligand)
  #   Site         179  ("Required for specificity")
  #   (no Signal / Transmembrane feature on this entry)
  p <- pelsa_fetch_uniprot("P00761")$features
  p <- p[p$accession == "P00761", , drop = FALSE]

  act <- p[p$feature_type == "Active site", ]
  expect_setequal(act$start, c(48L, 92L, 185L))
  expect_true(all(act$feature_class == "active_or_binding_site"))
  expect_true(all(act$class_score == 5L))

  bind <- p[p$feature_type == "Binding site", ]
  expect_setequal(bind$start, c(60L, 62L, 65L, 70L))

  site <- p[p$feature_type == "Site" & p$start == 179L, ]
  expect_equal(nrow(site), 1L)
  expect_equal(site$description, "Required for specificity")

  # Correctly NO transmembrane/signal feature on trypsin.
  expect_equal(nrow(p[p$feature_class == "transmembrane_or_signal", ]), 0L)
})
