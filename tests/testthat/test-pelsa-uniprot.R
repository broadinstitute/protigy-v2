################################################################################
# Tests for the PELSA parity-locked feature classifier (R/tab_pelsa_annotation_io.R).
#
#   pelsa_feature_to_class(ftype, desc)   -> feature_class (vectorized)
#   pelsa_feature_class_scores()          -> feature_class -> class_score
#
# The classifier is PARITY-CRITICAL: it must classify features IDENTICALLY to the
# notebook's uniprot_features.py::feature_to_class (classifier_version "fixed_v1").
# The in-app UniProt fetch + JSON parser were removed (the external fetch workflow
# now emits raw features; Protigy classifies them on load), so only the classifier
# parity remains under test here.
################################################################################

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

# ---- SCORES: parity-locked class_score lookup --------------------------------

test_that("SCORES equals the parity-locked feature_class_scores", {
  scores <- pelsa_feature_class_scores()
  # Parity-locked to the notebook's schema feature_class_scores. The bundled
  # schema.json was removed with the in-app fetch; these exact values ARE the
  # contract the external fetch workflow must match.
  expect_equal(unname(scores["active_or_binding_site"]), 5L)
  expect_equal(unname(scores["catalytic_domain"]), 3L)
  expect_equal(unname(scores["folded_domain"]), 2L)
  expect_equal(unname(scores["region_or_motif"]), 1L)
  expect_equal(unname(scores["repeat_or_coiled_coil"]), -1L)
  expect_equal(unname(scores["transmembrane_or_signal"]), 0L)
  expect_equal(unname(scores["low_complexity_or_disorder"]), -3L)
  expect_equal(unname(scores["other"]), 0L)
})
