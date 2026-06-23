################################################################################
# Tests for pelsa_export_input_files() in R/tab_pelsa_export_helpers.R
################################################################################

test_that("pelsa_export_input_files copies inputs (original names) + missing list", {
  src_dir <- tempfile(); dir.create(src_dir)
  # Shiny stores uploads under a mangled datapath; the original name is separate.
  fasta <- file.path(src_dir, "0.fasta"); writeLines(c(">P1", "MASE"), fasta)
  ann   <- file.path(src_dir, "1.tsv")
  writeLines("accession\tfeature_type\tstart\tend\tdescription", ann)
  out_dir <- tempfile(); dir.create(out_dir)

  pelsa_export_input_files(
    out_dir,
    fasta_path = fasta, fasta_name = "human.fasta",
    annotation_path = ann, annotation_name = "ann.tsv",
    missing_accessions = c("P9", "P8")
  )

  expect_true(file.exists(file.path(out_dir, "human.fasta")))
  expect_true(file.exists(file.path(out_dir, "ann.tsv")))
  expect_equal(readLines(file.path(out_dir, "missing_accessions.txt")),
               c("P9", "P8"))
})

test_that("pelsa_export_input_files handles a self-curated dataset (no annotation)", {
  src_dir <- tempfile(); dir.create(src_dir)
  fasta <- file.path(src_dir, "0.fa"); writeLines(c(">G1", "MK"), fasta)
  out_dir <- tempfile(); dir.create(out_dir)

  pelsa_export_input_files(
    out_dir,
    fasta_path = fasta, fasta_name = "custom.fa",
    annotation_path = NULL, annotation_name = NULL,
    missing_accessions = character(0)
  )

  expect_true(file.exists(file.path(out_dir, "custom.fa")))
  expect_length(list.files(out_dir, pattern = "\\.tsv$"), 0L)
  expect_equal(readLines(file.path(out_dir, "missing_accessions.txt")),
               character(0))
})

test_that("pelsa_export_input_files falls back to basename when no name given", {
  src_dir <- tempfile(); dir.create(src_dir)
  fasta <- file.path(src_dir, "proteome.fasta"); writeLines(c(">P1", "M"), fasta)
  out_dir <- tempfile(); dir.create(out_dir)

  pelsa_export_input_files(out_dir, fasta_path = fasta, fasta_name = NULL,
                           annotation_path = NULL, annotation_name = NULL,
                           missing_accessions = character(0))
  expect_true(file.exists(file.path(out_dir, "proteome.fasta")))
})
