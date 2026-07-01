# Tests for the SHARED significance-statistic (sig_stat) contract across the
# PELSA volcano, its exports, and the Woods / intensity panels. The user picks
# the stat in Statistics > Summary (stat_params()[[ome]]$stat): "adj.p.val"
# (default) classifies on adj.P.Val; "nom.p.val" classifies on the raw P.Value
# and draws the threshold at -log10(cutoff). Every PELSA surface that flags
# significance must honor the same choice so they agree on identical data.
#
# Discriminating peptide throughout: P.Value = 0.01 (passes nominal at 0.05) but
# adj.P.Val = 0.30 (fails adj at 0.05). It must be significant under nom.p.val
# and NOT significant under adj.p.val.

# A minimal per-peptide stat frame carrying BOTH p columns for one contrast.
.sig_stat_df <- function(contrast = "C1") {
  df <- data.frame(
    PEP.StrippedSequence = "PEPNOM",
    PG.ProteinAccessions = "ACC1",
    PG.Genes             = "GNOM",
    pep_start            = 10L,
    pep_end              = 14L,
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  df[[paste0("logFC.", contrast)]]     <- 2.0
  df[[paste0("adj.P.Val.", contrast)]] <- 0.30
  df[[paste0("P.Value.", contrast)]]   <- 0.01
  df$.row_id <- 1L
  df
}

.sig_stat_matched <- function() {
  data.frame(
    PEP.StrippedSequence = "PEPNOM",
    accession            = "ACC1",
    gene                 = "GNOM",
    pep_start            = 10L,
    pep_end              = 14L,
    .row_id              = 1L,
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
}

# --- pelsa_woods_peptide_data ------------------------------------------------

test_that("woods peptide data: nom.p.val flags the peptide significant", {
  stat <- .sig_stat_df()
  matched <- .sig_stat_matched()
  out <- pelsa_woods_peptide_data("ACC1", matched, stat, "C1",
                                  sig_cutoff = 0.05, sig_stat = "nom.p.val")
  expect_true(all(out$sig))
})

test_that("woods peptide data: adj.p.val (default) leaves it non-significant", {
  stat <- .sig_stat_df()
  matched <- .sig_stat_matched()
  out <- pelsa_woods_peptide_data("ACC1", matched, stat, "C1",
                                  sig_cutoff = 0.05)
  expect_false(any(out$sig))
})

# --- pelsa_intensity_proteins ------------------------------------------------

test_that("intensity proteins: nom.p.val includes the protein (>=1 sig peptide)", {
  stat <- .sig_stat_df()
  matched <- .sig_stat_matched()
  out <- pelsa_intensity_proteins(stat, matched, markers = character(0),
                                  contrast = "C1", sig_cutoff = 0.05,
                                  sig_stat = "nom.p.val")
  expect_true("ACC1" %in% as.character(out$accession))
})

test_that("intensity proteins: adj.p.val (default) excludes it", {
  stat <- .sig_stat_df()
  matched <- .sig_stat_matched()
  out <- pelsa_intensity_proteins(stat, matched, markers = character(0),
                                  contrast = "C1", sig_cutoff = 0.05)
  expect_false("ACC1" %in% as.character(out$accession))
})

# --- pelsa_volcano_export_df (exports must mirror the on-screen volcano) ------

test_that("export df: nom.p.val classifies on P.Value", {
  stat <- .sig_stat_df()
  matched <- .sig_stat_matched()
  fdf <- data.frame(accession = character(0), start = integer(0),
                    end = integer(0), feature_class = character(0))
  out <- pelsa_volcano_export_df(stat, matched, fdf, markers = character(0),
                                 contrast = "C1", panel = "all_peptide",
                                 sig_cutoff = 0.05, sig_stat = "nom.p.val")
  expect_true(out$Significant)
})

test_that("export df: adj.p.val (default) leaves it non-significant", {
  stat <- .sig_stat_df()
  matched <- .sig_stat_matched()
  fdf <- data.frame(accession = character(0), start = integer(0),
                    end = integer(0), feature_class = character(0))
  out <- pelsa_volcano_export_df(stat, matched, fdf, markers = character(0),
                                 contrast = "C1", panel = "all_peptide",
                                 sig_cutoff = 0.05)
  expect_false(out$Significant)
})

# --- pelsa_woods_export_ggplot: caption + star reflect the chosen stat --------
# The peptides frame is derived END-TO-END from pelsa_woods_peptide_data() with
# the SAME sig_stat, so the star count genuinely depends on the stat choice (not
# a hand-set sig column).

.woods_star_rows <- function(g) {
  star_layer <- g$layers[[which(vapply(g$layers,
    function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]]]
  nrow(star_layer$data)
}

test_that("woods export ggplot: nom.p.val stars the peptide + labels nom.P", {
  pep <- pelsa_woods_peptide_data("ACC1", .sig_stat_matched(), .sig_stat_df(),
                                  "C1", sig_cutoff = 0.05, sig_stat = "nom.p.val")
  g <- pelsa_woods_export_ggplot(pep, features = data.frame(), prot_len = 20L,
                                 gene = "GNOM", accession = "ACC1", contrast = "C1",
                                 sig_cutoff = 0.05, sig_stat = "nom.p.val")
  cap <- g$labels$caption
  expect_match(cap, "nom\\.P", perl = TRUE)
  expect_false(grepl("adj\\.P", cap))
  expect_equal(.woods_star_rows(g), 1L)
})

test_that("woods export ggplot: adj.p.val (default) stars nothing + labels adj.P", {
  pep <- pelsa_woods_peptide_data("ACC1", .sig_stat_matched(), .sig_stat_df(),
                                  "C1", sig_cutoff = 0.05)
  g <- pelsa_woods_export_ggplot(pep, features = data.frame(), prot_len = 20L,
                                 gene = "GNOM", accession = "ACC1", contrast = "C1",
                                 sig_cutoff = 0.05)
  cap <- g$labels$caption
  expect_match(cap, "adj\\.P", perl = TRUE)
  # discriminating peptide is NOT significant under adj.p.val -> no star
  expect_equal(.woods_star_rows(g), 0L)
})

test_that("woods export ggplot: falls back to adj.P.Val when no sig column", {
  # A peptides frame WITHOUT a `sig` column exercises the recompute fallback.
  pep <- data.frame(
    peptide_seq = c("A", "B"), pep_start = c(1L, 5L), pep_end = c(4L, 9L),
    logFC = c(2.0, -2.0), adj.P.Val = c(0.01, 0.40),  # only A passes adj < 0.05
    stringsAsFactors = FALSE
  )
  g <- pelsa_woods_export_ggplot(pep, features = data.frame(), prot_len = 20L,
                                 gene = "G", accession = "ACC1", contrast = "C1",
                                 sig_cutoff = 0.05)
  expect_equal(.woods_star_rows(g), 1L)
})

test_that("woods export ggplot: NA sig values are not starred (%in% TRUE)", {
  pep <- data.frame(
    peptide_seq = c("A", "B", "C"), pep_start = c(1L, 5L, 9L),
    pep_end = c(4L, 8L, 12L), logFC = c(2, -2, 1),
    adj.P.Val = c(0.01, 0.40, NA), sig = c(TRUE, NA, FALSE),
    stringsAsFactors = FALSE
  )
  g <- pelsa_woods_export_ggplot(pep, features = data.frame(), prot_len = 20L,
                                 gene = "G", accession = "ACC1", contrast = "C1",
                                 sig_cutoff = 0.05, sig_stat = "nom.p.val")
  expect_equal(.woods_star_rows(g), 1L)  # only the TRUE row, NA excluded
})

# --- pelsa_intensity_line_data: the remaining sig_stat consumer ---------------

# show_all = TRUE retains the peptide either way, so the `panel` label cleanly
# reflects the sig_stat-driven significance of the discriminating peptide.
# pelsa_intensity_line_data requires pep_occurrence_idx on matched_cache and
# aligns rows via `.row_id` -> processed_mat row index.
.intensity_args <- function() {
  pm <- matrix(c(10, 11, 20, 21), nrow = 1,
               dimnames = list("PEPNOM", c("s1", "s2", "s3", "s4")))
  matched <- .sig_stat_matched()
  matched$pep_occurrence_idx <- 1L
  list(stat = .sig_stat_df(), matched = matched, pm = pm,
       cmap = stats::setNames(c("A", "A", "B", "B"), c("s1", "s2", "s3", "s4")))
}

test_that("intensity line data: nom.p.val labels the peptide Significant", {
  a <- .intensity_args()
  ld <- pelsa_intensity_line_data("ACC1", a$stat, a$matched, a$pm, a$cmap,
                                  condition_order = c("A", "B"), contrast = "C1",
                                  sig_cutoff = 0.05, show_all = TRUE,
                                  sig_stat = "nom.p.val")
  expect_true(nrow(ld) > 0L)
  expect_true(all(ld$panel %in% "Significant"))
})

test_that("intensity line data: adj.p.val (default) labels it Non-significant", {
  a <- .intensity_args()
  ld <- pelsa_intensity_line_data("ACC1", a$stat, a$matched, a$pm, a$cmap,
                                  condition_order = c("A", "B"), contrast = "C1",
                                  sig_cutoff = 0.05, show_all = TRUE)
  expect_true(nrow(ld) > 0L)
  expect_true(all(ld$panel %in% "Non-significant"))
})
