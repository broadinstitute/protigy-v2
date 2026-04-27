test_that("tokenize_id_cell splits delimiters", {
  expect_identical(Protigy:::tokenize_id_cell("A|B;C, D"), c("A", "B", "C", "D"))
  expect_identical(Protigy:::tokenize_id_cell(""), character(0))
})

test_that("protigy_legacy_detect_keytype matches Protigy v1 order (UniProt char, RefSeq, ENSP, ENSG, Entrez)", {
  expect_identical(Protigy:::protigy_legacy_detect_keytype("P04637"), "UNIPROT")
  expect_identical(Protigy:::protigy_legacy_detect_keytype("NP_000305"), "REFSEQ")
  expect_identical(Protigy:::protigy_legacy_detect_keytype("NM_000314"), "REFSEQ")
  expect_identical(Protigy:::protigy_legacy_detect_keytype("ENSP00000269305"), "ENSEMBLPROT")
  expect_identical(Protigy:::protigy_legacy_detect_keytype("ENSG00000141510"), "ENSEMBL")
  expect_identical(Protigy:::protigy_legacy_detect_keytype(c("7157", "7158")), "ENTREZID")
  expect_identical(Protigy:::protigy_legacy_detect_keytype("foo"), "UNKNOWN")
  expect_identical(Protigy:::protigy_legacy_detect_keytype("NP_000468.1|P00883"), "REFSEQ")
})

test_that("protigy_legacy_id_query strips like Protigy mapIDs", {
  expect_identical(Protigy:::protigy_legacy_id_query("P04637.1", "UNIPROT"), "P04637")
  expect_identical(Protigy:::protigy_legacy_id_query("NP_000305.2", "REFSEQ"), "NP_000305")
  expect_identical(Protigy:::protigy_legacy_id_query("NP_000468.1|P00883", "REFSEQ"), "NP_000468")
  expect_identical(Protigy:::protigy_legacy_id_query("ENSG00000141510.17", "ENSEMBL"), "ENSG00000141510")
})

test_that("protigy_legacy_protein_site_display_id_one keeps ProteinID_siteID (legacy keytype rules)", {
  expect_identical(
    Protigy:::protigy_legacy_protein_site_display_id_one("NP_000468.1_K28k_1_1_28_28"),
    "NP_000468.1_K28k"
  )
  expect_identical(
    Protigy:::protigy_legacy_protein_site_display_id_one("P04637_K28k_garbage"),
    "P04637_K28k"
  )
  expect_identical(
    Protigy:::protigy_legacy_protein_site_display_id_one("ENSP00000269305_K28k_1_2"),
    "ENSP00000269305_K28k"
  )
  expect_identical(
    Protigy:::protigy_legacy_protein_site_display_id_one("ENSG00000141510.17_site1_rest"),
    "ENSG00000141510.17_site1"
  )
  expect_identical(Protigy:::protigy_legacy_protein_site_display_id_one("plain_row_id"), "plain_row_id")
})

test_that("preserve_gene_symbol_for_id_mapping uses collision-safe column name", {
  rdesc <- data.frame(
    geneSymbol = c("TP53", "EGFR"),
    id = c("a", "b"),
    row.names = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  rdesc$geneSymbol_original <- c("x", "y")
  out <- Protigy:::preserve_gene_symbol_for_id_mapping(rdesc)
  expect_true("geneSymbol_original_1" %in% names(out))
  expect_identical(out$geneSymbol_original_1, c("TP53", "EGFR"))
})

test_that("org_db_for_species accepts common aliases", {
  skip_if_not_installed("org.Hs.eg.db")
  expect_s4_class(Protigy:::org_db_for_species("Homo sapiens"), "OrgDb")
  expect_s4_class(Protigy:::org_db_for_species("hs"), "OrgDb")
  skip_if_not_installed("org.Mm.eg.db")
  expect_s4_class(Protigy:::org_db_for_species("Mus musculus"), "OrgDb")
})

test_that("protigy_legacy_map_ids_to_symbols maps RefSeq NP for human", {
  skip_if_not_installed("org.Hs.eg.db")
  out <- Protigy:::protigy_legacy_map_ids_to_symbols(c("NP_000468.1", "NP_000305"), "Homo sapiens")
  expect_identical(out$keytype, "REFSEQ")
  expect_true(grepl("ALB", out$symbols[1], ignore.case = TRUE))
  expect_true(grepl("PTEN", out$symbols[2], ignore.case = TRUE))
})

test_that("protigy_legacy_map_ids_to_symbols maps UniProt for human", {
  skip_if_not_installed("org.Hs.eg.db")
  out <- Protigy:::protigy_legacy_map_ids_to_symbols("P04637.1", "Homo sapiens")
  expect_identical(out$keytype, "UNIPROT")
  expect_true(grepl("TP53", out$symbols[1], ignore.case = TRUE))
})

test_that("protigy_legacy_map_ids_to_symbols maps Ensembl gene IDs (ENSG) for human", {
  skip_if_not_installed("org.Hs.eg.db")
  out <- Protigy:::protigy_legacy_map_ids_to_symbols(
    c("ENSG00000141510.17", "ENSG00000171862"),
    "Homo sapiens"
  )
  expect_identical(out$keytype, "ENSEMBL")
  expect_identical(out$n_total, 2L)
  expect_true(grepl("TP53", out$symbols[1], ignore.case = TRUE))
  expect_true(grepl("PTEN", out$symbols[2], ignore.case = TRUE))
  expect_identical(out$n_unmapped, 0L)
})

test_that("map_rdesc_ids_to_gene_symbols returns keytype and fills geneSymbol", {
  skip_if_not_installed("org.Hs.eg.db")
  rdesc <- data.frame(
    accession_numbers = c("NP_000468.1", "NP_000305"),
    row.names = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  mo <- Protigy:::map_rdesc_ids_to_gene_symbols(rdesc, "accession_numbers", "Homo sapiens")
  expect_identical(mo$id_mapping_keytype, "REFSEQ")
  expect_identical(mo$id_mapping_n_total, 2L)
  expect_identical(mo$id_mapping_n_unmapped, 0L)
  expect_true(grepl("ALB", mo$rdesc$geneSymbol[1], ignore.case = TRUE))
})

test_that("apply_gene_symbol_from_params maps accession_numbers RefSeq column (acetylome-style)", {
  skip_if_not_installed("org.Hs.eg.db")
  rdesc <- data.frame(
    accession_numbers = c("NP_000468.1|P00883", "NP_000468.1|P00883"),
    row.names = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  params <- list(
    gene_symbol_column = "None",
    convert_ids_to_gene_symbol = TRUE,
    id_source_column = "accession_numbers",
    id_mapping_species = "Homo sapiens"
  )
  out <- Protigy:::apply_gene_symbol_from_params(rdesc, params, "acetyl")
  expect_true(out$params$convert_ids_to_gene_symbol)
  expect_identical(out$params$id_mapping_keytype, "REFSEQ")
  expect_identical(out$params$id_mapping_n_total, 2L)
  expect_identical(out$params$id_mapping_n_unmapped, 0L)
  expect_true(all(grepl("ALB", out$rdesc$geneSymbol, ignore.case = TRUE)))
})

test_that("apply_gene_symbol_from_params disables conversion when mapping yields no symbols", {
  rdesc <- data.frame(
    id = c("foo", "bar__baz"),
    row.names = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  params <- list(
    gene_symbol_column = "None",
    convert_ids_to_gene_symbol = TRUE,
    id_source_column = "id",
    id_mapping_species = "Homo sapiens"
  )
  out <- Protigy:::apply_gene_symbol_from_params(rdesc, params, "proteome")
  expect_false(out$params$convert_ids_to_gene_symbol)
  expect_identical(out$params$id_source_column, "")
  expect_false("geneSymbol" %in% names(out$rdesc))
})

test_that("apply_gene_symbol_from_params disables conversion when column has no tokens", {
  rdesc <- data.frame(
    id = c("", "   "),
    row.names = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  params <- list(
    gene_symbol_column = "None",
    convert_ids_to_gene_symbol = TRUE,
    id_source_column = "id",
    id_mapping_species = "Homo sapiens"
  )
  out <- Protigy:::apply_gene_symbol_from_params(rdesc, params, "proteome")
  expect_false(out$params$convert_ids_to_gene_symbol)
})

test_that("apply_gene_symbol_from_params disables when only invalid UniProt-shaped ids", {
  skip_if_not_installed("org.Hs.eg.db")
  rdesc <- data.frame(
    id = c("P00000", "P00000"),
    row.names = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  params <- list(
    gene_symbol_column = "None",
    convert_ids_to_gene_symbol = TRUE,
    id_source_column = "id",
    id_mapping_species = "Homo sapiens"
  )
  out <- Protigy:::apply_gene_symbol_from_params(rdesc, params, "acetyl")
  expect_false(out$params$convert_ids_to_gene_symbol)
  expect_false("geneSymbol" %in% names(out$rdesc))
})
