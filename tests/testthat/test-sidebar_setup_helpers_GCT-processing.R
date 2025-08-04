
# load test data
data(brca_retrospective_v5.0_rnaseq_gct)
data(brca_retrospective_v5.0_phosphoproteome_gct)
data(brca_retrospective_v5.0_proteome_gct)

test_that("merge works on full brca test dataset", {
  # make the example GCTs object
  GCTs <- list(
    RNA = brca_retrospective_v5.0_rnaseq_gct,
    Phos = brca_retrospective_v5.0_phosphoproteome_gct,
    Prot = brca_retrospective_v5.0_proteome_gct
  )
  
  # make a fake server to simulate interactive enviornment
  server <- function(input, output, session) {
    GCTs_merged <- merge_processed_gcts(GCTs_processed = GCTs)
  }
  
  # use testServer to run the server
  testServer(server, {
    expect_true("protigy.ome" %in% names(GCTs_merged@rdesc))
    
    # get subsets from merged of the original gcts
    phos_subset <- cmapR::subset_gct(GCTs_merged, rid = which(GCTs_merged@rdesc$protigy.ome == "Phos"))
    rna_subset <- cmapR::subset_gct(GCTs_merged, rid = which(GCTs_merged@rdesc$protigy.ome == "RNA"))
    prot_subset <- cmapR::subset_gct(GCTs_merged, rid = which(GCTs_merged@rdesc$protigy.ome == "Prot"))
    
    # check that rdesc's match original gct (excluding org_id and id columns and ignoring rownames)
    expect_equal(GCTs$Phos@rdesc[, setdiff(names(GCTs$Phos@rdesc), c("org_id", "id"))], phos_subset@rdesc[, setdiff(names(GCTs$Phos@rdesc), c("org_id", "id"))], ignore_attr = "row.names")
    expect_equal(GCTs$Prot@rdesc[, setdiff(names(GCTs$Prot@rdesc), c("org_id", "id"))], prot_subset@rdesc[, setdiff(names(GCTs$Prot@rdesc), c("org_id", "id"))], ignore_attr = "row.names")
    expect_equal(GCTs$RNA@rdesc[, setdiff(names(GCTs$RNA@rdesc), c("org_id", "id"))], rna_subset@rdesc[, setdiff(names(GCTs$RNA@rdesc), c("org_id", "id"))], ignore_attr = "row.names")
    
    # check that the mat's match original gct (ignoring dimnames)
    expect_equal(GCTs$Phos@mat, phos_subset@mat, ignore_attr = "dimnames")
    expect_equal(GCTs$Prot@mat, prot_subset@mat, ignore_attr = "dimnames")
    expect_equal(GCTs$RNA@mat, rna_subset@mat, ignore_attr = "dimnames")
    
    # check that cdesc match original gct for Phos (has known conflicts)
    phos_cdesc_cols <- intersect(
      c(names(GCTs$Phos@cdesc), paste0(names(GCTs$Phos@cdesc), ".Phos")),
      names(phos_subset@cdesc)
    )
    phos_cdesc_from_merged <- phos_subset@cdesc[GCTs$Phos@cid, phos_cdesc_cols]
    names(phos_cdesc_from_merged) <- gsub("\\.Phos$", "", phos_cdesc_cols)
    expect_setequal(names(phos_cdesc_from_merged), names(GCTs$Phos@cdesc))
    expect_equal(GCTs$Phos@cdesc, phos_cdesc_from_merged[names(GCTs$Phos@cdesc)])
    
    # check that cdesc match original gct for Prot (has known conflicts)
    prot_cdesc_cols <- intersect(
      c(names(GCTs$Prot@cdesc), paste0(names(GCTs$Prot@cdesc), ".Prot")),
      names(prot_subset@cdesc)
    )
    prot_cdesc_from_merged <- prot_subset@cdesc[GCTs$Prot@cid, prot_cdesc_cols]
    names(prot_cdesc_from_merged) <- gsub("\\.Prot$", "", prot_cdesc_cols)
    expect_setequal(names(prot_cdesc_from_merged), names(GCTs$Prot@cdesc))
    expect_equal(GCTs$Prot@cdesc, prot_cdesc_from_merged[names(GCTs$Prot@cdesc)])
    
    # check that cdesc matches original gct for RNA (no known conflicts)
    expect_equal(GCTs$RNA@cdesc, rna_subset@cdesc[GCTs$RNA@cid, names(GCTs$RNA@cdesc)])
  })
})

test_that("merge works on full brca test dataset in reverse order", {
  # make the example GCTs object
  GCTs <- list(
    Prot = brca_retrospective_v5.0_proteome_gct,
    Phos = brca_retrospective_v5.0_phosphoproteome_gct,
    RNA = brca_retrospective_v5.0_rnaseq_gct
  )
  
  # make a fake server to simulate interactive enviornment
  server <- function(input, output, session) {
    GCTs_merged <- merge_processed_gcts(GCTs_processed = GCTs)
  }
  
  # use testServer to run the server
  testServer(server, {
    expect_true("protigy.ome" %in% names(GCTs_merged@rdesc))
    
    # get subsets from merged of the original gcts
    phos_subset <- cmapR::subset_gct(GCTs_merged, rid = which(GCTs_merged@rdesc$protigy.ome == "Phos"))
    rna_subset <- cmapR::subset_gct(GCTs_merged, rid = which(GCTs_merged@rdesc$protigy.ome == "RNA"))
    prot_subset <- cmapR::subset_gct(GCTs_merged, rid = which(GCTs_merged@rdesc$protigy.ome == "Prot"))
    
    # check that rdesc's match original gct (excluding org_id and id columns and ignoring rownames)
    expect_equal(GCTs$Phos@rdesc[, setdiff(names(GCTs$Phos@rdesc), c("org_id", "id"))], phos_subset@rdesc[, setdiff(names(GCTs$Phos@rdesc), c("org_id", "id"))], ignore_attr = "row.names")
    expect_equal(GCTs$Prot@rdesc[, setdiff(names(GCTs$Prot@rdesc), c("org_id", "id"))], prot_subset@rdesc[, setdiff(names(GCTs$Prot@rdesc), c("org_id", "id"))], ignore_attr = "row.names")
    expect_equal(GCTs$RNA@rdesc[, setdiff(names(GCTs$RNA@rdesc), c("org_id", "id"))], rna_subset@rdesc[, setdiff(names(GCTs$RNA@rdesc), c("org_id", "id"))], ignore_attr = "row.names")
    
    # check that the mat's match original gct (ignoring dimnames)
    expect_equal(GCTs$Phos@mat, phos_subset@mat, ignore_attr = "dimnames")
    expect_equal(GCTs$Prot@mat, prot_subset@mat, ignore_attr = "dimnames")
    expect_equal(GCTs$RNA@mat, rna_subset@mat, ignore_attr = "dimnames")
    
    # check that cdesc match original gct for Phos (has known conflicts)
    phos_cdesc_cols <- intersect(
      c(names(GCTs$Phos@cdesc), paste0(names(GCTs$Phos@cdesc), ".Phos")),
      names(phos_subset@cdesc)
    )
    phos_cdesc_from_merged <- phos_subset@cdesc[GCTs$Phos@cid, phos_cdesc_cols]
    names(phos_cdesc_from_merged) <- gsub("\\.Phos$", "", phos_cdesc_cols)
    expect_setequal(names(phos_cdesc_from_merged), names(GCTs$Phos@cdesc))
    expect_equal(GCTs$Phos@cdesc, phos_cdesc_from_merged[names(GCTs$Phos@cdesc)])
    
    # check that cdesc match original gct for Prot (has known conflicts)
    prot_cdesc_cols <- intersect(
      c(names(GCTs$Prot@cdesc), paste0(names(GCTs$Prot@cdesc), ".Prot")),
      names(prot_subset@cdesc)
    )
    prot_cdesc_from_merged <- prot_subset@cdesc[GCTs$Prot@cid, prot_cdesc_cols]
    names(prot_cdesc_from_merged) <- gsub("\\.Prot$", "", prot_cdesc_cols)
    expect_setequal(names(prot_cdesc_from_merged), names(GCTs$Prot@cdesc))
    expect_equal(GCTs$Prot@cdesc, prot_cdesc_from_merged[names(GCTs$Prot@cdesc)])
    
    # check that cdesc matches original gct for RNA (no known conflicts)
    expect_equal(GCTs$RNA@cdesc, rna_subset@cdesc[GCTs$RNA@cid, names(GCTs$RNA@cdesc)])
  })
})

test_that("merge works on a single GCT", {
  # make the example GCTs object
  GCTs <- list(
    Prot = brca_retrospective_v5.0_proteome_gct
  )
  
  # make a fake server to simulate interactive enviornment
  server <- function(input, output, session) {
    GCTs_merged <- merge_processed_gcts(GCTs_processed = GCTs)
  }
  
  # use testServer to run the server
  testServer(server, {
    expect_true("protigy.ome" %in% names(GCTs_merged@rdesc))
    
    expect_equal(GCTs_merged@mat, GCTs$Prot@mat)
    expect_equal(GCTs_merged@cdesc, GCTs$Prot@cdesc)
    expect_equal(
      GCTs_merged@rdesc[, setdiff(names(GCTs_merged@rdesc), "protigy.ome")], 
      GCTs$Prot@rdesc
    )
  })
})