


test_that("merge works on full brca test dataset", {
  # make the example GCTs object
  GCTs <- list(
    RNA = brca_retrospective_v5.0_rnaseq_gct,
    Phos = brca_retrospective_v5.0_phosphoproteome_gct,
    Prot = brca_retrospective_v5.0_proteome_gct#,
    #RNA = brca_retrospective_v5.0_rnaseq_gct
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
    
    # check that rdesc's match original gct
    expect_equal(GCTs$Phos@rdesc, phos_subset@rdesc[, names(GCTs$Phos@rdesc)])
    expect_equal(GCTs$Prot@rdesc, prot_subset@rdesc[, names(GCTs$Prot@rdesc)])
    expect_equal(GCTs$RNA@rdesc, rna_subset@rdesc[, names(GCTs$RNA@rdesc)])
    
    # check that the mat's match original gct
    expect_equal(GCTs$Phos@mat, phos_subset@mat[GCTs$Phos@rid, GCTs$Phos@cid])
    expect_equal(GCTs$Prot@mat, prot_subset@mat[GCTs$Prot@rid, GCTs$Prot@cid])
    expect_equal(GCTs$RNA@mat, rna_subset@mat[GCTs$RNA@rid, GCTs$RNA@cid])
    
    # check that cdesc match original gct for Phos (has known conflicts)
    phos_cdesc_cols <- c(
      setdiff(names(GCTs$Phos@cdesc), c("normalization.center", "normalization.scale")),
      "normalization.center.Phos", "normalization.scale.Phos"
    )
    phos_cdesc_from_merged <- phos_subset@cdesc[GCTs$Phos@cid, phos_cdesc_cols]
    names(phos_cdesc_from_merged) <- c(phos_cdesc_cols[1:16], 
                                       "normalization.center", 
                                       "normalization.scale")
    expect_equal(GCTs$Phos@cdesc, phos_cdesc_from_merged)
    
    # check that cdesc match original gct for Prot (has known conflicts)
    prot_cdesc_cols <- c(
      setdiff(names(GCTs$Prot@cdesc), c("normalization.center", "normalization.scale")),
      "normalization.center.Prot", "normalization.scale.Prot"
    )
    prot_cdesc_from_merged <- prot_subset@cdesc[GCTs$Prot@cid, prot_cdesc_cols]
    names(prot_cdesc_from_merged) <- c(prot_cdesc_cols[1:16], 
                                       "normalization.center", 
                                       "normalization.scale")
    expect_equal(GCTs$Prot@cdesc, prot_cdesc_from_merged)
    
    # check that cdesc matches original gct for RNA (no known conflicts
    expect_equal(GCTs$RNA@cdesc, rna_subset@cdesc[GCTs$RNA@cid, names(GCTs$RNA@cdesc)])
    
  })
})

