################################################################################
# Map feature IDs in rdesc columns to gene symbols via org.*.eg.db
# Legacy logic adapted from broadinstitute/protigy global.R::mapIDs (mapIds + try).
################################################################################

#' Split a single cell into ID tokens (common proteomics delimiters).
#' @noRd
tokenize_id_cell <- function(x) {
  if (is.null(x) || length(x) != 1L) {
    return(character(0))
  }
  x <- as.character(x)
  if (is.na(x) || !nzchar(trimws(x))) {
    return(character(0))
  }
  parts <- unlist(strsplit(x, "\\|", fixed = FALSE))
  parts <- unlist(strsplit(parts, ";", fixed = TRUE))
  parts <- unlist(strsplit(parts, ",", fixed = TRUE))
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

#' All unique ID tokens from an rdesc column (after splitting on delimiters).
#' @noRd
unique_tokens_from_rdesc_column <- function(rdesc, id_column) {
  if (!id_column %in% names(rdesc)) {
    return(character(0))
  }
  col <- rdesc[[id_column]]
  if (is.list(col)) {
    col <- vapply(col, function(x) paste(as.character(x), collapse = "|"), character(1))
  }
  col <- as.character(col)
  unique(unlist(lapply(col, tokenize_id_cell), use.names = FALSE))
}

#' Deep-enough copy of rdesc row metadata for safe revert after failed mapping.
#' @noRd
safe_copy_rdesc <- function(rdesc) {
  out <- as.data.frame(rdesc, stringsAsFactors = FALSE)
  rownames(out) <- rownames(rdesc)
  out
}

#' Turn off ID conversion and clear related setup fields (after skip or failure).
#' @noRd
disable_id_conversion_in_params <- function(params) {
  params$convert_ids_to_gene_symbol <- FALSE
  params$id_source_column <- ""
  params$id_mapping_keytype <- NULL
  params$id_mapping_n_total <- NULL
  params$id_mapping_n_unmapped <- NULL
  params
}

#' Resolve species label to org.*.eg.db object.
#' @noRd
org_db_for_species <- function(species) {
  sp <- as.character(species)[1]
  if (is.na(sp) || !nzchar(sp)) {
    stop("Species must be set for ID-to-gene-symbol mapping.")
  }
  if (sp %in% c("Homo sapiens", "human", "Hs", "hs")) {
    return(org.Hs.eg.db::org.Hs.eg.db)
  }
  if (sp %in% c("Mus musculus", "mouse", "Mm", "mm")) {
    return(org.Mm.eg.db::org.Mm.eg.db)
  }
  stop("Unsupported species for ID mapping: ", sp, ". Use Homo sapiens or Mus musculus.")
}

#' Detect AnnotationDbi keytype from ID strings (Protigy v1 global.R mapIDs).
#' Uses sequential `if` assignments so later rules overwrite earlier ones (e.g. ENSP…
#' matches `^E` for UniProt-style but must still resolve to ENSEMBLPROT).
#' @noRd
protigy_legacy_detect_keytype <- function(ids) {
  ids <- trimws(as.character(ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0L) {
    return("UNKNOWN")
  }
  keytype <- "UNKNOWN"
  if (length(grep("^(Q|P|O|A|E|H|F)", ids)) > 0L) {
    keytype <- "UNIPROT"
  }
  if (length(grep("^(NP_|NM_|NR_|NC_|NG_|NW_|NZ_|NT_|AC_|XM_|XR_|XP_|YP_|WP_)", ids)) > 0L) {
    keytype <- "REFSEQ"
  }
  if (length(grep("ENSP", ids)) > 0L) {
    keytype <- "ENSEMBLPROT"
  }
  if (length(grep("^ENSG[0-9]+", ids)) > 0L) {
    keytype <- "ENSEMBL"
  }
  if (length(ids) > 0L && all(grepl("^[0-9]+$", ids))) {
    keytype <- "ENTREZID"
  }
  keytype
}

#' Strip IDs to query keys (same rules as Protigy v1 mapIDs).
#' @noRd
protigy_legacy_id_query <- function(ids, keytype) {
  ids <- as.character(ids)
  if (keytype == "UNIPROT") {
    sub("(-|;|\\.|_|\\|).*", "", ids)
  } else if (keytype %in% c("REFSEQ", "ENSEMBLPROT", "ENSEMBL")) {
    sub("(\\.|;).*", "", ids)
  } else {
    trimws(ids)
  }
}

#' Map a character vector of row IDs to gene symbols using one keytype + mapIds (Protigy-style).
#'
#' @return `list(symbols = character, keytype = character, n_total = int, n_unmapped = int)`.
#' @noRd
protigy_legacy_map_ids_to_symbols <- function(ids, species) {
  n <- length(ids)
  ids <- as.character(ids)
  keytype <- protigy_legacy_detect_keytype(ids)
  if (keytype == "UNKNOWN") {
    return(list(
      symbols = rep(NA_character_, n),
      keytype = keytype,
      n_total = n,
      n_unmapped = n
    ))
  }
  org_db <- org_db_for_species(species)
  if (!(keytype %in% AnnotationDbi::keytypes(org_db))) {
    return(list(
      symbols = rep(NA_character_, n),
      keytype = keytype,
      n_total = n,
      n_unmapped = n
    ))
  }

  idq <- toupper(trimws(protigy_legacy_id_query(ids, keytype)))
  idq[is.na(idq) | !nzchar(idq)] <- NA_character_

  symbols <- rep(NA_character_, n)
  valid <- !is.na(idq) & nzchar(idq)
  if (!any(valid)) {
    return(list(
      symbols = symbols,
      keytype = keytype,
      n_total = n,
      n_unmapped = n
    ))
  }

  ukeys <- unique(idq[valid])
  mapped <- suppressWarnings(
    try(
      AnnotationDbi::mapIds(
        org_db,
        keys = ukeys,
        column = "SYMBOL",
        keytype = keytype,
        multiVals = "first",
        ifNotFound = NA
      ),
      silent = TRUE
    )
  )
  if (inherits(mapped, "try-error")) {
    return(list(
      symbols = symbols,
      keytype = keytype,
      n_total = n,
      n_unmapped = n
    ))
  }
  # as.character(mapIds(...)) drops names; keep them for keyed lookup
  nm_map <- names(mapped)
  mapped <- as.character(mapped)
  if (!is.null(nm_map) && length(nm_map) == length(mapped)) {
    names(mapped) <- nm_map
  } else if (length(mapped) == length(ukeys)) {
    names(mapped) <- ukeys
  }
  symbols[valid] <- unname(mapped[as.character(idq[valid])])
  symbols[is.na(symbols) | symbols == ""] <- NA_character_
  n_unmapped <- sum(is.na(symbols) | !nzchar(symbols))
  list(
    symbols = symbols,
    keytype = keytype,
    n_total = n,
    n_unmapped = as.integer(n_unmapped)
  )
}

#' Copy existing geneSymbol to a collision-safe backup column before overwriting.
#' @noRd
preserve_gene_symbol_for_id_mapping <- function(rdesc) {
  if (!"geneSymbol" %in% names(rdesc)) {
    return(rdesc)
  }
  base <- "geneSymbol_original"
  nm <- base
  existing <- names(rdesc)
  i <- 1L
  while (nm %in% existing) {
    nm <- paste0(base, "_", i)
    i <- i + 1L
  }
  rdesc[[nm]] <- rdesc$geneSymbol
  rdesc
}

#' Map IDs from `id_column` to `geneSymbol` using legacy Protigy mapIDs / mapIds logic.
#'
#' @return `list(rdesc, id_mapping_keytype, id_mapping_n_total, id_mapping_n_unmapped)`.
#' @noRd
map_rdesc_ids_to_gene_symbols <- function(rdesc, id_column, species) {
  if (!id_column %in% names(rdesc)) {
    stop("ID source column '", id_column, "' not found in row metadata.")
  }
  col <- rdesc[[id_column]]
  if (is.list(col)) {
    col <- vapply(col, function(x) paste(as.character(x), collapse = "|"), character(1))
  }
  col <- as.character(col)

  out <- protigy_legacy_map_ids_to_symbols(col, species)
  rdesc$geneSymbol <- out$symbols
  list(
    rdesc = rdesc,
    id_mapping_keytype = out$keytype,
    id_mapping_n_total = out$n_total,
    id_mapping_n_unmapped = out$n_unmapped
  )
}

#' @return `list(rdesc = rdesc, params = params)`.
#' @noRd
apply_gene_symbol_from_params <- function(rdesc, params, ome) {
  gene_symbol_col <- params$gene_symbol_column
  convert_on <- isTRUE(params$convert_ids_to_gene_symbol)

  if (convert_on && identical(gene_symbol_col, "None")) {
    id_src <- params$id_source_column
    if (is.null(id_src) || !nzchar(id_src)) {
      stop("Convert IDs to gene symbols is enabled but no ID source column was selected for ", ome, ".")
    }
    species <- params$id_mapping_species
    if (is.null(species) || !nzchar(as.character(species)[1])) {
      species <- "Homo sapiens"
    } else {
      species <- as.character(species)[1]
    }

    tokens <- unique_tokens_from_rdesc_column(rdesc, id_src)
    if (length(tokens) == 0) {
      message(
        "Gene symbol ID conversion skipped for dataset ", ome, ": ",
        "no ID tokens in column \"", id_src, "\". ",
        "Convert IDs to gene symbols was turned off."
      )
      return(list(rdesc = rdesc, params = disable_id_conversion_in_params(params)))
    }

    rdesc_backup <- safe_copy_rdesc(rdesc)
    rdesc <- safe_copy_rdesc(rdesc)
    rdesc <- preserve_gene_symbol_for_id_mapping(rdesc)
    map_out <- map_rdesc_ids_to_gene_symbols(rdesc, id_src, species)
    rdesc <- map_out$rdesc
    params$id_mapping_keytype <- map_out$id_mapping_keytype
    params$id_mapping_n_total <- map_out$id_mapping_n_total
    params$id_mapping_n_unmapped <- map_out$id_mapping_n_unmapped

    gs <- rdesc$geneSymbol
    if (length(gs) == 0L || all(is.na(gs) | !nzchar(gs))) {
      message(
        "Gene symbol ID conversion skipped for dataset ", ome, ": ",
        "no gene symbols resolved from \"", id_src, "\" (keytype was ",
        map_out$id_mapping_keytype, "). ",
        "Convert IDs to gene symbols was turned off."
      )
      return(list(rdesc = rdesc_backup, params = disable_id_conversion_in_params(params)))
    }

    n_tot <- map_out$id_mapping_n_total
    n_bad <- map_out$id_mapping_n_unmapped
    n_ok <- n_tot - n_bad
    message(
      "Dataset ", ome, ": ID mapping used AnnotationDbi keytype ",
      map_out$id_mapping_keytype, ". ",
      n_ok, "/", n_tot, " rows mapped to gene symbols; ",
      n_bad, " row(s) could not be converted."
    )

    return(list(rdesc = rdesc, params = params))
  }
  if ("geneSymbol" %in% names(rdesc)) {
    if (!is.null(gene_symbol_col) && gene_symbol_col != "None" &&
        gene_symbol_col != "geneSymbol" && gene_symbol_col %in% names(rdesc)) {
      message(
        "Gene symbol column already exists; original preserved as geneSymbol_original."
      )
      rdesc$geneSymbol_original <- rdesc$geneSymbol
      rdesc$geneSymbol <- rdesc[[gene_symbol_col]]
    }
  } else if (!is.null(gene_symbol_col) && gene_symbol_col != "None" && gene_symbol_col %in% names(rdesc)) {
    rdesc$geneSymbol <- rdesc[[gene_symbol_col]]
  }
  list(rdesc = rdesc, params = params)
}
