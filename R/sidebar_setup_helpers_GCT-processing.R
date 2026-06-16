################################################################################
# Module: SETUP SIDEBAR
# The main processGCT()` function and it's helpers
################################################################################

# Fix gene symbol column formatting
# Replaces semicolons with pipes, converts blank symbols to NA, and cleans up formatting
# Blank geneSymbol values are converted to NA and kept (rows are not removed)
# INPUT: rdesc data frame with geneSymbol column
# OUTPUT: updated rdesc with fixed geneSymbol column, and empty vector (for backward compatibility)
fix_gene_symbols <- function(rdesc) {
  if (!"geneSymbol" %in% names(rdesc)) {
    return(list(rdesc = rdesc, removed_rids = character(0)))
  }
  
  # Store original row names (for backward compatibility - no rows are removed)
  original_rids <- rownames(rdesc)
  
  # Convert geneSymbol to character vector if it's a list or other type
  if (is.list(rdesc$geneSymbol)) {
    rdesc$geneSymbol <- unlist(lapply(rdesc$geneSymbol, function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      paste(as.character(x), collapse = "|")
    }))
  }
  rdesc$geneSymbol <- as.character(rdesc$geneSymbol)
  
  # Replace semicolons with pipes
  rdesc$geneSymbol <- gsub(";", "|", rdesc$geneSymbol)
  
  # Remove blank gene symbols within strings (e.g., "EGFR| |" -> "EGFR")
  # Split by pipe, remove empty/whitespace-only entries, rejoin with pipe
  rdesc$geneSymbol <- vapply(rdesc$geneSymbol, function(x) {
    if (is.na(x) || x == "") return(NA_character_)
    parts <- strsplit(x, "\\|", fixed = FALSE)[[1]]
    parts <- parts[trimws(parts) != ""]  # Remove blank/whitespace-only parts
    result <- paste(parts, collapse = "|")
    # If result is empty after cleaning, return NA instead of empty string
    if (result == "") return(NA_character_)
    return(result)
  }, character(1))
  
  # Convert any remaining blank gene symbols to NA (keep all rows)
  # Blank gene symbols are valid - convert to NA but don't remove rows
  rdesc$geneSymbol[rdesc$geneSymbol == ""] <- NA_character_
  
  # No rows should be removed - blank geneSymbol values are converted to NA
  removed_rids <- character(0)
  
  # Remove any starting | characters (only for non-NA values)
  if (nrow(rdesc) > 0) {
    non_na_mask <- !is.na(rdesc$geneSymbol)
    if (any(non_na_mask)) {
      start_str <- substring(rdesc$geneSymbol[non_na_mask], 1, 1)
      start_pipe_mask <- start_str == "|"
      if (any(start_pipe_mask)) {
        rdesc$geneSymbol[non_na_mask][start_pipe_mask] <- substring(
          rdesc$geneSymbol[non_na_mask][start_pipe_mask], 2
        )
        # Convert to NA if result is empty
        empty_after_start <- rdesc$geneSymbol[non_na_mask][start_pipe_mask] == ""
        if (any(empty_after_start)) {
          na_indices <- which(non_na_mask)[start_pipe_mask][empty_after_start]
          rdesc$geneSymbol[na_indices] <- NA_character_
        }
      }
      
      # Remove any ending | characters (only for non-NA values)
      non_na_mask <- !is.na(rdesc$geneSymbol)
      if (any(non_na_mask)) {
        gene_values <- rdesc$geneSymbol[non_na_mask]
        end_str <- vapply(gene_values, function(x) {
          if (nchar(x) > 0) substring(x, nchar(x), nchar(x)) else ""
        }, character(1))
        end_pipe_mask <- end_str == "|"
        if (any(end_pipe_mask)) {
          rdesc$geneSymbol[non_na_mask][end_pipe_mask] <- vapply(
            rdesc$geneSymbol[non_na_mask][end_pipe_mask], 
            function(x) {
              if (nchar(x) > 1) substring(x, 1, nchar(x) - 1) else ""
            }, 
            character(1)
          )
          # Convert to NA if result is empty
          empty_after_end <- rdesc$geneSymbol[non_na_mask][end_pipe_mask] == ""
          if (any(empty_after_end)) {
            na_indices <- which(non_na_mask)[end_pipe_mask][empty_after_end]
            rdesc$geneSymbol[na_indices] <- NA_character_
          }
        }
      }
    }
  }
  
  return(list(rdesc = rdesc, removed_rids = removed_rids))
}

# Apply sample-level filtering using cdesc column values.
# Selected values are always kept; all other values are discarded.
apply_sample_filter <- function(data, cdesc, params, ome) {
  if (!isTRUE(params$sample_filter_enabled)) {
    return(list(data = data, cdesc = cdesc))
  }

  filter_column <- params$sample_filter_column
  filter_values <- params$sample_filter_values
  if (is.null(filter_column) || identical(filter_column, "")) {
    stop("Sample filtering is enabled, but no sample filter column was selected.")
  }
  if (!(filter_column %in% names(cdesc))) {
    stop("Sample filter column '", filter_column, "' was not found in cdesc for ", ome, ".")
  }
  if (is.null(filter_values) || length(filter_values) == 0) {
    stop("Sample filtering is enabled, but no filter values were selected for ", ome, ".")
  }

  filter_values <- as.character(filter_values)
  keep_samples <- as.character(cdesc[[filter_column]]) %in% filter_values
  keep_ids <- rownames(cdesc)[keep_samples]
  if (length(keep_ids) == 0) {
    stop("No samples remain after filtering ", ome, " by ", filter_column, ".")
  }

  data <- data[, keep_ids, drop = FALSE]
  cdesc <- cdesc[keep_ids, , drop = FALSE]

  return(list(data = data, cdesc = cdesc))
}

# Apply row-level filtering using rdesc column values.
# Selected values are always kept; all other values are discarded.
apply_row_filter <- function(data, rdesc, params, ome) {
  if (!isTRUE(params$row_filter_enabled)) {
    return(list(data = data, rdesc = rdesc))
  }

  filter_column <- params$row_filter_column
  filter_values <- params$row_filter_values
  if (is.null(filter_column) || identical(filter_column, "")) {
    stop("Row filtering is enabled, but no row filter column was selected.")
  }
  if (!(filter_column %in% names(rdesc))) {
    stop("Row filter column '", filter_column, "' was not found in rdesc for ", ome, ".")
  }
  if (is.null(filter_values) || length(filter_values) == 0) {
    stop("Row filtering is enabled, but no filter values were selected for ", ome, ".")
  }

  filter_values <- as.character(filter_values)
  keep_rows <- as.character(rdesc[[filter_column]]) %in% filter_values
  keep_ids <- rownames(rdesc)[keep_rows]
  if (length(keep_ids) == 0) {
    stop("No rows remain after filtering ", ome, " by ", filter_column, ".")
  }

  data <- data[keep_ids, , drop = FALSE]
  rdesc <- rdesc[keep_ids, , drop = FALSE]

  return(list(data = data, rdesc = rdesc))
}

################################################################################
# Gene symbol column selection and ID -> gene symbol mapping (org.*.eg.db)
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

#' One string: `ProteinID_siteID_garbage` -> `ProteinID_siteID` for volcano display.
#' Uses `protigy_legacy_detect_keytype()` + accession rules aligned with
#' `protigy_legacy_id_query()` (UniProt prefix), and RefSeq / ENSP / ENSG regex
#' at string start so versions like `NP_000468.1` are kept.
#' @noRd
protigy_legacy_protein_site_display_id_one <- function(s) {
  s <- trimws(as.character(s))
  if (length(s) != 1L) s <- s[1L]
  if (is.na(s) || !nzchar(s)) return(s)
  # Same space/underscore normalisation as earlier volcano display logic
  repeat {
    t2 <- gsub("_\\s+(\\d+)", "_\\1", s, perl = TRUE)
    t2 <- gsub("\\s+(_\\d+)", "\\1", t2, perl = TRUE)
    if (identical(t2, s)) break
    s <- t2
  }

  kt <- protigy_legacy_detect_keytype(s)
  p  <- ""

  if (kt == "UNIPROT") {
    q <- protigy_legacy_id_query(s, "UNIPROT")
    if (nzchar(q) && startsWith(s, q)) p <- q
  } else if (kt == "REFSEQ") {
    m <- regexpr(
      "^(?:NP|NM|NR|NC|NG|NW|NZ|NT|AC|XM|XR|XP|YP|WP)_\\d+(?:\\.\\d+)?",
      s,
      perl = TRUE
    )
    if (!identical(m, -1L)) {
      ml <- attr(m, "match.length")[1L]
      if (!is.na(ml) && ml > 0L) p <- substr(s, 1L, ml)
    }
  } else if (kt == "ENSEMBLPROT") {
    m <- regexpr("^ENSP\\d+(?:\\.\\d+)?", s, perl = TRUE)
    if (!identical(m, -1L)) {
      ml <- attr(m, "match.length")[1L]
      if (!is.na(ml) && ml > 0L) p <- substr(s, 1L, ml)
    }
  } else if (kt == "ENSEMBL") {
    m <- regexpr("^ENSG\\d+(?:\\.\\d+)?", s, perl = TRUE)
    if (!identical(m, -1L)) {
      ml <- attr(m, "match.length")[1L]
      if (!is.na(ml) && ml > 0L) p <- substr(s, 1L, ml)
    }
  }

  if (!nzchar(p)) return(s)

  if (nchar(s) <= nchar(p)) return(p)

  rest <- substr(s, nchar(p) + 1L, nchar(s))
  if (!startsWith(rest, "_")) return(s)

  suffix <- substr(rest, 2L, nchar(rest))
  if (!nzchar(suffix)) return(p)

  brk <- regexpr("_", suffix, fixed = TRUE)[[1L]]
  if (is.na(brk) || brk < 1L) {
    site <- suffix
  } else {
    site <- substr(suffix, 1L, brk - 1L)
  }
  if (!nzchar(site)) return(p)

  trimws(paste0(p, "_", site))
}

#' Vectorized `protigy_legacy_protein_site_display_id_one()`.
#' @noRd
protigy_legacy_protein_site_display_id <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L) return(x)
  vapply(x, protigy_legacy_protein_site_display_id_one, character(1L), USE.NAMES = FALSE)
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

# Deep copy of a data.frame (row metadata) — avoids shared columns with the source object.
# @noRd
df_deep_copy <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }
  if (!is.data.frame(df)) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  }
  unserialize(serialize(df, connection = NULL))
}

# Full GCT copy for the processing pipeline so reactive uploads are never mutated.
# Preserves cmapR::GCT slots: mat, rid, cid, rdesc, cdesc, version, src.
# @noRd
deep_clone_gct <- function(gct) {
  m <- gct@mat
  d <- dim(m)
  m_cp <- matrix(as.vector(m), nrow = d[1L], ncol = d[2L], dimnames = dimnames(m))
  out <- cmapR::GCT(
    mat = m_cp,
    rdesc = df_deep_copy(gct@rdesc),
    cdesc = df_deep_copy(gct@cdesc)
  )
  out@rid <- gct@rid
  out@cid <- gct@cid
  out@version <- gct@version
  out@src <- gct@src
  out
}

# Remove backup columns created when remapping gene symbols (not part of the user's file).
# @noRd
strip_gene_symbol_mapping_columns <- function(rdesc) {
  if (is.null(rdesc) || !is.data.frame(rdesc)) {
    return(rdesc)
  }
  nm <- names(rdesc)
  drop <- nm[grepl("^geneSymbol_original", nm)]
  if (length(drop)) {
    rdesc <- rdesc[, setdiff(nm, drop), drop = FALSE]
  }
  rdesc
}

# For QC/export "original" GCTs: use upload rdesc (same row order as transformed mat) so
# geneSymbol_original and other pipeline-only columns never appear in exports.
# @noRd
repackage_transformed_gct_with_upload_rdesc <- function(gct_transformed, gct_upload) {
  if (is.null(gct_transformed) || is.null(gct_upload)) {
    return(gct_transformed)
  }
  rids <- rownames(gct_transformed@mat)
  ru <- df_deep_copy(gct_upload@rdesc)
  rn <- rownames(ru)
  if (!is.null(rn) && length(rids) > 0L && all(rids %in% rn)) {
    gct_transformed@rdesc <- ru[rids, , drop = FALSE]
  } else {
    gct_transformed@rdesc <- strip_gene_symbol_mapping_columns(gct_transformed@rdesc)
  }
  gct_transformed
}

# Split a tab-delimited line while preserving empty fields between delimiters.
# @noRd
split_tab_fields <- function(line) {
  strsplit(line, "\t", fixed = TRUE)[[1]]
}

# Parse .gct cdesc metadata as raw character values (no numeric coercion).
# This preserves annotation values like "001" exactly as provided in file.
# @noRd
read_gct_cdesc_as_character <- function(file_path) {
  # Only the header region is needed: line 1 (version), line 2 (dims), line 3
  # (column id row), then nchd cdesc rows. The data matrix that follows is never
  # used here, so read just the first (3 + nchd) lines instead of the whole file.
  con <- file(file_path, open = "r")
  on.exit(close(con), add = TRUE)

  head2 <- readLines(con, n = 2L, warn = FALSE)
  if (length(head2) < 2L) {
    stop("Invalid .gct file (expected at least 3 lines): ", file_path)
  }

  dims <- suppressWarnings(as.integer(split_tab_fields(head2[2L])))
  if (length(dims) < 2L || any(is.na(dims[1:2]))) {
    stop("Invalid .gct dimensions line: ", file_path)
  }
  ncmat <- dims[2L]
  nrhd <- if (length(dims) >= 3L && !is.na(dims[3L])) dims[3L] else 0L
  nchd <- if (length(dims) >= 4L && !is.na(dims[4L])) dims[4L] else 0L

  # Read the column-id header row plus the nchd cdesc metadata rows. readLines
  # stops early on EOF, so a truncated header yields a short `lines` vector that
  # trips the same guards below (we intentionally do not pad here).
  rest <- readLines(con, n = 1L + nchd, warn = FALSE)
  lines <- c(head2, rest)
  if (length(lines) < 3L) {
    stop("Invalid .gct file (expected at least 3 lines): ", file_path)
  }

  header <- split_tab_fields(lines[3L])
  if (nrhd > 0L) {
    cid <- header[(nrhd + 2L):length(header)]
  } else {
    has_description <- any(grepl("description", header, ignore.case = TRUE))
    col_offset <- if (has_description) 2L else 1L
    cid <- header[(col_offset + 1L):length(header)]
  }
  cid <- as.character(cid)
  if (length(cid) != ncmat) {
    warning(
      "Parsed cdesc sample count (", length(cid), ") does not match matrix sample count (",
      ncmat, ") for ", basename(file_path), "."
    )
  }

  if (nchd <= 0L) {
    cdesc <- data.frame(id = cid, stringsAsFactors = FALSE)
    rownames(cdesc) <- cid
    return(cdesc)
  }

  meta_start <- 4L
  meta_end <- 3L + nchd
  if (length(lines) < meta_end) {
    stop("Invalid .gct file (missing cdesc header rows): ", file_path)
  }

  cdesc_values <- vector("list", length = nchd)
  cdesc_names <- character(nchd)
  for (i in seq_len(nchd)) {
    fields <- split_tab_fields(lines[meta_start + i - 1L])
    value_start <- nrhd + 2L
    value_end <- value_start + length(cid) - 1L
    if (length(fields) < value_end) {
      fields <- c(fields, rep(NA_character_, value_end - length(fields)))
    }
    cdesc_names[i] <- as.character(fields[1L])
    cdesc_values[[i]] <- as.character(fields[value_start:value_end])
  }

  cdesc <- as.data.frame(cdesc_values, stringsAsFactors = FALSE, check.names = FALSE)
  names(cdesc) <- make.unique(cdesc_names)
  rownames(cdesc) <- cid
  cdesc$id <- rownames(cdesc)
  cdesc
}

# Parse a GCT/GCTX file while preserving .gct cdesc annotation values as strings.
# @noRd
parse_gctx_preserve_cdesc <- function(file_path) {
  gct <- parse_gctx(file_path)
  if (!grepl("\\.gct$", tolower(file_path))) {
    return(gct)
  }

  cdesc_raw <- read_gct_cdesc_as_character(file_path)
  if (!setequal(rownames(cdesc_raw), gct@cid)) {
    warning(
      "Could not safely restore raw cdesc values from .gct for ",
      basename(file_path),
      "; parsed cdesc rownames did not match GCT sample IDs."
    )
    return(gct)
  }
  gct@cdesc <- cdesc_raw[gct@cid, , drop = FALSE]
  gct
}

# function to transform original GCT file so it is comparable to processed GCT file
# INPUT: parameters list from setup, list of parsed GCTs
# OUTPUT: transformed GCTs without filtering or normalization
transformGCTs <- function(GCTs, parameters) {
  
  message("\nProcessing GCTs...")
  
  processing_out <- mapply(
    GCTs, names(GCTs),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE,
    FUN = function(gct, ome) {
      
      # wrap everything in a try/catch statement
      my_shinyalert_tryCatch(
        text.warning = paste0("<b>Warning in ", ome, ":</b>"),
        append.warning = TRUE,
        text.error = paste0("<b>Error in ", ome, ":</b>"),
        append.error = TRUE,
        return.error = NULL,
        expr = {
          
          # also wrap everything in a withProgress
          withProgress(
            min = 0,
            max = 6, # number of preprocessing steps
            message = paste0("Processing ", ome, ":"),
            expr = {
              ## validate GCT
              gct <- validateGCT(gct)
              
              ## extract data and parameters
              cdesc <- gct@cdesc
              rdesc <- gct@rdesc
              data <- gct@mat
              params <- parameters[[ome]]
              
              ## remove unnecesary elements from parameters
              if (!params$group_normalization) {
                params$group_normalization_column <- NULL
              }
              if (params$data_filter != "StdDev") {
                params$data_filter_sd_pct <- NULL
              }

              ## Handle gene symbol column selection (and optional ID -> geneSymbol mapping).
              gs_out <- apply_gene_symbol_from_params(rdesc = rdesc, params = params, ome = ome)
              rdesc <- gs_out$rdesc
              params <- gs_out$params
              
              ## fix gene symbol formatting (replace semicolons with pipes, clean up)
              if ("geneSymbol" %in% names(rdesc)) {
                fix_result <- fix_gene_symbols(rdesc)
                rdesc <- fix_result$rdesc
                # No rows are removed - blank geneSymbol values are converted to NA
                # removed_rids is kept for backward compatibility but should be empty
              }
              
              incProgress(1, detail = "log transformation")
              
              ## log transformation
              output_list <- perform_log_transformation(data, params$log_transformation)
              data.log.trans <- output_list$data.log.transform
              params$log_transformation <- output_list$updated_method
              
              ## re-combine GCT and return
              transformed_GCT <- GCT(cdesc = cdesc, 
                                   rdesc = rdesc,
                                   mat = data.log.trans,
                                   cid=colnames(data.log.trans),
                                   rid=rownames(data.log.trans))
              
              return(transformed_GCT)
            }
          )
        }
      )
    })
}

# function to parse, normalize, filter, etc. GCT file(s)
# INPUT: parameters list from setup, list of parsed GCTs
# OUTPUT: named list of processed GCTs, updated parameters
processGCTs <- function(GCTs, parameters) {
  
  message("\nProcessing GCTs...")
  
  processing_out <- mapply(
    GCTs, names(GCTs),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE,
    FUN = function(gct, ome) {
      
      # wrap everything in a try/catch statement
      my_shinyalert_tryCatch(
        text.warning = paste0("<b>Warning in ", ome, ":</b>"),
        append.warning = TRUE,
        text.error = paste0("<b>Error in ", ome, ":</b>"),
        append.error = TRUE,
        return.error = NULL,
        expr = {
          
          # also wrap everything in a withProgress
          withProgress(
            min = 0,
            max = 6, # number of preprocessing steps
            message = paste0("Processing ", ome, ":"),
            expr = {
              ## validate GCT
              gct <- validateGCT(gct)
              
              ## extract data and parameters
              cdesc <- gct@cdesc
              rdesc <- gct@rdesc
              data <- gct@mat
              params <- parameters[[ome]]
              
              ## remove unnecesary elements from parameters
              if (!params$group_normalization) {
                params$group_normalization_column <- NULL
              }
              if (params$data_filter != "StdDev") {
                params$data_filter_sd_pct <- NULL
              }

              ## row filtering
              row_filter_out <- apply_row_filter(
                data = data,
                rdesc = rdesc,
                params = params,
                ome = ome
              )
              data <- row_filter_out$data
              rdesc <- row_filter_out$rdesc

              ## sample filtering
              sample_filter_out <- apply_sample_filter(
                data = data,
                cdesc = cdesc,
                params = params,
                ome = ome
              )
              data <- sample_filter_out$data
              cdesc <- sample_filter_out$cdesc
              
              ## Handle gene symbol column selection (and optional ID -> geneSymbol mapping).
              gs_out <- apply_gene_symbol_from_params(rdesc = rdesc, params = params, ome = ome)
              rdesc <- gs_out$rdesc
              params <- gs_out$params
              
              ## fix gene symbol formatting (replace semicolons with pipes, clean up)
              if ("geneSymbol" %in% names(rdesc)) {
                fix_result <- fix_gene_symbols(rdesc)
                rdesc <- fix_result$rdesc
                # No rows are removed - blank geneSymbol values are converted to NA
                # removed_rids is kept for backward compatibility but should be empty
              }
              
              incProgress(1, detail = "log transformation")
              
              ## log transformation
              output_list <- perform_log_transformation(data, params$log_transformation)
              data.log.trans <- output_list$data.log.trans
              params$log_transformation <- output_list$updated_method
              
              incProgress(1, detail = "normalization")
              
              ## data normalization
              output_list <- perform_data_normalization(
                data = data.log.trans, 
                method = params$data_normalization,
                perform.group.normalization = params$group_normalization,
                group.normalization.column = params$group_normalization_column,
                cdesc = cdesc)
              data.norm <- output_list$data.norm
              params$data_normalization <- output_list$updated_method
              
              incProgress(1, detail = "missing value filter")
              
              ## missing value filter
              data.missing.filtered <- perform_missing_filter(data.norm, params$max_missing)
              
              incProgress(1, detail = "standard deviation filter")
              
              ## data filter
              data.filtered <- perform_data_filtering(
                data = data.missing.filtered, 
                method = params$data_filter,
                group.column = params$annotation_column,
                cdesc = cdesc,
                sd.perc = params$data_filter_sd_pct)
              
              incProgress(1, detail = "compiling results")
              
              #update cdesc and rdesc if needed
              cdesc <- cdesc[rownames(cdesc)%in%colnames(data.filtered),,drop=F]
              rdesc <- rdesc[rownames(rdesc)%in%rownames(data.filtered),,drop=F]
              
              ## re-combine GCT and return
              processed_GCT <- GCT(cdesc = cdesc, 
                                   rdesc = rdesc,
                                   mat = data.filtered,
                                   cid=colnames(data.filtered),
                                   rid=rownames(data.filtered))
              
              return(list(processed_GCT = processed_GCT, params = params))
            }
          )
        }
      )
    })
  
  # have the whole output be NULL if there was an error
  if (any(sapply(processing_out, is.null))) return(NULL)
  
  # otherwise, continue
  # pull out the GCTs and updated parameters separately
  GCTs_processed <- sapply(processing_out, 
                           function(ome) ome$processed_GCT,
                           simplify = FALSE)
  parameters_updated <- sapply(processing_out,
                               function(ome) ome$params,
                               simplify = FALSE)
  
  # Convert numeric columns that are discrete to strings in all processed GCTs
  # This ensures discrete columns are treated as categorical, not continuous
  for (ome in names(GCTs_processed)) {
    for (col_name in names(GCTs_processed[[ome]]@cdesc)) {
      if (is.numeric(GCTs_processed[[ome]]@cdesc[[col_name]])) {
        if (is.discrete(GCTs_processed[[ome]]@cdesc[[col_name]], nfactor_cutoff = 20)) {
          GCTs_processed[[ome]]@cdesc[[col_name]] <- as.character(GCTs_processed[[ome]]@cdesc[[col_name]])
        }
      }
    }
  }
  
  GCTs_merged <- my_shinyalert_tryCatch(
    merge_processed_gcts(GCTs_processed, parameters_updated),
    text.warning = "<b>Warning in merging GCTs:</b>",
    show.warning = TRUE,
    append.warning = TRUE,
    text.error = "<b>Error in merging GCTs:</b>",
    show.error = TRUE,
    return.error = NULL,
    append.error = TRUE
  )
  
  # have the whole output be NULL if there was an error
  if (is.null(GCTs_merged)) return(NULL)
  
  # Convert numeric columns that are discrete to strings in merged GCT
  # Use cutoff 20 to match processGCTs logic
  for (col_name in names(GCTs_merged@cdesc)) {
    if (is.numeric(GCTs_merged@cdesc[[col_name]])) {
      if (is.discrete(GCTs_merged@cdesc[[col_name]], nfactor_cutoff = 20)) {
        GCTs_merged@cdesc[[col_name]] <- as.character(GCTs_merged@cdesc[[col_name]])
      }
    }
  }
  
  output <- list(
    GCTs = GCTs_processed,
    parameters = parameters_updated,
    GCTs_merged = GCTs_merged
  )
  
  message("\nDone with GCT processing!")
  
  return(output)
}

# perform log transformation
perform_log_transformation <- function(data, method) {
  if (method == "None") {
    data.log.transform <- data
    
    #if there are negative values in the matrix, do not log transform!
  } else if (any(data < 0, na.rm = T)) {
    warning(paste0("Dataset contains negative values! ", 
                   "Analysis will proceed WITHOUT log-transformation. ",
                   "If you wish to log-transform, please re-upload a ",
                   "dataset without negative values."))
    
    # don't do log transformation, update parameters
    method <- "None"
    data.log.transform <- data
    
    # log 2 transformation
  } else if (method == 'log2') {
    data[data == 0] <- NA
    data.log.transform <- log(data, 2)
    
    # log 10 transformation
  } else if (method == 'log10') {
    data[data == 0] <- NA
    data.log.transform <- log(data, 10)
    
  } else {
    stop("Invalid log transformation selection: ", method)
  }
  
  return(list(data.log.transform = data.log.transform,
              updated_method = method))
}

# perform data normalization
perform_data_normalization <- function(data, method, cdesc,
                                       perform.group.normalization,
                                       group.normalization.column) {
  if (method == "None") {
    data.norm <- data
  } else {
    
    # Disable two-component normalization for datasets with more than 20 samples (too slow)
    # This is a safety check in case the UI didn't prevent selection (e.g., from old parameters)
    if (method == "2-component" && ncol(data) > 20) {
      warning(
        paste0(
          "Two-component normalization is disabled for datasets with more than 20 samples ",
          "(current dataset has ", ncol(data), " samples) due to performance concerns. ",
          "No normalization will be applied."
        )
      )
      method <- "None"
      data.norm <- data
    } else {
      
      if (perform.group.normalization) {
        # get groups vector
        groups.vector <- cdesc[[group.normalization.column]]
        names(groups.vector) <- rownames(cdesc)
        
        # warning if there is any level in groups.vector with only one element
        freq_count <- aggregate(groups.vector, list(element = groups.vector), length)[[2]]
        if (any(freq_count == 1)) {
          warning(
            "One or more levels in the group normalization column only contain ",
            "one element. Consider group normalizing by a different column.")
        }
        
        # perform group-wise normalization
        data.norm <- normalize.data(data, method, groups.vector)
      } else {
        
        # perform regular normalization
        data.norm <- normalize.data(data, method)
      }
      
      # if two-component norm fails....
      if(inherits(data.norm, 'try-error')){
      # reset to no normalization
      data.norm <- data
      method <- "None"
      
      # send out a warning
      # the HTML will be rendered as part of a shinyalert
      warning(paste(
        'The two-component normalization failed to converge on at least one',
        'data column. Please note that this type of normalization expects',
        '<b>log-ratio</b> data that is approximately <b>centered around',
        'zero</b>. Please make sure this is the case by <b>inspecting the',
        'profile plots</b> under the QC tab.'))
      }
    }
  }
  
  return(list(data.norm = data.norm,
              updated_method = method))
}

# maximum missing value filter
perform_missing_filter <- function(data, max_missing) {
  # rowMeans(is.na(data)) == sum(is.na(x))/length(x) per row, in one C-level pass.
  # drop = FALSE keeps a matrix when exactly one row survives (the old code dropped
  # to a vector, which crashed the downstream data.frame(data, id = rownames(data))).
  missing_percent <- rowMeans(is.na(data))
  data[missing_percent <= max_missing / 100, , drop = FALSE]
}

# perform data filtering
perform_data_filtering <- function(data, method, group.column, cdesc, sd.perc) {
  if (method == "None") {
    data.filtered <- data
    
  } else if (method == "StdDev") {
    # turn data into the expected format
    data_with_id <- data.frame(data, id = rownames(data))
    
    # get the groups vector
    group.vec <- cdesc[[group.column]]
    names(group.vec) <- rownames(cdesc)
    
    # filter data
    filtering_out <- sd.filter(
      tab = data_with_id, 
      grp.vec = group.vec, 
      id.col = 'id',
      sd.perc = sd.perc)
    
    # get the output
    tab <- filtering_out$table
    data.filtered <- as.matrix(tab[, setdiff(names(tab), 'id')])
    
  } else {
    stop("Invalid data filter selected")
  }
  
  return(data.filtered)
}

# validate GCT is the correct input
validateGCT <- function(gct) {
  mat <- gct@mat
  cdesc <- gct@cdesc
  rdesc <- gct@rdesc
  
  # check that rdesc matches row names
  if (!setequal(rownames(mat), rownames(rdesc))) {
    stop("GCT data row names not match `rdesc` row names.")
  }
  
  # Check if cdesc is missing, empty, or only has "id" column - if so, create Sample.ID column
  # This handles GCTs that don't have proper cdesc metadata
  if (is.null(cdesc) || nrow(cdesc) == 0 || ncol(cdesc) == 0) {
    # Create new cdesc with Sample.ID column
    sample_ids <- colnames(mat)
    cdesc <- data.frame(
      Sample.ID = sample_ids,
      stringsAsFactors = FALSE
    )
    rownames(cdesc) <- sample_ids
  } else if (ncol(cdesc) == 1 && names(cdesc)[1] == "id") {
    # If cdesc only has exactly one column named "id", recreate with Sample.ID column
    sample_ids <- colnames(mat)
    cdesc <- data.frame(
      Sample.ID = sample_ids,
      stringsAsFactors = FALSE
    )
    rownames(cdesc) <- sample_ids
  } else if (!setequal(colnames(mat), rownames(cdesc))) {
    # cdesc has real metadata but rownames don't match - this is an error
    stop("GCT data column names does not match `cdesc` row names.")
  }
  
  # check for infinities
  if (any(is.infinite(mat))) {
    warning("Data contains infinite entries. Replacing these entries with NA.")
    mat[is.infinite(mat)] <- NA
  }
  
  # check for NaN's
  if (any(is.nan(mat))) {
    warning("Data contains NaN (Not a Number) entries. Replacing these entries with NA.")
    mat[is.nan(mat)] <- NA
  }
  
  # make sure cdesc/rdesc order matches data column/row names
  # warning here if rows/columns are misaligned?
  cdesc <- cdesc[colnames(mat), , drop = FALSE]
  rdesc <- rdesc[rownames(mat), , drop = FALSE]
  
  return(GCT(mat = mat, rdesc = rdesc, cdesc = cdesc))
}

# merge processed GCTs
merge_processed_gcts <- function(GCTs_processed, parameters_updated) {
  withProgress(message = "Merging GCTs", expr = {
    
    # add a protigy.ome column to each gct's rdesc using dataset labels from parameters
    GCTs_processed <- mapply(
      GCTs_processed, names(GCTs_processed), parameters_updated,
      SIMPLIFY = FALSE, USE.NAMES = TRUE, 
      FUN = function(gct, filename, params) {
        # Get the dataset label from parameters
        dataset_label <- params$dataset_label
        if (is.null(dataset_label)) {
          # Fallback to filename if no label is set
          dataset_label <- filename
        }
        
        # check if `protigy.ome` is a column in the current gct
        if ("protigy.ome" %in% names(gct@rdesc) & any(gct@rdesc$protigy.ome != dataset_label)) {
          warning("`protigy.ome` column already exists and will be overwritten in ", filename)
        }
        gct@rdesc$protigy.ome <- rep(dataset_label, dim(gct@rdesc)[1])
        return(gct)
      })
    
    incProgress()
    
    # merge GCTs first using cmapR::merge_gct
    GCTs_merged <- Reduce(
      function(gct1, gct2) {
        #before merging, need to make sure the rids are unique
        #first save the old IDs
        gct1@rdesc$old_id = gct1@rid
        gct2@rdesc$old_id = gct2@rid
        
        # Only apply prefix if not already prefixed (avoid duplication)
        # Check if the rid already starts with the ome name
        if (!any(startsWith(gct1@rid, paste0(gct1@rdesc$protigy.ome[1], "_")))) {
          rownames(gct1@mat) = rownames(gct1@rdesc) = gct1@rdesc$id = gct1@rid = paste(gct1@rdesc$protigy.ome,gct1@rid,sep="_")
        }
        if (!any(startsWith(gct2@rid, paste0(gct2@rdesc$protigy.ome[1], "_")))) {
          rownames(gct2@mat) = rownames(gct2@rdesc) = gct2@rdesc$id = gct2@rid = paste(gct2@rdesc$protigy.ome,gct2@rid,sep="_")
        }
        
        #now can merge and rids will always be unique
        merged <- cmapR::merge_gct(gct1, gct2, dim='row')
        incProgress()
        return(merged)
      },
      GCTs_processed)
    rownames(GCTs_merged@cdesc) <- GCTs_merged@cid
    # Keep the merged feature IDs as rdesc rownames (these are the actual feature IDs)
    rownames(GCTs_merged@rdesc) <- GCTs_merged@rid
    
    
    ## Now deal with the cdesc
    # cmapR::merge_gct will override any conflicting annotation columns in cdesc
    # with whatever is in the first GCT. Instead, we want to duplicate conflict
    # columns so no data is lost.
    
    # figure out which columns conflict with other omes
    conflict_columns <- c()
    for (i in seq_along(GCTs_processed)) {
      ome <- names(GCTs_processed)[i]
      gct <- GCTs_processed[[i]]
      
      # subset to only samples in ome
      samples_in_ome <- gct@cid
      merged_cdesc_subset <- GCTs_merged@cdesc[samples_in_ome, , drop = FALSE]
      
      # if there's a column with all NA, replace with values in this ome
      replace_NA_col <- intersect(
        names(which(sapply(merged_cdesc_subset, function(col) all(is.na(col))))),
        names(gct@cdesc)
      )
      if (length(replace_NA_col) > 0) {
        GCTs_merged@cdesc[samples_in_ome, replace_NA_col] <- gct@cdesc[samples_in_ome, replace_NA_col]
        merged_cdesc_subset <- GCTs_merged@cdesc[samples_in_ome, , drop = FALSE]
      }
      
      
      # find columns that have a conflict
      conflict_columns_ome <- names(which(
        sapply(names(gct@cdesc), function(col) {
          TRUE %in% c(
            any(gct@cdesc[[col]] != merged_cdesc_subset[[col]]), # any values are not the same
            any(is.na(gct@cdesc[[col]]) != is.na(merged_cdesc_subset[[col]])) # any NA's are not in the same place
          )
        })
      ))
      
      conflict_columns <- unique(c(conflict_columns, conflict_columns_ome))
    }
    
    incProgress()
    
    # remove conflicting columns and re-name by ome
    for (col in conflict_columns) {
      
      # get the omes that contain this conflict column
      omes_with_col <- names(which(
        sapply(GCTs_processed, function(gct) col %in% names(gct@cdesc))
      ))
      
      # get the new column names, make sure they're unique
      new_col_names <- utils::tail(
        n = length(omes_with_col),
        make.names(c(names(GCTs_merged@cdesc), paste0(col, '.', omes_with_col)),
                   unique = TRUE)
      )
      
      # get the new columns from each ome's GCT's cdesc
      # make sure samples are in the same order as they are in GCTs_merged
      all_samples <- rownames(GCTs_merged@cdesc)
      new_columns <- as.data.frame(sapply(omes_with_col, 
                                          function(ome) GCTs_processed[[ome]]@cdesc[all_samples, col],
                                          simplify = FALSE))
      names(new_columns) <- new_col_names
      
      
      GCTs_merged@cdesc <- GCTs_merged@cdesc %>%
        dplyr::mutate(new_columns, .after = dplyr::all_of(col)) %>%
        dplyr::select(-dplyr::all_of(col))
    }
    
    # Add missing columns logic
    # Find columns that exist in some datasets but not in the merged cdesc
    all_unique_columns <- unique(unlist(lapply(GCTs_processed, function(gct) names(gct@cdesc))))
    missing_columns <- setdiff(all_unique_columns, names(GCTs_merged@cdesc))
    
    if (length(missing_columns) > 0) {
      message("Adding missing columns to merged GCT: ", paste(missing_columns, collapse = ", "))
      
      # Add missing columns to the merged cdesc
      for (col in missing_columns) {
        # Find which datasets have this column
        omes_with_col <- names(which(
          sapply(GCTs_processed, function(gct) col %in% names(gct@cdesc))
        ))
        
        # For samples that don't have this column, fill with NA
        # For samples that do have this column, use their values
        all_samples <- rownames(GCTs_merged@cdesc)
        new_column <- rep(NA, length(all_samples))
        names(new_column) <- all_samples
        
        # Fill in values from datasets that have this column
        for (ome in omes_with_col) {
          samples_in_ome <- GCTs_processed[[ome]]@cid
          new_column[samples_in_ome] <- GCTs_processed[[ome]]@cdesc[samples_in_ome, col]
        }
        
        # Add the column to merged cdesc
        GCTs_merged@cdesc[[col]] <- new_column
      }
    }
    
    setProgress(1)
    
  })
  
  return(GCTs_merged)
}
