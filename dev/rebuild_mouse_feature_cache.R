# One-off: rebuild the mouse (10090) UniProt feature cache from the FASTA-fallback
# universe, to troubleshoot the under-populated / wrong-species cache.
#
# Run from repo root in R:  source("dev/rebuild_mouse_feature_cache.R")
# Requires network (hits rest.uniprot.org). NOT a test; do not commit the cache.

suppressMessages(devtools::load_all("."))

database_dir <- "inst/database"
species      <- "10090"
species_dir  <- file.path(database_dir, species)

# --- 1. Locate the mouse FASTA --------------------------------------------------
fasta_dir <- file.path(species_dir, "fasta")
fasta_files <- list.files(fasta_dir, pattern = "\\.fasta$", full.names = TRUE)
stopifnot(length(fasta_files) == 1L)
fasta_path <- fasta_files[[1L]]
message("FASTA: ", fasta_path)

# --- 2. Wipe the corrupted cache so the merge cannot retain stale (human) rows --
feat_dir <- file.path(species_dir, "uniprot_features")
for (f in c("uniprot_features.tsv", "schema.json")) {
  p <- file.path(feat_dir, f)
  if (file.exists(p)) {
    file.remove(p)
    message("Removed stale cache file: ", p)
  }
}

# --- 3. Build the universe = mouse FASTA accession keys (uniprot, pipe-aware) ----
fasta_map <- pelsa_read_fasta(fasta_path, mode = "uniprot")
universe  <- pelsa_refresh_accession_universe(
  gcts = NULL, existing_cache = NULL, fasta_map = fasta_map
)
message(sprintf("FASTA sequences: %d | universe accessions: %d",
                length(fasta_map), length(universe)))

# --- 4. Fetch + write (real network fetcher; no prior cache to merge over) -------
t0 <- Sys.time()
res <- pelsa_refresh_species_cache(
  species     = species,
  universe    = universe,
  species_dir = species_dir,
  fetch_fn    = pelsa_fetch_uniprot,
  existing    = NULL,
  progress    = NULL
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

# --- 5. Report ------------------------------------------------------------------
feats <- res$features
message("\n================ REBUILD RESULT ================")
message(sprintf("elapsed              : %.1f s", elapsed))
message(sprintf("universe accessions  : %d", res$n_accessions))
message(sprintf("feature rows written : %d", res$n_features))
message(sprintf("distinct accessions  : %d",
                length(unique(feats$accession))))
message(sprintf("unresolved accessions: %d", res$n_unresolved))
message(sprintf("cache path           : %s", res$path))

message("\nfeature_type distribution:")
print(sort(table(feats$feature_type), decreasing = TRUE))

# --- 6. Species sanity check: sample 5 cached accessions, confirm organism ------
samp <- head(unique(feats$accession), 5L)
message("\nSpecies sanity check (live UniProt) on a 5-accession sample: ",
        paste(samp, collapse = ", "))
chk <- pelsa_fetch_uniprot(samp)
# Pull organism ids straight from a raw search to verify 10090.
q <- paste0("accession:(", paste(samp, collapse = " OR "), ")")
url <- httr2::request(.PELSA_UNIPROT_BASE)
url <- httr2::req_url_path_append(url, "search")
url <- httr2::req_url_query(url, query = q,
                            fields = "accession,organism_id,organism_name",
                            format = "tsv")
resp <- httr2::req_perform(url)
cat(httr2::resp_body_string(resp))
message("================================================")
