# =============================================================================
# GOLD-STANDARD BASELINE: mouse (10090) UniProt feature fetch, computed
# INDEPENDENTLY of the app's pelsa_fetch_uniprot / pelsa_parse_uniprot_json.
#
# Purpose: produce a reference feature table + summary manifest that the
# INTEGRATED app result (after the planned fixes) can be cross-checked against.
# To be a real cross-check this script reimplements the fetch + parse from
# scratch (own httr2 calls, own minimal JSON->row parser) rather than calling
# the package functions under test. If both agree, the integration is correct.
#
# Run from repo root in R:  source("dev/mouse_feature_baseline.R")
# Requires network (rest.uniprot.org). Writes:
#   dev/baseline/mouse_10090_features.baseline.tsv   (8-col, schema order)
#   dev/baseline/mouse_10090_baseline.summary.json   (counts + sampled accs)
# These are gitignored artifacts (under dev/baseline/); do not commit.
# =============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

# ---- Config (independent constants; mirror the FIXED app values) ------------
UNIPROT_BASE <- "https://rest.uniprot.org/uniprotkb"
BATCH_SIZE   <- 100L          # UniProt /search hard cap = 100 OR conditions
UA           <- "pelsa_baseline/0.1 (independent gold-standard)"
RATE_PER_SEC <- 10
MAX_TRIES    <- 5L

FASTA_PATH <- file.path(
  "inst/database/10090/fasta",
  "UniProt.mouse.20171228.RISnrNF.553smORFs.264contams.fasta"
)
OUT_DIR  <- "dev/baseline"
OUT_TSV  <- file.path(OUT_DIR, "mouse_10090_features.baseline.tsv")
OUT_JSON <- file.path(OUT_DIR, "mouse_10090_baseline.summary.json")

# Parity-locked class scores (== schema.json::feature_class_scores). Duplicated
# here ON PURPOSE so the baseline does not depend on the package source.
SCORES <- c(
  active_or_binding_site = 5L, catalytic_domain = 3L, folded_domain = 2L,
  region_or_motif = 1L, repeat_or_coiled_coil = -1L,
  transmembrane_or_signal = 0L, low_complexity_or_disorder = -3L, other = 0L
)

# ---- Independent helpers -----------------------------------------------------

# Parse FASTA accession keys (UniProt pipe-aware), independent of pelsa_read_fasta.
read_fasta_accessions <- function(path) {
  lines <- readLines(path, warn = FALSE)
  hdr <- lines[startsWith(lines, ">")]
  hdr <- sub("^>", "", hdr)
  first_tok <- sub("\\s.*$", "", hdr)
  has_pipe <- grepl("\\|", first_tok)
  keys <- ifelse(has_pipe, sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", first_tok), first_tok)
  unique(keys[!is.na(keys) & nzchar(keys)])
}

# UniProt accession format (base or "-N" isoform), independent regex.
ACC_RE <- "^([OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})(-[0-9]+)?$"
is_valid_acc <- function(x) !is.na(x) & grepl(ACC_RE, x)
iso_base     <- function(x) sub("-[0-9]+$", "", x)

# Coarse feature class (independent port of the notebook classifier "fixed_v1").
feature_class <- function(ftype, desc) {
  ftype <- tolower(trimws(ftype %||% "")); desc <- tolower(trimws(desc %||% ""))
  disorder <- grepl("low complexity|compositionally biased|disordered", desc)
  site <- c("active site","binding site","metal binding","nucleotide binding","site","dna binding")
  tm   <- c("transmembrane","signal peptide","topological domain","intramembrane","signal")
  rep_ <- c("repeat","coiled-coil","coiled coil")
  cat_ <- c("kinase","methyltransferase","transferase","atpase","helicase","protease","dehydrogenase")
  if (ftype == "compositional bias") return("low_complexity_or_disorder")
  if (ftype %in% site) return("active_or_binding_site")
  if (ftype %in% tm)   return("transmembrane_or_signal")
  if (disorder)        return("low_complexity_or_disorder")
  if (ftype %in% rep_) return("repeat_or_coiled_coil")
  if (ftype == "domain") return(if (any(vapply(cat_, function(k) grepl(k, desc, fixed=TRUE), logical(1)))) "catalytic_domain" else "folded_domain")
  if (ftype %in% c("region","motif")) return("region_or_motif")
  "other"
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# Parse one UniProt entry (jsonlite list) into 0+ feature rows.
parse_entry <- function(e) {
  acc <- e$primaryAccession %||% ""
  feats <- e$features
  if (is.null(feats) || length(feats) == 0L) return(NULL)
  rows <- lapply(feats, function(f) {
    sv <- f$location$start$value; ev <- f$location$end$value
    if (is.null(sv) || is.null(ev)) return(NULL)
    ftype <- f$type %||% ""
    desc  <- f$description %||% ""
    if (!nzchar(desc) && !is.null(f$ligand$name)) desc <- f$ligand$name
    smod <- f$location$start$modifier %||% "EXACT"
    emod <- f$location$end$modifier %||% "EXACT"
    fc <- feature_class(ftype, desc)
    data.frame(
      accession = acc, feature_type = ftype,
      start = as.integer(sv), end = as.integer(ev),
      description = desc, feature_class = fc,
      class_score = as.integer(SCORES[[fc]]),
      coord_quality = if (smod == "EXACT" && emod == "EXACT") "exact" else "fuzzy",
      stringsAsFactors = FALSE)
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# Fetch one batch (<=100 base accessions). Returns list(entries=, status=).
fetch_batch <- function(accs) {
  q <- paste0("accession:(", paste(accs, collapse = " OR "), ")")
  req <- request(UNIPROT_BASE) |>
    req_url_path_append("search") |>
    req_url_query(query = q, format = "json", size = length(accs)) |>
    req_user_agent(UA) |>
    req_throttle(capacity = RATE_PER_SEC, fill_time_s = 1) |>
    req_retry(max_tries = MAX_TRIES,
              is_transient = function(r) resp_status(r) %in% c(429,500,502,503,504)) |>
    req_error(is_error = function(r) FALSE)
  # Follow cursor pagination defensively.
  entries <- list(); status <- NA_integer_
  repeat {
    resp <- req_perform(req); status <- resp_status(resp)
    if (status >= 400L) break
    parsed <- resp_body_json(resp)
    entries <- c(entries, parsed$results %||% list())
    nxt <- resp_header(resp, "Link")
    if (is.null(nxt) || !grepl("next", nxt)) break
    url <- sub('.*<([^>]+)>;\\s*rel="next".*', "\\1", nxt)
    req <- request(url) |> req_user_agent(UA) |>
      req_throttle(capacity = RATE_PER_SEC, fill_time_s = 1) |>
      req_error(is_error = function(r) FALSE)
  }
  list(entries = entries, status = status)
}

# ---- Run ---------------------------------------------------------------------
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

fasta_acc <- read_fasta_accessions(FASTA_PATH)
valid     <- is_valid_acc(fasta_acc)
query_accs <- sort(unique(iso_base(fasta_acc[valid])))   # base, deduped, valid
n_invalid  <- sum(!valid)

message(sprintf("FASTA keys: %d | invalid-format dropped: %d | base query accs: %d",
                length(fasta_acc), n_invalid, length(query_accs)))

batches <- split(query_accs, ceiling(seq_along(query_accs) / BATCH_SIZE))
all_entries <- list(); failed_batches <- 0L
t0 <- Sys.time()
for (i in seq_along(batches)) {
  fb <- fetch_batch(batches[[i]])
  if (!is.na(fb$status) && fb$status >= 500L) failed_batches <- failed_batches + 1L
  all_entries <- c(all_entries, fb$entries)
  if (i %% 25L == 0L || i == length(batches))
    message(sprintf("  batch %d/%d (entries so far: %d)", i, length(batches), length(all_entries)))
}
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

feat_list <- Filter(Negate(is.null), lapply(all_entries, parse_entry))
features  <- if (length(feat_list)) do.call(rbind, feat_list) else
  data.frame(accession=character(), feature_type=character(), start=integer(),
             end=integer(), description=character(), feature_class=character(),
             class_score=integer(), coord_quality=character())
rownames(features) <- NULL

# Resolved = base accessions UniProt returned an entry for.
returned_acc <- unique(vapply(all_entries, function(e) e$primaryAccession %||% NA_character_, character(1)))
returned_acc <- returned_acc[!is.na(returned_acc)]
unresolved   <- setdiff(query_accs, returned_acc)

# ---- Write baseline TSV + summary -------------------------------------------
features <- features[order(features$accession, features$start, features$end), ]
write.table(features, OUT_TSV, sep = "\t", quote = FALSE, row.names = FALSE)

set.seed(20260618)
samp_resolved   <- sample(unique(features$accession),
                          min(8L, length(unique(features$accession))))
samp_unresolved <- if (length(unresolved)) sample(unresolved, min(8L, length(unresolved))) else character(0)

summary <- list(
  generated_for = "10090 Mus musculus",
  fasta = basename(FASTA_PATH),
  batch_size = BATCH_SIZE,
  elapsed_secs = round(elapsed, 1),
  n_fasta_keys = length(fasta_acc),
  n_invalid_format_dropped = n_invalid,
  n_query_accessions = length(query_accs),
  n_feature_rows = nrow(features),
  n_distinct_accessions_with_features = length(unique(features$accession)),
  n_resolved_entries = length(returned_acc),
  n_unresolved = length(unresolved),
  failed_batches = failed_batches,
  feature_type_counts = as.list(sort(table(features$feature_type), decreasing = TRUE)),
  feature_class_counts = as.list(sort(table(features$feature_class), decreasing = TRUE)),
  sample_resolved_with_features = samp_resolved,
  sample_unresolved = samp_unresolved
)
write_json(summary, OUT_JSON, auto_unbox = TRUE, pretty = TRUE)

message("\n================ BASELINE SUMMARY ================")
message(sprintf("feature rows           : %d", nrow(features)))
message(sprintf("distinct w/ features   : %d", length(unique(features$accession))))
message(sprintf("resolved entries       : %d", length(returned_acc)))
message(sprintf("unresolved (base accs) : %d", length(unresolved)))
message(sprintf("invalid-format dropped : %d", n_invalid))
message(sprintf("failed (5xx) batches   : %d", failed_batches))
message(sprintf("elapsed                : %.1f s", elapsed))
message(sprintf("baseline TSV           : %s", OUT_TSV))
message(sprintf("summary JSON           : %s", OUT_JSON))
message("=================================================")
message("\nCROSS-CHECK after integration: rebuild the app's mouse cache, then")
message("compare row count, distinct accessions, and a join on")
message("(accession,feature_type,start,end) -- they should match this baseline.")
