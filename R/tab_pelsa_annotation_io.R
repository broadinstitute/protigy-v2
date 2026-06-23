################################################################################
# Module: PELSA annotation-file IO + parity-locked feature classifier.
#
# Reads a user-uploaded RAW feature annotation file (the external UniProt-fetch
# workflow's output) and classifies each feature into the parity-locked
# feature_class + class_score, producing the SAME 8-column frame the analysis
# overlap (pelsa_annotate_features) consumes.
#
# The classifier (pelsa_feature_to_class / pelsa_feature_class_scores) lives HERE
# so parity logic and its unit tests stay in ONE place; the external fetch
# workflow only emits raw features (accession, feature_type, start, end,
# description[, coord_quality]).
#
# PARITY (classifier_version "fixed_v1"): the keyword sets, the CHECK ORDER, and
# SCORES are parity-locked to the notebook's uniprot_features.py::feature_to_class.
################################################################################

# ---- Classifier (parity-locked) ---------------------------------------------

# class_score lookup, parity-locked to schema.json::feature_class_scores.
# Returned as a named integer vector (class_score column is int8 in the cache).
#
# @return named integer vector feature_class -> score
# @noRd
pelsa_feature_class_scores <- function() {
  c(
    active_or_binding_site     = 5L,
    catalytic_domain           = 3L,
    folded_domain              = 2L,
    region_or_motif            = 1L,
    repeat_or_coiled_coil      = -1L,
    transmembrane_or_signal    = 0L,
    low_complexity_or_disorder = -3L,
    other                      = 0L
  )
}

# Classify one UniProt feature into a coarse functional class.
#
# Vectorized port of the notebook's feature_to_class (classifier_version
# "fixed_v1"). The CHECK ORDER is parity-critical -- reordering changes results:
#   1. compositional bias                       -> low_complexity_or_disorder
#   2. site set (active/binding/metal/.../DNA)  -> active_or_binding_site
#   3. TM/signal set                            -> transmembrane_or_signal
#   4. desc-keyword disorder check (BEFORE repeat) -> low_complexity_or_disorder
#   5. repeat / coiled-coil set                 -> repeat_or_coiled_coil
#   6. domain: catalytic-by-keyword else folded
#   7. region / motif                           -> region_or_motif
#   8. else                                     -> other
# ftype and desc are lower-cased + trimmed (NA -> "").
#
# @param ftype character vector of UniProt feature types
# @param desc  character vector of feature descriptions (recycled to ftype)
# @return character vector of feature_class labels
# @noRd
pelsa_feature_to_class <- function(ftype, desc) {
  ftype <- tolower(trimws(ifelse(is.na(ftype), "", as.character(ftype))))
  if (missing(desc) || is.null(desc)) desc <- ""
  desc <- tolower(trimws(ifelse(is.na(desc), "", as.character(desc))))

  n <- length(ftype)
  if (length(desc) == 1L && n != 1L) desc <- rep(desc, n)
  if (length(desc) != n) {
    stop("pelsa_feature_to_class: ftype and desc lengths differ")
  }

  site_set <- c("active site", "binding site", "metal binding",
                "nucleotide binding", "site", "dna binding")
  tm_set   <- c("transmembrane", "signal peptide", "topological domain",
                "intramembrane", "signal")
  repeat_set <- c("repeat", "coiled-coil", "coiled coil")
  catalytic_kw <- c("kinase", "methyltransferase", "transferase", "atpase",
                    "helicase", "protease", "dehydrogenase")

  has_kw <- function(x, kws) {
    Reduce(`|`, lapply(kws, function(k) grepl(k, x, fixed = TRUE)),
           init = rep(FALSE, length(x)))
  }

  disorder_desc <- grepl("low complexity", desc, fixed = TRUE) |
    grepl("compositionally biased", desc, fixed = TRUE) |
    grepl("disordered", desc, fixed = TRUE)

  out <- character(n)
  # default
  out[] <- "other"

  # Evaluate in REVERSE priority so earlier (higher-priority) checks overwrite
  # later ones -- preserving the notebook's first-match-wins order.
  is_region_motif <- ftype %in% c("region", "motif")
  out[is_region_motif] <- "region_or_motif"

  is_domain <- ftype == "domain"
  out[is_domain] <- ifelse(has_kw(desc[is_domain], catalytic_kw),
                           "catalytic_domain", "folded_domain")

  is_repeat <- ftype %in% repeat_set
  out[is_repeat] <- "repeat_or_coiled_coil"

  # desc-keyword disorder check BEATS repeat + region/motif + domain
  out[disorder_desc] <- "low_complexity_or_disorder"

  is_tm <- ftype %in% tm_set
  out[is_tm] <- "transmembrane_or_signal"

  is_site <- ftype %in% site_set
  out[is_site] <- "active_or_binding_site"

  # compositional bias short-circuits FIRST (highest priority)
  is_compbias <- ftype == "compositional bias"
  out[is_compbias] <- "low_complexity_or_disorder"

  out
}

# ---- Empty frame -------------------------------------------------------------

# Empty 0-row frame with the 8 schema columns + correct types.
# @noRd
pelsa_empty_feature_frame <- function() {
  data.frame(
    accession     = character(0),
    feature_type  = character(0),
    start         = integer(0),
    end           = integer(0),
    description   = character(0),
    feature_class = character(0),
    class_score   = integer(0),
    coord_quality = character(0),
    stringsAsFactors = FALSE
  )
}

# ---- Raw annotation-file reader ----------------------------------------------

# Read a user-uploaded RAW feature annotation file (TSV) and classify it.
#
# Required columns: accession, feature_type, start, end, description.
# Optional column:  coord_quality (default "exact" when absent/blank).
# Derives feature_class via pelsa_feature_to_class() and class_score via
# pelsa_feature_class_scores(), returning the canonical 8-column frame that
# pelsa_annotate_features() / pelsa_read_feature_cache() also produce.
#
# NOTE: the input format is PROVISIONAL. When the finalized example file arrives,
# adjust ONLY the column mapping below.
#
# @param path single path to a .tsv annotation file
# @return 8-column data.frame (0 rows allowed); errors on missing file/columns
# @noRd
pelsa_read_annotation_file <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("pelsa_read_annotation_file: annotation file not found: ", path)
  }

  raw <- readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
  required <- c("accession", "feature_type", "start", "end", "description")
  missing <- setdiff(required, colnames(raw))
  if (length(missing) > 0L) {
    stop("pelsa_read_annotation_file: missing required column(s): ",
         paste(missing, collapse = ", "))
  }

  if (nrow(raw) == 0L) return(pelsa_empty_feature_frame())

  accession    <- as.character(raw$accession)
  feature_type <- as.character(raw$feature_type)
  start        <- as.integer(raw$start)
  end          <- as.integer(raw$end)
  description  <- ifelse(is.na(raw$description), "", as.character(raw$description))

  coord_quality <- if ("coord_quality" %in% colnames(raw)) {
    cq <- ifelse(is.na(raw$coord_quality), "", as.character(raw$coord_quality))
    cq[!nzchar(cq)] <- "exact"
    cq
  } else {
    rep("exact", nrow(raw))
  }

  feature_class <- pelsa_feature_to_class(feature_type, description)
  scores <- pelsa_feature_class_scores()
  class_score <- as.integer(scores[feature_class])
  class_score[is.na(class_score)] <- 0L

  data.frame(
    accession     = accession,
    feature_type  = feature_type,
    start         = start,
    end           = end,
    description   = description,
    feature_class = feature_class,
    class_score   = class_score,
    coord_quality = coord_quality,
    stringsAsFactors = FALSE
  )
}
