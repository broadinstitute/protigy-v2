################################################################################
# Module: PELSA Section 3 - volcano SELECTION/INTERACTION pure helpers.
#
# The single-selection model's pure logic: resolve a click to a peptide, compute
# the gold recolor arrays for the proxy restyle, the Find-accession match mask,
# and the pinned-panel metadata rows. No Shiny; unit-tested against the seeded
# synthetic generator's closed-form ground truth.
################################################################################

# ---- 7E: resolve a plotly_click event to a volcano-df peptide ---------------

# Map a plotly_click event (a one-row data.frame from event_data() carrying at
# least numeric `x` and `y`, the clicked point's logFC / -log10(P.Value)) back to
# the volcano-df row it came from, returning that peptide's identity.
#
# WHY coordinate-matching (not customdata/key): ggplotly() does NOT reliably
# round-trip a `key`/`customdata` aesthetic through plotly::toWebGL across the
# multiple geom traces (background / marker / labels) the volcano draws, and the
# event's curveNumber/pointNumber then index INTO the wrong trace after WebGL
# trace-merging. The point's (x, y) IS stable: plotly returns the exact
# (logFC, logP) of the clicked marker, so we match the nearest df row by squared
# Euclidean distance in (logFC, logP). Ties / multiple peptides at the same
# coordinate resolve to the FIRST df row (deterministic).
#
# Representative accession when a peptide maps to several: the volcano df's
# `winning_accession` (the 2I feature-annotation winner - the leading/
# representative accession for that peptide). Falls back to the first
# ;-separated token of PG.ProteinAccessions when winning_accession is NA/empty.
#
# @param event     a one-row data.frame/list with numeric `x` and `y` (an
#   event_data("plotly_click") row), or NULL.
# @param volcano_df a 3A frame (logFC, logP, id, winning_accession,
#   PG.ProteinAccessions, label).
# @return list(row = <1-based df row index>, peptide_seq = <id>,
#   accession = <representative accession>, label = <df label>), or NULL when no
#   match (NULL/empty event, empty df, or no finite coordinate).
# @noRd
pelsa_volcano_resolve_click <- function(event, volcano_df) {
  if (is.null(event) || !is.data.frame(volcano_df) || nrow(volcano_df) == 0L) {
    return(NULL)
  }
  ex <- suppressWarnings(as.numeric(event$x)[1L])
  ey <- suppressWarnings(as.numeric(event$y)[1L])
  if (length(ex) == 0L || length(ey) == 0L || is.na(ex) || is.na(ey)) {
    return(NULL)
  }
  if (!all(c("logFC", "logP") %in% colnames(volcano_df))) return(NULL)

  fx <- as.numeric(volcano_df$logFC)
  fy <- as.numeric(volcano_df$logP)
  d2 <- (fx - ex)^2 + (fy - ey)^2
  d2[is.na(d2)] <- Inf
  if (!any(is.finite(d2))) return(NULL)
  row <- which.min(d2)

  acc <- NA_character_
  if ("winning_accession" %in% colnames(volcano_df)) {
    acc <- as.character(volcano_df$winning_accession[row])
  }
  if (is.na(acc) || !nzchar(acc)) {
    pg <- if ("PG.ProteinAccessions" %in% colnames(volcano_df)) {
      as.character(volcano_df$PG.ProteinAccessions[row])
    } else {
      NA_character_
    }
    acc <- if (is.na(pg) || !nzchar(pg)) NA_character_ else
      trimws(strsplit(pg, ";", fixed = TRUE)[[1]][1L])
  }

  pep <- if ("id" %in% colnames(volcano_df)) {
    as.character(volcano_df$id[row])
  } else {
    NA_character_
  }
  lab <- if ("label" %in% colnames(volcano_df)) {
    as.character(volcano_df$label[row])
  } else {
    NA_character_
  }
  list(row = as.integer(row), peptide_seq = pep, accession = acc, label = lab)
}

# ---- 7E: sibling-peptide trace split (for the pinned-protein highlight) ------

# Split a volcano frame into the PINNED protein's peptides (the pinned peptide +
# its sibling peptides - every row whose winning_accession equals the pinned
# accession) and the REST. On pin, the main volcano is NOT rebuilt; instead the
# FADE is applied client-side via a plotlyProxy restyle (single mechanism) that
# sets a per-point marker-opacity vector on the background trace - full opacity
# for the pinned protein's peptides, dimmed for the rest. This mask drives that
# opacity vector (see pelsa_volcano_pin_opacity). It is also reused by the
# static PDF export path's build.
#
# Matching is on `winning_accession` (the representative accession 3A resolves
# per peptide), so a peptide pinned in a multi-protein group lights up its
# siblings under the SAME representative protein.
#
# @param volcano_df a 3A frame carrying winning_accession.
# @param accession  the pinned protein's representative accession (scalar), or
#   NULL/NA -> no siblings (all rows go to `rest`).
# @return list(siblings = <logical mask>, n_siblings = <integer>), the mask
#   length nrow(volcano_df) TRUE where the row belongs to the pinned protein.
# @noRd
pelsa_volcano_sibling_mask <- function(volcano_df, accession) {
  if (!is.data.frame(volcano_df)) {
    stop("pelsa_volcano_sibling_mask: volcano_df must be a data.frame")
  }
  n <- nrow(volcano_df)
  if (n == 0L || is.null(accession) || length(accession) != 1L ||
      is.na(accession) || !nzchar(accession) ||
      !"winning_accession" %in% colnames(volcano_df)) {
    return(list(siblings = rep(FALSE, n), n_siblings = 0L))
  }
  wa <- as.character(volcano_df$winning_accession)
  mask <- !is.na(wa) & wa == accession
  list(siblings = mask, n_siblings = sum(mask))
}

# Build the per-point marker-opacity vector for the BACKGROUND trace of the main
# volcano, for a plotlyProxy "restyle" fade (the perf fix). The main volcano is
# built ONCE (with sibling_acc = NULL) so its background trace contains EVERY
# non-marker point in pelsa_volcano_marker_split(df)$background row order; this
# restyle sets that trace's marker.opacity WITHOUT rebuilding the ~100k-point
# figure (a small message, not a ~15MB redraw).
#
# When `accession` is NULL/NA (unpin / contrast switch) -> every background point
# returns to the default opacity (the base look). When set -> the pinned
# protein's peptides get full opacity (1) and the rest are dimmed.
#
# The vector is aligned to the background-trace point order: it is computed over
# `pelsa_volcano_marker_split(df)$background`, the SAME split the build applies,
# so element j of this vector targets background point j of the rendered trace.
#
# @param df        the FULL volcano frame the base plot was built from.
# @param accession the pinned protein's representative accession, or NULL/NA.
# @return list(opacity = <numeric vector, length = #background points>,
#   n_siblings = <integer>); opacity is the base default everywhere when no pin.
# @noRd
pelsa_volcano_pin_opacity <- function(df, accession) {
  if (!is.data.frame(df)) {
    stop("pelsa_volcano_pin_opacity: df must be a data.frame")
  }
  bg <- pelsa_volcano_marker_split(df)$background
  nb <- nrow(bg)
  if (nb == 0L) return(list(opacity = numeric(0), n_siblings = 0L))

  no_pin <- is.null(accession) || length(accession) != 1L || is.na(accession) ||
    !nzchar(accession)
  if (no_pin) {
    return(list(opacity = rep(.PELSA_VOLCANO_BG_ALPHA, nb), n_siblings = 0L))
  }

  sib <- pelsa_volcano_sibling_mask(bg, accession)$siblings
  opacity <- rep(.PELSA_VOLCANO_BG_ALPHA_DIM, nb)
  opacity[sib] <- 1
  list(opacity = opacity, n_siblings = sum(sib))
}
