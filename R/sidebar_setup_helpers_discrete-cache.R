################################################################################
# Module: SETUP SIDEBAR — discrete-column cache (INT-2)
#
# gctSetupUI() runs inside a renderUI body, so it re-derives its dropdown
# choices from scratch on every panel rebuild. Two of those derivations scan
# every annotation column with is.discrete():
#   - groups_choices       : is.discrete() over @cdesc columns
#   - row_filter_columns   : is.discrete() over @rdesc columns
# On phospho-scale rdesc (~34k rows, high-cardinality string columns) the rdesc
# scan dominates the rebuild, and the rebuild fires far more often than the GCTs
# actually change (see INT-1: the Intensity toggle re-invalidates the panel).
#
# The discrete-column set is a PURE FUNCTION of the annotation table, so it is
# safe to memoize. The critical safety property to preserve is the one the
# original recompute-every-render design provided for free: it could NEVER show
# stale choices. When a file is re-uploaded / reprocessed and its columns change,
# the original always re-scanned the fresh table.
#
# We preserve that guarantee by deriving the cache through a Shiny reactive that
# depends on GCTs_unprocessed_internal_reactive(). Both upload paths (GCT parse
# AND CSV/Excel conversion) terminate by WRITING that reactiveVal, so any
# upload / removal / reprocess invalidates the reactive and forces a fresh scan.
# The reactive's value is therefore always consistent with the current GCTs —
# the cache only avoids RE-SCANNING when nothing about the GCTs changed (e.g. an
# Intensity-toggle rebuild). On a real change, the reactive recomputes.
#
# gctSetupUI() stays a plain (non-reactive) function for testability: it accepts
# an optional precomputed `discrete_columns` map and falls back to computing the
# scan inline when that map is absent (NULL) or missing the requested label —
# so behavior is identical whether or not the cache is wired in.
################################################################################

# Un-memoized core: discrete column names for one annotation table (rdesc/cdesc).
# Returns exactly the columns for which is.discrete() is TRUE, in original order.
compute_discrete_columns <- function(df) {
  all_cols <- names(df)
  if (length(all_cols) == 0L) {
    return(character(0))
  }
  all_cols[vapply(df[all_cols], function(col) is.discrete(col), logical(1))]
}

# Build the per-label discrete-column map for an entire GCTs list.
# Returns a named list: map[[label]] = list(rdesc = <cols>, cdesc = <cols>).
# This is the value cached by the server-side reactive (computed once whenever
# the GCTs reactiveVal changes).
build_discrete_columns_map <- function(GCTs) {
  if (is.null(GCTs) || length(GCTs) == 0L) {
    return(list())
  }
  out <- lapply(GCTs, function(gct) {
    list(
      rdesc = compute_discrete_columns(gct@rdesc),
      cdesc = compute_discrete_columns(gct@cdesc)
    )
  })
  names(out) <- names(GCTs)
  out
}

# Resolve discrete columns for a single (label, slot) inside gctSetupUI.
# Uses the precomputed map when it contains a fresh entry for this label/slot;
# otherwise computes inline from `df`. The inline fallback guarantees that:
#   (a) calling gctSetupUI without a map (tests / non-Shiny) still works, and
#   (b) a label not yet present in the map (e.g. a freshly added file before the
#       reactive recomputed) never shows STALE/EMPTY choices — it computes fresh.
# `df` MUST be the live annotation table for (label, slot) so the fallback is
# always correct.
resolve_discrete_columns <- function(discrete_columns, label, slot, df) {
  entry <- if (!is.null(discrete_columns)) discrete_columns[[label]] else NULL
  cached <- if (!is.null(entry)) entry[[slot]] else NULL
  # NOTE: a legitimately-empty result is character(0), NOT NULL. Only NULL means
  # "no cache entry for this (label, slot)" -> fall back to a live scan. A cached
  # character(0) (a table with no discrete columns) is a valid hit and is returned
  # as-is. Do not collapse these two cases in any future refactor.
  if (!is.null(cached)) {
    return(cached)
  }
  compute_discrete_columns(df)
}
