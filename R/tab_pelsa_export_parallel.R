################################################################################
# PELSA export: parallel render strategy (pure execution machinery; no PELSA
# domain knowledge). Sizes workers to the machine (leaving headroom) and fans a
# side-effecting per-figure render over a self-resetting `multisession` plan.
#
# Kept separate from tab_pelsa_export_helpers.R (already large) so the parallel
# machinery stays independently testable and free of plotting/stat logic.
################################################################################

# Tiered worker ceiling: modest on typical laptops/desktops, higher only on
# many-core workstations. Guards RAM -- each multisession worker is a full R
# process. @noRd
pelsa_export_max_workers <- function(avail = future::availableCores()) {
  if (avail > 16L) 8L else 4L
}

# Resource-aware worker count for a render batch of `n_items` figures:
#   * availableCores() honors cgroup/container/env limits (NOT detectCores())
#   * headroom = avail - 1 always leaves >= 1 core free (machine stays responsive)
#   * clamp to the tiered ceiling and to n_items (never more workers than work)
#   * result >= 1 (1 => future falls back to sequential, no spawn cost)
# @noRd
pelsa_export_workers <- function(n_items) {
  avail    <- future::availableCores()
  headroom <- max(1L, as.integer(avail) - 1L)
  ceiling_ <- pelsa_export_max_workers(avail)
  max(1L, min(headroom, ceiling_, as.integer(n_items)))
}

# Fan a side-effecting per-figure render over parallel workers.
#   items      : list of self-contained work units (each carries everything one
#                figure needs; must be small -- shipped to workers via PSOCK)
#   render_one : function(item) that builds the plot and writes the file. It is
#                SIDE-EFFECTING (return value ignored) and MUST wrap its body in
#                tryCatch(..., error = function(e) NULL) so one bad figure is
#                skipped rather than aborting the batch (matches the pre-parallel
#                per-figure tryCatch in the export bodies).
# Establishes its OWN `multisession` plan via with(plan(...), local = TRUE), which
# auto-restores the caller's prior plan on exit -- even on error -- so a user's
# global future plan is never clobbered (future package-developer pattern). Each
# call spins workers up and down independently.
# Progress: one progressr step per item; furrr relays worker progressions to the
# main session. With no handler registered the steps are silent no-ops.
# Returns invisibly NULL. @noRd
pelsa_export_render_map <- function(items, render_one) {
  if (!length(items)) return(invisible(NULL))
  workers <- pelsa_export_workers(length(items))
  with(future::plan(future::multisession, workers = workers), local = TRUE)
  p <- progressr::progressor(along = items)
  furrr::future_walk(items, function(item) {
    render_one(item)
    p()
  })
  invisible(NULL)
}
