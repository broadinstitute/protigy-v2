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

# Can we safely use multisession (PSOCK) workers? Only when the package is
# INSTALLED -- a fresh worker R session `library()`s the installed package to
# resolve the namespaced calls inside render_one. Under devtools::load_all()
# the functions live only in the main session's memory, so workers cannot see
# them and the parallel batch would throw. In that case we render sequentially
# in-process instead (still correct, just not parallel). NOTE: system.file() /
# find.package() are NOT reliable here (they return the source dir under
# load_all); installed.packages() is the true "workers can library() it" test.
# @noRd
pelsa_export_can_parallelize <- function() {
  "Protigy" %in% rownames(utils::installed.packages())
}

# Fan a side-effecting per-figure render over parallel workers, or -- when the
# package cannot be parallelized (not installed; see pelsa_export_can_parallelize())
# or there is only one worker available -- run render_one sequentially in-process.
#   items      : list of self-contained work units (each carries everything one
#                figure needs; must be small -- shipped to workers via PSOCK)
#   render_one : function(item) that builds the plot and writes the file. It is
#                SIDE-EFFECTING (return value ignored) and MUST wrap its body in
#                tryCatch(..., error = function(e) NULL) so one bad figure is
#                skipped rather than aborting the batch (matches the pre-parallel
#                per-figure tryCatch in the export bodies).
# Parallel path establishes its OWN `multisession` plan via with(plan(...),
# local = TRUE), which auto-restores the caller's prior plan on exit -- even on
# error -- so a user's global future plan is never clobbered (future
# package-developer pattern). Each call spins workers up and down independently.
# Sequential path sets NO plan at all (the caller's plan, if any, is left
# untouched) and simply loops render_one in-process -- this is what keeps
# devtools::load_all() dev sessions correct (see pelsa_export_can_parallelize()).
# Progress: one progressr step per item on BOTH paths; furrr relays worker
# progressions to the main session on the parallel path. With no handler
# registered the steps are silent no-ops.
# Returns invisibly NULL. @noRd
pelsa_export_render_map <- function(items, render_one) {
  if (!length(items)) return(invisible(NULL))
  p <- progressr::progressor(along = items)
  workers <- pelsa_export_workers(length(items))
  # Parallel only when the package is installed (PSOCK workers can library it)
  # AND there is more than one worker to use. Otherwise render in-process: this
  # keeps dev (load_all) + single-core + tiny-batch runs correct and fast, and
  # never leaves a silently-empty export.
  if (pelsa_export_can_parallelize() && workers > 1L) {
    with(future::plan(future::multisession, workers = workers), local = TRUE)
    furrr::future_walk(items, function(item) {
      render_one(item)
      p()
    })
  } else {
    for (item in items) {
      render_one(item)
      p()
    }
  }
  invisible(NULL)
}
