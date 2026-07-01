# Finding 2 regression guard: the PELSA volcano gold-overlay (overlay_n) must be
# reset whenever the base figure is rebuilt. The base figure rebuilds both on
# display-control changes (color mode, contrast, label mode, top-N, WebGL flip)
# AND on anything that clears volcano_df_cache (marker-add, significance cutoff,
# significance stat). If a cache-clearing reason is NOT also an overlay-reset
# reason, overlay_n goes stale and the next apply_gold_overlay() issues
# deleteTraces against trace indices that no longer exist (dropping the markers
# trace / erroring).
#
# .pelsa_volcano_cache_clear_reasons() and .pelsa_volcano_overlay_reset_reasons()
# are the single source of truth the section3 observers key off, so this pure
# invariant prevents the two lists from drifting apart again.

test_that("every cache-clear reason is also an overlay-reset reason", {
  clear <- .pelsa_volcano_cache_clear_reasons()
  reset <- .pelsa_volcano_overlay_reset_reasons()
  missing <- setdiff(clear, reset)
  expect_identical(missing, character(0))
})

test_that("marker / cutoff / stat changes are among the overlay-reset reasons", {
  reset <- .pelsa_volcano_overlay_reset_reasons()
  expect_true(all(c("markers", "sig_cutoff", "sig_stat") %in% reset))
})

# Anchor the pure invariant to the REAL observer: the base-rebuild observeEvent
# in PELSASection3_Ome_Server must actually depend on the cache-clearing
# reactives (marker_accessions / sig_cutoff_r / sig_stat_r). Without this, the
# reason-list helpers would be decorative and the observer could silently drop a
# trigger (regressing the gold-overlay deleteTraces-on-absent-index bug) while
# both pure tests above still pass. We deparse the function body and assert the
# overlay-reset observer's trigger expression mentions each reactive.
test_that("the base-rebuild observer's TRIGGER LIST has the cache-clear reactives", {
  # Walk the parsed module body, find the observeEvent whose HANDLER contains
  # overlay_n(0L) (the base-rebuild/overlay-reset observer), and assert its FIRST
  # argument (the trigger expression) references each cache-clear reactive. This
  # targets the trigger list specifically, not just any mention in the body.
  found <- local({
    hit <- NULL
    walk <- function(e) {
      if (is.call(e)) {
        if (identical(e[[1]], as.name("observeEvent")) && length(e) >= 3L) {
          handler_txt <- paste(deparse(e[[3L]]), collapse = " ")
          if (grepl("overlay_n\\(0L\\)", handler_txt)) {
            hit <<- paste(deparse(e[[2L]]), collapse = " ")  # the trigger expr
          }
        }
        for (i in seq_along(e)) walk(e[[i]])
      }
    }
    walk(body(PELSASection3_Ome_Server))
    hit
  })
  expect_false(is.null(found))  # the base-rebuild observer exists
  for (dep in c("marker_accessions\\(\\)", "sig_cutoff_r\\(\\)",
                "sig_stat_r\\(\\)")) {
    expect_true(grepl(dep, found),
                info = paste("overlay-reset trigger list must include", dep))
  }
})
