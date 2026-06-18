################################################################################
# Tests for the PELSA Setup section per-dataset scope + ordering (Task 5B).
#
# Covers:
#   - PURE ordering helpers (closed-form): pelsa_distinct_conditions,
#     pelsa_samples_for_condition, pelsa_default_replicate_order,
#     pelsa_merge_ordering, pelsa_build_sample_order.
#   - testServer behavior: per-dataset condition/replicate state, apply-all copy,
#     sample_order in setup_state, observer-dedup registry stays bounded.
################################################################################

library(testthat)

# A tiny cdesc fixture: 2 conditions x 2-3 replicates, rownames = sample names.
#   ctrl: s_ctrl_2, s_ctrl_1   (replicate ids 2,1 -> default sort gives 1 then 2)
#   drug: s_drug_b, s_drug_a, s_drug_c
.ordering_cdesc <- function() {
  data.frame(
    cond = c("ctrl", "ctrl", "drug", "drug", "drug"),
    rep  = c("r2", "r1", "rB", "rA", "rC"),
    row.names = c("s_ctrl_2", "s_ctrl_1", "s_drug_b", "s_drug_a", "s_drug_c"),
    stringsAsFactors = FALSE
  )
}

# ---- pelsa_distinct_conditions -----------------------------------------------

test_that("pelsa_distinct_conditions returns first-seen order, drops NA", {
  cdesc <- .ordering_cdesc()
  expect_identical(pelsa_distinct_conditions(cdesc, "cond"), c("ctrl", "drug"))

  cdesc$cond[2] <- NA
  expect_identical(pelsa_distinct_conditions(cdesc, "cond"), c("ctrl", "drug"))
})

test_that("pelsa_distinct_conditions errors on missing column", {
  expect_error(pelsa_distinct_conditions(.ordering_cdesc(), "nope"),
               "not in cdesc")
})

# ---- pelsa_samples_for_condition ---------------------------------------------

test_that("pelsa_samples_for_condition sorts samples by replicate column", {
  cdesc <- .ordering_cdesc()
  # ctrl: rep ids r2,r1 -> sorted r1,r2 -> samples s_ctrl_1, s_ctrl_2
  expect_identical(
    pelsa_samples_for_condition(cdesc, "cond", "rep", "ctrl"),
    c("s_ctrl_1", "s_ctrl_2")
  )
  # drug: rep ids rB,rA,rC -> sorted rA,rB,rC -> s_drug_a, s_drug_b, s_drug_c
  expect_identical(
    pelsa_samples_for_condition(cdesc, "cond", "rep", "drug"),
    c("s_drug_a", "s_drug_b", "s_drug_c")
  )
})

test_that("pelsa_samples_for_condition returns empty for absent condition", {
  expect_identical(
    pelsa_samples_for_condition(.ordering_cdesc(), "cond", "rep", "ghost"),
    character(0)
  )
})

test_that("pelsa_samples_for_condition falls back to sample-name order when replicate col is unset", {
  # When the condition column is chosen but the replicate column is still the
  # blank "(none)" default (or any non-cdesc value), the function must NOT throw
  # - it falls back to ordering by sample NAME. This is the runtime path when a
  # user sets the condition column before the replicate column.
  cdesc <- .ordering_cdesc()
  # ctrl samples (rownames) sorted by NAME: s_ctrl_1, s_ctrl_2.
  expect_identical(
    pelsa_samples_for_condition(cdesc, "cond", "(none)", "ctrl"),
    c("s_ctrl_1", "s_ctrl_2")
  )
  # drug samples sorted by NAME: s_drug_a, s_drug_b, s_drug_c.
  expect_identical(
    pelsa_samples_for_condition(cdesc, "cond", "(none)", "drug"),
    c("s_drug_a", "s_drug_b", "s_drug_c")
  )
  # NULL / NA replicate col also degrade gracefully (no throw).
  expect_identical(
    pelsa_samples_for_condition(cdesc, "cond", NULL, "ctrl"),
    c("s_ctrl_1", "s_ctrl_2")
  )
})

# ---- pelsa_default_replicate_order -------------------------------------------

test_that("pelsa_default_replicate_order is a named list keyed by condition", {
  cdesc <- .ordering_cdesc()
  def <- pelsa_default_replicate_order(cdesc, "cond", "rep")
  expect_identical(names(def), c("ctrl", "drug"))
  expect_identical(def$ctrl, c("s_ctrl_1", "s_ctrl_2"))
  expect_identical(def$drug, c("s_drug_a", "s_drug_b", "s_drug_c"))
})

# ---- pelsa_merge_ordering ----------------------------------------------------

test_that("pelsa_merge_ordering keeps saved, appends new, drops removed", {
  # saved order [B, A], available [A, B, C] -> keep [B, A], append C
  expect_identical(
    pelsa_merge_ordering(c("B", "A"), c("A", "B", "C")),
    c("B", "A", "C")
  )
  # saved includes a removed item X -> X dropped
  expect_identical(
    pelsa_merge_ordering(c("X", "A"), c("A", "B")),
    c("A", "B")
  )
  # NULL saved -> available order
  expect_identical(pelsa_merge_ordering(NULL, c("A", "B")), c("A", "B"))
  # de-dups saved
  expect_identical(pelsa_merge_ordering(c("A", "A", "B"), c("A", "B")),
                   c("A", "B"))
})

# ---- pelsa_prune_perdataset_state --------------------------------------------

test_that("pelsa_prune_perdataset_state drops unchecked datasets from every field", {
  state <- list(
    condition_col = list(A = "x", B = "y", C = "z"),
    replicate_col = list(A = "x", B = "y", C = "z"),
    replicate_order = list(A = list(c1 = "sa"), B = list(c1 = "sb"),
                           C = list(c1 = "sc")),
    sample_order = list(A = "sa", B = "sb", C = "sc")
  )
  pruned <- pelsa_prune_perdataset_state(state, checked = c("B", "C"))

  # A dropped from EVERY field; B/C retained with their values intact.
  for (f in names(state)) {
    expect_false("A" %in% names(pruned[[f]]), info = f)
    expect_setequal(names(pruned[[f]]), c("B", "C"))
  }
  expect_identical(pruned$condition_col$B, "y")
  expect_identical(pruned$replicate_order$C, list(c1 = "sc"))
})

test_that("pelsa_prune_perdataset_state: empty/NULL fields -> empty list", {
  pruned <- pelsa_prune_perdataset_state(
    list(a = list(), b = NULL, c = list(X = 1)), checked = "Z"
  )
  expect_identical(pruned$a, list())
  expect_identical(pruned$b, list())
  expect_identical(pruned$c, list())  # X not in checked
})

test_that("pelsa_prune_perdataset_state does not mutate its input", {
  state <- list(condition_col = list(A = "x", B = "y"))
  before <- state
  invisible(pelsa_prune_perdataset_state(state, checked = "A"))
  expect_identical(state, before)
})

# ---- pelsa_build_sample_order (closed-form, the canonical contract) ----------

test_that("pelsa_build_sample_order yields the exact ordered sample vector", {
  cdesc <- .ordering_cdesc()
  # condition_order drug-first; within drug pick rep order [c, a, b]; ctrl default.
  out <- pelsa_build_sample_order(
    condition_order = c("drug", "ctrl"),
    replicate_order_by_condition = list(
      drug = c("s_drug_c", "s_drug_a", "s_drug_b")
    ),
    cdesc = cdesc,
    condition_col = "cond",
    replicate_col = "rep"
  )
  expect_identical(
    out,
    c("s_drug_c", "s_drug_a", "s_drug_b",  # drug, explicit order
      "s_ctrl_1", "s_ctrl_2")              # ctrl, default replicate sort
  )
})

test_that("pelsa_build_sample_order: NULL orders -> full natural order", {
  cdesc <- .ordering_cdesc()
  out <- pelsa_build_sample_order(NULL, NULL, cdesc, "cond", "rep")
  expect_identical(
    out,
    c("s_ctrl_1", "s_ctrl_2", "s_drug_a", "s_drug_b", "s_drug_c")
  )
})

test_that("pelsa_build_sample_order reconciles removed/added samples + conditions", {
  cdesc <- .ordering_cdesc()
  # Saved condition order references a condition that no longer exists; saved
  # replicate order references a sample that no longer exists + omits one.
  out <- pelsa_build_sample_order(
    condition_order = c("ghost", "drug"),  # ghost dropped, ctrl appended
    replicate_order_by_condition = list(
      drug = c("s_drug_z", "s_drug_b")     # z dropped; a,c appended in default
    ),
    cdesc = cdesc,
    condition_col = "cond",
    replicate_col = "rep"
  )
  # drug first (saved), ctrl appended; drug: b (saved-kept), then a,c default-appended
  expect_identical(
    out,
    c("s_drug_b", "s_drug_a", "s_drug_c", "s_ctrl_1", "s_ctrl_2")
  )
})

test_that("pelsa_build_sample_order handles a single-replicate condition", {
  cdesc <- data.frame(
    cond = c("a", "b", "b"),
    rep  = c("r1", "r1", "r2"),
    row.names = c("sa1", "sb1", "sb2"),
    stringsAsFactors = FALSE
  )
  out <- pelsa_build_sample_order(c("a", "b"), NULL, cdesc, "cond", "rep")
  expect_identical(out, c("sa1", "sb1", "sb2"))
})

# ---- testServer: per-dataset state + apply-all + sample_order ----------------

.ordering_test_gp <- function() {
  # Two omes with DIFFERENT cdesc column names to exercise per-dataset scope.
  cdesc1 <- data.frame(
    grp = c("ctrl", "ctrl", "drug", "drug"),
    rid = c("2", "1", "B", "A"),
    row.names = c("p_c2", "p_c1", "p_dB", "p_dA"),
    stringsAsFactors = FALSE
  )
  cdesc2 <- data.frame(
    grp = c("ctrl", "drug"),
    rid = c("1", "1"),
    row.names = c("r_c1", "r_d1"),
    stringsAsFactors = FALSE
  )
  mk_gct <- function(cdesc) {
    mat <- matrix(rnorm(2 * nrow(cdesc)), nrow = 2,
                  dimnames = list(c("f1", "f2"), rownames(cdesc)))
    methods::new("GCT", mat = mat,
                 cdesc = cdesc,
                 rdesc = data.frame(id = c("f1", "f2"), stringsAsFactors = FALSE),
                 cid = rownames(cdesc), rid = c("f1", "f2"))
  }
  list(
    GCTs = list(prot = mk_gct(cdesc1), rna = mk_gct(cdesc2)),
    parameters = list(prot = list(annotation_column = NA),
                      rna  = list(annotation_column = NA))
  )
}

test_that("per-dataset condition/replicate columns are stored as named lists", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # No checkbox group: both uploaded datasets are non-skipped (analyzed) by
      # default. Columns default to "(none)" (no auto-seed) - set them per ome.
      session$flushReact()
      expect_setequal(checked_datasets(), c("prot", "rna"))
      expect_true(is.list(setup_state$condition_col))

      # Set the per-dataset column inputs (observers are registered for all
      # non-skipped omes at startup). prot is dataset 1 (_d1), rna is 2 (_d2).
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid",
                        pelsa_condition_col_d2 = "grp",
                        pelsa_replicate_col_d2 = "rid")
      session$flushReact()
      expect_identical(setup_state$condition_col[["prot"]], "grp")
      expect_identical(setup_state$condition_col[["rna"]], "grp")

      # sample_order computed for prot from its chosen columns.
      expect_true(!is.null(setup_state$sample_order[["prot"]]))
      expect_identical(
        sort(setup_state$sample_order[["prot"]]),
        sort(c("p_c2", "p_c1", "p_dB", "p_dA"))
      )
    }
  )
})

test_that("skipping a dataset PRESERVES its per-dataset config (greying only)", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # Both non-skipped by default; give rna a condition column.
      session$flushReact()
      session$setInputs(pelsa_condition_col_d2 = "grp",
                        pelsa_replicate_col_d2 = "rid")
      session$flushReact()
      expect_identical(setup_state$condition_col[["rna"]], "grp")

      # SKIP rna: it leaves the analyzed (checked) set, BUT its config column
      # choices are PRESERVED (greying is purely visual; un-skip restores it).
      setup_state$skip <- list(rna = TRUE)
      session$flushReact()
      expect_false("rna" %in% checked_datasets())
      expect_identical(setup_state$condition_col[["rna"]], "grp")
      expect_identical(setup_state$replicate_col[["rna"]], "rid")
    }
  )
})

test_that("a new upload removing a dataset prunes its per-dataset state", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()
      session$setInputs(pelsa_condition_col_d2 = "grp",
                        pelsa_replicate_col_d2 = "rid")
      session$flushReact()
      expect_true("rna" %in% names(setup_state$condition_col))

      # A NEW upload drops rna from the uploaded set (all_omes()) -> its
      # per-dataset state is pruned (keyed off all_omes(), not skip).
      GCTs_and_params(list(GCTs = list(prot = gp$GCTs$prot),
                           parameters = list(prot = gp$parameters$prot)))
      session$flushReact()
      expect_false("rna" %in% names(setup_state$condition_col))
      expect_false("rna" %in% names(setup_state$sample_order))
    }
  )
})

test_that("changing a dataset's condition_col reseeds its condition order", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()
      # Choose grp -> conditions ctrl, drug.
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()
      expect_setequal(setup_state$condition_order[["prot"]], c("ctrl", "drug"))

      # Switch cond col to rid -> conditions become the rid values.
      session$setInputs(pelsa_condition_col_d1 = "rid")
      session$flushReact()
      expect_setequal(setup_state$condition_order[["prot"]],
                      unique(gp$GCTs$prot@cdesc$rid))
    }
  )
})

test_that("apply-all copies source dataset config to compatible datasets", {
  gp <- .ordering_test_gp()  # both have cols grp, rid -> compatible
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()  # both datasets non-skipped by default
      # Source (active=prot): set species/compound/cols explicitly.
      session$setInputs(pelsa_species = "9606",
                        pelsa_compound = "Rapamycin",
                        pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()

      # Give the source a non-default condition order.
      session$setInputs(pelsa_condition_order_d1 = c("drug", "ctrl"))
      session$flushReact()

      session$setInputs(pelsa_apply_all = 1)
      session$flushReact()

      # Species / compound / markers transfer VERBATIM.
      expect_identical(setup_state$species[["rna"]], "9606")
      expect_identical(setup_state$compound[["rna"]], "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows[["rna"]]),
                   nrow(setup_state$marker_rows[["prot"]]))
      # Condition + replicate COLUMNS transfer (column names are shared).
      expect_identical(setup_state$condition_col[["rna"]], "grp")
      expect_identical(setup_state$replicate_col[["rna"]], "rid")
      # Condition ORDER transfers (condition VALUES are shared across datasets).
      expect_identical(setup_state$condition_order[["rna"]], c("drug", "ctrl"))

      # Honest apply-all: replicate ordering uses each dataset's own default - so
      # rna's replicate_order references ITS OWN samples (r_c1 / r_d1), never the
      # source's (p_c1 / p_c2 / p_dA / p_dB).
      rna_rep <- setup_state$replicate_order[["rna"]]
      rna_samples <- unlist(rna_rep, use.names = FALSE)
      src_samples <- c("p_c1", "p_c2", "p_dA", "p_dB")
      expect_false(any(src_samples %in% rna_samples))
      expect_true(all(rna_samples %in% c("r_c1", "r_d1")))
    }
  )
})

test_that("observer-dedup registry stays bounded across repeated re-renders", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # Choose prot's condition/replicate columns so its multi-replicate
      # conditions exist (columns default to "(none)" now -> no cond observers
      # until a real column is chosen).
      session$flushReact()
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()

      # Toggle the analyzed (non-skipped) set many times via the per-ome skip
      # flag (the checkbox group is gone; checked_datasets() derives from skip).
      for (k in 1:6) {
        setup_state$skip <- list()            # both analyzed
        session$flushReact()
        setup_state$skip <- list(rna = TRUE)  # rna skipped
        session$flushReact()
      }
      reg <- setup_observer_registry()
      # Registry is keyed by dataset/condition INDEX, so it is bounded by the
      # number of distinct datasets+conditions regardless of toggle count.
      expect_lt(length(reg), 12L)
      # And it must NOT have grown unboundedly (no per-toggle duplication).
      expect_true(length(reg) == length(unique(reg)))

      # prot has 2 MULTI-replicate conditions (ctrl=2, drug=2) -> 2 cond keys.
      # rna's conditions are SINGLE-replicate -> NO cond observers registered
      # (item 3: single-rep conditions collapse to a static label, no controls).
      cond_keys <- grep("_cond_", reg, value = TRUE)
      expect_setequal(cond_keys, c("ds_1_cond_1", "ds_1_cond_2"))
    }
  )
})

test_that("apply-all (button) leaves a target dataset editable afterward", {
  gp <- .ordering_test_gp()  # both compatible (cols grp, rid)
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()  # both datasets non-skipped
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()

      session$setInputs(pelsa_apply_all = 1)
      session$flushReact()
      # apply-all (a one-shot button) copied prot's cols to rna.
      expect_identical(setup_state$condition_col[["rna"]], "grp")

      # After apply-all, a direct edit to the TARGET dataset (rna, index 2) still
      # takes hold (the button does not lock the target).
      session$setInputs(pelsa_condition_col_d2 = "rid")
      session$flushReact()
      expect_identical(setup_state$condition_col[["rna"]], "rid")
    }
  )
})

# ---- REGRESSION: the setup_state SEAM (CRITICAL) -----------------------------
# The Tab server must return setup_state as a REACTIVE that yields a plain
# snapshot LIST (not the bare reactiveValues). The consumers (Sections 2 & 3)
# guard with is.function() and CALL the seam with (); a bare reactiveValues is
# NOT a function, so it would be silently downgraded to reactive(NULL) in
# production (markers / ordering / species lost). This test would have caught
# that: it asserts the returned seam is.function() AND that calling it exposes
# the live setup_state fields.
test_that("Tab server returns setup_state as a reactive yielding the live snapshot", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()  # prot non-skipped by default
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid",
                        pelsa_species = "homo_sapiens")
      session$flushReact()

      # Set a marker so we can prove it flows through the seam (per-ome).
      session$setInputs(pelsa_marker_input = "P12345")
      session$setInputs(pelsa_add_markers = 1)
      session$flushReact()

      seam <- session$returned$setup_state

      # (1) The seam is a FUNCTION / reactive (is.function TRUE)  -  the exact
      # property the consumer is.function() guards on. A bare reactiveValues
      # would FAIL this and be downgraded to reactive(NULL) in production.
      expect_true(is.function(seam))

      # (2) Calling it yields a plain LIST populated from the live reactiveValues.
      snap <- seam()
      expect_true(is.list(snap))
      expect_false(shiny::is.reactivevalues(snap))

      # (3) Every field the consumers read is present + populated from live state
      # (species / marker_rows are now PER-OME named lists).
      expect_identical(snap$species[["prot"]], "homo_sapiens")
      expect_identical(snap$condition_col[["prot"]], "grp")
      expect_setequal(snap$condition_order[["prot"]], c("ctrl", "drug"))
      expect_true(!is.null(snap$sample_order[["prot"]]))
      expect_true("P12345" %in% snap$marker_rows[["prot"]]$accession)
    }
  )
})

# ---- REGRESSION: H1  -  replicate order survives a condition-column switch -----
# Switching a dataset's condition column must RE-WIRE the per-condition replicate
# observers to the NEW column's conditions, so user replicate ordering for the
# new column is RETAINED (not silently dropped to default because a stale
# positional observer wrote to the OLD condition value).
test_that("H1: replicate order is retained after switching the condition column", {
  # cdesc where BOTH candidate condition columns yield multi-replicate conditions
  # so the per-condition replicate observers exist under each column.
  cdesc <- data.frame(
    colA = c("a1", "a1", "a2", "a2"),  # 2 conds x 2 reps
    colB = c("b1", "b2", "b1", "b2"),  # 2 conds x 2 reps (orthogonal split)
    rid  = c("r1", "r2", "r1", "r2"),
    row.names = c("s1", "s2", "s3", "s4"),
    stringsAsFactors = FALSE
  )
  mat <- matrix(rnorm(2 * nrow(cdesc)), nrow = 2,
                dimnames = list(c("f1", "f2"), rownames(cdesc)))
  gct <- methods::new("GCT", mat = mat, cdesc = cdesc,
                      rdesc = data.frame(id = c("f1", "f2"),
                                         stringsAsFactors = FALSE),
                      cid = rownames(cdesc), rid = c("f1", "f2"))
  gp <- list(GCTs = list(prot = gct),
             parameters = list(prot = list(annotation_column = NA)))

  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot", colors = list(prot = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()

      # Column A: conditions a1 (s1,s2), a2 (s3,s4). Set a custom replicate order
      # for a1's card (position j=1): reverse it to s2, s1.
      session$setInputs(pelsa_condition_col_d1 = "colA",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()
      session$setInputs(pelsa_replicate_order_d1_c1 = c("s2", "s1"))
      session$flushReact()
      expect_identical(setup_state$replicate_order[["prot"]][["a1"]],
                       c("s2", "s1"))

      # Switch to column B: conditions b1 (s1,s3), b2 (s2,s4). Set a custom
      # replicate order for b1's card (position j=1): reverse to s3, s1.
      session$setInputs(pelsa_condition_col_d1 = "colB")
      session$flushReact()
      session$setInputs(pelsa_replicate_order_d1_c1 = c("s3", "s1"))
      session$flushReact()

      # H1: the NEW column's replicate order is RETAINED under the NEW condition
      # name b1 (NOT silently written to the stale a1 / dropped to default).
      expect_identical(setup_state$replicate_order[["prot"]][["b1"]],
                       c("s3", "s1"))
      # And the canonical sample order honors it (b1 first if condition order
      # keeps b1 first; assert b1's samples appear reversed within the order).
      so <- setup_state$sample_order[["prot"]]
      expect_true(which(so == "s3") < which(so == "s1"))

      # Switch BACK to A and confirm no corruption: a fresh order takes hold.
      session$setInputs(pelsa_condition_col_d1 = "colA")
      session$flushReact()
      session$setInputs(pelsa_replicate_order_d1_c1 = c("s1", "s2"))
      session$flushReact()
      expect_identical(setup_state$replicate_order[["prot"]][["a1"]],
                       c("s1", "s2"))

      # No observer leak across the switches: the registry is bounded + unique.
      reg <- setup_observer_registry()
      expect_true(length(reg) == length(unique(reg)))
      # Only 2 positions ever (i=1, j in {1,2}) -> at most 2 cond keys.
      cond_keys <- grep("_cond_", reg, value = TRUE)
      expect_true(length(cond_keys) <= 2L)
    }
  )
})

test_that("an NA condition value is dropped from the wired sample_order", {
  # cdesc with an NA in the condition column: the NA-row sample must NOT appear
  # in the canonical sample_order (the pure helper drops NA; assert the SERVER
  # path too).
  cdesc <- data.frame(
    grp = c("ctrl", NA, "drug", "drug"),
    rid = c("1", "1", "A", "B"),
    row.names = c("s_ctrl", "s_na", "s_dA", "s_dB"),
    stringsAsFactors = FALSE
  )
  mat <- matrix(rnorm(2 * nrow(cdesc)), nrow = 2,
                dimnames = list(c("f1", "f2"), rownames(cdesc)))
  gct <- methods::new("GCT", mat = mat, cdesc = cdesc,
                      rdesc = data.frame(id = c("f1", "f2"),
                                         stringsAsFactors = FALSE),
                      cid = rownames(cdesc), rid = c("f1", "f2"))
  gp <- list(GCTs = list(prot = gct),
             parameters = list(prot = list(annotation_column = NA)))

  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()

      so <- setup_state$sample_order[["prot"]]
      expect_false("s_na" %in% so)  # NA-condition sample dropped
      expect_setequal(so, c("s_ctrl", "s_dA", "s_dB"))
    }
  )
})
