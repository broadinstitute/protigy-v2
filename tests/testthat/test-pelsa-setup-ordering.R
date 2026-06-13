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
      session$setInputs(pelsa_datasets = c("prot", "rna"))
      session$flushReact()

      # Per-dataset defaults populated for BOTH datasets.
      expect_true(is.list(setup_state$condition_col))
      expect_true(!is.null(setup_state$condition_col[["prot"]]))
      expect_true(!is.null(setup_state$condition_col[["rna"]]))

      # sample_order computed for both.
      expect_true(!is.null(setup_state$sample_order[["prot"]]))
      expect_identical(
        sort(setup_state$sample_order[["prot"]]),
        sort(c("p_c2", "p_c1", "p_dB", "p_dA"))
      )
    }
  )
})

test_that("unchecking a dataset prunes its per-dataset state", {
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
      session$setInputs(pelsa_datasets = c("prot", "rna"))
      session$flushReact()
      expect_true("rna" %in% names(setup_state$condition_col))

      session$setInputs(pelsa_datasets = "prot")
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
      session$setInputs(pelsa_datasets = "prot")
      session$flushReact()
      # Default cond col is grp -> conditions ctrl, drug.
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
      session$setInputs(pelsa_datasets = c("prot", "rna"))
      session$flushReact()
      # Source (active=prot): set cols explicitly.
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()

      session$setInputs(pelsa_apply_all = TRUE)
      session$flushReact()

      expect_identical(setup_state$condition_col[["rna"]], "grp")
      expect_identical(setup_state$replicate_col[["rna"]], "rid")
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
      # Toggle the dataset set many times.
      for (k in 1:6) {
        session$setInputs(pelsa_datasets = c("prot", "rna"))
        session$flushReact()
        session$setInputs(pelsa_datasets = "prot")
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

test_that("apply-all auto-unticks so a target dataset stays editable afterward", {
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
      session$setInputs(pelsa_datasets = c("prot", "rna"))
      session$flushReact()
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()

      session$setInputs(pelsa_apply_all = TRUE)
      session$flushReact()
      # apply-all copied prot's cols to rna.
      expect_identical(setup_state$condition_col[["rna"]], "grp")

      # The server calls updateCheckboxInput(..., value = FALSE) to auto-untick
      # (no lock-out). In testServer that does not echo back into input$, so we
      # assert the REAL anti-lockout property instead: after apply-all, a direct
      # edit to the TARGET dataset (rna, index 2) still takes hold.
      session$setInputs(pelsa_condition_col_d2 = "rid")
      session$flushReact()
      expect_identical(setup_state$condition_col[["rna"]], "rid")
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
      session$setInputs(pelsa_datasets = "prot")
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
