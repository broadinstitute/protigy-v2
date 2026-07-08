################################################################################
# PELSA Volcano section (Section 3) - reactive server-wiring helper.
#
# Extracted from PELSASection3_Ome_Server() (see tab_pelsa_section3.R) to keep
# that function under the file's coding-style size budget. This is NOT pure
# logic (it closes over live reactives/closures from the module server), so it
# lives alongside the tab server rather than in tab_pelsa_export_helpers.R
# (which holds the pure per-export body functions this wiring calls into).
#
# pelsa_wire_section3_exports() wires the 7F EXPORTS block: given the ome +
# every reactive/closure the three per-ome export bodies
# (pelsa_section3_export_volcano/_intensity/_woods, in
# R/tab_pelsa_export_helpers.R) need, it returns the
# list(volcano=, intensity=, woods=) of zero-arg-of-dir_name export functions
# that PELSASection3_Ome_Server attaches to its returned export list. Each
# closure re-derives its output from the cache + stat_results at EXPORT TIME
# (never the on-screen rendered object), matching the export contract in
# dev/module_requirements.md.
#
# pelsa_wire_label_mode_exclusion() wires the two-INDEPENDENT-pairs mutual-
# exclusion observer on the "Label peptides" checkbox group (checking
# "top_n_adjp" unchecks+disables "all_significant" and vice versa; same for
# "top_n_markers"/"all_markers"). Pure side-effect wiring against the passed
# `input`/`session`/`ns` - no reactive value is returned.
#
# pelsa_render_volcano_sidebar() builds the "Plot Controls" sidebar UI
# (contrast selector, find/clear, color-mode toggle, label-mode checkboxes +
# top-N inputs, best-panel toggle, color-key legend). Pure UI assembly off the
# passed-in reactive VALUES (already resolved, not reactives) plus `ns()`; no
# input/output/session side effects, so it is called from inside a renderUI().
#
# pelsa_build_pinned_woods() is the pure computation body of the 7G pinned
# protein COVERAGE + FEATURE + WOODS panel (the L's bottom arm): given the
# pinned accession + the active contrast's cache entry/stat_df_raw/feat_df +
# significance settings, it returns list(pep=, lanes=, intervals=, prot_len=,
# coverage_frac=). No reactive read/write of its own - PELSASection3_Ome_Server
# calls it from inside its `pinned_woods <- reactive({...})` wrapper, which
# supplies the resolved values (this keeps the req()/reactive dependency
# tracking in the module server, where testServer() can exercise it).
#
# pelsa_render_section3_layout() builds the L-SHAPED PINNED CARD markup: the
# upper-left arm (pinned metadata + intensity line plot), the top-right
# volcano (+ best-peptide panel) notch, the Plot Controls sidebar slot, and
# the full-width bottom coverage/feature/Woods track. Pure UI assembly off
# `ns()` plus the already-resolved `show_best_panel` flag (which gates the
# best-peptide conditionalPanel's initial condition string); no
# input/output/session side effects, called from inside output$section_contents.
#
# pelsa_build_volcano_df_cached() is the shared build-and-cache body for BOTH
# the 7C all-peptide active_volcano_df() and the 7D best-peptide
# best_volcano_df() reactives: given the resolved contrast/cache
# entry/feat_df/markers/significance settings + the panel ("all_peptide" |
# "best_peptide") + the CURRENT cache list + a notification label, it returns
# list(df=, cache=) - the built (or cache-hit) df and the cache list to store
# back (a single-entry list keyed by contrast, freeing the prior contrast's
# df). A build failure shows a notification and signals via
# validate(need(...)) same as the inline bodies did, so callers keep the same
# shiny::validate() gate behavior.
################################################################################

# @noRd
pelsa_wire_section3_exports <- function(ome,
                                        stat_results,
                                        cache_entry,
                                        feat_df,
                                        marker_accessions,
                                        color_mode,
                                        label_mode_for_ome,
                                        n_top_adjp_for_ome,
                                        n_top_markers_for_ome,
                                        best_show,
                                        sig_cutoff_r,
                                        sig_stat_r,
                                        is_self_curated_r,
                                        contrast_choices,
                                        processed_mat_r,
                                        condition_map_r,
                                        condition_order_r,
                                        parameters) {

  # Common tryCatch wrapper: log the failure (with the ome + a label) and
  # no-op so one bad export never aborts the whole zip.
  safe_export <- function(label, body) function(dir_name) tryCatch(
    body(dir_name),
    error = function(e) {
      message("PELSA ", label, " export failed for ", ome, ": ",
              conditionMessage(e))
      invisible(NULL)
    })

  # ---- 03_volcano/01_volcano : one volcano per contrast (all + best) -------
  # Coloring follows input$pelsa_color_mode; labels follow the ome-level
  # label mode (baked into the static ggplot); markers magenta; NO gold. The
  # body lives in pelsa_section3_export_volcano() (R/tab_pelsa_export_helpers.R)
  # -- this closure only gathers the current reactive values.
  export_volcano <- safe_export("volcano figures", function(dir_name) {
    pelsa_section3_export_volcano(
      dir_name          = dir_name,
      ome               = ome,
      stat_results      = stat_results(),
      cache_entry       = cache_entry(),
      feat_df           = feat_df(),
      marker_accessions = isolate(marker_accessions()),
      color_mode        = isolate(color_mode()) %||% "significance",
      # Label mode is ome-scoped: read the ONE stored selection for this ome
      # once, reused for every contrast (previously a per-contrast registry
      # lookup inside the loop, back when label mode varied by contrast).
      label_mode        = isolate(label_mode_for_ome()),
      n_top_adjp        = isolate(n_top_adjp_for_ome()),
      n_top_markers     = isolate(n_top_markers_for_ome()),
      want_best         = isTRUE(isolate(best_show())),
      # Single significance threshold for the whole export: drives the df
      # build (Significant / sig_direction / dashed y_cutoff) AND the
      # annotation text, so the dashed-line label always matches the cutoff
      # in force. Sourced from the SAME user-set cutoff as the in-app
      # volcano (Statistics > Summary), so the export mirrors exactly what
      # the user sees on screen.
      sig_cutoff        = isolate(sig_cutoff_r()),
      sig_stat          = isolate(sig_stat_r()),
      self_curated      = isolate(is_self_curated_r()),
      contrast_choices  = contrast_choices()
    )
  })

  # ---- 03_volcano/02_intensity_line : per protein (marker | significant) ---
  # Contrast-independent: one figure per protein. The significant set + the
  # panel split use the union-across-contrasts adj.P (synthetic min column).
  # Body lives in pelsa_section3_export_intensity().
  export_intensity <- safe_export("intensity figures", function(dir_name) {
    pelsa_section3_export_intensity(
      dir_name           = dir_name,
      ome                = ome,
      stat_results       = stat_results(),
      cache_entry        = cache_entry(),
      processed_mat      = isolate(processed_mat_r()),
      condition_map      = isolate(condition_map_r()),
      condition_order    = isolate(condition_order_r()),
      # Use the SAME user-set cutoff as the on-screen intensity panel so the
      # exported Significant/Non-significant split matches what the user sees.
      sig_cutoff         = isolate(sig_cutoff_r()),
      sig_stat           = isolate(sig_stat_r()),
      marker_accessions  = isolate(marker_accessions()),
      # y-axis label log base reflects this dataset's declared transform so a
      # log10 dataset is not mislabeled "log2(intensity)".
      log_transformation = isolate(parameters())$log_transformation
    )
  })

  # ---- 03_volcano/03_woods : per (protein x contrast), marker | significant -
  # Body lives in pelsa_section3_export_woods().
  export_woods <- safe_export("woods figures", function(dir_name) {
    pelsa_section3_export_woods(
      dir_name          = dir_name,
      ome               = ome,
      stat_results      = stat_results(),
      cache_entry       = cache_entry(),
      feat_df           = feat_df(),
      # Use the SAME user-set cutoff as the on-screen Woods panel.
      sig_cutoff        = isolate(sig_cutoff_r()),
      sig_stat          = isolate(sig_stat_r()),
      marker_accessions = isolate(marker_accessions()),
      contrast_choices  = contrast_choices()
    )
  })

  list(volcano = export_volcano, intensity = export_intensity,
       woods = export_woods)
}

# @noRd
pelsa_wire_label_mode_exclusion <- function(input, session, ns) {
  # Mutual exclusion, two INDEPENDENT pairs (adjp pair does not
  # affect the marker pair): checking "top_n_adjp" unchecks+disables
  # "all_significant" and vice versa; checking "top_n_markers"
  # unchecks+disables "all_markers" and vice versa. Mirrors the pattern in
  # R/tab_stat_plot.R:315-350 (a different subsystem's volcano, same idiom).
  # Only push a corrected selection when it actually differs from what the
  # client already holds. In a live browser every updateCheckboxGroupInput()
  # round-trips back as a NEW input$pelsa_label_mode value and re-fires THIS
  # observer; re-sending an unchanged selection is a self-perpetuating engine
  # -- combined with a user's fast second click (arriving mid round-trip) the
  # two value streams never reconcile and the checkbox blinks on/off forever.
  # Sending only genuine corrections makes a conflict-free selection emit
  # nothing, so the loop has no engine and settles in exactly one bounce
  # regardless of click speed. `runjs()` for the disabled/opacity styling is
  # idempotent (it just re-asserts the DOM state) and never re-fires the
  # observer, so it stays unconditional.
  update_if_changed <- function(new_sel) {
    if (!setequal(new_sel, input$pelsa_label_mode %||% character(0))) {
      updateCheckboxGroupInput(session, "pelsa_label_mode", selected = new_sel)
    }
  }

  observeEvent(input$pelsa_label_mode, {
    grp_id <- ns("pelsa_label_mode")
    modes <- input$pelsa_label_mode %||% character(0)

    if ("top_n_adjp" %in% modes) {
      update_if_changed(setdiff(modes, "all_significant"))
      shinyjs::runjs(sprintf(
        "$('#%s input[value=\"all_significant\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
        grp_id))
    } else if ("all_significant" %in% modes) {
      update_if_changed(setdiff(modes, "top_n_adjp"))
      shinyjs::runjs(sprintf(
        "$('#%s input[value=\"top_n_adjp\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
        grp_id))
    } else {
      shinyjs::runjs(sprintf(
        "$('#%s input[value=\"all_significant\"], #%s input[value=\"top_n_adjp\"]').prop('disabled', false).closest('label').css('opacity', 1);",
        grp_id, grp_id))
    }

    if ("top_n_markers" %in% modes) {
      update_if_changed(setdiff(modes, "all_markers"))
      shinyjs::runjs(sprintf(
        "$('#%s input[value=\"all_markers\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
        grp_id))
    } else if ("all_markers" %in% modes) {
      update_if_changed(setdiff(modes, "top_n_markers"))
      shinyjs::runjs(sprintf(
        "$('#%s input[value=\"top_n_markers\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
        grp_id))
    } else {
      shinyjs::runjs(sprintf(
        "$('#%s input[value=\"all_markers\"], #%s input[value=\"top_n_markers\"]').prop('disabled', false).closest('label').css('opacity', 1);",
        grp_id, grp_id))
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  invisible(NULL)
}

# @noRd
pelsa_render_volcano_sidebar <- function(ns,
                                         contrast_choices,
                                         active_contrast,
                                         is_self_curated,
                                         label_mode_for_ome,
                                         n_top_adjp_for_ome,
                                         n_top_markers_for_ome,
                                         color_mode_for_ome = "significance",
                                         show_best_panel_for_ome = FALSE) {
  tagList(
    selectInput(
      ns("pelsa_volcano_contrast"), "Select Contrast:",
      choices  = contrast_choices,                 # named: label -> suffix
      selected = active_contrast
    ),
    hr(),
    tags$strong("Find / highlight a protein:"),
    textInput(ns("pelsa_find_acc"), label = NULL,
              placeholder = "accession e.g. P12345"),
    actionButton(ns("pelsa_find_go"), "Highlight", class = "btn-sm"),
    actionButton(ns("pelsa_clear_sel"), "Clear selection & highlight",
                 class = "btn-sm"),
    uiOutput(ns("pelsa_find_notice")),
    hr(),
    # SINGLE color toggle (one source of truth) - NOT two checkboxes.
    # Self-curated species have no UniProt feature classes: disable the
    # feature option and force Significance. Gated on the RESOLVED TYPE.
    if (isTRUE(is_self_curated)) {
      tagList(
        radioButtons(
          ns("pelsa_color_mode"), "Color points by:",
          choices = c("Significance (two-sided)" = "significance"),
          selected = "significance"
        ),
        helpText("UniProt feature-class coloring is unavailable for a ",
                 "self-curated database.")
      )
    } else {
      radioButtons(
        ns("pelsa_color_mode"), "Color points by:",
        choices = c("Significance (two-sided)" = "significance",
                    "UniProt feature class"     = "feature"),
        selected = color_mode_for_ome
      )
    },
    hr(),
    strong("Label peptides:"),
    checkboxGroupInput(
      ns("pelsa_label_mode"), label = NULL,
      choices = c("All marker peptides"             = "all_markers",
                  "All significant peptides"        = "all_significant",
                  "Top N most significant peptides" = "top_n_adjp",
                  "Top N marker peptides"            = "top_n_markers"),
      selected = label_mode_for_ome
    ),
    conditionalPanel(
      condition = sprintf(
        "input['%s'].indexOf('top_n_adjp') > -1", ns("pelsa_label_mode")),
      numericInput(ns("pelsa_n_top_adjp"),
                   "N (downregulated; upregulated = ceil(N/2)):",
                   value = n_top_adjp_for_ome,
                   min = 1, step = 1, width = "220px")
    ),
    conditionalPanel(
      condition = sprintf(
        "input['%s'].indexOf('top_n_markers') > -1", ns("pelsa_label_mode")),
      numericInput(ns("pelsa_n_top_markers"),
                   "N (downregulated; upregulated = ceil(N/2)):",
                   value = n_top_markers_for_ome,
                   min = 1, step = 1, width = "220px")
    ),
    helpText("Applies to every contrast for this dataset."),
    hr(),
    # 7D best-peptide second panel toggle (lazy: the best-peptide df is built
    # only while this is ON; freed when toggled off).
    checkboxInput(ns("pelsa_show_best_panel"),
                  "Show best peptide per protein", value = show_best_panel_for_ome),
    helpText("Marker-protein peptides are always drawn in magenta on top."),
    hr(),
    actionButton(ns("pelsa_apply_all"),
                 "Apply these settings to all datasets",
                 icon = shiny::icon("copy"), class = "btn-sm"),
    helpText("Copies this dataset's color mode, label modes, N values, and ",
             "best-peptide toggle to every other dataset."),
    hr(),
    fluidRow(
      # LEFT column: the volcano point color key. Narrower than the feature
      # column - the key labels are short, so it cedes width to the right.
      column(5,
        tags$strong("Color key"),
        tags$ul(class = "pelsa-color-key",
          style = "list-style:none; padding-left:0; margin:0;",
          tags$li(tags$span(style = "color:#FF00FF;", "\u25cf"),
                  " marker protein"),
          tags$li(tags$span(style = sprintf("color:%s;", .PELSA_GOLD),
                            "\u25cf"), " highlighted"),
          tags$li(tags$span(style = "color:darkred;", "\u25cf"),
                  " significant up"),
          tags$li(tags$span(style = "color:#1f4e9c;", "\u25cf"),
                  " significant down"),
          tags$li(tags$span(style = "color:gray;", "\u25cf"),
                  " not significant")
        )
      ),
      # RIGHT column: the COMPLETE UniProt feature color reference - every
      # class in the palette, shown even when absent from this protein, so the
      # user has a full key to the Woods feature track. Wider than the color
      # key (7 vs 5) - its labels wrap ("low complexity / disorder"). For a
      # self-curated species there are no UniProt features, so the full color
      # key would map to nothing -- replace it with a short note instead.
      column(7,
        tags$strong("UniProt feature colors"),
        if (isTRUE(is_self_curated)) {
          tags$p(class = "text-muted",
                 "Feature annotations unavailable - self-curated database.")
        } else {
          .pelsa_feature_legend_ui()
        }
      )
    )
  )
}

# @noRd
pelsa_build_pinned_woods <- function(acc, entry, contrast, stat_df_raw,
                                     feat_df, sig_cutoff, sig_stat) {
  matched <- entry$matched %||% data.frame()
  stat_df <- pelsa_volcano_stat_df(stat_df_raw, matched)
  pep <- pelsa_woods_peptide_data(acc, matched, stat_df, contrast,
                                  sig_cutoff = sig_cutoff,
                                  sig_stat = sig_stat)

  # Protein length: prefer the cache coverage frame; fall back to the max
  # mapped residue so the axis still spans the peptides. cov_frac is the
  # validated fractional coverage (NA unless FASTA length resolved) - surfaced
  # for the metadata panel's "Sequence coverage" row.
  cov <- entry$coverage %||% data.frame()
  plen <- NA_integer_
  cov_frac <- NA_real_
  if (is.data.frame(cov) && all(c("accession", "protein_length") %in%
                                 colnames(cov))) {
    idx <- which(as.character(cov$accession) == acc)
    if (length(idx) > 0L) {
      plen <- as.integer(cov$protein_length[idx[1L]])
      if ("coverage" %in% colnames(cov))
        cov_frac <- as.numeric(cov$coverage[idx[1L]])
    }
  }
  if (is.na(plen) || plen < 1L) {
    plen <- if (nrow(pep) > 0L) max(pep$pep_end, na.rm = TRUE) else 1L
  }

  # Per-accession UniProt features (raw rows) -> lane-packed.
  fdf <- feat_df %||% data.frame()
  feats <- if (is.data.frame(fdf) && "accession" %in% colnames(fdf)) {
    fdf[as.character(fdf$accession) == acc, , drop = FALSE]
  } else {
    fdf[0, , drop = FALSE]
  }
  lanes <- pelsa_feature_lanes(feats, prot_len = plen)

  # Woods tooltip: append the overlapping annotation regions per peptide.
  if (nrow(pep) > 0L) {
    ann <- pelsa_woods_overlap_annotations(pep$pep_start, pep$pep_end, feats)
    ann_line <- ifelse(nzchar(ann), paste0("\nAnnotations: ", ann), "")
    pep$.tip <- sprintf(
      "%s\naa %d-%d (len %d)\nlogFC: %.2f\nadj.P: %.2g%s",
      pep$peptide_seq, pep$pep_start, pep$pep_end,
      pep$pep_end - pep$pep_start + 1L, pep$logFC, pep$adj.P.Val, ann_line)
  }

  # Per-feature overlapping peptides (for the feature-lane hover).
  if (is.data.frame(lanes) && nrow(lanes) > 0L && nrow(pep) > 0L) {
    lanes$.overlap_peps <- pelsa_feature_overlap_peptides(
      lanes$start, lanes$end, pep$pep_start, pep$pep_end)
  }

  list(pep = pep, lanes = lanes,
       intervals = pelsa_coverage_intervals(pep$pep_start, pep$pep_end),
       prot_len = plen, coverage_frac = cov_frac)
}

# @noRd
pelsa_render_section3_layout <- function(ns) {
  # L-SHAPED PINNED CARD: the pinned-peptide views form one continuous card -
  # the upper-left arm (metadata + intensity line plot) is visually continuous
  # with the full-width coverage/feature/Woods tracks along the bottom (the
  # `pelsa-pin-card` class draws them as one bordered surface). The volcano +
  # its controls are a SEPARATE card sitting in the top-right notch of the L.
  # The bottom tracks render only once a peptide is pinned.
  tagList(
    fluidRow(
      class = "pelsa-pin-card-top",
      # Upper-LEFT arm of the L: pinned metadata + intensity line plot.
      column(3,
        shinydashboardPlus::box(
          uiOutput(ns("pelsa_add_marker_ui")),
          uiOutput(ns("pelsa_pin_metadata")),
          plotly::plotlyOutput(ns("pelsa_intensity_plot"), height = "440px"),
          helpText("Click a point to pin its peptide profile."),
          width = NULL, title = "Pinned Peptide", headerBorder = TRUE,
          class = "pelsa-pin-arm"
        )
      ),
      # Top-RIGHT notch: the volcano (+ best-peptide panel) - its own card.
      column(6,
        tagList(
          shinydashboardPlus::box(
            plotly::plotlyOutput(ns("pelsa_volcano_plot"), height = "680px"),
            helpText(textOutput(ns("pelsa_marker_count"))),
            status = "primary", width = NULL, title = "PELSA Volcano",
            headerBorder = TRUE, solidHeader = TRUE
          ),
          conditionalPanel(
            condition = sprintf("input['%s']",
                                ns("pelsa_show_best_panel")),
            shinydashboardPlus::box(
              plotly::plotlyOutput(ns("pelsa_volcano_best_plot"),
                                   height = "560px"),
              status = "primary", width = NULL,
              title = "PELSA Volcano (best peptide per protein)",
              headerBorder = TRUE, solidHeader = TRUE
            )
          )
        )
      ),
      column(3,
        shinydashboardPlus::box(
          uiOutput(ns("pelsa_volcano_sidebar")),
          width = NULL, title = "Plot Controls", headerBorder = TRUE
        )
      )
    ),
    # Bottom arm of the L: the full-width per-protein 3-track panel (coverage
    # ruler + UniProt features + Woods), revealed on pin. Same `pelsa-pin-card`
    # styling as the upper-left arm so they read as ONE card.
    fluidRow(
      class = "pelsa-pin-card-bottom",
      column(12,
        shinydashboardPlus::box(
          plotly::plotlyOutput(ns("pelsa_woods_panel"), height = "420px"),
          helpText(paste0("Coverage (gold = residues with peptide evidence); ",
                          "UniProt features (hover for overlapping peptides); ",
                          "Woods plot (y = logFC direction; color = significance ",
                          "magnitude, -log10 adj.P). Click a Woods peptide to ",
                          "select it.")),
          width = NULL, title = "Protein coverage & Woods plot",
          headerBorder = TRUE, class = "pelsa-pin-arm"
        )
      )
    )
  )
}

# @noRd
pelsa_build_volcano_df_cached <- function(contrast, cache, entry, feat_df,
                                          markers, panel, sig_cutoff, sig_stat,
                                          is_self_curated, stat_raw,
                                          fail_label) {
  hit <- cache[[contrast]]
  if (!is.null(hit)) return(list(df = hit, cache = cache))

  matched <- entry$matched %||% data.frame()
  fdf <- feat_df %||% data.frame(accession = character(0),
                                 start = integer(0), end = integer(0),
                                 feature_class = character(0))
  stat_df <- pelsa_volcano_stat_df(stat_raw, matched)
  df <- tryCatch(
    pelsa_build_volcano_df(
      stat_df       = stat_df,
      matched_cache = if (nrow(matched) > 0L) matched else
        pelsa_volcano_empty_matched(),
      feat_df       = fdf,
      markers       = markers,
      contrast      = contrast,
      opts          = list(panel = panel, sig_cutoff = sig_cutoff,
                           sig_stat = sig_stat),
      is_self_curated = is_self_curated
    ),
    error = function(e) {
      showNotification(
        paste0("Could not build ", fail_label, ": ", conditionMessage(e)),
        type = "error", duration = 8
      )
      NULL
    }
  )
  validate(need(!is.null(df), paste0(
    toupper(substring(fail_label, 1, 1)), substring(fail_label, 2),
    " could not be built.")))

  # FREE the prior contrast: replace the cache with a SINGLE-entry list.
  list(df = df, cache = stats::setNames(list(df), contrast))
}
