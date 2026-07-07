################################################################################
# Module: PELSA - Section 2 (Summary)
#
# A DASHBOARD that READS the per-dataset analysis cache built by Setup's
# Start-Analysis (5D, the `pelsa_analysis` reactiveVal) and renders metrics +
# plots for the ACTIVE dataset. It NEVER recomputes the heavy objects in render -
# every panel reads from the (already-small) cache tables. The pure plot-data /
# shaping logic lives in tab_pelsa_section2_helpers.R; the ggplot builders and
# per-ome export bundle (6E) live in tab_pelsa_section2_plots.R.
#
# Sections (from the planning doc):
#   6A Experiment-wide : total peptide IDs (value box), per-protein coverage
#                        distribution, peptide-length DENSITY (mean+median dodged
#                        lines), missed-cleavage bar.
#   6B Per-condition CV KDE : one density curve per condition (ok rows), median
#                        lines, 99th-pctile xlim, conditions with <20 finite CVs
#                        skipped (noted).
#   6C Per-sample depth : bar per sample ordered by sample_order + companion
#                        summary table.
#   6D Mapping / annotation QC : inline failed-match / failed-annotation counts +
#                        COLLAPSIBLE bottom tables (FASTA-unmatched, unannotated).
#   6E Exports : per-ome export list re-derived from the cache for ALL analyzed
#                datasets (cv / coverage / depth / unmatched / unannotated /
#                peptide_metrics CSVs).
#
# CACHE CONTRACT: see tab_pelsa_analysis_helpers.R (@section Cache contract).
# A FAILED dataset entry is detected with pelsa_analysis_failed(entry).
################################################################################

# Exact caption required for the per-condition CV KDE (planning Decision).
.PELSA_CV_CAPTION <- paste0(
  "CV of (un-logged), non-normalized intensities -- replicate ",
  "reproducibility."
)

################################################################################
# Tab-level UI and Server
################################################################################

PELSASection2_Tab_UI <- function(id = "PELSASection2Tab") {
  ns <- NS(id)
  tagList(
    fluidRow(uiOutput(ns("summary_box")))
  )
}

PELSASection2_Tab_Server <- function(id = "PELSASection2Tab",
                                     GCTs_and_params,
                                     globals,
                                     GCTs_original,
                                     active_dataset,
                                     pelsa_analysis = NULL,
                                     pelsa_setup_state = NULL) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## GATHERING INPUTS ##

    # The cache + setup_state seams (5D). Tolerate being called without them
    # (legacy/test wiring) by coercing to empty reactives.
    analysis_cache <- if (is.function(pelsa_analysis)) {
      pelsa_analysis
    } else {
      reactive(NULL)
    }
    setup_state_r <- if (is.function(pelsa_setup_state)) {
      pelsa_setup_state
    } else {
      reactive(NULL)
    }

    # The active dataset's cache entry (or NULL). Defense-in-depth: a FAILED
    # entry is treated as NULL here too (mirrors Volcano's cache_entry() gate),
    # so the dashboard's per-output reactives never read a failed entry's missing
    # fields even if the top-level gate were bypassed.
    active_entry <- reactive({
      cache <- analysis_cache()
      ome <- active_dataset()
      if (is.null(cache) || is.null(ome)) return(NULL)
      entry <- cache[[ome]]
      if (pelsa_analysis_failed(entry)) return(NULL)
      entry
    })

    # The active dataset's canonical sample order (or NULL).
    active_sample_order <- reactive({
      ss <- setup_state_r()
      ome <- active_dataset()
      if (is.null(ss) || is.null(ome)) return(NULL)
      so <- ss$sample_order
      if (is.null(so)) NULL else so[[ome]]
    })

    # The active dataset's confirmed condition order (or NULL) for the CV KDE.
    active_condition_order <- reactive({
      ss <- setup_state_r()
      ome <- active_dataset()
      if (is.null(ss) || is.null(ome)) return(NULL)
      co <- ss$condition_order
      if (is.null(co)) NULL else co[[ome]]
    })

    ## TOP-LEVEL GATE ##
    # NULL cache  -> "Run Start Analysis in Setup first."
    # failed entry -> show its error/stage.
    # good entry  -> render the dashboard.
    output$summary_box <- renderUI({
      ome <- active_dataset()
      cache <- analysis_cache()

      if (is.null(cache)) {
        return(pelsa_section2_message_box(
          "PELSA - Summary",
          "Run Start Analysis in Setup first to populate the summary dashboard."
        ))
      }
      if (is.null(ome) || !(ome %in% names(cache))) {
        return(pelsa_section2_message_box(
          "PELSA - Summary",
          "This dataset has not been analyzed. Re-run Start Analysis in Setup."
        ))
      }

      entry <- cache[[ome]]
      if (pelsa_analysis_failed(entry)) {
        stage <- entry$stage
        # Harden against a length-0 stage (is.na() would error on length 0):
        # require a length-1 non-NA value before formatting the stage hint.
        stage_txt <- if (length(stage) == 1L && !is.na(stage))
          sprintf(" (stage: %s)", stage) else ""
        return(pelsa_section2_message_box(
          sprintf("PELSA - Summary: %s failed", ome),
          sprintf("Analysis failed for this dataset%s: %s",
                  stage_txt, entry$error),
          status = "danger"
        ))
      }

      # The bottom QC tables auto-expand when they carry rows (only empty tables
      # stay collapsed) - decide each from the cache entry at render time.
      has_unmatched <-
        (entry$qc$n_unmatched_rows %||% nrow(entry$unmatched %||% data.frame())) > 0L
      has_unannotated <-
        (entry$qc$n_unannotated_accessions %||% length(entry$unannotated)) > 0L
      pelsa_section2_dashboard_ui(ns, ome,
                                  has_unmatched = has_unmatched,
                                  has_unannotated = has_unannotated,
                                  splot_choices = splot_choices_for_ui())
    })

    ## 6A - EXPERIMENT-WIDE ##

    output$total_peptide_ids <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else entry$qc$n_peptides
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Total peptides identified",
        icon     = icon("dna"),
        color    = "aqua"
      )
    })

    output$fully_quantified_count <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else entry$qc$n_fully_quantified
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Fully-quantified peptides",
        icon     = icon("check-double"),
        color    = "green"
      )
    })

    output$failed_match_count <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else
        (entry$qc$n_unmatched_rows %||% nrow(entry$unmatched %||% data.frame()))
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Peptides failed FASTA match",
        icon     = icon("triangle-exclamation"),
        color    = "yellow"
      )
    })

    output$annotated_with_features_count <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else
        (entry$qc$n_annotated_with_features %||% NA_integer_)
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Proteins with >=1 annotation",
        icon     = icon("circle-check"),
        color    = "teal"
      )
    })

    output$annotated_zero_feature_count <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else
        (entry$qc$n_annotated_zero_feature %||% NA_integer_)
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Proteins with 0 annotation",
        icon     = icon("circle-minus"),
        color    = "yellow"
      )
    })

    output$failed_annotation_count <- shinydashboard::renderValueBox({
      ome <- active_dataset()
      ss  <- setup_state_r()
      is_self_curated <- !is.null(ss) && !is.null(ome) &&
        isTRUE(ss$self_curated[[ome]])
      if (is_self_curated) {
        return(shinydashboard::valueBox(
          value    = "-",
          subtitle = "Self-curated database (no UniProt feature annotation)",
          icon     = icon("circle-info"),
          color    = "blue"
        ))
      }
      entry <- active_entry()
      # TRUE failure residual = accessions absent from the annotation for NO known
      # reason. A self-describing annotation (with a `disposition` column) buckets
      # merged/demerged/deleted accessions as "excluded for a reason", so those
      # DROP OUT of this count. Fall back to the legacy unannotated count when the
      # annotation carries no disposition info (n_annotation_failed absent -> the
      # two are equal). The merged/deleted breakdown is shown as a subtitle hint.
      qc <- if (is.null(entry)) list() else (entry$qc %||% list())
      n <- if (is.null(entry)) NA_integer_ else
        (qc$n_annotation_failed %||% qc$n_unannotated_accessions %||%
           length(entry$unannotated))
      excluded <- (qc$n_annotated_merged %||% 0L) +
        (qc$n_annotated_demerged %||% 0L) + (qc$n_annotated_deleted %||% 0L)
      subtitle <- if (excluded > 0L) {
        sprintf("Proteins failed annotation (+%s excluded: merged/deleted)",
                format(excluded, big.mark = ","))
      } else {
        "Proteins failed annotation"
      }
      # `n` is NA before analysis (entry NULL). Guard the color test against NA
      # (`if (NA > 0L)` errors) and treat NA as "no failures yet" -> neutral.
      has_failures <- isTRUE(n > 0L)
      shinydashboard::valueBox(
        value    = format(n, big.mark = ","),
        subtitle = subtitle,
        icon     = icon("circle-question"),
        color    = if (has_failures) "red" else "black"
      )
    })

    # Per-protein sequence coverage - experiment-wide (default) OR per-condition.
    coverage_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      if ((entry$qc$n_matched_rows %||% 0L) == 0L) {
        return(pelsa_blank_plot(
          "No peptides mapped to FASTA - check species / FASTA."))
      }
      mode <- if (identical(input$coverage_mode %||% "overall", "per_condition"))
        "per_condition" else "overall"
      pelsa_coverage_plot(entry$coverage_by_sample, entry$condition_map,
                         active_condition_order(), mode = mode)
    })
    output$coverage_plot <- renderPlotly({
      ggplotly(coverage_plot_reactive())
    })
    output$coverage_skipped_note <- renderUI({
      entry <- active_entry()
      req(entry)
      if (identical(input$coverage_mode %||% "overall", "overall")) return(NULL)
      agg <- pelsa_bar_error_data(entry$coverage_by_sample, "coverage",
                                  entry$condition_map, active_condition_order())
      if (nrow(agg$skipped) == 0L) return(NULL)
      msg <- paste(sprintf("%s (n=%d)", agg$skipped$condition, agg$skipped$n),
                  collapse = ", ")
      tags$p(sprintf("Skipped (<2 replicate samples): %s", msg),
            style = "color:#6c757d; font-size:0.9em;")
    })

    # Peptide-length - experiment-wide (default) OR per-condition.
    length_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      mode <- if (identical(input$length_mode %||% "overall", "per_condition"))
        "per_condition" else "overall"
      pelsa_length_plot(entry$length_by_sample, entry$condition_map,
                       active_condition_order(), mode = mode)
    })
    output$length_plot <- renderPlotly({
      ggplotly(length_plot_reactive())
    })
    output$length_skipped_note <- renderUI({
      entry <- active_entry()
      req(entry)
      if (identical(input$length_mode %||% "overall", "overall")) return(NULL)
      agg <- pelsa_bar_error_data(entry$length_by_sample, "mean_length",
                                  entry$condition_map, active_condition_order())
      if (nrow(agg$skipped) == 0L) return(NULL)
      msg <- paste(sprintf("%s (n=%d)", agg$skipped$condition, agg$skipped$n),
                  collapse = ", ")
      tags$p(sprintf("Skipped (<2 replicate samples): %s", msg),
            style = "color:#6c757d; font-size:0.9em;")
    })

    # Missed-cleavage rate - experiment-wide (default) OR per-condition.
    missed_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      mode <- if (identical(input$missed_mode %||% "overall", "per_condition"))
        "per_condition" else "overall"
      pelsa_missed_cleavage_plot(entry$missed_cleavage_rate_by_sample,
                                entry$condition_map, active_condition_order(),
                                mode = mode)
    })
    output$missed_plot <- renderPlotly({
      ggplotly(missed_plot_reactive())
    })
    output$missed_skipped_note <- renderUI({
      entry <- active_entry()
      req(entry)
      if (identical(input$missed_mode %||% "overall", "overall")) return(NULL)
      agg <- pelsa_bar_error_data(entry$missed_cleavage_rate_by_sample, "rate",
                                  entry$condition_map, active_condition_order())
      if (nrow(agg$skipped) == 0L) return(NULL)
      msg <- paste(sprintf("%s (n=%d)", agg$skipped$condition, agg$skipped$n),
                  collapse = ", ")
      tags$p(sprintf("Skipped (<2 replicate samples): %s", msg),
            style = "color:#6c757d; font-size:0.9em;")
    })

    ## 6B - PER-CONDITION CV KDE ##

    # CV - per-condition KDE (default) OR pooled experiment-wide density (toggle).
    cv_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      if (identical(input$cv_mode %||% "per_condition", "overall")) {
        pelsa_cv_overall_plot(entry$cv)
      } else {
        pelsa_cv_kde_plot(entry$cv, active_condition_order())
      }
    })
    output$cv_plot <- renderPlotly({
      ggplotly(cv_plot_reactive())
    })
    output$cv_caption <- renderUI({
      tags$p(.PELSA_CV_CAPTION, style = "color:#6c757d; font-style:italic;")
    })
    # The skipped-condition note belongs to the per-condition KDE only.
    output$cv_skipped_note <- renderUI({
      entry <- active_entry()
      req(entry)
      if (identical(input$cv_mode %||% "per_condition", "overall")) return(NULL)
      elig <- pelsa_cv_kde_eligibility(entry$cv, active_condition_order())
      if (nrow(elig$skipped) == 0L) return(NULL)
      msg <- paste(
        sprintf("%s (n=%d)", elig$skipped$condition, elig$skipped$n),
        collapse = ", ")
      tags$p(
        sprintf("Skipped (<20 finite CVs): %s", msg),
        style = "color:#6c757d; font-size:0.9em;")
    })

    ## 6C - PER-SAMPLE DEPTH ##

    depth_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      pelsa_depth_bar_plot(entry$n_quantified, active_sample_order())
    })
    output$depth_plot <- renderPlotly({
      ggplotly(depth_plot_reactive())
    })

    ## 6F - INTENSITY RANK (S-PLOT) ##

    # Client WebGL capability (default TRUE for the pre-probe NULL).
    use_webgl <- reactive(webgl_capability(globals$webgl_supported))

    # Per-ome STICKY customization store: [[ome]] -> list(selected_markers,
    # label_trypsin, sample). Seeded on first visit; read by render + export.
    splot_state <- reactiveValues()

    # The active dataset's processed GCT + params + marker rows.
    splot_gct <- reactive({
      gp <- GCTs_and_params(); ome <- active_dataset()
      if (is.null(gp) || is.null(ome)) NULL else gp$GCTs[[ome]]
    })
    splot_params <- reactive({
      gp <- GCTs_and_params(); ome <- active_dataset()
      if (is.null(gp) || is.null(ome)) list() else (gp$parameters[[ome]] %||% list())
    })
    splot_marker_rows <- reactive({
      ss <- setup_state_r(); ome <- active_dataset()
      mr <- if (is.null(ss) || is.null(ome)) NULL else ss$marker_rows[[ome]]
      if (is.null(mr) || !is.data.frame(mr) || !"accession" %in% names(mr))
        pelsa_empty_marker_rows() else mr
    })

    # Marker choices: ALL markers, but DISABLE those with no matched peptide
    # anywhere in the dataset (dataset-wide, isoform-matched).
    splot_marker_choices <- reactive({
      rows <- splot_marker_rows(); entry <- active_entry()
      accs  <- as.character(rows$accession)
      genes <- as.character(rows$gene %||% rep("", length(accs)))
      labels <- ifelse(nzchar(genes), paste0(accs, " (", genes, ")"), accs)
      matched_keys <- if (!is.null(entry) && is.data.frame(entry$matched))
        unique(tolower(pelsa_isoform_base(trimws(
          as.character(entry$matched$accession))))) else character(0)
      disabled <- !(tolower(pelsa_isoform_base(trimws(accs))) %in% matched_keys)
      list(accs = accs, labels = labels, disabled = disabled)
    })

    # Sample list ordered by the dataset's confirmed sample_order (alpha fallback).
    splot_samples <- reactive({
      gct <- splot_gct(); if (is.null(gct)) return(character(0))
      cols <- colnames(pelsa_dataset_matrix(gct, character(0)))
      so <- active_sample_order()
      c(intersect(so, cols), setdiff(cols, so))
    })

    # Seed the sticky store on first visit and expose the S-plot input choices
    # (samples + marker choices + the sticky selection) for the dashboard UI to
    # BAKE IN at build time. Building the inputs with their choices avoids the
    # update*Input-vs-renderUI race: summary_box re-renders on cache updates and
    # recreates these inputs, so a one-shot post-render update (keyed only on
    # active_dataset()) would land on a replaced DOM node and leave them blank.
    splot_choices_for_ui <- reactive({
      ome <- active_dataset(); req(ome)
      ch <- splot_marker_choices(); samples <- splot_samples()
      # Read/seed the sticky store under isolate(): the UI needs only the CURRENT
      # sticky value at render time, and depending reactively on splot_state
      # would re-render the whole summary_box on every S-plot input edit (the
      # write-back observers mutate splot_state) - flickering the dashboard.
      st <- isolate({
        if (is.null(splot_state[[ome]])) {
          splot_state[[ome]] <- list(
            selected_markers = ch$accs[!ch$disabled],
            label_trypsin = FALSE,
            sample = if (length(samples) > 0L) samples[[1]] else NULL)
        }
        splot_state[[ome]]
      })
      # Keep the sticky sample valid against the current sample set.
      sample_sel <- st$sample
      if (is.null(sample_sel) || !(sample_sel %in% samples))
        sample_sel <- if (length(samples) > 0L) samples[[1]] else NULL
      list(
        samples          = samples,
        sample_selected  = sample_sel,
        marker_accs      = ch$accs,
        marker_labels    = ch$labels,
        marker_disabled  = ch$disabled,
        markers_selected = st$selected_markers %||% character(0)
      )
    })

    # Keep the trypsin checkbox in sync with the sticky store on dataset switch
    # (the checkbox default in the UI is FALSE; re-apply the remembered value).
    observeEvent(active_dataset(), {
      ome <- active_dataset(); req(ome)
      st <- splot_state[[ome]]
      if (!is.null(st))
        updateCheckboxInput(session, "splot_trypsin",
                            value = isTRUE(st$label_trypsin))
    }, ignoreNULL = TRUE)

    # Write input edits back into the active ome's sticky store.
    # ignoreNULL = TRUE (default): skips the initial NULL before picker is
    # populated; the picker returns a non-NULL empty value on deselect-all, so
    # ignoreNULL = TRUE still fires the write-back.
    observeEvent(input$splot_markers, {
      ome <- active_dataset(); req(ome)
      st <- splot_state[[ome]] %||% list()
      st$selected_markers <- input$splot_markers
      splot_state[[ome]] <- st
    })
    observeEvent(input$splot_trypsin, {
      ome <- active_dataset(); req(ome)
      st <- splot_state[[ome]] %||% list()
      st$label_trypsin <- isTRUE(input$splot_trypsin)
      splot_state[[ome]] <- st
    })
    observeEvent(input$splot_sample, {
      ome <- active_dataset(); req(ome)
      st <- splot_state[[ome]] %||% list()
      st$sample <- input$splot_sample
      splot_state[[ome]] <- st
    })

    splot_prep <- reactive({
      ome <- active_dataset(); entry <- active_entry(); gct <- splot_gct()
      req(ome, entry, gct)
      st <- splot_state[[ome]]; req(st)
      peptides <- pelsa_dataset_peptide_frame(gct)
      mat <- pelsa_dataset_matrix(gct, colnames(peptides))
      sample <- st$sample
      if (is.null(sample) || !(sample %in% colnames(mat))) sample <- colnames(mat)[[1]]
      pelsa_splot_prepare(mat, sample, peptides, entry$matched,
                          st$selected_markers %||% character(0),
                          .PELSA_TRYPSIN_ACCESSIONS, isTRUE(st$label_trypsin),
                          splot_params())
    })
    output$splot_plot <- renderPlotly({
      prep <- splot_prep()
      validate(need(nrow(prep$background) > 0L,
                    "No finite intensities in this sample."))
      pelsa_splot_build_plotly(prep, use_webgl = use_webgl())
    })

    ## 6D - MAPPING / ANNOTATION QC (collapsible, bottom) ##

    output$unmatched_table <- DT::renderDataTable({
      entry <- active_entry()
      req(entry)
      um <- entry$unmatched
      display <- data.frame(
        `Peptide`       = um$peptide_sequence,
        `Accession`     = um$accession,
        `Gene`          = um$gene,
        `Peptide position` = um$pep_position,
        `Reason`        = um$reason,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        display, rownames = FALSE, selection = "none",
        extensions = "Buttons",
        options = list(pageLength = 100, scrollY = "300px", scrollCollapse = TRUE,
                       dom = "Bfrtip", buttons = c("csv"))
      )
    })

    output$unannotated_table <- DT::renderDataTable({
      entry <- active_entry()
      req(entry)
      ua <- as.character(entry$unannotated %||% character(0))
      display <- data.frame(Accession = ua, check.names = FALSE,
                            stringsAsFactors = FALSE)
      DT::datatable(
        display, rownames = FALSE, selection = "none",
        extensions = "Buttons",
        options = list(pageLength = 100, scrollY = "300px", scrollCollapse = TRUE,
                       dom = "Bfrtip", buttons = c("csv"))
      )
    })

    ## 6E - EXPORTS ##
    # Per-ome export list re-derived from the cache for ALL analyzed datasets
    # (NOT just the active one). The cache is the source - each export function
    # reads its dataset's cache entry and writes one CSV into dir_name.
    all_exports <- reactive({
      cache <- analysis_cache()
      if (is.null(cache)) return(list())
      datasets <- names(cache)
      datasets <- datasets[!vapply(cache, pelsa_analysis_failed, logical(1))]
      ss <- setup_state_r()
      gp <- GCTs_and_params()
      stats::setNames(lapply(datasets, function(ome) {
        co <- if (is.null(ss)) NULL else ss$condition_order[[ome]]
        so <- if (is.null(ss)) NULL else ss$sample_order[[ome]]
        gct <- if (is.null(gp)) NULL else gp$GCTs[[ome]]
        params <- if (is.null(gp)) list() else (gp$parameters[[ome]] %||% list())
        mr <- if (is.null(ss)) NULL else ss$marker_rows[[ome]]
        marker_accs <- if (is.data.frame(mr) && "accession" %in% names(mr))
          unique(as.character(mr$accession)) else character(0)
        custom <- isolate(splot_state[[ome]])
        pelsa_section2_exports_for(cache[[ome]], ome,
                                   condition_order = co, sample_order = so,
                                   gct = gct, marker_accs = marker_accs,
                                   params = params, custom = custom)
      }), datasets)
    })

    return(all_exports)
  })
}

################################################################################
# UI builders (pure tag constructors)
################################################################################

# A simple message box (used for the NULL-cache / failed-entry gates).
# @noRd
pelsa_section2_message_box <- function(title, message, status = "primary") {
  fluidRow(
    shinydashboardPlus::box(
      div(
        style = paste(
          "background-color: #f8f9fa; border-left: 4px solid #007bff;",
          "padding: 12px; border-radius: 0 4px 4px 0;"
        ),
        icon("info-circle", style = "color: #007bff; margin-right: 8px;"),
        span(message, style = "color: #495057;")
      ),
      status = status, width = 12, title = title,
      headerBorder = TRUE, solidHeader = TRUE
    )
  )
}

# A compact inline experiment-wide / per-condition radio toggle for a plot panel.
# `default` is the selected VALUE ("overall" or "per_condition"). @noRd
pelsa_mode_toggle <- function(ns, id, default = "overall") {
  radioButtons(
    ns(id), label = NULL, inline = TRUE,
    choices  = c("Experiment-wide" = "overall",
                 "Per-condition"   = "per_condition"),
    selected = default
  )
}

# The full dashboard layout for one analyzed dataset (6A-6D). Pure markup; all
# inputIds/outputIds wrapped in ns().
# @noRd
pelsa_section2_dashboard_ui <- function(ns, ome,
                                        has_unmatched = FALSE,
                                        has_unannotated = FALSE,
                                        splot_choices = NULL) {
  # S-plot input choices are baked in at BUILD time (not applied via a post-
  # render update*Input, which races the renderUI that recreates this box on
  # cache updates). splot_choices is a list(samples, sample_selected,
  # marker_accs, marker_labels, marker_disabled, markers_selected); NULL yields
  # empty controls (the pre-analysis / no-data case). See the caller in
  # PELSASection2_Tab_Server's summary_box renderUI.
  sc <- splot_choices %||% list()
  splot_samples_choices  <- sc$samples %||% character(0)
  splot_sample_selected  <- sc$sample_selected
  splot_marker_values    <- sc$marker_accs %||% character(0)
  splot_marker_labels    <- sc$marker_labels %||% splot_marker_values
  splot_marker_choices_v <- stats::setNames(splot_marker_values, splot_marker_labels)
  splot_marker_disabled  <- sc$marker_disabled %||% logical(0)
  splot_markers_selected <- sc$markers_selected %||% character(0)
  tagList(
    # 6A value boxes (inline counts incl. the 6D mapping/annotation QC totals).
    # Row 1: peptide identification + FASTA match. Row 2: the three-way
    # annotation breakdown (>=1 annotation / 0 annotation / failed annotation).
    fluidRow(
      shinydashboard::valueBoxOutput(ns("total_peptide_ids"), width = 4),
      shinydashboard::valueBoxOutput(ns("fully_quantified_count"), width = 4),
      shinydashboard::valueBoxOutput(ns("failed_match_count"), width = 4)
    ),
    fluidRow(
      shinydashboard::valueBoxOutput(ns("annotated_with_features_count"),
                                     width = 4),
      shinydashboard::valueBoxOutput(ns("annotated_zero_feature_count"),
                                     width = 4),
      shinydashboard::valueBoxOutput(ns("failed_annotation_count"), width = 4)
    ),

    # 6C depth bar + 6A missed cleavage.
    fluidRow(
      shinydashboardPlus::box(
        plotlyOutput(ns("depth_plot")),
        title = "Number of quantified peptides", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      ),
      shinydashboardPlus::box(
        pelsa_mode_toggle(ns, "missed_mode", "overall"),
        plotlyOutput(ns("missed_plot")),
        uiOutput(ns("missed_skipped_note")),
        title = "Missed-cleavage rate", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      )
    ),

    # 6A coverage + peptide length (each with an experiment-wide / per-condition
    # toggle; default experiment-wide).
    fluidRow(
      shinydashboardPlus::box(
        pelsa_mode_toggle(ns, "coverage_mode", "overall"),
        plotlyOutput(ns("coverage_plot")),
        uiOutput(ns("coverage_skipped_note")),
        title = "Protein sequence coverage", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      ),
      shinydashboardPlus::box(
        pelsa_mode_toggle(ns, "length_mode", "overall"),
        plotlyOutput(ns("length_plot")),
        uiOutput(ns("length_skipped_note")),
        title = "Average peptide length", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      )
    ),

    # 6B CV KDE (toggle; default per-condition).
    fluidRow(
      shinydashboardPlus::box(
        pelsa_mode_toggle(ns, "cv_mode", "per_condition"),
        plotlyOutput(ns("cv_plot")),
        uiOutput(ns("cv_skipped_note")),
        uiOutput(ns("cv_caption")),
        title = "Coefficient of variation (CV)",
        status = "primary", width = 12, headerBorder = TRUE, solidHeader = TRUE
      )
    ),

    # 6F Intensity rank (S-plot): full-width; dedicated right-hand control column.
    fluidRow(
      shinydashboardPlus::box(
        title = "Intensity rank (S-plot)", status = "primary", width = 12,
        headerBorder = TRUE, solidHeader = TRUE,
        fluidRow(
          column(9, plotlyOutput(ns("splot_plot"), height = "520px")),
          column(
            3,
            selectInput(ns("splot_sample"), "Sample",
                        choices = splot_samples_choices,
                        selected = splot_sample_selected),
            tags$div(
              title = paste0("Peptides from the following trypsin proteins are ",
                             "to be labeled: \"Q29463\", \"P00760\", \"P00761\""),
              checkboxInput(ns("splot_trypsin"),
                            "Label trypsin peptides on the plot", value = FALSE)
            ),
            shinyWidgets::pickerInput(
              ns("splot_markers"), "Label markers",
              choices = splot_marker_choices_v, multiple = TRUE,
              selected = splot_markers_selected,
              choicesOpt = if (length(splot_marker_disabled))
                list(disabled = splot_marker_disabled) else NULL,
              options = list(actionsBox = TRUE)
            )
          )
        )
      )
    ),

    # 6D collapsible QC tables PINNED AT THE BOTTOM. Auto-expanded when they have
    # rows; only an empty table stays collapsed.
    fluidRow(
      shinydashboardPlus::box(
        DT::dataTableOutput(ns("unmatched_table")),
        title = "QC: peptides that failed FASTA match",
        status = "warning", width = 6, headerBorder = TRUE, solidHeader = TRUE,
        collapsible = TRUE, collapsed = !has_unmatched
      ),
      shinydashboardPlus::box(
        DT::dataTableOutput(ns("unannotated_table")),
        title = "QC: proteins that failed annotation",
        status = "warning", width = 6, headerBorder = TRUE, solidHeader = TRUE,
        collapsible = TRUE, collapsed = !has_unannotated
      )
    )
  )
}

