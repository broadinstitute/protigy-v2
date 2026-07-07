################################################################################
# Module: PELSA - Section 2 (Summary)
#
# A DASHBOARD that READS the per-dataset analysis cache built by Setup's
# Start-Analysis (5D, the `pelsa_analysis` reactiveVal) and renders metrics +
# plots for the ACTIVE dataset. It NEVER recomputes the heavy objects in render -
# every panel reads from the (already-small) cache tables. The pure plot-data /
# shaping logic lives in tab_pelsa_section2_helpers.R.
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
        title = "Peptides quantified per sample", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      ),
      shinydashboardPlus::box(
        pelsa_mode_toggle(ns, "missed_mode", "overall"),
        plotlyOutput(ns("missed_plot")),
        uiOutput(ns("missed_skipped_note")),
        title = "Missed-cleavage distribution", status = "primary",
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
        title = "Per-protein sequence coverage", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      ),
      shinydashboardPlus::box(
        pelsa_mode_toggle(ns, "length_mode", "overall"),
        plotlyOutput(ns("length_plot")),
        uiOutput(ns("length_skipped_note")),
        title = "Peptide-length distribution", status = "primary",
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
        title = "Per-condition CV",
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

################################################################################
# Plot builders (pure; read the small cache tables, build a ggplot)
################################################################################

# A blank placeholder plot carrying a centered message (used when a panel cannot
# be drawn, e.g. zero FASTA matches). @noRd
pelsa_blank_plot <- function(message) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 4) +
    theme_void()
}

# Condition (or pooled "Experiment-wide") bar+error-bar plot: one bar per row
# of `bar_df` (mean), an error bar at mean +/- sd (omitted when sd is NA,
# e.g. a defensive n=1 row that slipped past the caller's min_replicates
# gate), and a value+n label above each bar/whisker. No x-axis title -- the
# bar's own tick label (condition name, or "Experiment-wide") is
# self-explanatory, mirroring pelsa_depth_bar_plot's bar-label layout but
# WITHOUT that plot's "Sample" x title (this builder's x is a condition, not
# a sample). export=TRUE applies the same static-figure styling used by the
# other Section-2 QC plots (title size 12 centered, black size-8 axis text).
# @noRd
pelsa_condition_bar_plot <- function(bar_df, y_label, title, fill,
                                     y_fmt = function(v) sprintf("%.1f", v),
                                     blank_msg = "Not enough replicate samples to plot.",
                                     export = FALSE) {
  if (is.null(bar_df) || !is.data.frame(bar_df) || nrow(bar_df) == 0L) {
    return(pelsa_blank_plot(blank_msg))
  }
  df <- bar_df
  df$condition <- factor(df$condition, levels = df$condition)
  df$ymin <- ifelse(is.na(df$sd), df$mean, df$mean - df$sd)
  df$ymax <- ifelse(is.na(df$sd), df$mean, df$mean + df$sd)
  head_room <- 0.06 * max(df$ymax, na.rm = TRUE)
  df$label_y <- df$ymax + head_room
  df$bar_label <- sprintf("%s\n(n=%d)", y_fmt(df$mean), df$n)

  label_size <- if (export) 4 else 3
  x_text_size <- if (export) 9 else 11
  p <- ggplot(df, aes(x = .data$condition, y = .data$mean)) +
    geom_col(fill = fill) +
    geom_errorbar(aes(ymin = .data$ymin, ymax = .data$ymax),
                  data = df[!is.na(df$sd), , drop = FALSE],
                  width = 0.2) +
    geom_text(aes(y = .data$label_y, label = .data$bar_label),
              vjust = 0, size = label_size, fontface = "bold") +
    scale_y_continuous(labels = scales::label_comma(),
                       expand = expansion(mult = c(0, 0.18))) +
    labs(x = NULL, y = y_label, title = title) +
    protigy_plot_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = x_text_size,
                                     colour = if (export) "black" else NULL))
  if (export) {
    p$theme$plot.title.position <- NULL
    p <- p + ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text  = ggplot2::element_text(size = 8, colour = "black"))
  }
  p
}

# Experiment-wide DENSITY with BOTH a dashed mean and a dashed median line, the
# text annotations vertically dodged so they don't overlap. The shared builder
# behind the coverage + length panels' "Experiment-wide" toggle mode.
#
# @param vals      numeric values (NA / non-finite dropped).
# @param value_fmt function(value) -> string used in the "mean = .." / "median =
#                  .." labels (e.g. coverage formats as a percentage).
# @noRd
#
# @param x_hi      optional numeric upper x-limit. The left edge is always
#                  clamped to 0 via coord_cartesian. When non-NULL and finite,
#                  the right edge is also clamped to x_hi so a long right tail
#                  of outliers doesn't blow out the scale (and keeps this
#                  experiment-wide mode aligned with the per-condition KDE,
#                  which clamps at the 99th percentile). NULL (the default)
#                  leaves the right edge un-clamped (auto), which the coverage
#                  and peptide-length callers rely on.
pelsa_overall_density_plot <- function(vals, x_label, title,
                                       value_fmt = function(v) sprintf("%.1f", v),
                                       fill = "#59a14f", subtitle = NULL,
                                       blank_msg = "Not enough values for a density.",
                                       x_hi = NULL, x_scale = NULL, export = FALSE) {
  vals <- vals[is.finite(vals)]
  if (length(vals) < 2L) return(pelsa_blank_plot(blank_msg))
  ann_size <- if (export) 4.2 else 3.2
  m  <- mean(vals)
  md <- stats::median(vals)
  y_top <- tryCatch(max(stats::density(vals)$y, na.rm = TRUE),
                    error = function(e) 1)
  if (!is.finite(y_top) || y_top <= 0) y_top <- 1
  ys <- pelsa_dodge_offsets(2L, y_top = y_top * 0.95, y_range = y_top)
  df <- data.frame(x = vals)

  # White halo behind the mean/median labels so they stay readable over the
  # density fill (matches the per-condition + CV panels). Build a 2-row frame in
  # the (x, y, label) shape pelsa_halo_text_layers expects; y_top drives its
  # offset scale. hjust = -0.05 in the halo mirrors the colored labels below so
  # the ring sits centered on the same glyphs.
  halo_df <- data.frame(
    x = c(m, md),
    y = c(ys[1], ys[2]),
    label = c(paste0("mean = ", value_fmt(m)),
              paste0("median = ", value_fmt(md))),
    stringsAsFactors = FALSE
  )
  p <- ggplot(df, aes(x = .data$x)) +
    geom_density(fill = fill, alpha = 0.4, color = fill) +
    geom_vline(xintercept = m,  linetype = "dashed", color = "#e15759") +
    geom_vline(xintercept = md, linetype = "dashed", color = "#4e79a7") +
    pelsa_halo_text_layers(halo_df, x_hi = max(vals), peak = y_top, size = ann_size) +
    annotate("text", x = m,  y = ys[1], label = paste0("mean = ", value_fmt(m)),
             color = "#e15759", hjust = -0.05, size = ann_size, fontface = "bold") +
    annotate("text", x = md, y = ys[2],
             label = paste0("median = ", value_fmt(md)),
             color = "#4e79a7", hjust = -0.05, size = ann_size, fontface = "bold")
  # Always clamp the left edge to 0 (vals here are always non-negative counts,
  # lengths, or fractions), mirroring pelsa_per_condition_density_plot's x_lo.
  # Without this, a floating density curve whose mass sits away from 0 can
  # silently drop 0 off the rendered x-axis. NA for the upper bound means
  # "use the data's natural extent," preserving the unclamped-right-edge
  # behavior for callers (length/coverage) that don't pass x_hi.
  right_bound <- if (!is.null(x_hi) && is.finite(x_hi) && x_hi > 0) x_hi else NA
  base_theme <- protigy_plot_theme()
  if (export) {
    base_theme$plot.title.position <- NULL
  }
  p <- p + labs(x = x_label, y = "Density", title = title, subtitle = subtitle) +
    base_theme
  if (export) {
    p <- p + ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.text     = ggplot2::element_text(size = 8, colour = "black"))
  }
  p <- p + coord_cartesian(xlim = c(0, right_bound))
  if (!is.null(x_scale)) p <- p + x_scale
  p
}

# Per-condition DENSITY: one curve per ELIGIBLE condition (>= min_n finite
# values), a vertical dashed median line per condition with dodged white-halo
# labels, x-limit at the 99th percentile. The shared builder behind the coverage
# + length panels' "Per-condition" toggle mode (the CV panel keeps its own
# pelsa_cv_kde_plot, which carries the >=20-CV skipped-condition note).
#
# @param df         data.frame with a `condition` column + `value_col`.
# @param value_col  name of the numeric value column.
# @param value_fmt  function(value) -> string for the median labels.
# @param min_n      minimum finite values for a condition's density (default 2).
# @noRd
pelsa_per_condition_density_plot <- function(df, value_col,
                                             condition_order = NULL,
                                             x_label, title, subtitle = NULL,
                                             value_fmt = function(v) sprintf("%.1f", v),
                                             min_n = 2L,
                                             blank_msg = "No per-condition data to display.",
                                             x_scale = NULL, export = FALSE) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
      !all(c("condition", value_col) %in% names(df))) {
    return(pelsa_blank_plot(blank_msg))
  }
  d <- data.frame(condition = as.character(df$condition),
                  value = suppressWarnings(as.numeric(df[[value_col]])),
                  stringsAsFactors = FALSE)
  d <- d[is.finite(d$value) & !is.na(d$condition) & nzchar(d$condition), ,
         drop = FALSE]
  if (nrow(d) == 0L) return(pelsa_blank_plot(blank_msg))

  # Eligibility + display order: requested condition_order first (present only),
  # then any remaining conditions in natural order; drop conditions with < min_n.
  counts <- table(d$condition)
  present <- unique(d$condition)
  req <- as.character(condition_order %||% character(0))
  req <- req[!is.na(req) & nzchar(req)]
  ordered <- c(intersect(req, present), setdiff(present, req))
  eligible <- ordered[vapply(ordered, function(cond) {
    as.integer(counts[[cond]] %||% 0L) >= min_n
  }, logical(1))]
  if (length(eligible) == 0L) {
    return(pelsa_blank_plot(
      sprintf("No condition has >= %d values to draw a density.", min_n)))
  }
  d <- d[d$condition %in% eligible, , drop = FALSE]
  d$condition <- factor(d$condition, levels = eligible)

  x_hi <- stats::quantile(d$value, 0.99, na.rm = TRUE, names = FALSE)
  if (!is.finite(x_hi) || x_hi <= min(d$value, na.rm = TRUE)) {
    x_hi <- max(d$value, na.rm = TRUE)
  }
  x_lo <- min(0, min(d$value, na.rm = TRUE))

  medians <- stats::aggregate(value ~ condition, data = d,
                              FUN = function(x) stats::median(x, na.rm = TRUE))
  peak <- tryCatch(max(stats::density(d$value)$y, na.rm = TRUE),
                   error = function(e) 1)
  if (!is.finite(peak) || peak <= 0) peak <- 1
  medians$y <- pelsa_dodge_offsets(nrow(medians), y_top = peak * 0.95,
                                   y_range = peak)
  medians$x <- medians$value
  # Disclose the per-condition n alongside each median so a curve drawn from a
  # handful of values is self-evidently noisy (rather than presented as an
  # authoritative median). Mirrors the CV-KDE labels.
  medians$n <- as.integer(counts[as.character(medians$condition)])
  medians$label <- vapply(seq_len(nrow(medians)), function(i) {
    if (export) {
      sprintf("median = %s", value_fmt(medians$value[i]))
    } else {
      sprintf("%s median = %s (n=%d)", medians$condition[i],
              value_fmt(medians$value[i]), medians$n[i])
    }
  }, character(1))

  base_theme <- protigy_plot_theme()
  if (export) base_theme$plot.title.position <- NULL
  p <- ggplot(d, aes(x = .data$value, color = .data$condition,
                fill = .data$condition)) +
    geom_density(alpha = 0.15) +
    geom_vline(data = medians,
               aes(xintercept = .data$x, color = .data$condition),
               linetype = "dashed", show.legend = FALSE) +
    pelsa_halo_text_layers(medians, x_hi = x_hi, peak = peak) +
    geom_text(data = medians,
              aes(x = .data$x, y = .data$y, label = .data$label,
                  color = .data$condition),
              hjust = -0.05, size = 3, show.legend = FALSE, fontface = "bold") +
    coord_cartesian(xlim = c(x_lo, x_hi)) +
    labs(x = x_label, y = "Density", color = "Condition", fill = "Condition",
         title = title, subtitle = subtitle) +
    base_theme +
    guides(color = guide_legend(override.aes = list(size = 2)),
           fill  = guide_legend(override.aes = list(size = 2)))
  if (export) {
    p <- p + ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.text     = ggplot2::element_text(size = 8, colour = "black"))
  }
  if (!is.null(x_scale)) p <- p + x_scale
  p
}

# 6A: per-protein sequence coverage bar+error-bar (mean +/- SD across
# replicate samples per condition, or pooled experiment-wide). @noRd
pelsa_coverage_plot <- function(coverage_by_sample, condition_map,
                                condition_order = NULL,
                                mode = c("overall", "per_condition"),
                                min_replicates = 2L, export = FALSE) {
  mode <- match.arg(mode)
  bar_df <- if (identical(mode, "per_condition")) {
    pelsa_bar_error_data(coverage_by_sample, "coverage", condition_map,
                        condition_order, min_replicates)$data
  } else {
    pelsa_bar_error_data_overall(coverage_by_sample, "coverage", min_replicates)
  }
  pelsa_condition_bar_plot(
    bar_df, y_label = "Sequence coverage (%)",
    title = "Per-protein sequence coverage", fill = "#4e79a7",
    y_fmt = function(v) sprintf("%.1f%%", 100 * v),
    blank_msg = sprintf(
      "No condition has >= %d replicate samples with coverage data.",
      min_replicates),
    export = export)
}

# 6A: peptide-length bar+error-bar (mean +/- SD across replicate samples per
# condition, or pooled experiment-wide). @noRd
pelsa_length_plot <- function(length_by_sample, condition_map,
                              condition_order = NULL,
                              mode = c("overall", "per_condition"),
                              min_replicates = 2L, export = FALSE) {
  mode <- match.arg(mode)
  bar_df <- if (identical(mode, "per_condition")) {
    pelsa_bar_error_data(length_by_sample, "mean_length", condition_map,
                        condition_order, min_replicates)$data
  } else {
    pelsa_bar_error_data_overall(length_by_sample, "mean_length", min_replicates)
  }
  pelsa_condition_bar_plot(
    bar_df, y_label = "Peptide length (residues)",
    title = "Peptide-length distribution", fill = "#59a14f",
    y_fmt = function(v) sprintf("%.1f", v),
    blank_msg = sprintf(
      "No condition has >= %d replicate samples with length data.",
      min_replicates),
    export = export)
}

# 6B: experiment-wide CV DENSITY (pooled across conditions). Unlike the
# per-condition KDE (which drops conditions with < 20 finite CVs), the pooled
# view intentionally includes EVERY "ok" CV -- pooling is exactly what makes a
# small condition's CVs usable. The subtitle discloses the pooled count so the
# two toggle modes are not silently describing different universes. @noRd
pelsa_cv_overall_plot <- function(cv) {
  vals <- pelsa_cv_ok_values(cv)
  subtitle <- if (length(vals) > 0L)
    sprintf("all conditions pooled (n = %d CVs)", length(vals)) else NULL
  # Clamp the pooled density to the 99th percentile of the same ok CVs, mirroring
  # the per-condition KDE (pelsa_cv_kde_plot) so the two toggle modes share a
  # scale. NULL when there are no values, leaving the blank-plot path untouched.
  x_hi <- if (length(vals) > 0L)
    stats::quantile(vals, 0.99, na.rm = TRUE, names = FALSE) else NULL
  pelsa_overall_density_plot(
    vals, x_label = "CV (%)", title = "CV distribution", fill = "#af7aa1",
    value_fmt = function(v) sprintf("%.1f%%", v), subtitle = subtitle,
    blank_msg = "No CV data - a raw GCT + condition column are required.",
    x_hi = x_hi)
}

# 6A: missed-cleavage RATE bar+error-bar (mean +/- SD across replicate
# samples per condition, or pooled experiment-wide). Rate = fraction of a
# sample's quantified peptides with >= 1 missed cleavage. @noRd
pelsa_missed_cleavage_plot <- function(missed_cleavage_rate_by_sample,
                                       condition_map, condition_order = NULL,
                                       mode = c("overall", "per_condition"),
                                       min_replicates = 2L, export = FALSE) {
  mode <- match.arg(mode)
  bar_df <- if (identical(mode, "per_condition")) {
    pelsa_bar_error_data(missed_cleavage_rate_by_sample, "rate",
                        condition_map, condition_order, min_replicates)$data
  } else {
    pelsa_bar_error_data_overall(missed_cleavage_rate_by_sample, "rate",
                                 min_replicates)
  }
  pelsa_condition_bar_plot(
    bar_df, y_label = "Missed-cleavage rate (%)",
    title = "Missed-cleavage distribution", fill = "#f28e2b",
    y_fmt = function(v) sprintf("%.1f%%", 100 * v),
    blank_msg = sprintf(
      "No condition has >= %d replicate samples with missed-cleavage data.",
      min_replicates),
    export = export)
}

# 6B: per-condition CV KDE. One density curve per ELIGIBLE condition (>= 20
# finite "ok" CVs), a vertical dashed median line per condition (labels dodged),
# x-limit at the 99th percentile of cv_pct. @noRd
pelsa_cv_kde_plot <- function(cv, condition_order = NULL, export = FALSE) {
  if (is.null(cv) || !is.data.frame(cv) || nrow(cv) == 0L) {
    return(pelsa_blank_plot("No CV data - a raw GCT + condition column are required."))
  }
  elig <- pelsa_cv_kde_eligibility(cv, condition_order)
  if (length(elig$eligible) == 0L) {
    return(pelsa_blank_plot(
      "No condition has >= 20 finite CVs to draw a KDE."))
  }
  ok <- cv[!is.na(cv$cv_status) & cv$cv_status == "ok" &
             cv$condition %in% elig$eligible &
             is.finite(cv$cv_pct), , drop = FALSE]
  if (nrow(ok) == 0L) return(pelsa_blank_plot("No finite CVs to display."))

  ok$condition <- factor(ok$condition, levels = elig$eligible)
  x_hi <- stats::quantile(ok$cv_pct, 0.99, na.rm = TRUE, names = FALSE)
  if (!is.finite(x_hi) || x_hi <= 0) x_hi <- max(ok$cv_pct, na.rm = TRUE)

  medians <- stats::aggregate(cv_pct ~ condition, data = ok,
                              FUN = function(x) stats::median(x, na.rm = TRUE))
  # Estimate the density peak height to anchor the dodged median labels near the
  # top, then dodge each condition's label DOWNWARD so they never overlap (the
  # same pattern the length-density plot uses).
  peak <- tryCatch(max(stats::density(ok$cv_pct)$y, na.rm = TRUE),
                   error = function(e) 1)
  if (!is.finite(peak) || peak <= 0) peak <- 1
  medians$y <- pelsa_dodge_offsets(nrow(medians), y_top = peak * 0.95,
                                   y_range = peak)
  medians$x <- medians$cv_pct
  cv_counts <- table(ok$condition)
  medians$n <- as.integer(cv_counts[as.character(medians$condition)])
  medians$label <- sprintf("%s median = %.1f%% (n=%d)", medians$condition,
                           medians$cv_pct, medians$n)

  base_theme <- protigy_plot_theme()
  if (export) base_theme$plot.title.position <- NULL
  p <- ggplot(ok, aes(x = .data$cv_pct, color = .data$condition,
                 fill = .data$condition)) +
    geom_density(alpha = 0.15) +
    geom_vline(data = medians,
               aes(xintercept = .data$x, color = .data$condition),
               linetype = "dashed", show.legend = FALSE) +
    # White halo behind the median labels (multiple nudged white copies) so the
    # text stays readable over overlapping density curves, then the colored text.
    pelsa_halo_text_layers(medians, x_hi = x_hi, peak = peak) +
    geom_text(data = medians,
              aes(x = .data$x, y = .data$y, label = .data$label,
                  color = .data$condition),
              hjust = -0.05, size = 3, show.legend = FALSE, fontface = "bold") +
    coord_cartesian(xlim = c(0, x_hi)) +
    labs(x = "CV (%)", y = "Density", color = "Condition", fill = "Condition",
         title = "Per-condition CV distribution") +
    base_theme +
    guides(color = guide_legend(override.aes = list(size = 2)),
           fill  = guide_legend(override.aes = list(size = 2)))
  if (export) {
    p <- p + ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.text     = ggplot2::element_text(size = 8, colour = "black"))
  }
  p
}

# White-halo outline for the per-condition median labels. ggplot has no native
# text-halo, and shadowtext does not round-trip through ggplotly. We emulate one
# by drawing the label several times in white UNDER the colored text, each copy
# offset by a small fraction of the x/y extents. The offsets are baked into the
# DATA (new x/y columns) rather than applied with nudge_x/nudge_y: ggplotly
# silently drops position_nudge, collapsing the halo onto one point, whereas
# pre-offset coordinates survive the round-trip. Eight offsets (cardinal +
# diagonal). Returns ONE geom_text layer over the expanded frame.
# @noRd
pelsa_halo_text_layers <- function(medians, x_hi, peak, size = 3) {
  # Tight, symmetric ring: small fraction of the axis extents, eight directions
  # (cardinal + diagonal) so the white copies form a halo around the glyphs
  # rather than a one-sided shadow. Diagonal copies use 1/sqrt(2) so all eight
  # sit on a circle of roughly equal radius.
  dx <- (if (is.finite(x_hi) && x_hi > 0) x_hi else 1) * 0.0025
  dy <- (if (is.finite(peak) && peak > 0) peak else 1) * 0.006
  d <- 1 / sqrt(2)
  offs <- data.frame(
    ox = c(-1, 1,  0, 0, -d,  d, -d, d),
    oy = c( 0, 0, -1, 1, -d, -d,  d, d)
  )
  halo <- do.call(rbind, lapply(seq_len(nrow(offs)), function(i) {
    h <- medians
    h$x <- medians$x + offs$ox[i] * dx
    h$y <- medians$y + offs$oy[i] * dy
    h
  }))
  geom_text(data = halo,
            aes(x = .data$x, y = .data$y, label = .data$label),
            color = "white", hjust = -0.05, size = size,
            inherit.aes = FALSE, show.legend = FALSE, fontface = "bold")
}

# 6C: per-sample depth bar, ordered by sample_order (alphabetical fallback).
# @noRd
pelsa_depth_bar_plot <- function(n_quantified, sample_order = NULL,
                                 head_frac = 0.04, export = FALSE) {
  df <- pelsa_depth_bar_data(n_quantified, sample_order)
  if (nrow(df) == 0L) {
    return(pelsa_blank_plot("No per-sample depth data."))
  }
  # Lift each count label `head_frac` of the tallest bar ABOVE the bar top
  # (in-app default 0.04; the export path passes a smaller value). Baked into
  # label_y (ggplotly drops nudge_y); vjust = 0 anchors the label bottom.
  df$label_y <- df$n + head_frac * max(df$n, na.rm = TRUE)
  x_title <- if (export) NULL else "Sample"
  label_size <- if (export) 4 else 3
  x_text_size <- if (export) 9 else 11
  p <- ggplot(df, aes(x = .data$sample, y = .data$n)) +
    geom_col(fill = "#76b7b2") +
    geom_text(aes(y = .data$label_y, label = prettyNum(.data$n, big.mark = ",")),
              vjust = 0, size = label_size, fontface = "bold") +
    scale_y_continuous(labels = scales::label_comma(),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(x = x_title, y = "Peptides quantified",
         title = "Peptides quantified per sample") +
    protigy_plot_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = x_text_size,
                                     colour = if (export) "black" else NULL))
  if (export) {
    # Deleting the list element (not assigning theme(plot.title.position=NULL))
    # is what actually drops the "plot"-wide-centering override so the title
    # falls back to ggplot2's panel-centered default -- a `+ theme(x = NULL)`
    # merge does NOT unset an already-set element in this ggplot2 version.
    p$theme$plot.title.position <- NULL
    p <- p + ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text  = ggplot2::element_text(size = 8, colour = "black"))
  }
  p
}

################################################################################
# Export builders (6E) - re-derive each file from the cache entry
################################################################################

# Per-sample QC summary: one row per sample, the non-NA peptide count (depth).
# @noRd
pelsa_qc_sample_summary <- function(entry) {
  nq <- entry$n_quantified
  data.frame(
    sample = names(nq) %||% character(0),
    n_peptides_quantified = as.integer(nq %||% integer(0)),
    stringsAsFactors = FALSE
  )
}

# Per-condition QC summary: median/mean CV (unchanged, from the per-peptide
# cv frame), and mean/sd of the per-sample-averaged coverage, peptide-length,
# and missed-cleavage rate (matching the Summary bar+error-bar panels).
# Columns absent from the cache are simply omitted (graceful).
# @noRd
pelsa_qc_condition_summary <- function(entry, condition_order = NULL) {
  cv <- entry$cv %||% data.frame()
  agg_cv <- function(fun) {
    if (!is.data.frame(cv) || nrow(cv) == 0L ||
        !all(c("condition", "cv_pct") %in% colnames(cv))) {
      return(stats::setNames(numeric(0), character(0)))
    }
    tapply(as.numeric(cv$cv_pct), as.character(cv$condition), function(x) {
      x <- x[is.finite(x)]
      if (length(x) == 0L) NA_real_ else fun(x)
    })
  }
  med_cv  <- agg_cv(stats::median)
  mean_cv <- agg_cv(mean)

  cmap <- entry$condition_map %||% character(0)
  bar_stats <- function(per_sample_df, value_col) {
    agg <- pelsa_bar_error_data(per_sample_df %||% data.frame(), value_col,
                                cmap, condition_order, min_replicates = 1L)$data
    list(mean = stats::setNames(agg$mean, agg$condition),
        sd   = stats::setNames(agg$sd, agg$condition))
  }
  cov_stats <- bar_stats(entry$coverage_by_sample, "coverage")
  len_stats <- bar_stats(entry$length_by_sample, "mean_length")
  mc_stats  <- bar_stats(entry$missed_cleavage_rate_by_sample, "rate")

  conds <- unique(c(names(med_cv), names(cov_stats$mean), names(len_stats$mean),
                    names(mc_stats$mean)))
  if (length(conds) == 0L) return(data.frame())
  if (!is.null(condition_order)) {
    ordered <- intersect(condition_order, conds)
    conds <- c(ordered, setdiff(conds, ordered))
  }
  # n_peptides_quantified = peptides QUANTIFIED (canonical finite & non-zero) in
  # >= 1 sample of the condition, taken from the cache entry's per-condition
  # membership count (pelsa_condition_membership). This matches the per-sample
  # summary's "quantified" semantics; it is NOT a count of all CV rows (which
  # includes peptides that are non-finite / all-NA within the condition).
  n_pep <- entry$n_peptides_by_condition %||% integer(0)
  # A condition can appear in `conds` (it has CV samples) yet have ZERO quantified
  # peptides, so it is absent from the membership-derived n_pep -> n_pep[conds]
  # would be NA. That count is genuinely 0 (no peptide quantified in the
  # condition), not "unknown", so coerce the missing case to 0L.
  n_quant <- as.integer(n_pep[conds])
  n_quant[is.na(n_quant)] <- 0L

  data.frame(
    condition                    = conds,
    n_peptides_quantified        = n_quant,
    median_cv_pct                = unname(med_cv[conds]),
    mean_cv_pct                  = unname(mean_cv[conds]),
    mean_coverage                = unname(cov_stats$mean[conds]),
    sd_coverage                  = unname(cov_stats$sd[conds]),
    mean_peptide_length          = unname(len_stats$mean[conds]),
    sd_peptide_length            = unname(len_stats$sd[conds]),
    mean_missed_cleavage_rate    = unname(mc_stats$mean[conds]),
    sd_missed_cleavage_rate      = unname(mc_stats$sd[conds]),
    stringsAsFactors = FALSE
  )
}

# Experiment-wide QC summary: totals + FASTA/annotation failure counts/
# percents, plus mean/sd of the per-sample-averaged coverage, peptide-length,
# and missed-cleavage rate pooled across ALL samples (matching the Summary
# "Experiment-wide" bar+error-bar panels).
# @noRd
pelsa_qc_experiment_summary <- function(entry) {
  qc <- entry$qc %||% list()
  n_total <- as.integer(qc$n_peptides %||% NA_integer_)
  n_unmatched <- as.integer(qc$n_unmatched_rows %||%
                              nrow(entry$unmatched %||% data.frame()))
  n_unann <- as.integer(qc$n_unannotated_accessions %||%
                          length(entry$unannotated %||% character(0)))
  n_acc <- nrow(entry$coverage %||% data.frame())  # distinct matched accessions
  pct <- function(num, den) if (is.na(den) || den <= 0L) NA_real_ else 100 * num / den

  overall_stats <- function(per_sample_df, value_col) {
    agg <- pelsa_bar_error_data_overall(per_sample_df %||% data.frame(),
                                        value_col, min_replicates = 1L)
    if (nrow(agg) == 0L) list(mean = NA_real_, sd = NA_real_)
    else list(mean = agg$mean, sd = agg$sd)
  }
  cov <- overall_stats(entry$coverage_by_sample, "coverage")
  len <- overall_stats(entry$length_by_sample, "mean_length")
  mc  <- overall_stats(entry$missed_cleavage_rate_by_sample, "rate")

  data.frame(
    n_peptides_total             = n_total,
    n_unmatched_peptides         = n_unmatched,
    pct_unmatched_peptides       = pct(n_unmatched, n_total),
    n_unannotated_proteins       = n_unann,
    pct_unannotated_proteins     = pct(n_unann, n_acc),
    mean_missed_cleavage_rate    = mc$mean,
    sd_missed_cleavage_rate      = mc$sd,
    mean_coverage                = cov$mean,
    sd_coverage                  = cov$sd,
    mean_peptide_length          = len$mean,
    sd_peptide_length            = len$sd,
    stringsAsFactors = FALSE
  )
}

# Build the per-ome export bundle for ONE analyzed dataset. Returns a single
# `qc` function that writes the three summary CSVs + five figures into the
# 02_qc/ stage subfolder. condition_order / sample_order honor the user's
# confirmed ordering (NULL -> the builders' alphabetical fallback).
# @noRd
pelsa_section2_exports_for <- function(entry, ome, condition_order = NULL,
                                       sample_order = NULL, gct = NULL,
                                       marker_accs = NULL, params = NULL,
                                       custom = NULL) {
  qc_bundle <- function(dir_name) {
    out <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_QC)

    utils::write.csv(pelsa_qc_sample_summary(entry),
                     file.path(out, "qc_sample_summary.csv"), row.names = FALSE)
    utils::write.csv(pelsa_qc_condition_summary(entry, condition_order),
                     file.path(out, "qc_condition_summary.csv"), row.names = FALSE)
    utils::write.csv(pelsa_qc_experiment_summary(entry),
                     file.path(out, "qc_experiment_summary.csv"), row.names = FALSE)

    cvd <- entry$cv %||% data.frame()
    nq  <- entry$n_quantified
    save_fig <- function(p, base, w = 5.6, h = 3.5) tryCatch(
      pelsa_save_figure(p, out, base, width = w, height = h),
      error = function(e) NULL)

    cbs <- entry$coverage_by_sample %||% data.frame()
    lbs <- entry$length_by_sample %||% data.frame()
    mbs <- entry$missed_cleavage_rate_by_sample %||% data.frame()
    cmap <- entry$condition_map %||% character(0)
    if (nrow(cbs) > 0L) {
      save_fig(pelsa_coverage_plot(cbs, cmap, mode = "overall", export = TRUE),
               "coverage_distribution_experiment_wide")
      save_fig(pelsa_coverage_plot(cbs, cmap, condition_order,
                                  mode = "per_condition", export = TRUE),
               "coverage_distribution_per_condition")
    }
    if (nrow(lbs) > 0L) {
      save_fig(pelsa_length_plot(lbs, cmap, mode = "overall", export = TRUE),
               "peptide_length_density_experiment_wide")
      save_fig(pelsa_length_plot(lbs, cmap, condition_order,
                                 mode = "per_condition", export = TRUE),
               "peptide_length_density_per_condition")
    }
    if (nrow(mbs) > 0L) {
      save_fig(pelsa_missed_cleavage_plot(mbs, cmap, mode = "overall",
                                          export = TRUE),
               "missed_cleavage_rate_experiment_wide")
      save_fig(pelsa_missed_cleavage_plot(mbs, cmap, condition_order,
                                          mode = "per_condition", export = TRUE),
               "missed_cleavage_rate_per_condition")
    }
    if (is.data.frame(cvd) && nrow(cvd) > 0L)
      save_fig(pelsa_cv_kde_plot(cvd, condition_order, export = TRUE), "cv_kde")
    if (length(nq) > 0L)
      save_fig(pelsa_depth_bar_plot(nq, sample_order, head_frac = 0.02, export = TRUE),
               "n_peptides_per_sample")

    if (!is.null(gct)) {
      tryCatch(
        pelsa_splot_export_for(dir_name, gct, entry$matched, marker_accs,
                               params, custom),
        error = function(e) warning(sprintf(
          "pelsa_section2_exports_for: S-plot export failed for '%s': %s",
          ome, conditionMessage(e)), call. = FALSE))
    }

    invisible(out)
  }
  list(qc = qc_bundle)
}
