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
  "CV of sum-normalized (un-logged) intensities - replicate ",
  "reproducibility after loading/depth correction."
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

      pelsa_section2_dashboard_ui(ns, ome)
    })

    ## 6A - EXPERIMENT-WIDE ##

    output$total_peptide_ids <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else entry$qc$n_peptides
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Total peptide IDs (original rows)",
        icon     = icon("dna"),
        color    = "aqua"
      )
    })

    output$failed_match_count <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else
        (entry$qc$n_unmatched_rows %||% nrow(entry$unmatched))
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Peptides failed FASTA match",
        icon     = icon("triangle-exclamation"),
        color    = "yellow"
      )
    })

    output$failed_annotation_count <- shinydashboard::renderValueBox({
      entry <- active_entry()
      n <- if (is.null(entry)) NA_integer_ else
        (entry$qc$n_unannotated_accessions %||% length(entry$unannotated))
      shinydashboard::valueBox(
        value    = format(n %||% NA_integer_, big.mark = ","),
        subtitle = "Proteins failed annotation",
        icon     = icon("circle-question"),
        color    = "orange"
      )
    })

    # Per-protein sequence coverage distribution.
    coverage_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      if ((entry$qc$n_matched_rows %||% 0L) == 0L) {
        return(pelsa_blank_plot(
          "No peptides mapped to FASTA - check species / FASTA."))
      }
      pelsa_coverage_distribution_plot(entry$coverage)
    })
    output$coverage_plot <- renderPlotly({
      ggplotly(coverage_plot_reactive())
    })

    # Peptide-length DENSITY with dodged mean + median lines.
    length_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      pelsa_length_density_plot(entry$peptide_metrics)
    })
    output$length_plot <- renderPlotly({
      ggplotly(length_plot_reactive())
    })

    # Missed-cleavage bar.
    missed_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      pelsa_missed_cleavage_plot(entry$peptide_metrics)
    })
    output$missed_plot <- renderPlotly({
      ggplotly(missed_plot_reactive())
    })

    ## 6B - PER-CONDITION CV KDE ##

    cv_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      pelsa_cv_kde_plot(entry$cv, active_condition_order())
    })
    output$cv_plot <- renderPlotly({
      ggplotly(cv_plot_reactive())
    })
    output$cv_caption <- renderUI({
      tags$p(.PELSA_CV_CAPTION, style = "color:#6c757d; font-style:italic;")
    })
    output$cv_skipped_note <- renderUI({
      entry <- active_entry()
      req(entry)
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

    output$depth_table <- DT::renderDataTable({
      entry <- active_entry()
      req(entry)
      ds <- entry$depth_summary
      tab <- data.frame(
        Metric = c("Mean peptides / sample", "Median peptides / sample",
                   "CV (%) across samples", "Total peptides"),
        Value  = c(
          round(ds$mean_n %||% NA_real_, 1),
          round(ds$median_n %||% NA_real_, 1),
          round(ds$cv_pct %||% NA_real_, 2),
          ds$total_n_peptides %||% NA_integer_
        ),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      DT::datatable(tab, rownames = FALSE,
                    options = list(dom = "t", ordering = FALSE))
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
        options = list(pageLength = 10, scrollY = "300px", scrollCollapse = TRUE,
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
        options = list(pageLength = 10, scrollY = "300px", scrollCollapse = TRUE,
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
      stats::setNames(lapply(datasets, function(ome) {
        pelsa_section2_exports_for(cache[[ome]], ome)
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

# The full dashboard layout for one analyzed dataset (6A-6D). Pure markup; all
# inputIds/outputIds wrapped in ns().
# @noRd
pelsa_section2_dashboard_ui <- function(ns, ome) {
  tagList(
    # 6A value boxes (inline counts incl. the 6D mapping/annotation QC totals).
    fluidRow(
      shinydashboard::valueBoxOutput(ns("total_peptide_ids"), width = 4),
      shinydashboard::valueBoxOutput(ns("failed_match_count"), width = 4),
      shinydashboard::valueBoxOutput(ns("failed_annotation_count"), width = 4)
    ),

    # 6A coverage + peptide length.
    fluidRow(
      shinydashboardPlus::box(
        plotlyOutput(ns("coverage_plot")),
        title = "Per-protein sequence coverage", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      ),
      shinydashboardPlus::box(
        plotlyOutput(ns("length_plot")),
        title = "Peptide-length distribution", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      )
    ),

    # 6A missed cleavage + 6C depth bar.
    fluidRow(
      shinydashboardPlus::box(
        plotlyOutput(ns("missed_plot")),
        title = "Missed-cleavage distribution", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      ),
      shinydashboardPlus::box(
        plotlyOutput(ns("depth_plot")),
        DT::dataTableOutput(ns("depth_table")),
        title = "Peptides quantified per sample", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      )
    ),

    # 6B per-condition CV KDE.
    fluidRow(
      shinydashboardPlus::box(
        plotlyOutput(ns("cv_plot")),
        uiOutput(ns("cv_skipped_note")),
        uiOutput(ns("cv_caption")),
        title = "Per-condition CV (replicate reproducibility)",
        status = "primary", width = 12, headerBorder = TRUE, solidHeader = TRUE
      )
    ),

    # 6D collapsible QC tables PINNED AT THE BOTTOM (default collapsed).
    fluidRow(
      shinydashboardPlus::box(
        DT::dataTableOutput(ns("unmatched_table")),
        title = "QC: peptides that failed FASTA match",
        status = "warning", width = 6, headerBorder = TRUE, solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE
      ),
      shinydashboardPlus::box(
        DT::dataTableOutput(ns("unannotated_table")),
        title = "QC: proteins that failed annotation",
        status = "warning", width = 6, headerBorder = TRUE, solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE
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

# 6A: per-protein sequence coverage distribution (histogram of the fraction).
# @noRd
pelsa_coverage_distribution_plot <- function(coverage) {
  vals <- pelsa_coverage_values(coverage)
  if (length(vals) == 0L) {
    return(pelsa_blank_plot("No coverage values to display."))
  }
  med <- stats::median(vals, na.rm = TRUE)
  over_n <- pelsa_over_length_count(coverage)
  df <- data.frame(coverage = vals)
  subtitle <- sprintf("median coverage = %.1f%%", 100 * med)
  if (over_n > 0L) {
    subtitle <- sprintf("%s | %d clamped (over-length)", subtitle, over_n)
  }
  ggplot(df, aes(x = .data$coverage)) +
    geom_histogram(bins = 30, fill = "#4e79a7", color = "white") +
    geom_vline(xintercept = med, linetype = "dashed", color = "#e15759") +
    labs(x = "Sequence coverage (fraction)", y = "Proteins",
         title = "Per-protein sequence coverage", subtitle = subtitle) +
    theme_bw()
}

# 6A: peptide-length DENSITY with BOTH a dashed mean and a dashed median line,
# their text annotations vertically dodged so they don't overlap. @noRd
pelsa_length_density_plot <- function(peptide_metrics) {
  vals <- pelsa_length_values(peptide_metrics)
  if (length(vals) < 2L) {
    return(pelsa_blank_plot("Not enough peptides for a length density."))
  }
  m  <- mean(vals, na.rm = TRUE)
  md <- stats::median(vals, na.rm = TRUE)
  dens <- stats::density(vals)
  y_top <- max(dens$y, na.rm = TRUE)
  ys <- pelsa_dodge_offsets(2L, y_top = y_top * 0.95, y_range = y_top)
  df <- data.frame(length = vals)

  ggplot(df, aes(x = .data$length)) +
    geom_density(fill = "#59a14f", alpha = 0.4, color = "#59a14f") +
    geom_vline(xintercept = m,  linetype = "dashed", color = "#e15759") +
    geom_vline(xintercept = md, linetype = "dashed", color = "#4e79a7") +
    annotate("text", x = m,  y = ys[1], label = sprintf("mean = %.1f", m),
             color = "#e15759", hjust = -0.05, size = 3.2) +
    annotate("text", x = md, y = ys[2], label = sprintf("median = %.1f", md),
             color = "#4e79a7", hjust = -0.05, size = 3.2) +
    labs(x = "Peptide length (residues)", y = "Density",
         title = "Peptide-length distribution") +
    theme_bw()
}

# 6A: missed-cleavage bar (0,1,2,...). @noRd
pelsa_missed_cleavage_plot <- function(peptide_metrics) {
  df <- pelsa_missed_cleavage_data(peptide_metrics)
  if (nrow(df) == 0L) {
    return(pelsa_blank_plot("No missed-cleavage data."))
  }
  df$missed <- factor(df$missed, levels = sort(unique(df$missed)))
  ggplot(df, aes(x = .data$missed, y = .data$count)) +
    geom_col(fill = "#f28e2b") +
    labs(x = "Missed cleavages", y = "Peptides",
         title = "Missed-cleavage distribution") +
    theme_bw()
}

# 6B: per-condition CV KDE. One density curve per ELIGIBLE condition (>= 20
# finite "ok" CVs), a vertical dashed median line per condition (labels dodged),
# x-limit at the 99th percentile of cv_pct. @noRd
pelsa_cv_kde_plot <- function(cv, condition_order = NULL) {
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
  medians$label <- sprintf("%s median = %.1f%%", medians$condition,
                           medians$cv_pct)

  ggplot(ok, aes(x = .data$cv_pct, color = .data$condition,
                 fill = .data$condition)) +
    geom_density(alpha = 0.15) +
    geom_vline(data = medians,
               aes(xintercept = .data$cv_pct, color = .data$condition),
               linetype = "dashed", show.legend = FALSE) +
    geom_text(data = medians,
              aes(x = .data$cv_pct, y = .data$y, label = .data$label,
                  color = .data$condition),
              hjust = -0.05, size = 3, show.legend = FALSE) +
    coord_cartesian(xlim = c(0, x_hi)) +
    labs(x = "CV (%)", y = "Density", color = "Condition", fill = "Condition",
         title = "Per-condition CV distribution") +
    theme_bw()
}

# 6C: per-sample depth bar, ordered by sample_order (alphabetical fallback).
# @noRd
pelsa_depth_bar_plot <- function(n_quantified, sample_order = NULL) {
  df <- pelsa_depth_bar_data(n_quantified, sample_order)
  if (nrow(df) == 0L) {
    return(pelsa_blank_plot("No per-sample depth data."))
  }
  ggplot(df, aes(x = .data$sample, y = .data$n)) +
    geom_col(fill = "#76b7b2") +
    labs(x = "Sample", y = "Peptides quantified",
         title = "Peptides quantified per sample") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

################################################################################
# Export builders (6E) - re-derive each file from the cache entry
################################################################################

# Build the per-ome export list for ONE analyzed dataset's cache entry. Each
# element is a function(dir_name) that writes one CSV re-derived from the cache.
# @noRd
pelsa_section2_exports_for <- function(entry, ome) {
  write_one <- function(df, suffix) {
    function(dir_name) {
      utils::write.csv(
        df,
        file = file.path(dir_name, sprintf("pelsa_%s_%s.csv", suffix, ome)),
        row.names = FALSE
      )
    }
  }

  cv_df <- entry$cv %||% data.frame()
  coverage_df <- entry$coverage %||% data.frame()
  depth_df <- {
    nq <- entry$n_quantified
    per_sample <- data.frame(
      sample       = names(nq) %||% character(0),
      n_quantified = as.integer(nq %||% integer(0)),
      stringsAsFactors = FALSE
    )
    ds <- entry$depth_summary
    if (is.data.frame(ds) && nrow(ds) > 0L) {
      for (col in names(ds)) per_sample[[col]] <- ds[[col]][1]
    }
    per_sample
  }
  unmatched_df <- entry$unmatched %||% data.frame()
  unannotated_df <- data.frame(
    accession = as.character(entry$unannotated %||% character(0)),
    stringsAsFactors = FALSE
  )
  peptide_metrics_df <- entry$peptide_metrics %||% data.frame()

  list(
    cv              = write_one(cv_df, "cv"),
    coverage        = write_one(coverage_df, "coverage"),
    depth           = write_one(depth_df, "depth"),
    unmatched       = write_one(unmatched_df, "unmatched"),
    unannotated     = write_one(unannotated_df, "unannotated"),
    peptide_metrics = write_one(peptide_metrics_df, "peptide_metrics")
  )
}
