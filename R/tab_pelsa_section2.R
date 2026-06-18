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

      # The bottom QC tables auto-expand when they carry rows (only empty tables
      # stay collapsed) - decide each from the cache entry at render time.
      has_unmatched <-
        (entry$qc$n_unmatched_rows %||% nrow(entry$unmatched %||% data.frame())) > 0L
      has_unannotated <-
        (entry$qc$n_unannotated_accessions %||% length(entry$unannotated)) > 0L
      pelsa_section2_dashboard_ui(ns, ome,
                                  has_unmatched = has_unmatched,
                                  has_unannotated = has_unannotated)
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

    # Per-protein sequence coverage - experiment-wide density (default) OR a
    # per-condition density (toggle).
    coverage_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      if ((entry$qc$n_matched_rows %||% 0L) == 0L) {
        return(pelsa_blank_plot(
          "No peptides mapped to FASTA - check species / FASTA."))
      }
      if (identical(input$coverage_mode %||% "overall", "per_condition")) {
        pelsa_coverage_by_condition_plot(entry$coverage_by_condition,
                                         active_condition_order())
      } else {
        pelsa_coverage_distribution_plot(entry$coverage)
      }
    })
    output$coverage_plot <- renderPlotly({
      ggplotly(coverage_plot_reactive())
    })

    # Peptide-length density - experiment-wide (default) OR per-condition (toggle).
    length_plot_reactive <- reactive({
      entry <- active_entry()
      req(entry)
      if (identical(input$length_mode %||% "overall", "per_condition")) {
        pelsa_length_by_condition_plot(entry$length_by_condition,
                                       active_condition_order())
      } else {
        pelsa_length_density_plot(entry$peptide_metrics)
      }
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
      stats::setNames(lapply(datasets, function(ome) {
        co <- if (is.null(ss)) NULL else ss$condition_order[[ome]]
        so <- if (is.null(ss)) NULL else ss$sample_order[[ome]]
        pelsa_section2_exports_for(cache[[ome]], ome,
                                   condition_order = co, sample_order = so)
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
                                        has_unannotated = FALSE) {
  tagList(
    # 6A value boxes (inline counts incl. the 6D mapping/annotation QC totals).
    fluidRow(
      shinydashboard::valueBoxOutput(ns("total_peptide_ids"), width = 3),
      shinydashboard::valueBoxOutput(ns("fully_quantified_count"), width = 3),
      shinydashboard::valueBoxOutput(ns("failed_match_count"), width = 3),
      shinydashboard::valueBoxOutput(ns("failed_annotation_count"), width = 3)
    ),

    # 6C depth bar + 6A missed cleavage.
    fluidRow(
      shinydashboardPlus::box(
        plotlyOutput(ns("depth_plot")),
        title = "Peptides quantified per sample", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      ),
      shinydashboardPlus::box(
        plotlyOutput(ns("missed_plot")),
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
        title = "Per-protein sequence coverage", status = "primary",
        width = 6, headerBorder = TRUE, solidHeader = TRUE
      ),
      shinydashboardPlus::box(
        pelsa_mode_toggle(ns, "length_mode", "overall"),
        plotlyOutput(ns("length_plot")),
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

# Experiment-wide DENSITY with BOTH a dashed mean and a dashed median line, the
# text annotations vertically dodged so they don't overlap. The shared builder
# behind the coverage + length panels' "Experiment-wide" toggle mode.
#
# @param vals      numeric values (NA / non-finite dropped).
# @param value_fmt function(value) -> string used in the "mean = .." / "median =
#                  .." labels (e.g. coverage formats as a percentage).
# @noRd
pelsa_overall_density_plot <- function(vals, x_label, title,
                                       value_fmt = function(v) sprintf("%.1f", v),
                                       fill = "#59a14f", subtitle = NULL,
                                       blank_msg = "Not enough values for a density.") {
  vals <- vals[is.finite(vals)]
  if (length(vals) < 2L) return(pelsa_blank_plot(blank_msg))
  m  <- mean(vals)
  md <- stats::median(vals)
  y_top <- tryCatch(max(stats::density(vals)$y, na.rm = TRUE),
                    error = function(e) 1)
  if (!is.finite(y_top) || y_top <= 0) y_top <- 1
  ys <- pelsa_dodge_offsets(2L, y_top = y_top * 0.95, y_range = y_top)
  df <- data.frame(x = vals)

  ggplot(df, aes(x = .data$x)) +
    geom_density(fill = fill, alpha = 0.4, color = fill) +
    geom_vline(xintercept = m,  linetype = "dashed", color = "#e15759") +
    geom_vline(xintercept = md, linetype = "dashed", color = "#4e79a7") +
    annotate("text", x = m,  y = ys[1], label = paste0("mean = ", value_fmt(m)),
             color = "#e15759", hjust = -0.05, size = 3.2) +
    annotate("text", x = md, y = ys[2],
             label = paste0("median = ", value_fmt(md)),
             color = "#4e79a7", hjust = -0.05, size = 3.2) +
    labs(x = x_label, y = "Density", title = title, subtitle = subtitle) +
    theme_bw()
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
                                             x_label, title,
                                             value_fmt = function(v) sprintf("%.1f", v),
                                             min_n = 2L,
                                             blank_msg = "No per-condition data to display.") {
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
    sprintf("%s median = %s (n=%d)", medians$condition[i],
            value_fmt(medians$value[i]), medians$n[i])
  }, character(1))

  ggplot(d, aes(x = .data$value, color = .data$condition,
                fill = .data$condition)) +
    geom_density(alpha = 0.15) +
    geom_vline(data = medians,
               aes(xintercept = .data$x, color = .data$condition),
               linetype = "dashed", show.legend = FALSE) +
    pelsa_halo_text_layers(medians, x_hi = x_hi, peak = peak) +
    geom_text(data = medians,
              aes(x = .data$x, y = .data$y, label = .data$label,
                  color = .data$condition),
              hjust = -0.05, size = 3, show.legend = FALSE) +
    coord_cartesian(xlim = c(x_lo, x_hi)) +
    labs(x = x_label, y = "Density", color = "Condition", fill = "Condition",
         title = title) +
    theme_bw()
}

# 6A: per-protein sequence coverage DENSITY (experiment-wide mode). @noRd
pelsa_coverage_distribution_plot <- function(coverage) {
  vals <- pelsa_coverage_values(coverage)
  over_n <- pelsa_over_length_count(coverage)
  subtitle <- if (over_n > 0L)
    sprintf("%d clamped (over-length)", over_n) else NULL
  pelsa_overall_density_plot(
    vals, x_label = "Sequence coverage (fraction)",
    title = "Per-protein sequence coverage", fill = "#4e79a7",
    value_fmt = function(v) sprintf("%.1f%%", 100 * v), subtitle = subtitle,
    blank_msg = "Not enough coverage values for a density.")
}

# 6A: per-protein sequence coverage DENSITY (per-condition mode). @noRd
pelsa_coverage_by_condition_plot <- function(coverage_by_condition,
                                             condition_order = NULL) {
  pelsa_per_condition_density_plot(
    coverage_by_condition, value_col = "coverage",
    condition_order = condition_order,
    x_label = "Sequence coverage (fraction)",
    title = "Per-protein sequence coverage by condition",
    value_fmt = function(v) sprintf("%.1f%%", 100 * v),
    blank_msg = "No per-condition coverage - a condition column is required.")
}

# 6A: peptide-length DENSITY (experiment-wide mode). @noRd
pelsa_length_density_plot <- function(peptide_metrics) {
  vals <- pelsa_length_values(peptide_metrics)
  pelsa_overall_density_plot(
    vals, x_label = "Peptide length (residues)",
    title = "Peptide-length distribution", fill = "#59a14f",
    value_fmt = function(v) sprintf("%.1f", v),
    blank_msg = "Not enough peptides for a length density.")
}

# 6A: peptide-length DENSITY (per-condition mode). @noRd
pelsa_length_by_condition_plot <- function(length_by_condition,
                                           condition_order = NULL) {
  pelsa_per_condition_density_plot(
    length_by_condition, value_col = "peptide_length",
    condition_order = condition_order,
    x_label = "Peptide length (residues)",
    title = "Peptide-length distribution by condition",
    value_fmt = function(v) sprintf("%.1f", v),
    blank_msg = "No per-condition lengths - a condition column is required.")
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
  pelsa_overall_density_plot(
    vals, x_label = "CV (%)", title = "CV distribution", fill = "#af7aa1",
    value_fmt = function(v) sprintf("%.1f%%", v), subtitle = subtitle,
    blank_msg = "No CV data - a raw GCT + condition column are required.")
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
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = "Missed cleavages", y = "# of peptides",
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
  medians$x <- medians$cv_pct
  cv_counts <- table(ok$condition)
  medians$n <- as.integer(cv_counts[as.character(medians$condition)])
  medians$label <- sprintf("%s median = %.1f%% (n=%d)", medians$condition,
                           medians$cv_pct, medians$n)

  ggplot(ok, aes(x = .data$cv_pct, color = .data$condition,
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
              hjust = -0.05, size = 3, show.legend = FALSE) +
    coord_cartesian(xlim = c(0, x_hi)) +
    labs(x = "CV (%)", y = "Density", color = "Condition", fill = "Condition",
         title = "Per-condition CV distribution") +
    theme_bw()
}

# White-halo outline for the per-condition median labels. ggplot has no native
# text-halo, and shadowtext does not round-trip through ggplotly. We emulate one
# by drawing the label several times in white UNDER the colored text, each copy
# offset by a small fraction of the x/y extents. The offsets are baked into the
# DATA (new x/y columns) rather than applied with nudge_x/nudge_y: ggplotly
# silently drops position_nudge, collapsing the halo onto one point, whereas
# pre-offset coordinates survive the round-trip. Four cardinal offsets keep the
# plotly payload light. Returns ONE geom_text layer over the expanded frame.
# @noRd
pelsa_halo_text_layers <- function(medians, x_hi, peak, size = 3) {
  dx <- (if (is.finite(x_hi) && x_hi > 0) x_hi else 1) * 0.006
  dy <- (if (is.finite(peak) && peak > 0) peak else 1) * 0.012
  offs <- data.frame(ox = c(-1, 1, 0, 0), oy = c(0, 0, -1, 1))
  halo <- do.call(rbind, lapply(seq_len(nrow(offs)), function(i) {
    h <- medians
    h$x <- medians$x + offs$ox[i] * dx
    h$y <- medians$y + offs$oy[i] * dy
    h
  }))
  geom_text(data = halo,
            aes(x = .data$x, y = .data$y, label = .data$label),
            color = "white", hjust = -0.05, size = size,
            inherit.aes = FALSE, show.legend = FALSE)
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

# Per-condition QC summary: median/mean CV, sequence coverage and peptide length.
# Coverage + length come from the QC tab's per-condition cache fields
# (coverage_by_condition / length_by_condition); CV from the per-peptide cv
# frame. Columns absent from the cache are simply omitted (graceful).
# @noRd
pelsa_qc_condition_summary <- function(entry, condition_order = NULL) {
  agg <- function(df, col, fun) {
    if (!is.data.frame(df) || nrow(df) == 0L ||
        !all(c("condition", col) %in% colnames(df))) {
      return(stats::setNames(numeric(0), character(0)))
    }
    tapply(as.numeric(df[[col]]), as.character(df$condition), function(x) {
      x <- x[is.finite(x)]
      if (length(x) == 0L) NA_real_ else fun(x)
    })
  }
  cv  <- entry$cv %||% data.frame()
  cov <- entry$coverage_by_condition %||% data.frame()
  len <- entry$length_by_condition %||% data.frame()

  med_cv  <- agg(cv,  "cv_pct",         stats::median)
  mean_cv <- agg(cv,  "cv_pct",         mean)
  med_cov  <- agg(cov, "coverage",       stats::median)
  mean_cov <- agg(cov, "coverage",       mean)
  med_len  <- agg(len, "peptide_length", stats::median)
  mean_len <- agg(len, "peptide_length", mean)

  conds <- unique(c(names(med_cv), names(med_cov), names(med_len)))
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
    condition             = conds,
    n_peptides_quantified = n_quant,
    median_cv_pct         = unname(med_cv[conds]),
    mean_cv_pct           = unname(mean_cv[conds]),
    median_coverage       = unname(med_cov[conds]),
    mean_coverage         = unname(mean_cov[conds]),
    median_peptide_length = unname(med_len[conds]),
    mean_peptide_length   = unname(mean_len[conds]),
    stringsAsFactors = FALSE
  )
}

# Experiment-wide QC summary: totals + FASTA/annotation failure counts/percents.
# @noRd
pelsa_qc_experiment_summary <- function(entry) {
  qc <- entry$qc %||% list()
  n_total <- as.integer(qc$n_peptides %||% NA_integer_)
  n_unmatched <- as.integer(qc$n_unmatched_rows %||%
                              nrow(entry$unmatched %||% data.frame()))
  n_unann <- as.integer(qc$n_unannotated_accessions %||%
                          length(entry$unannotated %||% character(0)))
  n_acc <- nrow(entry$coverage %||% data.frame())  # distinct matched accessions
  pm <- entry$peptide_metrics %||% data.frame()
  mmc <- if (is.data.frame(pm) && "missed_cleavages" %in% colnames(pm) &&
             nrow(pm) > 0L)
    mean(as.numeric(pm$missed_cleavages), na.rm = TRUE) else NA_real_
  pct <- function(num, den) if (is.na(den) || den <= 0L) NA_real_ else 100 * num / den
  data.frame(
    n_peptides_total         = n_total,
    n_unmatched_peptides     = n_unmatched,
    pct_unmatched_peptides   = pct(n_unmatched, n_total),
    n_unannotated_proteins   = n_unann,
    pct_unannotated_proteins = pct(n_unann, n_acc),
    mean_missed_cleavages    = mmc,
    stringsAsFactors = FALSE
  )
}

# Build the per-ome export bundle for ONE analyzed dataset. Returns a single
# `qc` function that writes the three summary CSVs + five figures into the
# 02_qc/ stage subfolder. condition_order / sample_order honor the user's
# confirmed ordering (NULL -> the builders' alphabetical fallback).
# @noRd
pelsa_section2_exports_for <- function(entry, ome, condition_order = NULL,
                                       sample_order = NULL) {
  qc_bundle <- function(dir_name) {
    out <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_QC)

    utils::write.csv(pelsa_qc_sample_summary(entry),
                     file.path(out, "qc_sample_summary.csv"), row.names = FALSE)
    utils::write.csv(pelsa_qc_condition_summary(entry, condition_order),
                     file.path(out, "qc_condition_summary.csv"), row.names = FALSE)
    utils::write.csv(pelsa_qc_experiment_summary(entry),
                     file.path(out, "qc_experiment_summary.csv"), row.names = FALSE)

    cov <- entry$coverage %||% data.frame()
    pm  <- entry$peptide_metrics %||% data.frame()
    cvd <- entry$cv %||% data.frame()
    nq  <- entry$n_quantified
    save_fig <- function(p, base, w = 8, h = 5) tryCatch(
      pelsa_save_figure(p, out, base, width = w, height = h),
      error = function(e) NULL)

    if (is.data.frame(cov) && nrow(cov) > 0L)
      save_fig(pelsa_coverage_distribution_plot(cov), "coverage_distribution")
    if (is.data.frame(pm) && nrow(pm) > 0L) {
      save_fig(pelsa_length_density_plot(pm), "peptide_length_density")
      save_fig(pelsa_missed_cleavage_plot(pm), "missed_cleavage_bar")
    }
    if (is.data.frame(cvd) && nrow(cvd) > 0L)
      save_fig(pelsa_cv_kde_plot(cvd, condition_order), "cv_kde")
    if (length(nq) > 0L)
      save_fig(pelsa_depth_bar_plot(nq, sample_order), "n_peptides_per_sample")

    invisible(out)
  }
  list(qc = qc_bundle)
}
