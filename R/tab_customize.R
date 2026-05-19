################################################################################
# Module: CUSTOMIZE
#
# Shiny functions (UI and server) for the color customization tab.
#
# State model
# -----------
# Two reactiveVals are owned by this module:
#   * current_colors  - the live edits.  Returned to the parent so other tabs
#                       can read it via globals$colors (see app_server.R).
#   * restore_target  - the snapshot the "Restore last saved" button reverts to.
#                       Set when the module first sees globals$colors and again
#                       on successful import / explicit reset.
#
# Picker observers are registered ONE PER PICKER inside the renderUI for
# `output$color_pickers_ui`.  Old observers are destroyed before each render.
# `ignoreInit = TRUE` ensures programmatic writes to current_colors (which
# rebuild the picker UI with the new value) do not re-fire the observer with
# the value the parent just wrote -- that was the feedback loop the previous
# implementation papered over with an `importing` flag.
################################################################################

#' @importFrom colourpicker colourInput updateColourInput
# UI for the customize tab
customizeTabUI <- function(id = "customizeTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`

  preset_choices <- c("(custom)", "Paul Tol Bright", "Paul Tol Vibrant",
                      "Paul Tol Muted", "ColorBrewer Set2",
                      "ColorBrewer Paired", "Viridis")

  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      box(
        title = "Color Palette Customization",
        status = "primary",
        solidHeader = TRUE,
        width = 12,

        # Empty / preflight state -- gated by the server-set output flag.
        conditionalPanel(
          condition = sprintf("output['%s'] == false", ns("data_ready")),
          div(
            class = "text-center text-muted",
            style = "padding: 40px;",
            icon("info-circle", class = "text-info",
                 style = "font-size: 48px; margin-bottom: 20px;"),
            h4("Upload and process data to customize colors"),
            p("Please upload and process your GCT files in the Setup sidebar before customizing color schemes.",
              style = "font-size: 14px; margin-top: 15px;")
          )
        ),

        # Active state.
        conditionalPanel(
          condition = sprintf("output['%s'] == true", ns("data_ready")),

          # Mode + ome selector
          fluidRow(
            column(
              width = 3,
              selectInput(
                ns("color_mode"),
                label = "Apply colors to:",
                choices = c("All omes (synced)" = "multi_ome",
                            "One ome at a time" = "per_ome"),
                selected = "multi_ome"
              )
            ),
            column(
              width = 9,
              conditionalPanel(
                condition = "input.color_mode == 'per_ome'",
                ns = ns,
                uiOutput(ns("ome_selector_ui"))
              )
            )
          ),

          hr(),

          # Annotation column selector
          fluidRow(column(width = 12,
                          uiOutput(ns("annotation_column_selector_ui")))),

          hr(),

          # Color pickers grid
          uiOutput(ns("color_pickers_ui")),

          hr(),

          # Preset palette controls (H4) -- three flex children on one
          # line, all vertically centered on the selectInput's INPUT BOX
          # (not its label-plus-input column). The trick: render only
          # the selectInput's input box inside the flex row, with its
          # label as a sibling div absolutely positioned above. Then
          # plain align-items:center works correctly on the three real
          # controls (input, checkbox, button).
          fluidRow(column(
            width = 12,
            tags$div(
              style = "padding-top:6px;",
              # Static label above the row (mimics shiny's
              # selectInput label styling).
              tags$label(
                "Apply preset palette:",
                `for` = ns("preset_palette"),
                class = "control-label",
                style = "display:block; font-weight:700; margin-bottom:5px;"
              ),
              div(
                style = "display:flex; align-items:center; gap:16px;",
                div(
                  style = "flex: 0 0 280px;",
                  selectInput(
                    ns("preset_palette"),
                    label = NULL,
                    choices = preset_choices,
                    selected = "(custom)",
                    width = "100%"
                  )
                ),
                # Negate the checkbox's natural top margin so it
                # centers on the input's vertical midline.
                div(
                  style = "margin-top:-10px;",
                  checkboxInput(ns("reverse_palette"),
                                label = "Reverse", value = FALSE)
                ),
                div(
                  style = "margin-left:48px; margin-top:-10px;",
                  actionButton(ns("apply_preset"),
                               label = "Apply Preset",
                               icon = icon("paint-brush"),
                               class = "btn btn-primary")
                )
              )
            )
          )),

          # Preset preview strip (shows palette that will be applied)
          uiOutput(ns("swatch_preview_ui")),

          hr(),

          # Import / Export -- left column shrunk so the Export / Restore /
          # Reset buttons (which start immediately after the left column in
          # Bootstrap's grid) sit close to the YAML uploader. The right
          # column is offset by ~29px (fileInput's label height + bottom
          # margin) so the Export button's top aligns with the fileInput's
          # input box top, not its label.
          fluidRow(
            column(
              width = 4,
              fileInput(
                ns("import_yaml"),
                label = "Import Color Scheme (YAML):",
                accept = c(".yaml", ".yml"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
              ),
              helpText(
                "Expected: YAML with a top-level ", tags$code("colors:"),
                " section keyed by ome -> column -> value: hex."
              ),
              downloadLink(ns("download_example_yaml"),
                           label = "Download example YAML")
            ),
            column(
              width = 4,
              # Offset matches the fileInput's label-row height so the
              # button stack aligns with the fileInput's input box, not
              # its label.
              tags$div(
                style = "padding-top:22px;",
                downloadButton(
                  ns("export_yaml"),
                  label = "Export Current Scheme",
                  class = "btn btn-primary"
                ),
                br(), br(),
                actionButton(
                  ns("restore_defaults"),
                  label = "Restore last saved",
                  icon = icon("rotate-left"),
                  class = "btn btn-default"
                ),
                br(), br(),
                actionButton(
                  ns("reset_to_app_defaults"),
                  label = "Reset to app defaults",
                  icon = icon("eraser"),
                  class = "btn btn-default"
                )
              )
            )
          ),

          hr(),

          # Inline last-change status + Undo (H5)
          fluidRow(column(
            width = 12,
            div(
              style = "display:flex; align-items:center; gap:12px;",
              tags$strong("Status:"),
              textOutput(ns("last_change_text"), inline = TRUE),
              actionButton(ns("undo_last_change"),
                           label = "Undo",
                           icon = icon("arrow-rotate-left"),
                           class = "btn btn-sm btn-default")
            )
          )),

          hr(),
        )
      )
    )
  )
}


# server for the customize tab
customizeTabServer <- function(id = "customizeTab", GCTs_and_params, globals) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## ============================================================ INPUTS ==

    GCTs <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })

    GCTs_merged <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs_merged
    })

    parameters <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })

    all_omes <- reactive(names(GCTs()))

    default_annotations <- reactive({
      req(globals$default_annotations)
      globals$default_annotations
    })

    # Server-driven flag for the conditionalPanel gating. outputOptions with
    # suspendWhenHidden=FALSE ensures the value is delivered to JS even before
    # the panel body is shown for the first time.
    output$data_ready <- reactive(!is.null(GCTs_and_params()))
    outputOptions(output, "data_ready", suspendWhenHidden = FALSE)


    ## ====================================================== STATE (2 vals) ==

    # Live, current colors (returned to parent -- see app_server.R:39).
    current_colors  <- reactiveVal(NULL)
    # Snapshot the "Restore last saved" button reverts to. Set on first init
    # and on successful import / explicit reset.
    restore_target  <- reactiveVal(NULL)
    # Last change record -- list(prev_colors=..., desc=character(1)) -- drives
    # both the inline status text and the Undo button.
    last_change     <- reactiveVal(NULL)
    # Last import structured result (informational).
    import_meta     <- reactiveVal(NULL)
    # Snapshot of the FIRST colors the app showed for the current dataset.
    # Captured (and re-captured) by the structural-refresh observer below.
    # Reset uses this directly so we never re-derive via make_custom_colors()
    # at click-time -- avoids surfacing S4/requireNamespace errors and
    # decouples Reset from any post-init mutations of globals$colors.
    factory_defaults <- reactiveVal(NULL)

    # Registry of active per-picker observeEvents. We use an environment
    # (rather than a list with `<<-`) so that the registry's identity is
    # stable across reactive contexts and tests can introspect it.
    # Old observers are destroyed before each context-render so they don't
    # fire on stale picker IDs.
    picker_observers <- new.env(parent = emptyenv())


    ## ============================================ INITIALIZE FROM globals ==

    # Refresh current_colors only when:
    #   - it's never been set, OR
    #   - the structural signature of globals$colors no longer matches
    #     current_colors (new dataset uploaded, omes/columns changed).
    # Pure color-value differences in globals$colors are ignored -- that prevents
    # a feedback loop with app_server.R:39 which writes our own output back to
    # globals$colors.
    observeEvent(globals$colors, {
      req(globals$colors)

      incoming_sig <- colors_structure_signature(globals$colors)
      current_sig  <- colors_structure_signature(isolate(current_colors()))

      needs_refresh <-
        is.null(isolate(current_colors())) ||
        length(isolate(current_colors())) == 0 ||
        !identical(incoming_sig, current_sig)

      if (needs_refresh) {
        current_colors(globals$colors)
        restore_target(globals$colors)
        # Drop history that referenced the previous dataset's structure.
        # Without this, Undo would write a structurally-stale color list
        # back through app_server.R:39, corrupting downstream tabs.
        last_change(NULL)
        import_meta(NULL)
        # Pin "factory" to what the app first showed for THIS dataset.
        # Re-pinned on every structural change (new dataset upload).
        factory_defaults(globals$colors)
      }
    }, ignoreNULL = TRUE)


    ## ============================================ display_context (struct) ==

    # Cache the structural signature in a reactiveVal so downstream
    # consumers only invalidate when the SHAPE of current_colors() changes
    # (ome names, column names, condition values) -- not when hex values
    # change. We use a reactiveVal + observe pair instead of reactive()
    # because reactive() always invalidates downstream on every re-run,
    # even if its return value is identical to the previous run.
    # reactiveVal only invalidates downstream when set to a different value.
    struct_sig <- reactiveVal("")
    observe({
      cc <- current_colors()
      new_sig <- if (is.null(cc)) "" else colors_structure_signature(cc)
      if (!identical(new_sig, isolate(struct_sig()))) {
        struct_sig(new_sig)
      }
    })

    # Depends only on the STRUCTURAL signature of current_colors plus the
    # user's mode/ome/annotation selections. Color value edits do NOT
    # invalidate this reactive -- this is what keeps the picker grid from
    # being destroyed and rebuilt on every color tweak (issue C1).
    display_context_struct <- reactive({
      sig <- struct_sig()
      req(nzchar(sig), input$color_mode)

      colors <- isolate(current_colors())
      req(colors)

      display_ome <- if (input$color_mode == "multi_ome") {
        "multi_ome"
      } else {
        req(input$selected_ome)
        input$selected_ome
      }
      req(display_ome %in% names(colors))

      req(input$selected_annotation_column)
      annot_col <- input$selected_annotation_column
      req(annot_col %in% names(colors[[display_ome]]))

      color_info <- colors[[display_ome]][[annot_col]]
      req(isTRUE(color_info$is_discrete))

      list(
        display_ome = display_ome,
        annot_col   = annot_col,
        vals        = color_info$vals
      )
    })

    ## ====================================================== display_context ==

    # Live view -- structure + current color values. Use this where the
    # caller actually needs color hex values (swatch preview, preset apply).
    display_context <- reactive({
      s <- display_context_struct()
      req(s)
      colors <- current_colors()
      req(colors, s$display_ome %in% names(colors))
      req(s$annot_col %in% names(colors[[s$display_ome]]))
      color_info <- colors[[s$display_ome]][[s$annot_col]]
      list(
        display_ome = s$display_ome,
        annot_col   = s$annot_col,
        color_info  = color_info
      )
    })


    ## ================================================ ome selector UI ==

    output$ome_selector_ui <- renderUI({
      req(all_omes())
      selectInput(
        ns("selected_ome"),
        label = "Select Ome:",
        choices = all_omes(),
        selected = all_omes()[1]
      )
    })


    ## ============================== annotation column selector UI ==

    output$annotation_column_selector_ui <- renderUI({
      req(current_colors())
      colors <- current_colors()
      req(input$color_mode)

      display_ome <- if (input$color_mode == "multi_ome") {
        "multi_ome"
      } else {
        req(input$selected_ome)
        input$selected_ome
      }

      if (!(display_ome %in% names(colors))) return(NULL)

      all_annot_columns <- names(colors[[display_ome]])
      discrete_annot_columns <- all_annot_columns[
        vapply(all_annot_columns,
               function(col) isTRUE(colors[[display_ome]][[col]]$is_discrete),
               logical(1))
      ]

      if (length(discrete_annot_columns) == 0) {
        return(div(class = "alert alert-info",
                   "No discrete annotation columns available for color customization."))
      }

      # Determine default annotation column.
      req(default_annotations())
      default_annot <- if (display_ome == "multi_ome") {
        default_annots <- unique(unlist(default_annotations()))
        intersect(default_annots, discrete_annot_columns)[1]
      } else {
        candidate <- default_annotations()[[display_ome]]
        if (is.null(candidate) || !candidate %in% discrete_annot_columns) {
          discrete_annot_columns[1]
        } else {
          candidate
        }
      }
      if (is.null(default_annot) || length(default_annot) == 0 ||
          is.na(default_annot)) {
        default_annot <- discrete_annot_columns[1]
      }

      # Preserve current selection if still valid.
      current_selection <- isolate(input$selected_annotation_column)
      selected_annot <- if (!is.null(current_selection) &&
                              current_selection %in% discrete_annot_columns) {
        current_selection
      } else {
        default_annot
      }

      selectInput(
        ns("selected_annotation_column"),
        label = "Annotation Column:",
        choices = discrete_annot_columns,
        selected = selected_annot
      )
    })


    ## ========================================== preset swatch preview ==

    output$swatch_preview_ui <- renderUI({
      req(input$preset_palette)
      ctx <- tryCatch(display_context(), error = function(e) NULL)
      if (is.null(ctx)) return(NULL)

      # "(custom)" means there is no pending preset selection to preview.
      if (identical(input$preset_palette, "(custom)")) {
        return(
          div(
            class = "text-muted",
            style = "padding:6px 0 2px 0;",
            "Select a preset to preview colors before applying."
          )
        )
      }

      n <- length(ctx$color_info$vals)
      pal <- tryCatch(
        get_preset_palette(
          input$preset_palette,
          n,
          reverse = isTRUE(input$reverse_palette)
        ),
        error = function(e) NULL
      )
      if (is.null(pal)) return(NULL)

      div(
        style = "display:flex; gap:10px; flex-wrap:wrap; padding:8px 0 2px 0;",
        lapply(seq_along(ctx$color_info$vals), function(i) {
          val <- as.character(ctx$color_info$vals[i])
          col <- pal[i]
          div(
            style = "display:flex; flex-direction:column; align-items:center; min-width:60px;",
            div(
              style = sprintf(
                "background:%s; width:40px; height:24px; border:1px solid #ccc; border-radius:3px;",
                col
              ),
              title = sprintf("%s: %s", val, col)
            ),
            tags$span(
              style = "font-size:11px; margin-top:3px; max-width:80px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
              val
            )
          )
        })
      )
    })

    ## =================================================== color pickers ==

    # Picker observer registration is decoupled from the renderUI so that
    # tests using shiny::testServer (which runs no DOM) can exercise picker
    # change logic without rendering the UI. Both this observe() and the
    # renderUI below depend on display_context(), so they re-fire together.
    observe({
      ctx <- display_context_struct()
      # (debug instrumentation removed)

      # Destroy stale observers from the previous context.
      for (nm in ls(picker_observers, all.names = TRUE)) {
        try(picker_observers[[nm]]$destroy(), silent = TRUE)
        rm(list = nm, envir = picker_observers)
      }

      # Capture the context fields by value so each registered observer sees
      # its own (i, picker_id, display_ome, annot_col) -- without `force()` /
      # `local()` the lapply closures all reference the final iteration value.
      display_ome <- ctx$display_ome
      annot_col   <- ctx$annot_col
      vals        <- ctx$vals

      lapply(seq_along(vals), function(i) {
        local({
          local_i        <- i
          local_picker   <- paste0("color_", display_ome, "_", annot_col, "_", local_i)
          local_ome      <- display_ome
          local_col      <- annot_col

          obs <- observeEvent(input[[local_picker]], {
            new_color <- input[[local_picker]]
            if (is.null(new_color)) return()

            colors <- isolate(current_colors())
            if (is.null(colors)) return()
            if (!(local_ome %in% names(colors))) return()
            if (!(local_col %in% names(colors[[local_ome]]))) return()

            info <- colors[[local_ome]][[local_col]]
            if (local_i > length(info$colors)) return()
            cur <- info$colors[local_i]

            norm <- normalize_hex_color(new_color)
            if (is.na(norm)) return()
            # short-circuit on no-op (case-insensitive comparison)
            if (toupper(norm) == toupper(cur)) return()

            val_i <- as.character(info$vals[local_i])
            prev_colors <- colors  # snapshot for undo

            updated <- if (isolate(input$color_mode) == "multi_ome") {
              sync_colors_across_omes(colors, local_col, val_i, norm)
            } else {
              colors[[local_ome]][[local_col]]$colors[local_i] <- norm
              colors
            }
            current_colors(updated)
            last_change(list(
              prev_colors = prev_colors,
              desc = sprintf("%s (%s) -> %s", val_i, local_col, norm)
            ))
          }, ignoreInit = TRUE, ignoreNULL = TRUE)
          assign(local_picker, obs, envir = picker_observers)
        })
      })
    })

    output$color_pickers_ui <- renderUI({
      ctx <- display_context_struct()
      colors <- isolate(current_colors())
      req(colors)
      req(ctx$display_ome %in% names(colors))
      req(ctx$annot_col %in% names(colors[[ctx$display_ome]]))
      initial_hex <- colors[[ctx$display_ome]][[ctx$annot_col]]$colors

      pickers <- lapply(seq_along(ctx$vals), function(i) {
        picker_id <- paste0("color_", ctx$display_ome, "_", ctx$annot_col, "_", i)
        column(
          width = 3,
          colourpicker::colourInput(
            ns(picker_id),
            label = as.character(ctx$vals[i]),
            value = initial_hex[i],
            showColour = "both",
            palette = "square",
            allowedCols = NULL,
            closeOnClick = FALSE
          )
        )
      })

      tagList(
        tags$div(
          style = "font-weight:700; margin-bottom:10px;",
          paste("Selected Colors for:", ctx$annot_col)
        ),
        fluidRow(pickers)
      )
    })

    ## ============================== push value updates into existing pickers ==

    # Whenever current_colors() changes VALUES (without changing structure),
    # push the new hex into the already-rendered pickers via
    # updateColourInput. This keeps the popup open and the grid stable; the
    # renderUI above only re-fires when the structural context changes.
    observe({
      colors <- current_colors()
      req(colors)
      s <- isolate(display_context_struct())
      if (is.null(s)) return()
      if (!(s$display_ome %in% names(colors))) return()
      if (!(s$annot_col %in% names(colors[[s$display_ome]]))) return()
      hex <- colors[[s$display_ome]][[s$annot_col]]$colors
      for (i in seq_along(s$vals)) {
        if (i > length(hex)) next
        picker_id <- paste0("color_", s$display_ome, "_", s$annot_col, "_", i)
        colourpicker::updateColourInput(session, picker_id, value = hex[i])
      }
    })


    ## ============================================ inline status + Undo ==

    output$last_change_text <- renderText({
      lc <- last_change()
      if (is.null(lc)) "No recent changes." else paste("Last change:", lc$desc)
    })

    # Disable Undo when there's nothing to undo.
    observe({
      shinyjs::toggleState("undo_last_change", condition = !is.null(last_change()))
    })

    observeEvent(input$undo_last_change, {
      lc <- last_change()
      req(lc)
      current_colors(lc$prev_colors)
      undone_marker <- list(prev_colors = lc$prev_colors,
                            desc = paste("Undone:", lc$desc))
      last_change(undone_marker)
      # Clear after one cycle so Undo doesn't loop on itself. Identity-check
      # at flush time so we don't stomp a NEW change the user made between
      # the Undo click and the deferred callback firing.
      session$onFlushed(function() {
        if (identical(isolate(last_change()), undone_marker)) last_change(NULL)
      }, once = TRUE)
    })


    ## ===================================================== Apply preset ==

    observeEvent(input$apply_preset, {
      req(input$preset_palette)
      if (input$preset_palette == "(custom)") {
        showNotification("Select a preset palette before clicking Apply.",
                         type = "warning", duration = 3)
        return()
      }

      ctx <- tryCatch(display_context(), error = function(e) NULL)
      req(ctx)

      n <- length(ctx$color_info$vals)
      pal <- tryCatch(
        get_preset_palette(input$preset_palette, n,
                           reverse = isTRUE(input$reverse_palette)),
        error = function(e) {
          showNotification(paste("Failed to apply preset:", e$message),
                           type = "error", duration = 5)
          NULL
        }
      )
      req(pal)

      colors <- isolate(current_colors())
      prev_colors <- colors

      if (isolate(input$color_mode) == "multi_ome") {
        for (i in seq_along(ctx$color_info$vals)) {
          colors <- sync_colors_across_omes(
            colors, ctx$annot_col, as.character(ctx$color_info$vals[i]), pal[i]
          )
        }
      } else {
        colors[[ctx$display_ome]][[ctx$annot_col]]$colors <- pal
      }

      current_colors(colors)
      last_change(list(
        prev_colors = prev_colors,
        desc = sprintf("Preset \"%s\" applied to %s (%d colors)",
                       input$preset_palette, ctx$annot_col, n)
      ))
    })


    ## ======================================================= Import ==

    observeEvent(input$import_yaml, {
      req(input$import_yaml)
      if (is.null(current_colors()) || length(current_colors()) == 0) {
        shinyalert::shinyalert(
          title = "Data Not Ready",
          text  = "Please upload and process your data files first before importing a color scheme.",
          type  = "warning"
        )
        return()
      }
      file_path <- input$import_yaml$datapath

      tryCatch({
        res <- import_colors_from_yaml_full(file_path, isolate(current_colors()))
        import_meta(res)

        if (res$n_columns_updated == 0) {
          msg <- sprintf(
            "File parsed (%s format) but no columns matched current data. Check ome / column / value names.",
            res$format
          )
          if (length(res$missing_omes) > 0) {
            msg <- paste0(msg, "\nOmes in file not in session: ",
                          paste(res$missing_omes, collapse = ", "))
          }
          shinyalert::shinyalert(
            title = "Nothing changed",
            text  = msg,
            type  = "warning"
          )
          return()
        }

        prev_colors <- isolate(current_colors())
        current_colors(res$colors)
        restore_target(res$colors)
        last_change(list(
          prev_colors = prev_colors,
          desc = sprintf("Import (%s, %d columns updated)",
                         res$format, res$n_columns_updated)
        ))

        # Build success message
        msg <- sprintf("Imported (%s). %d column%s updated across %d ome%s.",
                       res$format,
                       res$n_columns_updated,
                       if (res$n_columns_updated == 1) "" else "s",
                       res$n_omes_in_yaml,
                       if (res$n_omes_in_yaml == 1) "" else "s")
        if (length(res$invalid_entries) > 0) {
          msg <- paste0(msg, sprintf(
            "\n%d invalid hex entr%s skipped: %s%s",
            length(res$invalid_entries),
            if (length(res$invalid_entries) == 1) "y" else "ies",
            paste(utils::head(res$invalid_entries, 5), collapse = "; "),
            if (length(res$invalid_entries) > 5) ", ..." else ""
          ))
        }
        if (length(res$missing_omes) > 0) {
          msg <- paste0(msg, "\nOmes in file not in session: ",
                        paste(res$missing_omes, collapse = ", "))
        }
        if (length(res$skipped_continuous_function_palettes) > 0) {
          msg <- paste0(msg,
            "\nSkipped continuous (function-form) palettes: ",
            paste(res$skipped_continuous_function_palettes, collapse = ", "))
        }
        if (isTRUE(res$alpha_stripped_count > 0)) {
          msg <- paste0(msg, sprintf(
            "\n%d hex entr%s carried alpha; normalized to 6-digit form.",
            res$alpha_stripped_count,
            if (res$alpha_stripped_count == 1) "y" else "ies"
          ))
        }

        shinyalert::shinyalert(
          title = "Import successful",
          text  = msg,
          type  = "success"
        )
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Import failed",
          text  = e$message,
          type  = "error"
        )
      })
    })


    ## ======================================================= Export ==

    output$export_yaml <- downloadHandler(
      filename = function() {
        paste0("color_palette_", Sys.Date(), ".yaml")
      },
      content = function(file) {
        req(current_colors())
        export_colors_to_yaml(current_colors(), file)
      }
    )

    output$download_example_yaml <- downloadHandler(
      filename = function() "protigy_color_scheme_example.yaml",
      content = function(file) {
        yaml::write_yaml(list(
          metadata = list(
            created_date = as.character(Sys.Date()),
            note = "Example Protigy color scheme. Edit and re-import."
          ),
          colors = list(
            multi_ome = list(
              Treatment = list(
                Control = "#4477AA",
                Treated = "#EE6677"
              )
            )
          )
        ), file)
      }
    )


    ## ======================================================= Restore ==

    observeEvent(input$restore_defaults, {
      tgt <- isolate(restore_target())
      if (is.null(tgt)) {
        showNotification("No saved restore target available.",
                         type = "warning", duration = 3)
        return()
      }
      if (identical(isolate(current_colors()), tgt)) {
        showNotification("Already at the saved restore point.",
                         type = "message", duration = 3)
        return()
      }

      shinyalert::shinyalert(
        title = "Restore last saved colors?",
        text  = "All edits since the last save/import will be replaced with the saved scheme.",
        type  = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Restore",
        cancelButtonText  = "Cancel",
        confirmButtonCol  = "#3c8dbc",
        callbackR = function(ok) {
          if (isTRUE(ok)) {
            prev <- isolate(current_colors())
            current_colors(tgt)
            last_change(list(prev_colors = prev,
                             desc = "Restored last saved colors"))
          }
        }
      )
    })

    observeEvent(input$reset_to_app_defaults, {
      shinyalert::shinyalert(
        title = "Reset to app defaults?",
        text  = "All customizations and any imported scheme will be discarded.",
        type  = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Reset",
        cancelButtonText  = "Cancel",
        # Destructive action -- use Bootstrap/AdminLTE danger red so the
        # button reads as "this will discard work", not "this is the
        # primary action". White text on red gives WCAG AA contrast.
        confirmButtonCol  = "#dd4b39",
        callbackR = function(ok) {
          if (!isTRUE(ok)) return()
          # Explicit guard: req() inside callbackR silent-fails (silent.shiny.error),
          # leaving the user with no feedback. Use showNotification instead.
          app_defaults <- isolate(factory_defaults())
          if (is.null(app_defaults)) {
            showNotification(
              "No app defaults available -- upload data first.",
              type = "error", duration = 4
            )
            return()
          }

          # shinyjs auto-namespaces via the current (module) session, so pass
          # the bare id -- passing ns(...) here would namespace twice.
          shinyjs::reset("import_yaml")

          prev <- isolate(current_colors())
          current_colors(app_defaults)
          restore_target(app_defaults)
          last_change(list(prev_colors = prev,
                           desc = "Reset to app defaults"))
          # Reset wipes the import provenance so any "imported X.yaml"
          # indicator vanishes after a factory reset.
          import_meta(NULL)
        }
      )
    })

    # Disable Restore button when current == target (nothing to restore).
    observe({
      ready <- !is.null(current_colors()) && !is.null(restore_target()) &&
        !identical(current_colors(), restore_target())
      shinyjs::toggleState("restore_defaults", condition = ready)
    })


    ## ============================================================ RETURN ==
    return(current_colors)

  })
}
