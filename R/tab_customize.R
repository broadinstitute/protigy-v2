################################################################################
# Module: CUSTOMIZE
#
# Shiny funcions (UI and server)
################################################################################

#' @import colourpicker
#' @import shinydashboard
# UI for the customize tab
customizeTabUI <- function(id = "customizeTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`

  tagList(
    fluidRow(
      box(
        title = "Color Palette Customization",
        status = "primary",
        solidHeader = TRUE,
        width = 12,

        # Top control panel
        fluidRow(
          column(
            width = 4,
            selectInput(
              ns("color_mode"),
              label = "Color Definition Mode:",
              choices = c("Multi-ome (Unified)" = "multi_ome",
                          "Per-ome (Individual)" = "per_ome"),
              selected = "multi_ome"
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              condition = "input.color_mode == 'per_ome'",
              ns = ns,
              uiOutput(ns("ome_selector_ui"))
            )
          ),
          column(
            width = 4,
            actionButton(
              ns("reset_all"),
              label = "Reset All Colors",
              icon = icon("undo"),
              class = "btn-warning"
            )
          )
        ),

        hr(),

        # Import/Export section
        fluidRow(
          column(
            width = 6,
            fileInput(
              ns("import_yaml"),
              label = "Import Color Scheme (YAML):",
              accept = c(".yaml", ".yml"),
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            )
          ),
          column(
            width = 6,
            br(),
            downloadButton(
              ns("export_yaml"),
              label = "Export Current Scheme",
              class = "btn-info"
            )
          )
        ),

        hr(),

        # Dynamic color picker UI
        uiOutput(ns("color_pickers_ui"))
      )
    )
  ) # end tagList
}

# server for the customize tab
customizeTabServer <- function(id = "customizeTab", GCTs_and_params, globals) {

  ## module function
  moduleServer(id, function (input, output, session) {

    ## GATHERING INPUTS ##

    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns

    # GCTs of individual omes to use for analysis/visualization
    GCTs <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })

    # Large merged GCT with all omes containing `protigy.ome` column in `rdesc`
    GCTs_merged <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs_merged
    })

    # parameters used to process GCTs
    parameters <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })

    # all omes present
    all_omes <- reactive(names(GCTs()))

    # selected annotation columns per ome
    default_annotations <- reactive({
      req(globals$default_annotations)
      globals$default_annotations
    })


    ## INITIALIZE COLORS ##

    # Initialize custom_colors as reactiveVal
    custom_colors <- reactiveVal()

    # Use existing colors from globals (initialized in sidebar_setup)
    # This ensures consistency and uses the colorblind-safe palette
    observeEvent(globals$colors, {
      req(globals$colors, globals$colors_default)
      custom_colors(globals$colors)
    }, once = TRUE, ignoreNULL = TRUE)


    ## DYNAMIC UI FOR OME SELECTOR ##

    output$ome_selector_ui <- renderUI({
      req(all_omes())
      selectInput(
        ns("selected_ome"),
        label = "Select Ome:",
        choices = all_omes(),
        selected = all_omes()[1]
      )
    })


    ## DYNAMIC UI FOR COLOR PICKERS ##

    output$color_pickers_ui <- renderUI({
      req(custom_colors())
      colors <- custom_colors()

      # Determine which ome to display
      req(input$color_mode)
      display_ome <- if (input$color_mode == "multi_ome") {
        "multi_ome"
      } else {
        req(input$selected_ome)
        input$selected_ome
      }

      if (!(display_ome %in% names(colors))) {
        return(p("No color data available for selected ome."))
      }

      # Get annotation columns for this ome
      all_annot_columns <- names(colors[[display_ome]])

      # Filter to only selected annotation columns
      req(default_annotations())
      if (display_ome == "multi_ome") {
        # For multi-ome, show columns selected in any ome
        selected_annots <- unique(unlist(default_annotations()))
      } else {
        # For individual ome, show only its selected annotation column
        selected_annots <- default_annotations()[[display_ome]]
      }

      annot_columns <- intersect(selected_annots, all_annot_columns)

      # Create UI for each annotation column
      column_boxes <- lapply(annot_columns, function(annot_col) {
        color_info <- colors[[display_ome]][[annot_col]]

        # Only show discrete colors for now
        if (!color_info$is_discrete) {
          return(NULL)
        }

        vals <- color_info$vals
        col_colors <- color_info$colors

        # Create color pickers for each value
        color_pickers <- lapply(seq_along(vals), function(i) {
          picker_id <- paste0("color_", display_ome, "_", annot_col, "_", i)
          column(
            width = 3,
            colourpicker::colourInput(
              ns(picker_id),
              label = as.character(vals[i]),
              value = col_colors[i],
              showColour = "both",
              palette = "square",
              allowedCols = NULL
            )
          )
        })

        # Return box with color pickers and reset button
        box(
          title = annot_col,
          status = "info",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          fluidRow(
            column(
              width = 10,
              fluidRow(color_pickers)
            ),
            column(
              width = 2,
              actionButton(
                ns(paste0("reset_", display_ome, "_", annot_col)),
                label = "Reset",
                icon = icon("undo"),
                class = "btn-sm btn-default"
              )
            )
          )
        )
      })

      # Remove NULL elements and return
      column_boxes <- column_boxes[!sapply(column_boxes, is.null)]
      tagList(column_boxes)
    })


    ## HANDLE COLOR CHANGES ##

    # Observe all color picker changes
    observe({
      req(custom_colors())
      colors <- custom_colors()

      # Determine which ome we're displaying
      req(input$color_mode)
      display_ome <- if (input$color_mode == "multi_ome") {
        "multi_ome"
      } else {
        req(input$selected_ome)
        input$selected_ome
      }

      if (!(display_ome %in% names(colors))) return()

      # Filter to only selected annotation columns
      req(default_annotations())
      if (display_ome == "multi_ome") {
        selected_annots <- unique(unlist(default_annotations()))
      } else {
        selected_annots <- default_annotations()[[display_ome]]
      }
      annot_columns <- intersect(selected_annots, names(colors[[display_ome]]))

      # Check each annotation column
      for (annot_col in annot_columns) {
        color_info <- colors[[display_ome]][[annot_col]]

        if (!color_info$is_discrete) next

        vals <- color_info$vals

        # Check each value
        for (i in seq_along(vals)) {
          picker_id <- paste0("color_", display_ome, "_", annot_col, "_", i)
          new_color <- input[[picker_id]]

          if (!is.null(new_color) && new_color != color_info$colors[i]) {
            # Update colors
            updated_colors <- colors

            if (input$color_mode == "multi_ome") {
              # Sync across all omes
              updated_colors <- sync_colors_across_omes(
                updated_colors,
                annot_col,
                vals[i],
                new_color
              )
            } else {
              # Update only this ome
              updated_colors[[display_ome]][[annot_col]]$colors[i] <- new_color
            }

            custom_colors(updated_colors)
          }
        }
      }
    })


    ## HANDLE RESET BUTTONS ##

    # Reset all colors
    observeEvent(input$reset_all, {
      req(globals$colors_default)
      custom_colors(globals$colors_default)
      shinyalert::shinyalert(
        title = "Colors Reset",
        text = "All colors have been reset to defaults.",
        type = "success"
      )
    })

    # Reset individual annotation columns
    observe({
      req(custom_colors(), globals$colors_default)

      # Determine which ome we're displaying
      req(input$color_mode)
      display_ome <- if (input$color_mode == "multi_ome") {
        "multi_ome"
      } else {
        req(input$selected_ome)
        input$selected_ome
      }

      colors <- custom_colors()
      if (!(display_ome %in% names(colors))) return()

      # Filter to only selected annotation columns
      req(default_annotations())
      if (display_ome == "multi_ome") {
        selected_annots <- unique(unlist(default_annotations()))
      } else {
        selected_annots <- default_annotations()[[display_ome]]
      }
      annot_columns <- intersect(selected_annots, names(colors[[display_ome]]))

      # Check for reset button clicks
      for (annot_col in annot_columns) {
        reset_button_id <- paste0("reset_", display_ome, "_", annot_col)

        observeEvent(input[[reset_button_id]], {
          updated_colors <- reset_colors_to_default(
            custom_colors(),
            globals$colors_default,
            ome = display_ome,
            annot_column = annot_col
          )
          custom_colors(updated_colors)
        }, ignoreInit = TRUE)
      }
    })


    ## HANDLE IMPORT/EXPORT ##

    # Import colors from YAML
    observeEvent(input$import_yaml, {
      req(input$import_yaml, custom_colors())

      file_path <- input$import_yaml$datapath

      tryCatch({
        updated_colors <- import_colors_from_yaml(file_path, custom_colors())
        custom_colors(updated_colors)
        shinyalert::shinyalert(
          title = "Import Successful",
          text = "Color scheme imported successfully!",
          type = "success"
        )
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Import Failed",
          text = paste("Failed to import colors:", e$message),
          type = "error"
        )
      })
    })

    # Export colors to YAML
    output$export_yaml <- downloadHandler(
      filename = function() {
        paste0("color_palette_", Sys.Date(), ".yaml")
      },
      content = function(file) {
        req(custom_colors())
        export_colors_to_yaml(custom_colors(), file)
      }
    )


    ## RETURN ##
    return(custom_colors)

  })
}
