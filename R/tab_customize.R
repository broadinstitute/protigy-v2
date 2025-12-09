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

        # Conditionally render content based on whether data is processed
        uiOutput(ns("customize_content_ui"))
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


    ## CONDITIONAL UI RENDERING ##

    # Render the main content conditionally based on whether data is processed
    output$customize_content_ui <- renderUI({
      # Check if GCTs are processed
      if (is.null(GCTs_and_params())) {
        return(
          tagList(
            div(
              style = "text-align: center; padding: 40px;",
              icon("info-circle", style = "font-size: 48px; color: #3498db; margin-bottom: 20px;"),
              h4("Data Not Yet Processed"),
              p("Please upload and process your GCT files in the Setup sidebar before customizing color schemes.",
                style = "color: #7f8c8d; font-size: 14px; margin-top: 15px;")
            )
          )
        )
      }

      # Data is processed, show the color customization UI
      tagList(
        # Top control panel
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("color_mode"),
              label = "Color Definition Mode:",
              choices = c("Multi-ome (Unified)" = "multi_ome",
                          "Per-ome (Individual)" = "per_ome"),
              selected = "multi_ome"
            )
          ),
          column(
            width = 6,
            conditionalPanel(
              condition = "input.color_mode == 'per_ome'",
              ns = ns,
              uiOutput(ns("ome_selector_ui"))
            )
          )
        ),

        hr(),

        # Annotation column selector
        fluidRow(
          column(
            width = 12,
            uiOutput(ns("annotation_column_selector_ui"))
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
              class = "btn btn-primary"
            ),
            br(), br(),
            actionButton(
              ns("restore_defaults"),
              label = "Restore Default Colors",
              icon = icon("undo"),
              class = "btn btn-default"
            ),
            br(), br(),
            actionButton(
              ns("reset_to_app_defaults"),
              label = "Reset to App Defaults",
              icon = icon("refresh"),
              class = "btn btn-default"
            )
          )
        ),

        hr(),

        # Dynamic color picker UI
        uiOutput(ns("color_pickers_ui"))
      )
    })


    ## INITIALIZE COLORS ##

    # Initialize custom_colors as reactiveVal
    custom_colors <- reactiveVal()

    # Store default colors (either from app generation or from imported YAML)
    default_colors_stored <- reactiveVal(NULL)
    
    # Store original app-generated defaults (never overwritten by imports)
    original_app_defaults <- reactiveVal(NULL)

    # Flag to prevent observe block from interfering during import
    importing <- reactiveVal(FALSE)

    # Use existing colors from globals (initialized in sidebar_setup)
    # This ensures consistency and uses the colorblind-safe palette
    # Note: Removed 'once = TRUE' to allow updates if globals$colors changes
    # but protect against overwriting user customizations during manual edits
    observeEvent(globals$colors, {
      req(globals$colors)

      # Only update if custom_colors is not yet initialized or during import
      # This prevents overwriting user's manual color changes
      if (is.null(custom_colors()) || length(custom_colors()) == 0 || importing()) {
        custom_colors(globals$colors)
        # Store as default if not already set (first time initialization)
        if (is.null(default_colors_stored())) {
          default_colors_stored(globals$colors)
        }
        # Always store original app defaults (never overwrite)
        if (is.null(original_app_defaults())) {
          original_app_defaults(globals$colors)
        }
      }
    }, ignoreNULL = TRUE)


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


    ## DYNAMIC UI FOR ANNOTATION COLUMN SELECTOR ##

    output$annotation_column_selector_ui <- renderUI({
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
        return(NULL)
      }

      # Get all available annotation columns for this ome (only discrete ones)
      all_annot_columns <- names(colors[[display_ome]])
      discrete_annot_columns <- character(0)
      
      for (annot_col in all_annot_columns) {
        if (colors[[display_ome]][[annot_col]]$is_discrete) {
          discrete_annot_columns <- c(discrete_annot_columns, annot_col)
        }
      }

      if (length(discrete_annot_columns) == 0) {
        return(p("No discrete annotation columns available for color customization."))
      }

      # Determine default annotation column (analysis annotation column)
      default_annot <- NULL
      req(default_annotations())
      if (display_ome == "multi_ome") {
        # For multi-ome, use the first available default annotation
        default_annots <- unique(unlist(default_annotations()))
        default_annot <- intersect(default_annots, discrete_annot_columns)[1]
      } else {
        # For individual ome, use its selected annotation column
        default_annot <- default_annotations()[[display_ome]]
        if (!default_annot %in% discrete_annot_columns) {
          default_annot <- discrete_annot_columns[1]
        }
      }

      # If default not found, use first available
      if (is.null(default_annot) || length(default_annot) == 0 || is.na(default_annot)) {
        default_annot <- discrete_annot_columns[1]
      }

      selectInput(
        ns("selected_annotation_column"),
        label = "Annotation Column:",
        choices = discrete_annot_columns,
        selected = default_annot
      )
    })


    ## DYNAMIC UI FOR COLOR PICKERS ##

    output$color_pickers_ui <- renderUI({
      req(custom_colors())
      req(input$selected_annotation_column)
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

      # Get the selected annotation column
      annot_col <- input$selected_annotation_column

      if (!(annot_col %in% names(colors[[display_ome]]))) {
        return(p("Selected annotation column not available."))
      }

      color_info <- colors[[display_ome]][[annot_col]]

      # Only show discrete colors
      if (!color_info$is_discrete) {
        return(p("Selected annotation column does not have discrete values for color customization."))
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

      # Return box with color pickers
      box(
        title = paste("Colors for:", annot_col),
        status = "info",
        width = 12,
        collapsible = FALSE,
        fluidRow(color_pickers)
      )
    })


    ## HANDLE COLOR CHANGES ##

    # Observe all color picker changes
    # NOTE: Skip observation during import to prevent interference
    # We depend on custom_colors() to trigger when UI renders, then check inputs
    observe({
      # Depend on custom_colors() so this runs when UI is rendered/updated
      colors_ui <- custom_colors()
      
      # Skip if currently importing - check this FIRST before any other operations
      if (importing()) {
        return()
      }

      # Get current colors without creating additional reactive dependency
      colors <- isolate(custom_colors())
      req(colors)

      # Determine which ome we're displaying
      req(input$color_mode)
      display_ome <- if (input$color_mode == "multi_ome") {
        "multi_ome"
      } else {
        req(input$selected_ome)
        input$selected_ome
      }

      if (!(display_ome %in% names(colors))) return()

      # Get the selected annotation column
      req(input$selected_annotation_column)
      annot_col <- input$selected_annotation_column

      if (!(annot_col %in% names(colors[[display_ome]]))) return()

      color_info <- colors[[display_ome]][[annot_col]]

      if (!color_info$is_discrete) return()

      vals <- color_info$vals

      # Check each value in the selected annotation column
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
          
          # Show notification that color was updated
          showNotification(
            paste("Color updated for", vals[i], "in", annot_col),
            type = "message",
            duration = 2
          )
        }
      }
    })


    ## HANDLE IMPORT/EXPORT ##

    # Import colors from YAML
    observeEvent(input$import_yaml, {
      req(input$import_yaml)

      # Validate that data has been loaded and colors initialized
      if (is.null(custom_colors()) || length(custom_colors()) == 0) {
        shinyalert::shinyalert(
          title = "Data Not Ready",
          text = "Please upload and process your data files first before importing a color scheme.",
          type = "warning"
        )
        return()
      }

      file_path <- input$import_yaml$datapath

      tryCatch({
        # Set importing flag to prevent observe block interference
        importing(TRUE)

        updated_colors <- import_colors_from_yaml(file_path, custom_colors())

        # Store imported colors as the new defaults
        default_colors_stored(updated_colors)

        # Update the reactive value
        # The color picker UI will automatically update via renderUI when custom_colors() changes
        # We don't need to manually update color pickers, which prevents triggering the observe block
        custom_colors(updated_colors)

        # Reset importing flag AFTER updating custom_colors
        # Use a delay to ensure UI updates complete before allowing observer to run
        # This prevents the observe block from immediately reacting to the color change
        shinyjs::delay(200, {
          importing(FALSE)
        })

        shinyalert::shinyalert(
          title = "Import Successful",
          text = "Color scheme imported successfully! Colors have been updated. These colors are now set as defaults for restore.",
          type = "success"
        )
      }, error = function(e) {
        importing(FALSE)  # Reset flag on error

        # Log error for debugging
        message("Color import error: ", e$message)

        # Provide helpful error message to user
        error_msg <- if (grepl("colors.*not found", e$message, ignore.case = TRUE)) {
          paste("Invalid color scheme file. Please ensure the YAML file contains a 'colors' section.",
                "\nError:", e$message)
        } else if (grepl("yaml|parse", e$message, ignore.case = TRUE)) {
          paste("Failed to parse YAML file. Please check that the file is valid YAML format.",
                "\nError:", e$message)
        } else {
          paste("Failed to import color scheme:", e$message,
                "\nPlease check that the file is a valid Protigy color scheme.")
        }

        shinyalert::shinyalert(
          title = "Import Failed",
          text = error_msg,
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

    # Restore default colors
    observeEvent(input$restore_defaults, {
      # Check if we have stored defaults (from import) or need to regenerate
      if (!is.null(default_colors_stored())) {
        # Use stored defaults (from imported YAML)
        default_colors <- default_colors_stored()
      } else {
        # No stored defaults, regenerate from app's color generation
        req(GCTs(), GCTs_merged())
        default_colors <- make_custom_colors(GCTs(), GCTs_merged())
      }
      
      # Update custom_colors
      custom_colors(default_colors)
      
      # Update color picker inputs for the currently selected annotation column
      req(input$color_mode)
      display_ome <- if (input$color_mode == "multi_ome") {
        "multi_ome"
      } else {
        req(input$selected_ome)
        input$selected_ome
      }
      
      if (display_ome %in% names(default_colors)) {
        if (!is.null(input$selected_annotation_column)) {
          annot_col <- input$selected_annotation_column
          
          if (annot_col %in% names(default_colors[[display_ome]])) {
            color_info <- default_colors[[display_ome]][[annot_col]]
            
            if (color_info$is_discrete) {
              vals <- color_info$vals
              col_colors <- color_info$colors
              
              for (i in seq_along(vals)) {
                picker_id <- paste0("color_", display_ome, "_", annot_col, "_", i)
                colourpicker::updateColourInput(
                  session,
                  picker_id,
                  value = col_colors[i]
                )
              }
            }
          }
        }
      }
      
      # Show notification
      showNotification(
        "Default colors restored successfully!",
        type = "message",
        duration = 3
      )
    })

    # Reset to original app-generated defaults (clears imported YAML defaults)
    observeEvent(input$reset_to_app_defaults, {
      req(GCTs(), GCTs_merged())
      
      # Clear stored defaults (from imported YAML)
      default_colors_stored(NULL)
      
      # Clear the file input
      shinyjs::reset("import_yaml")
      
      # Regenerate original app defaults
      app_default_colors <- make_custom_colors(GCTs(), GCTs_merged())
      
      # Update original app defaults storage
      original_app_defaults(app_default_colors)
      
      # Update custom_colors
      custom_colors(app_default_colors)
      
      # Update color picker inputs for the currently selected annotation column
      req(input$color_mode)
      display_ome <- if (input$color_mode == "multi_ome") {
        "multi_ome"
      } else {
        req(input$selected_ome)
        input$selected_ome
      }
      
      if (display_ome %in% names(app_default_colors)) {
        if (!is.null(input$selected_annotation_column)) {
          annot_col <- input$selected_annotation_column
          
          if (annot_col %in% names(app_default_colors[[display_ome]])) {
            color_info <- app_default_colors[[display_ome]][[annot_col]]
            
            if (color_info$is_discrete) {
              vals <- color_info$vals
              col_colors <- color_info$colors
              
              for (i in seq_along(vals)) {
                picker_id <- paste0("color_", display_ome, "_", annot_col, "_", i)
                colourpicker::updateColourInput(
                  session,
                  picker_id,
                  value = col_colors[i]
                )
              }
            }
          }
        }
      }
      
      # Show notification
      showNotification(
        "Reset to original app-generated default colors. Imported YAML defaults have been cleared.",
        type = "message",
        duration = 4
      )
    })


    ## RETURN ##
    return(custom_colors)

  })
}
