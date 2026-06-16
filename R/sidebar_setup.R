################################################################################
# Module: SETUP SIDEBAR
# Main shiny functions (server and UI)
################################################################################

# Whether current setup choices allow copying settings to every dataset.
# Returns list(ok = TRUE) or list(ok = FALSE, msg = <user-facing string>).
gct_setup_apply_to_all_valid <- function(
    annotation_column,
    group_normalization,
    group_normalization_column,
    sample_filter_enabled,
    sample_filter_column,
    row_filter_enabled,
    row_filter_column,
    gene_symbol_column,
    convert_ids_to_gene_symbol,
    id_source_column,
    groups_in_all,
    rdesc_in_all) {
  if (is.null(annotation_column)) {
    annotation_column <- NA_character_
  }
  if (is.null(group_normalization_column)) {
    group_normalization_column <- NA_character_
  }
  if (is.null(sample_filter_column)) {
    sample_filter_column <- ""
  }
  if (is.null(row_filter_column)) {
    row_filter_column <- ""
  }
  if (is.null(gene_symbol_column)) {
    gene_symbol_column <- "None"
  }
  if (is.null(id_source_column)) {
    id_source_column <- ""
  }
  if (length(groups_in_all) == 0L) {
    return(list(
      ok = FALSE,
      msg = paste0(
        "'Apply settings to all datasets' was disabled. ",
        "Choose consistent shared annotation/filter columns across datasets to enable this setting."
      )
    ))
  }
  if (!(annotation_column %in% groups_in_all)) {
    return(list(
      ok = FALSE,
      msg = paste0(
        "'Apply settings to all datasets' was disabled. ",
        "Choose a consistent analysis annotation column that exists in all datasets to enable this setting."
      )
    ))
  }
  if (isTRUE(group_normalization) && !(group_normalization_column %in% groups_in_all)) {
    return(list(
      ok = FALSE,
      msg = paste0(
        "'Apply settings to all datasets' was disabled. ",
        "Choose a consistent group normalization column that exists in all datasets to enable this setting."
      )
    ))
  }
  if (isTRUE(sample_filter_enabled) && nzchar(sample_filter_column) &&
        !(sample_filter_column %in% groups_in_all)) {
    return(list(
      ok = FALSE,
      msg = paste0(
        "'Apply settings to all datasets' was disabled. ",
        "Sample filter column '", sample_filter_column,
        "' is not available in all datasets. Choose a consistent column to enable this setting."
      )
    ))
  }
  if (isTRUE(row_filter_enabled) && nzchar(row_filter_column)) {
    if (length(rdesc_in_all) == 0L || !(row_filter_column %in% rdesc_in_all)) {
      return(list(
        ok = FALSE,
        msg = if (length(rdesc_in_all) == 0L) {
          paste0(
            "'Apply settings to all datasets' was disabled. ",
            "Choose consistent shared row metadata columns across datasets to enable this setting."
          )
        } else {
          paste0(
            "'Apply settings to all datasets' was disabled. ",
            "Row filter column '", row_filter_column,
            "' is not available in all datasets. Choose a consistent column to enable this setting."
          )
        }
      ))
    }
  }
  if (isTRUE(convert_ids_to_gene_symbol)) {
    if (!identical(gene_symbol_column, "None")) {
      return(list(
        ok = FALSE,
        msg = paste0(
          "'Apply settings to all datasets' was disabled. ",
          "ID-to-gene-symbol conversion requires Gene symbol column to be set to None when apply-to-all is used."
        )
      ))
    }
    if (!nzchar(id_source_column)) {
      return(list(
        ok = FALSE,
        msg = paste0(
          "'Apply settings to all datasets' was disabled. ",
          "Choose an ID source column for mapping to gene symbols to enable this setting."
        )
      ))
    }
    if (length(rdesc_in_all) == 0L || !(id_source_column %in% rdesc_in_all)) {
      return(list(
        ok = FALSE,
        msg = if (length(rdesc_in_all) == 0L) {
          paste0(
            "'Apply settings to all datasets' was disabled. ",
            "Choose consistent shared row metadata columns across datasets to enable this setting."
          )
        } else {
          paste0(
            "'Apply settings to all datasets' was disabled. ",
            "ID source column '", id_source_column,
            "' is not available in all datasets. Choose a consistent column to enable this setting."
          )
        }
      ))
    }
  }
  list(ok = TRUE, msg = NA_character_)
}

# Collision-free, filename-stable element id for a file's remove button.
# A plain gsub-to-underscore (e.g. gsub("[^a-zA-Z0-9_]", "_", name)) is NOT
# injective: "a-b.gct" and "a_b.gct" both collapse to one id, which (a) emits
# duplicate HTML ids and (b) makes the register-once dedup in the server skip the
# second file's handler, leaving its remove button non-functional. Hex-encoding
# the filename bytes is injective (distinct names -> distinct ids) AND stable
# (the same name always yields the same id, which the monotonic dedup relies on).
# Index-based ids cannot be used here: the persistent observer captures the
# filename at first registration, so a positional id would target the wrong file
# after a removal reorders the list.
gct_remove_btn_id <- function(filename) {
  paste0("remove_file_",
         paste(sprintf("%02x", utf8ToInt(enc2utf8(filename))), collapse = ""))
}

# UI for the sidebar setup
setupSidebarUI <- function(id = "setupSidebar") {
  # namespace function, wrap inputId's and outputId's with this (e.g. `ns(id)`)
  ns <- NS(id) 
  
  # tagList(
  #   # file input
  #   fileInput(ns("dataFiles"), 
  #             paste("Choose data file(s). Supported formats: GCT, CSV, Excel. All files should include the same samples."),
  #             multiple = TRUE,
  #             accept = c(".gct", ".csv", ".xlsx", ".xls")),
  #   hr(),
  #   
  #   # the main body of the sidebar, contents assigned in setupSidebarServer
  #   uiOutput(ns('sideBarMain')),
  #   
  #   # navigation buttons on the bottom left/right of sidebar
  #   fluidRow(
  #     column(6, uiOutput(ns('leftButton'))),
  #     column(6, uiOutput(ns('rightButton')))
  #   )
  # ) # end tagList
  
  # Fine-tuning formatting of initial file upload
  tags$div(
    style = "margin-bottom: 10px; padding: 15px; width: 100%", 
    tagList(
      # File upload section - conditionally shown
      uiOutput(ns("fileUploadSection")),

      hr(),
      
      # the main body of the sidebar, contents assigned in setupSidebarServer
      uiOutput(ns('sideBarMain')),
      
      # navigation buttons on the bottom left/right of sidebar
      fluidRow(
        column(6, uiOutput(ns('leftButton'))),
        column(6, uiOutput(ns('rightButton')))
      )
    )
  )
}

# server for the sidebar setup
setupSidebarServer <- function(id = "setupSidebar", parent) { moduleServer( 
  id,
  
  ## module function
  function (input, output, session) {
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    ### INITIALIZATION ###
    
    # initialize main outputs from this module
    GCTs_and_params <- reactiveVal() # GCT object and corresponding parameters
    globals <- reactiveValues() # global values for plots, displays, etc.
    GCTs_original <- reactiveVal() # the original GCTS (not processed)

    # Export internal reactive state for shinytest2 integration tests.
    # These are no-ops in production (shiny.testmode is FALSE by default).
    shiny::exportTestValues(
      sidebar_setup_complete   = { !is.null(GCTs_and_params()) },
      sidebar_labels_validated = { labelsGO() > 0 }
    )

    # initialize INTERNAL reactive values....only used in this module
    parameters_internal_reactive <- reactiveVal()
    GCTs_unprocessed_internal_reactive <- reactiveVal()
    accumulated_files <- reactiveVal(NULL)  # Store accumulated file uploads
    # Track remove-button ids that already have a live observeEvent so each id is
    # registered exactly once per session (prevents observer accumulation /
    # multi-fire on every accumulated_files() invalidation). Monotonic by design:
    # never reset, so re-adding a previously seen filename does not double-register.
    registered_remove_btns <- reactiveVal(character(0))

    # INT-2: memoized per-ome discrete-column map for the setup panel's dropdowns.
    # Depends ONLY on the GCTs reactiveVal, so it recomputes exactly when the GCTs
    # change (any upload / removal / reprocess via either upload path, since both
    # write GCTs_unprocessed_internal_reactive). gctSetupUI consumes this instead
    # of re-scanning is.discrete() over every annotation column on every rebuild
    # (e.g. on every Intensity-data toggle). Recompute-on-change preserves the
    # original "always fresh" guarantee; it only skips redundant re-scans.
    discrete_columns_map <- reactive({
      build_discrete_columns_map(GCTs_unprocessed_internal_reactive())
    })

    # initialize reactiveValues with back/next logic for when user navigates
    # through each GCT file to input parameters
    backNextLogic <- reactiveValues(placeChanged = 0)
    
    # initialize reactiveVal to indicate when labels & gcts are validated + submitted
    labelsGO <- reactiveVal(0)
    gctsGO <- reactiveVal(0)
    csvExcel_identifier_columns_reactive <- reactiveVal(NULL)
    sample_filter_input_state <- reactiveValues()
    row_filter_input_state <- reactiveValues()
    # Labels that just received default_parameters; after first parse, gene_symbol_column
    # is set from rdesc (geneSymbol if present, else None) once — never overwrites user edits.
    gene_symbol_defaults_pending_labels <- reactiveVal(character(0))
    
    # read in default settings and choices from yamls
    default_parameters <- read_yaml(system.file('setup_parameters/setupDefaults.yaml', package = 'Protigy'))
    parameter_choices <- read_yaml(system.file('setup_parameters/setupChoices.yaml', package = 'Protigy'))


    ### FILE UPLOAD SECTION UI ###
    
    # Conditionally show/hide file upload section based on setup completion
    output$fileUploadSection <- renderUI({
      # Hide file upload UI after setup is complete
      if (!is.null(GCTs_and_params())) {
        return(NULL)
      }
      
      tagList(
        h4("Upload your data file(s)"), 
        
        # File input
        fileInput(ns("dataFiles"),
                  paste("GCT, CSV, TSV, SSV, or Excel"),
                  multiple = TRUE,
                  accept = c(".gct", ".csv", ".xlsx", ".xls", ".tsv", ".ssv")),
        
        # Display uploaded files list with remove buttons
        uiOutput(ns("uploadedFilesList"))
      )
    })

    ### UPLOADED FILES LIST UI ###

    # Display list of uploaded files with remove buttons
    output$uploadedFilesList <- renderUI({
      # Hide file upload UI after setup is complete
      if (!is.null(GCTs_and_params())) {
        return(NULL)
      }
      
      # Use validate instead of req for better error handling
      validate(need(accumulated_files(), ""))

      files <- accumulated_files()

      # Additional safety check
      if (is.null(files) || nrow(files) == 0) {
        return(NULL)
      }

      tagList(
        h5(paste0("Uploaded Files (", nrow(files), ")")),
        div(
          style = "max-height: 200px; overflow-y: auto; margin-bottom: 10px; width: 100%;",
          lapply(1:nrow(files), function(i) {
            # Collision-free, filename-stable id (see gct_remove_btn_id).
            btn_id <- gct_remove_btn_id(files$name[i])
            div(
              style = "padding: 8px; margin: 3px 0; background-color: #f8f9fa; border-radius: 3px; display: flex; align-items: flex-start; justify-content: space-between; width: 100%; box-sizing: border-box; min-height: 35px; height: auto;",
              div(
                style = "flex: 1 1 auto; min-width: 0; padding-right: 10px; color: #333; font-size: 13px; word-wrap: break-word; overflow-wrap: anywhere; word-break: break-word; white-space: normal; line-height: 1.4;",
                files$name[i]
              ),
              actionButton(
                ns(btn_id),
                label = NULL,
                icon = icon("times"),
                class = "btn-sm btn-primary",
                style = "padding: 2px 8px; font-size: 12px; flex-shrink: 0;",
                `data-filename` = files$name[i]  # Store filename as data attribute
              )
            )
          })
        ),
        actionButton(
          ns("clearAllFiles"),
          "Clear All",
          class = "btn-sm btn-primary",
          icon = icon("trash")
        )
      )
    })


    ### STEP 1: LABEL ASSIGNMENT ###
    
    # once files uploaded, accumulate and display label assignment
    observeEvent(
      eventExpr = input$dataFiles,
      ignoreInit = TRUE,
      handlerExpr = {
        # Get newly uploaded files
        new_files <- input$dataFiles

        # Validate file format consistency
        new_extensions <- tools::file_ext(tolower(new_files$name))

        # Check if adding to existing files
        if (!is.null(accumulated_files())) {
          existing_extensions <- tools::file_ext(tolower(accumulated_files()$name))
          all_extensions <- c(existing_extensions, new_extensions)

          # Validate all files are same type
          if (!(all(all_extensions == "gct") || all(all_extensions %in% c("csv", "xlsx", "xls", "tsv", "ssv")))) {
            showNotification(
              ui = HTML(paste0("<b>File Type Mismatch</b><br>", "All uploaded files must be the same type (all GCT or all CSV/Excel/TSV/SSV). Please remove existing files before uploading a different file type.")),
              type = "error",
              duration = NULL,
              closeButton = TRUE
            )
            return()
          }

          # Check for duplicate filenames
          if (any(new_files$name %in% accumulated_files()$name)) {
            showNotification(
              ui = HTML(paste0("<b>Duplicate Files</b><br>", "Some files have already been uploaded. Duplicate files will be skipped.")),
              type = "warning",
              duration = NULL,
              closeButton = TRUE
            )
            # Filter out duplicates
            new_files <- new_files[!new_files$name %in% accumulated_files()$name, ]
            if (nrow(new_files) == 0) return()
          }

          # Accumulate files
          accumulated_files(rbind(accumulated_files(), new_files))
        } else {
          # First upload - validate file types
          if (!(all(new_extensions == "gct") || all(new_extensions %in% c("csv", "xlsx", "xls", "tsv", "ssv")))) {
            showNotification(
              ui = HTML(paste0("<b>Error</b><br>", "Please upload files of the same type only (GCT, CSV/Excel/TSV/SSV). Mixed file types are not supported.")),
              type = "error",
              duration = NULL,
              closeButton = TRUE
            )
            return()
          }

          # First upload
          accumulated_files(new_files)

          # Reset internal state for new session
          parameters_internal_reactive(NULL)
          GCTs_unprocessed_internal_reactive(NULL)
        }

        # Trigger workflow based on file type (using accumulated files)
        file_extensions <- tools::file_ext(tolower(accumulated_files()$name))

        if (all(file_extensions == "gct")) {
          # All GCT files - use existing workflow
          labelAssignment()
        } else if (all(file_extensions %in% c("csv", "xlsx", "xls", "tsv", "ssv"))) {
          # All CSV/Excel/TSV/SSV files - use same workflow
          csvExcelWorkflow()

          # Automatically switch to CSV/TSV/SSV/Excel Processing help tab
          updateTabsetPanel(session = parent,
                           inputId = "navbar-tabs",
                           selected = "Help-Analysis")

          # Switch to the CSV/TSV/SSV/Excel Processing tab within the help section
          shinyjs::runjs("
            setTimeout(function() {
              $('a[data-value=\"CSV/TSV/SSV/Excel Processing\"]').click();
            }, 100);
          ")
        }
      })


    ### FILE REMOVAL HANDLERS ###

    # Handle individual file removal - Use filename as unique identifier to prevent index issues
    observe({
      # Hide file removal handlers after setup is complete
      if (!is.null(GCTs_and_params())) {
        return(NULL)
      }
      
      # Use validate for cleaner NULL handling
      validate(need(accumulated_files(), ""))

      files <- accumulated_files()

      # Additional safety check
      if (is.null(files) || nrow(files) == 0) {
        return(NULL)
      }

      # Create observers for each remove button using filename as unique identifier.
      # Only register button ids that do not already have a live observeEvent, so the
      # handler set does not grow on every accumulated_files() invalidation. Existing
      # handlers look the file up BY NAME at click time and no-op if it is gone, so
      # keeping them alive across add/remove/clear cycles is safe.
      # isolate() so reading/writing the tracker does not make this observe depend
      # on itself (the observe must only re-run on accumulated_files() changes).
      already_registered <- isolate(registered_remove_btns())
      newly_registered <- character(0)
      lapply(1:nrow(files), function(i) {
        # Collision-free, filename-stable id (see gct_remove_btn_id). Distinct
        # filenames never share an id, so the register-once dedup below cannot
        # drop a second file's handler.
        btn_id <- gct_remove_btn_id(files$name[i])
        filename <- files$name[i]  # Capture filename at observer creation time

        # Skip ids that already have a live handler (also de-dupes within this batch).
        if (btn_id %in% already_registered || btn_id %in% newly_registered) {
          return(NULL)
        }
        newly_registered <<- c(newly_registered, btn_id)

        observeEvent(input[[btn_id]], {
          # Wrap in tryCatch to handle any reactive errors
          tryCatch({
            # Isolate to get current file list at button click time
            current_files <- isolate(accumulated_files())

            # Safety check
            if (is.null(current_files)) {
              return(NULL)
            }

            # Find the file by name (not index) to handle cases where files were removed
            file_idx <- which(current_files$name == filename)
            
            if (length(file_idx) == 0) {
              # File already removed, do nothing
              return(NULL)
            }

            removed_name <- current_files$name[file_idx[1]]

            # Remove the file by name
            remaining_files <- current_files[current_files$name != filename, , drop = FALSE]

            if (nrow(remaining_files) == 0) {
              # Show notification FIRST (before state changes)
              showNotification(
                paste("Removed:", removed_name),
                type = "message",
                duration = 3
              )

              # Small delay to ensure notification is displayed
              Sys.sleep(0.1)

              # No files left - reset everything
              accumulated_files(NULL)
              parameters_internal_reactive(NULL)
              GCTs_unprocessed_internal_reactive(NULL)
              output$sideBarMain <- renderUI({ NULL })
              output$rightButton <- renderUI({ NULL })
              output$leftButton <- renderUI({ NULL })
            } else {
              # Update accumulated files first
              accumulated_files(remaining_files)

              # IMPORTANT: Reset parameters to force re-initialization
              parameters_internal_reactive(NULL)
              GCTs_unprocessed_internal_reactive(NULL)

              # Refresh the workflow with remaining files
              file_extensions <- tools::file_ext(tolower(remaining_files$name))
              if (all(file_extensions == "gct")) {
                labelAssignment()
              } else {
                csvExcelWorkflow()
              }

              # Show notification after state is updated
              showNotification(
                paste("Removed:", removed_name),
                type = "message",
                duration = 3
              )
            }
          }, error = function(e) {
            # Silently handle any reactive errors
            message("Error removing file: ", e$message)
          })
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })

      # Record the ids we just registered so they are not registered again on the
      # next invalidation. Tracker is never cleared, guaranteeing one handler per id.
      if (length(newly_registered) > 0L) {
        isolate(registered_remove_btns(c(already_registered, newly_registered)))
      }
    })

    # Handle clear all files
    observeEvent(input$clearAllFiles, {
      # Only allow clearing files if setup is not complete
      if (!is.null(GCTs_and_params())) {
        showNotification(
          "Cannot clear files after setup is complete. Please go back to setup to modify files.",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      accumulated_files(NULL)
      parameters_internal_reactive(NULL)
      GCTs_unprocessed_internal_reactive(NULL)
      output$sideBarMain <- renderUI({ NULL })
      output$rightButton <- renderUI({ NULL })
      output$leftButton <- renderUI({ NULL })

      showNotification(
        "All files cleared",
        type = "message",
        duration = 3
      )
    })


    # also display label assignment if user navigates back to it
    observeEvent(
      eventExpr = input$backToLabelsButton,
      ignoreInit = TRUE,
      handlerExpr = {
        # Save current setup widgets before leaving (Back from dataset 1 only runs here;
        # Next/Back between datasets already run collectInputs).
        # Do not run after analysis (advanced settings): inputs are not mounted and would
        # NULL-out parameters.
        if (is.null(GCTs_and_params())) {
          collectInputs()
        }
        # Reset GCTs_and_params to allow file upload/removal again
        GCTs_and_params(NULL)
        labelAssignment()
      })
    
    # validate labels once submitted
    observeEvent(input$submitLabelsButton, {
      out <- my_shinyalert_tryCatch({
        all_labels <- sapply(accumulated_files()$name,
                             function(n) input[[paste0('Label_', n)]])
        validate_labels(all_labels)
      }, return.error = FALSE)

      # increment labelsGO if labels are valid
      if (out) labelsGO(labelsGO() + 1)
    })
    
    # move the current tab to the analysis help tab
    observeEvent(labelsGO(), {
      updateTabsetPanel(session = parent, 
                        inputId = "navbar-tabs", 
                        selected = "Help-Analysis")
      
      # Switch to the Dataset Setup tab within the help section
      shinyjs::runjs("
        setTimeout(function() {
          $('a[data-value=\"Dataset Setup\"]').click();
        }, 100);
      ")
    }, ignoreInit = TRUE)
    
    
    ### STEP 2: INPUT GCT PARAMETERS ###
    
    # once labels assignment submitted, set values for back/next logic
    observeEvent(labelsGO(), {
      # current place in the next/back logic
      backNextLogic$place <- 1

      # maximum place (i.e. the total number of data files)
      backNextLogic$maxPlace <- length(accumulated_files()$name)
    }, ignoreInit = TRUE)

    # update GCT parameters with gct file paths and labels once labels are submitted
    observeEvent(labelsGO(), {
      # Rebuild parameters for GCT uploads (supports adding files mid-session).
      # Keep existing parameters only for CSV/Excel workflow, where converted
      # parameters are already populated before labelsGO() increments.
      existing_params <- parameters_internal_reactive()
      file_extensions <- tools::file_ext(tolower(accumulated_files()$name))
      is_gct_workflow <- all(file_extensions == "gct")

      if (!is_gct_workflow && !is.null(existing_params) && length(existing_params) > 0) {
        # CSV/Excel case: parameters already have labels and structure, no need to rebuild
        message("Using existing CSV/Excel parameters with labels: ", paste(names(existing_params), collapse = ", "))
        gene_symbol_defaults_pending_labels(character(0))
      } else {
        # GCT case: build parameters from file uploads and user-provided labels
        new_parameters <- list()
        defaults_pending <- character(0)
        apply(accumulated_files(), 1, function(file) {
          file <- as.list(file)
          
          # get the label using the same inputId notation as in labelSetupUI()
          label <- input[[paste0('Label_', file$name)]] 
          
          # figure out which files were already parsed and have saved parameters
          already_parsed_files <- sapply(parameters_internal_reactive(), 
                                         function(l) l$gct_file_path)
          
          # use the old parameters if they exist
          if (file$datapath %in% already_parsed_files) {
            idx <- which(already_parsed_files == file$datapath)
            new_parameters[[label]] <<- parameters_internal_reactive()[[idx]]
            
            # otherwise use the defaults
          } else {
            new_parameters[[label]] <<- c(gct_file_path = file$datapath,
                                          gct_file_name = file$name,
                                          default_parameters)
            defaults_pending <<- c(defaults_pending, label)
          }
        })
        parameters_internal_reactive(new_parameters) # update GCT parameters reactiveVal
        gene_symbol_defaults_pending_labels(defaults_pending)
      }
    }, ignoreInit = TRUE)
    
    # parse the GCTs for setup
    observeEvent(labelsGO(), {
      parameters <- parameters_internal_reactive()
      existing_gcts <- GCTs_unprocessed_internal_reactive()
      file_extensions <- tools::file_ext(tolower(accumulated_files()$name))
      is_gct_workflow <- all(file_extensions == "gct")
      
      # Check if GCTs are already parsed/converted (CSV/Excel case) or need parsing (GCT case)
      if (!is_gct_workflow && !is.null(existing_gcts) && length(existing_gcts) > 0) {
        # CSV/Excel case: GCTs already converted and stored, just trigger UI update
        message("Using existing CSV/Excel converted GCTs: ", paste(names(existing_gcts), collapse = ", "))
        backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
      } else {
        # GCT case: need to parse GCT files from disk
        parsed_file_paths <- sapply(GCTs_unprocessed_internal_reactive(), function(gct) gct@src)
        GCTs <- my_shinyalert_tryCatch({
          withProgress(
            min = 0, 
            max = length(parameters),
            message = "Parsing GCTs...", 
            expr = {
              lapply(parameters, function(p) {
                # check if the GCT has already been parsed
                if (p$gct_file_path %in% parsed_file_paths) {
                  parsed_label = names(which(parsed_file_paths == p$gct_file_path))
                  stopifnot(length(parsed_label) == 1)
                  incProgress(amount = 1)
                  return(GCTs_unprocessed_internal_reactive()[[parsed_label]])
                  
                  # otherwise, parse the GCT
                } else {
                  gct <- parse_gctx_preserve_cdesc(p$gct_file_path)
                  incProgress(amount = 1)
                  return(gct)
                }
              })
            })
        }, return.error = NULL)
        
        if (!is.null(GCTs)) {
          # update reactiveVal
          GCTs_unprocessed_internal_reactive(GCTs)
          
          pending <- gene_symbol_defaults_pending_labels()
          if (length(pending) > 0) {
            pm <- parameters_internal_reactive()
            for (nm in names(GCTs)) {
              if (nm %in% pending) {
                rdesc_n <- names(GCTs[[nm]]@rdesc)
                pm[[nm]]$gene_symbol_column <- if ("geneSymbol" %in% rdesc_n) "geneSymbol" else "None"
              }
            }
            parameters_internal_reactive(pm)
            gene_symbol_defaults_pending_labels(setdiff(pending, names(GCTs)))
          }
          
          # indicates if place or something about GCT files changed
          backNextLogic$placeChanged <- backNextLogic$placeChanged + 1 
        }
      }
    }, ignoreInit = TRUE, priority = -1)
    
    # display the correct GCT processing page, handling back/next logic
    observeEvent(
      eventExpr = backNextLogic$placeChanged, 
      ignoreInit = TRUE,
      handlerExpr = {
        # get the correct label for this file
        label <- names(parameters_internal_reactive())[backNextLogic$place]
        params_now <- parameters_internal_reactive()
        gcts_now <- GCTs_unprocessed_internal_reactive()
        if (is.null(gcts_now)) {
          gcts_now <- list()
        }

        # Defensive alignment: if a GCT is missing for the current label, attempt
        # to parse it from the stored file path so setup navigation cannot skip/crash.
        if (!is.null(label) && !(label %in% names(gcts_now)) && !is.null(params_now[[label]]$gct_file_path)) {
          reparsed <- my_shinyalert_tryCatch(
            text.error = "<b>Dataset Setup Error:</b>",
            append.error = TRUE,
            show.error = TRUE,
            return.error = NULL,
            expr = {
              parse_gctx_preserve_cdesc(params_now[[label]]$gct_file_path)
            }
          )

          if (!is.null(reparsed)) {
            gcts_now[[label]] <- reparsed
            GCTs_unprocessed_internal_reactive(gcts_now)
          }
        }

        # If still missing, notify and stop rendering this step to avoid NULL-slot errors.
        if (is.null(label) || is.null(GCTs_unprocessed_internal_reactive()[[label]])) {
          showNotification(
            "A dataset could not be loaded for setup. Please re-upload files and try again.",
            type = "error",
            duration = 8
          )
          return(NULL)
        }
        
        # main GCT processing UI
        output$sideBarMain <- renderUI({gctSetupUI(ns = ns,
                                                   label = label,
                                                   parameter_choices = parameter_choices,
                                                   parameters = parameters_internal_reactive(),
                                                   current_place = backNextLogic$place,
                                                   max_place = backNextLogic$maxPlace,
                                                   GCTs = GCTs_unprocessed_internal_reactive(),
                                                   discrete_columns = discrete_columns_map())})
        
        # left button (back to labels or just back)
        if (backNextLogic$place == 1) {
          output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), 
                                                      "Back",
                                                      icon = icon("chevron-left"))})
        } else {
          output$leftButton <- renderUI({actionButton(ns("backButton"), 
                                                      "Back",
                                                      icon = icon("chevron-left"))})
        }
        
        # right button (next or submit gct for processing)
        if (backNextLogic$place == backNextLogic$maxPlace) {
          output$rightButton <- renderUI({
            actionButton(ns("submitGCTButton"),
                         "Submit",
                         class = "btn btn-primary")})
        } else {
          output$rightButton <- renderUI({
            actionButton(ns("nextButton"), "Next >", class = "btn btn-primary")
          })
        }
      })
    
    # update parameter choices when intensity data is toggled
    current_intensity <- reactive({
      label <- names(parameters_internal_reactive())[isolate(backNextLogic$place)]
      checkbox_value <- input[[paste0(label, '_intensity_data')]]
      # Convert checkbox boolean to "Yes"/"No" string
      if (is.logical(checkbox_value)) {
        if (checkbox_value) "Yes" else "No"
      } else {
        checkbox_value  # Fallback for old string values
      }
    })
    observeEvent(current_intensity(), {
      # INT-1: do NOT call collectInputs() here. It writes parameters_internal_reactive,
      # and output$sideBarMain (a renderUI) reads that reactiveVal, so the write forced a
      # full setup-panel rebuild (the visible grey-out) on every intensity toggle.
      # The call was redundant for persistence: every exit path (Next/Back/Submit/
      # back-to-labels) re-runs collectInputs() from live widget state before acting, so
      # no in-progress edit is lost, and the toggled intensity_data is re-collected then.
      # This handler updates only data_normalization and max_missing. It reads their
      # LIVE widget values (with a stored fallback for the pre-paint NULL window) — NOT
      # the stored reactiveVal — because the stored value is only refreshed by
      # collectInputs() at navigation, so a user who edits the dropdown and then toggles
      # would otherwise have their edit reset to the stale stored value. Reading live
      # preserves the user's in-progress selection. The intensity-dependent choice list
      # is derived from current_intensity() (the live checkbox).

      # gather current label and parameters
      label = names(parameters_internal_reactive())[backNextLogic$place]
      parameters = parameters_internal_reactive()[[label]]

      # INT-1: read the LIVE widget values for the two fields this handler updates,
      # falling back to the STORED value only when the widget hasn't reported yet
      # (first paint / pre-flush, where live input is NULL). The stored reactiveVal
      # is NOT kept in sync with the dropdown/numeric on every keystroke — only
      # collectInputs() (run at Next/Back/Submit) writes it — so reading the stored
      # value here would reset an in-progress edit: e.g. user picks "Quantile", then
      # toggles intensity, and the stored (pre-edit) value would overwrite "Quantile".
      # Reading live preserves the user's selection through a toggle. These reads are
      # read-only dependencies and do NOT cause a panel rebuild.
      live_norm <- input[[paste0(label, '_data_normalization')]]
      if (is.null(live_norm) || !nzchar(live_norm)) {
        live_norm <- parameters$data_normalization
      }
      live_max_missing <- input[[paste0(label, '_max_missing')]]
      if (is.null(live_max_missing) || is.na(suppressWarnings(as.numeric(live_max_missing)[1]))) {
        live_max_missing <- parameters$max_missing
      }

      # indicator for intensity data (check out the yaml format)
      ind = paste0("intensity_data_", tolower(current_intensity()))

      # update data normalization
      # Filter out 2-component normalization if dataset has more than 20 samples (too slow)
      norm_choices <- parameter_choices$data_normalization[[ind]]
      gct <- GCTs_unprocessed_internal_reactive()[[label]]
      n_samples <- if (!is.null(gct)) ncol(gct@mat) else 0
      if (n_samples > 20) {
        norm_choices <- norm_choices[norm_choices != "2-component"]
      }
      # Keep the user's current selection if it is still valid in the new intensity
      # branch; otherwise fall back to the default. (If current selection is
      # 2-component but it should be disabled, use default.)
      norm_selected <- ifelse(
        live_norm %in% norm_choices,
        live_norm,
        default_parameters$data_normalization)
      if (n_samples > 20 && norm_selected == "2-component") {
        norm_selected <- default_parameters$data_normalization
      }
      updateSelectInput(
        inputId = paste0(label, '_data_normalization'),
        choices = norm_choices,
        selected = norm_selected)

      # update max missing
      updateNumericInput(
        inputId = paste0(label, '_max_missing'),
        min = parameter_choices$max_missing[[ind]]$min,
        max = parameter_choices$max_missing[[ind]]$max,
        step = parameter_choices$max_missing[[ind]]$step,
        value = min(as.numeric(live_max_missing), parameter_choices$max_missing[[ind]]$max))
    })

    # update sample filter values choices when sample filter column changes
    observe({
      req(parameters_internal_reactive(), GCTs_unprocessed_internal_reactive())
      current_label <- names(parameters_internal_reactive())[backNextLogic$place]
      req(current_label)
      selected_column <- input[[paste0(current_label, "_sample_filter_column")]]
      req(selected_column, selected_column != "")
      gct <- GCTs_unprocessed_internal_reactive()[[current_label]]
      req(gct, selected_column %in% names(gct@cdesc))

      choices <- sort(unique(as.character(gct@cdesc[[selected_column]])))
      choices <- choices[!is.na(choices)]
      selected_values <- isolate(input[[paste0(current_label, "_sample_filter_values")]])
      if (is.null(selected_values)) {
        selected_values <- character(0)
      }
      selected_values <- intersect(selected_values, choices)

      # Avoid re-sending the same input payload repeatedly (prevents UI flicker).
      next_state <- list(
        column = selected_column,
        choices = choices,
        selected = selected_values
      )
      current_state <- isolate(sample_filter_input_state[[current_label]])
      if (identical(current_state, next_state)) {
        return(invisible(NULL))
      }
      sample_filter_input_state[[current_label]] <- next_state

      updateSelectizeInput(
        inputId = paste0(current_label, "_sample_filter_values"),
        choices = choices,
        selected = selected_values,
        server = FALSE
      )
    })

    # update row filter values choices when row filter column changes
    observe({
      req(parameters_internal_reactive(), GCTs_unprocessed_internal_reactive())
      current_label <- names(parameters_internal_reactive())[backNextLogic$place]
      req(current_label)
      selected_column <- input[[paste0(current_label, "_row_filter_column")]]
      req(selected_column, selected_column != "")
      gct <- GCTs_unprocessed_internal_reactive()[[current_label]]
      req(gct, selected_column %in% names(gct@rdesc))

      choices <- sort(unique(as.character(gct@rdesc[[selected_column]])))
      choices <- choices[!is.na(choices)]
      selected_values <- isolate(input[[paste0(current_label, "_row_filter_values")]])
      if (is.null(selected_values)) {
        selected_values <- character(0)
      }
      selected_values <- intersect(selected_values, choices)

      # Avoid re-sending the same input payload repeatedly (prevents UI flicker).
      next_state <- list(
        column = selected_column,
        choices = choices,
        selected = selected_values
      )
      current_state <- isolate(row_filter_input_state[[current_label]])
      if (identical(current_state, next_state)) {
        return(invisible(NULL))
      }
      row_filter_input_state[[current_label]] <- next_state

      updateSelectizeInput(
        inputId = paste0(current_label, "_row_filter_values"),
        choices = choices,
        selected = selected_values,
        server = FALSE
      )
    })
    
    
    # reset applyToAll to FALSE if it is not a valid option
    groups_in_all_omes <- reactive({
      base::Reduce(base::intersect, lapply(GCTs_unprocessed_internal_reactive(), function(gct) names(gct@cdesc)))
    })
    rdesc_in_all_omes <- reactive({
      base::Reduce(base::intersect, lapply(GCTs_unprocessed_internal_reactive(), function(gct) names(gct@rdesc)))
    })
    observe({
      req(parameters_internal_reactive(), GCTs_unprocessed_internal_reactive())
      # Do not use req() on intersect vectors: length-0 intersect is valid and must still invalidate apply-to-all.
      groups_in_all <- isolate(groups_in_all_omes())
      rdesc_in_all <- isolate(rdesc_in_all_omes())
      
      # get relevant inputs
      current_label <- names(parameters_internal_reactive())[backNextLogic$place]
      current_annotation_column <- input[[paste0(current_label, "_annotation_column")]]
      current_group_norm_column <- input[[paste0(current_label, "_group_normalization_column")]]
      current_group_norm_selection <- input[[paste0(current_label, "_group_normalization")]]
      current_sample_filter_enabled <- input[[paste0(current_label, "_sample_filter_enabled")]]
      current_sample_filter_column <- input[[paste0(current_label, "_sample_filter_column")]]
      current_row_filter_enabled <- input[[paste0(current_label, "_row_filter_enabled")]]
      current_row_filter_column <- input[[paste0(current_label, "_row_filter_column")]]
      current_gene_symbol_column <- input[[paste0(current_label, "_gene_symbol_column")]]
      current_convert_ids <- input[[paste0(current_label, "_convert_ids_to_gene_symbol")]]
      current_id_source_column <- input[[paste0(current_label, "_id_source_column")]]
      if (is.null(current_sample_filter_column)) {
        current_sample_filter_column <- ""
      }
      if (is.null(current_row_filter_column)) {
        current_row_filter_column <- ""
      }
      if (is.null(current_id_source_column)) {
        current_id_source_column <- ""
      }
      
      valid <- gct_setup_apply_to_all_valid(
        annotation_column = current_annotation_column,
        group_normalization = current_group_norm_selection,
        group_normalization_column = current_group_norm_column,
        sample_filter_enabled = current_sample_filter_enabled,
        sample_filter_column = current_sample_filter_column,
        row_filter_enabled = current_row_filter_enabled,
        row_filter_column = current_row_filter_column,
        gene_symbol_column = current_gene_symbol_column,
        convert_ids_to_gene_symbol = current_convert_ids,
        id_source_column = current_id_source_column,
        groups_in_all = groups_in_all,
        rdesc_in_all = rdesc_in_all
      )
      
      # update applyToAll to FALSE if necessary
      if (!isTRUE(valid$ok)) {
        was_on <- isTRUE(isolate(input$applyToAll))
        updateCheckboxInput(session = session, inputId = "applyToAll", value = FALSE)
        if (was_on) {
          showNotification(valid$msg, type = "warning", duration = 8, session = session)
        }
      }
    })
    
    
    
    # change next/back buttons if applyToAll == TRUE
    observeEvent(input$applyToAll, {
      if (isTRUE(input$applyToAll)) {
        current_label <- names(parameters_internal_reactive())[backNextLogic$place]
        groups_in_all <- isolate(groups_in_all_omes())
        rdesc_in_all <- isolate(rdesc_in_all_omes())
        sc <- input[[paste0(current_label, "_sample_filter_column")]]
        rc <- input[[paste0(current_label, "_row_filter_column")]]
        if (is.null(sc)) sc <- ""
        if (is.null(rc)) rc <- ""
        idsc <- input[[paste0(current_label, "_id_source_column")]]
        if (is.null(idsc)) idsc <- ""
        valid <- gct_setup_apply_to_all_valid(
          annotation_column = input[[paste0(current_label, "_annotation_column")]],
          group_normalization = input[[paste0(current_label, "_group_normalization")]],
          group_normalization_column = input[[paste0(current_label, "_group_normalization_column")]],
          sample_filter_enabled = input[[paste0(current_label, "_sample_filter_enabled")]],
          sample_filter_column = sc,
          row_filter_enabled = input[[paste0(current_label, "_row_filter_enabled")]],
          row_filter_column = rc,
          gene_symbol_column = input[[paste0(current_label, "_gene_symbol_column")]],
          convert_ids_to_gene_symbol = input[[paste0(current_label, "_convert_ids_to_gene_symbol")]],
          id_source_column = idsc,
          groups_in_all = groups_in_all,
          rdesc_in_all = rdesc_in_all
        )
        if (!isTRUE(valid$ok)) {
          showNotification(valid$msg, type = "warning", duration = 8, session = session)
          updateCheckboxInput(session = session, inputId = "applyToAll", value = FALSE)
          return()
        }
      }
      
      # change next button to submit
      if (input$applyToAll | backNextLogic$place == backNextLogic$maxPlace) {
        output$rightButton <- renderUI({actionButton(ns("submitGCTButton"), 
                                                     "Submit", 
                                                     class = "btn btn-primary")})
      } else {
        output$rightButton <- renderUI({
          actionButton(ns("nextButton"), "Next >", class = "btn btn-primary")
        })
      }
      
      # change back button to "back to labels"
      if (input$applyToAll | backNextLogic$place == 1) {
        output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), 
                                                    "Back",
                                                    icon = icon("chevron-left"))})
      } else {
        output$leftButton <- renderUI({actionButton(ns("backButton"), 
                                                    "Back",
                                                    icon = icon("chevron-left"))})
      }
    }) 
    
    # logic for when next button is clicked
    observeEvent(input$nextButton, {
      if (backNextLogic$place < backNextLogic$maxPlace) {
        backNextLogic$place <- backNextLogic$place + 1
        backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
      }
    })
    
    # logic for when back button is clicked
    observeEvent(input$backButton, {
      if (backNextLogic$place > 1) {
        backNextLogic$place <- backNextLogic$place - 1
        backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
      }
    })
    
    # collect user options once next button is hit
    observeEvent(
      eventExpr = input$nextButton, 
      ignoreInit = TRUE,
      priority = 1, # this code is executed before other observeEvent with priority = 0 (default)
      handlerExpr = collectInputs())
    
    # collect user options once back button is hit
    observeEvent(
      eventExpr = input$backButton, 
      ignoreInit = TRUE,
      priority = 1, # this code is executed before other observeEvent with priority = 0 (default)
      handlerExpr = collectInputs())
    
    # collect user options once submit GCT button is hit
    observeEvent(
      eventExpr = input$submitGCTButton, 
      ignoreInit = TRUE,
      priority = 1, # this code is executed before other observeEvent with priority = 0 (default)
      handlerExpr = collectInputs())
    
    
    ### STEP 3: GCT PROCESSING ###
    
    # process GCTs 
    observeEvent(input$submitGCTButton, {
      parameters <- parameters_internal_reactive()
      GCTs_up <- GCTs_unprocessed_internal_reactive()
      # Work on deep clones so setup/upload GCTs are not mutated (no geneSymbol_original on upload).
      GCTs_work <- stats::setNames(
        lapply(names(GCTs_up), function(nm) deep_clone_gct(GCTs_up[[nm]])),
        names(GCTs_up)
      )
      
      # call processGCTs function in a tryCatch
      processing_output <- processGCTs(GCTs = GCTs_work, parameters = parameters)
      
      # Use updated parameters (e.g. ID conversion turned off after failed mapping)
      parameters_after_process <- parameters
      if (!is.null(processing_output)) {
        parameters_after_process <- processing_output$parameters
      }
      transformation_output <- transformGCTs(GCTs = GCTs_work, parameters = parameters_after_process)
      # Original GCTs for export/QC: log-transformed mat but row metadata as uploaded (no mapping backups)
      if (!is.null(transformation_output)) {
        for (nm in names(transformation_output)) {
          te <- transformation_output[[nm]]
          if (!is.null(te) && nm %in% names(GCTs_up)) {
            transformation_output[[nm]] <- repackage_transformed_gct_with_upload_rdesc(te, GCTs_up[[nm]])
          }
        }
      }
      
      if (!is.null(processing_output)) {
        # set GCTs_and_params reactiveVal
        GCTs_and_params(processing_output)
        
        # Notify if ID conversion was auto-disabled for any dataset (details in warnings)
        for (ome in names(processing_output$parameters)) {
          if (ome %in% names(parameters) &&
              isTRUE(parameters[[ome]]$convert_ids_to_gene_symbol) &&
              !isTRUE(processing_output$parameters[[ome]]$convert_ids_to_gene_symbol)) {
            showNotification(
              paste0(
                "Dataset ", ome, ": \"Convert IDs to gene symbols\" was turned off because ",
                "no gene symbols could be resolved. Check the R console for message() details."
              ),
              type = "warning",
              duration = 8,
              session = session
            )
          }
        }
        
        # Keep sidebar setup parameters in sync with processing (conversion flags, etc.)
        merged_params <- parameters_internal_reactive()
        if (!is.null(merged_params)) {
          for (ome in names(processing_output$parameters)) {
            merged_params[[ome]] <- processing_output$parameters[[ome]]
            # User's setup choice must survive processing (same list is mutated in pipeline)
            if (ome %in% names(parameters)) {
              merged_params[[ome]]$gene_symbol_column <- parameters[[ome]]$gene_symbol_column
            }
          }
          parameters_internal_reactive(merged_params)
        }
        
        # save the original GCTs for output
        # these have been log transformed if selected
        GCTs_original(transformation_output)
        
        # increment gctsGO reactiveVal to show that processing is done
        gctsGO(gctsGO() + 1)
      }
    })
    
    # set new global variables
    observeEvent(gctsGO(), {
      req(GCTs_and_params())

      parameters <- GCTs_and_params()$parameters
      all_omes <- names(parameters)

      globals$omes <- all_omes
      globals$default_ome <- all_omes[1]
      globals$default_annotations <- sapply(
        all_omes,
        function(ome) parameters[[ome]]$annotation_column,
        simplify = FALSE
      )

      # Initialize colors with colorblind-safe palette
      GCTs <- GCTs_and_params()$GCTs
      GCTs_merged <- GCTs_and_params()$GCTs_merged
      req(GCTs, GCTs_merged)
      globals$colors <- make_custom_colors(GCTs, GCTs_merged)
    }, ignoreInit = TRUE)

    # move the current tab to the summary tab
    observeEvent(gctsGO(), {
      updateTabsetPanel(session = parent, inputId = "navbar-tabs", selected = "Summary")
    }, ignoreInit = TRUE)
    
    
    ### STEP 4: ADVANCED SETTINGS ###
    
    # once GCT setup submitted, go to advanced settings
    observeEvent(gctsGO(), {
      labels = names(GCTs_and_params()$parameters)
      output$sideBarMain <- renderUI({
        tagList(
          advancedSettingsUI(ns = ns, parameters = GCTs_and_params()$parameters),
        )})
      output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), 
                                                  "Back to setup",
                                                  icon = icon("chevron-left"))})
      output$rightButton <- NULL
    }, ignoreInit = TRUE)
    
    # add default -ome to globals
    observeEvent(
      input$default_ome, 
      globals$default_ome <- input$default_ome,
      ignoreInit = TRUE)
    
    
    
    
    ### LOCAL HELPER FUNCTIONS ###
    # these functions interact with the session's input/output and are used in 
    # multiple observeEvent() calls, so it's easier to have them defined as
    # as local helper functions
    
    # collect user inputs, has to be used in separate observeEvent() calls
    collectInputs <- function() {
      # get the current label
      all_labels <- names(parameters_internal_reactive())
      current_label <- all_labels[backNextLogic$place]

      # select labels for assignment
      applyToAll <- ifelse(is.null(input$applyToAll), FALSE, input$applyToAll)
      if (applyToAll) {
        sc <- input[[paste0(current_label, "_sample_filter_column")]]
        rc <- input[[paste0(current_label, "_row_filter_column")]]
        if (is.null(sc)) sc <- ""
        if (is.null(rc)) rc <- ""
        idsc <- input[[paste0(current_label, "_id_source_column")]]
        if (is.null(idsc)) idsc <- ""
        valid <- gct_setup_apply_to_all_valid(
          annotation_column = input[[paste0(current_label, "_annotation_column")]],
          group_normalization = input[[paste0(current_label, "_group_normalization")]],
          group_normalization_column = input[[paste0(current_label, "_group_normalization_column")]],
          sample_filter_enabled = input[[paste0(current_label, "_sample_filter_enabled")]],
          sample_filter_column = sc,
          row_filter_enabled = input[[paste0(current_label, "_row_filter_enabled")]],
          row_filter_column = rc,
          gene_symbol_column = input[[paste0(current_label, "_gene_symbol_column")]],
          convert_ids_to_gene_symbol = input[[paste0(current_label, "_convert_ids_to_gene_symbol")]],
          id_source_column = idsc,
          groups_in_all = isolate(groups_in_all_omes()),
          rdesc_in_all = isolate(rdesc_in_all_omes())
        )
        if (!isTRUE(valid$ok)) {
          showNotification(valid$msg, type = "warning", duration = 8, session = session)
          updateCheckboxInput(session = session, inputId = "applyToAll", value = FALSE)
          applyToAll <- FALSE
        }
      }
      if (applyToAll) {
        assignment_labels = all_labels # all labels
      } else {
        assignment_labels <- current_label # just the current label
      }

      # get the current parameters
      new_parameters <- parameters_internal_reactive()

      # get the list of all parameters names to update
      parameter_names <- c(names(default_parameters),
                           'annotation_column',
                           'group_normalization_column',
                           'gene_symbol_column',
                           'sample_filter_enabled',
                           'sample_filter_column',
                           'sample_filter_values',
                           'row_filter_enabled',
                           'row_filter_column',
                           'row_filter_values')

      # assign new user selections
      # NOTE: there are fields in `new_parameters` that aren't updated here,
      # which means you can't easily forgo the for loop for an apply equivalent
      for (label in assignment_labels) {
        for (param in parameter_names) {
          input_value <- input[[paste0(current_label, '_', param)]]

          # Keep stored values when inputs are absent (first paint, UI rebuild) or when
          # the field has no widget (e.g. id_mapping_* stats from processing). Otherwise
          # NULL overwrites break gene symbol / ID mapping persistence.
          if (is.null(input_value) && param %in% c(
                "gene_symbol_column",
                "convert_ids_to_gene_symbol",
                "id_source_column",
                "id_mapping_species",
                "id_mapping_keytype",
                "id_mapping_n_total",
                "id_mapping_n_unmapped")) {
            next
          }

          # Convert intensity_data checkbox boolean to "Yes"/"No" string
          if (param == "intensity_data" && is.logical(input_value)) {
            input_value <- if (input_value) "Yes" else "No"
          }
          if (param == "sample_filter_column" && is.null(input_value)) {
            input_value <- ""
          }
          if (param == "sample_filter_values" && is.null(input_value)) {
            input_value <- character(0)
          }
          if (param == "row_filter_column" && is.null(input_value)) {
            input_value <- ""
          }
          if (param == "row_filter_values" && is.null(input_value)) {
            input_value <- character(0)
          }
          if (param == "id_source_column" && is.null(input_value)) {
            input_value <- ""
          }

          new_parameters[[label]][[param]] <- input_value
        }
      }

      # assign reactiveVal
      parameters_internal_reactive(new_parameters)
    }
    
    # label assignment, has to be used in separate observeEvent() calls
    labelAssignment <- function() {
      output$sideBarMain <- renderUI({labelSetupUI(ns = ns,
                                                   gctFileNames = accumulated_files()$name)})
      output$rightButton <- renderUI({actionButton(ns("submitLabelsButton"), 
                                                   "Submit",
                                                   class = "btn btn-primary")})
      output$leftButton <- NULL
      
      # update with saved labels if they exist
      lapply(names(parameters_internal_reactive()), function(label) {
        filename <- parameters_internal_reactive()[[label]]$gct_file_name
        updateTextInput(inputId = paste0('Label_', filename), value = label)
      })
    }
    

    # CSV/Excel/TSV workflow function - starts with label assignment (same as GCT workflow)
    csvExcelWorkflow <- function() {
      csvExcelLabelAssignment()
    }
    
    # Label assignment for CSV/Excel/TSV files (same pattern as GCT workflow)
    csvExcelLabelAssignment <- function() {
      output$sideBarMain <- renderUI({csvExcelLabelSetupUI(ns = ns,
                                                           dataFileNames = accumulated_files()$name)})
      output$rightButton <- renderUI({actionButton(ns("submitCSVExcelLabelsButton"),
                                                   "Next",
                                                   class = "btn btn-primary")})
      output$leftButton <- NULL

      # Update with saved labels if they exist (same as GCT workflow)
      lapply(names(parameters_internal_reactive()), function(label) {
        filename <- parameters_internal_reactive()[[label]]$gct_file_name
        updateTextInput(inputId = paste0('CSVExcelLabel_', filename), value = label)
      })
    }

    # Handle CSV/Excel/TSV label submission
    observeEvent(input$submitCSVExcelLabelsButton, {
      # Collect labels from input fields
      labels <- sapply(accumulated_files()$name, function(file) {
        input[[paste0('CSVExcelLabel_', file)]]
      })
      
      # Validate labels
      if (any(labels == "")) {
        showNotification(
          ui = HTML(paste0("<b>Error</b><br>", "Please provide labels for all files.")),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }
      
      # Check for duplicate labels
      if (length(unique(labels)) != length(labels)) {
        showNotification(
          ui = HTML(paste0("<b>Error</b><br>", "Please provide unique labels for each file.")),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }
      
      # Store labels in parameters_internal_reactive (same as GCT workflow)
      for (i in seq_along(labels)) {
        filename <- accumulated_files()$name[i]
        label <- labels[i]
        parameters_internal_reactive(c(
          parameters_internal_reactive(),
          setNames(list(list(gct_file_name = filename)), label)
        ))
      }

      # Move to identifier column selection
      csvExcelIdentifierSelection(labels)
    })

    # Identifier column selection step
    csvExcelIdentifierSelection <- function(labels) {
      output$sideBarMain <- renderUI({csvExcelIdentifierSetupUI(ns = ns,
                                                               dataFiles = accumulated_files(),
                                                               labels = labels)})
      output$rightButton <- renderUI({actionButton(ns("submitCSVExcelIdentifiersButton"), 
                                                   "Next",
                                                   class = "btn btn-primary")})
      output$leftButton <- renderUI({actionButton(ns("backToCSVExcelLabelsButton"), 
                                                  "Back",
                                                  icon = icon("chevron-left"),
                                                  class = "btn btn-default")})
      
      # Update with saved identifier columns if they exist
      stored_identifiers <- csvExcel_identifier_columns_reactive()
      if (!is.null(stored_identifiers)) {
        for (i in seq_along(stored_identifiers)) {
          updateSelectInput(inputId = paste0("identifierColumn_", i), 
                           selected = stored_identifiers[i])
        }
      }
    }
    
    # Handle identifier column submission
    observeEvent(input$submitCSVExcelIdentifiersButton, {
      # Collect identifier columns for each file
      identifier_columns <- sapply(seq_len(nrow(accumulated_files())), function(i) {
        input[[paste0("identifierColumn_", i)]]
      })

      # Validate identifier columns
      if (any(is.null(identifier_columns)) || any(identifier_columns == "")) {
        showNotification(
          ui = HTML(paste0("<b>Error</b><br>", "Please select identifier columns for all datasets.")),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }

      # Store identifier columns for later retrieval
      csvExcel_identifier_columns_reactive(identifier_columns)

      # Move to experimental design setup
      csvExcelExpDesignSetup(identifier_columns)
    })

    # Experimental design setup step
    csvExcelExpDesignSetup <- function(identifier_columns) {

      output$sideBarMain <- renderUI({csvExcelExpDesignSetupUI(ns = ns,
                                                      dataFiles = accumulated_files(),
                                                              labels = sapply(accumulated_files()$name, function(file) {
                                                                input[[paste0('CSVExcelLabel_', file)]]
                                                              }))})
      output$rightButton <- NULL
      output$leftButton <- renderUI({actionButton(ns("backToCSVExcelIdentifiersButton"),
                                                  "Back",
                                                  icon = icon("chevron-left"),
                                                  class = "btn btn-default")})
    }

    # Handle back navigation
    observeEvent(input$backToCSVExcelLabelsButton, {
      csvExcelLabelAssignment()
    })

    observeEvent(input$backToCSVExcelIdentifiersButton, {
      labels <- sapply(accumulated_files()$name, function(file) {
        input[[paste0('CSVExcelLabel_', file)]]
      })
      csvExcelIdentifierSelection(labels)
    })
    
    # Create a reactive to store sample names for template generation
    template_sample_names <- reactive({
      req(accumulated_files())

        tryCatch({
        all_samples <- c()
        for (i in seq_len(nrow(accumulated_files()))) {
          file_path <- accumulated_files()$datapath[i]
          file_ext <- tools::file_ext(tolower(accumulated_files()$name[i]))
          
          if (file_ext == "csv") {
            data <- readr::read_csv(file_path, n_max = 1, show_col_types = FALSE)
            samples <- names(data)
          } else if (file_ext == "tsv") {
            data <- readr::read_tsv(file_path, n_max = 1, show_col_types = FALSE)
            samples <- names(data)
          } else if (file_ext %in% c("xlsx", "xls")) {
            data <- readxl::read_excel(file_path, n_max = 1)
            samples <- names(data)
            } else {
            samples <- c()
          }
          
          all_samples <- c(all_samples, samples)
        }
        
        # Get unique sample names
        unique(all_samples)
        }, error = function(e) {
        c("Sample1", "Sample2", "Sample3") # Fallback template
      })
    })
    
    # Download handler for experimental design template
    output$downloadExpDesignTemplate <- downloadHandler(
      filename = "experimental_design_template.csv",
      content = function(file) {
        # Get sample names from reactive
        sample_names <- template_sample_names()
        
        # Create template data frame with columnName, Experiment, and Group columns
        template_df <- data.frame(
          columnName = sample_names,
          Experiment = rep("", length(sample_names)),
          Group = rep("", length(sample_names)),
            stringsAsFactors = FALSE
          )
          
        # Write the template to file
        write.csv(template_df, file, row.names = FALSE)
      }
    )
    
    # Reactive output to control process button visibility for CSV/Excel
    output$expDesignFileUploaded <- reactive({
      return(!is.null(input$expDesignFile))
    })
    outputOptions(output, "expDesignFileUploaded", suspendWhenHidden = FALSE)
    
    
    

    # Process CSV/Excel data when experimental design is uploaded
    observeEvent(input$processCSVExcel, {
      req(input$expDesignFile)
      
      my_shinyalert_tryCatch(
        text.error = "<b>CSV/Excel Processing Error:</b>",
        append.error = TRUE,
        show.error = TRUE,
        return.error = NULL,
        expr = {
          # Process CSV/Excel files with progress indication
          withProgress(message = "Processing CSV/Excel files...", {
            setProgress(0.2, detail = "Reading experimental design")
            
            # Read experimental design
            exp_design <- readExperimentalDesign(input$expDesignFile$datapath)
            
            setProgress(0.5, detail = "Converting data to analysis format")
            
            # Process CSV/Excel files with per-dataset identifier columns
            identifier_columns <- csvExcel_identifier_columns_reactive()
            
            # Validate identifier columns
            if (is.null(identifier_columns) || any(identifier_columns == "")) {
              stop("Please select identifier columns for all datasets.")
            }
            
            # Get labels from input fields
            labels <- sapply(accumulated_files()$name, function(file) {
              input[[paste0('CSVExcelLabel_', file)]]
            })

            csv_excel_result <- processCSVExcelWorkflowWithPerDatasetIdentifiers(accumulated_files(), exp_design, identifier_columns, labels)
            
            setProgress(0.8, detail = "Setting up analysis parameters")
            
            # Store converted GCT objects and parameters for later processing (same as GCT workflow)
            GCTs_unprocessed_internal_reactive(csv_excel_result$GCTs)
            parameters_internal_reactive(csv_excel_result$parameters)
            
            # Set up back/next navigation logic for parameter setup
            backNextLogic$place <- 1
            backNextLogic$maxPlace <- length(csv_excel_result$GCTs)
            backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
            
            setProgress(1.0, detail = "Ready for parameter setup")
          })
          
          # Show success message
          showNotification(
            ui = HTML(paste0("<b>Files Converted!</b><br>", "Successfully converted ", length(csv_excel_result$GCTs),
                        " CSV/Excel file(s) to analysis format. Now configure analysis parameters...")),
            type = "message",
            duration = NULL,
            closeButton = TRUE
          )
          
          # Automatically switch to Dataset Setup help tab
          updateTabsetPanel(session = parent, 
                           inputId = "navbar-tabs", 
                           selected = "Help-Analysis")
          
          # Switch to the Dataset Setup tab within the help section
          shinyjs::runjs("
            setTimeout(function() {
              $('a[data-value=\"Dataset Setup\"]').click();
            }, 100);
          ")
          
          # Trigger the standard parameter setup workflow (same as GCT files)
          labelsGO(labelsGO() + 1)
        }
      )
    })
    
    # return GCTs and parameters together in one list
    return(list(GCTs_and_params = GCTs_and_params,
                globals = globals,
                GCTs_original = GCTs_original))
    
  }) # end moduleServer
} # end setupSidebarServer