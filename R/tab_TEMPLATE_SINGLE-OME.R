################################################################################
# Module: TEMPLATE_SINGLE-OME
#
# This is a template for how to add a module that contains identical tabs for  
# each ome. This template has built-in functionality for handing omes, 
# displaying plots/tables/etc in boxes, and exporting outputs.
################################################################################
# This template contains two pairs of server/UI module functions:
#
# templateSingleOme_Tab_UI & templateSingleOme_Tab_Server handle the overall skeleton
# for having tabs with multiple -omes. Much of this code shouldn't need to be 
# changed if you are making a new module that matches this ome tab structure.
#
# templateSingleOme_Ome_UI & templateSingleOme_Ome_Server contain the code for the content
# of a single ome tab. Generally speaking, you should be able to write code in 
# here just like any regular shiny module server/ui without worrying about how 
# all of the omes come together.
################################################################################

## Steps for adding a new module
# 0a. Create a copy of the script and rename it with your new module's name
# 0b. Cmd/Ctrl + F "templateSingleOme" and replace all with your new module's name
# 1. Cmd/Ctrl + F "TODO:" to find to-do comments. These comments point to where
#    to edit function inputs, plots, etc.
# 2. Add helper functions in helper script(s) called 
#    "tab_[MODULE-NAME]_helpers_[DESCRIPTOR].R"
# 3. add necessary imports in protigyRevamp-package.R or above a function 
#    definition using roxygen tags
# 4. call your tab's UI function (`templateSingleOme_Tab_UI()`) in app_UI 
#    (located in app_ui.R)
# 5. call your tab's server function (`templateSingleOme_Tab_Server()`) in app_server 
#    (located in app_server.R)
# 6. Add the outputs (i.e. exports) from your module to `all_exports` in app_server

################################################################################
# Shiny functions (UI and server)
################################################################################

# UI for the templateSingleOme tab
# contains the structure for the big tabbed box with omes
templateSingleOme_Tab_UI <- function(id = "templateSingleOmeTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # display omes tabs
    fluidRow(uiOutput(ns("ome_tabset_box")))
    
  ) # end tagList
}

# server for the templateSingleOme tab
# contains the structure for the big tabbed box with omes
templateSingleOme_Tab_Server <- function(id = "templateSingleOmeTab",
                                   GCTs_and_params, 
                                   globals, 
                                   GCTs_original) { 
  
  # TODO: edit this tab's inputs...depending on the module, you may not need
  # `globals` or `GCTs_original` 
  # (I would be surprised if you didn't need `GCTs_and_params`)
  
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    ## GATHERING INPUTS ##
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    # TODO: edit the rest of the inputs in this section, remove unnecessary ones
    # Parameters that can be removed if unnecessary are:
    # `parameters`
    # `default_annotations`
    # `custom_colors`
    # `GCTs_merged`
    #
    # `all_omes` and `default_ome` are used in this function, DO NOT REMOVE
    
    # GCTs to use for analysis/visualization
    GCTs <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })
    
    # parameters used to process GCTs
    parameters <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })
    
    # Large merged GCT with all omes containing `protigy.ome` column in `rdesc`
    GCTs_merged <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs_merged
    })
    
    # named list of default annotation columns for each ome
    default_annotations <- reactive({
      req(parameters())
      sapply(parameters(), function(p) p$annotation_column, simplify = FALSE)
    })
    
    # vector of all omes
    all_omes <- reactive(names(GCTs())) # don't remove
    
    # gather relevant variables from globals
    default_ome <- reactive(globals$default_ome) # don't remove this variable!
    custom_colors <- reactive(globals$colors)
    
    
    ## OME TABS ##
    
    # handles compiling ome tabs into styled tabset box
    output$ome_tabset_box <- renderUI({
      req(all_omes(), default_ome())
      
      # generate a tab for each -ome
      tabs <- lapply(all_omes(), function(ome){
        tabPanel(
          title = ome,
          
          # call the UI function for each individual ome
          templateSingleOme_Ome_UI(id = ns(ome), ome = ome)
          
        ) # end tabPanel
      }) # end lapply
      
      # combine all tabs into tabSetPanel
      tab_set_panel <- do.call(
        tabsetPanel, 
        c(tabs, list(id = ns("ome_tabs"), selected = isolate(default_ome())))
      )
      
      # put everything in a big box with ome tabs and return
      # add necessary CSS classes
      add_css_attributes(
        shinydashboardPlus::box(
          tab_set_panel,
          width = 12
        ), 
        classes = c("box-no-header", "box-with-tabs")
      )
    }) # end renderUI
    
    # update selected tab based on default -ome
    observe({
      updateTabsetPanel(inputId = "ome_tabs", selected = default_ome())
    })
    
    # call the server function for each individual ome
    all_plots <- reactiveVal() # initialize
    observeEvent(all_omes(), {
      output_plots <- sapply(all_omes(), function(ome) {
        templateSingleOme_Ome_Server(
          # TODO: edit inputs to the ome server function, the last 4 may be unnecessary
          id = ome,
          ome = ome,
          GCT_processed = reactive(GCTs()[[ome]]),
          parameters = reactive(parameters()[[ome]]),
          GCT_original = reactive(GCTs_original()[[ome]]),
          default_annotation_column = reactive(default_annotations()[[ome]]),
          color_map = reactive(custom_colors()[[ome]])
        )
      }, simplify = FALSE)
      
      all_plots(output_plots) # set reactive value with outputs
    })
    
    return(all_plots)
  })
}



# UI for an individual ome
templateSingleOme_Ome_UI <- function (id, ome) {
                
  ns <- NS(id)
  
  tagList(
    
    # TODO: add UI for each ome tab here. Make sure to use `ns`!
    # Example included below
    
    # example plot
    fluidRow(
      shinydashboardPlus::box(
        
        # add the contents of the box (e.g. a plot) here
        plotlyOutput(ns("example_plot")),
        
        # add a sidebar for controls (remove if not needed)
        sidebar = boxSidebar(
          uiOutput(ns("example_plot_sidebar_contents")),
          id = ns("example_plot_sidebar"),
          width = 25,
          icon = icon("gears", class = "fa-2xl"),
          background = "rgba(91, 98, 104, 0.9)"
        ),
        
        # add a dropdown for help text (remove if not needed)
        dropdownMenu = boxDropdown(
          icon = icon("question", class = "fa-xl"),
          uiOutput(ns("example_plot_dropdown_contents"))
        ),
        
        # other box parameters
        status = "primary",
        width = 12,
        title = "Example Plot",
        headerBorder = TRUE,
        solidHeader = TRUE
      ) # end box
    ) # end fluidRow
  )
}


# server for an individual ome
templateSingleOme_Ome_Server <- function(id,
                                   ome,
                                   GCT_processed,
                                   parameters,
                                   GCT_original,
                                   default_annotation_column,
                                   color_map) {
  
  # TODO: edit ome server inputs, remove unnecessary ones
  # make sure to keep `id` (necessary for the module to work) and `ome` (so you
  # know which ome you're working with)
  # NOTE: other than `id` and `ome`, all inputs should be reactive!
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace, use in renderUI-like functions
    ns <- session$ns
    
    # TODO: render plots/tables/etc according to the example below.
    
    ## RENDER EXAMPLE PLOT ##
    
    # A reactive function that creates your plot for a single ome
    # This function should use reactive variables and should only be called in
    # a reactive context.
    # The output should be used for display AND for exporting....
    # meaning it should output something that can be rendered in the app's UI
    # and also downloaded as a PDF.
    # Note: this means that this function's output should be non-interactive 
    # (for example, output a ggplot object here, convert to plotly later)
    example_plot_reactive <- reactive({
      
      # validate/need/require statements
      validate(
        need(GCT_processed(), "GCTs not processed") %then%
          need(default_annotation_column(), "don't know what annotation to use")
      )
      
      # generating a dummy example plot here
      # Note: it's helpful to have the -ome somewhere in the title
      ggplot() + ggtitle(paste("Example for:", ome))
    })
    
    # Render the plot to display in the app
    # This is also where you would do anything that is app-display-specific  
    # (like converting ggplot to plotly)
    output$example_plot <- renderPlotly({
      
      # call your reactive function, defined above
      gg <- example_plot_reactive()
      
      # convert to plotly
      ggplotly(gg)
    })
    
    # add sidebar content, if desired
    output$example_plot_sidebar_contents <- renderUI({
      tagList(
        "Sidebar contents here"
      )
    })
    
    # add dropdown content, if desired
    output$example_plot_dropdown_contents <- renderUI({
      tagList(
        p("Dropdown contents here")
      )
    })
    
    ## COMPILE EXPORTS ##
    
    # TODO: make a function to save desired exports
    # input to the functionis the output directory path
    # the function should save your plot/table/etc. in this directory
    # you should call the reactive object for the export here
    
    # Example of export function
    example_plot_export_function <- function(dir_name) {
      ggsave(
        filename = paste0("example_plot_", ome, ".pdf"), 
        plot = example_plot_reactive(), # call the reactive object
        device = 'pdf',
        path = dir_name # save in the desired output directory
      )
    }
    
    
    # TODO: return a named list of custom export functions, example included
    return(
      list(
        example_plot = example_plot_export_function
      )
    )
  })
}

