################################################################################
# Module: SETUP SIDEBAR
# File contains helper functions that are used for app UI components or 
# server-side validation of user inputs.
################################################################################

################################################################################
# Helper UI functions.
################################################################################

# function for label assignment UI
labelSetupUI <- function(ns, gctFileNames) {
  tagList(
    h4('Assign labels'),
    lapply(gctFileNames, function(file) {
      textInput(inputId = ns(paste0('Label_', file)),
                label = file,
                placeholder = "Proteome or Prot")
    })
  )
}

# function containing setup elements for a single GCT file
gctSetupUI <- function(ns, 
                       label, 
                       parameter_choices, 
                       parameters, 
                       current_place, 
                       max_place,
                       GCTs) {
  # groups column choices pulled from cdesc
  groups_choices <- names(GCTs[[label]]@cdesc)
  
  # find which groups are present in all omes
  groups_choices_all_omes <- base::Reduce(base::intersect, 
                                    lapply(GCTs, function(gct) names(gct@cdesc)))
  
  tagList(
    h4('Setup for ',
       strong(span(label, style = "color:#00c0ef")),
       paste0(' (', current_place, '/', max_place, ')')), 
    
    ## groups column selection
    fluidRow(column(12, selectInput(ns(paste0(label, '_groups_column')),
                                    "Groups column",
                                    choices = groups_choices,
                                    selected = ifelse(
                                      is.null(parameters[[label]]$groups_column),
                                      groups_choices[1],
                                      parameters[[label]]$groups_column)))),
    
    ## intentisy data input
    fluidRow(column(12, selectInput(ns(paste0(label, '_intensity_data')), 
                                    'Intensity data',
                                    choices = parameter_choices$intensity_data,
                                    selected = parameters[[label]]$intensity_data))),
    
    ## log transformation input
    fluidRow(column(12, selectInput(ns(paste0(label, '_log_transformation')),
                                    label = 'Log-transformation',
                                    choices = parameter_choices$log_transformation,
                                    selected = parameters[[label]]$log_transformation))),
    
    ## data normalization input
    fluidRow(column(12, selectInput(ns(paste0(label, '_data_normalization')),
                                    label = 'Data normalization',
                                    choices = parameter_choices$data_normalization$intensity_data_no,
                                    selected = parameters[[label]]$data_normalization))),
    
    ## group-wise normalization
    conditionalPanel(
      condition = paste0("input['", label, "_data_normalization'] != 'None'"),
      checkboxInput(ns(paste0(label, '_group_normalization')),
                                        label = "Perform group-wise normalization",
                                        value = parameters[[label]]$group_normalization),
      ns = ns
    ),
    
    ## max missing value input
    fluidRow(column(12, numericInput(ns(paste0(label, '_max_missing')), 
                                     'Max. % missing values',
                                     min = parameter_choices$max_missing$intensity_data_no$min,
                                     max = parameter_choices$max_missing$intensity_data_no$max,
                                     step = parameter_choices$max_missing$intensity_data_no$step,
                                     value = parameters[[label]]$max_missing))),
    
    ## data filter input 
    fluidRow(column(12, selectInput(ns(paste0(label, '_data_filter')),
                                    label = 'Filter data',
                                    choices = parameter_choices$data_filter$intensity_data_no,
                                    selected = parameters[[label]]$data_filter))),
    
    ## apply to all checkbox
    if (max_place > 1) {
      # only shows up if the groups column selection is present in all -omes
      conditionalPanel(
        condition = paste0("['", 
                           paste(groups_choices_all_omes, collapse = "', '"), 
                           "'].includes(input['", label, "_groups_column'])"),
        fluidRow(column(12, checkboxInput(ns('applyToAll'), 'Apply settings to all -omes'))),
        ns = ns
      )
      
    }
  )
}

# function for advanced settings UI
advancedSettingsUI <- function(ns, labels) {
  tagList(
    h4("Advanced settings"),
    if (length(labels) > 1) {
      fluidRow(column(12, selectInput(ns('default_ome'),
                                      "Default -ome",
                                      choices = labels)))
    },
    fluidRow(column(12, selectInput(ns('default_annotation'),
                                    "Default annotation of interest",
                                    choices = NULL))),
    fluidRow(column(12, actionButton(ns('selectGroupsButton'), 'Select groups'))),
    fluidRow(column(12, actionButton(ns('customizeColorsButton'), 'Customize colors'))),
    hr()
  )
}


################################################################################
# Helper functions for the setup sidebar module
################################################################################

# function to validate file labels
# all_labels is a named vector:
# elements in vector are all user inputted labels
# names are the corresponding file names
validate_labels <- function(all_labels) {
  # check that each label is a valid name
  for (i in seq_along(all_labels)) {
    label = all_labels[i]
    filename = names(all_labels)[i]
    if (make.names(label) != label) {
      stop(paste("Invalid label for", filename))
    }
  }
  
  # check that labels aren't repeated
  if (length(unique(all_labels)) != length(all_labels)) {
    stop("All labels must be unique")
  }
  
  return(TRUE)
}
