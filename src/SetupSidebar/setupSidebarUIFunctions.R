################################################################################
# Module: SETUP SIDEBAR
# Helper UI functions. Most of these are called in renderUI() functions inside 
# of setupSidebarServer()
################################################################################

# function for label assignment UI
labelSetupUI <- function(ns, gctFileNames) {
  tagList(
    p(strong('Assign labels')),
    lapply(gctFileNames, function(file) {
      textInput(inputId = ns(paste0('Label_', file)),
                label = file,
                placeholder = "Proteome or Prot")
    })
  )
}

# function containing setup elements for a single GCT file
gctSetupUI <- function(ns, label, parameters, parameter_choices) {
  tagList(
    p(strong(paste('Setup for', label))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_intensity_data')), 
                                    'Intensity data',
                                    choices = parameter_choices$intensity_data,
                                    selected = parameters[[label]]$intensity_data))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_log_transform')),
                                    label = 'Log-transformation',
                                    choices = parameter_choices$log_transformation,
                                    selected = parameters[[label]]$log_transform))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_data_normalization')),
                                    label = 'Data normalization',
                                    choices = parameter_choices$data_normalization,
                                    selected = parameters[[label]]$data_normalization))),
    fluidRow(column(12, numericInput(ns(paste0(label, '_max_missing')), 
                                     'Max. % missing values',
                                     min = parameter_choices$max_missing$min,
                                     max = parameter_choices$max_missing$max,
                                     value = parameters[[label]]$max_missing,
                                     step = parameter_choices$max_missing$step))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_data_filter')),
                                    label = 'Filter data',
                                    choices = parameter_choices$data_filter,
                                    selected = parameters[[label]]$data_filter))),
    if (length(parameters) > 1) {
      fluidRow(column(12, checkboxInput(ns('applyToAll'), 'Apply settings to all -omes')))
    }
  )
}

# function for advanced settings UI
advancedSettingsUI <- function(ns) {
  tagList(
    p(strong("Advanced settings")),
    fluidRow(column(12, actionButton(ns('selectGroupsButton'), 'Select groups'))),
    fluidRow(column(12, actionButton(ns('customizeColorsButton'), 'Customize colors'))),
    hr()
  )
}
