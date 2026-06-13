################################################################################
# Module: PELSA helpers
#
# Pure (non-reactive) helper functions shared across the PELSA section modules
# (tab_pelsa_section1.R, tab_pelsa_section2.R, tab_pelsa_section3.R).
#
# Keep these free of Shiny reactivity so they remain fully unit-testable. As the
# PELSA functionality is built out, add computation/plotting helpers here (or in
# section-specific helper files, e.g. tab_pelsa_section1_helpers.R) rather than
# inline in the module servers.
################################################################################

# Standard placeholder box shown in each PELSA section before its analysis has
# been implemented. Returns a shinydashboardPlus box describing the section.
#
# @param ns       the module's namespace function (session$ns)
# @param ome      character, the ome label this section is rendered for
# @param title    character, the box/section title
# @param message  character, the placeholder body text
# @return a fluidRow containing a styled box
pelsa_placeholder_box <- function(ns, ome, title, message) {
  fluidRow(
    shinydashboardPlus::box(
      div(
        style = paste(
          "background-color: #f8f9fa; border-left: 4px solid #007bff;",
          "padding: 12px; margin-bottom: 15px; border-radius: 0 4px 4px 0;"
        ),
        icon("info-circle", style = "color: #007bff; margin-right: 8px;"),
        strong("Coming soon: ", style = "color: #495057;"),
        span(message, style = "color: #495057;")
      ),
      p(paste0("Ome: ", ome)),
      status       = "primary",
      width        = 12,
      title        = title,
      headerBorder = TRUE,
      solidHeader  = TRUE
    )
  )
}
