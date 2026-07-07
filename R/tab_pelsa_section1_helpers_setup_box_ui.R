################################################################################
# PELSA Setup section (Section 1) - the per-dataset Setup box UI (Task 5B/5D).
#
# Split out of tab_pelsa_section1_helpers.R (which holds the compound/marker +
# ordering helpers) purely to keep files/functions within the repo's size
# guidelines. pelsa_setup_box_ui() is the pure tag constructor the module
# server (tab_pelsa_section1.R) calls to render the Setup box; it delegates to
# three private per-layer builders below (data-input / config / action) so no
# single function stays over the ~50-line guideline.
################################################################################

# A small uppercase eyebrow header with a leading icon for a Setup section.
# Color is supplied by the parent .pelsa-layer-* class (CSS var); pairing the
# icon + text label here means the layer is NEVER signalled by color alone.
#
# @param icon_name a Font Awesome icon name (shiny::icon).
# @param label     the section header text.
# @return a shiny tag (the section header).
# @noRd
pelsa_section_head <- function(icon_name, label) {
  shiny::tags$div(
    class = "pelsa-section-head",
    shiny::icon(icon_name), shiny::tags$span(label)
  )
}

# 2 + 3 + 4 + 5. DATA-INPUT LAYER (blue) of the Setup box: FASTA/annotation
# upload, self-curated toggle, treatment compound, and the marker paste box +
# reactive table for ONE dataset. Factored out of pelsa_setup_box_ui() so that
# function stays a thin assembler.
#
# @param compounds character vector of compound preset names.
# @param ns        the module namespacer (session$ns / NS(id)).
# @param selected_compound the persisted compound for THIS dataset ("" = none).
# @param self_curated      persisted self-curated flag for THIS dataset.
# @return a shiny tag (the data-input section).
# @noRd
pelsa_setup_data_section <- function(compounds, ns, selected_compound, self_curated) {
  shiny::tags$div(
    class = "pelsa-section pelsa-layer-data",
    pelsa_section_head("table-list", "Data inputs"),

    # 2. FASTA + annotation upload for THIS dataset. Default: UniProt-style FASTA
    #    (pipe-aware) + a required raw annotation file. The self-curated checkbox
    #    switches the FASTA parse to first-token and greys out the annotation
    #    uploader (a self-curated database has no UniProt annotation file).
    shiny::fileInput(
      ns("pelsa_fasta"),
      label  = "FASTA file (.fasta / .fa)",
      accept = c(".fasta", ".fa")
    ),
    shiny::checkboxInput(
      ns("pelsa_self_curated"),
      label = "Self-curated database (no annotation file)",
      value = self_curated
    ),
    {
      annotation_uploader <- shiny::tags$div(
        id = ns("pelsa_annotation_wrap"),
        shiny::fileInput(
          ns("pelsa_annotation"),
          label  = "Feature annotation file (.tsv)",
          accept = c(".tsv", ".txt", ".tab")
        )
      )
      # Grey out from the FIRST render when this dataset is already self-curated
      # (re-entering its tab), so the disabled state is correct without waiting on
      # the self-curated observer to fire. The observer keeps it in sync on toggle.
      if (isTRUE(self_curated)) shinyjs::disabled(annotation_uploader)
      else annotation_uploader
    },

    # 3. Treatment compound (presets from compound_markers.yaml).
    #    Selecting a compound REPLACES THIS dataset's marker table with its
    #    presets ("(none)" clears the table).
    shiny::selectInput(
      ns("pelsa_compound"),
      label   = "Treatment compound",
      choices = c("(none)" = "", compounds),
      selected = selected_compound
    ),

    # 3b. Add a new compound (empty preset) to compound_markers.yaml.
    shiny::tags$div(
      class = "pelsa-add-compound",
      shiny::textInput(
        ns("pelsa_new_compound"),
        label       = "Add a new compound",
        placeholder = "e.g. AY-9944 (no spaces, ASCII only)"
      ),
      shiny::actionButton(
        ns("pelsa_add_compound_btn"), "Add compound",
        icon = shiny::icon("plus")
      )
    ),

    shiny::tags$hr(),

    # 4. Marker paste box + add button.
    shiny::tags$div(class = "pelsa-section-subhead", "Marker proteins"),
    shiny::textAreaInput(
      ns("pelsa_marker_input"),
      label       = "Add marker proteins (accessions)",
      placeholder = "P12345 Q99999 ... (space/comma/semicolon/newline)",
      rows        = 3
    ),
    shiny::actionButton(ns("pelsa_add_markers"), "Add markers"),

    # 5. Marker reactive table + remove/clear (this dataset's markers).
    shiny::tags$div(
      style = "margin-top: 10px;",
      DT::dataTableOutput(ns("pelsa_marker_table"))
    ),
    shiny::div(
      style = "margin-top: 8px;",
      shiny::actionButton(ns("pelsa_remove_markers"), "Remove selected"),
      shiny::actionButton(ns("pelsa_clear_markers"), "Clear all"),
      shiny::actionButton(
        ns("pelsa_set_default_markers_btn"),
        "Set as default marker list for this compound",
        icon = shiny::icon("floppy-disk")
      )
    )
  )
}

# 6. ORDERING / CONFIG LAYER (purple) of the Setup box: this dataset's
# condition/replicate config placeholder (rendered server-side) + the
# "Apply this dataset's setup to all others" button.
#
# @param ns the module namespacer (session$ns / NS(id)).
# @return a shiny tag (the config section).
# @noRd
pelsa_setup_config_section <- function(ns) {
  shiny::tags$div(
    class = "pelsa-section pelsa-layer-config",
    pelsa_section_head("sliders", "Condition / replicate configuration"),
    shiny::uiOutput(ns("pelsa_perdataset_config")),
    shiny::div(
      style = "margin-top: 10px;",
      shiny::actionButton(
        ns("pelsa_apply_all"),
        "Apply this dataset's setup to all others",
        icon = shiny::icon("clone")
      )
    )
  )
}

# 7. ACTION LAYER (green) of the Setup box: START ANALYSIS (5D). The PRIMARY
# action - visually dominant. Gated by a pre-flight validation checklist; on
# success it runs the compute pipeline (staged withProgress), drives the
# container's analyzed-datasets seam, and redirects to the Summary tab.
# Validation errors render inline below the button.
#
# @param ns the module namespacer (session$ns / NS(id)).
# @return a shiny tag (the action section).
# @noRd
pelsa_setup_action_section <- function(ns) {
  shiny::tags$div(
    class = "pelsa-section pelsa-layer-action",
    pelsa_section_head("play", "Run analysis"),
    shiny::helpText(
      "Validate the setup above and compute every checked dataset, then jump ",
      "to the Summary tab."
    ),
    shiny::actionButton(
      ns("pelsa_start"), "Start Analysis",
      icon  = shiny::icon("play"),
      class = "btn-primary pelsa-start-btn"
    ),
    shiny::uiOutput(ns("pelsa_validation_msgs"))
  )

  # 7b. The per-species UniProt-refresh maintenance layer was removed: feature
  #     annotations are now supplied per dataset via the FASTA + annotation
  #     uploaders in the data-input layer (the external fetch workflow produces
  #     the annotation file). No in-app fetching remains.
}

# The PELSA Setup box markup for ONE dataset (pure tag constructor).
#
# Builds the per-dataset Setup form: a Skip toggle, a per-dataset FASTA uploader,
# a self-curated checkbox, a (greyable) annotation-file uploader, the compound
# selector, the marker paste box + table placeholder, the per-dataset
# condition/replicate config placeholder (rendered server-side), an "Apply this
# dataset's setup to all others" button, and the Start-Analysis action layer.
# Kept pure (a function of its choice vectors + `ns`) so the module renderUI
# stays thin and the markup is testable without a running session. All inputIds
# are namespaced via `ns`. Each logical group is delegated to a private
# per-layer builder above; this function only assembles them.
#
# LEFT  - the run configuration the user fills in top-to-bottom for THIS
#         dataset: data-input layer (species, compound, markers) then the
#         ordering/config layer (condition / replicate config + reorder).
# RIGHT - the action layer (Start Analysis, made dominant) on top.
#
# The Skip toggle sits ABOVE the form. When on, the server greys out the form
# wrapper (id pelsa_setup_form) via the .pelsa-skipped class - state underneath
# is preserved, so un-skipping restores the config.
#
# @param compounds character vector of compound preset names.
# @param ns        the module namespacer (session$ns / NS(id)).
# @param selected_compound the persisted compound for THIS dataset ("" = none).
# @param selected_skip     persisted Skip flag for THIS dataset (TRUE = skip).
# @param self_curated      persisted self-curated flag for THIS dataset. When
#                  TRUE the FASTA is parsed first-token and the annotation
#                  uploader is greyed out (a self-curated database has no UniProt
#                  annotation file).
# @return a shiny tag (the Setup box).
# @noRd
pelsa_setup_box_ui <- function(compounds, ns,
                               selected_compound = "",
                               selected_skip     = FALSE,
                               self_curated      = FALSE) {
  data_section   <- pelsa_setup_data_section(compounds, ns, selected_compound, self_curated)
  config_section <- pelsa_setup_config_section(ns)

  skip_toggle <- shiny::tags$div(
    class = "pelsa-section pelsa-layer-skip",
    shiny::checkboxInput(
      ns("pelsa_skip"),
      label = "Skip PELSA analysis for this dataset",
      value = selected_skip
    )
  )
  setup_form <- shiny::tags$div(
    id = ns("pelsa_setup_form"),
    class = "pelsa-setup-form",
    data_section, config_section
  )

  left_col  <- shiny::tagList(skip_toggle, setup_form)
  right_col <- shiny::tagList(pelsa_setup_action_section(ns))

  add_css_attributes(
    shinydashboardPlus::box(
      width = 12,
      title = "PELSA Setup",
      solidHeader = TRUE,
      status      = "primary",

      shiny::fluidRow(
        shiny::column(6, left_col),
        shiny::column(6, right_col)
      )
    ),
    classes = c("box-no-header", "box-with-tabs")
  )
}

# Positional input-id encoders for the per-dataset config controls. IDs are
# keyed by dataset index i (position in all_omes()) and condition index j, so
# arbitrary dataset/condition strings can never collide or produce illegal ids.
# These are the single source of truth for the bare (un-namespaced) ids; the
# module server ns()-wraps them for UI and uses them bare for update*Input().
# @noRd
pelsa_setup_ids <- function() {
  list(
    condition_col   = function(i)    sprintf("pelsa_condition_col_d%d", i),
    replicate_col   = function(i)    sprintf("pelsa_replicate_col_d%d", i),
    condition_order = function(i)    sprintf("pelsa_condition_order_d%d", i),
    condition_reset = function(i)    sprintf("pelsa_condition_reset_d%d", i),
    replicate_cards = function(i)    sprintf("pelsa_replicate_cards_d%d", i),
    replicate_order = function(i, j) sprintf("pelsa_replicate_order_d%d_c%d", i, j),
    replicate_reset = function(i, j) sprintf("pelsa_replicate_reset_d%d_c%d", i, j)
  )
}
