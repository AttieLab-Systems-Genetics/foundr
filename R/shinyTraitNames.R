#' Shiny Module UI for Trait Names
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitNames
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitNamesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shiny_names"))
}

#' Shiny Module Server for Trait Names
#'
#' Select trait names in one of two modes, depending on the fixed `multiples`:
#' `FALSE` = only one trait name,
#' `TRUE` =  multiple names.
#' The order of choices depends on `traitArranged()`.
#' 
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitArranged reactive data frames
#' @param multiples fixed logical for multiple trait names
#'
#' @return reactive vector of trait names
#' 
#' @importFrom shiny moduleServer observeEvent reactive req
#'             selectizeInput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#' @export
#'
shinyTraitNames <- function(id, main_par, traitArranged, multiples = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyTraitNames inputs: (see output$shiny_modcomp below)
    #   input$trait Trait Names
    
    # Select traits
    output$shiny_names <- shiny::renderUI({
      inputId <- ifelse(multiples, "Related Traits:", "Key Trait:")
      shiny::selectizeInput(ns("trait"), inputId, choices = NULL,
                            multiple = multiples)
    })
    shiny::observeEvent(
      traitArranged(),
      {
        choices <- traitNamesArranged()
        selected <- trait_selection()
        if(!(all(selected) %in% choices))
          selected <- NULL
        shiny::updateSelectizeInput(session, "trait", choices = choices,
                                    server = TRUE, selected = selected)
      },
      ignoreNULL = FALSE, label = "update_trait")
    trait_selection <- shiny::reactiveVal(NULL, label = "trait_selection")
    shiny::observeEvent(input$trait, trait_selection(input$trait))
    shiny::observeEvent(traitArranged(), trait_selection(NULL))
    
    traitNamesArranged <- shiny::reactive({
      if(shiny::isTruthy(traitArranged())) {
        unite_datatraits(
          dplyr::distinct(
            traitArranged(),
            .data$dataset, .data$trait))
      } else {
        NULL
      }
    },
    label = "traitNamesArranged")
    
    ###############################################
    # vector returned as reactive
    trait_selection
  })
}
