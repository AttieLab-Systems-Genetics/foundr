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
#' The order of choices depends on `traitStatsArranged()`,
#' which is decided in the `shinyTraitStats` module.
#' 
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param traitStats,traitStatsArranged reactive data frames
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
shinyTraitNames <- function(id, traitStats, traitStatsArranged,
                            multiples = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyTraitNames inputs: (see output$shiny_modcomp below)
    #   input$trait Trait Names
    
    # Select traits
    output$shiny_names <- shiny::renderUI({
      shiny::req(traitNamesArranged())
      inputId <- ifelse(multiples, "Related Traits:", "Key Trait:")
      shiny::selectizeInput(ns("trait"), inputId, choices = NULL,
                            multiple = multiples)
    })
    shiny::observeEvent(
      shiny::req(traitNamesArranged()),
      {
        choices <- traitNamesArranged()
        selected <- NULL
        shiny::updateSelectizeInput(session, "trait", choices = choices,
                                    server = TRUE, selected = selected)
      },
      label = "update_trait")
    
    traitNamesArranged <- shiny::reactive({
      shiny::req(traitStatsArranged())
      unite_datatraits(
        dplyr::distinct(
          traitStatsArranged(),
          .data$dataset, .data$trait))
    },
    label = "traitNamesArranged")
    
    trait_selection <- shiny::reactive(input$trait, label = "trait_selection")
    
    ###############################################
    # vector returned as reactive
    trait_selection
  })
}
