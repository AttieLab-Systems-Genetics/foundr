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
#' Select trait names in one of two modes, depending on the fixed `multiples()`:
#' `FALSE` = only one trait name,
#' `TRUE` =  multiple names.
#' The order of choices depends on `traitStatsArranged()`,
#' which is decided in the `shinyTraitStats` module.
#' 
#' @param input,output,session standard shiny arguments
#' @param module_par,main_par reactive arguments from `foundrServer` and `shinyModules`
#' @param traitModule reactive object with list created by `listof_wgcnamodules`
#' @param modrole reactive with name of module role
#' @param multiples reactive logical for multiple trait names
#'
#' @return reactive vector of trait names
#' 
#' @importFrom shiny observeEvent reactive reactiveVal req
#'             selectizeInput tagList updateSelectizeInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitNames <- function(input, output, session,
                        module_par, main_par,
                        traitData,
                        traitStats,
                        traitStatsArranged,
                        multiples = shiny::reactive(FALSE)) {
  ns <- session$ns
  
  # INPUTS
  # shinyModules inputs: (see shinyModules.R)
  #   main_par$dataset
  #   main_par$response
  # shinyTraitNames inputs: (see output$shiny_modcomp below)
  #   input$trait Trait Names
  
  traitNamesArranged <- shiny::reactive({
    shiny::req(traitStatsArranged())
    unite_datatraits(
      dplyr::distinct(
        traitStatsArranged(),
        .data$dataset, .data$trait))
  })
  
  # Select traits
  output$shiny_names <- shiny::renderUI({
    shiny::req(main_par$dataset, traitNamesArranged(), traitData())
    inputId <- ifelse(multiples(), "Related Traits:", "Key Trait:")
    shiny::selectizeInput(ns("trait"), inputId, choices = NULL,
                          multiple = multiples())
  })
  trait_selection <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$trait, {
    trait_selection(input$trait)
  })
  shiny::observeEvent(
    module_par$order,
    {
      # Use current selection of trait_selection().
      # But make sure they are still in the traitNamesArranged().
      choices <- traitNamesArranged()
      selected <- NULL
      trait_selection(NULL)
      shiny::updateSelectizeInput(session, "trait", choices = choices,
                                  server = TRUE, selected = selected)
      
    })
  shiny::observeEvent(
    shiny::req(main_par$dataset, main_par$response,
               traitData(), traitNamesArranged()),
    {
      # Use current selection of trait_selection().
      # But make sure they are still in the traitNamesArranged().
      selected <- trait_selection()
      choices <- traitNamesArranged()
      selected <- selected[selected %in% choices]
      if(!length(selected))
        selected <- NULL
      shiny::updateSelectizeInput(session, "trait", choices = choices,
                                  server = TRUE, selected = selected)
    })
  

  ###############################################
  # vector returned as reactive
  trait_selection
}
