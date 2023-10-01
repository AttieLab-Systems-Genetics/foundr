#' Shiny Module Input for Time Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimePanel
#' @export
#' @importFrom shiny NS tagList
#'
shinyTimePanelInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyTimeTableInput(ns("shinyTimeTable")),
    shinyTimePlotInput(ns("shinyTimePlot")))
}

#' Shiny Module Output for Times Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimePanel
#' @export
#' @importFrom shiny NS
#'
shinyTimePanelOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shinyTimePlotOutput(ns("shinyTimePlot"))
}

#' Shiny Module Server for Times Plots
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitData,traitSignal,traitStats static objects
#'
#' @return nothing returned
#' @importFrom shiny moduleServer
#' @export
#'
shinyTimePanel <- function(id, main_par,
                            traitData, traitSignal, traitStats) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # passed inputs:
    #   main_par$height
    #   main_par$facet
    #   main_par$strains

    # MODULES
    timeOutput <- shinyTimeTable("shinyTimeTable", input, main_par, 
                                 traitData, traitSignal, traitStats)
    
    shinyTimePlot("shinyTimePlot", main_par, traitSignal, timeOutput)
  })
}