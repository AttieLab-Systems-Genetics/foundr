#' Shiny Module Input for Time Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimePanel
#' @export
#' @importFrom shiny NS
#'
shinyTimePanelInput <- function(id) {
  ns <- shiny::NS(id)
  
  shinyTimeTableInput(ns("shinyTimeTable"))
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

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(9, shiny::uiOutput(ns("strains"))),
      shiny::column(3, shiny::checkboxInput(ns("facet"), "Facet by strain?", TRUE))),
  
    shinyTimePlotOutput(ns("shinyTimePlot")))
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
    #   main_par$dataset
    #   main_par$height
    #   input$strains
    #   input$facet

    # MODULES
    timeOutput <- shinyTimeTable("shinyTimeTable", input, main_par, 
                                 traitData, traitSignal, traitStats)
    
    shinyTimePlot("shinyTimePlot", input, main_par, traitSignal, timeOutput)
    
    # SERVER-SIDE Inputs
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput(ns("strains"), "Strains",
                                choices = choices, selected = choices, inline = TRUE)
    })
    
    
  })
}