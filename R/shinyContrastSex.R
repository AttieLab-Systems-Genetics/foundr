#' Shiny Sex Input for Contrast Plots
#'
#' @return nothing returned
#' @rdname shinyContrastSex
#' @export
shinyContrastSexInput <- function(id) {
  ns <- shiny::NS(id)
  shinyContrastPlotInput(ns("shinyContrastPlot"))
}
#' Shiny Sex UI for Contrast Plots
#'
#' @return nothing returned
#' @rdname shinyContrastSex
#' @export
shinyContrastSexUI <- function(id) {
  ns <- shiny::NS(id)
  shinyContrastPlotUI(ns("shinyContrastPlot"))
}
#' Shiny Sex Output for Contrast Plots
#'
#' @return nothing returned
#' @rdname shinyContrastSex
#' @export
shinyContrastSexOutput <- function(id) {
  ns <- shiny::NS(id)
  shinyContrastPlotOutput(ns("shinyContrastPlot"))
}
#' Shiny Sex Server for Contrast Plots
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par input parameters
#' @param contrastTable reactive data frame
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column fluidRow moduleServer NS observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyContrastSex <- function(id, panel_par, main_par,
                            contrastTable, customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyContrastSex inputs
    #   main_par$tabpanel
    #   main_par$height
    #   main_par$strains
    #   panel_par$sex

    # RETURNS
    
    # Contrast Trait Plots
    shinyContrastPlot("shinyContrastPlot",
                      panel_par, main_par,
                      contrastTable, customSettings, 
                      shiny::reactive("Sex Contrasts"))
  })
}
