#' Shiny Sex Output for Contrast Plots
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastSex
#' @importFrom shiny column fluidRow NS radioButtons uiOutput
#' @export
#'
shinyContrastSexOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyContrastPlotInput(ns("shinyContrastPlot")),
    
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("sex"))),
      shiny::column(9, shinyContrastPlotUI(ns("shinyContrastPlot")))),

    shinyContrastPlotOutput(ns("shinyContrastPlot")))
}

#' Shiny Sex Server for Contrast Plots
#'
#' @param id identifier
#' @param panel_par,main_par input parameters
#' @param contrastTable reactive data frame
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column moduleServer observeEvent
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
    output$sex <- shiny::renderUI({
      sexes <- c("Both Sexes", "Female", "Male", "Sex Contrast")
      
      shiny::selectInput(ns("sex"), "", sexes)
    })
    
    # RETURNS
    
    # Contrast Trait Plots
    shinyContrastPlot("shinyContrastPlot",
                      input, panel_par, main_par,
                      contrastTable, customSettings, 
                      shiny::reactive("Sex Contrasts"))
  })
}
