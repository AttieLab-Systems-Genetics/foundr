# NOT USED YET

#' Shiny Module Input for Biplots
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyBiplot
#' @importFrom shiny column fluidRow NS radioButtons uiOutput
#' @export
#'
shinyBiplotInput <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(ns("strain"))
}
#' Shiny Module Output for Biplots
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyBiplot
#' @importFrom shiny column fluidRow NS radioButtons uiOutput
#' @export
#'
shinyBiplotOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("conbiplot"))
}
#' Shiny Module Server for Contrast Plots
#'
#' @param id identifier
#' @param sex_par,main_par input parameters
#' @param contrastTable reactive data frame
#' @param customSettings list of custom settings
#' @param modTitle character string title for section
#'
#' @return reactive object 
#' @importFrom shiny column moduleServer observeEvent
#'             reactive reactiveVal renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyBiplot <- function(id, call_par, sex_par, main_par,
                            contrastTable, info, threshold, ord_selection) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyBiplot inputs
    #   main_par$tabpanel
    #   main_par$height
    # RETURNS
    #   contrastBiPlot
    
    # Server-side Inputs
    output$strain <- shiny::renderUI({
      shiny::req(contrastTable(), info())
      rows <- unique(contrastTable()[[info()$row]])
      
      shiny::selectInput(ns("strain"), "Vector Highlight", c("NONE", rows))
    })
    
    contrastBiPlot <- shiny::reactive({
      shiny::req(contrastTable())
      
      ggplot_conditionContrasts(
        contrastTable(), bysex = sex_par$sex,
        ordername = ord_selection(),
        plottype = "biplot",
        threshold = threshold(),
        strain = input$strain,
        interact = shiny::isTruthy(call_par$interact))
    }, label = "contrastBiPlot")

    output$conbiplot <- shiny::renderUI({
      if(shiny::isTruthy(call_par$interact)) {
        shiny::tagList(
          shiny::renderText("Rays disappear if interactive."),
          shiny::renderPlot(print(shiny::req(contrastBiPlot()))),
          plotly::renderPlotly(shiny::req(contrastBiPlot())))
      } else {
        shiny::renderPlot(print(shiny::req(contrastBiPlot())))
      }
    })
    #############################################
    contrastBiPlot
  })
}
