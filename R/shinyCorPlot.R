#' Shiny Module Output for Trait Correlations
#' @return nothing returned
#' @rdname shinyCorPlot
#' @export
shinyCorPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Correlation"),
    shiny::fluidRow( 
      shiny::column(6, shiny::sliderInput(ns("mincor"), "Minimum:", 0, 1, 0.7)),
      shiny::column(6, shiny::checkboxInput(ns("abscor"), "Absolute Correlation?", TRUE))),
    shiny::uiOutput(ns("shiny_output")))
}
#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param CorTable reactive data frames
#' @param panel_par,main_par reactive inputs from calling modules
#' @param customSettings static list of settings
#'
#' @return reactive object
#' @importFrom shiny h3 isTruthy moduleServer NS plotOutput reactive renderUI
#'             renderPlot req tagList uiOutput 
#' @export
#'
shinyCorPlot <- function(id, panel_par, main_par, CorTable,
                         customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # calling module inputs
    #   input$mincor:     Minimum Correlation
    #   main_par$height:    Plot height
    # shinyCorPlot inputs
    #   input$abscor:         Absolute Correlation
    #
    # RETURNS
    # corplot()
    
    output$shiny_output <- shiny::renderUI({
      shiny::req(main_par$height, corplot())
      
      shiny::plotOutput(ns("corplot"),
                        height = paste0(main_par$height, "in"))
    })

    corplot <- shiny::reactive({
      shiny::req(input$mincor, CorTable())
      
      ggplot_bestcor(
        mutate_datasets(CorTable(), customSettings$dataset, undo = TRUE), 
        input$mincor, shiny::isTruthy(input$abscor))
    })
    output$corplot <- shiny::renderPlot({
      shiny::req(corplot())
      
      print(corplot())
    })

    ##############################################################
    # Return
    corplot
  })
}
