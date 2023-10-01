#' Shiny Module UI for Trait Correlations
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyCorPlot
#' @importFrom shiny NS checkboxInput
#' @export
#'
shinyCorPlotUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::checkboxInput(ns("abscor"), "Absolute Correlation?", TRUE)
}

#' Shiny Module UI for Trait Correlations
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyCorPlot
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyCorPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shiny_output"))
}

#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param CorTable reactive data frames
#' @param panel_par,main_par reactive inputs from calling modules
#'
#' @return reactive object
#' @importFrom shiny isTruthy moduleServer plotOutput reactive renderUI
#'             renderPlot req  
#' @export
#'
shinyCorPlot <- function(id, panel_par, main_par, CorTable) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Temporary kludge
    customSettings <- shiny::reactiveValues(dataset = NULL)
    
    # INPUTS
    # calling module inputs
    #   panel_par$mincor:     Minimum Correlation
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
      shiny::req(panel_par$mincor, CorTable())
      
      ggplot_bestcor(
        mutate_datasets(CorTable(), customSettings$dataset, undo = TRUE), 
        panel_par$mincor, shiny::isTruthy(input$abscor))
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
