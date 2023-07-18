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
#' @param stats_par,module_par reactive inputs from calling modules
#'
#' @return reactive object
#' @importFrom shiny callModule isTruthy moduleServer 
#'             plotOutput reactive renderUI renderPlot req  
#' @export
#'
shinyCorPlot <- function(id, stats_par, module_par, CorTable) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Temporary kludge
    customSettings <- shiny::reactiveValues(dataset = NULL)
    
    # INPUTS
    # calling module inputs
    #   stats_par$mincor:     Minimum Correlation
    #   module_par$height:    Plot height
    # shinyCorPlot inputs
    #   input$abscor:         Absolute Correlation
    #
    # RETURNS
    # corplot()
    
    output$shiny_output <- shiny::renderUI({
      shiny::req(module_par$height)
      
      shiny::plotOutput(ns("corplot"),
                        height = paste0(module_par$height, "in"))
    })

    corplot <- shiny::reactive({
      shiny::req(stats_par$mincor)
      
      ggplot_bestcor(
        mutate_datasets(CorTable(), customSettings$dataset, undo = TRUE), 
        stats_par$mincor, shiny::isTruthy(input$abscor))
    })
    output$corplot <- shiny::renderPlot({
      print(corplot())
    })

    ##############################################################
    # Return
    corplot
  })
}
