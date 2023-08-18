#' Shiny Module UI for Trait Correlations
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyCorTable
#' @importFrom shiny NS selectInput
#' @export
#'
shinyCorTableUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::selectInput(ns("corterm"), "Correlation Type",
                     c("cellmean", "signal"), "cellmean")
}

#' Shiny Module UI for Trait Correlations
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyCorTable
#' @importFrom shiny h3 NS tagList
#' @importFrom DT dataTableOutput
#' @export
#'
shinyCorTableOutput <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList( 
    shiny::h3("Correlations"),
    DT::dataTableOutput(ns("cortable")))
}

#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param main_par,traits_par reactive inputs from calling modules
#' @param keyTrait reactive character string
#' @param traitSignal static data frame
#' @param customSettings list of custom settings
#'
#' @return reactive object
#' @importFrom dplyr distinct filter select
#' @importFrom tidyr unite
#' @importFrom shiny isTruthy moduleServer observeEvent reactive reactiveVal req
#' @importFrom DT renderDataTable
#' @importFrom rlang .data
#' @export
#'
shinyCorTable <- function(id, main_par, traits_par,
                          keyTrait, traitSignal,
                          customSettings = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # calling module inputs
    #   traits_par$mincor:     Minimum Correlation
    #   traits_par$reldataset: Related Datasets
    # shinyCorTable inputs
    #   input$corterm:        Correlation Term
    #
    # RETURNS
    # corobject()
    
    output$cortable <- DT::renderDataTable(
      {
        shiny::req(keyTrait(), corobject(), traits_par$mincor,
                   input$corterm)
        
        summary_bestcor(
          mutate_datasets(
            corobject(),
            customSettings$dataset),
          traits_par$mincor)
      },
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 5))
    
    # Wrap input$corterm
    term_selection <- shiny::reactiveVal(NULL, label = "term_selection")
    shiny::observeEvent(input$corterm,
                        term_selection(input$corterm))
    
    corobject <- shiny::reactive({
      shiny::req(term_selection())
      
      if(!shiny::isTruthy(keyTrait()))
        return(NULL)
      
      corTable(keyTrait(), traitSignal,
               term_selection(), 0.0,
               traits_par$reldataset)
    })
    
    ##############################################################
    # Return
    corobject
  })
}
