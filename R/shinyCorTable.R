#' Shiny Module UI for Trait Correlations
#' @return nothing returned
#' @rdname shinyCorTable
#' @export
shinyCorTableOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Correlation"),
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
#' @importFrom shiny h3 isTruthy moduleServer NS observeEvent reactive reactiveVal
#'             req tagList
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom rlang .data
#' @export
#'
shinyCorTable <- function(id, main_par, traits_par,
                          keyTrait, traitSignal,
                          customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # calling module inputs
    #   traits_par$reldataset: Related Datasets
    #
    # RETURNS
    # corobject()
    
    output$cortable <- DT::renderDataTable(
      {
        shiny::req(keyTrait(), corobject())
        
        summary_bestcor(
          mutate_datasets(
            corobject(),
            customSettings$dataset),
          0.0)
      },
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 5))
    
    corobject <- shiny::reactive({
      if(!shiny::isTruthy(keyTrait()))
        return(NULL)
      
      corTable(keyTrait(), traitSignal,
               "cellmean", 0.0,
               traits_par$reldataset)
    })
    
    ##############################################################
    # Return
    corobject
  })
}
