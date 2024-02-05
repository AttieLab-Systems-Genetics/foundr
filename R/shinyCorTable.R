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
#' @importFrom shiny isTruthy moduleServer observeEvent reactive reactiveVal req
#' @importFrom DT renderDataTable
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
