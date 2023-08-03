#' Shiny Module Input for Trait Correlations
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyCorTable
#' @importFrom shiny NS
#' @export
#'
shinyCorTableInput <- function(id) {
  ns <- shiny::NS(id)
  
  shinyTraitNamesUI(ns("shinyName"))
}

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
#' @importFrom shiny NS uiOutput
#' @importFrom DT dataTableOutput
#' @export
#'
shinyCorTableOutput <- function(id) {
  ns <- shiny::NS(id)
  
  DT::dataTableOutput(ns("cortable"))
}

#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param main_par,traits_par reactive inputs from calling modules
#' @param traitArranged,traitSignal reactive data frames
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
                          traitArranged, traitSignal, customSettings = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    key_traitOutput <- shinyTraitNames("shinyName", main_par, traitArranged)
    
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
        shiny::req(corobject(), traits_par$mincor, input$corterm)
        
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
      shiny::req(key_traitOutput(), traitSignal(),
                 term_selection(), traits_par$mincor)
      
      # Select rows of traitSignal() with Key Traot or Related Datasets.
      object <- select_data_pairs(traitSignal(), key_traitOutput(),
                                  traits_par$reldataset)
      
      if(!shiny::isTruthy(traits_par$reldataset))
        return(dplyr::distinct(object, dataset, trait))
      
      # Filter by mincor
      dplyr::filter(
        bestcor(
          # Filter to Related Datasets or matching `nameOption()`.
          object,
          key_traitOutput(),
          term_selection()),
        .data$absmax >= traits_par$mincor)
    })
    
    ##############################################################
    # Return
    corobject
  })
}
