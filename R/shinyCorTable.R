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
#' @param traitArranged,traitSignal reactive data frames
#' @param stats_par reactive inputs from calling modules
#'
#' @return reactive object
#' @importFrom dplyr filter select
#' @importFrom tidyr unite
#' @importFrom shiny moduleServer reactive req
#' @importFrom DT renderDataTable
#' @importFrom rlang .data
#' @export
#'
shinyCorTable <- function(id, stats_par, traitArranged, traitSignal) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    key_traitOutput <- shinyTraitNames("shinyName", traitArranged)
    
    # Temporary kludge
    customSettings <- shiny::reactiveValues(dataset = NULL)
    
    # INPUTS
    # calling module inputs
    #   stats_par$mincor:     Minimum Correlation
    #   stats_par$reldataset: Related Datasets
    # shinyCorTable inputs
    #   input$corterm:        Correlation Term
    #
    # RETURNS
    # corobject()
    
    output$cortable <- DT::renderDataTable(
      {
        shiny::req(corobject(), stats_par$mincor, input$corterm)
        
        summary_bestcor(
          mutate_datasets(
            corobject(),
            customSettings$dataset),
          stats_par$mincor)
      },
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 5))
    
    corobject <- shiny::reactive({
      shiny::req(key_traitOutput(), traitSignal(),
                 input$corterm, stats_par$mincor)
      
      if(!shiny::isTruthy(stats_par$reldataset))
        return(NULL)
      
      # Filter by mincor
      dplyr::filter(
        bestcor(
          # Filter to Related Datasets or matching `nameOption()`.
          dplyr::select(
            dplyr::filter(
              tidyr::unite(
                traitSignal(),
                datatraits,
                .data$dataset, .data$trait,
                sep = ": ", remove = FALSE),
              (.data$datatraits %in% key_traitOutput()) |
                (.data$dataset %in% stats_par$reldataset)),
            -.data$datatraits),
          key_traitOutput(),
          input$corterm),
        .data$absmax >= stats_par$mincor)
    })
    
    ##############################################################
    # Return
    corobject
  })
}
