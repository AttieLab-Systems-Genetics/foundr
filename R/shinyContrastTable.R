#' Shiny Module Input for Trait Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastTable
#' @importFrom shiny NS
#' @export
#'
shinyContrastTableInput <- function(id) {
  ns <- shiny::NS(id)

  shinyTraitOrderInput(ns("shinyOrder"))
}

#' Shiny Module Server for Contrast Panel
#'
#' @param id identifier
#' @param panel_par,main_par parameters from calling modules
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#' @param allDatasets initially select all datasets if `TRUE`
#'
#' @return reactive object 
#' @importFrom shiny column downloadHandler moduleServer observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyContrastTable <- function(id, panel_par, main_par,
                            traitSignal, traitStats,
                            customSettings = NULL, allDatasets = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyContrastTable inputs
    #   main_par$tabpanel
    # RETURNS
    #   contrastTable()
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- shinyTraitOrder("shinyOrder", panel_par, main_par,
                                   traitStats, traitSignal,
                                   customSettings, allDatasets)
    
    ###############################################################
    
    shiny::reactive({
      shiny::req(orderOutput())
      
      conditionContrasts(traitSignal, orderOutput(), 
        termname = orderOutput()$term[1], rawStats = traitStats)
    }, label = "contrastTable")
  })
}
