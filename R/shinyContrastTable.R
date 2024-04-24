#' Shiny Module Input for Trait Panel
#' @return nothing returned
#' @rdname shinyContrastTable
#' @export
shinyContrastTableInput <- function(id) {
  ns <- shiny::NS(id)
  shinyTraitOrderInput(ns("shinyOrder"))
}
#' Shiny Module UI for Trait Panel
#' @return nothing returned
#' @rdname shinyContrastTable
#' @export
shinyContrastTableUI <- function(id) {
  ns <- shiny::NS(id)
  shinyTraitOrderUI(ns("shinyOrder"))
}
#' Shiny Module Server for Contrast Panel
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par parameters from calling modules
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#' @param keepDatatraits keep datatraits if not `NULL`
#'
#' @return reactive object 
#' @importFrom shiny column moduleServer NS observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyContrastTable <- function(id, panel_par, main_par,
                            traitSignal, traitStats,
                            customSettings = NULL, keepDatatraits = shiny::reactive(NULL)) {
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
                                   traitStats, customSettings, keepDatatraits)
    
    ###############################################################
    
    shiny::reactive({
      shiny::req(orderOutput())
      
      conditionContrasts(traitSignal, orderOutput(), 
        termname = orderOutput()$term[1], rawStats = traitStats)
    }, label = "contrastTable")
  })
}
