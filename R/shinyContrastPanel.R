#' Shiny Module Input for Trait Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @importFrom shiny NS
#' @export
#'
shinyContrastPanelInput <- function(id) {
  ns <- shiny::NS(id)
  
  shinyContrastsInput(ns("shinyContrasts"))
}

#' Shiny Module Output for Contrast Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @importFrom shiny NS
#' @export
#'
shinyContrastPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shinyContrastsOutput(ns("shinyContrasts"))
}

#' Shiny Module Server for Contrast Panel
#'
#' @param input,output,session standard shiny arguments
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny moduleServer
#' @export
#'
shinyContrastPanel <- function(id, main_par,
                            traitSignal, traitStats,
                            customSettings = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # RETURNS
    #   contrastsOutput
    
    # MODULES
    contrastsOutput <- shinyContrasts("shinyContrasts", main_par,
                                      traitSignal, traitStats,
                                      customSettings)

    ###############################################################
    contrastsOutput
  })
}
