#' Shiny Module Input for Contrasts over Time
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastTime
#' @export
#' @importFrom shiny NS
#'
shinyContrastTimeInput <- function(id) {
  ns <- shiny::NS(id)
  
  shinyTimeTraitsInput(ns("shinyTimeTraits"))
  
}
#' Shiny Module Server for Contrasts over Time
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny h3 isTruthy moduleServer reactive renderText renderUI
#'             tagList
#' @importFrom stringr str_to_title
#' @export
#'
shinyContrastTime <- function(id, panel_par, main_par,
                              traitSignal, traitStats, contrastTable,
                              customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # RETURNS
    #   contrastOutput
    
    # MODULES
    # Identify Time Traits.
    timeTraitsOutput <- shinyTimeTraits("shinyTimeTraits", panel_par, main_par,
                                        traitSignal, contrastTable)
    
    # Contrast Time Signal
    shiny::reactive({
      shiny::req(contrastTable(), timeTraitsOutput$traits, panel_par$strains,
                 timeTraitsOutput$response, timeTraitsOutput$time)
      
      # Convert `contrastTable()` to a `Signal` style data frame.
      contrastSignal <- contrast2signal(contrastTable())
      
      traitTimes(contrastSignal, contrastSignal, traitStats,
                 timeTraitsOutput$traits, timeTraitsOutput$time,
                 timeTraitsOutput$response, strains = panel_par$strains)
    }, label = "contrastTime")
  })
}
