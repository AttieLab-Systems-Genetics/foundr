#' Shiny Module Input for Times Table
#' @return nothing returned
#' @rdname shinyTimeTable
#' @export
shinyTimeTableInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(4, shinyTraitOrderInput(ns("shinyOrder"))), # Order
    shiny::column(8, shinyTimeTraitsInput(ns("shinyTimeTraits")))) # Traits
}
#' Shiny Module UI for Times Table
#' @return nothing returned
#' @rdname shinyTimeTable
#' @export
shinyTimeTableUI <- function(id) {
  ns <- shiny::NS(id)
  shinyTimeTraitsUI(ns("shinyTimeTraits")) # Time Unit
}
#' Shiny Module Output for Times Table
#' @return nothing returned
#' @rdname shinyTimeTable
#' @export
shinyTimeTableOutput <- function(id) {
  ns <- shiny::NS(id)
  shinyTimeTraitsOutput(ns("shinyTimeTraits")) # Response
}
#' Shiny Module Server for Times Plots
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitData,traitSignal,traitStats static objects
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow h3 observeEvent moduleServer NS plotOutput
#'             reactive reactiveVal renderPlot renderUI req selectInput
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom shiny column fluidRow NS
#' @export
shinyTimeTable <- function(id, panel_par, main_par,
                       traitData, traitSignal, traitStats) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # OUTPUTS
    # traitTimeData
    
    # Identify all Time Traits.
    timetrait_all <- timetraitsall(traitSignal)
    
    # Subset Stats to time traits.
    traitStatsInput <- time_trait_subset(traitStats, timetrait_all)
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- shinyTraitOrder("shinyOrder", panel_par, main_par,
                                   traitStatsInput, customSettings)
    
    # Identify Time Traits.
    timeTraitsOutput <- shinyTimeTraits("shinyTimeTraits", panel_par, main_par,
                                        traitSignal, orderOutput)
    
    ###############################################################
    
    shiny::reactive({
      shiny::req(timeTraitsOutput$traits, timeTraitsOutput$time,
                 timeTraitsOutput$response, orderOutput())
      
      traitTimes(traitData, traitSignal, traitStats,
                 timeTraitsOutput$traits, timeTraitsOutput$time,
                 timeTraitsOutput$response,
                 strains = panel_par$strains)
    }, label = "traitTimes")
  })
}