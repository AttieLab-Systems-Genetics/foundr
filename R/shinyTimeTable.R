#' Shiny Module Input for Times Table
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimeTable
#' @export
#' @importFrom shiny NS tagList
#'
shinyTimeTableInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyTraitOrderInput(ns("shinyOrder")),
    shinyTimeTraitsInput(ns("shinyTimeTraits")))
}

#' Shiny Module Server for Times Plots
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitData,traitSignal,traitStats static objects
#' @param responses possible types of responses
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow h3 observeEvent moduleServer plotOutput
#'             reactive reactiveVal renderPlot renderUI req selectInput
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTimeTable <- function(id, panel_par, main_par,
                       traitData, traitSignal, traitStats,
                       responses = c("value", "cellmean")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # passed inputs:
    #   main_par$height
    #   main_par$facet
    #.  main_par$strains
    # local inputs:
    #   time
    #   time_trait
    #   time_response
    
    # OUTPUTS
    # traitTimeData
    
    # Identify all Time Traits.
    timetrait_all <- timetraitsall(traitSignal)
    
    # Subset Stats to time traits.
    traitStatsInput <- time_trait_subset(traitStats, timetrait_all)
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- shinyTraitOrder("shinyOrder", panel_par, main_par,
                                   traitStatsInput, traitSignal,
                                   customSettings, TRUE)
    
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
                 strains = main_par$strains)
    }, label = "traitTimes")
  })
}