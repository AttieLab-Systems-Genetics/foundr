#' Shiny Module UI for Trait Stats
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitStats
#' @export
#'
shinyTraitStatsUI <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("shiny_stats"))
}

#' Shiny Module Server for Dendro Plots
#'
#' @param input,output,session standard shiny arguments
#' @param traitData,traitSignal,traitStats reactive data frames
#' @param module_par reactive list from calling module
#'
#' @return reactive object for `shinyTraitStatsUI`
#' @importFrom shiny reactive reactiveValues renderUI req
#'             tagList uiOutput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitStats <- function(input, output, session,
                            module_par,
                            traitData, traitSignal, traitStats) {
  ns <- session$ns
  
  # INPUTS
  #   module_par$dataset
  #   module_par$response
  # shinyTraitStats inputs: (see output$shiny_stats)

  # MODULES
  nameOutput <- shiny::callModule(
    foundr::shinyTraitNames, "shinyName",
    input, module_par,
    traitData, traitStats, orderstats)
  
  namesOutput <- shiny::callModule(
    foundr::shinyTraitNames, "shinyNames",
    input, module_par,
    traitData, traitStats, corobject,
    shiny::reactive(TRUE))
  
  # Shiny UI Server Side
  output$shiny_stats <- shiny::renderUI({
    shiny::tagList(
      shiny::uiOutput(ns("order")),
      shinyTraitNamesUI(ns("shinyName")),
      shinyTraitNamesUI(ns("shinyNames"))
    )
  })
  
  # Order Criteria for Trait Names
  output$order <- shiny::renderUI({
    p_types <- paste0("p_", unique(traitStats()$term))
    choices <- c(p_types, "alphabetical", "original")
    shiny::selectInput(ns("order"), "Order traits by", choices, p_types[1])
  })
  
  orderstats <- shiny::reactive({
    shiny::req(input$order, traitStats())
    orderTraitStats(input$order, traitStats())
  })
  corobject <- shiny::reactive({
    shiny::req(traitSignal())
    bestcor(traitSignal(),
            nameOutput(),
            "cellmean") # input$corterm -- see shinyCorrelation
  })

  # Arrange Trait Stats (order traits for menu and summary table)
  shiny::reactive({
    shiny::req(orderstats(), nameOutput())
    list(
      proband = nameOutput(),
      traits = namesOutput(),
      orders = summary_strainstats(orderstats()),
      cors = summary_bestcor(corobject())
    )
  })
}
