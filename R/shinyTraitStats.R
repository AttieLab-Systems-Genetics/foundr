#' Shiny Module UI for Trait Stats
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitStats
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitStatsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shiny_stats"))
}

#' Shiny Module Server for Dendro Plots
#'
#' @param input,output,session standard shiny arguments
#' @param traitSignal,traitStats reactive data frames
#'
#' @return reactive object for `shinyTraitStatsUI`
#' @importFrom shiny callModule reactive renderUI req
#'             selectInput tagList uiOutput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitStats <- function(input, output, session,
                            traitSignal, traitStats) {
  ns <- session$ns
  
  # INPUTS
  # shinyTraitStats inputs
  #   input$keydataset
  #   input$order
  #   input$reldataset

  # MODULES
  nameOutput <- shiny::callModule(
    shinyTraitNames, "shinyName",
    input, traitStats, orderstats)
  
  namesOutput <- shiny::callModule(
    shinyTraitNames, "shinyNames",
    input, traitStats, corobject,
    shiny::reactive(TRUE))
  
  # Shiny UI Server Side
  output$shiny_stats <- shiny::renderUI({
    shiny::tagList(
      shiny::uiOutput(ns("keydataset")),
      shiny::uiOutput(ns("order")),
      shinyTraitNamesUI(ns("shinyName")),
      shiny::uiOutput(ns("reldataset")),
      shinyTraitNamesUI(ns("shinyNames"))
    )
  })
  
  datasets <- shiny::reactive({
    shiny::req(traitStats())
    unique(traitStats()$dataset)
  })
  
  # Datasets. More work to allow multiple datasets.
  output$keydataset <- renderUI({
    shiny::selectInput(ns("keydataset"), "Key Datasets:",
                       datasets(), input$keydataset)
  })
  output$reldataset <- renderUI({
    shiny::selectInput(ns("reldataset"), "Related Datasets:",
                       datasets(), input$reldataset)
  })
  
  # Order Criteria for Trait Names
  output$order <- shiny::renderUI({
    p_types <- paste0("p_", unique(traitStats()$term))
    choices <- c(p_types, "alphabetical", "original")
    shiny::selectInput(ns("order"), "Order traits by", choices, p_types[1])
  })
  
  orderstats <- shiny::reactive({
    shiny::req(input$keydataset, input$order, traitStats())
    orderTraitStats(
      input$order, 
      dplyr::filter(
        traitStats(),
        .data$dataset %in% input$keydataset))
  })
  corobject <- shiny::reactive({
    shiny::req(input$keydataset, traitSignal())
    # input$corterm = "cellmean" -- see shinyCorrelation.R
    bestcor(
      # Filter to Related Datasets or matching `nameOption()`.
      dplyr::select(
        dplyr::filter(
          tidyr::unite(
            traitSignal(),
            datatraits,
            .data$dataset, .data$trait,
            sep = ": ", remove = FALSE),
          (.data$datatraits %in% nameOutput()) |
            (.data$dataset %in% input$reldataset)),
        -.data$datatraits),
      nameOutput(),
      "cellmean")
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
