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
#' @importFrom shiny callModule column fluidRow observeEvent reactive
#'             reactiveVal renderUI req selectInput tagList uiOutput
#'             updateSelectInput
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
    traitStats, orderstats)
  
  namesOutput <- shiny::callModule(
    shinyTraitNames, "shinyNames",
    traitStats, corobject, shiny::reactive(TRUE))
  
  # Shiny UI Server Side
  output$shiny_stats <- shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("keydataset"))),
        shiny::column(3, shiny::uiOutput(ns("order"))),
        shiny::column(6, shinyTraitNamesUI(ns("shinyName")))),
      shiny::fluidRow(
        shiny::column(6, shiny::uiOutput(ns("reldataset"))),
        shiny::column(6, shinyTraitNamesUI(ns("shinyNames")))))
  })
  
  datasets <- shiny::reactive({
    shiny::req(traitStats())
    unique(traitStats()$dataset)
  })
  
  # Datasets. More work to allow multiple datasets.
  output$keydataset <- renderUI({
    shiny::selectInput(ns("keydataset"), "Key Datasets:",
                       datasets(), datasets()[1], multiple = TRUE)
  })
  key_selection <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$keydataset, {
    key_selection(input$keydataset)
  })
  shiny::observeEvent(
    datasets(),
    {
      key_selection(datasets()[1])
      selected <- key_selection()
      choices <- datasets()
      selected <- selected[selected %in% choices]
      if(!length(selected))
        selected <- choices[1]
      shiny::updateSelectInput(session, "keydataset", choices = choices,
                               selected = selected)
    })
  output$reldataset <- renderUI({
    shiny::selectInput(ns("reldataset"), "Related Datasets:",
                       datasets(), datasets()[1], multiple = TRUE)
  })
  rel_selection <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$reldataset, {
    rel_selection(input$reldataset)
  })
  shiny::observeEvent(
    datasets(),
    {
      rel_selection(datasets()[1])
      selected <- rel_selection()
      choices <- datasets()
      selected <- selected[selected %in% choices]
      if(!length(selected))
        selected <- choices[1]
      shiny::updateSelectInput(session, "reldataset", choices = choices,
                               selected = selected)
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
    shiny::req(nameOutput(), traitSignal())
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
      cors = summary_bestcor(corobject(), mincor = 0)
    )
  })
}
