#' Shiny Module Input for Trait Stats
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitOrder
#' @importFrom shiny NS column fluidRow uiOutput
#' @export
#'
shinyTraitOrderInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(6, shiny::uiOutput(ns("keydataset"))),
    shiny::column(6, shiny::uiOutput(ns("order")))
  )
}

#' Shiny Module UI for Trait Stats
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitOrder
#' @importFrom shiny h3 NS tagList
#' @importFrom DT dataTableOutput
#' @export
#'
shinyTraitOrderUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(  
    shiny::h3("Stats"),
    DT::dataTableOutput(ns("key_stats")))
}

#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param main_par input reactive list
#' @param traitStats,traitSignal static data frame
#' @param customSettings custom settings list
#' @param allDatasets initially select all datasets if `TRUE`
#'
#' @return reactive object
#' @importFrom shiny moduleServer observeEvent reactive reactiveVal renderUI req 
#'             selectInput updateSelectInput
#' @importFrom DT renderDataTable
#' @importFrom plotly plotlyOutput renderPlotly
#' @export
#'
shinyTraitOrder <- function(id, main_par, traitStats, traitSignal = NULL,
                            customSettings = NULL, allDatasets = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    #   main_par$tabpanel
    # shinyTraitOrder inputs
    #   input$keydataset
    #   input$order
    #
    # RETURNS
    # orderstats()
    
    # Key Datasets.
    output$keydataset <- renderUI({
      datasets <- unique(traitStats$dataset)
      choices <- datasets[1]
      if(allDatasets) choices <- datasets
      shiny::selectInput(ns("keydataset"), "Key Datasets:",
                         datasets, choices, multiple = TRUE)
    })
    key_selection <- shiny::reactiveVal(NULL, label = "key_selection")
    shiny::observeEvent(input$keydataset, key_selection(input$keydataset))
    shiny::observeEvent(
      shiny::tagList(shiny::req(key_selection()), main_par$tabpanel),
      {
        selected <- key_selection()
        shiny::updateSelectInput(session, "keydataset", selected = selected)
      })

    # Order Criteria for Trait Names
    output$order <- shiny::renderUI({
      choices <- orderChoices(traitStats)
      shiny::selectInput(ns("order"), "Order traits by", choices)
    })
    order_selection <- shiny::reactiveVal(NULL, label = "order_selection")
    shiny::observeEvent(input$order, order_selection(input$order))
    
    # Table
    output$key_stats <- DT::renderDataTable(
      {
        shiny::req(orderstats())
        
        # Summary gives nice table; use open thresholds to include all.
        summary_strainstats(orderstats(),
                            threshold = c(deviance = 0, p = 1))
      },
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 5))

    orderstats <- shiny::reactive({
      shiny::req(order_selection(), key_stats())
      
      orderTraitStats(order_selection(), key_stats())
    })
    key_stats <- shiny::reactive({
      if(shiny::isTruthy(key_selection())) {
        dplyr::filter(
          traitStats,
          .data$dataset %in% key_selection())
      } else {
        NULL
      }
    })

    ##########################################################
    # Return
    orderstats
  })
}
