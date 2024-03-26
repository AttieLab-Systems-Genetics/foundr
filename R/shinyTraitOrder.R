#' Shiny Module Input for Trait Stats
#' @rdname shinyTraitOrder
#' @export
shinyTraitOrderInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("order"))
}
#' Shiny Module UI for Trait Stats
#' @rdname shinyTraitOrder
#' @export
shinyTraitOrderUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(  
    shiny::h3("Stats"),
    DT::dataTableOutput(ns("key_stats")))
}
#' Shiny Module Server for Trait Stats
#'
#' 
#' @param id identifier for shiny reactive
#' @param panel_par,main_par input reactive list
#' @param traitStats static data frame
#' @param customSettings custom settings list
#' @param keepDatatraits keep datatraits if not `NULL`
#'
#' @return reactive object
#' @importFrom shiny column fluidRow h3 moduleServer NS observeEvent reactive 
#'             reactiveVal renderUI req selectInput tagList uiOutput updateSelectInput
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom plotly plotlyOutput renderPlotly
#' @export
#'
shinyTraitOrder <- function(id, panel_par, main_par,
                            traitStats,
                            customSettings = NULL, keepDatatraits = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    #   main_par$tabpanel
    # shinyTraitOrder inputs
    #   input$keydataset
    #   input$order
    #
    # RETURNS
    #   orderstats()
    
    # Key Datasets.
#    output$keydataset <- renderUI({
#      datasets <- unique(traitStats$dataset)
#      choices <- datasets[1]
#      if(allDatasets) choices <- datasets
#      shiny::selectInput(ns("keydataset"), "Key Datasets:",
#                         datasets, choices, multiple = TRUE)
#    })
    key_selection <- shiny::reactiveVal(NULL, label = "key_selection")
    shiny::observeEvent(main_par$dataset, key_selection(main_par$dataset))
#    shiny::observeEvent(input$keydataset, key_selection(input$keydataset))
#    shiny::observeEvent(
#      shiny::tagList(shiny::req(key_selection()), main_par$tabpanel,
#                     panel_par$contrast),
#      {
#        selected <- key_selection()
#        shiny::updateSelectInput(session, "keydataset", selected = selected)
#      })

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
      if(shiny::isTruthy(keepDatatraits())) {
        dplyr::select(
          dplyr::filter(
            tidyr::unite(
              traitStats,
              datatraits, dataset, trait, sep = ": ", remove = FALSE),
            datatraits %in% keepDatatraits()),
          -datatraits)
      } else {
        if(shiny::isTruthy(key_selection())) {
          dplyr::filter(
            traitStats,
            .data$dataset %in% key_selection())
        } else {
          NULL
        }
      }
    })

    ##########################################################
    # Return
    orderstats
  })
}
