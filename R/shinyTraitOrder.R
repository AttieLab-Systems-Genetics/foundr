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
#' @importFrom shiny NS column fluidRow uiOutput
#' @importFrom DT dataTableOutput
#' @export
#'
shinyTraitOrderUI <- function(id) {
  ns <- shiny::NS(id)
  
  DT::dataTableOutput(ns("key_stats"))
}

#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param traitStats reactive data frames
#'
#' @return reactive object
#' @importFrom shiny moduleServer observeEvent reactive renderUI req 
#'             selectInput updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitOrder <- function(id, traitStats) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyTraitOrder inputs
    #   input$keydataset
    #   input$order
    #
    # RETURNS
    # orderstats()
    
    datasets <- shiny::reactive({
      shiny::req(traitStats())
      
      unique(traitStats()$dataset)
    })
    
    # Key Datasets.
    output$keydataset <- renderUI({
      shiny::selectInput(ns("keydataset"), "Key Datasets:",
                         datasets(), datasets()[1], multiple = TRUE)
    })
    shiny::observeEvent(
      datasets(),
      {
        selected <- datasets()[1]
        choices <- datasets()
        selected <- selected[selected %in% choices]
        if(!length(selected))
          selected <- choices[1]
        shiny::updateSelectInput(session, "keydataset", choices = choices,
                                 selected = selected)
      })
    
    # Order Criteria for Trait Names
    output$order <- shiny::renderUI({
      shiny::req(traitStats())
      
      p_types <- paste0("p_", unique(traitStats()$term))
      choices <- c(p_types, "alphabetical", "original")
      shiny::selectInput(ns("order"), "Order traits by", choices, p_types[1])
    })
    
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
      shiny::req(input$keydataset, input$order, traitStats())
      orderTraitStats(
        input$order, 
        dplyr::filter(
          traitStats(),
          .data$dataset %in% input$keydataset))
    })
    
    ##########################################################
    # Return
    orderstats
  })
}
