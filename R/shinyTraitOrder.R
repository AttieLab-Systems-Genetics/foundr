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

#' Shiny Module Output for Trait Stats
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitOrder
#' @importFrom shiny checkboxInput column fluidRow NS numericInput
#'             plotOutput tagList
#' @export
#'
shinyTraitOrderOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6,
                    shiny::checkboxInput(ns("sex"), "By Sex?", FALSE)),
      shiny::column(6,
                    shiny::numericInput(ns("ntrait"), "Rows:", 20, 5, 100, 5))),
    shiny::plotOutput(ns("plot")))
}

#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param traitStats,traitSignal static data frame
#'
#' @return reactive object
#' @importFrom shiny moduleServer observeEvent reactive reactiveVal renderUI req 
#'             selectInput updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitOrder <- function(id, traitStats, traitSignal = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyTraitOrder inputs
    #   input$keydataset
    #   input$order
    #
    # RETURNS
    # orderstats()
    
    # Key Datasets.
    output$keydataset <- renderUI({
      datasets <- unique(traitStats$dataset)
      shiny::selectInput(ns("keydataset"), "Key Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
    key_selection <- shiny::reactiveVal(NULL, label = "key_selection")
    shiny::observeEvent(input$keydataset, key_selection(input$keydataset))
    shiny::observeEvent(
      shiny::req(key_selection()),
      {
        selected <- key_selection()
        shiny::updateSelectInput(session, "keydataset", selected = selected)
      })

    # Order Criteria for Trait Names
    output$order <- shiny::renderUI({
      p_types <- paste0("p_", unique(traitStats$term))
      choices <- c(p_types, "alphabetical", "original")
      shiny::selectInput(ns("order"), "Order traits by", choices, p_types[1])
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
      shiny::req(order_selection())
      
      if(shiny::isTruthy(key_selection())) {
        orderTraitStats(
          order_selection(), 
          dplyr::filter(
            traitStats,
            .data$dataset %in% key_selection()))
      } else {
        NULL
      }
    })
    
    # Plot
    strainplot <- shiny::reactive({
      shiny::req(orderstats(), input$ntrait)
      
      object <- strain_diff(traitSignal, orderstats())
      ggplot_strain_diff(object, bysex = input$sex, ntrait = input$ntrait)
    })
    output$plot <- shiny::renderPlot({
      shiny::req(strainplot())
      
      print(strainplot())
    })
    
    ##########################################################
    # Return
    orderstats
  })
}
