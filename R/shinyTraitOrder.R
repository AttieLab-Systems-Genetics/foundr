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
  
  shiny::uiOutput(ns("plot"))
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
  moduleServer(id, function(input, output, session) {
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
    
    # Plot
    contrasts <- shiny::reactive({
      shiny::req(orderstats())
      
      conditionContrasts(traitSignal, orderstats(),
                         rawStats = key_stats())
    }, label = "contrasts")
    contrastVolcano <- shiny::reactive({
      shiny::req(contrasts(), sextype())
      
      plot(contrasts(), bysex = sextype(), volcano = TRUE,
           interact = shiny::isTruthy(input$interact))
    }, label = "contrastVolcano")
    contrastPlot <- shiny::reactive({
      shiny::req(contrasts(), input$ntrait, sextype())
      
      plot(contrasts(), bysex = sextype(), ntrait = input$ntrait)
    }, label = "contrastPlot")
    
    sexes <- c("Both Sexes", "Female", "Male", "Sex Contrast")
    names(sexes) <- c("F+M","F","M","F-M")
    sextype <- shiny::reactive({
      names(sexes)[match(shiny::req(input$sex), sexes)]
    }, label = "sextype")
    
    output$plot <- shiny::renderUI({
      shiny::req(contrasts())
      
      condition <- customSettings$condition
      if(shiny::isTruthy(condition))
        condition <- stringr::str_to_title(condition)
      else
        condition <- "Condition"

      shiny::tagList(
        shiny::h3(paste(condition, "Contrasts")),
        shiny::fluidRow(
          shiny::column(6,
                        shiny::selectInput(ns("sex"), "Sex:",
                                           as.vector(sexes))),
          shiny::column(3,
                        shiny::numericInput(ns("ntrait"), "Rows:",
                                            20, 5, 100, 5)),
          shiny::column(3,
                        shiny::checkboxInput(ns("interact"), "Interactive?"))),
        shiny::uiOutput(ns("conplot")),
        shiny::uiOutput(ns("convolc")))
    })
    output$convolc <- shiny::renderUI({
      if(shiny::isTruthy(input$interact)) {
        plotly::renderPlotly(shiny::req(contrastVolcano()))
      } else {
        shiny::renderPlot(print(shiny::req(contrastVolcano())))
      }
    })
    output$conplot <- shiny::renderUI({
      if(shiny::isTruthy(input$interact)) {
        plotly::renderPlotly(shiny::req(contrastPlot()))
      } else {
        shiny::renderPlot(print(shiny::req(contrastPlot())))
      }
    })
    
    ##########################################################
    # Return
    orderstats
  })
}
