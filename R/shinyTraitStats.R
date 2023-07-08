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

#' Shiny Module Output for Trait Stats
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitStats
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitStatsOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shinyTraitCorsOutput(ns("shinyCors"))
}

#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param module_par reactive input from calling module
#' @param traitSignal,traitStats reactive data frames
#'
#' @return reactive object for `shinyTraitStatsUI`
#' @importFrom shiny callModule column fluidRow moduleServer observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitStats <- function(id, module_par, traitSignal, traitStats) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyTraitStats inputs
    #   input$keydataset
    #   input$order
    #   input$reldataset
    #
    # RETURNS
    # list with elements
    #   key_trait = key_traitOutput(): Key Trait
    #   rel_traits = rel_traitsOutput(): Related Traits
    #   key_stats = summary_strainstats(): Table of Key Dataset Stats
    #   corstable = summary_bestcor(): Table of Dataset Correlations
    #   corsplot = plot of Dataset Correlations
    
    # MODULES
    key_traitOutput <- shinyTraitNames("shinyName", traitStats, orderstats)
    corsOutput <- shinyTraitCors("shinyCors", input, module_par,
                                 key_traitOutput, traitSignal)
    rel_traitsOutput <- shinyTraitNames("shinyNames", traitStats,
                                        shiny::reactive({
                                          corsOutput()$table
                                        }), TRUE)
    
    # Shiny UI Server Side
    output$shiny_stats <- shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(3, shiny::uiOutput(ns("keydataset"))),
          shiny::column(3, shiny::uiOutput(ns("order"))),
          shiny::column(6, shinyTraitNamesUI(ns("shinyName")))),
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput(ns("reldataset"))),
          shiny::column(6, shinyTraitNamesUI(ns("shinyNames")))),
        
        shinyTraitCorsUI(ns("shinyCors")))
    })
    
    datasets <- shiny::reactive({
      shiny::req(traitStats())
      unique(traitStats()$dataset)
    })
    
    # Key and Related Datasets.
    output$keydataset <- renderUI({
      shiny::selectInput(ns("keydataset"), "Key Datasets:",
                         datasets(), datasets()[1], multiple = TRUE)
    })
    output$reldataset <- renderUI({
      shiny::selectInput(ns("reldataset"), "Related Datasets:",
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

    ##########################################################
    # Return
    shiny::reactive({
      shiny::req(orderstats(), key_traitOutput())
      
      list(
        key_trait = key_traitOutput(),
        rel_traits = rel_traitsOutput(),
        key_stats = summary_strainstats(orderstats()),
        corstable = summary_bestcor(corsOutput()$table, mincor = 0),
        corsplot = corsOutput()$plot
      )
    })
  })
}
