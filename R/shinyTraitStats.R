#' Shiny Module UI for Trait Stats
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitStats
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitStatsInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shiny_stats"))
}

#' Shiny Module UI for Trait Stats
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitStats
#' @importFrom shiny NS 
#' @export
#'
shinyTraitStatsUI <- function(id) {
  ns <- shiny::NS(id)
  
  shinyTraitOrderUI(ns("shinyOrder"))
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
  
  shiny::tagList(
    shinyCorPlotOutput(ns("shinyCorPlot")),
    shinyCorTableOutput(ns("shinyCorTable")))
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
#'             reactive renderUI req selectInput sliderInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitStats <- function(id, module_par, traitSignal, traitStats) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyTraitStats inputs
    #   input$reldataset
    #   input$mincor
    #
    # RETURNS
    # list with elements
    #   key_trait = key_traitOutput(): Key Trait
    #   rel_traits = rel_traitsOutput(): Related Traits
    #   key_stats = summary_strainstats(): Table of Key Dataset Stats
    #   corstable = summary_bestcor(): Table of Dataset Correlations
    #   corsplot = plot of Dataset Correlations
    
    # MODULES
    orderOutput <- shinyTraitOrder("shinyOrder", traitStats)
    key_traitOutput <- shinyTraitNames("shinyName", traitStats, orderOutput)
    corTableOutput <- shinyCorTable("shinyCorTable", input,
                                         key_traitOutput, traitSignal)
    corPlotOutput <- shinyCorPlot("shinyCorPlot", input, module_par,
                                       corTableOutput)
    rel_traitsOutput <- shinyTraitNames("shinyNames", traitStats,
                                        corTableOutput, TRUE)

    # Shiny UI Server Side
    output$shiny_stats <- shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shinyTraitOrderInput(ns("shinyOrder"))),
          shiny::column(6, shinyTraitNamesUI(ns("shinyName")))),
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput(ns("reldataset"))),
          shiny::column(6, shinyTraitNamesUI(ns("shinyNames")))),
        
        shiny::fluidRow(
          shiny::column(6, shinyCorTableUI(ns("shinyCorTable"))),
          shiny::column(6, shinyCorPlotUI(ns("shinyCorPlot")))),
        
        shiny::sliderInput(ns("mincor"), "Minimum:", 0, 1, 0.7))
    })
    
    datasets <- shiny::reactive({
      shiny::req(traitStats())
      unique(traitStats()$dataset)
    })
    
    # Related Datasets.
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
        shiny::updateSelectInput(session, "reldataset", choices = choices,
                                 selected = selected)
      })
    
    ##########################################################
    # Return
    shiny::reactive({
      shiny::req(orderOutput(), key_traitOutput())
      
      list(
        key_trait = key_traitOutput(),
        rel_traits = rel_traitsOutput(),
        key_stats = summary_strainstats(orderOutput()),
        corstable = summary_bestcor(corTableOutput(), mincor = 0),
        corsplot = corPlotOutput()
      )
    })
  })
}
