#' Shiny Module Output for Stats Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyStatsPanel
#' @export
#' @importFrom shiny NS uiOutput
#'
shinyStatsPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyContrastPlotInput(ns("shinyContrastPlot")),
    shinyContrastPlotUI(ns("shinyContrastPlot")),
    
    shinyContrastPlotOutput(ns("shinyContrastPlot"))
  )
}

#' Shiny Module Server for Stats Plots
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments from `server()`
#' @param traitStats static data frame
#' @param customSettings list of custom settings
#' @param facet facet on `strain` if `TRUE`
#'
#' @return reactive object for `shinyStatsPanelOutput`
#' @importFrom shiny column fluidRow moduleServer observeEvent plotOutput
#'             reactive renderPlot renderUI req selectInput selectizeInput
#'             tagList uiOutput updateSelectInput sliderInput renderUI
#' @importFrom plotly plotlyOutput ggplotly renderPlotly
#' @importFrom ggplot2 ylim
#' @importFrom rlang .data
#' @export
#'
shinyStatsPanel <- function(id, main_par, traitStats, customSettings = NULL,
                         facet = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # *** allow to flip from SD to p.value
    # *** fix all the kludgey stuff--may need to refactor
    # *** shinyContrastPlot: add scatterplot
    # *** rename as contrast now has generic contrast/stat
    # *** refactor data so Charles can use
    
    # INPUTS
    # Main inputs:
    #   main_par$height
    #   main_par$dataset
    # Stats inputs: (see output$tab_volcano below)
    #   input$term
    #   input$traitnames
    #   input$interact
    #   input$volsd
    #   input$volpval
    
    # MODULES
    # Contrast Trait Plots
    shinyContrastPlot("shinyContrastPlot",
                      input, main_par,
                      traitStatsSelected, customSettings, 
                      shiny::reactive("Stats Contrasts"))
    
    # Dataset selection.
    datasets <- shiny::reactive({
      unique(traitStats$dataset)
    })
    data_selection <- shiny::reactiveVal(NULL, label = "data_selection")
    shiny::observeEvent(main_par$dataset,
                        data_selection(main_par$dataset))

    # Stats for selected datasets.
    traitStatsSelected <- shiny::reactive({
      shiny::req(data_selection())
      
      dplyr::filter(traitStats, .data$dataset %in% data_selection())
    })
  })
}
