#' Shiny Module UI for Correlation Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyCorrelation
#' @export
#'

shinyCorrelationUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("shiny_correlation"))
  )
}

#' Shiny Module Server for Correlation Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer`
#' @param traitSignal reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyCorrelationUI`
#' @importFrom shiny column fluidRow observeEvent plotOutput reactive
#'             reactiveVal renderPlot renderUI req selectInput selectizeInput
#'             tagList uiOutput updateSelectizeInput sliderInput renderUI
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom plotly plotlyOutput ggplotly renderPlotly
#' @importFrom ggplot2 ylim
#' @export
#'
shinyCorrelation <- function(input, output, session,
                       main_par,
                       traitSignal) {
  ns <- session$ns

  # INPUTS
  # Main inputs: (see shinyapp.R)
  #   main_par$dataset
  #   main_par$response
  #   main_par$height
  # Correlation inputs: (see output$tab_correlation below)
  #   input$corterm
  #   input$mincor

  # Placeholders for reactive inputs.
  customSettings <- shiny::reactiveValues(
    dataset = NULL)
  
  trait_selection <- shiny::reactive(NULL)
  
  datasets <- shiny::reactive({
    if(shiny::isTruthy(main_par$dataset))
      main_par$dataset
    else
      unique(traitSignal()$dataset)
  })

  # Shiny Correlation UI
  output$shiny_correlation <- shiny::renderUI({
    shiny::req(main_par$height)
    
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(ns("corterm"), "Correlation Type",
                             c("cellmean", "signal"))),
        shiny::column(
          6,
          shiny::checkboxInput(ns("abscor"), "Absolute Correlation?", TRUE)
        )),
      shiny::sliderInput(ns("mincor"), "Minimum:", 0, 1, 0.7),
      
      shiny::plotOutput(ns("corplot"), height = paste0(main_par$height, "in")),
      DT::dataTableOutput(ns("cortable")))
  })

  # Correlation object. Also created for use with traitnames.
  corobject <- shiny::reactive({
    shiny::req(input$corterm, traitSignal())
    bestcor(traitSignal(),
            trait_selection(),
            term = input$corterm) 
  })

  corplot <- shiny::reactive({
    shiny::req(input$mincor, input$abscor, corobject())
    ggplot_bestcor(
      mutate_datasets(corobject(), customSettings$dataset, undo = TRUE), 
      input$mincor, input$abscor)
  })
  output$corplot <- shiny::renderPlot({
    print(corplot())
  })
  cortable <- shiny::reactive({
    shiny::req(corobject(), input$mincor, input$corterm)
    summary_bestcor(
      mutate_datasets(corobject(), customSettings$dataset),
      input$mincor)
  })
  output$cortable <- DT::renderDataTable(
    cortable(),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))

  # List returned
  reactive({
    shiny::req(corplot(), cortable(), datasets())
    list(
      plot = print(corplot()),
      table = cortable(),
      traits = datasets())
  })
}


