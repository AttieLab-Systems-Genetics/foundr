#' Shiny Module UI for Dendro Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyDendro
#' @export
#'
shinyDendroUI <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("shiny_dendro"))
}

#' Shiny Module Server for Dendro Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer` 
#' @param traitModule reactive object with list created by `listof_wgcnamodules`
#'
#' @return reactive object for `shinyDendroUI`
#' @importFrom shiny plotOutput reactive renderPlot renderUI req
#'             tagList uiOutput
#' @importFrom DT renderDataTable
#' @export
#'
shinyDendro <- function(input, output, session,
                        main_par, traitModule) {
  ns <- session$ns
  
  # INPUTS
  # Main inputs: (see shinyapp.R)
  #   main_par$height (see shinyapp.R::foundrUI sidebarPanel)
  # Dendro inputs: (see shinyModules.R)
  #   input$dataset
  #   input$response

  # OUTPUTS
  # output$dendro dendro plot and table
  
  # RETURNS
  # list with
  #   plot (see dendroplots() below)
  #   table (see dendrotable() below)
  
  datasets <- shiny::reactive(
    c("LivMet","PlaMet0","PlaMet120","Metab"))
  responses <- shiny::reactive(
    c("value","cellmean","signal","rest","noise"))
  
  output$shiny_dendro <- shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("dataset"), "Dataset:",
            datasets())),
        shiny::column(
          6,
          shiny::selectInput(
            ns("response"), "Response:",
            responses()))),
      
      shiny::uiOutput(ns("dendro")))
  })
    
  # Dendrogram
  output$dendro <- shiny::renderUI({
    shiny::req(main_par$height, input$dataset, input$response)
    shiny::tagList(
      shiny::plotOutput(ns("dendroplot"),
                        height = paste0(main_par$height, "in")),

      DT::renderDataTable(
        dendrotable(),
        escape = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)))
  })
  
  dendrotable <- shiny::reactive({
    shiny::req(traitModule(), input$dataset, input$response)
    dplyr::filter(
      summary(traitModule()[[input$dataset]]),
      response == input$response)
  })
  dendroplots <- shiny::reactive({
    shiny::req(traitModule(), input$dataset, input$response)

    foundr::ggplot_listof_wgcnaModules(
      traitModule()[[input$dataset]],
      input$response)
  })
  output$dendroplot <- shiny::renderPlot({
    print(dendroplots())
  })

  ###############################################
  # Module comparisons
  # See WGCNAshiny.Rmd
  
  # List returned
  reactive({
    shiny::req(input$dataset, input$response, dendroplots(), dendrotable())
    list(
      plot = print(dendroplots()),
      table = dendrotable(),
      traits = c(input$dataset, input$response))
  })
}

      
  