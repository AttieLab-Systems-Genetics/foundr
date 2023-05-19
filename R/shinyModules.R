#' Shiny Module UI for Modules Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyModules
#' @export
#'
shinyModulesUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("shiny_module"))
  )
}

#' Shiny Module Server for Modules Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer` 
#' @param traitDataInput,traitSignalInput,traitStatsInput reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyModulesUI`
#' @importFrom shiny column fluidRow observeEvent plotOutput reactive
#'             reactiveVal renderPlot renderUI req selectInput selectizeInput
#'             tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @importFrom cowplot plot_grid
#' @export
#'
shinyModules <- function(input, output, session,
                       main_par,
                       traitDataInput, traitSignalInput, traitStatsInput,
                       traitModule) {
  ns <- session$ns
  
  # Call Shiny Modules here.
  dendroOut <- shiny::callModule(
    shinyDendro, "shinydendro", 
    main_par, traitModule)
  modcompOut <- shiny::callModule(
    shinyModuleComp, "shinymodcomp", 
    main_par, traitModule)
  
  # INPUTS
  # Main inputs: (see shinyapp.R)
  #   main_par$height (see shinyapp.R::foundrUI sidebarPanel)
  #   main_par$facet (see shinyapp.R::foundrServer output$settings)
  #.  main_par$strains (see shinyapp.R::foundrServer output$strains)
  # Modules inputs: (see output$shiny_module below)
  #   input$dataset
  #   input$response
  #   input$butmod
  
  datasets <- shiny::reactive(
    c("LivMet","PlaMet0","PlaMet120","Metab"))
  responses <- shiny::reactive(
    c("value","cellmean","signal","rest","noise"))
  
  output$shiny_module <- shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectInput(
            ns("dataset"), "Dataset:",
            datasets())),
        shiny::column(
          4,
          shiny::selectInput(
            ns("response"), "Response:",
            responses())),
          shiny::column(
          4,
          shiny::radioButtons(
            ns("butmod"), "Module Plots",
            c("Dendrogram", "Modules","Other"), "Dendrogram",
            inline = TRUE))),
      
      shiny::uiOutput(ns("condmod")))
  })
  
  output$condmod <- shiny::renderUI({
    switch(
      input$butmod,
      Dendrogram = shinyDendroUI(ns("shinydendro")),
      Modules = shinyModuleCompUI(ns("shinymodcomp")),
      Other = shiny::uiOutput(ns("dendro")))
  })
  #        shiny::uiOutput(ns("dendro"))))#,
  
  
  ###############################################
  # Dendrogram
  output$dendro <- shiny::renderUI({
    shiny::req(main_par$height, input$dataset, input$response)
    shiny::tagList(
      shiny::plotOutput(ns("moduleplot"),
                        height = paste0(main_par$height, "in")),

      DT::renderDataTable(
        moduletable(),
        escape = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)))
  })
  
  moduletable <- shiny::reactive({
    shiny::req(traitModule(), input$dataset, input$response)#, input$butmod)
    dplyr::filter(
      summary(traitModule()[[input$dataset]]),
      response == input$response)
  })
  moduleplots <- shiny::reactive({
    shiny::req(traitModule(), input$dataset, input$response)#, input$butmod)

    foundr::ggplot_listof_wgcnaModules(
      traitModule()[[input$dataset]],
      input$response)
  })
  output$moduleplot <- shiny::renderPlot({
    print(moduleplots())
  })

  ###############################################
  # Module comparisons
  # See WGCNAshiny.Rmd
  
  # List returned
  reactive({
    shiny::req(input$dataset, input$response, moduleplots(), moduletable(),
               input$butmod)
    switch(
      input$butmod,
      Dendrogram = dendroOut(),
      Modules = modcompOut(),
      Other = {
        list(
          plot = print(moduleplots()),
          table = moduletable(),
          traits = c(input$dataset, input$response))
      })
  })
}

      
  