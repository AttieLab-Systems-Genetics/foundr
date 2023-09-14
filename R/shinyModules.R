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
    shinyModuleDendro, "shinydendro", 
    input, main_par, traitModule)
  modcompOut <- shiny::callModule(
    shinyModuleComp, "shinymodcomp", 
    input, main_par, traitModule)
  eigenOut <- shiny::callModule(
    shinyModuleEigen, "shinyeigen", 
    input, main_par, traitModule)
  namesOut <- shiny::callModule(
    shinyModuleNames, "shinynames", 
    input, main_par, traitModule)
  
  # INPUTS
  # foundrServer inputs: (see shinyapp.R)
  #   main_par$height (see shinyapp.R::foundrUI sidebarPanel)
  #   main_par$facet (see shinyapp.R::foundrServer output$settings)
  #.  main_par$strains (see shinyapp.R::foundrServer output$strains)
  # shinyModules inputs: (see output$shiny_module below)
  #   input$dataset
  #   input$response
  #   input$butmod
  
  datasets <- shiny::reactive(
    c("LivMet","PlaMet0","PlaMet120","Metab"))
  responses <- shiny::reactive(
    c("value","cellmean","signal","rest"))
  
  output$shiny_module <- shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::radioButtons(
            ns("butmod"), "Module Plots",
            c("Dendrogram", "Modules","Eigens","Names"), "Dendrogram",
            inline = TRUE)),
        shiny::column(
          4,
          shiny::selectInput(
            ns("dataset"), "Dataset:",
            datasets())),
        shiny::column(
          4,
          shiny::selectInput(
            ns("response"), "Response:",
            responses()))),
      
      shiny::uiOutput(ns("condmod")))
  })
  
  output$condmod <- shiny::renderUI({
    shiny::req(input$butmod)
    shiny::tagList(
      shiny::uiOutput(ns("modresp")),
      
      switch(
        input$butmod,
        Dendrogram = shinyModuleDendroUI(ns("shinydendro")),
        Modules    = shinyModuleCompUI(ns("shinymodcomp")),
        Eigens     = shinyModuleEigenUI(ns("shinyeigen")),
        Names      = shinyModuleNamesUI(ns("shinynames")))
    )
    
  })
  output$modresp <- shiny::renderUI({
    if(input$butmod != "Dendrogram") {
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectInput(
            ns("responseF"), "Facet Response:",
            "cellmean")),
        shiny::column(
          4,
          shiny::selectInput(
            ns("responseC"), "Color Response:",
            "value")))
    }
  })
  
  # Responses for color (C) and facet (F)
  responseFs <- shiny::reactive({
    shiny::req(input$response)
    unique(c(input$response, responses()))
  })
  responseCs <- shiny::reactive({
    shiny::req(input$responseF)
    responses()[responses() != input$responseF]
  })
  shiny::observeEvent(
    input$response,
    {
      shiny::updateSelectInput(
        session, "responseF",
        choices = responseFs(),
        selected = responseFs()[1])
    })
  shiny::observeEvent(
    input$responseF,
    {
      shiny::updateSelectInput(
        session, "responseC",
        choices = responseCs(),
        selected = responseCs()[1])
    })

  # List returned
  reactive({
    shiny::req(input$dataset, input$response, input$butmod)
    switch(
      input$butmod,
      Dendrogram = dendroOut(),
      Modules    = modcompOut(),
      Eigens     = eigenOut(),
      names      = namesOut())
  })
}

      
  