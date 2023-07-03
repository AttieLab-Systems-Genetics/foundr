library(shiny)
devtools::install_cran("plotly") #  not yet on UW dataviz
devtools::install_cran("markdown") #  not yet on UW dataviz
devtools::install_cran("cowplot") #  not yet on UW dataviz
devtools::install_cran("ggdendro") #  not yet on UW dataviz
#devtools::install_github("byandell/foundr")
library(foundr)

dirpath <- "~/FounderDietStudy"
traitData <- readRDS(file.path(dirpath, "Enrich", "EnrichData.rds"))
traitStats <- readRDS(file.path(dirpath, "Enrich", "EnrichStats.rds"))
traitSignal <- readRDS(file.path(dirpath, "Enrich", "EnrichSignal.rds"))
traitData$dataset <- "Enrich"
traitSignal$dataset <- "Enrich"
traitStats$dataset <- "Enrich"


################################################################

title <- "Test Shiny Trait Names"
testApp <- foundr::shinyTraitNames
testUI <- foundr::shinyTraitNamesUI

reactlog::reactlog_enable()

ui <- function() {
  # INPUTS
  #   input$facet 
  #   input$strains 
  #
  # OUTPUTS
  #   output$filename
  #   output$downloadPlot
  #   output$downloadTable
  
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("filename")),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::uiOutput("inputs"),
          
          testUI("shinyTest")))
      ))
}

server <- function(input, output, session) {
  
  datasets <- shiny::reactive(
    c("LivMet","PlaMet0","PlaMet120","Metab"))
  responses <- shiny::reactive(
    c("value","cellmean","signal","rest","noise"))
  output$inputs <- renderUI({
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput(
          "dataset", "Dataset:",
          datasets(), input$dataset)),
      shiny::column(
        6,
        shiny::selectInput(
          "response", paste(input$modrole, "Response:"),
          responses(), input$response))
      )
  })

  # DATA OBJECTS 
  traitDataInput <- shiny::reactive({
    traitData
  })
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
  #proband <- shiny::reactive("Enrich: 15N2-Urea_enrichment_15_18wk")
  proband <- shiny::reactive(NULL)
   
  moduleOutput <- shiny::callModule(
    testApp, "shinyTest", 
    input,
    traitDataInput,
    traitStatsInput,
    traitStatsInput)
  
  # I/O FROM MODULE
  output$filename <- renderUI({
    shiny::req(moduleOutput())
    filename <- paste(moduleOutput(), collapse = ", ")
    shiny::textAreaInput("filename", "Traits", filename)
  })
}

shiny::shinyApp(ui = ui, server = server)
