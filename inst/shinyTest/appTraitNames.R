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

dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
cat(dirpath, "\n", file = stderr())
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

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
  output$inputs <- renderUI({
    shiny::selectInput("dataset", "Dataset:", datasets(), input$dataset)
  })

  # DATA OBJECTS 
  traitStatsInput <- shiny::reactive({
    shiny::req(input$dataset)
    dplyr::filter(
      traitStats,
      .data$dataset %in% input$dataset)
  })

  moduleOutput <- shiny::callModule(
    testApp, "shinyTest", 
    input, input,
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
