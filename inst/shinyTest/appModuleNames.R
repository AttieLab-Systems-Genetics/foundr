library(shiny)
devtools::install_cran("plotly") #  not yet on UW dataviz
devtools::install_cran("markdown") #  not yet on UW dataviz
devtools::install_cran("cowplot") #  not yet on UW dataviz
devtools::install_cran("ggdendro") #  not yet on UW dataviz
#devtools::install_github("byandell/foundr")
library(foundr)

harmonizeddir <- "/mnt/researchdrive/adattie/General/founder_diet_study/HarmonizedData"
traitDir <- "Normalized" # "Unnormalized"
filename <- file.path(harmonizeddir, traitDir, "traitModule.rds")
if(file.exists(filename)) {
  traitModule <- readRDS(filename)
} else {
  traitModule <- NULL
}

################################################################

title <- "Test Shiny Module Names"
testApp <- foundr::shinyModuleNames
testUI <- foundr::shinyModuleNamesUI

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
        4,
        shiny::selectInput(
          "dataset", "Dataset:",
          datasets(), input$dataset)),
      shiny::column(
        4,
        shiny::selectInput(
          "modrole", "Module Role:",
          c("Facet","Color"), input$modrole)),
      shiny::column(
        4,
        shiny::selectInput(
          "response", paste(input$modrole, "Response:"),
          responses(), input$response))
      )
  })

  # DATA OBJECTS 
  traitModuleInput <- shiny::reactive({
    traitModule
  })
  modrole <- shiny::reactive({
    shiny::req(input$modrole)
  })
  
  moduleOutput <- shiny::callModule(
    testApp, "shinyTest", 
    input, 
    traitModuleInput, modrole)
  
  # I/O FROM MODULE
  # MODULE INPUT: File Prefix
  output$filename <- renderUI({
    shiny::req(moduleOutput())
    filename <- paste(moduleOutput(), collapse = ", ")
    shiny::textAreaInput("filename", "Modules", filename)
  })
}

shiny::shinyApp(ui = ui, server = server)
