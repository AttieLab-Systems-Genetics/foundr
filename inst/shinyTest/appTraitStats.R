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

title <- "Test Shiny Trait Stats"

ui <- function() {
  # INPUTS
  # OUTPUTS
  #   output$datatable

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::tagList(
          shiny::uiOutput("inputs"),
          foundr::shinyTraitStatsUI("shinyStat")
      )),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::textOutput("name"),
          DT::dataTableOutput("datatable"),
          shiny::textOutput("names"),
          DT::dataTableOutput("datatables")))
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

  # MODULES
  traitOutput <- shiny::callModule(
    foundr::shinyTraitStats, "shinyStat",
    input,
    traitDataInput, traitSignalInput, traitStatsInput)
  
  # I/O FROM MODULE
  output$name <- renderText(
    shiny::req(traitOutput()$proband))
  output$datatable <- DT::renderDataTable(
    shiny::req(traitOutput()$orders),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
  output$names <- renderText(
    paste(shiny::req(traitOutput()$traits), collapse = ", "))
  output$datatables <- DT::renderDataTable(
    shiny::req(traitOutput()$cors),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
}

shiny::shinyApp(ui = ui, server = server)
