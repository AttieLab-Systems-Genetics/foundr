library(shiny)
devtools::install_cran("plotly") #  not yet on UW dataviz
devtools::install_cran("markdown") #  not yet on UW dataviz
devtools::install_cran("cowplot") #  not yet on UW dataviz
devtools::install_cran("ggdendro") #  not yet on UW dataviz
#devtools::install_github("byandell/foundr")
library(foundr)

# dirpath <- "~/FounderDietStudy"

dirpath <- "C:/Users/ADMIN/Documents/GitHub/foundr/data"

# traitData <- readRDS(file.path(dirpat h, "Enrich", "EnrichData.rds"))
# traitStats <- readRDS(file.path(dirpath, "Enrich", "EnrichStats.rds"))
# traitSignal <- readRDS(file.path(dirpath, "Enrich", "EnrichSignal.rds"))

traitData <- readRDS(file.path(dirpath, "EnrichData.rds"))
traitStats <- readRDS(file.path(dirpath, "EnrichStats.rds"))
traitSignal <- readRDS(file.path(dirpath,"EnrichSignal.rds"))
traitData$dataset <- "Enrich"
traitSignal$dataset <- "Enrich"
traitStats$dataset <- "Enrich"

################################################################

title <- "Test Shiny Module"
testApp <- foundr::shinyTraitSolo
testUI <- foundr::shinyTraitSoloUI

library(reactlog)
reactlog_enable()

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
        shiny::uiOutput("upload"),
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        shiny::selectInput("trait","Traits:",c("15N2-Urea_enrichment_120_18wk","N-Methyl-D3-Creatinine_enrichment_0_18wk","5,5,5-D3-Leucine_enrichment_120_18wk","Trimethyl-D9-Carnitine_enrichment_60_18wk")),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::uiOutput("filename")), # See MODULE INPUT below
          shiny::column(
            3,
            shiny::downloadButton("downloadPlot", "Plots")),
          shiny::column(
            3,
            shiny::downloadButton("downloadTable", "Data")))
      ),

      shiny::mainPanel(
        testUI("shinyTest")
      )))
}

server <- function(input, output, session) {

  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices, inline = TRUE)
  })
  # output$upload <- shiny::renderUI({
  #   shiny::fileInput("upload", "Upload CSV, XLS, XLSX or RDS:", accept = c(".csv",".xls",".xlsx",".rds"),
  #                    width = "100%")
  # })

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

  moduleOutput <- shiny::callModule(
    testApp, "shinyTest",
    input,
    traitStatsInput, traitStatsInput)



  # I/O FROM MODULE
  # MODULE INPUT: File Prefix
  output$filename <- renderUI({
    shiny::req(moduleOutput())
    filename <- paste0(
      "module_",
      paste(moduleOutput()$traits, collapse = "."))
    shiny::textAreaInput("filename", "File Prefix", filename)
  })

  # MODULE OUTPUT: Plot
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$filename), ".pdf")
    },
    content = function(file) {
      grDevices::pdf(file, width = 9, height = 6)
      print(moduleOutput()$plot)
      grDevices::dev.off()
    })

  # MODULE OUTPUT: DataTable
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$filename), ".csv")
    },
    content = function(file) {
      utils::write.csv(
        moduleOutput()$table,
        file, row.names = FALSE)
    })
}

shiny::shinyApp(ui = ui, server = server)
