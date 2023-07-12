dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitData <- dplyr::filter(
  readRDS(file.path(dirpath, "traitData.rds")),
  dataset %in% c("Physio", "PlaMet0"))
traitSignal <- dplyr::filter(
  readRDS(file.path(dirpath, "traitSignal.rds")),
  dataset %in% c("Physio", "PlaMet0"))
traitStats <- dplyr::filter(
  readRDS(file.path(dirpath, "traitStats.rds")),
  dataset %in% c("Physio", "PlaMet0"))

################################################################

title <- "Test Shiny Trait Pairs with Trait Names"

ui <- function() {
  # INPUTS
  #   input$facet: Facet by strain?
  #   input$strains: Strains to select
  #   input$height: Plot Height
  #   input$
  #
  # OUTPUTS (see shinyTraitPairs)
  #   output$filename: 
  #   output$downloadPlot
  #   output$downloadTable

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        foundr::shinyTraitStatsInput("shinyStats"),
        foundr::shinyTraitTableUI("shinyObject"),
        
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        
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
        shiny::tagList(
          foundr::shinyTraitPairsUI("shinyPairs"),
          foundr::shinyTraitTableOutput("shinyObject")
        )
      )))
}

server <- function(input, output, session) {
  
  # MODULES
  statsOutput <- foundr::shinyTraitStats("shinyStats", input,
                                         traitSignalInput, traitStatsInput)
  tableOutput <- foundr::shinyTraitTable("shinyObject", input, statsOutput,
                                         traitDataInput, traitSignalInput)
  
  solosOutput <- foundr::shinyTraitSolos("shinySolos", input, tableOutput)
  pairsOutput <- foundr::shinyTraitPairs("shinyPairs", input, statsOutput,
                                         tableOutput)
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices, inline = TRUE)
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
  
  # RETURN OBJECTS FROM MODULES
  datasets<-shiny::reactive({
    shiny::req(tableOutput())
    unique(tableOutput()$dataset)
  })
  
  # I/O FROM MODULE
  
  # MODULE INPUT: File Prefix
  output$filename <- renderUI({
    shiny::req(datasets())
    filename <- paste0(
      "module_",
      paste(datasets(), collapse = "."))
    shiny::textAreaInput("filename", "File Prefix", filename)
  })

  # MODULE OUTPUT: Plot
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$filename), ".pdf")
    },
    content = function(file) {
      grDevices::pdf(file, width = 9, height = 6)
      print(pairsOutput())
      grDevices::dev.off()
    })

  # MODULE OUTPUT: DataTable
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$filename), ".csv")
    },
    content = function(file) {
      utils::write.csv(
        tableOutput(),
        file, row.names = FALSE)
    })
}

shiny::shinyApp(ui = ui, server = server)
