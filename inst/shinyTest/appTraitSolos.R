dirpath <- "~/FounderDietStudy"
traitData <- readRDS(file.path(dirpath, "Enrich", "EnrichData.rds"))
traitStats <- readRDS(file.path(dirpath, "Enrich", "EnrichStats.rds"))
traitSignal <- readRDS(file.path(dirpath, "Enrich", "EnrichSignal.rds"))
traitData$dataset <- "Enrich"
traitSignal$dataset <- "Enrich"
traitStats$dataset <- "Enrich"

################################################################

title <- "Test Shiny Trait Solos"

ui <- function() {
  # INPUTS
  #   input$facet: Facet by strain?
  #   input$strains: Strains to select
  #   input$height: Plot Height
  #   input$
  #
  # OUTPUTS (see shinyTraitSolos)
  #   output$filename: 
  #   output$downloadPlot
  #   output$downloadTable

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("trait","Traits:",c("Enrich: 15N2-Urea_enrichment_120_18wk","Enrich: N-Methyl-D3-Creatinine_enrichment_0_18wk","Enrich: 5,5,5-D3-Leucine_enrichment_120_18wk","Enrich: Trimethyl-D9-Carnitine_enrichment_60_18wk")),
        foundr::shinyTraitObjectUI("shinyObject"),
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
          foundr::shinyTraitSolosUI("shinySolos"),
          foundr::shinyTraitObjectOutput("shinyObject")
        )
      )))
}

server <- function(input, output, session) {
  
  # CALL MODULES
  objectOutput <- shiny::callModule(
    foundr::shinyTraitObject, "shinyObject",
    input, trait_names,
    traitDataInput, traitSignalInput)
  solosOutput <- shiny::callModule(
    foundr::shinyTraitSolos, "shinySolos",
    input, objectOutput)
  
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
  trait_names <- shiny::reactive({
    shiny::req(input$trait)
  })
  datasets <- shiny::reactive({
    shiny::req(objectOutput())
    
    unique(objectOutput()$dataset)
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
      print(solosOutput())
      grDevices::dev.off()
    })

  # MODULE OUTPUT: DataTable
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$filename), ".csv")
    },
    content = function(file) {
      utils::write.csv(
        objectOutput(),
        file, row.names = FALSE)
    })
}

shiny::shinyApp(ui = ui, server = server)
