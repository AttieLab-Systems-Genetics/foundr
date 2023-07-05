dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitData <- readRDS(file.path(dirpath, "traitData.rds"))
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test Shiny Trait Solos with Trait Names"

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
        shiny::uiOutput("upload"),
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        foundr::shinyTraitStatsUI("shinyStat"),
#        shiny::selectInput("trait","Traits:",c("Enrich: 15N2-Urea_enrichment_120_18wk","Enrich: N-Methyl-D3-Creatinine_enrichment_0_18wk","Enrich: 5,5,5-D3-Leucine_enrichment_120_18wk","Enrich: Trimethyl-D9-Carnitine_enrichment_60_18wk")),
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
        foundr::shinyTraitSoloUI("shinySolo")
      )))
}

server <- function(input, output, session) {

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
  datasets<-shiny::reactive({
    "Enrich"
  })
  
  # Traits
  trait_names <- shiny::reactive({
    shiny::req(statsOutput())
    c(statsOutput()$key_trait, statsOutput()$rel_traits)
  })

  # CALL MODULE
  statsOutput <- shiny::callModule(
    foundr::shinyTraitStats, "shinyStat",
    traitSignalInput, traitStatsInput)
  soloOutput <- shiny::callModule(
    foundr::shinyTraitSolo, "shinySolo",
    input, trait_names,
    traitDataInput, traitSignalInput, datasets)

  # I/O FROM MODULE
  # MODULE INPUT: File Prefix
  output$filename <- renderUI({
    shiny::req(soloOutput())
    filename <- paste0(
      "module_",
      paste(soloOutput()$traits, collapse = "."))
    shiny::textAreaInput("filename", "File Prefix", filename)
  })

  # MODULE OUTPUT: Plot
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$filename), ".pdf")
    },
    content = function(file) {
      grDevices::pdf(file, width = 9, height = 6)
      print(soloOutput()$plot)
      grDevices::dev.off()
    })

  # MODULE OUTPUT: DataTable
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$filename), ".csv")
    },
    content = function(file) {
      utils::write.csv(
        soloOutput()$table,
        file, row.names = FALSE)
    })
}

shiny::shinyApp(ui = ui, server = server)
