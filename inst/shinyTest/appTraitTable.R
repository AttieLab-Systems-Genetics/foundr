dirpath <- "~/FounderDietStudy"
traitData <- readRDS(file.path(dirpath, "Enrich", "EnrichData.rds"))
traitStats <- readRDS(file.path(dirpath, "Enrich", "EnrichStats.rds"))
traitSignal <- readRDS(file.path(dirpath, "Enrich", "EnrichSignal.rds"))
traitData$dataset <- "Enrich"
traitSignal$dataset <- "Enrich"
traitStats$dataset <- "Enrich"

################################################################

title <- "Test Shiny Trait Table"

ui <- function() {
  # INPUTS
  #   input$facet: Facet by strain?
  #   input$strains: Strains to select
  #   input$height: Plot Height
  #   input$
  #
  # OUTPUTS (see shinyTraitTable)
  #   traitSolosObject()

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("trait","Traits:",c("Enrich: 15N2-Urea_enrichment_120_18wk","Enrich: N-Methyl-D3-Creatinine_enrichment_0_18wk","Enrich: 5,5,5-D3-Leucine_enrichment_120_18wk","Enrich: Trimethyl-D9-Carnitine_enrichment_60_18wk")),
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        
        foundr::shinyTraitTableUI("shinyTest"),
        shiny::downloadButton("downloadTable", "Data")
      ),

      shiny::mainPanel(
        foundr::shinyTraitTableOutput("shinyTest")
      )))
}

server <- function(input, output, session) {
  
  # MODULES
  moduleOutput <- foundr::shinyTraitTable("shinyTest", input, trait_names,
                                           traitDataInput, traitSignalInput)
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices,
                              inline = TRUE)
  })

  # DATA OBJECTS
  traitDataInput <- shiny::reactive({
    traitData
  })
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  trait_names <- shiny::reactive({
    shiny::req(input$trait)
  })
  
  # MODULE OUTPUT: DataTable
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      "traitObject.csv"
    },
    content = function(file) {
      utils::write.csv(
        moduleOutput(),
        file, row.names = FALSE)
    })
}

shiny::shinyApp(ui = ui, server = server)
