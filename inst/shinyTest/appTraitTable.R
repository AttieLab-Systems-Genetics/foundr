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
        shiny::uiOutput("traits"),
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
  moduleOutput <- foundr::shinyTraitTable("shinyTest", input, input,
                                          keyTrait, relTraits,
                                          traitData, traitSignal)
  # Mockup of trait names
  keyTrait <- shiny::reactive({
    shiny::req(input$trait)
    },
    label = "keyTrait")
  relTraits <- shiny::reactive({
    NULL
  },
  label = "relTraits")
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices,
                              inline = TRUE)
  })
  output$traits <- shiny::renderUI({
    traits <- unique(foundr::unite_datatraits(traitSignal))[1:5]

    shiny::selectInput("trait","Traits:", traits)
  })

  # DATA OBJECTS
  traitDataInput <- shiny::reactive({
    traitData
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
