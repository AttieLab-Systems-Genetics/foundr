dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
#traitData <- readRDS(file.path(dirpath, "traitData.rds"))
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

db <- RSQLite::dbConnect(RSQLite::SQLite(),
                         file.path(dirpath, "traitData.sqlite"))
traitData <- dplyr::tbl(db, "traitData")

if(FALSE) {
  traitData <- dplyr::filter(
    readRDS(file.path(dirpath, "traitData.rds")),
    dataset %in% c("Physio", "PlaMet0"))
  traitSignal <- dplyr::filter(
    readRDS(file.path(dirpath, "traitSignal.rds")),
    dataset %in% c("Physio", "PlaMet0"))
  traitStats <- dplyr::filter(
    readRDS(file.path(dirpath, "traitStats.rds")),
    dataset %in% c("Physio", "PlaMet0"))
}

################################################################

title <- "Test Shiny Trait Panel"

ui <- function() {
  # INPUTS
  #   input$facet: Facet by strain?
  #   input$strains: Strains to select
  #   input$height: Plot Height
  # OUTPUTS (see shinyTraitPairs)
  #   output$filename: 
  #   output$downloadPlot
  #   output$downloadTable

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        foundr::shinyTraitPanelInput("shinyPanel"),

        shiny::hr(style="border-width:5px;color:black;background-color:black"),
        
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),

        shiny::hr(style="border-width:5px;color:black;background-color:black"),
        
        foundr::shinyTraitPanelUI("shinyPanel")
      ),

      shiny::mainPanel(
        foundr::shinyTraitPanelOutput("shinyPanel")
      )))
}

server <- function(input, output, session) {
  
  # CALL MODULES
  foundr::shinyTraitPanel("shinyPanel", input,
                          traitData, traitSignalInput, traitStatsInput)
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput(
      "strains", "Strains",
      choices = choices, selected = choices, inline = TRUE)
  })

  # DATA OBJECTS
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
}

shiny::shinyApp(ui = ui, server = server)
