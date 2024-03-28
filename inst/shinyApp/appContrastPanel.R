dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))
traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))
dirpath <- file.path("~/Documents/Research/attie_alan/FounderDietStudy/deployLiver")
traitSignal <- readRDS(file.path(dirpath, "liverSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "liverStats.rds"))
traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))

customSettings <- list(condition = "diet")

################################################################

title <- "Test Shiny Contrast Trait Panel"

ui <- function() {
  # INPUTS
  #   input$dataset: Datasets to select
  #   input$height: Plot Height
  # OUTPUTS (see shinyTraitPairs)
  #   output$filename: 
  #   output$downloadPlot
  #   output$downloadTable

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(
          shiny::column(3, shiny::uiOutput("dataset")),
          shiny::column(9, foundr::shinyContrastPanelInput("shinyPanel"))),
        foundr::shinyContrastPanelUI("shinyPanel"),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1)),
      
    shiny::mainPanel(
        foundr::shinyContrastPanelOutput("shinyPanel")
      )))
}

server <- function(input, output, session) {
  
#  shiny::onStop(function() {RSQLite::dbDisconnect(db)})
  
  # CALL MODULES
  foundr::shinyContrastPanel("shinyPanel", input,
                          traitSignal, traitStats, traitModule,
                          customSettings)
  
  # SERVER-SIDE INPUTS
  output$dataset <- shiny::renderUI({
    # Dataset selection.
    datasets <- unique(traitStats$dataset)
    
    # Get datasets.
    shiny::selectInput("dataset", "Datasets:",
                       datasets, datasets[1], multiple = TRUE)
  })
  
}

shiny::shinyApp(ui = ui, server = server)
