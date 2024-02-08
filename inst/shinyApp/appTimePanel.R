dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitData <- readRDS(file.path(dirpath, "traitData.rds"))
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test shinyTime Module"

ui <- function() {
  # INPUTS
  #   input$facet 
  #   input$strains
  #   input$height

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("dataset"),
        foundr::shinyTimePanelInput("shinyTimePanel"),
        
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        ),
      
      shiny::mainPanel(
        foundr::shinyTimePanelOutput("shinyTimePanel")
      )))
}

server <- function(input, output, session) {
  
  # MODULES
  foundr::shinyTimePanel("shinyTimePanel", input,
                         traitData, traitSignal, traitStats)
  
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
