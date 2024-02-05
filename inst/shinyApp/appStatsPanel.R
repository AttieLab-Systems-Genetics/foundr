dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))
traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))

################################################################

title <- "Test Shiny Stats Module"

ui <- function() {
  # INPUTS
  #   input$height 

  shiny::fluidPage(
    shiny::titlePanel(title),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("dataset"),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1)),
        
      shiny::mainPanel(
        foundr::shinyStatsPanelOutput("shinyTest")
        )))
}

server <- function(input, output, session) {
  
  # SERVER-SIDE INPUTS
  output$dataset <- shiny::renderUI({
    # Dataset selection.
    datasets <- unique(traitStats$dataset)

    # Get new input parameters for Stats.
    shiny::selectInput("dataset", "Datasets:",
                       datasets, datasets[1], multiple = TRUE)
  })

  foundr::shinyStatsPanel("shinyTest", input, traitStats)
}

shiny::shinyApp(ui = ui, server = server)
