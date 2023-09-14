library(shiny)
devtools::install_cran("plotly") #  not yet on UW dataviz
devtools::install_cran("markdown") #  not yet on UW dataviz
devtools::install_cran("cowplot") #  not yet on UW dataviz
devtools::install_cran("ggdendro") #  not yet on UW dataviz
#devtools::install_github("byandell/foundr")

#dirpath <- "~/FounderDietStudy/Enrich"
#traitStats <- readRDS(file.path(dirpath, "EnrichStats.rds"))
#traitStats$dataset <- "Enrich"

dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test Shiny Volcano Module"

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
        foundr::shinyVolcanoOutput("shinyTest")
        )))
}

server <- function(input, output, session) {
  
  # SERVER-SIDE INPUTS
  output$dataset <- shiny::renderUI({
    # Dataset selection.
    datasets <- unique(traitStats$dataset)

    # Get new input parameters for Volcano.
    shiny::selectInput("dataset", "Datasets:",
                       datasets, datasets[1], multiple = TRUE)
  })

  volcanoOutput <- foundr::shinyVolcano("shinyTest", input, traitStats)
}

shiny::shinyApp(ui = ui, server = server)
