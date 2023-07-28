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
        foundr::shinyVolcanoInput("shinyTest"),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        foundr::shinyVolcanoUI("shinyTest")),
        
      shiny::mainPanel(
        foundr::shinyVolcanoOutput("shinyTest")
        )))
}

server <- function(input, output, session) {
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices, inline = TRUE)
  })
  
  volcanoOutput <- foundr::shinyVolcano("shinyTest", input, traitStats)
}

shiny::shinyApp(ui = ui, server = server)
