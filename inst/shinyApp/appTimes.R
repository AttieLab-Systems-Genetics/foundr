dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitData <- readRDS(file.path(dirpath, "traitData.rds"))
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test ShinyTimes Module"

ui <- function() {
  # INPUTS
  #   input$facet 
  #   input$strains
  #   input$height

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        foundr::shinyTimePanelInput("shinyTest"),
        
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        
#        foundr::shinyTimePanelUI("shinyTest")
        ),
      
      shiny::mainPanel(
        foundr::shinyTimePanelOutput("shinyTest")
        )))
}

server <- function(input, output, session) {
  
#  shiny::onStop(function() {RSQLite::dbDisconnect(db)})
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices, inline = TRUE)
  })

  timesOutput <- foundr::shinyTimePanel("shinyTest", input, 
                  traitData, traitSignal, traitStats)
}

shiny::shinyApp(ui = ui, server = server)
