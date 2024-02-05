dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitData <- readRDS(file.path(dirpath, "traitData.rds"))
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))
customSettings <- list(condition = "diet")

#db <- RSQLite::dbConnect(RSQLite::SQLite(),
#                         file.path(dirpath, "traitData.sqlite"))
#traitData <- dplyr::tbl(db, "traitData")

################################################################

title <- "Test Shiny Trait Panel"

ui <- function() {
  # INPUTS
  #   input$facet: Facet by strain?
  #   input$strains: Strains to select
  #   input$height: Plot Height

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(
          shiny::column(3, shiny::uiOutput("dataset")),
          shiny::column(9, foundr::shinyTraitPanelInput("shinyPanel"))),
        foundr::shinyTraitPanelUI("shinyPanel"),

        shiny::hr(style="border-width:5px;color:black;background-color:black"),
                
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1)
      ),

      shiny::mainPanel(
        foundr::shinyTraitPanelOutput("shinyPanel")
      )))
}

server <- function(input, output, session) {
  
#  shiny::onStop(function() {RSQLite::dbDisconnect(db)})
  
  # CALL MODULES
  foundr::shinyTraitPanel("shinyPanel", input,
                          traitData, traitSignal, traitStats,
                          customSettings)
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput(
      "strains", "Strains",
      choices = choices, selected = choices, inline = TRUE)
  })
  output$dataset <- shiny::renderUI({
    # Dataset selection.
    datasets <- unique(traitStats$dataset)
    
    # Get datasets.
    shiny::selectInput("dataset", "Datasets:",
                       datasets, datasets[1], multiple = TRUE)
  })
  
}

shiny::shinyApp(ui = ui, server = server)
