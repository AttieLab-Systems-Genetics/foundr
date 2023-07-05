dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test Shiny Trait Stats"

ui <- function() {
  # INPUTS
  # OUTPUTS
  #   output$datatable

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        foundr::shinyTraitStatsUI("shinyStat")),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::textOutput("name"),
          DT::dataTableOutput("datatable"),
          shiny::textOutput("names"),
          DT::dataTableOutput("datatables")))
    ))
}

server <- function(input, output, session) {
  
  # DATA OBJECTS 
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
  
  # MODULES
  traitOutput <- shiny::callModule(
    foundr::shinyTraitStats, "shinyStat",
    traitSignalInput, traitStatsInput)
  
  # I/O FROM MODULE
  output$name <- renderText({
    shiny::req(traitOutput())
    traitOutput()$proband
  })
  output$datatable <- DT::renderDataTable(
    {
      shiny::req(traitOutput())
      traitOutput()$orders
    },
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
  output$names <- renderText({
    shiny::req(traitOutput())
    paste(traitOutput()$traits, collapse = ", ")
  })
  output$datatables <- DT::renderDataTable(
    {
      shiny::req(traitOutput())
      traitOutput()$cors
    },
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
}

shiny::shinyApp(ui = ui, server = server)
