dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

shiny::reactlogShow()

title <- "Test Shiny Trait Stats"

ui <- function() {

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        foundr::shinyTraitStatsUI("shinyStat")),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::textOutput("key_trait"),
          DT::dataTableOutput("key_stats"),
          shiny::textOutput("rel_traits"),
          DT::dataTableOutput("rel_cors")))
    ))
}

server <- function(input, output, session) {
  
  # INPUTS (see shinyTraitStats)
  # OUTPUTS (see shinyTraitStats)
  #   output$key_trait: Key Trait
  #   output$key_stats: Key Dataset Stats
  #   output$rel_traits: Related Traits
  #   output$rel_cors: Related Datasets Correlations
  
  # DATA OBJECTS 
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
  
  # MODULES
  traitOutput <- foundr::shinyTraitStats("shinyStat",
                                         traitSignalInput, traitStatsInput)
  
  # I/O FROM MODULE
  output$key_trait <- renderText({
    shiny::req(traitOutput())
    traitOutput()$key_trait
  })
  output$key_stats <- DT::renderDataTable(
    {
      shiny::req(traitOutput())
      traitOutput()$key_stats
    },
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
  output$rel_traits <- renderText({
    shiny::req(traitOutput())
    paste(traitOutput()$rel_traits, collapse = ", ")
  })
  output$rel_cors <- DT::renderDataTable(
    {
      shiny::req(traitOutput())
      traitOutput()$rel_cors
    },
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
}

shiny::shinyApp(ui = ui, server = server)
