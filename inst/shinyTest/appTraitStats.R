dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitSignal <- dplyr::filter(
  readRDS(file.path(dirpath, "traitSignal.rds")),
  dataset %in% c("Physio", "PlaMet0"))
traitStats <- dplyr::filter(
  readRDS(file.path(dirpath, "traitStats.rds")),
  dataset %in% c("Physio", "PlaMet0"))

################################################################

#shiny::reactlogShow()

title <- "Test Shiny Trait Stats"

ui <- function() {

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::tagList(
          shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
          foundr::shinyTraitStatsInput("shinyStats"))),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::textOutput("key_trait"),
          foundr::shinyTraitStatsUI("shinyStats"),
          shiny::textOutput("rel_traits"),
          foundr::shinyTraitStatsOutput("shinyStats")))
    ))
}

server <- function(input, output, session) {
  
  # INPUTS (see shinyTraitStats)
  # OUTPUTS (see shinyTraitStats)
  #   output$key_trait: Key Trait
  #   output$key_stats: Key Dataset Stats
  #   output$rel_traits: Related Traits
  #   output$corstable: Related Datasets Correlations
  
  # DATA OBJECTS 
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
  
  # MODULES
  statsOutput <- foundr::shinyTraitStats("shinyStats", input,
                                         traitSignalInput, traitStatsInput)
  
  # I/O FROM MODULE
  output$key_trait <- renderText({
    shiny::req(statsOutput())
    statsOutput()$key_trait
  })
  output$rel_traits <- renderText({
    shiny::req(statsOutput())
    paste(statsOutput()$rel_traits, collapse = ", ")
  })
}

shiny::shinyApp(ui = ui, server = server)
