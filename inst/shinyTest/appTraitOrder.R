dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test Shiny Trait Order Table"

ui <- function() {

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Key Datasets and Trait.
        foundr::shinyTraitOrderInput("shinyOrder"),
        # Related Datasets and Traits.
        shiny::uiOutput("reldataset")),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::textOutput("key_trait"),
          foundr::shinyTraitOrderUI ("shinyOrder")))
    ))
}

server <- function(input, output, session) {
  
  # INPUTS
  # OUTPUTS
  #   output$key_trait: Key Trait
  # OUTPUTS
  #   orderstats()
  
  # DATA OBJECTS 
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
  
  # MODULES
  # Order Traits by Stats.
  orderOutput <- foundr::shinyTraitOrder("shinyOrder", traitStatsInput)

  # I/O FROM MODULE
  output$key_trait <- renderText({
    shiny::req(orderOutput())
    
    foundr::unite_datatraits(orderOutput(), key = TRUE)[1]
  })
}

shiny::shinyApp(ui = ui, server = server)
