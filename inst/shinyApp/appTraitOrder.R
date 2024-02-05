dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
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
        shiny::uiOutput("dataset"),
        foundr::shinyTraitOrderInput("shinyOrder"),
        # Related Datasets and Traits.
        shiny::uiOutput("reldataset")),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::textOutput("key_trait"),
          foundr::shinyTraitOrderUI("shinyOrder"))
    )))
}

server <- function(input, output, session) {
  
  # INPUTS
  # OUTPUTS
  #   output$key_trait: Key Trait
  # OUTPUTS
  #   orderstats()
  
  # MODULES
  # Order Traits by Stats.
  orderOutput <- foundr::shinyTraitOrder("shinyOrder", input, input,
                                         traitStats, traitSignal)

  # I/O FROM MODULE
  output$dataset <- shiny::renderUI({
    # Dataset selection.
    datasets <- unique(traitStats$dataset)
    
    # Get datasets.
    shiny::selectInput("dataset", "Datasets:",
                       datasets, datasets[1], multiple = TRUE)
  })
  output$key_trait <- renderText({
    shiny::req(orderOutput())
    
    foundr::unite_datatraits(orderOutput(), key = TRUE)[1]
  })
  
}

shiny::shinyApp(ui = ui, server = server)
