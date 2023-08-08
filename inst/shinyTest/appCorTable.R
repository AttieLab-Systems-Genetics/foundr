dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test Shiny Trait Correlation Table"

ui <- function() {

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Key Datasets and Trait.
        shiny::fluidRow(
          shiny::column(6, foundr::shinyTraitOrderInput("shinyOrder")),
          shiny::column(6, foundr::shinyTraitNamesUI("shinyKeyTrait"))),
        
        # Related Datasets and Traits.
        shiny::uiOutput("reldataset"),
        foundr::shinyCorTableUI("shinyCorTable"),
        shiny::sliderInput("mincor", "Minimum:", 0, 1, 0.7)),
      
      shiny::mainPanel(
        shiny::tagList(
          foundr::shinyTraitOrderUI ("shinyOrder"),
          shiny::textOutput("keyTrait"),
          foundr::shinyCorTableOutput("shinyCorTable")))
    ))
}

server <- function(input, output, session) {
  
  # INPUTS (see shinyTraitStats)
  # OUTPUTS (see shinyTraitStats)
  #   output$key_trait: Key Trait
  #   output$key_stats: Key Dataset Stats
  #   output$rel_traits: Related Traits
  #   output$corstable: Table of Datasets Correlations
  #   output$corsplot: Plot of Datasets Correlations
  # OUTPUTS (see shinyTraitStats)
  #   corobject()
  
  
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
  
  # Key Trait.
  keyTraitOutput <- foundr::shinyTraitNames("shinyKeyTrait", input, orderOutput)
  
  # Correlation Table.
  corTableOutput <- foundr::shinyCorTable("shinyCorTable", input, input,
                                          keyTraitOutput, traitSignalInput)

  # I/O FROM MODULE
  output$keyTrait <- renderText({
    shiny::req(orderOutput(), corTableOutput())
    
    foundr::unite_datatraits(corTableOutput(), key = TRUE)[1]
  })
  
  # Related Datasets.
  datasets <- shiny::reactive({
    shiny::req(traitStatsInput())
    unique(traitStatsInput()$dataset)
  })
  output$reldataset <- renderUI({
    shiny::req(datasets())
    shiny::selectInput("reldataset", "Related Datasets:",
                       datasets(), datasets()[1], multiple = TRUE)
  })
}

shiny::shinyApp(ui = ui, server = server)
