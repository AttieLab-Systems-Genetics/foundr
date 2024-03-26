dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
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
          shiny::column(3, shiny::uiOutput("dataset")),
          shiny::column(3, foundr::shinyTraitOrderInput("shinyOrder")),
          shiny::column(6, foundr::shinyTraitNamesUI("shinyKeyTrait"))),
        
        # Related Datasets and Traits.
        shiny::uiOutput("reldataset")),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::textOutput("orderTable"),
          shiny::textOutput("keyTrait"),
          shiny::textOutput("corTable"),
          foundr::shinyTraitOrderUI ("shinyOrder"),
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
  
  # MODULES
  # Order Traits by Stats.
  orderOutput <- foundr::shinyTraitOrder("shinyOrder", input, input, traitStats)
  
  # Key Trait.
  keyTraitOutput <- foundr::shinyTraitNames("shinyKeyTrait", input, orderOutput)
  
  # Correlation Table.
  corTableOutput <- foundr::shinyCorTable("shinyCorTable", input, input,
                                          keyTraitOutput, traitSignal)
  
  # I/O FROM MODULE
  output$dataset <- shiny::renderUI({
    # Dataset selection.
    datasets <- unique(traitStats$dataset)
    
    # Get datasets.
    shiny::selectInput("dataset", "Datasets:",
                       datasets, datasets[1], multiple = TRUE)
  })
  output$keyTrait <- renderText({
    paste("keyTrait", shiny::req(keyTraitOutput()))
  })
  output$orderTable <- renderText({
    shiny::req(orderOutput())
    
    paste("orderOutput", foundr::unite_datatraits(orderOutput())[1])
  })
  output$corTable <- renderText({
    shiny::req(corTableOutput())
    
    paste("corTableOutput", foundr::unite_datatraits(corTableOutput(), key = TRUE)[1])
  })
  
  # Related Datasets.
  datasets <- shiny::reactive({
    unique(traitStats$dataset)
  })
  output$reldataset <- renderUI({
    shiny::req(datasets())
    shiny::selectInput("reldataset", "Related Datasets:",
                       datasets(), datasets()[1], multiple = TRUE)
  })
}

shiny::shinyApp(ui = ui, server = server)
