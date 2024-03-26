dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test Shiny Trait Correlation Plot"

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
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput("reldataset")),
          shiny::column(6, foundr::shinyTraitNamesUI("shinyNames"))),
        
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1)),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::textOutput("key_trait"),
          foundr::shinyCorTableOutput("shinyCorTable"),
          shiny::textOutput("rel_traits"),
          foundr::shinyCorPlotOutput("shinyCorPlot")
          ))
    ))
}

server <- function(input, output, session) {
  
  # INPUTS
  # OUTPUTS
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
  # Related Traits.
  rel_traitsOutput <- foundr::shinyTraitNames("shinyNames", input,
                                              corTableOutput, TRUE)
  # Correlation Plot
  corPlotOutput <- foundr::shinyCorPlot("shinyCorPlot", input, input,
                                        corTableOutput)
  
  # I/O FROM MODULE
  output$dataset <- shiny::renderUI({
    # Dataset selection.
    datasets <- unique(traitStats$dataset)
    
    # Get datasets.
    shiny::selectInput("dataset", "Datasets:",
                       datasets, datasets[1], multiple = TRUE)
  })
  output$key_trait <- renderText({
    shiny::req(orderOutput(), corTableOutput())
    
    foundr::unite_datatraits(corTableOutput(), key = TRUE)[1]
  })
  output$rel_traits <- renderText({
    shiny::req(rel_traitsOutput())
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
