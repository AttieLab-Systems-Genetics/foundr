dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitData <- dplyr::filter(
  readRDS(file.path(dirpath, "traitData.rds")),
  dataset %in% c("Physio", "PlaMet0"))
traitSignal <- dplyr::filter(
  readRDS(file.path(dirpath, "traitSignal.rds")),
  dataset %in% c("Physio", "PlaMet0"))
traitStats <- dplyr::filter(
  readRDS(file.path(dirpath, "traitStats.rds")),
  dataset %in% c("Physio", "PlaMet0"))

################################################################

title <- "Test Shiny Trait Pairs with Trait Names"

ui <- function() {
  # INPUTS
  #   input$facet: Facet by strain?
  #   input$strains: Strains to select
  #   input$height: Plot Height
  #   input$
  #
  # OUTPUTS (see shinyTraitPairs)
  #   output$filename: 
  #   output$downloadPlot
  #   output$downloadTable

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Key Datasets and Trait.
        shiny::fluidRow(
          shiny::column(6, foundr::shinyTraitOrderInput("shinyOrder")),
          shiny::column(6, foundr::shinyCorTableInput("shinyCorTable"))),
        
        # Related Datasets and Traits.
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput("reldataset")),
          shiny::column(6, foundr::shinyTraitNamesUI("shinyNames"))),
        
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        
        # Trait Table Response.
        foundr::shinyTraitTableUI("shinyTable"),
        
        foundr::shinyCorTableUI("shinyCorTable")
      ),

      shiny::mainPanel(
        shiny::tagList(
          foundr::shinyTraitPairsUI("shinyPairs"),
          foundr::shinyTraitTableOutput("shinyObject")
        )
      )))
}

server <- function(input, output, session) {
  
  customSettings <- NULL
  # MODULES
  # Order Traits by Stats.
  orderOutput <- foundr::shinyTraitOrder("shinyOrder", traitStatsInput)
  # Key Trait and Correlation Table.
  corTableOutput <- foundr::shinyCorTable("shinyCorTable", input, input,
                                  orderOutput, traitSignalInput,
                                  customSettings)
  # Related Traits.
  rel_traitsOutput <- foundr::shinyTraitNames("shinyNames", input,
                                      corTableOutput, TRUE)

  # Filter static traitData based on selected trait_names.
  traitDataInput <- shiny::reactive({
    shiny::req(trait_names())
    
    foundr::subset_trait_names(traitData, trait_names())
  })
  
  tableOutput <- foundr::shinyTraitTable("shinyTable", input, trait_names,
                                 traitDataInput, traitSignalInput)
  
  pairsOutput <- foundr::shinyTraitPairs("shinyPairs", input, trait_names,
                                 tableOutput)
  
  # Trait Names.
  trait_names <- shiny::reactive({
    shiny::req(corTableOutput())
    
    c(foundr::unite_datatraits(corTableOutput(), key = TRUE),
      rel_traitsOutput())
  },
  label = "trait_names")
  
  # Related Datasets.
  datasets <- shiny::reactive({
    unique(traitStats$dataset)
  })
  output$reldataset <- renderUI({
    shiny::selectInput("reldataset", "Related Datasets:",
                       datasets(), datasets()[1], multiple = TRUE)
  })
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices, inline = TRUE)
  })

  # DATA OBJECTS
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
}

shiny::shinyApp(ui = ui, server = server)
