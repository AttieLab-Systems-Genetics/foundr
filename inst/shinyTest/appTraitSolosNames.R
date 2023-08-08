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
customSettings <- NULL

################################################################

title <- "Test Shiny Trait Solos with Trait Names"

ui <- function() {
  # INPUTS
  #   input$facet: Facet by strain?
  #   input$strains: Strains to select
  #   input$height: Plot Height
  #   input$
  #
  # OUTPUTS (see shinyTraitSolos)
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
          shiny::column(6, foundr::shinyTraitNamesUI("shinyKeyTrait"))),
        
        foundr::shinyCorTableUI("shinyCorTable"),
        
        foundr::shinyTraitTableUI("shinyTable"),
        
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        
      ),

      shiny::mainPanel(
        shiny::tagList(
          foundr::shinyTraitSolosUI("shinySolos"),
          foundr::shinyTraitTableOutput("shinyTable"),
          foundr::shinyCorTableOutput("shinyCorTable")
        )
      )))
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
  # Order Traits by Stats.
  orderOutput <- foundr::shinyTraitOrder("shinyOrder", traitStatsInput)

  # Key Trait.
  keyTraitOutput <- foundr::shinyTraitNames("shinyKeyTrait", input, orderOutput)
  
  # Correlation Table.
  corTableOutput <- foundr::shinyCorTable("shinyCorTable", input, input,
                                  keyTraitOutput, traitSignalInput,
                                  customSettings)
  
  # Filter static traitData based on selected trait_names.
  traitDataInput <- shiny::reactive({
    shiny::req(trait_names())
    
    foundr::subset_trait_names(traitData, trait_names())
  })
  
  tableOutput <- foundr::shinyTraitTable("shinyTable", input, trait_names,
                                 traitDataInput, traitSignalInput)
  solosOutput <- foundr::shinyTraitSolos("shinySolos", input, tableOutput)
  
  # Trait Names.
  trait_names <- shiny::reactive({
    shiny::req(keyTraitOutput())
  },
  label = "trait_names")
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices, inline = TRUE)
  })
}

shiny::shinyApp(ui = ui, server = server)
