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
customSettings <- list(condition = "diet")

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
        
        # Related Datasets and Traits.
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput("reldataset")),
          shiny::column(6, shinyTraitNamesUI("shinyRelTraits"))),
        
        foundr::shinyCorTableUI("shinyCorTable"),
        shiny::sliderInput("mincor", "Minimum:", 0, 1, 0.7),
        
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
  
  # MODULES
  # Order Traits by Stats.
  orderOutput <- foundr::shinyTraitOrder("shinyOrder", traitStats, traitSignal)

  # Key Trait.
  keyTraitOutput <- foundr::shinyTraitNames("shinyKeyTrait", input, orderOutput)
  
  # Correlation Table.
  corTableOutput <- foundr::shinyCorTable("shinyCorTable", input, input,
                                  keyTraitOutput, traitSignal,
                                  customSettings)
  # Related Traits.
  relTraitsOutput <- shinyTraitNames("shinyRelTraits", input,
                                     corTableOutput, TRUE)
  # Trait Table.
  tableOutput <- foundr::shinyTraitTable("shinyTable", input, input,
                                         keyTraitOutput, relTraitsOutput,
                                         traitData, traitSignal)
  # Solo plot.
  solosOutput <- foundr::shinyTraitSolos("shinySolos", input, tableOutput)

  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices, inline = TRUE)
  })
  
  # Related Datasets.
  datasets <- shiny::reactive({
    unique(traitStats$dataset)
  })
  output$reldataset <- renderUI({
    shiny::selectInput("reldataset", "Related Datasets:",
                       datasets(), datasets()[1], multiple = TRUE)
  })
}

shiny::shinyApp(ui = ui, server = server)
