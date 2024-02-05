dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitData <- readRDS(file.path(dirpath, "traitData.rds"))
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test Shiny Trait Solos"

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
        shiny::selectInput("trait","Traits:",c("Enrich: 15N2-Urea_enrichment_120_18wk","Enrich: N-Methyl-D3-Creatinine_enrichment_0_18wk","Enrich: 5,5,5-D3-Leucine_enrichment_120_18wk","Enrich: Trimethyl-D9-Carnitine_enrichment_60_18wk")),
        foundr::shinyTraitTableUI("shinyObject"),
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::uiOutput("filename")), # See MODULE INPUT below
          shiny::column(
            3,
            shiny::downloadButton("downloadPlot", "Plots")),
          shiny::column(
            3,
            shiny::downloadButton("downloadTable", "Data")))
      ),

      shiny::mainPanel(
        shiny::tagList(
          foundr::shinyTraitSolosUI("shinySolos"),
          foundr::shinyTraitTableOutput("shinyObject")
        )
      )))
}

server <- function(input, output, session) {
  
  # MODULES
  tableOutput <- foundr::shinyTraitTable("shinyObject", input, input,
                                         keyTrait, relTraits,
                                         traitData, traitSignal)
  solosOutput <- foundr::shinyTraitSolos("shinySolos", input, tableOutput)
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices, inline = TRUE)
  })

  # REACTIVES
  keyTrait <- shiny::reactive({
    shiny::req(input$trait)[1]
  })
  relTraits <- shiny::reactive({
    NULL
  })
  datasets <- shiny::reactive({
    shiny::req(tableOutput())
    
    unique(tableOutput()$dataset)
  })
}

shiny::shinyApp(ui = ui, server = server)
