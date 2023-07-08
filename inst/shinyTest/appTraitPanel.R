dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitData <- readRDS(file.path(dirpath, "traitData.rds"))
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

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
        foundr::shinyTraitPanelUI("shinyPanel"),

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
        foundr::shinyTraitPanelOutput("shinyPanel")
      )))
}

server <- function(input, output, session) {
  
  # CALL MODULES
  panelOutput <- foundr::shinyTraitPanel("shinyPanel", input,
                                         traitDataInput,
                                         traitSignalInput,
                                         traitStatsInput)
  
  # *** return is corrupted somehow
  # *** need to arrange NULL pairsplot

  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput(
      "strains", "Strains",
      choices = choices, selected = choices, inline = TRUE)
  })

  # DATA OBJECTS
  traitDataInput <- shiny::reactive({
    traitData
  })
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
  
  # RETURN OBJECTS FROM MODULES
  trait_names <- shiny::reactive({
    shiny::req(panelOutput())
    
    c(panelOutput()$traits)
  },
  label = "trait_names")

  # I/O FROM MODULE
  
  # MODULE INPUT: File Prefix
  output$filename <- renderUI({
    shiny::req(trait_names())
    
    filename <- paste0(
      "module_",
      paste(trait_names(), collapse = "."))
    shiny::textAreaInput("filename", "File Prefix", filename)
  })

  # MODULE OUTPUT: Plot
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$filename), ".pdf")
    },
    content = function(file) {
      shiny::req(panelOutput())
      grDevices::pdf(file, width = 9, height = 6)
      print(panelOutput()$solos)
      if(!is.null(panelOutput()$pairs))
        print(panelOutput()$pairs)
      grDevices::dev.off()
    })

  # MODULE OUTPUT: DataTable
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$filename), ".csv")
    },
    content = function(file) {
      shiny::req(panelOutput())
      utils::write.csv(
        panelOutput()$object,
        file, row.names = FALSE)
    })
}

shiny::shinyApp(ui = ui, server = server)
