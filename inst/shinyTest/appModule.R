dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitSignal <- readRDS(file.path(dirpath, "liverSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "liverStats.rds"))

################################################################

title <- "Test Shiny Module"

ui <- function() {
  
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("dataset"),
        shiny::fluidRow(
          shiny::column(8,
            foundr::shinyContrastModuleInput("shinyContrastModule")),
          shiny::column(4, shiny::uiOutput("term"))),

        shiny::sliderInput("p.value", "p.value:", 0, 1, 0.05, 0.05)
        ),
      
      shiny::mainPanel(
#        shiny::uiOutput("intro"),
        foundr::shinyContrastModuleOutput("shinyContrastModule")
      )
    ))
}

server <- function(input, output, session) {
  
  # *** need persistent module choice (reactiveVal)
  # *** table from traits()
  # *** sliders from Volcano
  
  # MODULE
  moduleOutput <- foundr::shinyContrastModule("shinyContrastModule",
    input, input, traitContrPval, modules)
  
  # Datasets.
  output$dataset <- renderUI({
    datasets <- unique(traitStats$dataset)
    shiny::selectInput("dataset", "Datasets:", datasets, "LivMet",
                       multiple = TRUE)
  })
  output$term <- renderUI({
    terms <- unique(traitStats$term)
    shiny::selectInput("term", "Term:", terms, terms[1])
  })
  
  traitSignalInput <- reactive({
    shiny::req(input$dataset)
    
    dplyr::filter(traitSignal, dataset %in% input$dataset)
  })
  traitStatsInput <- reactive({
    shiny::req(input$dataset)
    
    dplyr::filter(traitStats, dataset %in% input$dataset)
  })
  traitContr <- reactive({
    shiny::req(traitSignalInput(), traitStatsInput(), input$term)
    
    foundr::conditionContrasts(traitSignalInput(), traitStatsInput(),
                                 termname = input$term)
  })
  traitContrPval <- reactive({
    shiny::req(traitContr(), input$p.value)
    
    dplyr::filter(shiny::req(traitContr()), .data$p.value <= input$p.value)
  })
  
  output$intro <- renderUI({
    shiny::renderText("intro", {
      paste("Guideline is to have power of 6 and size of 4 for unsigned modules.",
            "Power larger than 30 is too high.",
            "Topology curves are not necessarily helpful.",
            "This tool lets one explore the topology a bit.")
    })
  })

  modules <- shiny::reactive({
    shiny::req(traitContrPval())
    
    tryCatch(modulr::listof_wgcnaModules(
      dplyr::select(traitContrPval(), -p.value),
      params = list(power = 6, minSize = 4)),
      error = function(e) NULL)
  })
}

shiny::shinyApp(ui = ui, server = server)
