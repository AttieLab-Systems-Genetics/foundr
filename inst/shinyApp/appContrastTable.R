dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitSignal <- readRDS(file.path(dirpath, "liverSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "liverStats.rds"))
traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))

################################################################

title <- "Shiny Module Contrast Table"

ui <- function() {
  
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("dataset"),
        shiny::uiOutput("sex"),
        foundr::shinyContrastTableInput("shinyContrastTable"),
        shiny::uiOutput("module")
      ),
      
      shiny::mainPanel(
        shiny::tagList(
        foundr::shinyContrastTableUI("shinyContrastTable"),
        shiny::h3("Contrasts"),
        shiny::uiOutput("table")))
    ))
}

server <- function(input, output, session) {
  
  # MODULE
  # Contrast Module Table
  tableContrast <- foundr::shinyContrastTable("shinyContrastTable",
    input, input, traitSignal, traitStats, customSettings, keepDatatraits)

  # SERVER-SIDE INPUTS
  output$dataset <- shiny::renderUI({
    # Dataset selection.
    datasets <- unique(traitStats$dataset)
    
    # Get datasets.
    shiny::selectInput("dataset", "Datasets:",
                       datasets, datasets[1], multiple = TRUE)
  })
  sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
  output$sex <- shiny::renderUI({
    shiny::selectInput("sex", "", as.vector(sexes))
  })
  
  datamodule <- shiny::reactive({
    traitModule[shiny::req(input$dataset)]
  })
  output$module <- shiny::renderUI({
    shiny::selectizeInput("module", "Module:", NULL)
  })
  shiny::observeEvent(
    shiny::req(datamodule(), input$sex, input$dataset),
    {
      if(foundr:::is_sex_module(datamodule())) {
        sextraits <- NULL
      } else {
        sextraits <- unique(datamodule()[[input$dataset]]$value$modules$module)
      }
      shiny::updateSelectizeInput(session, "module", choices = sextraits,
                                  selected = "", server = TRUE)
    })
  keepDatatraits <- reactive({
    shiny::req(input$dataset, datamodule())
    if(foundr:::is_sex_module(datamodule()) || !shiny::isTruthy(input$module))
      return(NULL)
    foundr:::keptDatatraits(traitModule, input$dataset[1], input$module)
  })
  
  # Output Table
  output$table <- shiny::renderUI({
    shiny::req(tableContrast(), input$dataset, input$sex)
    tbl <- dplyr::filter(tableContrast(), sex %in% input$sex)
    if(foundr:::is_sex_module(datamodule())) {
      tbl <- dplyr::filter(tbl, dataset %in% input$dataset)
    }
    DT::renderDataTable(foundr::summary_conditionContrasts(tbl, ntrait = 0))
  })
}

shiny::shinyApp(ui = ui, server = server)
