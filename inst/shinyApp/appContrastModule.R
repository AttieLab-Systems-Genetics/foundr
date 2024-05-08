dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitSignal <- readRDS(file.path(dirpath, "liverSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "liverStats.rds"))
traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))

################################################################

title <- "Shiny Module Contrast Module"

ui <- function() {
  
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("dataset"),
        foundr::shinyContrastTableInput("shinyContrastTable")
      ),
      
      shiny::mainPanel(
        shiny::tagList(
          foundr::shinyContrastModuleInput("shinyContrastModule"),
          shiny::fluidRow(
            shiny::column(4, shiny::uiOutput("sex")),
            shiny::column(8, shiny::uiOutput("module"))),
          foundr::shinyContrastModuleOutput("shinyContrastModule")
        )
      )
    ))
}

server <- function(input, output, session) {
  
  # This now works with two calls to shinyContrastTable.
  # However, it is slow because the second call to create `traitContrast`
  # pulls in all 52965 traits.
  # Would like to only pull in those for selected module.
  # But this requires moving `module` parameter up one level (to panel).
  # This gets tricky because also need to move up `sex`.
  
  # Other consideration: modifying this stuff to do same thing with stats
  # across traits in a module.
  
  # *** need persistent module choice (reactiveVal)
  # *** table from traits()
  # *** sliders from Volcano
  # *** simplify using traitModule as below
  # *** move module choice to side panel
  
  # *** need new handling for Mark's modules
  # *** see `eigen_traits_contr_object` in eigen_traits.R
  # *** and mods to `eigen_traits_dataset_value`
  
  # MODULE
  # Contrast Module Table
  moduleContrast <- foundr::shinyContrastTable("shinyContrastTable",
    input, input, traitSignal, traitStats, customSettings)
  # Contrast Trait Table
  traitContrast <- foundr::shinyContrastTable("shinyContrastTable",
    input, input, traitSignal, traitStats, customSettings, keepDatatraits)
  # Contrast Modules.
  # *** problem for MixMod is that traitContrast and moduleContrast may be wrong.
  moduleOutput <- foundr::shinyContrastModule("shinyContrastModule",
    input, input, traitModule, moduleContrast, traitContrast)
  
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
  output$module <- shiny::renderUI({
    shiny::selectizeInput("module", "Module:", NULL)
  })
  shiny::observeEvent(
    shiny::req(datatraits(), input$dataset, input$sex),
    {
      # First zero out input$module.
      shiny::updateSelectizeInput(session, "module",
                                  selected = "", server = TRUE)
      # Then set choices.
      shiny::updateSelectizeInput(session, "module", choices = datatraits(),
                                  selected = "", server = TRUE)
  })

  datamodule <- shiny::reactive({
    traitModule[shiny::req(input$dataset[1])]
  })
  datatraits <- shiny::reactive({
    shiny::req(input$sex, input$dataset, datamodule())
    
    if(foundr:::is_sex_module(datamodule())) {
      out <- unique(datamodule()[[input$dataset[1]]][[input$sex]]$modules$module)
      paste0(input$dataset[1], ": ", names(sexes)[match(input$sex, sexes)], "_", out)
    } else {
      paste0(input$dataset[1], ": ", unique(datamodule()[[input$dataset]]$value$modules$module))
    }
  }, label = "datatraits")
  
  keepDatatraits <- reactive({
    module <- NULL
    if(shiny::isTruthy(input$module))
      module <- input$module
    
    foundr:::keptDatatraits(traitModule, shiny::req(input$dataset)[1], module)
  })
}

shiny::shinyApp(ui = ui, server = server)
