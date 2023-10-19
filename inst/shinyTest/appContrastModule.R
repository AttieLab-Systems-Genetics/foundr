dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitSignal <- readRDS(file.path(dirpath, "liverSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "liverStats.rds"))
traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))

################################################################

title <- "Test Shiny Module"

ui <- function() {
  
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(
          shiny::column(8,
            foundr::shinyContrastTableInput("shinyContrastTable")),
          shiny::column(4,
            foundr::shinyContrastModuleInput("shinyContrastModule"))),
        shiny::numericInput("ntrait", "Traits:",
                                             20, 5, 100, 5),
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
  # *** simplify using traitModule as below
  # *** move module choice to side panel
  
  # MODULE
  # Contrast Trait Table
  contrastOutput <- foundr::shinyContrastTable("shinyContrastTable",
    input, input, traitSignal, traitStats, customSettings)
  # Contrast Modules.
  moduleOutput <- foundr::shinyContrastModule("shinyContrastModule",
    input, input, traitContrPval, traitModule)
  
  traitContrPval <- reactive({
    shiny::req(contrastOutput())
    pvalue <- attr(traitModule, "p.value") # set by construction of `traitModule`
    
    dplyr::filter(shiny::req(contrastOutput()), .data$p.value <= pvalue)
  })
  
  output$intro <- renderUI({
    shiny::renderText("intro", {
      paste("Guideline is to have power of 6 and size of 4 for unsigned modules.")
    })
  })
}

shiny::shinyApp(ui = ui, server = server)
