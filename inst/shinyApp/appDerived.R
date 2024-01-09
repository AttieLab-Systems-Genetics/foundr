dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))

################################################################

title <- "Test Shiny Derived Traits"

ui <- function() {
  # INPUTS
  #   see shinyTraitNames 
  #
  # OUTPUTS (see shinyTraitNames)
  #   output$name: Traits

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("spreadsheet")),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::uiOutput("inputs"),
          
          foundr::shinyTraitNamesUI("shinyTest")))
      ))
}

server <- function(input, output, session) {
  
  # INPUTS (see shinyTraitNames)
  #   input$dataset: Dataset
  # OUTPUTS (see shinyTraitNames)
  #   output$name: Traits
  
  # MODULES
  moduleOutput <- foundr::shinyTraitNames("shinyTest", input, traitStatsInput)
  
  datasets <- shiny::reactive({
      unique(traitStats$dataset)
    },
    label = "datasets")

  # INPUTS  
  output$inputs <- renderUI({
    shiny::selectInput("dataset", "Dataset:", datasets(), multiple = TRUE)
  })

  # DATA OBJECTS 
  traitStatsInput <- shiny::reactive({
    if(shiny::isTruthy(input$dataset)) {
      dplyr::filter(
        traitStats,
        .data$dataset %in% input$dataset)
    } else {
      NULL
    }
   },
   label = "traitStatsInput")
  
  # I/O FROM MODULE
  output$spreadsheet <- shiny::renderUI({
    shiny::tagList(
      shiny::h3("Derived New Traits"),
      shiny::renderText({
        paste("Derive new traits from existing traits.",
              "Input spreadsheet with columns")
      })
    )
    shiny::fileInput("derived", "Derived Traits (CSV, XLS, XLSX):", accept = c(".csv",".xls",".xlsx",".rds"),
                     width = "100%")
  })
}

shiny::shinyApp(ui = ui, server = server)
