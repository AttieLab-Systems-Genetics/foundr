dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test Shiny Trait Names"

reactlog::reactlog_enable()

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
        shiny::uiOutput("name")),
      
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
  
  datasets <- shiny::reactive({
    unique(traitStats$dataset)
  })

  # INPUTS  
  output$inputs <- renderUI({
    shiny::selectInput("dataset", "Dataset:", datasets(), input$dataset)
  })

  # DATA OBJECTS 
  traitStatsInput <- shiny::reactive({
    shiny::req(input$dataset)
    dplyr::filter(
      traitStats,
      .data$dataset %in% input$dataset)
  })

  # CALL MODULE
  moduleOutput <- shiny::callModule(
    foundr::shinyTraitNames, "shinyTest", 
    traitStatsInput, traitStatsInput)
  
  # I/O FROM MODULE
  output$name <- renderUI({
    shiny::req(moduleOutput())
    name <- paste(moduleOutput(), collapse = ", ")
    shiny::textAreaInput("name", "Traits", name)
  })
}

shiny::shinyApp(ui = ui, server = server)
