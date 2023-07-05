dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")
cat(dirpath, "\n", file = stderr())
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test Shiny Trait Names"

reactlog::reactlog_enable()

ui <- function() {
  # INPUTS
  #   input$facet 
  #   input$strains 
  #
  # OUTPUTS
  #   output$filename
  #   output$downloadPlot
  #   output$downloadTable
  
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("filename")),
      
      shiny::mainPanel(
        shiny::tagList(
          shiny::uiOutput("inputs"),
          
          foundr::shinyTraitNamesUI("shinyTest")))
      ))
}

server <- function(input, output, session) {
  
  datasets <- shiny::reactive(
    c("LivMet","PlaMet0","PlaMet120","Metab"))
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

  moduleOutput <- shiny::callModule(
    foundr::shinyTraitNames, "shinyTest", 
    traitStatsInput, traitStatsInput)
  
  # I/O FROM MODULE
  output$filename <- renderUI({
    shiny::req(moduleOutput())
    filename <- paste(moduleOutput(), collapse = ", ")
    shiny::textAreaInput("filename", "Traits", filename)
  })
}

shiny::shinyApp(ui = ui, server = server)
