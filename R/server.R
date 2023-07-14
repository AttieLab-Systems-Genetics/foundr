#' Shiny Server for foundr Package
#'
#' @param input,output,session shiny server parameters
#' @param traitData,traitSignal,traitStats trait data frame
#' @param customSettings list of custom settings
#'
#' @return reactive server
#' 
#' @export
#' 
#' @importFrom shiny downloadHandler reactive reactiveValues renderUI req textAreaInput
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#'
server <- function(input, output, session,
                   traitData = NULL, traitSignal = NULL, traitStats = NULL,
                   customSettings = NULL) {
  
  # CALL MODULES
  panelOutput <- shinyTraitPanel("tabTrait", input,
                                 traitData,
                                 traitSignalInput, traitStatsInput)
  timeOutput <- shinyTimesPanel("tabTimes", input, 
                                traitData, traitSignalInput, traitStatsInput)
  
  output$intro <- foundrIntro(customSettings$help)
  
  # SERVER-SIDE INPUTS
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput(
      "strains", "Strains",
      choices = choices, selected = choices, inline = TRUE)
  })
  
  # DATA OBJECTS
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
      print(panelOutput()$corsplot)
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
