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
  traitOutput <- shinyTraitPanel("tabTraits", input,
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
  
  # Hide Time tab unless we have time entries.
  shiny::observeEvent(
    input$height,
    {
      if(length(timetraitsall())) {
        shiny::showTab(inputId = "tabpanel", target = "Times")
      } else {
        shiny::hideTab(inputId = "tabpanel", target = "Times")
      }
    })
  timetraitsall <- shiny::reactive({
    foundr::timetraitsall(traitSignalInput())
  })
  
  output$tabInput <- shiny::renderUI({
    shiny::req(input$tabpanel)
    
    switch(shiny::req(input$tabpanel),
           Traits = shinyTraitPanelInput("tabTraits"),
           Times  = if(length(timetraitsall())) shinyTimesPanelInput("tabTimes"))
  })
  output$tabUI <- shiny::renderUI({
    shiny::req(input$tabpanel)
    
    switch(shiny::req(input$tabpanel),
           Traits = shinyTraitPanelUI("tabTraits"),
           Times  = if(length(timetraitsall())) shinyTimesPanelUI("tabTimes"))
  })

  # DATA OBJECTS
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
}
