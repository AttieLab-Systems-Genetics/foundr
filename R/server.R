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
#' @importFrom shiny checkboxGroupInput hideTab observeEvent reactive renderUI
#'             req showTab
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#'
server <- function(input, output, session,
                   traitData = NULL, traitSignal = NULL, traitStats = NULL,
                   customSettings = NULL) {
  
  # CALL MODULES
  traitOutput <- shinyTraitPanel("tabTraits", input,
                                 traitData,
                                 traitSignalInput, traitStatsInput,
                                 customSettings)
  timeOutput <- shinyTimesPanel("tabTimes", input, 
                                traitData, traitSignalInput, traitStatsInput)
  volcanoOutput <- shinyVolcano("tabVolcano", input, traitStats,
                                customSettings)
  
  
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
      if(length(timetraits_all())) {
        shiny::showTab(inputId = "tabpanel", target = "Times")
      } else {
        shiny::hideTab(inputId = "tabpanel", target = "Times")
      }
    })
  timetraits_all <- shiny::reactive({
    foundr::timetraitsall(traitSignalInput())
  })
  
  output$tabInput <- shiny::renderUI({
    shiny::req(input$tabpanel)
    
    switch(shiny::req(input$tabpanel),
           Traits = shinyTraitPanelInput("tabTraits"),
           Volcano = shinyVolcanoInput("tabVolcano"),
           Times  = if(length(timetraits_all())) shinyTimesPanelInput("tabTimes"))
  })
  output$tabUI <- shiny::renderUI({
    shiny::req(input$tabpanel)
    
    switch(shiny::req(input$tabpanel),
           Traits = shinyTraitPanelUI("tabTraits"),
           Volcano = shinyVolcanoUI("tabVolcano"),
           Times  = if(length(timetraits_all())) shinyTimesPanelUI("tabTimes"))
  })

  # DATA OBJECTS
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
}
