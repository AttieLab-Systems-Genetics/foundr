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
  
  # Entry key
  entrykey <- shiny::reactive({
    out <- !shiny::isTruthy(customSettings$entrykey)
    if(!out & shiny::isTruthy(input$appEntry)) {
      out <- (input$appEntry == customSettings$entrykey)
    }
    out
  })

  # Hide all tabs unless Entry word provided.
  shiny::observeEvent(
    shiny::tagList(input$height, entrykey()),
    {
      if(entrykey()) {
        shiny::showTab(inputId = "tabpanel", target = "Traits")
        if(length(timetraits_all()))
          shiny::showTab(inputId = "tabpanel", target = "Times")
        shiny::showTab(inputId = "tabpanel", target = "Volcano")
        shiny::showTab(inputId = "tabpanel", target = "About")
      } else {
        shiny::hideTab(inputId = "tabpanel", target = "Traits")
        shiny::hideTab(inputId = "tabpanel", target = "Times")
        shiny::hideTab(inputId = "tabpanel", target = "Volcano")
        shiny::hideTab(inputId = "tabpanel", target = "About")
      }
    })
  # Hide Time tab unless we have time entries.
  shiny::observeEvent(
    input$height,
    {
      if(entrykey()) {
        if(length(timetraits_all())) {
          shiny::showTab(inputId = "tabpanel", target = "Times")
        } else {
          shiny::hideTab(inputId = "tabpanel", target = "Times")
        }}
    })
  timetraits_all <- shiny::reactive({
    foundr::timetraitsall(traitSignalInput())
  })
  
  output$sideInput <- shiny::renderUI({
    shiny::req(input$tabpanel)

    if(entrykey()) {
      shiny::tagList(
        switch(shiny::req(input$tabpanel),
               Traits = shinyTraitPanelInput("tabTraits"),
               Volcano = shinyVolcanoInput("tabVolcano"),
               Times  = if(length(timetraits_all())) {
                 shinyTimesPanelInput("tabTimes") 
               }),
        
        shiny::tagList(
          shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
          shiny::checkboxInput("facet", "Facet by strain?", FALSE),
          shiny::sliderInput("height", "Plot height (in):", 3, 10, 6,
                             step = 1)),
        
        switch(shiny::req(input$tabpanel),
               Traits = shinyTraitPanelUI("tabTraits"),
               Volcano = shinyVolcanoUI("tabVolcano"),
               Times  = if(length(timetraits_all())) {
                 shinyTimesPanelUI("tabTimes")}
        )
      )
    }
  })
  # Don't show Entry Key if there is no need.
  output$entrykey <- shiny::renderUI({
    if(shiny::isTruthy(customSettings$entrykey))
      shiny::textInput("appEntry", "Entry Key:")
  })

  # DATA OBJECTS
  traitSignalInput <- shiny::reactive({
    traitSignal
  })
  traitStatsInput <- shiny::reactive({
    traitStats
  })
}
