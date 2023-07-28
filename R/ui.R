#' Shiny UI for foundr Package
#'
#' @param title title for app
#' 
#' @return reactive UI
#' @export
#' @importFrom shiny checkboxInput column fluidPage fluidRow mainPanel
#'             sidebarLayout sidebarPanel sliderInput tabPanel tabsetPanel
#'             titlePanel uiOutput
#'
ui <- function(title) {
  # INPUTS
  #   input$facet: Facet by strain?
  #   input$strains: Strains to select
  #   input$height: Plot Height
  #
  # OUTPUTS (see shinyTraitPairs)
  #   output$filename: 
  #   output$downloadPlot
  #   output$downloadTable
  
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("tabInput"),
        
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        
        shiny::uiOutput("tabUI"),
      ),
      
      shiny::mainPanel(
        shiny::tabsetPanel(
          type = "tabs", header = "", id = "tabpanel",
          shiny::tabPanel("Traits", shinyTraitPanelOutput("tabTraits")),
          shiny::tabPanel("Volcano",  shinyVolcanoOutput("tabVolcano")),
          shiny::tabPanel("Times",  shinyTimesPanelOutput("tabTimes")),
          shiny::tabPanel("About",  shiny::uiOutput("intro"))
      ))))
}
