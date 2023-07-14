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
  #   input$
  #
  # OUTPUTS (see shinyTraitPairs)
  #   output$filename: 
  #   output$downloadPlot
  #   output$downloadTable
  
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shinyTraitPanelUI("tabTrait"),
        
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::uiOutput("filename")), # See MODULE INPUT below
          shiny::column(
            3,
            shiny::downloadButton("downloadPlot", "Plots")),
          shiny::column(
            3,
            shiny::downloadButton("downloadTable", "Data")))
      ),
      
      shiny::mainPanel(
        shiny::tabsetPanel(
          type = "tabs", header = "", id = "tabpanel",
          shiny::tabPanel("Traits", shinyTraitPanelOutput("tabTrait")),
          shiny::tabPanel("Times",  shinyTimesPanelUI("tabTimes")),
          shiny::tabPanel("About",  shiny::uiOutput("intro"))
      ))))
}
