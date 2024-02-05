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
  #   input$height: Plot Height
  #
  # OUTPUTS
  #   output$filename: 
  #   output$downloadPlots
  #   output$downloadTables
  
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("sideInput"),
        shiny::uiOutput("entrykey")
      ),
      
      shiny::mainPanel(
        shiny::uiOutput("mainOutput"))))
}
