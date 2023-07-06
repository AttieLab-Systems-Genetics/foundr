#' Shiny Module UI for traitSolos
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' 
#' @rdname shinyTraitSolos
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitSolosUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shiny_solosPlot"))
}

#' Shiny Module Server for trait solos Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer`
#' @param traitSolosObject reactive objects from `foundrServer`
#'
#' @return reactive object
#' 
#' @importFrom shiny observeEvent plotOutput radioButtons reactive reactiveVal 
#'             renderPlot renderUI req tagList uiOutput
#' @importFrom DT renderDataTable dataTableOutput
#' @export
#'

shinyTraitSolos <- function(input, output, session,
                            main_par, traitSolosObject) {
  ns <- session$ns

  # INPUTS
  # Main inputs:
  #   main_par$facet
  #   main_par$height

  # OUTPUTS
  # output$shiny_solosPlot

  # RETURNS
  # solosPlot()

  #############################################################
  # Output: Plots or Data
  output$shiny_solosPlot <- shiny::renderUI({
    shiny::req(main_par$height)
    
    shiny::plotOutput(ns("solosPlot"), height = paste0(main_par$height, "in"))
  })
  
  # Plot
  solosPlot <- shiny::reactive({
    shiny::req(traitSolosObject())
    
    ggplot_traitSolos(
      traitSolosObject(),
      facet_strain = main_par$facet,
      boxplot = TRUE)
  })
  output$solosPlot <- shiny::renderPlot({
    print(solosPlot())
  })

  #############################################################

  solosPlot
}
