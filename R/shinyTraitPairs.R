#' Shiny Module UI for TraitPairs
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' 
#' @rdname shinyTraitPairs
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitPairsUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shiny_traitPairs"))
}

#' Shiny Module Server for trait solos Plots
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param panel_par,main_par reactive arguments from `foundrServer`
#' @param trait_names reactive with trait names
#' @param traitSolosObject reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyTaitSolosUI`
#' 
#' @importFrom shiny isTruthy moduleServer observeEvent plotOutput radioButtons 
#'             reactive renderPlot renderUI req tagList uiOutput
#' @importFrom DT renderDataTable dataTableOutput
#' @export
#'
shinyTraitPairs <- function(id, panel_par, main_par, trait_names, traitSolosObject) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # Main inputs:
    #   main_par$height
    #   panel_par$facet
    # TraitPairs inputs:
    #   input$pair (obsolete)
    
    # OUTPUTS
    # output$pairsPlot
    
    # RETURNS
    # pairsPlot()
    
    # Output: Plots or Data
    output$shiny_traitPairs <- shiny::renderUI({
      shiny::req(trait_names(), traitSolosObject())
      
      shiny::plotOutput(ns("pairsPlot"), height = paste0(main_par$height, "in"))
    })
    
    # Plot
    pairsPlot <- shiny::reactive({
      shiny::req(traitSolosObject(), trait_names())
      
      ggplot_traitPairs(
        traitPairs(
          traitSolosObject(),
          trait_names(),
          pair()),
        facet_strain = shiny::isTruthy(panel_par$facet),
        parallel_lines = TRUE)
      },
      label = "pairsPlot")
    output$pairsPlot <- shiny::renderPlot({
      print(pairsPlot())
    })
    
    # INPUT PAIR
    pair <- shiny::reactive({
      trait_pairs(trait_names())
      },
      label = "pair")
    # Obsolete
    output$pair <- shiny::renderUI({
      # Somehow when main_par$height is changed this is reset.
      shiny::req(trait_names())
      if(length(trait_names()) < 2)
        return(NULL)
      choices <- trait_pairs(trait_names(), key = FALSE)
      
      shiny::selectInput(
        "pair", "Select pairs for scatterplots",
        choices = choices, selected = choices[1],
        multiple = TRUE, width = '100%')
    })
    
    #############################################################
    
    pairsPlot
  })
}
