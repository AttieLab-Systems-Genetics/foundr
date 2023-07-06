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
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer`
#' @param trait_names reactive with trait names.
#' @param traitSolosObject reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyTaitSolosUI`
#' 
#' @importFrom shiny observeEvent plotOutput radioButtons reactive reactiveVal 
#'             renderPlot renderUI req tagList uiOutput
#' @importFrom DT renderDataTable dataTableOutput
#' @export
#'

shinyTraitPairs <- function(input, output, session,
                           main_par, trait_names,
                           traitSolosObject) {
  ns <- session$ns

  # INPUTS
  # Main inputs:
  #   main_par$trait (passed as trait_names())
  #   main_par$strains
  #   main_par$facet
  #   main_par$height
  # TraitPairs inputs:
  #   input$pair

  # OUTPUTS
  # output$scatPlot

  # RETURNS
  # list with elements
  #   plot = distPlot()

  #############################################################
  # Output: Plots or Data
  output$shiny_traitPairs <- shiny::renderUI({
    shiny::tagList(
      shiny::uiOutput("pair"),
      
      shiny::uiOutput(ns("scatplot"))
    )
  })
  
  # Pair
  output$pair <- shiny::renderUI({
    # Somehow when input$height is changed this is reset.
    shiny::req(trait_names())
    if(length(trait_names()) < 2)
      return(NULL)
    choices <- trait_pairs(trait_names())
    
    shiny::selectInput(
      "pair", "Select pairs for scatterplots",
      choices = choices, selected = choices[1],
      multiple = TRUE, width = '100%')
  })

  # Plot
  output$scatPlot <- shiny::renderUI({
    shiny::req(trait_names(), datasets_selected(), input$order)
    shiny::tagList(
      shiny::uiOutput("pair"),
      shiny::plotOutput("scatplot", height = paste0(input$height, "in"))
    )
  })
  output$scatplot <- shiny::renderPlot({
    print(scatsplot())
  })
  scatsplot <- shiny::reactive({
    shiny::req(traitSolosObject(), trait_names(), input$pair)
    
    ggplot_traitPairs(
      traitPairs(
        traitSolosObject(),
        trait_names(),
        input$pair),
      facet_strain = input$facet,
      parallel_lines = TRUE)
  })
  
  #############################################################

  # List returned
  reactive({
    shiny::req(distPlot())
    list(
      plot = print(distPlot()))
  })
}
