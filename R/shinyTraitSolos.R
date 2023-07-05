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
shinyTraitSoloUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shiny_traitSolos"))
}

#' Shiny Module Server for trait solos Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer`
#' @param trait_names reactive with trait names.
#' @param traitData,traitSignal reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyTaitSolosUI`
#' 
#' @importFrom shiny observeEvent plotOutput radioButtons reactive reactiveVal 
#'             renderPlot renderUI req tagList uiOutput
#' @importFrom DT renderDataTable dataTableOutput
#' @export
#'

shinyTraitSolo <- function(input, output, session,
                           main_par, trait_names,
                           traitData,traitSignal,datasets_selected) {
  ns <- session$ns

  # INPUTS
  # Main inputs:
  #   main_par$trait (passed as trait_names())
  #   main_par$strains
  #   main_par$facet
  #   main_par$height
  # TraitSolo inputs:
  #   input$butresp

  # OUTPUTS
  # output$distPlot
  # output$plots
  # output$datatable

  # RETURNS
  # list with elements
  #   plot = distPlot()
  #   table = datameans()
  #   traits = datasets_selected()

  #############################################################
  # Output: Plots or Data
  output$shiny_traitSolos <- shiny::renderUI({
    shiny::tagList(
      shiny::radioButtons(ns("butresp"), "Response",
                          c("value", "cellmean", "signal"),
                          "value", inline = TRUE),

      shiny::uiOutput(ns("plots")),
      DT::dataTableOutput(ns("datatable"))
    )
  })

  trait_selection <- shiny::reactiveVal(NULL)
  shiny::observeEvent(trait_names(), {
    trait_selection(trait_names())
  })

  # Trait Data for Selected Traits
  traitSolosObject <- shiny::reactive({
    traitSolos(shiny::req(traitData()),
               shiny::req(traitSignal()),
               shiny::req(trait_selection()),
               shiny::req(input$butresp),
               shiny::req(main_par$strains))
  })

  # Plots
  distPlot <- shiny::reactive({
    shiny::req(traitSolosObject())
    
    ggplot_traitSolos(
      traitSolosObject(),
      facet_strain = main_par$facet,
      boxplot = TRUE)
  })
  output$distPlot <- shiny::renderPlot({
    print(distPlot())
  })
  output$plots <- shiny::renderUI({
    shiny::req(main_par$height)
    shiny::plotOutput(ns("distPlot"), height = paste0(main_par$height, "in"))
  })

  # Data Table
  datameans <- shiny::reactive({
    shiny::req(trait_selection(), main_par$strains, input$butresp)

    summary(traitSolosObject())
  })
  output$datatable <- DT::renderDataTable(
    shiny::req(datameans()),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))

  #############################################################

  # List returned
  reactive({
    shiny::req(distPlot(), datameans(), datasets_selected())
    list(
      plot = print(distPlot()),
      table = datameans(),
      traits = datasets_selected())
  })

}
