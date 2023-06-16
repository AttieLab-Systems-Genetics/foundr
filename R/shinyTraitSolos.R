#' Shiny Module UI for traitSolos
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitSolos
#' @export
#'
shinyTraitSoloUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("shiny_traitSolos"))
  )
}

#' Shiny Module Server for trait solos Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer`
#' @param datasets_selected traitSignalInput reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyTaitSolosUI`
#' @importFrom shiny reactiveVal req renderUI tagList uiOutput renderPlot reactive
#'             tagList renderUI column conditionalPanel fluidRow plotOutput radioButtons
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom dplyr filter
#' @export
#'

shinyTraitSolo <- function(input, output, session,
                           main_par,
                           traitSignalInput,traitDataInput,datasets_selected) {
  ns <- session$ns


  # INPUTS
  # Main inputs:
  #   main_par$trait
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
  # list with
  #   distPlot()
  #   datameans()
  #   datasets_selected()


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

  traitDataSelectType<-traitDataInput
  traitSignalSelectType<-traitSignalInput

  trait_selection <- shiny::reactiveVal(NULL)

  # now store your current selection in the reactive value
  shiny::observeEvent(main_par$trait, {
    trait_selection(main_par$trait)
  })

  # Trait Data for Selected Traits
  traitDataSelectTrait <- shiny::reactive({
    traitSolos(shiny::req(traitDataSelectType()),
               shiny::req(traitSignalSelectType()),
               shiny::req(trait_selection()),
               shiny::req(input$butresp),
               shiny::req(main_par$strains))
  })

  # Plots
  distPlot <- shiny::reactive({
    shiny::req(traitDataSelectTrait())
    ggplot_traitSolos(
      traitDataSelectTrait(),
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
    shiny::req(trait_selection(), main_par$strains)
    response <- shiny::req(input$butresp)

    summary(traitDataSelectTrait())
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
