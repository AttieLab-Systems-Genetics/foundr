#' Shiny Module UI for Effects Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyEffects
#' @export
#'
shinyEffectsUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("shiny_effects"))
  )
}

#' Shiny Module Server for effects Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer`
#' @param traitStatsSelectType,traitStatsArranged reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyEffectsUI`
#' @importFrom reactiveVal req renderUI tagList uiOutput renderPlot reactive
#'             tagList uiOutput updateSelectizeInput sliderInput renderUI
#' @importFrom DT renderDataTable dataTableOutput
#' @export
#'
shinyEffects <- function(input, output, session,
                         main_par,
                         traitStatsSelectType, traitStatsArranged) {
  ns <- session$ns

  corobject <- NULL
  trait_selection <- shiny::reactiveVal(NULL)

  # INPUTS
  # Effects inputs: (see output$tab_effects below)
  #   input$corterm
  #   input$mincor
  #   input$term

  # OUTPUTS
  # output$tab_effects is returned via shinyEffectsUI
  # output$effectsplot is displayed in parent output$tab_effects
  # output$tablesum


  # RETURNS
  # list with
  #   timeplots() (see timeplots() below)
  #   statstable() (see statstable() below)



  output$shiny_effects <- shiny::renderUI({
    trstats <- shiny::req(traitStatsSelectType())
    shiny::tagList(

      # Condition for plot based on `interact` parameter.
      shiny::uiOutput(ns("effects")),

      # Data table.
      DT::dataTableOutput(ns("tablesum")))
  })



  ######### CHANGE THIS  #########

  corobject <- shiny::reactive({
    bestcor(traitSignalSelectType(),
            trait_selection(),
            input$corterm)
  })


  #corobject <- NULL

  ####################################
  # effectsplot <- shiny::reactive({
  #   if(shiny::isTruthy(corobject()))
  #     corobj <- corobject()
  #   else
  #     corobj <- NULL
  #   print(effectplot(traitStatsSelectType(), trait_selection(),
  #                    effecthelper(corobj, input$mincor)))
  # })



  effectsplot <- shiny::reactive({
    corobj <- NULL

    print(effectplot(traitStatsSelectType(), trait_selection(),
                     effecthelper(corobj, input$mincor)))
  })


  ####################

  output$effects <- shiny::renderPlot({
    effectsplot()
  })

  output$tablesum <- DT::renderDataTable(
    {
      shiny::req(traitStatsSelectType())
      summary_strainstats(
        #  do we need this?
        #  mutate_datasets(
        traitStatsSelectType())
    },
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))

  datasets <- shiny::reactive({
    unique(traitStatsArranged()$dataset)
  })

  # List returned
  reactive({
    shiny::req(effectsplot(), traitStatsArranged(), datasets())
    list(
      plot = effectsplot(),
      table = traitStatsArranged(),
      traits = datasets())
  })
}


