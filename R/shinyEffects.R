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
#' @param traitDataInput,traitSignalInput,traitStatsInput reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyEffectsUI`
#' @importFrom shiny column fluidRow observeEvent plotOutput reactive
#'             reactiveVal renderPlot renderUI req selectInput selectizeInput
#'             tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @importFrom dplyr arrange filter mutate rename
#' @importFrom ggplot2 aes element_text facet_grid geom_boxplot
#'             geom_jitter ggplot scale_size theme
#' @importFrom tidyr pivot_longer unite
#' @importFrom rlang .data
#'
#' @export
#'
shinyEffects <- function(input, output, session,
                         main_par,
                         traitDataInput, traitSignalInput, traitStats) {
  ns <- session$ns



  # INPUTS
  # Effects inputs: (see output$tab_effects below)
  #   input$corterm
  #   input$mincor
  #   input$volsd
  #   input$volpval
  #   input$term

  # OUTPUTS
  # output$tab_effects is returned via shinyEffectsUI
  # output$effects is displayed in parent output$tab_effects
  # output$corplot
  # output$tablesum


  # RETURNS
  # list with
  #   timeplots() (see timeplots() below)
  #   statstable() (see statstable() below)



  output$shiny_effects <- shiny::renderUI({
    trstats <- shiny::req(traitStats())
    shiny::tagList(

      # Condition for plot based on `interact` parameter.
      shiny::uiOutput(ns("effects")),

      # Data table.
      DT::dataTableOutput(ns("tablesum")))
  })



  ###############
  output$corplot <- shiny::renderPlot({
    shiny::req(input$mincor, corobject())
    if(is.null(corobject()) || !nrow(corobject()))
      return(print(plot_null("Need to specify at least one trait.")))

    print(corplot())
  })

  ######### CHANGE THIS  #########

  # corobject <- shiny::reactive({
  #   bestcor(traitSignalSelectType(),
  #           trait_selection(),
  #           input$corterm)
  # })


  corobject <- NULL

  ####################################
  effectsplot <- shiny::reactive({
    if(shiny::isTruthy(corobject()))
      corobj <- corobject()
    else
      corobj <- NULL
    print(effectplot(traitStatsSelectType(), trait_selection(),
                     effecthelper(corobj, input$mincor)))
  })

  output$effects <- shiny::renderPlot({
    effectsplot()
  })

  output$tablesum <- DT::renderDataTable(
    {
      shiny::req(traitStatsSelectType(), input$volsd, input$volpval, input$term)
      summary_strainstats(
        # do we need this?
        #  mutate_datasets(
        traitStatsSelectType(),
        #    customSettings$dataset),
        terms = input$term,
        threshold = c(SD = input$volsd, p = 10 ^ (-input$volpval)))
    },
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))

  # List returned
  reactive({
    shiny::req(timetrait_selection(), timeplots(), statstable())
    list(
      plot = timeplots(),
      table = statstable(),
      traits = timetrait_selection())
  })
}


