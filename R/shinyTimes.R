#' Shiny Module UI for Times Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimes
#' @export
#'
shinyTimesUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("shiny_time"))
  )
}

#' Shiny Module Server for Times Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer` 
#' @param traitDataInput,traitSignalInput,traitStatsInput reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyTimesUI`
#' @importFrom shiny column fluidRow observeEvent plotOutput reactive
#'             reactiveVal renderPlot renderUI req selectInput selectizeInput
#'             tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @importFrom cowplot plot_grid
#' @export
#'
shinyTimes <- function(input, output, session,
                       main_par,
                       traitDataInput, traitSignalInput, traitStatsInput) {
  ns <- session$ns
  
  # INPUTS
  # passed inputs: (see shinyapp.R)
  #   main_par$height (see shinyapp.R::foundrUI sidebarPanel)
  #   main_par$facet (see shinyapp.R::foundrServer output$settings)
  #.  main_par$strains (see shinyapp.R::foundrServer output$strains)
  # local inputs: (see output$tab_time below)
  #   time
  #   time_trait
  #   time_response
  
  # OUTPUTS
  # output$tab_time is returned via shinyTimesUI
  # output$timeplots is displayed in parent output$tab_time
  
  # RETURNS
  # list with
  #   timeplots() (see timeplots() below)
  #   statstable() (see statstable() below)

  # Not used for now. Refer to shinyapp.R
  output$shiny_time <- shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectInput(ns("time"), "Time Unit:",
                             c("week", "minute","week_summary","minute_summary"))),
        shiny::column(
          4,
          shiny::selectizeInput(ns("time_trait"), "Traits:",
                                NULL, multiple = TRUE)),
        shiny::column(
          4,
          shiny::selectInput(ns("time_response"), "Response:",
                             c("value", "cellmean", "signal")))),
      
      shiny::uiOutput(ns("timeplots"))
    )
  })
  
  # Main return
  output$timeplots <- shiny::renderUI({
    shiny::req(main_par$height)
    shiny::tagList(
      shiny::plotOutput(ns("timeplot"), height = paste0(main_par$height, "in")),
      
      DT::renderDataTable(
        statstable(),
        escape = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)))
  })
  
  statstable <- shiny::reactive({
    stats_time_table(traitTimeSum())
  })
  timeplots <- shiny::reactive({
    shiny::req(traitTime(), traitTimeSum(), main_par$strains)

    ggplot_traitTimes(
      traitTime(),
      traitTimeSum(),
      facet_strain = main_par$facet)
  })
  output$timeplot <- shiny::renderPlot({
    print(timeplots())
  })
  shiny::observeEvent(
    timetraits(),
    {
      # Use current selection of trait_selection().
      # But make sure they are still in the traitNamesArranged().
      selected <- timetrait_selection()
      choices <- timetraits()
      selected <- selected[selected %in% choices]
      if(!length(selected))
        selected <- NULL
      shiny::updateSelectizeInput(session, "time_trait", choices = choices,
                                  server = TRUE, selected = selected)
    })
  timetrait_selection <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$time_trait, {
    timetrait_selection(input$time_trait)
  })
  
  timetraits <- shiny::reactive({
    shiny::req(input$time)
    foundr::timetraits(traitSignalInput(), input$time)
  })
  traitTime <- shiny::reactive({
    shiny::req(timetrait_selection(), input$time_response, input$time)
    foundr::traitTimes(
      traitDataInput(), traitSignalInput(),
      timetrait_selection(), input$time_response, input$time,
      strains = main_par$strains)
  })
  traitTimeSum <- shiny::reactive({
    shiny::req(timetrait_selection(), input$time)
    foundr::traitTimes(
      traitStatsInput(),
      timetrait_selection(), "p.value", input$time, "terms")
  })
  
  # List returned
  reactive({
    shiny::req(timetrait_selection(), timeplots(), statstable())
    list(
      timeplots = timeplots(),
      statstable = statstable(),
      timetraits = timetrait_selection())
  })
}

      
  