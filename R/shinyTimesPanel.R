#' Shiny Module UI for Times Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimes
#' @export
#' @importFrom shiny NS uiOutput
#'
shinyTimesPanelUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::uiOutput(ns("shiny_times"))
  )
}

#' Shiny Module Server for Times Plots
#'
#' @param main_par reactive arguments 
#' @param traitData static objects
#' @param traitSignalInput,traitStatsInput reactive objects
#'
#' @return reactive object for `shinyTimesUI`
#' @importFrom shiny column fluidRow observeEvent plotOutput reactive
#'             reactiveVal renderPlot renderUI req selectInput selectizeInput
#'             tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTimesPanel <- function(id, main_par,
                            traitData, traitSignalInput, traitStatsInput) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # passed inputs:
    #   main_par$height
    #   main_par$facet
    #.  main_par$strains
    # local inputs:
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
    output$shiny_times <- shiny::renderUI({
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
    
    # Filter static traitData based on selected trait_names.
    traitDataInput <- shiny::reactive({
      shiny::req(trait_names())
      
      subset_trait_names(traitData, trait_names())
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
      trait_names(),
      {
        # Use current selection of trait_selection().
        # But make sure they are still in the traitNamesArranged().
        selected <- timetrait_selection()
        choices <- trait_names()
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
    
    trait_names <- shiny::reactive({
      shiny::req(input$time)
      
      timetraits(traitSignalInput(), input$time)
    })
    traitTime <- shiny::reactive({
      shiny::req(timetrait_selection(), input$time_response, input$time)
      
      traitTimes(
        traitDataInput(), traitSignalInput(),
        timetrait_selection(), input$time_response, input$time,
        strains = main_par$strains)
    })
    traitTimeSum <- shiny::reactive({
      shiny::req(timetrait_selection(), input$time)
      
      traitTimes(
        traitStatsInput(),
        timetrait_selection(), "p.value", input$time, "terms")
    })
    
    # List returned
    shiny::reactive({
      shiny::req(timetrait_selection(), timeplots(), statstable())
      list(
        plot = timeplots(),
        table = statstable(),
        traits = timetrait_selection())
    })
  })
}
      
  