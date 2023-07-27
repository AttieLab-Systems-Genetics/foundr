#' Shiny Module Input for Times Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimesPanel
#' @export
#' @importFrom shiny NS uiOutput
#'
shinyTimesPanelInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shinyInput"))
}

#' Shiny Module UI for Times Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimesPanel
#' @importFrom shiny column downloadButton fluidRow NS uiOutput
#' @export
#'
shinyTimesPanelUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(6, shiny::uiOutput(ns("filename"))),
    shiny::column(3, shiny::downloadButton(ns("downloadPlot"), "Plots")),
    shiny::column(3, shiny::downloadButton(ns("downloadTable"), "Data")))
}

#' Shiny Module Output for Times Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimesPanel
#' @export
#' @importFrom shiny NS uiOutput
#'
shinyTimesPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("timeplots"))
}

#' Shiny Module Server for Times Plots
#'
#' @param main_par reactive arguments 
#' @param traitData static objects
#' @param traitSignal,traitStats reactive objects
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow observeEvent plotOutput reactive
#'             reactiveVal renderPlot renderUI req selectInput selectizeInput
#'             tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTimesPanel <- function(id, main_par,
                            traitData, traitSignal, traitStats) {
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
    
    # MODULES
    tableOutput <- shinyTraitTable("shinyTable", main_par,
                                   timetrait_names,
                                   traitDataInput, traitSignal)
    
    output$shinyInput <- shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(
            ns("time"), "Time Unit:",
            timeunits(), time_selection())),
          shiny::column(6, shiny::selectInput(
            ns("time_response"), "Response:",
            c("value", "cellmean", "signal"), response_selection()))),
        
        shiny::selectizeInput(ns("time_trait"), "Traits:",
                              NULL, multiple = TRUE),
        
        # Trait Table Response.
        shinyTraitTableUI(ns("shinyTable"))
      )
    })
    timeunits <- shiny::reactive({
      options <- NULL
      if(shiny::isTruthy(traits_week())) {
        options <- c("week","week_summary")
      }
      if(shiny::isTruthy(traits_minute())) {
        options <- c(options, "minute","minute_summary")
      }
      options
    })
    time_selection <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$time, {
      time_selection(input$time)
    })
    response_selection <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$time_response, {
      response_selection(input$time_response)
    })
    
    # Filter static traitData based on selected trait_names.
    traitDataInput <- shiny::reactive({
      shiny::req(timetrait_names())

      subset_trait_names(traitData, timetrait_names())
    })
    
    # Main return
    output$timeplots <- shiny::renderUI({
      shiny::req(main_par$height, tableOutput(), statstable())
      
      shiny::tagList(
        shiny::plotOutput(ns("timeplot"),
                          height = paste0(main_par$height, "in")),
        
        shinyTraitTableOutput(ns("shinyTable")),

        DT::renderDataTable(
          statstable(),
          escape = FALSE,
          options = list(scrollX = TRUE, pageLength = 10)))
    })
    
    statstable <- shiny::reactive({
      shiny::req(traitTimeSum())
      
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
      shiny::req(trait_names(), response_selection()),
      {
        # Use current selection of trait_selection().
        # But make sure they are still in the traitNamesArranged().
        selected <- timetrait_selection()
        choices <- trait_names()
        selected <- selected[selected %in% choices]
        if(!length(selected))
          selected <- choices[1]
        shiny::updateSelectizeInput(session, "time_trait", choices = choices,
                                    server = TRUE, selected = selected)
      })
    timetrait_selection <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$time_trait, {
      timetrait_selection(input$time_trait)
    })
    
    # Trait Names: timetrait_names() include time info; trait_names() do not.
    timetrait_all <- shiny::reactive({
      timetraitsall(shiny::req(traitSignal()))
    })
    timetrait_names <- shiny::reactive({
      timetraits_filter(timetrait_all(), shiny::req(time_selection()),
                        shiny::req(timetrait_selection()))
    })
    traits_week <- shiny::reactive({
      timetraits(timetrait_all(), "week")
    })
    traits_minute <- shiny::reactive({
      timetraits(timetrait_all(), "minute")
    })
    trait_names <- shiny::reactive({
      if(shiny::isTruthy(main_par$tabpanel)) {
        shiny::req(main_par$tabpanel)
      }
      
      switch(shiny::req(time_selection()),
             week = traits_week(),
             minute = traits_minute())
    })
    
    # Times Data Object
    traitTime <- shiny::reactive({
      shiny::req(timetrait_selection(), response_selection(), time_selection())
      
      traitTimes(
        traitDataInput(), traitSignal(),
        timetrait_selection(), response_selection(), time_selection(),
        strains = main_par$strains)
    })
    traitTimeSum <- shiny::reactive({
      shiny::req(timetrait_selection(), time_selection())
      
      traitTimes(
        traitStats(),
        timetrait_selection(), "p.value", time_selection(), "terms")
    })
    
    # DOWNLOADS
    # Download File Prefix
    output$filename <- renderUI({
      shiny::req(timetrait_selection())
      
      filename <- paste0(
        "Traits_", timetrait_selection()[1])
      shiny::textAreaInput(ns("filename"), "File Prefix", filename)
    })
    
    # Download Plot
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".pdf")
      },
      content = function(file) {
        shiny::req(timeplots())
        grDevices::pdf(file, width = 9, height = 6)
        print(timeplots())
        invisible()
        grDevices::dev.off()
      })
    
    # Download DataTable
    output$downloadTable <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".csv")
      },
      content = function(file) {
        shiny::req(tableOutput())
        utils::write.csv(
          summary(tableOutput()),
#          statstable(),
          file, row.names = FALSE)
      })
  })
}