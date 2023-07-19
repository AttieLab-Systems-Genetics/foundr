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
#' @param traitSignalInput,traitStatsInput reactive objects
#'
#' @return nothing returned
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
    
    output$shinyInput <- shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(
            ns("time"), "Time Unit:",
            c("week", "minute","week_summary","minute_summary"))),
          shiny::column(6, shiny::selectInput(
            ns("time_response"), "Response:",
            c("value", "cellmean", "signal")))),
        
        shiny::selectizeInput(ns("time_trait"), "Traits:",
                              NULL, multiple = TRUE),
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
          selected <- choices[1]
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
        shiny::req(statstable())
        utils::write.csv(
          statstable(),
          file, row.names = FALSE)
      })
  })
}
      
  