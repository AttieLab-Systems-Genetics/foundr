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
  
  shiny::uiOutput(ns("downloadsx"))
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
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitData,traitSignal,traitStats static objects
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow h3 observeEvent plotOutput reactive
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
    
    # Setup for TraitOrder
    timetrait_all <- timetraitsall(traitSignal)
    timeunits <- time_units(timetrait_all)
    
    traitStatsInput <- time_trait_subset(traitStats, timetrait_all)
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- shinyTraitOrder("shinyOrder", main_par,
                                   traitStatsInput, traitSignal,
                                   customSettings, TRUE)
    
    # Trait Table.
    tableOutput <- shinyTraitTable("shinyTable", main_par, main_par,
                                   keyTrait, relTraits,
                                   traitData, traitSignal)
    
    # Inputs
    output$shinyInput <- shiny::renderUI({
      shiny::tagList(
        shinyTraitOrderInput(ns("shinyOrder")),
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(
            ns("time"), "Time Unit:",
            timeunits, time_selection())),
          shiny::column(6, shiny::selectInput(
            ns("time_response"), "Response:",
            c("value", "cellmean", "signal"), response_selection()))),
        
        shiny::selectizeInput(ns("time_trait"), "Traits:",
                              NULL, multiple = TRUE),
        
        # Trait Table Response.
        shinyTraitTableUI(ns("shinyTable"))
      )
    })
    time_selection <- shiny::reactiveVal(NULL, label = "time_selection")
    shiny::observeEvent(input$time, time_selection(input$time))
    shiny::observeEvent(
      shiny::req(orderOutput()),
      {
        selected <- time_selection()
        choices <- time_units(timetrait_order())
        selected <- selected[selected %in% choices]
        if(!length(selected))
          selected <- choices[1]
        shiny::updateSelectInput(session, "time",
                                 choices = choices, selected = selected)
      }
    )
    response_selection <- shiny::reactiveVal("cellmean",
                                             label = "response_selection")
    shiny::observeEvent(input$time_response,
                        response_selection(input$time_response))

    # Main return
    output$timeplots <- shiny::renderUI({
      shiny::req(main_par$height, tableOutput(), statstable(),
                 timeplots(), timestats())
      
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::radioButtons(ns("buttime"), "",
                                c("Traits","Stats"),
                                inline = TRUE)),
          shiny::column(
            8,
            shiny::uiOutput(ns("downloads")))),
        
        shiny::uiOutput(ns("time_stat")),
          
        shinyTraitTableOutput(ns("shinyTable")),

        shiny::h3("Stats: -log10(p.value)"),
        DT::renderDataTable(
          statstable(),
          escape = FALSE,
          options = list(scrollX = TRUE, pageLength = 10)))
    })
    output$time_stat <- shiny::renderUI({
      switch(
        shiny::req(input$buttime),
        Traits = {
          shiny::plotOutput(
            ns("timeplot"),
            height = paste0(main_par$height, "in"))
        },
        Stats  =  {
          shiny::plotOutput(
            ns("statplot"),
            height = paste0(main_par$height, "in"))
        })
    })
    output$timeplot <- shiny::renderPlot(print(timeplots()))
    output$statplot <- shiny::renderPlot(print(timestats()))

    statstable <- shiny::reactive({
      shiny::req(traitTimeSum())
      
      stats_time_table(traitTimeSum())
    }, label = "statstable")
    timeplots <- shiny::reactive({
      shiny::req(traitTime(), main_par$strains)
      
      ggplot_traitTimes(
        traitTime(),
        facet_strain = main_par$facet)
    }, label = "timeplots")
    timestats <- shiny::reactive({
      shiny::req(traitTimeSum())
      
      ggplot_traitTimes(
        traitTimeSum())
    }, label = "timestats")
    shiny::observeEvent(
      shiny::tagList(response_selection(), time_selection(), orderOutput()),
      {
        # Use current selection of trait_selection().
        # But make sure they are still in the orderOutput().
        selected <- timetrait_selection()
        choices <- shiny::req(trait_names())
        selected <- selected[selected %in% choices]
        if(!length(selected))
          selected <- choices[1]
        shiny::updateSelectizeInput(session, "time_trait", choices = choices,
                                    server = TRUE, selected = selected)
      })
    timetrait_selection <- shiny::reactiveVal(NULL,
                                              label = "timetrait_selection")
    shiny::observeEvent(input$time_trait, timetrait_selection(input$time_trait))
    
    # Trait Names: keyTrait() include time info; trait_names() do not.
    timetrait_order <- shiny::reactive({
      out <- timetrait_all
      
      if(shiny::isTruthy(orderOutput())) {
        out <- dplyr::filter(
          dplyr::left_join(
            dplyr::select(
              orderOutput(),
              .data$dataset, .data$trait),
            out,
            by = c("dataset", "trait")),
          !is.na(timetrait))
      }
      out
    }, label = "timetrait_order")
    
    keyTrait <- shiny::reactive({
      timetraits_filter(shiny::req(timetrait_order()),
                        shiny::req(time_selection()),
                        shiny::req(timetrait_selection()))
    })
    relTraits <- shiny::reactiveVal(NULL)
    
    trait_names <- shiny::reactive({
      shiny::req(time_selection())
      if(shiny::isTruthy(main_par$tabpanel)) {
        shiny::req(main_par$tabpanel)
      }
      
      # Make sure timeunit aligns with trait names.
      object <- shiny::req(timetrait_order())
      timeunit <- time_selection()
      if(!(timeunit %in% object$timetrait))
        timeunit <- sort(unique(object$timetrait))[1]
      
      timetraits(object, timeunit)
    }, label = "trait_names")
    
    # Times Data Object
    traitTime <- shiny::reactive({
      shiny::req(timetrait_selection(), response_selection(), time_selection(),
                 orderOutput())
      
      traitTimes(
        traitData, traitSignal,
        timetrait_selection(), response_selection(), time_selection(),
        strains = main_par$strains)
    }, label = "traitTime")
    traitTimeSum <- shiny::reactive({
      shiny::req(timetrait_selection(), time_selection(), orderOutput())
      
      traitTimes(
        traitStats,
        timetrait_selection(), "p.value", time_selection(), "terms")
    }, label = "traitTimeSum")
    
    # DOWNLOADS
    output$downloads <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(3, shiny::downloadButton(ns("downloadPlots"), "Plots")),
        shiny::column(3, shiny::downloadButton(ns("downloadTables"), "Tables")),
        shiny::column(6, shiny::uiOutput(ns("filename"))))
    })
    # Download File Prefix
    output$filename <- renderUI({
      shiny::req(timetrait_selection())
      
      filename <- paste0(
        "Traits_", timetrait_selection()[1])
      shiny::textAreaInput(ns("filename"), "File Prefix", filename)
    })
    
    # Download Plot
    output$downloadPlots <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".pdf")
      },
      content = function(file) {
        shiny::req(timeplots(), timestats(), main_par$height)
        grDevices::pdf(file, width = 9, height = main_par$height)
        print(timeplots())
        print(timestats())
        invisible()
        grDevices::dev.off()
      })
    
    # Download DataTable
    output$downloadTables <- shiny::downloadHandler(
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