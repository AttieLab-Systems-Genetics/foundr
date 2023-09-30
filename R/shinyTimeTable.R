#' Shiny Module Input for Times Table
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimeTable
#' @export
#' @importFrom shiny NS uiOutput
#'
shinyTimeTableInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shinyInput"))
}

#' Shiny Module Output for Times Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimeTable
#' @export
#' @importFrom shiny NS uiOutput
#'
shinyTimeTableOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shinyOutput"))
}

#' Shiny Module Server for Times Plots
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitData,traitSignal,traitStats static objects
#' @param responses possible types of responses
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow h3 observeEvent moduleServer plotOutput
#'             reactive reactiveVal renderPlot renderUI req selectInput
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTimeTable <- function(id, main_par,
                       traitData, traitSignal, traitStats,
                       responses = c("value", "cellmean")) {
  shiny::moduleServer(id, function(input, output, session) {
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
    # traitTimeData
    
    # Identify all Time Traits.
    timetrait_all <- timetraitsall(traitSignal)
    
    # Subset Stats to time traits.
    traitStatsInput <- time_trait_subset(traitStats, timetrait_all)
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- shinyTraitOrder("shinyOrder", main_par,
                                   traitStatsInput, traitSignal,
                                   customSettings, TRUE)

    # Inputs
    output$shinyInput <- shiny::renderUI({
      timeunits <- time_units(timetrait_all)
      shiny::tagList(
        shinyTraitOrderInput(ns("shinyOrder")),
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("time"), "Time Unit:",
            timeunits, time_selection())),
          shiny::column(6, shiny::selectInput(ns("time_response"), "Response:",
            responses, response_selection()))),
        
        shiny::selectizeInput(ns("time_trait"), "Traits:", NULL,
                              multiple = TRUE)
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
        if(!length(selected)) selected <- choices[1]
        shiny::updateSelectInput(session, "time",
                                 choices = choices, selected = selected)
      }
    )
    response_selection <- shiny::reactiveVal("cellmean",
                                             label = "response_selection")
    shiny::observeEvent(input$time_response,
                        response_selection(input$time_response))

    # Main return
    output$shinyOutput <- shiny::renderUI({
    })

    shiny::observeEvent(
      shiny::tagList(response_selection(), time_selection(), orderOutput(),
                     main_par$tabpanel),
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
            dplyr::select(orderOutput(), .data$dataset, .data$trait),
            out,
            by = c("dataset", "trait")),
          !is.na(timetrait))
      }
      out
    }, label = "timetrait_order")
    
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
    
    ###############################################################
    
    shiny::reactive({
      shiny::req(timetrait_selection(), response_selection(), time_selection(),
                 orderOutput())
      
      traitTimes(traitData, traitSignal, traitStats,
                 timetrait_selection(), time_selection(), response_selection(),
                 strains = main_par$strains)
    }, label = "traitTimes")
  })
}