#' Shiny Module Input for Time Traits
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimeTraits
#' @export
#' @importFrom shiny NS uiOutput
#'
shinyTimeTraitsInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shinyInput"))
}

#' Shiny Module Server for Times Plots
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitStats static object
#' @param traitOrder reactive object
#' @param responses possible types of responses
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow h3 observeEvent moduleServer plotOutput
#'             reactive reactiveVal renderPlot renderUI req selectInput
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTimeTraits <- function(id, main_par,
                       traitSignal, traitOrder,
                       responses = c("value", "cellmean")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # local inputs:
    #   time
    #   time_trait
    #   time_response
    
    # OUTPUTS
    #   list with inputs
    
    # Identify all Time Traits.
    timetrait_all <- timetraitsall(traitSignal)

    # Inputs
    output$shinyInput <- shiny::renderUI({
      timeunits <- time_units(timetrait_all)
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("time"), "Time Unit:",
            timeunits, selections$time)),
          shiny::column(6, shiny::selectInput(ns("response"), "Response:",
            responses, selections$response))),
        
        shiny::selectizeInput(ns("traits"), "Traits:", NULL,
                              multiple = TRUE)
      )
    })
    selections <- shiny::reactiveValues(
      time = input$time, # NULL
      response = input$response, # "cellmean"
      traits = input$traits) # NULL
    shiny::observeEvent(input$time, selections$time <- input$time)
    shiny::observeEvent(input$response, selections$response <- input$response)
    shiny::observeEvent(input$traits, selections$traits <- input$traits)
    
    # Update `input$time` choices and selected.
    shiny::observeEvent(
      shiny::req(traitOrder()),
      {
        selected <- selections$time
        choices <- time_units(timetrait_order())
        selected <- selected[selected %in% choices]
        if(!length(selected)) selected <- choices[1]
        shiny::updateSelectInput(session, "time",
                                 choices = choices, selected = selected)
      }
    )

    # Update Trait choices and selected.
    shiny::observeEvent(
      shiny::tagList(selections$response, selections$time, traitOrder(),
                     main_par$tabpanel),
      {
        # Use current selection of trait_selection().
        # But make sure they are still in the traitOrder() object.
        selected <- selections$traits
        choices <- shiny::req(trait_names())
        selected <- selected[selected %in% choices]
        if(!length(selected))
          selected <- choices[1]
        shiny::updateSelectizeInput(session, "time_trait", choices = choices,
                                    server = TRUE, selected = selected)
      })

    # Trait Order Criterion.
    timetrait_order <- shiny::reactive({
      out <- timetrait_all
      
      if(shiny::isTruthy(traitOrder())) {
        out <- dplyr::filter(
          dplyr::left_join(
            dplyr::select(traitOrder(), .data$dataset, .data$trait),
            out,
            by = c("dataset", "trait")),
          !is.na(timetrait))
      }
      out
    }, label = "timetrait_order")
    
    # Trait names (removing key time information).
    trait_names <- shiny::reactive({
      shiny::req(selections$time)
      if(shiny::isTruthy(main_par$tabpanel)) {
        shiny::req(main_par$tabpanel)
      }
      
      # Make sure timeunit aligns with trait names.
      object <- shiny::req(timetrait_order())
      timeunit <- selections$time
      if(!(timeunit %in% object$timetrait))
        timeunit <- sort(unique(object$timetrait))[1]
      
      timetraits(object, timeunit)
    }, label = "trait_names")
    
    ###############################################################
    selections
  })
}