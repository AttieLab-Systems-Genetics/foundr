#' Shiny Module Input for Time Traits
#' @return nothing returned
#' @rdname shinyTimeTraits
#' @export
shinyTimeTraitsInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::selectizeInput(ns("traits"), "Traits:", NULL, multiple = TRUE)
}
#' Shiny Module UI for Time Traits
#' @return nothing returned
#' @rdname shinyTimeTraits
#' @export
shinyTimeTraitsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shinyUI")) # Time Unit
}
#' Shiny Module Output for Time Traits
#' @return nothing returned
#' @rdname shinyTimeTraits
#' @export
shinyTimeTraitsOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shinyOutput")) # Response
}
#' Shiny Module Server for Times Plots
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitSignal static object
#' @param traitOrder reactive object
#' @param responses possible types of responses
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow h3 observeEvent moduleServer NS plotOutput
#'             radioButtons reactive reactiveValues renderPlot renderUI req
#'             selectInput selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTimeTraits <- function(id, panel_par, main_par,
                       traitSignal, traitOrder,
                       responses = c("value", "normed", "cellmean")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # local inputs:
    #   input$time
    #   input$traits
    #   input$response
    # OUTPUTS
    #   list with inputs
    
    # Identify all Time Traits.
    timetrait_all <- timetraitsall(traitSignal)

    # Inputs
    output$shinyUI <- shiny::renderUI({
      timeunits <- time_units(timetrait_all)
  
      shiny::selectInput(ns("time"), "Time Unit:", timeunits, selections$time)
    })
    output$shinyOutput <- shiny::renderUI({
      shiny::radioButtons(ns("response"), "Response:",
                         responses, selections$response, inline = TRUE)
    })
    selections <- shiny::reactiveValues(time = NULL, response = "cellmean",
                                        traits = NULL)
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
                     trait_names(), main_par$tabpanel, panel_par$contrast),
      {
        # Use current selection of trait_selection().
        # But make sure they are still in the traitOrder() object.
        selected <- selections$traits
        choices <- shiny::req(trait_names())
        selected <- selected[selected %in% choices]
        if(!length(selected))
          selected <- choices[1]
        shiny::updateSelectizeInput(session, "traits", choices = choices,
                                    server = TRUE, selected = selected)
        selections$traits <- selected
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