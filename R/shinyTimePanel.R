#' Shiny Module Input for Time Panel
#' @return nothing returned
#' @rdname shinyTimePanel
#' @export
shinyTimePanelInput <- function(id) { # 4:Order, 8:Traits
  ns <- shiny::NS(id)
  shinyTimeTableInput(ns("shinyTimeTable"))
}
#' Shiny Module UI for Time Panel
#' @return nothing returned
#' @rdname shinyTimePanel
#' @export
shinyTimePanelUI <- function(id) { # Time Unit
  ns <- shiny::NS(id)
  shinyTimeTableUI(ns("shinyTimeTable"))
}
#' Shiny Module Output for Times Plot
#' @return nothing returned
#' @rdname shinyTimePanel
#' @export
shinyTimePanelOutput <- function(id) { # Response; Plots or Tables
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyTimePlotUI(ns("shinyTimePlot")),
    shiny::fluidRow(
      shiny::column(6, shinyTimeTableOutput(ns("shinyTimeTable"))), # Response
      shiny::column(6, shinyTimePlotInput(ns("shinyTimePlot")))),
    shiny::fluidRow(
      shiny::column(9, shiny::uiOutput(ns("strains"))),
      shiny::column(3, shiny::checkboxInput(ns("facet"), "Facet by strain?", TRUE))),
    shinyTimePlotOutput(ns("shinyTimePlot"))) # Plots or Tables
}

#' Shiny Module Server for Times Plots
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitData,traitSignal,traitStats static objects
#'
#' @return nothing returned
#' @importFrom shiny moduleServer NS
#' @export
#'
shinyTimePanel <- function(id, main_par,
                            traitData, traitSignal, traitStats) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # *** Want Times panel to look more like Traits panel.
    # Move Response (from shinyTimeTraitsOutput) to main from side.
    # Put Order and Traits (from shinyTraitOrderInput and shinyTimeTraitsInput) up with datasets.
    # Keep Time unit (from shinyTimeTraitsUI) on side panel.
    # Key Datasets and Trait.
    # shinyTimePanel
    # Mostly works but lost shinyTimeTraitsUI and time unit.
    
    
    # INPUTS
    # passed inputs:
    #   main_par$dataset
    #   main_par$height
    #   input$strains
    #   input$facet

    # MODULES
    timeOutput <- shinyTimeTable("shinyTimeTable", input, main_par, 
                                 traitData, traitSignal, traitStats)
    
    shinyTimePlot("shinyTimePlot", input, main_par, traitSignal, timeOutput)
    
    # SERVER-SIDE Inputs
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput(ns("strains"), "Strains",
                                choices = choices, selected = choices, inline = TRUE)
    })
    
    
  })
}