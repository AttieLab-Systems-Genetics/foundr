#' Shiny Module Input for Contrast Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @importFrom shiny NS radioButtons tagList
#' @export
#'
shinyContrastPanelInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::radioButtons(ns("contrast"), "",
                        c("Contrast by Sex", "Contrast over Time"),
                        inline = TRUE),
    shiny::uiOutput(ns("shinyInput")))
}

#' Shiny Module Output for Contrast Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyContrastPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shinyOutput"))
}

#' Shiny Module Server for Contrast Panel
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column fluidRow h3 isTruthy moduleServer reactive
#'             renderText renderUI tagList
#' @importFrom stringr str_to_title
#' @export
#'
shinyContrastPanel <- function(id, main_par,
                            traitSignal, traitStats,
                            customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # RETURNS
    #   contrastOutput
    
    # Identify all Time Traits.
    timetrait_all <- timetraitsall(traitSignal)
    # Subset Stats to time traits.
    traitStatsTime <- time_trait_subset(traitStats, timetrait_all)
    
    # MODULES
    # Contrast Trait Table
    contrastOutput <- shinyContrastTable("shinyContrastTable",
      input, main_par, traitSignal, traitStats, customSettings)
    # Contrast Trait Plots
    shinyContrastPlot("shinyContrastPlot",
      main_par, contrastOutput, customSettings)
    # Contrast Trait Table
    contrastTimeOutput <- shinyContrastTable("shinyContrastTimeTable",
      input, main_par, traitSignal, traitStatsTime, customSettings, TRUE)
    # Contrast Time Traits
    timeOutput <- shinyContrastTime("shinyContrastTime", input, main_par,
      traitSignal, traitStatsTime, contrastTimeOutput, customSettings)
    # Contrast Time Plots and Tables
    shinyTimePlot("shinyTimePlot", main_par, traitSignal, timeOutput)
    
    # Input
    output$shinyInput <- shiny::renderUI({
      switch(shiny::req(input$contrast),
        "Contrast by Sex" = {
          shiny::fluidRow(
            shiny::column(9, shinyContrastTableInput(ns("shinyContrastTable"))),
            shiny::column(3, shinyContrastPlotInput(ns("shinyContrastPlot"))))
        },
        "Contrast over Time" = {
          shiny::tagList(
            shinyContrastTableInput(ns("shinyContrastTimeTable")),
            shinyContrastTimeInput(ns("shinyContrastTime")),
            shinyTimePlotInput(ns("shinyTimePlot")))
        })
    })
    
    # Output
    output$shinyOutput <- shiny::renderUI({
      shiny::tagList(
        shiny::uiOutput(ns("text")),
        
        switch(shiny::req(input$contrast),
          "Contrast by Sex" = shinyContrastPlotOutput(ns("shinyContrastPlot")),
          "Contrast over Time" = {
            shiny::req(timeOutput())
            
            shinyTimePlotOutput(ns("shinyTimePlot"))
          }))
    })
    
    output$text <- shiny::renderUI({
      condition <- customSettings$condition
      if(shiny::isTruthy(condition))
        condition <- stringr::str_to_title(condition)
      else
        condition <- "Condition"
      
      shiny::tagList(
        shiny::h3(paste(condition, "Contrasts")),
        shiny::renderText({
          out <- paste0(
            "This panel examines contrasts (differences or ratios) of ",
            condition, " means by strain and sex.",
            "These may be viewed by sex or averaged over sex",
            " (Both Sexes) or by contrast of Female - Male",
            " (Sex Contrast).")
          if(shiny::req(input$contrast) == "Contrast over Time")
            out <- paste(out, "Contrasts over time are by trait.")
          out
        }))
    })

    ###############################################################
    contrastOutput
  })
}
