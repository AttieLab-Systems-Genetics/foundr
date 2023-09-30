#' Shiny Module Input for Contrasts over Time
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastTime
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyContrastTimeInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shinyInput"))
}

#' Shiny Module Output for Contrasts over Time
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastTime
#' @importFrom shiny NS tagList uiOutput
#' @export
#'
shinyContrastTimeOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::uiOutput(ns("text")),
    shiny::uiOutput(ns("shinyOutput")))
}

#' Shiny Module Server for Contrasts over Time
#'
#' @param input,output,session standard shiny arguments
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny h3 isTruthy moduleServer reactive renderText renderUI
#'             tagList
#' @importFrom stringr str_to_title
#' @export
#'
shinyContrastTime <- function(id, main_par,
                            traitSignal, traitStats,
                            customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # RETURNS
    #   contrastOutput
    
    # MODULES
    contrastOutput <- shinyContrasts("shinyContrasts", main_par,
                                      traitSignal, traitStats,
                                      customSettings)
    
    contrastTimeData <- shiny::reactive({
      timetrait_selection <- timetraits(contrastOutput(), "minute")
      out <- traitTimes(contrastOutput(), contrastOutput(),
        timetrait_selection[1], "cellmean", "minute")
      browser()
      out
    })
    
    
    shinyTimePlot("shinyTime", main_par, traitData, traitSignal,
                  contrastTimeData) 

    output$text <- shiny::renderUI({
      condition <- customSettings$condition
      if(shiny::isTruthy(condition))
        condition <- stringr::str_to_title(condition)
      else
        condition <- "Condition"
      
      shiny::tagList(
        shiny::h3(paste(condition, "Contrasts")),
        shiny::renderText({
          paste0("This panel examines contrasts (differences or ratios) of ",
                 condition, " means by strain and sex.",
                 "These may be viewed by sex or averaged over sex",
                 " (Both Sexes) or by contrast of Female - Male",
                 " (Sex Contrast).")}),
        shiny::column(4, shiny::radioButtons(ns("buttype"),
          "", c("Traits","Times"), "Traits", inline = TRUE)),
      )
    })
    
    output$shinyInput <- shiny::renderUI({
      switch(
        shiny::req(input$buttype),
        Traits = shinyContrastsInput(ns("shinyContrasts")),
        Times  = shinyTimePlotInput(ns("shinyTime")))
    })
    
    output$shinyOutput <- shiny::renderUI({
      switch(
        shiny::req(input$buttype),
        Traits = shinyContrastsOutput(ns("shinyContrasts")),
        Times  = shinyTimePlotOutput(ns("shinyTime")))
    })

    ###############################################################
    contrastOutput
  })
}
