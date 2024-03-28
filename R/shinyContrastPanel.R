#' Shiny Module Input for Contrast Panel
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @export
shinyContrastPanelInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shinyInput")) # Order, Traits (if butby == "Time")
}
#' Shiny Module Input for Contrast Panel
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @export
shinyContrastPanelUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("shinyUI")), # Time Unit (if butby == "Time")
    shiny::uiOutput(ns("butby")))
}
#' Shiny Module Output for Contrast Panel
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @export
shinyContrastPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shinyOutput"))
}
#' Shiny Module Server for Contrast Panel
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitSignal,traitStats,traitModule static objects
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column fluidRow h3 isTruthy moduleServer NS radioButtons
#'             reactive renderText renderUI tagList uiOutput
#' @importFrom stringr str_to_title
#' @export
#'
shinyContrastPanel <- function(id, main_par,
                            traitSignal, traitStats, traitModule,
                            customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # RETURNS
    #   moduleContrast
    
    # *** not totally set with the Time button
    # *** make sure data are really there, and make Time plot work
    # *** sometimes the traits don't work properly in moduleContrast
    # *** think about better naming conventions
    
    # Identify all Time Traits.
    timetrait_all <- timetraitsall(traitSignal)
    # Subset Stats to time traits.
    traitStatsTime <- time_trait_subset(traitStats, timetrait_all)
    
    # MODULES
    # Contrast Module Table
    moduleContrast <- shinyContrastTable("shinyContrastTable",
      input, main_par, traitSignal, traitStats, customSettings)
    # Contrast Trait Table
    traitContrast <- shinyContrastTable("shinyContrastTable",
      input, main_par, traitSignal, traitStats, customSettings, keepDatatraits)
    # Contrast Trait Plots by Sex
    shinyContrastSex("shinyContrastSex",
      input, main_par, moduleContrast, customSettings)
    # Contrast Time Trait Table
    contrastTimeOutput <- shinyContrastTable("shinyContrastTimeTable",
      input, main_par, traitSignal, traitStatsTime, customSettings)
    # Contrast Time Traits
    timeOutput <- shinyContrastTime("shinyContrastTime", input, main_par,
      traitSignal, traitStatsTime, contrastTimeOutput, customSettings)
    # Contrast Time Plots and Tables
    shinyTimePlot("shinyTimePlot", input, main_par, traitSignal, timeOutput)
    # Contrast Modules.
    moduleOutput <- shinyContrastModule("shinyContrastModule",
      input, main_par, traitModule, moduleContrast, traitContrast)
    
    # SERVER-SIDE Inputs
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput(ns("strains"), "Strains",
                                choices = choices, selected = choices, inline = TRUE)
    })
    output$butby <- shiny::renderUI({
      if(length(timetraits_dataset())) {
        buttons <- c("Sex", "Module", "Time")
      } else {
        buttons <- c("Sex", "Module")
      }
      shiny::radioButtons(ns("contrast"), "Contrast by ...",
                          buttons, inline = TRUE)
    })
    timetraits_dataset <- shiny::reactive({
      shiny::req(main_par$dataset)
      
      foundr::timetraitsall(dplyr::filter(traitSignal, dataset %in% main_par$dataset))
    })
    
    
    keepDatatraits <- reactive({
      foundr:::keptDatatraits(traitModule, shiny::req(main_par$dataset)[1])
    })
    
    # Input
    output$shinyInput <- shiny::renderUI({
      shiny::req(input$contrast)
      switch(
        input$contrast,
        Sex =, Module = {
          shiny::column(4, shinyContrastTableInput(ns("shinyContrastTable")))
        },
        Time = {
          shiny::fluidRow(
            shiny::column(4, shinyContrastTableInput(ns("shinyContrastTimeTable"))), # Order
            shiny::column(8, shinyContrastTimeInput(ns("shinyContrastTime")))) # Traits
        })
    })
    output$shinyUI <- shiny::renderUI({
      shiny::req(input$contrast)
      if(input$contrast == "Time") {
        shinyContrastTimeUI(ns("shinyContrastTime")) # Time Unit
      }
    })
    
    # Output
    output$shinyOutput <- shiny::renderUI({
      shiny::req(input$contrast)
      shiny::tagList(
        shiny::uiOutput(ns("text")),
        
        if(input$contrast == "Time") {
          shiny::fluidRow(
            shiny::column(9, shiny::uiOutput(ns("strains"))),
            shiny::column(3, shiny::checkboxInput(ns("facet"), "Facet by strain?", TRUE)))
        },
        
        switch(input$contrast,
          Sex = shinyContrastSexOutput(ns("shinyContrastSex")),
          Time = {
            shiny::tagList(
              shinyTimePlotUI(ns("shinyTimePlot")),
              shinyTimePlotOutput(ns("shinyTimePlot"))
            )
          },
          Module = shinyContrastModuleOutput(ns("shinyContrastModule"))))
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
          if(shiny::req(input$contrast) == "Time")
            out <- paste(out, "Contrasts over time are by trait.")
          if(shiny::req(input$contrast) == "Module")
            out <- paste(out, "WGCNA modules by dataset and sex have",
                         "power=6, minSize=4.",
                         "Select a Module to see module members.")
          out
        }))
    })

    ###############################################################
    moduleContrast
  })
}
