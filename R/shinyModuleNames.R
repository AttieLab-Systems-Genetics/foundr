#' Shiny Module UI for Dendro Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyModuleNames
#' @export
#'
shinyModuleNamesUI <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("shiny_names"))
}

#' Shiny Module Server for Dendro Plots
#'
#' @param input,output,session standard shiny arguments
#' @param module_par,main_par reactive arguments from `foundrServer` and `shinyModules`
#' @param traitModule reactive object with list created by `listof_wgcnamodules`
#'
#' @return reactive object for `shinyModuleNamesUI`
#' @importFrom shiny plotOutput reactive renderPlot renderUI req
#'             tagList uiOutput
#' @importFrom DT renderDataTable
#' @export
#'
shinyModuleNames <- function(input, output, session,
                        module_par, main_par, traitModule) {
  ns <- session$ns
  
  # INPUTS
  # shinyModules inputs: (see shinyModules.R)
  #   module_par$dataset
  #   module_par$responseF Facet Response
  #   module_par$responseC Color Response
  # shinyModuleNames inputs: (see output$shiny_modcomp below)
  #   input$fmodules  Facet Modules
  #   input$cmodules  Color Modules

  output$shiny_names <- shiny::renderUI({
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("fmodules"), "Facet Modules:",
          c("gray","turquoise"),
          multiple = TRUE)),
      shiny::column(
        4,
        shiny::selectInput(
          ns("cmodules"), "Color Modules:",
          c("gray","turquoise"),
          multiple = TRUE)),
      shiny::column(
        2,
        checkboxInput(ns("abs"), "Absolute kME?")))
  })
  
  # Module kME information
  mods <- shiny::reactive({
    shiny::req(module_par$dataset)
    browser()
    foundr::module_kMEs(traitModule()[[module_par$dataset]])
  })
  fmodules <- shiny::reactive({
    shiny::req(mods(), module_par$responseF)
    levels(mods()[[paste0(module_par$responseF, "_col")]])
  })
  cmodules <- shiny::reactive({
    shiny::req(mods(), module_par$responseC)
    levels(mods()[[paste0(module_par$responseC, "_col")]])
  })
  shiny::observeEvent(
    module_par$responseF,
    {
      shiny::updateSelectInput(
        session, "fmodules",
        choices = fmodules(),
        selected = fmodules())
    })
  shiny::observeEvent(
    module_par$responseC,
    {
      shiny::updateSelectInput(
        session, "cmodules",
        choices = cmodules(),
        selected = cmodules())
    })

  ###############################################
  # List returned
  reactive({
    input
  })
}
