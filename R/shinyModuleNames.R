#' Shiny Module UI for Module Names
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
#' @param module_par reactive arguments from `foundrServer` and `shinyModules`
#' @param traitModule reactive object with list created by `listof_wgcnamodules`
#' @param modrole reactive with name of module role
#'
#' @return reactive object for `shinyModuleNamesUI`
#' @importFrom shiny plotOutput reactive renderPlot renderUI req
#'             tagList uiOutput
#' @importFrom DT renderDataTable
#' @export
#'
shinyModuleNames <- function(input, output, session,
                        module_par, traitModule, modrole) {
  ns <- session$ns
  
  # INPUTS
  # shinyModules inputs: (see shinyModules.R)
  #   module_par$dataset
  #   module_par$modrole
  #   module_par$response
  # shinyModuleNames inputs: (see output$shiny_modcomp below)
  #   input$modulenames  Module Names
  
  output$shiny_names <- shiny::renderUI({
    shiny::req(modrole())
    shiny::selectizeInput(
      ns("modulenames"), paste(modrole(), "Modules:"),
      choices = NULL,
      multiple = TRUE)
  })
  
  # Module kME information
  mods <- shiny::reactive({
    shiny::req(module_par$dataset)
    foundr::module_kMEs(traitModule()[[module_par$dataset]])
  })
  fmodule_names <- shiny::reactiveVal(NULL)
  shiny::observeEvent(
    input$modulenames,
    fmodule_names(input$modulenames))
  
  modulenames <- shiny::reactive({
    shiny::req(mods(), module_par$response)
    levels(mods()[[paste0(module_par$response, "_col")]])
  })
  shiny::observeEvent(
    shiny::req(module_par$dataset, module_par$modrole, module_par$response),
    {
      selected <- choices <- modulenames()
      shiny::updateSelectizeInput(
        session, "modulenames",
        choices = choices,
        server = TRUE,
        selected = selected)
    })

  ###############################################
  # List returned
  fmodule_names
}
