#' Shiny Module Input for Modules of Contrasts
#' @return nothing returned
#' @rdname shinyContrastModule
#' @export
shinyContrastModuleInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyContrastPlotInput(ns("shinyContrastPlot")),
    shinyContrastPlotUI(ns("shinyContrastPlot")))
}
#' Shiny Module Output for Modules of Contrasts
#' @return nothing returned
#' @rdname shinyContrastModule
#' @export
shinyContrastModuleOutput <- function(id) {
  ns <- shiny::NS(id)
  shinyContrastPlotOutput(ns("shinyContrastPlot"))
}
#' Shiny Module Server for Modules of Contrasts
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param moduleContrast,traitContast reactive data frames
#' @param traitModule static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny h3 moduleServer NS reactive renderPlot renderUI req
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom stringr str_to_title
#' @export
#'
shinyContrastModule <- function(id, panel_par, main_par,
                                traitModule, moduleContrast, traitContrast,
                                customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # MODULES
    # Contrast Eigen Plots
    shinyContrastPlot("shinyContrastPlot",
      panel_par, main_par, contrastTable, customSettings,
      modTitle)
    
    contrastTable <- shiny::reactive({
      if(shiny::isTruthy(panel_par$module)) traits() else eigens()      
    })
    modTitle <- shiny::reactive({
      if(shiny::isTruthy(panel_par$module)) 
        paste("Eigentrait Contrasts for Module", panel_par$module)
      else
        "Eigentrait Contrasts across Modules"
    })

    # INPUTS
    
    # Restrict `traitModule` to datasets in `moduleContrast()`
    datamodule <- shiny::reactive({
      traitModule[shiny::req(main_par$dataset)]
    })
    
    # Eigen Contrasts.
    eigens <- shiny::reactive({
      shiny::req(datamodule(), moduleContrast())
      
      eigen_contrast_dataset(datamodule(), moduleContrast())
    })

    # Compare Selected Module Eigens to Traits in Module
    traits <- shiny::reactive({
      shiny::req(datamodule(), panel_par$sex, panel_par$module, main_par$dataset,
                 traitContrast(), eigens())
      
      eigen_traits_dataset(datamodule(), panel_par$sex, panel_par$module,
                           traitContrast(), eigens())
    })

    ##############################################################
    eigens
  })
}
