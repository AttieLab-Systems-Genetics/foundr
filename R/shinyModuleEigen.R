#' Shiny Module UI for Dendro Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyModuleEigen
#' @export
#'
shinyModuleEigenUI <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("shiny_modeigen"))
}

#' Shiny Module Server for Dendro Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer` 
#' @param traitModule reactive object with list created by `listof_wgcnamodules`
#'
#' @return reactive object for `shinyModuleEigenUI`
#' @importFrom shiny plotOutput reactive renderPlot renderUI req
#'             tagList uiOutput
#' @importFrom DT renderDataTable
#' @export
#'
shinyModuleEigen <- function(input, output, session,
                             module_par, main_par, traitModule) {
  ns <- session$ns
  
  # INPUTS
  # foundrServer inputs: (see shinyapp.R)
  #   main_par$height (see shinyapp.R::foundrUI sidebarPanel)
  # shinyModules inputs: (see shinyModules.R)
  #   module_par$dataset
  #   module_par$responseF
  #   module_par$responseC

  # The sign of correlation of modules could provide adjustment to direction in kME plots above.
  # However, using the absolute value seems to show the strong relationship (or not).
  
  output$shiny_modeigen <- shiny::renderUI({
    shiny::tagList(
      plotly::renderPlotly({ eigenplot() }),
      
      DT::renderDataTable({
        DT::datatable(
          eigentable(),
          options = list(paging =TRUE, pageLength = 5))
      }))
  })
  
  eigens <- shiny::reactive({
    shiny::req(module_par$dataset)
    foundr::eigen_cor(traitModule()[[module_par$dataset]])
  })
  
  eigenplot <- shiny::reactive({
    shiny::req(eigens(), module_par$responseF, module_par$responseC)
    # *** somewhere in subset by responses it gets messed up.
    # *** also when returning to moduleComp it loses info.
    ggplot2::autoplot(
      eigens(),
      module_par$responseF, module_par$responseC)
  })
  
  eigentable <- shiny::reactive({
    shiny::req(eigens(), module_par$responseF, module_par$responseC)
    foundr::subset_eigen_cor(eigens(), module_par$responseC, module_par$responseF)
  })  

  ###############################################
  # List returned
  reactive({
    shiny::req(module_par$dataset, module_par$responseF, module_par$responseC,
               eigenplot(), eigentable())
    list(
      plot = print(eigenplot()),
      table = eigentable(),
      traits = c(module_par$dataset, module_par$responseF, module_par$responseC))
  })
}

      
  