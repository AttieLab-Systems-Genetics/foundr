#' Shiny Module UI for Dendro Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyModuleDendro
#' @export
#'
shinyModuleDendroUI <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("shiny_dendro"))
}

#' Shiny Module Server for Dendro Plots
#'
#' @param input,output,session standard shiny arguments
#' @param module_par,main_par reactive arguments from `foundrServer` and `shinyModules`
#' @param traitModule reactive object with list created by `listof_wgcnamodules`
#'
#' @return reactive object for `shinyModuleDendroUI`
#' @importFrom shiny plotOutput reactive renderPlot renderUI req
#'             tagList uiOutput
#' @importFrom DT renderDataTable
#' @export
#'
shinyModuleDendro <- function(input, output, session,
                        module_par, main_par, traitModule) {
  ns <- session$ns
  
  # INPUTS
  # foundrServer inputs: (see shinyapp.R)
  #   main_par$height (see shinyapp.R::foundrUI sidebarPanel)
  # shinyModules inputs: (see shinyModules.R)
  #   module_par$dataset
  #   module_par$response

  # OUTPUTS
  # output$dendro dendro plot and table
  
  # RETURNS
  # list with
  #   plot (see dendroplots() below)
  #   table (see dendrotable() below)
  
  datasets <- shiny::reactive(
    c("LivMet","PlaMet0","PlaMet120","Metab"))
  responses <- shiny::reactive(
    c("value","cellmean","signal","rest"))
  
  # Dendrogram
  output$shiny_dendro <- shiny::renderUI({
    shiny::req(main_par$height, module_par$dataset, module_par$response)
    shiny::tagList(
      shiny::plotOutput(ns("dendroplot"),
                        height = paste0(main_par$height, "in")),

      DT::renderDataTable(
        dendrotable(),
        escape = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)))
  })
  
  dendrotable <- shiny::reactive({
    shiny::req(traitModule(), module_par$dataset, module_par$response)
    dplyr::filter(
      summary(traitModule()[[module_par$dataset]]),
      response == module_par$response)
  })
  dendroplots <- shiny::reactive({
    shiny::req(traitModule(), module_par$dataset, module_par$response)

    foundr::ggplot_listof_wgcnaModules(
      traitModule()[[module_par$dataset]],
      module_par$response)
  })
  output$dendroplot <- shiny::renderPlot({
    print(dendroplots())
  })

  ###############################################
  # Module comparisons
  # See WGCNAshiny.Rmd
  
  # List returned
  reactive({
    shiny::req(module_par$dataset, module_par$response, dendroplots(), dendrotable())
    list(
      plot = print(dendroplots()),
      table = dendrotable(),
      traits = c(module_par$dataset, module_par$response))
  })
}

      
  