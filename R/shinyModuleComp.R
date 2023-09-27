#' Shiny Module UI for Dendro Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyModuleComp
#' @export
#'
shinyModuleCompUI <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("shiny_modcomp"))
}

#' Shiny Module Server for Dendro Plots
#'
#' @param input,output,session standard shiny arguments
#' @param module_par,main_par reactive arguments from `foundrServer` and `shinyModules`
#' @param traitModule reactive object with list created by `listof_wgcnamodules`
#'
#' @return reactive object for `shinyModuleCompUI`
#' @importFrom shiny plotOutput reactive renderPlot renderUI req
#'             tagList uiOutput
#' @importFrom DT renderDataTable
#' @export
#'
shinyModuleComp <- function(input, output, session,
                        module_par, main_par, traitModule) {
  ns <- session$ns
  
  # INPUTS
  # foundrServer inputs: (see shinyapp.R)
  #   main_par$height (see shinyapp.R::foundrUI sidebarPanel)
  # shinyModules inputs: (see shinyModules.R)
  #   module_par$dataset
  #   module_par$response
  #   module_par$responseF Facet Response
  #   module_par$responseC Color Response
  # shinyModuleComp inputs: (see output$shiny_modcomp below)
  #   input$fmodules  Facet Modules
  #   input$cmodules  Color Modules

  output$shiny_modcomp <- shiny::renderUI({
    shiny::tagList(
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
          checkboxInput(ns("abs"), "Absolute kME?"))
      ),
      
      shiny::uiOutput(ns("modcomply"))
    )
  })
  
  # Module kME information
  mods <- shiny::reactive({
    shiny::req(module_par$dataset)
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
  
  moddata <- shiny::reactive({
    shiny::req(mods(), module_par$responseF, module_par$responseC,
               input$fmodules, input$cmodules)
    
    foundr::subset_module_kMEs(mods(), module_par$responseF, module_par$responseC,
                               facetmodules = input$fmodules, colormodules = input$cmodules)
  })

  modcompplot <- shiny::reactive({
    shiny::req(moddata(), module_par$responseF, module_par$responseC)
    
    ggplot2::autoplot(
      moddata(),
      module_par$responseF, module_par$responseC, input$abs)
  })
  modcomptable <- shiny::reactive({
    shiny::req(traitModule(), module_par$dataset, module_par$response)
    
    dplyr::arrange(
      tidyr::pivot_wider(
        dplyr::select(
          summary(traitModule()[[module_par$dataset]]),
          -maxkME, -minkME),
        names_from = "response", values_from = "count",
        values_fill = 0),
      dplyr::desc(.data[[module_par$response]]))
  })
  output$modcomply <- shiny::renderUI({
    shiny::req(modcompplot(), modcomptable())
    
    shiny::tagList(
      plotly::renderPlotly({
        modcompplot()
      }),
      
      DT::renderDataTable({
        DT::datatable(
          modcomptable(),
          escape = FALSE,
          options = list(paging =TRUE, scrollX = TRUE, pageLength = 10))
      })
    )
  })

  ###############################################
  # List returned
  reactive({
    shiny::req(module_par$dataset, module_par$responseF, module_par$responseC,
               modcompplot(), modcomptable())
    list(
      plot = print(modcompplot()),
      table = modcomptable(),
      traits = c(module_par$dataset, module_par$responseF, module_par$responseC))
  })
}
