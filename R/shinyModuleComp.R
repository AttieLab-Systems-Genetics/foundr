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
#' @param main_par reactive arguments from `foundrServer` 
#' @param traitModule reactive object with list created by `listof_wgcnamodules`
#'
#' @return reactive object for `shinyModuleCompUI`
#' @importFrom shiny plotOutput reactive renderPlot renderUI req
#'             tagList uiOutput
#' @importFrom DT renderDataTable
#' @export
#'
shinyModuleComp <- function(input, output, session,
                        main_par, traitModule) {
  ns <- session$ns
  
  # INPUTS
  # Main inputs: (see shinyapp.R)
  #   main_par$height (see shinyapp.R::foundrUI sidebarPanel)
  # Dendro inputs: (see shinyModules.R)
  #   input$dataset
  #   input$response

  # OUTPUTS
  # output$dendro dendro plot and table
  
  # RETURNS
  # list with
  #   plot (see dendroplots() below)
  #   table (see dendrotable() below)
  
  datasets <- shiny::reactive(
    c("LivMet","PlaMet0","PlaMet120","Metab"))
  responses <- shiny::reactive(
    c("value","cellmean","signal","rest","noise"))
  
  output$shiny_modcomp <- shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("dataset"), "Dataset:",
            datasets())),
        shiny::column(
          6,
          shiny::selectInput(
            ns("response"), "Response:",
            responses()))),
      
      shiny::uiOutput(ns("modcomp")))
  })
  
  mods <- shiny::reactive({
    shiny::req(input$dataset)
    foundr::module_kMEs(traitModule()[[input$dataset]])
  })
  fmodules <- shiny::reactive({
    shiny::req(mods(), input$responseF)
    levels(mods()[[paste0(input$responseF, "_col")]])
  })
  cmodules <- shiny::reactive({
    shiny::req(mods(), input$responseC)
    levels(mods()[[paste0(input$responseC, "_col")]])
  })
  
  output$modcomp <- shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectInput(
            ns("responseF"), "Facet Response:",
            "cellmean")),
        shiny::column(
          4,
          shiny::selectInput(
            ns("responseC"), "Color Response:",
            "value"))),

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
  
  responseFs <- shiny::reactive({
    shiny::req(input$response)
    unique(c(input$response, responses))
  })
  responseCs <- shiny::reactive({
    shiny::req(input$responseF)
    responses()[responses() != input$responseF]
  })
  shiny::observeEvent(
    input$response,
    {
      shiny::updateSelectInput(
        session, "responseF",
        choices = responseFs(),
        selected = responseFs()[1])
    })
  shiny::observeEvent(
    input$responseF,
    {
      shiny::updateSelectInput(
        session, "responseC",
        choices = responseCs(),
        selected = responseCs()[1])
      
      shiny::updateSelectInput(
        session, "fmodules",
        choices = fmodules(),
        selected = fmodules())
    })
  shiny::observeEvent(
    input$responseC,
    {
      shiny::updateSelectInput(
        session, "cmodules",
        choices = cmodules(),
        selected = cmodules())
    })
  
  moddata <- shiny::reactive({
    shiny::req(mods(), input$responseF, input$responseC,
               input$fmodules, input$cmodules)
    
    foundr::subset_module_kMEs(mods(), input$responseF, input$responseC,
                               facetmodules = input$fmodules, colormodules = input$cmodules)
  })

  modcompplot <- shiny::reactive({
    shiny::req(moddata(), input$responseF, input$responseC)
    
    ggplot2::autoplot(
      moddata(),
      input$responseF, input$responseC, input$abs)
  })
  modcomptable <- shiny::reactive({
    shiny::req(traitModule(), input$dataset, input$response)
    
    dplyr::arrange(
      tidyr::pivot_wider(
        dplyr::select(
          summary(traitModule()[[input$dataset]]),
          -maxkME, -minkME),
        names_from = "response", values_from = "count",
        values_fill = 0),
      dplyr::desc(.data[[input$response]]))
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
          options = list(paging =TRUE, pageLength = 5))
      })
    )
  })

  ###############################################
  # List returned
  reactive({
    shiny::req(input$dataset, input$response,
               modcompplot(), modcomptable())
    list(
      plot = print(modcompplot()),
      table = modcomptable(),
      traits = c(input$dataset, input$response))
  })
}

      
  