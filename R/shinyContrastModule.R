#' Shiny Module Output for Modules of Contrasts
#' @return nothing returned
#' @rdname shinyContrastModule
#' @export
shinyContrastModuleOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyContrastPlotInput(ns("shinyContrastPlot")),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("sex"))),
      shiny::column(9, shinyContrastPlotUI(ns("shinyContrastPlot")))),
    shiny::uiOutput(ns("module")),
    shinyContrastPlotOutput(ns("shinyContrastPlot")))
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
      input, main_par, contrastTable, customSettings,
      modTitle)
    
    contrastTable <- shiny::reactive({
      if(shiny::isTruthy(input$module)) traits() else eigens()      
    })
    modTitle <- shiny::reactive({
      if(shiny::isTruthy(input$module)) 
        paste("Eigentrait Contrasts for Module", input$module)
      else
        "Eigentrait Contrasts across Modules"
    })

    # INPUTS
    output$module <- shiny::renderUI({
      shiny::selectizeInput(ns("module"), "Module:", NULL)
    })

    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput(ns("sex"), "", as.vector(sexes))
    })
    
    # Restrict `traitModule` to datasets in `moduleContrast()`
    datamodule <- shiny::reactive({
      traitModule[shiny::req(main_par$dataset)]
    })
    
    # Eigen Contrasts.
    eigens <- shiny::reactive({
      shiny::req(datamodule(), moduleContrast())
      
      eigen_contrast_dataset(datamodule(), moduleContrast())
    })

    datatraits <- shiny::reactive({
      shiny::req(input$sex, eigens(), eigens(), main_par$dataset)
      
      tidyr::unite(eigens(), datatraits, dataset, trait,
                   sep = ": ")$datatraits
    }, label = "datatraits")
    shiny::observeEvent(
      shiny::req(datatraits(), main_par$dataset, input$sex),
      {
        # Use subset of traits if dealing with sex modules.
        sextraits <- datatraits()
        if(is_sex_module(shiny::req(datamodule())))
          sextraits <- sextraits[
            grep(paste0(": ", names(sexes)[match(input$sex, sexes)], "_"), sextraits)]
        
        shiny::updateSelectizeInput(session, "module", choices = sextraits,
                                    selected = "", server = TRUE)
    })
    
    # Compare Selected Module Eigens to Traits in Module
    traits <- shiny::reactive({
      shiny::req(datamodule(), input$sex, input$module, main_par$dataset,
                 traitContrast(), eigens())
      
      eigen_traits_dataset(datamodule(), input$sex, input$module,
                           traitContrast(), eigens())
    })

    ##############################################################
    eigens
  })
}
