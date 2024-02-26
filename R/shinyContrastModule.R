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
#' @param traitContrast reactive data frames
#' @param contrastModule static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny h3 moduleServer NS reactive renderPlot renderUI req
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom stringr str_to_title
#' @export
#'
shinyContrastModule <- function(id, panel_par, main_par,
                              traitContrast, contrastModule,
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
      shiny::selectizeInput(ns("module"), "Module:",
                            shiny::req(datatraits()))
    })
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput(ns("sex"), "", as.vector(sexes))
    })
    
    datasets <- shiny::reactive({
      shiny::req(traitContrast())
      
      datasets <- unique(traitContrast()$dataset)
      datasets[datasets %in% names(contrastModule)]
    })
    # Restrict `contrastModule` to datasets in `traitContrast()`
    datamodule <- shiny::reactive({
      contrastModule[shiny::req(datasets())]
    })
    
    # Eigen Contrasts.
    eigens <- shiny::reactive({
      shiny::req(datamodule(), traitContrast())
      
      eigen_contrast_dataset(datamodule(), traitContrast())
    })

    datatraits <- shiny::reactive({
      shiny::req(input$sex)
      
      tidyr::unite(shiny::req(eigens()), datatraits, dataset, trait,
                   sep = ": ")$datatraits
    }, label = "datatraits") 
    shiny::observeEvent(
      shiny::req(datasets(), input$sex, eigens()), {
      sextraits <- datatraits()[
        grep(paste0(": ", names(sexes)[match(input$sex, sexes)], "_"),
             datatraits())]
        
      shiny::updateSelectizeInput(session, "module", choices = sextraits,
                                  selected = "", server = TRUE)
    })
    
    # Compare Selected Module Eigens to Traits in Module
    traits <- shiny::reactive({
      shiny::req(datamodule(), input$sex, input$module,
                 traitContrast(), eigens())
      
      eigen_traits_dataset(datamodule(), input$sex, input$module,
                           traitContrast(), eigens())
    })

    ##############################################################
    eigens
  })
}
