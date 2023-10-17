#' Shiny Module Input for Modules of Contrasts
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimeTraits
#' @export
#' @importFrom shiny NS selectInput
#'
shinyContrastModuleInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::selectInput(ns("sex"), "Sex:",
                     c("Both Sexes", "Female", "Male", "Sex Contrast"))
}

#' Shiny Module Output for Modules of Contrasts
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimeTraits
#' @export
#' @importFrom shiny NS tagList uiOutput
#'
shinyContrastModuleOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4, shiny::radioButtons(ns("butshow"),
                         "", c("Plots","Tables"), "Plots", inline = TRUE)),
      shiny::column(8, shinyDownloadsOutput(ns("downloads")))),
    
    shiny::uiOutput(ns("plots"))
  )
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
#' @importFrom shiny h3 moduleServer reactive renderPlot renderUI req
#'             selectizeInput tagList updateSelectizeInput
#' @importFrom stringr str_to_title
#' @export
#'
shinyContrastModule <- function(id, panel_par, main_par,
                              traitContrast, contrastModule,
                              customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # MODULES
    shinyDownloads("downloads", "Module", input, postfix,
                   plotObject, tableObject)
    
    # INPUTS
    # RETURNS
    #   contrastOutput
    
    datasets <- shiny::reactive({
      shiny::req(traitContrast())
      
      datasets <- unique(traitContrast()$dataset)
      datasets[datasets %in% names(contrastModule)]
    })
    # Restrict `contrastModule` to datasets in `traitContrast()`
    datamodule <- shiny::reactive({
      contrastModule[shiny::req(datasets())]
    })
    
    output$plots <- shiny::renderUI({
      shiny::req(input$butshow)
      
      switch(
        input$butshow,
        Plots = {
          shiny::tagList(
            shiny::uiOutput(ns("module")),
            shiny::uiOutput(ns("plotchoice")))
        },
        Tables = {
          shiny::tagList(
            shiny::h3("Eigentrait Table"),
            DT::renderDataTable(summary(eigens())))
        })
    })
    output$plotchoice <- shiny::renderUI({
      if(shiny::isTruthy(input$module)) {
        # Select module for eigen trait comparison.
        shiny::uiOutput(ns("traits"))
      } else {
        shiny::uiOutput(ns("eigens"))
      }
    })
    # Show Eigen Contrasts.
    eigens <- shiny::reactive({
      shiny::req(datamodule(), traitContrast())
      
      eigen_contrast_dataset(datamodule(), traitContrast())
    })
    output$eigens <- shiny::renderUI({
      shiny::req(eigens(), input$sex)
      
      shiny::tagList(
        shiny::h3("Eigentrait Contrasts"),
        shiny::renderPlot(print(
          ggplot_conditionContrasts(eigens(), bysex = input$sex))),
        shiny::renderPlot(print(
          ggplot_conditionContrasts(eigens(), bysex = input$sex,
                                    volcano = TRUE))))
    })
    
    output$module <- shiny::renderUI({
      shiny::selectizeInput(ns("module"), "Module:", 
        tidyr::unite(eigens(), datatraits, dataset, trait,
                     sep = ": ")$datatraits)
    })
    observeEvent(datasets(), {
      shiny::updateSelectizeInput(session, "module", selected = NULL,
                                  server = TRUE)
    })
    
    # Compare Eigens to Traits
    traits <- shiny::reactive({
      shiny::req(datamodule(), input$sex, input$module,
                 traitContrast(), eigens())
      
      eigen_traits_dataset(datamodule(), input$sex, input$module,
                           traitContrast(), eigens())
    })
    output$traits <- shiny::renderUI({
      shiny::req(traits(), input$sex, input$module)
      
      shiny::tagList(
        shiny::h3("Eigentrait Members"),
        shiny::renderPlot(print(
          ggplot_conditionContrasts(traits(), bysex = input$sex))),
        shiny::renderPlot(print(
          ggplot_conditionContrasts(traits(), bysex = input$sex,
                                    volcano = TRUE)))
      )
    })
    
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    
    # DOWNLOADS
    postfix <- shiny::reactive({
      shiny::req(input$sex, datasets())
      
      filename <- paste(datasets(), collapse = ",")
            
      if(shiny::isTruthy(input$module) &&
         input$butshow == "Plots") {
        filename <- paste(filename, input$module, sep = "_")
      } else {
        filename <- paste(filename, names(sexes)[match(input$sex, sexes)], 
                          sep = "_")
      }
      filename
    })
    plotObject <- shiny::reactive({
      shiny::req(input$sex)
      if(shiny::isTruthy(input$module)) {
        shiny::req(traits())
        
        print(plot(traits(), bysex = input$sex))
        print(plot(traits(), bysex = input$sex, volcano = TRUE))
      } else {
        shiny::req(eigens())
        
        print(plot(eigens(), bysex = input$sex))
        print(plot(eigens(), bysex = input$sex, volcano = TRUE))
      }
    })
    tableObject <- shiny::reactive({
      shiny::req(eigens())
      
      summary(eigens())
    })
    
    ##############################################################
    eigens
  })
}
