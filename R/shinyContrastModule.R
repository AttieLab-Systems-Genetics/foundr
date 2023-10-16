#' Shiny Module Input for Modules of Contrasts
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimeTraits
#' @export
#' @importFrom shiny column fluidRow NS selectInput uiOutput
#'
shinyContrastModuleInput <- function(id) {
  ns <- shiny::NS(id)
  
  # Datasets and Sex.
  shiny::fluidRow(
    shiny::column(6, shiny::uiOutput(ns("dataset"))),
    shiny::column(6, shiny::selectInput(ns("sex"), "Sex:",
                       c("Both Sexes", "Female", "Male", "Sex Contrast"))))
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
    
    shiny::checkboxInput(ns("members"), "Module Members?"),
    shiny::uiOutput(ns("eigens"))
  )
}

#' Shiny Module Server for Modules of Contrasts
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitContrast,contrastModule static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny h3 moduleServer reactive renderPlot renderUI req
#'             selectInput tagList
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
    
    # Datasets.
    output$dataset <- renderUI({
      datasets <- unique(traitContrast()$dataset)
      shiny::selectInput(ns("dataset"), "Datasets:", datasets, "LivMet",
                         multiple = TRUE)
    })
    
    # Show Eigen Contrasts.
    eigens <- shiny::reactive({
      shiny::req(contrastModule(), traitContrast())
      
      eigen_contrast(contrastModule(), traitContrast())
    })
    output$eigens <- shiny::renderUI({
      shiny::req(eigens(), input$sex)
      
      switch(
        input$butshow,
        Plots = {
          if(shiny::isTruthy(input$members)) {
            shiny::uiOutput(ns("module"))
          } else {
            shiny::tagList(
              shiny::h3("Eigentrait Contrasts"),
              shiny::renderPlot(print(plot(eigens(), bysex = input$sex))),
              shiny::renderPlot(print(plot(eigens(), bysex = input$sex,
                                           volcano = TRUE))))
          }
        },
        Tables = {
          shiny::tagList(
            shiny::h3("Eigentrait Table"),
            DT::renderDataTable(summary(eigens())))
        })
    })
    
    # Select module for eigen trait comparison.
    output$module <- shiny::renderUI({
      shiny::req(eigens())
      
      shiny::tagList(
        shiny::h3("Eigentrait Members"),
        shiny::selectInput(ns("module"), "Module:", eigens()$trait),
        shiny::uiOutput(ns("traits"))
      )
    })
    
    # Compare Eigens to Traits
    traits <- shiny::reactive({
      shiny::req(contrastModule(), input$sex, input$module,
                 traitContrast(), eigens())
      
      eigen_traits(contrastModule(), input$sex, input$module,
                   traitContrast(), eigens())
    })
    output$traits <- shiny::renderUI({
      shiny::req(traits(), input$sex)
      
      shiny::tagList(
        shiny::renderPlot(print(plot(traits(), bysex = input$sex))),
        shiny::renderPlot(print(plot(traits(), bysex = input$sex,
                                     volcano = TRUE)))
      )
    })
    
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    
    # DOWNLOADS
    postfix <- shiny::reactive({
      shiny::req(input$sex)
      
      filename <- paste(input$dataset, collapse = ",")
            
      if(shiny::isTruthy(input$members) && shiny::isTruthy(input$module) &&
         input$butshow == "Plots") {
        filename <- paste(filename, input$module, sep = "_")
      } else {
        filename <- paste(filename, names(sexes)[match(input$sex, sexes)], 
                          sep = "_")
      }
      filename
    })
    plotObject <- shiny::reactive({
      shiny::req(traits(), input$sex)
      if(shiny::isTruthy(input$members)) {
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
  })
}
