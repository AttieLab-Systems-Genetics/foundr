#' Shiny Module Output for Modules of Contrasts
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastModule
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
    
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("sex"), "Sex:",
                          c("Both Sexes", "Female", "Male", "Sex Contrast"))),
      shiny::column(3, shiny::uiOutput(ns("ordername"))),
      shiny::column(6, shiny::uiOutput(ns("module")))),
    
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
    
    output$ordername <- shiny::renderUI({
      orders <- if(shiny::isTruthy(input$module)) {
        c("kME","p.value")
      } else {
        c("module","kME","p.value","size")
      }
      
      shiny::selectInput(ns("ordername"), "Order by:", orders)
    })
    
    output$plots <- shiny::renderUI({
      shiny::req(input$butshow)
      
      switch(
        input$butshow,
        Plots = {
          shiny::uiOutput(ns("plotchoice"))
        },
        Tables = {
          shiny::tagList(
            shiny::h3("Eigentrait Table"),
            DT::renderDataTable(tableObject(), escape = FALSE,
              options = list(scrollX = TRUE, pageLength = 10)))
        })
    })
    output$plotchoice <- shiny::renderUI({
      shiny::tagList(
        if(shiny::isTruthy(input$module)) {
          vollabel <- "kME line:"
          volmin <- 0
          volmax <- 1
          volvalue = 0.8
          volstep = 0.1
          
          # Select module for eigen trait comparison.
          shiny::uiOutput(ns("traits"))
        } else {
          vollabel <- "Module line:"
          volmin<- 0
          volmax = 10
          volstep <- 1
          
          shiny::uiOutput(ns("eigens"))
        },
        
        # *** This gets complicated as need to update when things change
        # *** input$module, datasets(), datatraits()
        # *** Also watch out for limits on modules as this is factor.
        {
          vol <- vol_default(shiny::req(input$ordername))
          shiny::fluidRow(
            shiny::column(6, shiny::sliderInput(ns("volsd"),
              "SD line:", min = 0, max = 2, value = 1, step = 0.1)),
            shiny::column(6, shiny::sliderInput(ns("volvert"),
              paste(vol$label, "line:"), min = vol$min, max = vol$max,
              value = vol$value, step = vol$step)))
        })
    })
    threshold <- shiny::reactive({
      shiny::req(input$volvert, input$volsd)
      
      out <- c(SD = input$volsd,
               p.value = 0.01, kME = 0.8, module = 10, size = 15)
      out[input$ordername] <- input$volvert
      out
    })

    # Show Eigen Contrasts.
    eigens <- shiny::reactive({
      shiny::req(datamodule(), traitContrast())
      
      eigen_contrast_dataset(datamodule(), traitContrast())
    })
    output$eigens <- shiny::renderUI({
      shiny::req(eigens(), input$sex, panel_par$ntrait,
                 ordername = input$ordername, threshold())
      
      shiny::tagList(
        shiny::h3("Eigentrait Contrasts"),
        shiny::renderPlot(print(
          ggplot_conditionContrasts(eigens(), bysex = input$sex,
                                    ntrait = panel_par$ntrait,
                                    ordername = input$ordername))),
        shiny::renderPlot(print(
          ggplot_conditionContrasts(eigens(), bysex = input$sex,
                                    ordername = input$ordername,
                                    volcano = TRUE, threshold = threshold()))))
    })
    
    datatraits <- shiny::reactive({
      tidyr::unite(shiny::req(eigens()), datatraits, dataset, trait,
                   sep = ": ")$datatraits
    }, label = "datatraits") 
    output$module <- shiny::renderUI({
      shiny::selectizeInput(ns("module"), "Module:", shiny::req(datatraits()))
    })
    shiny::observeEvent(
      shiny::req(datasets(), input$sex, eigens()), {
      shiny::updateSelectizeInput(session, "module", choices = datatraits(),
                                  selected = "", server = TRUE)
    })
    
    # Compare Eigens to Traits
    traits <- shiny::reactive({
      shiny::req(datamodule(), input$sex, input$module,
                 traitContrast(), eigens())
      
      eigen_traits_dataset(datamodule(), input$sex, input$module,
                           traitContrast(), eigens())
    })
    output$traits <- shiny::renderUI({
      shiny::req(traits(), input$sex, input$module, panel_par$ntrait,
                 ordername = input$ordername, threshold())
      
      shiny::tagList(
        shiny::h3("Eigentrait Members"),
        shiny::renderPlot(print(
          ggplot_conditionContrasts(traits(), bysex = input$sex,
                                    ntrait = panel_par$ntrait,
                                    ordername = input$ordername))),
        shiny::renderPlot(print(
          ggplot_conditionContrasts(traits(), bysex = input$sex,
                                    ordername = input$ordername,
                                    volcano = TRUE, threshold = threshold())))
      )
    })
    
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    
    # *** broken appContrastModule
    
    # DOWNLOADS
    postfix <- shiny::reactive({
      shiny::req(input$sex, datasets())
      
      filename <- paste(datasets(), collapse = ",")
            
      if(shiny::isTruthy(input$module)) {
        filename <- input$module
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
      shiny::req(eigens(), input$ordername)
      
      summary({
        if(shiny::isTruthy(input$module))
          traits()
        else
          eigens()},
        ntrait = Inf, ordername = input$ordername)
    })
    
    ##############################################################
    eigens
  })
}
