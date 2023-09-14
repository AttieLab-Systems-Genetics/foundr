#' Shiny Module Input for Trait Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @importFrom shiny column fluidRow NS uiOutput
#' @export
#'
shinyContrastPanelInput <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(9,
                  shinyTraitOrderInput(ns("shinyOrder"))),
    shiny::column(3,
                  shiny::numericInput(ns("ntrait"), "Rows:",
                                    20, 5, 100, 5)))
}

#' Shiny Module Output for Contrast Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyContrastPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::radioButtons(ns("butshow"), "", c("Plots","Tables"), "Plots",
                            inline = TRUE)),
      shiny::column(
        2,
        shiny::uiOutput(ns("downloads"))),
      shiny::column(
        6,
        shiny::uiOutput(ns("filename")))),
    shiny::uiOutput(ns("traitOutput"))
  )
}

#' Shiny Module Server for Contrast Panel
#'
#' @param input,output,session standard shiny arguments
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column downloadHandler h3 moduleServer observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyContrastPanel <- function(id, main_par,
                            traitSignal, traitStats,
                            customSettings = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyContrastPanel inputs
    #   main_par$tabpanel
    #   main_par$height
    #   main_par$strains
    #
    # RETURNS
    #   
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- shinyTraitOrder("shinyOrder", main_par,
                                   traitStats, traitSignal,
                                   customSettings)

    # Output
    output$traitOutput <- shiny::renderUI({
      switch(shiny::req(input$butshow),
             Plots  = shiny::uiOutput(ns("plot")),
             Tables = DT::renderDataTable(contable()))
    })
    
    # Plot
    contrasts <- shiny::reactive({
      shiny::req(orderOutput())
      
      termname <- orderOutput()$term[1]
      conditionContrasts(traitSignal, orderOutput(), termname,
                         rawStats = traitStats)
    }, label = "contrasts")
    contrasts_strains <- shiny::reactive({
      shiny::req(contrasts(), main_par$strains)
      
      dplyr::filter(contrasts(), .data$strain %in% main_par$strains)
    })
    contrastVolcano <- shiny::reactive({
      shiny::req(contrasts_strains(), sextype())
      
      plot(contrasts_strains(), bysex = sextype(), volcano = TRUE,
           interact = shiny::isTruthy(input$interact))
    }, label = "contrastVolcano")
    contrastPlot <- shiny::reactive({
      shiny::req(contrasts_strains(), input$ntrait, sextype())
      
      plot(contrasts_strains(), bysex = sextype(), ntrait = input$ntrait)
    }, label = "contrastPlot")
    
    sexes <- c("Both Sexes", "Female", "Male", "Sex Contrast")
    names(sexes) <- c("F+M","F","M","F-M")
    sextype <- shiny::reactive({
      names(sexes)[match(shiny::req(input$sex), sexes)]
    }, label = "sextype")
    
    output$plot <- shiny::renderUI({
      shiny::req(contrasts_strains())
      
      condition <- customSettings$condition
      if(shiny::isTruthy(condition))
        condition <- stringr::str_to_title(condition)
      else
        condition <- "Condition"
      
      shiny::tagList(
        shiny::h3(paste(condition, "Contrasts")),
        shiny::fluidRow(
          shiny::column(8,
                        shiny::selectInput(ns("sex"), "Sex:",
                                           as.vector(sexes))),
          shiny::column(4,
                        shiny::checkboxInput(ns("interact"), "Interactive?"))),
        shiny::uiOutput(ns("conplot")),
        shiny::uiOutput(ns("convolc")))
    })
    output$convolc <- shiny::renderUI({
      if(shiny::isTruthy(input$interact)) {
        plotly::renderPlotly(shiny::req(contrastVolcano()))
      } else {
        shiny::renderPlot(print(shiny::req(contrastVolcano())))
      }
    })
    output$conplot <- shiny::renderUI({
      if(shiny::isTruthy(input$interact)) {
        plotly::renderPlotly(shiny::req(contrastPlot()))
      } else {
        shiny::renderPlot(print(shiny::req(contrastPlot())))
      }
    })
    
    # Table
    contable <- shiny::reactive({
      summary(shiny::req(contrasts()), shiny::req(input$ntrait))
    })
    
    # DOWNLOADS
    output$downloads <- shiny::renderUI({
      shiny::req(input$butshow)
      
      shiny::downloadButton(ns(paste0("download", input$butshow)),
                            input$butshow)
    })
    # Download File Prefix
    output$filename <- renderUI({
      filename <- "Contrasts_"
      datasets <- paste(unique(orderOutput()$dataset), collapse = ",")
      filename <- paste0(filename, datasets)
      
      shiny::textAreaInput(ns("filename"), "File Prefix:", filename)
    })
    
    # Download Plot
    output$downloadPlots <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".pdf")
      },
      content = function(file) {
        shiny::req(contrastPlot(), contrastVolcano())
        grDevices::pdf(file, width = 9, height = main_par$height)
        print(contrastPlot())
        print(contrastVolcano())
        grDevices::dev.off()
      })
    
    # Download DataTable
    output$downloadTables <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".csv")
      },
      content = function(file) {
        shiny::req(contable())
        utils::write.csv(
          contable(),
          file, row.names = FALSE)
      })
    
    ###############################################################
    orderOutput
  })
}
