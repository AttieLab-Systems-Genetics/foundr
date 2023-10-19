#' Shiny Module Output for Contrast Plots
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPlot
#' @importFrom shiny column fluidRow NS radioButtons uiOutput
#' @export
#'
shinyContrastPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4, shiny::radioButtons(ns("butshow"),
                         "", c("Plots","Tables"), "Plots", inline = TRUE)),
      shiny::column(8, shinyDownloadsOutput(ns("downloads")))),
    
    shiny::uiOutput(ns("traitOutput")))
}

#' Shiny Module Server for Contrast Plots
#'
#' @param id identifier
#' @param panel_par,main_par input parameters
#' @param contrastTable reactive data frame
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column moduleServer observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyContrastPlot <- function(id, panel_par, main_par,
                            contrastTable, customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyContrastPlot inputs
    #   main_par$tabpanel
    #   main_par$height
    #   main_par$strains
    # RETURNS
    
    # MODULES
    shinyDownloads("downloads", "Contrast", input, postfix,
                   plotObject, tableObject)

    # Output
    output$traitOutput <- shiny::renderUI({
      switch(shiny::req(input$butshow),
             Plots  = shiny::uiOutput(ns("plot")),
             Tables = DT::renderDataTable(tableObject(), escape = FALSE,
                        options = list(scrollX = TRUE, pageLength = 10)))
    })
    
    # Plot
    contrasts_strains <- shiny::reactive({
      shiny::req(contrastTable(), main_par$strains)
      
      dplyr::filter(contrastTable(), .data$strain %in% main_par$strains)
    })
    contrastPlot <- shiny::reactive({
      shiny::req(contrasts_strains(), panel_par$ntrait, input$sex)
      
      plot(contrasts_strains(), bysex = input$sex, ntrait = panel_par$ntrait)
    }, label = "contrastPlot")
    contrastVolcano <- shiny::reactive({
      shiny::req(contrasts_strains(), input$sex,
                 input$volsd, input$volpval)
      
      plot(contrasts_strains(), bysex = input$sex, volcano = TRUE,
           threshold = c(SD = input$volsd, p = 10 ^ -input$volpval),
           interact = shiny::isTruthy(input$interact))
    }, label = "contrastVolcano")
    
    output$plot <- shiny::renderUI({
      shiny::req(contrasts_strains())
      
      sexes <- c("Both Sexes", "Female", "Male", "Sex Contrast")
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(8, shiny::selectInput(ns("sex"), "", sexes)),
          shiny::column(4, shiny::checkboxInput(ns("interact"),
                             "Interactive?"))),
        shiny::uiOutput(ns("conplot")),
        shiny::uiOutput(ns("convolc")),
        
        # Sliders from Volcano plot display.
        shiny::fluidRow(
          shiny::column(6, shiny::sliderInput(ns("volsd"),
            "SD line:", min = 0, max = 2, value = 1, step = 0.1)),
          shiny::column(6, shiny::sliderInput(ns("volpval"),
            "-log10(p.value) line:", min = 0, max = 10, value = 2, step = 0.5)))
      )
    })
    shiny::observeEvent(
      shiny::req(contrastTable()),
      {
        maxsd <- signif(max(abs(contrastTable()$value), na.rm = TRUE), 2)
        shiny::updateSliderInput(session, "volsd", max = maxsd)
        
        maxpval <- min(10,
          round(-log10(min(contrastTable()$p.value, na.rm = TRUE)), 1))
        shiny::updateSliderInput(session, "volpval", max = maxpval)
      }, label = "observeSlider")
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
    tableObject <- shiny::reactive({
      summary(shiny::req(contrastTable()), shiny::req(panel_par$ntrait))
    })
    
    # DOWNLOADS
    postfix <- shiny::reactive({
      paste(unique(shiny::req(contrastTable())$dataset), collapse = ",")
    })
    plotObject <- shiny::reactive({
      print(shiny::req(contrastPlot()))
      print(shiny::req(contrastVolcano()))
    })
  })
}
