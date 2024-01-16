#' Shiny Module Input for Contrast Plots
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPlot
#' @importFrom shiny column fluidRow NS radioButtons uiOutput
#' @export
#'
shinyContrastPlotInput <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4, shiny::radioButtons(ns("butshow"),
        "", c("Plots","Tables"), "Plots", inline = TRUE)),
      shiny::column(8, shinyDownloadsOutput(ns("downloads")))))
}
#' Shiny Module UI for Contrast Plots
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPlot
#' @importFrom shiny column fluidRow NS radioButtons uiOutput
#' @export
#'
shinyContrastPlotUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(4, shiny::uiOutput(ns("ordername"))),
    shiny::column(8, shiny::checkboxInput(ns("interact"),
                                          "Interactive?")))
}
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
    # Sliders from Volcano plot display.
    shiny::fluidRow(
      shiny::column(6, shiny::sliderInput(ns("volsd"),
        "SD line:", min = 0, max = 2, value = 1, step = 0.1)),
      shiny::column(6, shiny::uiOutput(ns("volvert")))),
  
    shiny::uiOutput(ns("traitOutput")))
}
#' Shiny Module Server for Contrast Plots
#'
#' @param id identifier
#' @param sex_par,panel_par,main_par input parameters
#' @param contrastTable reactive data frame
#' @param customSettings list of custom settings
#' @param modTitle character string title for section
#'
#' @return reactive object 
#' @importFrom shiny column moduleServer observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyContrastPlot <- function(id, sex_par, panel_par, main_par,
                            contrastTable, customSettings = NULL,
                            modTitle = shiny::reactive("Eigentrait Contrasts")) {
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

    # Input
    output$ordername <- shiny::renderUI({
      orders <- c("p.value","kME","size","module")
      orders <- orders[!is.na(match(orders, names(contrastTable())))]

      shiny::selectInput(ns("ordername"), "Order by:", orders)
    })
    vol <- shiny::reactive({ vol_default(shiny::req(input$ordername)) })
    
    output$volvert <- shiny::renderUI({
      shiny::req(vol())
      
      shiny::sliderInput(ns("volvert"),
                         paste(vol()$label, "line:"),
                         min = vol()$min, max = vol()$max,
                         value = vol()$value, step = vol()$step)
    })
    
    # Output
    output$traitOutput <- shiny::renderUI({
      switch(shiny::req(input$butshow),
             Plots  = shiny::uiOutput(ns("plot")),
             Tables = DT::renderDataTable(tableObject(), escape = FALSE,
                        options = list(scrollX = TRUE, pageLength = 10)))
    })
    
    # Plot
    
    # Filter to desired strains.
    contrasts_strains <- shiny::reactive({
      shiny::req(contrastTable(), main_par$strains)
      
      dplyr::filter(contrastTable(), .data$strain %in% main_par$strains)
    })
    
    # Generic plot function for `traits` and `eigens`.``
    plotfn <- function(data, plottype) {
      ggplot_conditionContrasts(
        data, bysex = sex_par$sex,
        ntrait = panel_par$ntrait,
        ordername = input$ordername,
        plottype = plottype, threshold = threshold(),
        strain = input$strain,
        interact = shiny::isTruthy(input$interact))
    }
    threshold <- shiny::reactive({
      shiny::req(input$volvert, input$volsd, input$ordername)
      
      out <- c(SD = input$volsd,
               p.value = 0.01, kME = 0.8, module = 10, size = 15)
      if(input$ordername == "p.value")
        out[input$ordername] <- 10 ^ -input$volvert
      else
        out[input$ordername] <- input$volvert
      out
    })
    
    contrastVolcano <- shiny::reactive({
      shiny::req(contrasts_strains())
      
      plotfn(contrasts_strains(), "volcano")
    }, label = "contrastVolcano")
    contrastBiPlot <- shiny::reactive({
      shiny::req(contrasts_strains())
      
      plotfn(contrasts_strains(), "biplot")
    }, label = "contrastBiPlot")
    contrastDotPlot <- shiny::reactive({
      shiny::req(contrasts_strains())
      
      plotfn(contrasts_strains(), "dotplot")
    }, label = "contrastDotPlot")
    
    output$plot <- shiny::renderUI({
      shiny::req(contrasts_strains(), main_par$strains)
      
      shiny::tagList(
        shiny::h3(modTitle()),
        shiny::h4({"Volcano Plot"}),
        shiny::uiOutput(ns("convolcano")),
        shiny::h4("BiPlot"),
        shiny::selectInput(ns("strain"), "Strain Highlight", c("NONE", main_par$strains)),
        shiny::uiOutput(ns("conbiplot")),
        shiny::h4("DotPlot"),
        shiny::uiOutput(ns("condotplot"))
      )
    })
    shiny::observeEvent(
      shiny::req(contrastTable(), input$ordername, vol()),
      {
        maxsd <- signif(max(abs(contrastTable()$value), na.rm = TRUE), 2)
        shiny::updateSliderInput(session, "volsd", max = maxsd)
        
        if(input$ordername == "p.value") {
          maxvert <- min(10,
                         round(-log10(min(contrastTable()$p.value, na.rm = TRUE)), 1))
        } else {
          maxvert <- vol()$max
        }
        shiny::updateSliderInput(session, "volvert", max = maxvert)
      }, label = "observeSlider")
    output$convolcano <- shiny::renderUI({
      if(shiny::isTruthy(input$interact)) {
        plotly::renderPlotly(shiny::req(contrastVolcano()))
      } else {
        shiny::renderPlot(print(shiny::req(contrastVolcano())))
      }
    })
    output$conbiplot <- shiny::renderUI({
      if(shiny::isTruthy(input$interact)) {
        shiny::tagList(
          shiny::renderText("Rays disappear if interactive."),
          shiny::renderPlot(print(shiny::req(contrastBiPlot()))),
          plotly::renderPlotly(shiny::req(contrastBiPlot())))
      } else {
        shiny::renderPlot(print(shiny::req(contrastBiPlot())))
      }
    })
    output$condotplot <- shiny::renderUI({
      if(shiny::isTruthy(input$interact)) {
        plotly::renderPlotly(shiny::req(contrastDotPlot()))
      } else {
        shiny::renderPlot(print(shiny::req(contrastDotPlot())))
      }
    })
    
    # Table
    tableObject <- shiny::reactive({
      summary(shiny::req(contrastTable()), shiny::req(panel_par$ntrait))
    })
    
    # DOWNLOADS
    postfix <- shiny::reactive({
      shiny::req(contrastTable())
      
      paste(unique(contrastTable()$dataset), collapse = ",")
    })
    plotObject <- shiny::reactive({
      print(shiny::req(contrastVolcano()))
      print(shiny::req(contrastBiPlot()))
      print(shiny::req(contrastDotPlot()))
    })
  })
}
