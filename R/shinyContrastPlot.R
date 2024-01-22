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
  
    shiny::uiOutput(ns("title")),
    
    shiny::uiOutput(ns("traitOutput")))
}
#' Shiny Module Server for Contrast Plots
#'
#' @param id identifier
#' @param sex_par,main_par input parameters
#' @param contrastTable reactive data frame
#' @param customSettings list of custom settings
#' @param modTitle character string title for section
#'
#' @return reactive object 
#' @importFrom shiny column moduleServer observeEvent
#'             reactive reactiveVal renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyContrastPlot <- function(id, sex_par, main_par,
                            contrastTable, customSettings = NULL,
                            modTitle = shiny::reactive("Eigentrait Contrasts")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyContrastPlot inputs
    #   main_par$tabpanel
    #   main_par$height
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
    ord_selection <- shiny::reactiveVal(NULL, label = "ord_selection")
    shiny::observeEvent(input$ordername, ord_selection(input$ordername))
    
    vol <- shiny::reactive({ vol_default(shiny::req(ord_selection())) },
                           label = "vol")
    
    output$volvert <- shiny::renderUI({
      shiny::req(vol())
      
      shiny::sliderInput(ns("volvert"),
                         paste(vol()$label, "line:"),
                         min = vol()$min, max = vol()$max,
                         value = vol()$value, step = vol()$step)
    })
    vert_selection <- shiny::reactiveVal(NULL, label = "vert_selection")
    shiny::observeEvent(input$volvert, vert_selection(input$volvert))

    output$title <- shiny::renderUI({
      shiny::tagList(
        shiny::h3(modTitle()),
        shiny::uiOutput(ns("rownames"))
      )
    })
    output$rownames <- shiny::renderUI({
      title <- shiny::req(info())$title
      if(title == "Strains") {
        choices <- names(foundr::CCcolors)
      } else {
        choices <- termStats(contrastTable(), signal = FALSE, drop_noise = TRUE)
      }
        shiny::checkboxGroupInput(ns("rownames"), "",
          choices = choices, selected = choices, inline = TRUE)
    })
    row_selection <- shiny::reactiveVal(NULL, label = "row_selection")
    shiny::observeEvent(input$rownames, row_selection(input$rownames))
    
    # Output
    output$traitOutput <- shiny::renderUI({
      switch(shiny::req(input$butshow),
             Plots  = shiny::uiOutput(ns("plot")),
             Tables = DT::renderDataTable(tableObject(), escape = FALSE,
                        options = list(scrollX = TRUE, pageLength = 10)))
    })
    
    # Plot
    info <- shiny::reactive({
      # Set up particulars for contrast or stat
      if(inherits(shiny::req(contrastTable()), "conditionContrasts"))
        list(row = "strain", col = "value", title = "Strains")
      else
        list(row = "term", col = "SD", title = "Terms")
    })
    # Filter to desired strains.
    contrasts_strains <- shiny::reactive({
      shiny::req(contrastTable(), row_selection(), info())
      
      dplyr::filter(contrastTable(), .data[[info()$row]] %in% row_selection())
    })
    
    # Generic plot function for `traits` and `eigens`.``
    plotfn <- function(data, plottype) {
      ggplot_conditionContrasts(
        data, bysex = sex_par$sex,
        ntrait = input$ntrait,
        ordername = ord_selection(),
        plottype = plottype, threshold = threshold(),
        strain = input$strain,
        interact = shiny::isTruthy(input$interact))
    }
    threshold <- shiny::reactive({
      shiny::req(vert_selection(), input$volsd, ord_selection())
      
      out <- c(SD = input$volsd,
               p.value = 0.01, kME = 0.8, module = 10, size = 15)
      if(ord_selection() == "p.value")
        out[ord_selection()] <- 10 ^ -vert_selection()
      else
        out[ord_selection()] <- vert_selection()
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
      shiny::req(contrasts_strains(), input$ntrait)
      
      plotfn(contrasts_strains(), "dotplot")
    }, label = "contrastDotPlot")
    
    output$plot <- shiny::renderUI({
      shiny::tagList(
        shiny::h4("Volcano Plot"),
        shiny::uiOutput(ns("convolcano")),
        shiny::h4("BiPlot"),
        shiny::selectInput(ns("strain"), "Vector Highlight", c("NONE", row_selection())),
        shiny::uiOutput(ns("conbiplot")),
        shiny::h4("DotPlot"),
        shiny::uiOutput(ns("condotplot"))
      )
    })
    shiny::observeEvent(
      shiny::req(contrastTable(), ord_selection(), vol(), info()),
      {
        maxsd <- signif(max(abs(contrastTable()[[info()$col]]), na.rm = TRUE), 2)
        shiny::updateSliderInput(session, "volsd", max = maxsd)
        
        if(ord_selection() == "p.value") {
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
      shiny::tagList(
        shiny::numericInput(ns("ntrait"), "Traits:", 20, 5, 100, 5),
        
        if(shiny::isTruthy(input$interact)) {
          plotly::renderPlotly(shiny::req(contrastDotPlot()))
        } else {
          shiny::renderPlot(print(shiny::req(contrastDotPlot())))
        })
    })
    
    # Table
    tableObject <- shiny::reactive({
      summary(shiny::req(contrastTable()), 
              ntrait = shiny::req(input$ntrait))
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
