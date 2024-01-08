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
      shiny::column(3, shiny::uiOutput(ns("module"))),
      shiny::column(3, shiny::checkboxInput(ns("interact"),
                                            "Interactive?"))),
    
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
        c("p.value","kME")
      } else {
        c("p.value","kME","size","module")
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
      vol <- vol_default(shiny::req(input$ordername))
      shiny::tagList(
        {
          shiny::fluidRow(
            shiny::column(6, shiny::sliderInput(ns("volsd"),
                                                "SD line:", min = 0, max = 2, value = 1, step = 0.1)),
            shiny::column(6, shiny::sliderInput(ns("volvert"),
                                                paste(vol$label, "line:"), min = vol$min, max = vol$max,
                                                value = vol$value, step = vol$step)))
        },
        shiny::selectInput(ns("strain"), "Strain SD", c("NONE",names(qtl2::CCcolors))),
        if(shiny::isTruthy(input$module)) {
          shiny::uiOutput(ns("traits"))
        } else {
          shiny::uiOutput(ns("eigens"))
        })
    })
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

    # Generic plot function for `traits` and `eigens`.``
    plotfn <- function(data, plottype) {
      ggplot_conditionContrasts(
        data, bysex = input$sex,
        ntrait = panel_par$ntrait,
        ordername = input$ordername,
        plottype = plottype, threshold = threshold(),
        strain = input$strain)
    }
    
    # Show Eigen Contrasts.
    eigens <- shiny::reactive({
      shiny::req(datamodule(), traitContrast())
      
      eigen_contrast_dataset(datamodule(), traitContrast())
    })
    eigen_volcano <- shiny::reactive({
      shiny::req(eigens())
      
      plotfn(eigens(), "volcano")
    })
    eigen_biplot <- shiny::reactive({
      shiny::req(eigens())
      
      plotfn(eigens(), "biplot")
    })
    eigen_dotplot <- shiny::reactive({
      shiny::req(eigens())
      
      plotfn(eigens(), "dotplot")
    })
    output$eigens <- shiny::renderUI({
      shiny::req(eigens(), input$sex, panel_par$ntrait,
                 ordername = input$ordername, threshold(),
                 input$strain)
      
      shiny::tagList(
        shiny::h3("Eigentrait Contrasts"),
        shiny::h4("Volcano Plot"),
        if(shiny::isTruthy(input$interact)) {
          plotly::renderPlotly(shiny::req(eigen_volcano()))
        } else {
          shiny::renderPlot(print(shiny::req(eigen_volcano())))
        },
        shiny::h4("Biplot"),
        if(shiny::isTruthy(input$interact)) {
          shiny::tagList(
            shiny::renderText("Rays disappear if interactive."),
            shiny::renderPlot(print(shiny::req(eigen_biplot()))),
            plotly::renderPlotly(shiny::req(eigen_biplot())))
        } else {
          shiny::renderPlot(print(shiny::req(eigen_biplot())))
        },
        shiny::h4("Dotplot"),
        if(shiny::isTruthy(input$interact)) {
          plotly::renderPlotly(shiny::req(eigen_dotplot()))
        } else {
          shiny::renderPlot(print(shiny::req(eigen_dotplot())))
        })
    })
    
    datatraits <- shiny::reactive({
      shiny::req(input$sex)
      
      tidyr::unite(shiny::req(eigens()), datatraits, dataset, trait,
                   sep = ": ")$datatraits
    }, label = "datatraits") 
    output$module <- shiny::renderUI({
      shiny::selectizeInput(ns("module"), "Module:", shiny::req(datatraits()))
    })
    shiny::observeEvent(
      shiny::req(datasets(), input$sex, eigens()), {
      sextraits <- datatraits()[
        grep(paste0(": ", names(sexes)[match(input$sex, sexes)], "_"),
             datatraits())]
        
      shiny::updateSelectizeInput(session, "module", choices = sextraits,
                                  selected = "", server = TRUE)
    })
    
    # Compare Eigens to Traits
    traits <- shiny::reactive({
      shiny::req(datamodule(), input$sex, input$module,
                 traitContrast(), eigens())
      
      eigen_traits_dataset(datamodule(), input$sex, input$module,
                           traitContrast(), eigens())
    })
    trait_volcano <- shiny::reactive({
      shiny::req(traits())
      
      plotfn(traits(), "volcano")
    })
    trait_biplot <- shiny::reactive({
      shiny::req(traits())
      
      plotfn(traits(), "biplot")
    })
    trait_dotplot <- shiny::reactive({
      shiny::req(traits())
      
      plotfn(traits(), "dotplot")
    })
    output$traits <- shiny::renderUI({
      shiny::req(traits(), input$sex, input$module, panel_par$ntrait,
                 ordername = input$ordername, threshold())
      
      shiny::tagList(
        shiny::h3("Eigentrait Members"),
        shiny::h4("Volcano Plot"),
        if(shiny::isTruthy(input$interact)) {
          plotly::renderPlotly(shiny::req(trait_volcano()))
        } else {
          shiny::renderPlot(print(shiny::req(trait_volcano())))
        },
        shiny::h4("Biplot"),
        if(shiny::isTruthy(input$interact)) {
          shiny::tagList(
            shiny::renderText("Rays disappear if interactive."),
            shiny::renderPlot(print(shiny::req(trait_biplot()))),
            plotly::renderPlotly(shiny::req(trait_biplot())))
        } else {
          shiny::renderPlot(print(shiny::req(trait_biplot())))
        },
        shiny::h4("Dotplot"),
        if(shiny::isTruthy(input$interact)) {
          plotly::renderPlotly(shiny::req(trait_dotplot()))
        } else {
          shiny::renderPlot(print(shiny::req(trait_dotplot())))
        })
    })
    
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    
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
        
        print(trait_volcano())
        print(trait_biplot())
        print(trait_dotplot())
      } else {
        shiny::req(eigens())
        
        print(eigen_volcano())
        plotfn(eigen_biplot())
        plotfn(eigen_dotplot())
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
