#' Shiny Module Input for Trait Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitPanel
#' @importFrom shiny column fluidRow NS uiOutput
#' @export
#'
shinyTraitPanelInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Key Datasets and Trait.
    shiny::fluidRow(
      shiny::column(6, shinyTraitOrderInput(ns("shinyOrder"))),
      shiny::column(6, shinyTraitNamesUI(ns("shinyKeyTrait")))),
    
    # Related Datasets and Traits.
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("reldataset"))),
      shiny::column(6, shinyTraitNamesUI(ns("shinyRelTraits")))),
    
    # Correlation Type, Absolute, Minimum Settings.
    shinyCorTableUI(ns("shinyCorTable")),
    shiny::sliderInput(ns("mincor"), "Minimum:", 0, 1, 0.7)
  )
}

#' Shiny Module Output for Trait Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitPanel
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitPanelOutput <- function(id) {
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

#' Shiny Module Server for Trait Panel
#'
#' @param input,output,session standard shiny arguments
#' @param traitData,traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column downloadHandler h3 moduleServer observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitPanel <- function(id, main_par,
                            traitData, traitSignal, traitStats,
                            customSettings = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyTraitPanel inputs
    #   main_par$facet: Facet by strain?
    #   main_par$strains: Strains to select
    #   main_par$height: Plot Height
    #   main_par$plot_choice: plot choice
    #
    # RETURNS
    #   output$solos 
    #   output$pairs
    #   output$object
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- shinyTraitOrder("shinyOrder", main_par,
                                   traitStats, traitSignal,
                                   customSettings)
    
    # Key Trait.
    keyTraitOutput <- shinyTraitNames("shinyKeyTrait", main_par, orderOutput)
    
    # Key Trait and Correlation Table.
    corTableOutput <- shinyCorTable("shinyCorTable", main_par, input,
                                    keyTraitOutput, traitSignal,
                                    customSettings)
    # Related Traits.
    relTraitsOutput <- shinyTraitNames("shinyRelTraits", main_par,
                                        corTableOutput, TRUE)
    # Correlation Plot
    corPlotOutput <- shinyCorPlot("shinyCorPlot", input, main_par,
                                  corTableOutput)
    # Trait Table.
    tableOutput <- shinyTraitTable("shinyTable", input, main_par,
                                   keyTraitOutput, relTraitsOutput,
                                   traitData, traitSignal,
                                   customSettings)
    # Solo and Pairs Plots.
    solosOutput <- shinyTraitSolos("shinySolos", main_par, tableOutput)
    pairsOutput <- shinyTraitPairs("shinyPairs", main_par, trait_names,
                                   tableOutput)
    
    # Trait Names.
    trait_names <- shiny::reactive({
      c(shiny::req(keyTraitOutput()), relTraitsOutput())
      },
      label = "trait_names")
    
    # Related Datasets.
    datasets <- shiny::reactive({
      unique(traitStats$dataset)
    })
    output$reldataset <- renderUI({
      shiny::selectInput(ns("reldataset"), "Related Datasets:",
                         datasets(), datasets()[1], multiple = TRUE)
    })

    # Output
    output$traitOutput <- shiny::renderUI({
      shiny::tagList(
#      shiny::fluidRow(
#        shiny::column(3, shiny::downloadButton(ns("downloadPlots"), "Plots")),
#        shiny::column(3, shiny::downloadButton(ns("downloadTables"), "Data"))),
      switch(shiny::req(input$butshow),
             Plots = {
               shiny::tagList(
                 # Trait Table Response.
                 shinyTraitTableUI(ns("shinyTable")),
                 shiny::uiOutput(ns("plots")))
              },
             Tables = shiny::uiOutput(ns("tables"))))
    })
    # Tables
    output$tables <- shiny::renderUI({
      shiny::tagList(
        shiny::radioButtons(ns("buttable"), "", c("Cell Means","Correlations","Stats"), "Cell Means",
                            inline = TRUE),
        shinyTraitTableOutput(ns("shinyTable")),
        shinyCorTableOutput(ns("shinyCorTable")),
        shinyTraitOrderUI(ns("shinyOrder"))
      )
    })
    # Plots
    output$plots <- shiny::renderUI({
      shiny::tagList(
        shiny::h3("Trait Plots"),
        shinyTraitSolosUI(ns("shinySolos")),
        if(length(shiny::req(trait_names())) > 1)
          shiny::tagList(
            shiny::h3("Trait Pairs"),
            shinyTraitPairsUI(ns("shinyPairs"))),
        if(is_bestcor(corTableOutput()))
          shiny::tagList(
            shiny::h3("Correlations"),
            shinyCorPlotUI(ns("shinyCorPlot")),
            shinyCorPlotOutput(ns("shinyCorPlot"))))
    })
    
    # DOWNLOADS
    output$downloads <- shiny::renderUI({
      shiny::req(input$butshow)
      
      shiny::downloadButton(ns(paste0("download", input$butshow)),
                            input$butshow)
    })
    # Download File Prefix
    output$filename <- renderUI({
      shiny::req(trait_names())
      
      filename <- "Traits_"
      if(shiny::req(input$butshow) == "Tables")
        filename <- paste0(input$buttable, "_")
      filename <- paste0(filename, trait_names()[1])
      
      shiny::textAreaInput(ns("filename"), "File Prefix:", filename)
    })
    
    # Download Plot
    output$downloadPlots <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".pdf")
      },
      content = function(file) {
        shiny::req(solosOutput(), main_par$height)
        grDevices::pdf(file, width = 9, height = main_par$height)
        print(solosOutput())
        if(length(shiny::req(trait_names())) > 1)
          print(pairsOutput())
        if(is_bestcor(corTableOutput()) & shiny::isTruthy(corPlotOutput()))
          print(corPlotOutput())
        invisible()
        grDevices::dev.off()
      })
    
    # Download DataTable
    output$downloadTables <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".csv")
      },
      content = function(file) {
        shiny::req(tableOutput())
        utils::write.csv(
          switch(shiny::req(input$buttable),
                 "Cell Means" = summary(tableOutput()),
                 Correlations = summary_bestcor(
                   mutate_datasets(
                     corTableOutput(),
                     customSettings$dataset),
                   0.0),
                 Stats = summary_strainstats(
                   orderOutput(),
                   threshold = c(deviance = 0, p = 1)))
          ,
          file, row.names = FALSE)
      })
    
    ###############################################################
    trait_names
  })
}
