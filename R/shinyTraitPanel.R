#' Shiny Module Input for Trait Panel
#' @return nothing returned
#' @rdname shinyTraitPanel
#' @export
shinyTraitPanelInput <- function(id) { # 4:Order, 8:Traits
  ns <- shiny::NS(id)
  shiny::tagList(
    # Key Datasets and Trait.
    shiny::fluidRow(
      shiny::column(4, shinyTraitOrderInput(ns("shinyOrder"))),
      shiny::column(8, shinyTraitNamesUI(ns("shinyKeyTrait")))))
}
#' Shiny Module UI for Trait Panel
#' @return nothing returned
#' @rdname shinyTraitPanel
#' @export
shinyTraitPanelUI <- function(id) { # Related Datasets and Traits
  ns <- shiny::NS(id)
  shiny::tagList(
    # Related Datasets and Traits.
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("reldataset"))),
      shiny::column(6, shinyTraitNamesUI(ns("shinyRelTraits")))))
}
#' Shiny Module Output for Trait Panel
#' @return nothing returned
#' @rdname shinyTraitPanel
#' @export
shinyTraitPanelOutput <- function(id) { # Plots or Tables
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("text")),
    shiny::fluidRow(
      shiny::column(4, shiny::radioButtons(ns("butshow"),
        "", c("Plots","Tables"), "Plots", inline = TRUE)),
      shiny::column(8, shinyDownloadsOutput(ns("downloads")))),
    shiny::fluidRow(
      shiny::column(6, shinyTraitTableUI(ns("shinyTable"))), # Response
      shiny::column(6, shiny::uiOutput(ns("downtable")))),
    shiny::fluidRow(
      shiny::column(9, shiny::uiOutput(ns("strains"))),
      shiny::column(3, shiny::checkboxInput(ns("facet"), "Facet by strain?", TRUE))),
    shiny::uiOutput(ns("traitOutput")))
}
#' Shiny Module Server for Trait Panel
#'
#' @param id identifier for shiny reactive
#' @param traitData,traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column fluidRow h3 moduleServer NS observeEvent reactive
#'             reactiveVal renderUI req selectInput tagList uiOutput updateSelectInput
#' @importFrom DT renderDataTable
#' @importFrom stringr str_remove str_replace
#' @export
shinyTraitPanel <- function(id, main_par,
                            traitData, traitSignal, traitStats,
                            customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyTraitPanel inputs
    #   main_par$height: Plot Height
    #   input$butshow: show Plots or Tables
    #   input$mincor: minimum correlation
    #   input$reldataset: relative datasets
    #   input$facet: Facet by strain?
    #   input$strains: Strains to select
    #
    # RETURNS
    #   trait_names()
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- shinyTraitOrder("shinyOrder", input, main_par,
                                   traitStats, customSettings)
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
                                  corTableOutput, customSettings)
    # Trait Table.
    tableOutput <- shinyTraitTable("shinyTable", input, main_par,
                                   keyTraitOutput, relTraitsOutput,
                                   traitData, traitSignal,
                                   customSettings)
    # Solo and Pairs Plots.
    solosOutput <- shinyTraitSolos("shinySolos", input, main_par, tableOutput)
    pairsOutput <- shinyTraitPairs("shinyPairs", input, main_par, trait_names,
                                   tableOutput)
    # Downloads
    shinyDownloads("downloads", "Trait", input, postfix,
                   plotObject, tableObject)
    
    # SERVER-SIDE Inputs
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput(ns("strains"), "Strains",
        choices = choices, selected = choices, inline = TRUE)
    })
    
    # Trait Names.
    trait_names <- shiny::reactive({
      c(shiny::req(keyTraitOutput()), relTraitsOutput())
      },
      label = "trait_names")
    
    # Related Datasets.
    output$reldataset <- renderUI({
      datasets <- unique(traitStats$dataset)
      selected <- data_selection()
      shiny::selectInput(ns("reldataset"), "Related Datasets:",
                         datasets, selected, multiple = TRUE)
    })
    data_selection <- shiny::reactiveVal(unique(traitStats$dataset)[1], label = "data_selection")
    shiny::observeEvent(input$reldataset, data_selection(input$reldataset))
    

    # Output
    output$text <- shiny::renderUI({
      condition <- customSettings$condition
      if(shiny::isTruthy(condition))
        condition <- tolower(condition)
      else
        condition <- "Condition"
      
      shiny::tagList(
        shiny::h3("Traits"),
        shiny::renderText({
          paste0(
            "This panel examines traits by ",
            condition, ", strain and sex. ",
            "Traits are typically ordered by significance of model terms. ",
            "Response value shows raw data; normed shows values after normal scores preserving mean and SD;",
            "cellmean shows normed values averaged over replicates. ",
            "Selecting Related Traits yields multiple Trait Plots plus Pairs Plots. ",
            "Correlation sorts Related Traits.")
        }))
    })
    output$downtable <- shiny::renderUI({
      if(shiny::req(input$butshow == "Tables")) {
        shiny::radioButtons(ns("buttable"), "Download:",
          c("Cell Means","Correlations","Stats"), "Cell Means", inline = TRUE)
      }
    })
    output$traitOutput <- shiny::renderUI({
      shiny::tagList(
        switch(shiny::req(input$butshow),
          Plots = {
            shiny::tagList(
              shiny::h3("Trait Plots"),
              # Trait Solos Plot
              shinyTraitSolosUI(ns("shinySolos")),
              # Trait Pairs Plot
              if(length(shiny::req(trait_names())) > 1)
                shiny::tagList(
                  shiny::h3("Trait Pairs"),
                  shinyTraitPairsUI(ns("shinyPairs"))))
          },
          Tables = {
            shiny::tagList(
              shinyTraitTableOutput(ns("shinyTable")),
              shinyTraitOrderUI(ns("shinyOrder")))
          }),
        
        # Correlation Plots or Tables
        switch(shiny::req(input$butshow),
          Plots = {
            if(is_bestcor(corTableOutput()))
              shinyCorPlotOutput(ns("shinyCorPlot"))
          },
          Table = {
            shinyCorTableOutput(ns("shinyCorTable"))
          }))
    })

    # DOWNLOADS
    postfix <- shiny::reactive({
      filename <- stringr::str_replace(trait_names()[1], ": ", "_")
      if(shiny::req(input$butshow) == "Tables")
        filename <- paste0(stringr::str_remove(input$buttable, " "), "_",
                           filename)
      filename
    })
    plotObject <- shiny::reactive({
      shiny::req(solosOutput(), main_par$height)
      
      print(solosOutput())
      if(length(shiny::req(trait_names())) > 1)
        print(pairsOutput())
      if(is_bestcor(corTableOutput()) & shiny::isTruthy(corPlotOutput()))
        print(corPlotOutput())
    })
    tableObject <- shiny::reactive({
      shiny::req(tableOutput())
      switch(shiny::req(input$buttable),
             "Cell Means" = summary(tableOutput()),
             Correlations = summary_bestcor(
               mutate_datasets(corTableOutput(), customSettings$dataset),
               0.0),
             Stats = summary_strainstats(orderOutput(),
                                         threshold = c(deviance = 0, p = 1)))
    })

    ###############################################################
    trait_names
  })
}
