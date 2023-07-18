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
      shiny::column(6, shinyCorTableInput(ns("shinyCorTable")))),
    
    # Related Datasets and Traits.
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("reldataset"))),
      shiny::column(6, shinyTraitNamesUI(ns("shinyNames")))),
    
    # Correlation Type, Absolute, Minimum Settings.
    shiny::fluidRow( 
      shiny::column(6, shinyCorTableUI(ns("shinyCorTable"))),
      shiny::column(6, shinyCorPlotUI(ns("shinyCorPlot")))),
    shiny::sliderInput(ns("mincor"), "Minimum:", 0, 1, 0.7),
    
    # Trait Table Response.
    shinyTraitTableUI(ns("shinyTable")),
    
    # Plot and Table Choices
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("plot_choice"))),
      shiny::column(6, shiny::uiOutput(ns("table_choice"))))
  )
}

#' Shiny Module UI for Trait Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitPanel
#' @importFrom shiny column fluidRow NS uiOutput
#' @export
#'
shinyTraitPanelUI <- function(id) {
  ns <- shiny::NS(id)
  
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
    shiny::uiOutput(ns("plots")),
    shiny::uiOutput(ns("tables"))
  )
}

#' Shiny Module Server for Trait Panel
#'
#' @param input,output,session standard shiny arguments
#' @param traitData static data frame
#' @param traitSignal,traitStats reactive data frames
#'
#' @return reactive object 
#' @importFrom shiny callModule column fluidRow observeEvent reactive
#'             reactiveVal renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitPanel <- function(id, module_par,
                            traitData, traitSignal, traitStats) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyTraitPanel inputs
    #   module_par$facet: Facet by strain?
    #   module_par$strains: Strains to select
    #   module_par$height: Plot Height
    #   module_par$plot_choice: plot choice
    #
    # RETURNS
    #   output$solos 
    #   output$pairs
    #   output$object
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- shinyTraitOrder("shinyOrder", traitStats)
    # Key Trait and Correlation Table.
    corTableOutput <- shinyCorTable("shinyCorTable", input, orderOutput,
                                    traitSignal)
    # Related Traits.
    rel_traitsOutput <- shinyTraitNames("shinyNames", corTableOutput, TRUE)
    # Correlation Plot
    corPlotOutput <- shinyCorPlot("shinyCorPlot", input, module_par,
                                  corTableOutput)
    
    # Filter static traitData based on selected trait_names.
    traitDataInput <- shiny::reactive({
      shiny::req(trait_names())
      
      subset_trait_names(traitData, trait_names())
    })
    
    tableOutput <- shinyTraitTable("shinyTable", module_par, trait_names,
                                   traitDataInput, traitSignal)
    
    solosOutput <- shinyTraitSolos("shinySolos", module_par, tableOutput)
    pairsOutput <- shinyTraitPairs("shinyPairs", module_par, trait_names,
                                   tableOutput)
    
    # Trait Names.
    trait_names <- shiny::reactive({
      shiny::req(corTableOutput())
      
      c(unite_datatraits(corTableOutput(), key = TRUE),
        rel_traitsOutput())
      },
      label = "trait_names")
    
    # Related Datasets.
    datasets <- shiny::reactive({
      shiny::req(traitStats())
      unique(traitStats()$dataset)
    })
    output$reldataset <- renderUI({
      shiny::selectInput(ns("reldataset"), "Related Datasets:",
                         datasets(), datasets()[1], multiple = TRUE)
    })
    shiny::observeEvent(
      datasets(),
      {
        selected <- datasets()[1]
        choices <- datasets()
        selected <- selected[selected %in% choices]
        if(!length(selected))
          selected <- choices[1]
        shiny::updateSelectInput(session, "reldataset", choices = choices,
                                 selected = selected)
      })
    
    # Tables
    output$table_choice <- shiny::renderUI({
      choices <- c("Means","Stats")
      if(is_bestcor(corTableOutput()))
        choices <- c(choices, "Relations")
      shiny::checkboxGroupInput(ns("tables"), "Tables:",
                                choices, choices, inline = TRUE)
    })
    output$tables <- shiny::renderUI({
      shiny::req(input$tables)
      
      shiny::tagList(
        if("Means" %in% input$tables)
          shinyTraitTableOutput(ns("shinyTable")),
        if("Relations" %in% input$tables)
          shinyCorTableOutput(ns("shinyCorTable")),
        if("Stats" %in% input$tables)
          shinyTraitOrderUI (ns("shinyOrder"))
      )
    })
    
    # Plot
    output$plot_choice <- shiny::renderUI({
      choices <- "Traits"
      if(length(shiny::req(trait_names())) > 1)
        choices <- c(choices, "Pairs")
      if(is_bestcor(corTableOutput()))
        choices <- c(choices, "Relations")
      shiny::checkboxGroupInput(ns("plots"), "Plots:",
                                choices, choices, inline = TRUE)
    })
    output$plots <- shiny::renderUI({
      shiny::req(input$plots)
      
      shiny::tagList(
        if("Traits" %in% input$plots)
          shinyTraitSolosUI(ns("shinySolos")),
        if("Pairs" %in% input$plots)
          shinyTraitPairsUI(ns("shinyPairs")),
        if("Relations" %in% input$plots)
          shinyCorPlotOutput(ns("shinyCorPlot")))
    })

    ##########################################################
    
    shiny::reactive({
      shiny::req(trait_names(), solosOutput(), tableOutput())
      list(
        solos = solosOutput(),
        cors = corTableOutput(),
        pairs = pairsOutput(),
        table = tableOutput(),
        traits = trait_names()
      )
    })
  })
}
