#' Shiny Module UI for Trait Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitPanel
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitPanelUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyTraitStatsInput(ns("shinyStats")),
    shinyTraitTableUI(ns("shinyTable")),
    
    shiny::uiOutput(ns("plot_choice"))
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
    shinyTraitStatsUI(ns("shinyStats")),
    shiny::uiOutput(ns("plots")),
    shinyTraitTableOutput(ns("shinyTable"))
  )
}

#' Shiny Module Server for Trait Panel
#'
#' @param input,output,session standard shiny arguments
#' @param traitData,traitSignal,traitStats reactive data frames
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
    
    # ** Need to sort out inputs: what is in this module and what is above? **
    # MODULES
    statsOutput <- shinyTraitStats("shinyStats", module_par, traitSignal,
                                   traitStats)
    tableOutput <- shinyTraitTable("shinyTable", module_par, statsOutput,
                                   traitData, traitSignal)
    
    solosOutput <- shinyTraitSolos("shinySolos", module_par, tableOutput)
    pairsOutput <- shinyTraitPairs("shinyPairs", module_par, statsOutput,
                                   tableOutput)
    
    # Trait Names from shinyTraitStats()
    trait_names <- shiny::reactive({
      shiny::req(statsOutput())
      c(statsOutput()$key_trait, statsOutput()$rel_traits)
    },
    label = "trait_names")

    # Plot
    output$plot_choice <- shiny::renderUI({
      shiny::req(statsOutput())
      
      choices <- "Solos"
      if(!is.null(statsOutput()$corsplot))
        choices <- c(choices, "Cors")
      if(length(shiny::req(trait_names())) > 1)
        choices <- c(choices, "Pairs")
      shiny::checkboxGroupInput(ns("plots"), "Plots:",
                                choices, choices, inline = TRUE)
    })
    
    output$plots <- shiny::renderUI({
      shiny::req(input$plots)
      
      shiny::tagList(
        if("Solos" %in% input$plots)
          shinyTraitSolosUI(ns("shinySolos")),
        if("Pairs" %in% input$plots)
          shinyTraitPairsUI(ns("shinyPairs")),
        if("Cors" %in% input$plots)
          shinyTraitStatsOutput(ns("shinyStats")))
    })
    

    ##########################################################
    
    shiny::reactive({
      shiny::req(trait_names(), solosOutput(), tableOutput())
      list(
        solos = solosOutput(),
        cors = statsOutput()$corsplot,
        pairs = pairsOutput(),
        table = tableOutput(),
        traits = trait_names()
      )
    })
  })
}
