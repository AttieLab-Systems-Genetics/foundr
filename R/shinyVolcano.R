#' Shiny Module Output for Volcano Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyVolcano
#' @export
#' @importFrom shiny NS uiOutput
#'
shinyVolcanoOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4, shiny::radioButtons(ns("butshow"),
                                           "", c("Plots","Tables"), "Plots", inline = TRUE)),
      shiny::column(8, shinyDownloadsOutput(ns("downloads")))),
    
    shinyContrastPlotInput(ns("shinyContrastPlot")),
    shiny::fluidRow(
      shiny::column(3, shiny::numericInput(ns("ntrait"), "Traits:",
                                           20, 5, 100, 5)),
      shiny::column(9, shinyContrastPlotUI(ns("shinyContrastPlot")))),
    
    shiny::uiOutput(ns("shiny_output"))
  )
}

#' Shiny Module Server for Volcano Plots
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments from `server()`
#' @param traitStats static data frame
#' @param customSettings list of custom settings
#' @param facet facet on `strain` if `TRUE`
#'
#' @return reactive object for `shinyVolcanoOutput`
#' @importFrom shiny column fluidRow moduleServer observeEvent plotOutput
#'             reactive renderPlot renderUI req selectInput selectizeInput
#'             tagList uiOutput updateSelectInput sliderInput renderUI
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom plotly plotlyOutput ggplotly renderPlotly
#' @importFrom ggplot2 ylim
#' @importFrom rlang .data
#' @export
#'
shinyVolcano <- function(id, main_par, traitStats, customSettings = NULL,
                         facet = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # *** allow to flip from SD to p.value
    # *** fix all the kludgey stuff--may need to refactor
    # *** shinyContrastPlot: add scatterplot
    # *** rename as contrast now has generic contrast/stat
    # *** refactor data so Charles can use
    
    # INPUTS
    # Main inputs:
    #   main_par$height
    #   main_par$dataset
    # Volcano inputs: (see output$tab_volcano below)
    #   input$term
    #   input$traitnames
    #   input$interact
    #   input$volsd
    #   input$volpval
    
    # MODULES
    shinyDownloads("downloads", "Volcano", input, postfix,
                   plotObject, tableObject)
    # Contrast Trait Plots
    shinyContrastPlot("shinyContrastPlot",
                      input, input, main_par,
                      traitStatsSelected, customSettings, 
                      shiny::reactive("Sex Contrasts"))
    
    # Server-side UIs
    output$shiny_input <- shiny::renderUI({
      # Get new input parameters for Volcano.
      shiny::selectInput(ns("dataset"), "Datasets:",
                         datasets(), datasets()[1], multiple = TRUE)
    })
    
    # Dataset selection.
    datasets <- shiny::reactive({
      unique(traitStats$dataset)
    })
    data_selection <- shiny::reactiveVal(NULL, label = "data_selection")
    shiny::observeEvent(main_par$dataset,
                        data_selection(main_par$dataset))
    shiny::observeEvent(
      !shiny::isTruthy(main_par$tabpanel) | main_par$tabpanel == "Volcano",
      {
        selected <- data_selection()
        shiny::updateSelectInput(session, "dataset", selected = selected)
      },
      label = "update_dataset")
    
    
    # Stats for selected datasets.
    traitStatsSelected <- shiny::reactive({
      shiny::req(data_selection())
      
      dplyr::filter(traitStats, .data$dataset %in% data_selection())
    })
    
    output$shiny_output <- shiny::renderUI({
      
      shiny::tagList(
        shiny::uiOutput(ns("downloads")),
        shinyContrastPlotOutput(ns("shinyContrastPlot")))
    })
    
    term_stats <- shiny::reactive({
      termStats(traitStats, FALSE, customSettings$condition_name)
    })
    term_selection <- shiny::reactiveVal(NULL, label = "term_selection")
    shiny::observeEvent(input$term, term_selection(input$term))
    trait_selection <- shiny::reactiveVal(NULL, label = "trait_selection")
    shiny::observeEvent(input$traitnames, trait_selection(input$traitnames))
    inter_selection <- shiny::reactiveVal(NULL, label = "inter_selection")
    shiny::observeEvent(input$interact, inter_selection(input$interact))
    
    output$volcano_inter <- shiny::renderUI({
      shiny::tagList(
        # Condition for plot based on `interact` parameter.
        if(shiny::isTruthy(inter_selection())) {
          plotly::plotlyOutput(ns("volcanoly"),
                               height = paste0(shiny::req(main_par$height), "in"))
        } else {
          shiny::plotOutput(ns("volcanopr"),
                            height = paste0(shiny::req(main_par$height), "in"))
        },
        # *** Add terms selectInput about here.
        shiny::renderPlot({
          biplot_evidence(traitStatsSelected(), "p.value", "blah")
        })
      )
    })
    volcano_plot <- shiny::reactive({
      shiny::req(traitStatsSelected(), term_selection(),
                 input$volsd, input$volpval)
      
      volcano(traitStatsSelected(), term_selection(),
              threshold = c(SD = input$volsd, p = 10 ^ -input$volpval),
              interact = inter_selection(),
              traitnames = trait_selection(),
              facet = facet,
              condition_name = customSettings$condition_name)
    })
    output$volcanoly <- plotly::renderPlotly({
      plotly::ggplotly(
        volcano_plot() +
          #        ggplot2::xlim(shiny::req(input$volsd), NA) +
          ggplot2::ylim(shiny::req(input$volpval), NA))
    })
    output$volcanopr <- shiny::renderPlot({
      print(volcano_plot())
    })
    
    tableObject <- shiny::reactive({
      shiny::req(traitStatsSelected(), term_selection(),
                 input$volsd, input$volpval)
      
      summary_strainstats(
        mutate_datasets(traitStatsSelected(), customSettings$dataset),
        terms = term_selection(),
        threshold = c(SD = input$volsd, p = 10 ^ (-input$volpval)))
    })
    output$tablesum <- DT::renderDataTable(
      tableObject(),
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 10))
    
    # DOWNLOADS
    postfix <- shiny::reactive({
      shiny::req(term_selection(), data_selection())
      
      paste0(paste(term_selection(), collapse = "."), "_",
             paste(data_selection(), collapse = "."))
    })
    plotObject <- shiny::reactive({
      print(shiny::req(volcano_plot()))
    })
  })
}
