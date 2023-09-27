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
  
  shiny::uiOutput(ns("shiny_output"))
}

#' Shiny Module Server for Volcano Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `server()`
#' @param traitStats static data frame
#' @param customSettings list of custom settings
#' @param facet facet on `strain` if `TRUE`
#'
#' @return reactive object for `shinyVolcanoUI`
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
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(ns("term"), "Volcano term:", term_stats())),
          shiny::column(
            4,
            shiny::checkboxInput(ns("traitnames"), "Trait names:", TRUE)),
          shiny::column(
            4,
            shiny::checkboxInput(ns("interact"), "Interactive?", FALSE))),
        
        shiny::uiOutput(ns("volcano_inter")),
        
        # Sliders from Volcano plot display.
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::sliderInput(ns("volsd"), "SD line:",
                           0, signif(max(traitStats$SD, na.rm = TRUE), 2),
                           1, step = 0.1)),
          shiny::column(
            6,
            shiny::sliderInput(ns("volpval"), "-log10(p.value) line:",
                           0, min(10, round(-log10(min(traitStats$p.value, na.rm = TRUE)), 1)),
                           2, step = 0.5))),
        
        # Data table.
        DT::dataTableOutput(ns("tablesum"),
                            escape = FALSE,
                            options = list(scrollX = TRUE, pageLength = 10)))
    })
    
    term_stats <- shiny::reactive({
      termStats(traitStats, FALSE)
    })
    term_selection <- shiny::reactiveVal(NULL, label = "term_selection")
    shiny::observeEvent(input$term, term_selection(input$term))
    trait_selection <- shiny::reactiveVal(NULL, label = "trait_selection")
    shiny::observeEvent(input$traitnames, trait_selection(input$traitnames))
    inter_selection <- shiny::reactiveVal(NULL, label = "inter_selection")
    shiny::observeEvent(input$interact, inter_selection(input$interact))
    
    output$volcano_inter <- shiny::renderUI({
      # Condition for plot based on `interact` parameter.
      if(shiny::isTruthy(inter_selection())) {
        plotly::plotlyOutput(ns("volcanoly"),
                             height = paste0(shiny::req(main_par$height), "in"))
      } else {
        shiny::plotOutput(ns("volcanopr"),
                          height = paste0(shiny::req(main_par$height), "in"))
      }
    })
    volcano_plot <- shiny::reactive({
      shiny::req(traitStatsSelected(), term_selection(),
                 input$volsd, input$volpval)
      
      volcano(traitStatsSelected(), term_selection(),
              threshold = c(SD = input$volsd, p = 10 ^ -input$volpval),
              interact = inter_selection(),
              traitnames = trait_selection(),
              facet = facet)
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
    
    volcano_table <- shiny::reactive({
      shiny::req(traitStatsSelected(), term_selection(),
                 input$volsd, input$volpval)
      
      summary_strainstats(
        mutate_datasets(traitStatsSelected(), customSettings$dataset),
        terms = term_selection(),
        threshold = c(SD = input$volsd, p = 10 ^ (-input$volpval)))
    })
    output$tablesum <- DT::renderDataTable(
      volcano_table(),
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 10))
    
    # DOWNLOADS
    output$downloads <- renderUI({
      shiny::fluidRow(
        shiny::column(3, shiny::downloadButton(ns("downloadPlots"), "Plots")),
        shiny::column(3, shiny::downloadButton(ns("downloadTables"), "Tables")),
        shiny::column(6, shiny::uiOutput(ns("filename"))))
    })
    # Download File Prefix
    output$filename <- renderUI({
      shiny::req(term_selection(), volcano_plot(), volcano_table())
      
      filename <- paste0(
        "Volcano_", term_selection(), "_",
        paste(data_selection(), collapse = "."))
      shiny::textAreaInput(ns("filename"), "File Prefix", filename)
    })
    
    # Download Plots
    output$downloadPlots <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".pdf")
      },
      content = function(file) {
        shiny::req(volcano_plot(), main_par$height)
        grDevices::pdf(file, width = 9, height = main_par$height)
        print(volcano_plot())
        grDevices::dev.off()
      })
    
    # Download Tables
    output$downloadTables <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".csv")
      },
      content = function(file) {
        shiny::req(volcano_table())
        utils::write.csv(
          volcano_table(),
          file, row.names = FALSE)
      })
  
    #################################################
    
    input
  })
}


