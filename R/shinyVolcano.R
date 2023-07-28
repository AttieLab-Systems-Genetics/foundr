#' Shiny Module Input for Volcano Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyVolcano
#' @export
#' @importFrom shiny NS uiOutput
#'
shinyVolcanoInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shiny_input"))
}

#' Shiny Module UI for Volcano Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyVolcano
#' @export
#' @importFrom shiny column downloadButton fluidRow NS uiOutput
#'
shinyVolcanoUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(6, shiny::uiOutput(ns("filename"))),
    shiny::column(3, shiny::downloadButton(ns("downloadPlot"), "Plots")),
    shiny::column(3, shiny::downloadButton(ns("downloadTable"), "Data")))
}

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
shinyVolcano <- function(id, main_par, traitStats, customSettings = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # Main inputs: (see shinyapp.R)
    #   main_par$height (see shinyapp.R::foundrUI sidebarPanel)
    # Volcano inputs: (see output$tab_volcano below)
    #   input$term
    #   input$traitnames
    #   input$interact
    #   input$volsd
    #   input$volpval
    
    # Server-side UIs
    output$shiny_input <- shiny::renderUI({
      # Get new input parameters for Volcano.
      shiny::tagList(
        shiny::selectInput(ns("dataset"), "Datasets:",
                           datasets(), multiple = TRUE),
        
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
        
        # Sliders from Volcano plot display.
        shiny::sliderInput(ns("volsd"), "SD line:",
              0, signif(max(traitStats$SD, na.rm = TRUE), 2),
              1, step = 0.1),
        shiny::sliderInput(ns("volpval"), "-log10(p.value) line:",
              0, min(10, round(-log10(min(traitStats$p.value, na.rm = TRUE)), 1)),
              2, step = 0.5))
    })
    
    # Dataset selection.
    datasets <- shiny::reactive({
      unique(traitStats$dataset)
    })
    data_selection <- shiny::reactiveVal(NULL, label = "data_selection")
    shiny::observeEvent(input$dataset,
                        data_selection(input$dataset))
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
        # Condition for plot based on `interact` parameter.
        if(shiny::isTruthy(input$interact)) {
          plotly::plotlyOutput(ns("volcanoly"))
        } else {
          shiny::plotOutput(ns("volcanopr"),
                            height = paste0(main_par$height, "in"))
        },
        
        # Data table.
        DT::dataTableOutput(ns("tablesum")))
    })
    
    term_stats <- shiny::reactive({
      termStats(traitStats, FALSE)
    })
    term_selection <- shiny::reactiveVal(NULL, label = "term_selection")
    shiny::observeEvent(input$term,
                        term_selection(input$term))
    trait_selection <- shiny::reactiveVal(NULL, label = "trait_selection")
    shiny::observeEvent(input$traitnames,
                        trait_selection(input$traitnames))
    inter_selection <- shiny::reactiveVal(NULL, label = "inter_selection")
    shiny::observeEvent(input$interact,
                        inter_selection(input$interact))
    
    volcano_plot <- shiny::reactive({
      shiny::req(traitStatsSelected(), term_selection(),
                 input$volsd, input$volpval)
      
      volcano(traitStatsSelected(), term_selection(),
              threshold = c(SD = input$volsd, p = 10 ^ -input$volpval),
              interact = (inter_selection()),
              traitnames = (trait_selection()))
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
    # Download File Prefix
    output$filename <- renderUI({
      shiny::req(term_selection(), volcano_plot(), volcano_table())
      
      filename <- paste0(
        "Volcano_", term_selection(), "_",
        paste(data_selection(), collapse = "."))
      shiny::textAreaInput(ns("filename"), "File Prefix", filename)
    })
    
    # Download Plot
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".pdf")
      },
      content = function(file) {
        shiny::req(volcano_plot())
        grDevices::pdf(file, width = 9, height = 6)
        print(volcano_plot())
        grDevices::dev.off()
      })
    
    # Download DataTable
    output$downloadTable <- shiny::downloadHandler(
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


