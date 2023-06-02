#' Shiny Module UI for Volcano Plot
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyVolcano
#' @export
#'

shinyVolcanoUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("shiny_volcano"))
  )
}

#' Shiny Module Server for Volcano Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer`
#' @param traitStatsSelectType,traitStatsArranged reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyVolcanoUI`
#' @importFrom shiny column fluidRow observeEvent plotOutput reactive
#'             reactiveVal renderPlot renderUI req selectInput selectizeInput
#'             tagList uiOutput updateSelectizeInput sliderInput renderUI
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom plotly plotlyOutput ggplotly renderPlotly
#' @importFrom ggplot2 ylim
#' @export
#'
shinyVolcano <- function(input, output, session,
                       main_par,
                       traitStatsSelectType, traitStatsArranged) {
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

  output$shiny_volcano <- shiny::renderUI({
    trstats <- shiny::req(traitStatsSelectType())
    shiny::tagList(
      # Get new input parameters for Volcano.
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectInput(ns("term"), "Volcano term:", termstats())),
        shiny::column(
          4,
          shiny::checkboxInput(ns("traitnames"), "Trait names:", TRUE)),
        shiny::column(
          4,
          shiny::checkboxInput(ns("interact"), "Interactive?", FALSE))),

      # Condition for plot based on `interact` parameter.
      shiny::uiOutput(ns("condinteract")),

      # Sliders from Volcano plot display.
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::sliderInput(
            ns("volsd"), "SD line:",
            0, signif(max(trstats$SD, na.rm = TRUE), 2),
            1, step = 0.1)),
        shiny::column(
          6,
          shiny::sliderInput(
            ns("volpval"), "-log10(p.value) line:",
            0, min(10,
                   round(-log10(min(trstats$p.value, na.rm = TRUE)), 1)),
            2, step = 0.5))),

      # Data table.
      DT::dataTableOutput(ns("tablesum")))
  })

  output$condinteract <- shiny::renderUI({
    if(input$interact) {
      plotly::plotlyOutput(ns("volcanoly"))
    } else {
      shiny::plotOutput(ns("volcanopr"),
                        height = paste0(main_par$height, "in"))
    }
  })

  termstats <- shiny::reactive({
    shiny::req(traitStatsSelectType())
    termStats(traitStatsSelectType(), FALSE)
  })

  volcanoplot <- shiny::reactive({
    shiny::req(traitStatsSelectType(), input$term, input$volsd, input$volpval)
    volcano(traitStatsSelectType(), input$term,
            threshold = c(SD = input$volsd, p = 10 ^ -input$volpval),
            interact = (input$interact),
            traitnames = (input$traitnames))
  })
  output$volcanoly <- plotly::renderPlotly({
    plotly::ggplotly(
      volcanoplot() +
        #        ggplot2::xlim(shiny::req(input$volsd), NA) +
        ggplot2::ylim(shiny::req(input$volpval), NA))
  })
  output$volcanopr <- shiny::renderPlot({
    print(volcanoplot())
  })

  output$tablesum <- DT::renderDataTable(
    {
      shiny::req(traitStatsSelectType(), input$volsd, input$volpval, input$term)
      summary_strainstats(
      # do we need this?
      #  mutate_datasets(
        traitStatsSelectType(),
      #    customSettings$dataset),
        terms = input$term,
        threshold = c(SD = input$volsd, p = 10 ^ (-input$volpval)))
    },
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))

  datasets <- shiny::reactive({
    unique(traitStatsArranged()$dataset)
  })

  # List returned
  reactive({
    shiny::req(volcanoplot(), traitStatsArranged(), datasets())
    list(
      plot = print(volcanoplot()),
      table = traitStatsArranged(),
      traits = datasets())
  })
}


