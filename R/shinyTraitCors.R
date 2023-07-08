#' Shiny Module UI for Trait Correlations
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitCors
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitCorsUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shiny_cors"))
}

#' Shiny Module UI for Trait Correlations
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitCors
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitCorsOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shiny_corsOutput"))
}

#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param traitSignal,traitStats reactive data frames
#' @param stats_par,module_par reactive inputs from calling modules
#'
#' @return reactive object for `shinyTraitCorsUI`
#' @importFrom shiny callModule column fluidRow moduleServer observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitCors <- function(id, stats_par, module_par, key_trait, traitSignal) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Temporary kludge
    customSettings <- shiny::reactiveValues(dataset = NULL)
    
    # INPUTS
    # calling module inputs
    #   module_par$height
    # shinyTraitCors inputs
    #   input$corterm
    #   input$abscor
    #   input$mincor
    #
    # RETURNS
    # corobject()
    
    # Shiny UI Server Side
    output$shiny_cors <- shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(ns("corterm"), "Correlation Type",
                               c("cellmean", "signal"), "cellmean")),
          shiny::column(
            6,
            shiny::checkboxInput(ns("abscor"), "Absolute Correlation?", TRUE))),
        
        shiny::sliderInput(ns("mincor"), "Minimum:", 0, 1, 0.7))
    })
    output$shiny_corsOutput <- shiny::renderUI({
      shiny::req(module_par$height)
      
      shiny::plotOutput(ns("corplot"),
                        height = paste0(module_par$height, "in"))
    })

    corplot <- shiny::reactive({
      shiny::req(input$mincor, corobject())
      
      if(is.null(corobject()) || !nrow(corobject()))
        return(plot_null("Need to specify at least one trait."))
      
      ggplot_bestcor(
        mutate_datasets(corobject(), customSettings$dataset, undo = TRUE), 
        input$mincor, input$abscor)
    })
    output$corplot <- shiny::renderPlot({
      print(corplot())
    })
    output$cortable <- DT::renderDataTable(
      {
        shiny::req(corobject(), input$mincor, input$corterm)
        summary_bestcor(
          mutate_datasets(
            corobject(),
            customSettings$dataset),
          input$mincor)
      },
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 10))
    
    corobject <- shiny::reactive({
      shiny::req(key_trait(), stats_par$reldataset, traitSignal(),
                 input$corterm)
      
      bestcor(
        # Filter to Related Datasets or matching `nameOption()`.
        dplyr::select(
          dplyr::filter(
            tidyr::unite(
              traitSignal(),
              datatraits,
              .data$dataset, .data$trait,
              sep = ": ", remove = FALSE),
            (.data$datatraits %in% key_trait()) |
              (.data$dataset %in% stats_par$reldataset)),
          -.data$datatraits),
        key_trait(),
        input$corterm)
    })
    
    ##############################################################
    # Return
    shiny::reactive({
      list(
        table = corobject(),
        plot = corplot())
      },
      label = "shinyTraitCors_return")
  })
}
