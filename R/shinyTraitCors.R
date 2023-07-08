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

#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param traitSignal,traitStats reactive data frames
#'
#' @return reactive object for `shinyTraitCorsUI`
#' @importFrom shiny callModule column fluidRow moduleServer observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyTraitCors <- function(id, traitSignal, trait_names) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyTraitCors inputs
    #   input$keydataset
    #   input$order
    #   input$reldataset
    #
    # RETURNS
    # list with elements
    #   key_trait = key_traitOutput(): Key Trait
    #   rel_traits = rel_traitsOutput(): Related Traits
    #   key_stats = summary_strainstats(): Key Dataset Stats
    #   rel_cors = summary_bestcor(): Related Dataset Correlations
    
    # *** need key_trait (or trait_names()[1]) and rel_dataset from shinyTraitStats ***
    # *** put these one level up?
    # *** need to simplify bestcor ***
    
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
            shiny::checkboxInput(ns("abscor"), "Absolute Correlation?", TRUE)
          )),
        shiny::sliderInput(ns("mincor"), "Minimum:", 0, 1, 0.7),
        
        shiny::plotOutput(ns("corplot"), height = paste0(input$height, "in")),
        DT::dataTableOutput(ns("cortable")))
    })

    corplot <- shiny::reactive({
      ggplot_bestcor(
        mutate_datasets(corobject(), customSettings$dataset, undo = TRUE), 
        input$mincor, input$abscor)
    })
    output$corplot <- shiny::renderPlot({
      shiny::req(input$mincor, corobject())
      if(is.null(corobject()) || !nrow(corobject()))
        return(print(plot_null("Need to specify at least one trait.")))
      
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
      shiny::req(trait_names(), traitSignal(), input$corterm)
      
      # *** need reldataset ***
      
      # *** This code should be simplified into bestcor(). ***
      bestcor(
        # Filter to Related Datasets or matching `nameOption()`.
        dplyr::select(
          dplyr::filter(
            tidyr::unite(
              traitSignal(),
              datatraits,
              .data$dataset, .data$trait,
              sep = ": ", remove = FALSE),
            (.data$datatraits %in% trait_names()[1]) |
              (.data$dataset %in% reldataset)),
          -.data$datatraits),
        key_traitOutput(),
        input$corterm)
    })
    
    ##############################################################
    # Return
    corobject
  })
}
