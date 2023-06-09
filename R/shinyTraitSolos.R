#' Shiny Module UI for traitSolos
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTraitSolos
#' @export
#'
shinyTraitSoloUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("shiny_traitSolos"))
  )
}

#' Shiny Module Server for trait solos Plots
#'
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments from `foundrServer`
#' @param datasets_selected traitSignalInput reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyTaitSolosUI`
#' @importFrom shiny reactiveVal req renderUI tagList uiOutput renderPlot reactive
#'             tagList renderUI column conditionalPanel fluidRow plotOutput radioButtons
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom dplyr filter
#' @export
#'

shinyTraitSolo <- function(input, output, session,
                         main_par,
                         datasets_selected,traitSignalInput) {
  ns <- session$ns


  # INPUTS
  # Main inputs:
  #   main_par$trait
  #   main_par$strains
  #   main_par$facet
  #   main_par$height
  #   main_par$upload
  #   main_par$upload$datapath
  # TraitSolo inputs:
  #   input$butresp
  #   input$dataset


  # OUTPUTS
  # output$distPlot
  # output$plots
  # output$datatable


  # RETURNS
  # list with
  #   distPlot()
  #   datatable()


  #############################################################
  # Output: Plots or Data
  output$shiny_traitSolos <- shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
         shiny::column(
           6,
           shiny::radioButtons(ns("buttrait"), "Single Plots",
                               c("Trait Plots","Pair Plots"),
                               "Trait Plots", inline = TRUE)),
        shiny::column(
          6,
          shiny::radioButtons(ns("butresp"), "Response",
                              c("value", "cellmean", "signal"),
                              "value", inline = TRUE))),
      shiny::conditionalPanel(
        condition = "input.buttrait == 'Trait Plots'",
        shiny::plotOutput(ns("plots"))),
      shiny::conditionalPanel(
        condition = "input.buttrait == 'Pair Plots'",
        shiny::plotOutput(ns("scatPlot"))),
      DT::dataTableOutput(ns("datatable"))
    )
  })

  # Trait Data from selected datasets
  traitDataSelectType <- shiny::reactive({
    shiny::req(datasets_selected())
    out <- dplyr::filter(
      traitDataInput(),
      .data$dataset %in% datasets_selected())
    if("condition" %in% names(out)) {
      if(all(is.na(out$condition)))
        out$condition <- NULL
    }
    out
  })

  # SELECTING SUBSETS OF INPUT DATA by dataset
  # Select Data Types
  datasets <- shiny::reactive({
    shiny::req(traitDataInput())
    rename_datasets(
      traitDataInput(),
      customSettings$dataset,
      FALSE)
  })
  datasets_selected <- shiny::reactive({
    rename_datasets(
      shiny::req(input$dataset),
      customSettings$dataset,
      TRUE)
  })

  # New component for Modules. Placeholder for now.
  traitModulesParam <- shiny::reactive({
    if(inherits(traitdata, "traitObject")) {
      traitdata$Modules
    } else {
      NULL
    }
  })

  # Trait Data: <dataset>, trait, strain, sex, <condition>, value
  newtraitdata <- NULL
  #  newtraitdata <- shiny::reactive({
  #   if(shiny::isTruthy(main_par$upload)) {
  #     newTraitData(main_par$upload$datapath,
  #                  customSettings$condition,
  #                  customSettings$dataset["uploaded"])
  #   } else {
  #     NULL
  #   }
  # })

  traitDataInput <- shiny::reactive({
   bindNewTraitData(newtraitdata(), traitDataParam())
  })

  # Trait Signal from selected datasets
  traitSignalSelectType <- shiny::reactive({
    shiny::req(datasets_selected())
    out <- dplyr::filter(
      traitSignalInput(),
      .data$dataset %in% datasets_selected())


    if("condition" %in% names(out)) {
      if(all(is.na(out$condition)))
        out$condition <- NULL
    }
    out
  })

  trait_selection <- shiny::reactiveVal(NULL)

  traitDataParam <- shiny::reactive({
    if(inherits(traitdata, "traitObject")) {
      traitdata$Data
    } else {
      traitdata
    }
  })

  # now store your current selection in the reactive value
  shiny::observeEvent(main_par$trait, {
    trait_selection(main_par$trait)
  })

  # Trait Data for Selected Traits
  traitDataSelectTrait <- shiny::reactive({
    traitSolos(shiny::req(traitDataSelectType()),
               shiny::req(traitSignalSelectType()),
               shiny::req(trait_selection()),
               shiny::req(input$butresp),
               shiny::req(main_par$strains))
  })
  # browser()
  # Plots
  distPlot <- shiny::reactive({
    shiny::req(traitDataSelectTrait())
    ggplot_traitSolos(
      traitDataSelectTrait(),
      facet_strain = main_par$facet,
      boxplot = TRUE)
  })
  output$distPlot <- shiny::renderPlot({
    print(distPlot())
  })
  output$plots <- shiny::renderUI({
    shiny::req(main_par$height)
    shiny::plotOutput(ns("distPlot"), height = paste0(main_par$height, "in"))
  })

  # Data Table
  datameans <- shiny::reactive({
    shiny::req(trait_selection(), main_par$strains)
    response <- shiny::req(input$butresp)

  summary(traitDataSelectTrait(), customSettings)
  })
  output$datatable <- DT::renderDataTable(
    shiny::req(datameans()),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))

  #############################################################


  # List returned
  reactive({
    shiny::req(distPlot(), datatable(), datasets())
    list(
      plot = print(distPlot()),
      table = datatable(),
      traits = datasets())
  })

  }
