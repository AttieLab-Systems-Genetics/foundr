#' Shiny Module Input for Trait Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @importFrom shiny column fluidRow NS uiOutput
#' @export
#'
shinyContrastPanelInput <- function(id) {
  ns <- shiny::NS(id)

  shinyTraitOrderInput(ns("shinyOrder"))
}

#' Shiny Module Output for Contrast Panel
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyContrastPanel
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyContrastPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::radioButtons(ns("butshow"), "", c("Plots","Tables"), "Plots",
                            inline = TRUE)),
      shiny::column(
        2,
        shiny::uiOutput(ns("downloads"))),
      shiny::column(
        6,
        shiny::uiOutput(ns("filename")))),
    shiny::uiOutput(ns("traitOutput"))
  )
}

#' Shiny Module Server for Contrast Panel
#'
#' @param input,output,session standard shiny arguments
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column downloadHandler h3 moduleServer observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
shinyContrastPanel <- function(id, main_par,
                            traitSignal, traitStats,
                            customSettings = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyContrastPanel inputs
    #   main_par$tabpanel
    #
    # RETURNS
    #   
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- shinyTraitOrder("shinyOrder", main_par,
                                   traitStats, traitSignal,
                                   customSettings)

    # Output
    output$traitOutput <- shiny::renderUI({
      switch(shiny::req(input$butshow),
             Plots  = shinyTraitOrderOutput(ns("shinyOrder")),
             Tables = shinyTraitOrderUI(ns("shinyOrder")))
    })

    # DOWNLOADS
    output$downloads <- shiny::renderUI({
      shiny::req(input$butshow)
      
      shiny::downloadButton(ns(paste0("download", input$butshow)),
                            input$butshow)
    })
    # Download File Prefix
    output$filename <- renderUI({
      filename <- "Contrasts_"
      datasets <- paste(unique(orderOutput()$dataset), collapse = ",")
      filename <- paste0(filename, datasets)
      
      shiny::textAreaInput(ns("filename"), "File Prefix:", filename)
    })
    
    # Download Plot
    output$downloadPlots <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".pdf")
      },
      content = function(file) {
        shiny::req(orderOutput())
        grDevices::pdf(file, width = 9, height = main_par$height)
        print(orderOutput())
        grDevices::dev.off()
      })
    
    # Download DataTable
    output$downloadTables <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".csv")
      },
      content = function(file) {
        shiny::req(orderOutput())
        utils::write.csv(
          orderOutput(),
          file, row.names = FALSE)
      })
    
    ###############################################################
    orderOutput
  })
}
