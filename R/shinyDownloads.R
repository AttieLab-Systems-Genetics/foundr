#' Shiny Module Output for Downloads
#' @return nothing returned
#' @rdname shinyDownloads
#' @export
shinyDownloadsOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
  shiny::renderText("DownloadsOutput"),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("downloads"))),
      shiny::column(9, shiny::uiOutput(ns("filename")))))
}
#' Shiny Module Server for Downloads
#'
#' @param id identifier for shiny reactive
#' @param prefix static prefix for filename
#' @param main_par input parameters from calling routine
#' @param postfix reactive postfix for filename
#' @param plotObjec reactive plot print object
#' @param tableObject reactive table pring object
#'
#' @return nothing 
#' @importFrom shiny column downloadButton downloadHandler fluidRow
#'             moduleServer NS renderUI req textAreaInput uiOutput
#' @importFrom utils write.csv    
#' @importFrom grDevices dev.off pdf
#' @export
#'
shinyDownloads <- function(id, prefix, main_par, postfix,
                           plotObject, tableObject) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # DOWNLOADS
    output$downloads <- shiny::renderUI({
      shiny::req(main_par$butshow)
      
      shiny::downloadButton(ns(paste0("download", main_par$butshow)),
                            main_par$butshow)
    })
    # Download File Prefix
    output$filename <- renderUI({
      filename <- paste0(prefix, "_", shiny::req(postfix()))
      
      shiny::textAreaInput(ns("filename"), "File Prefix:", filename)
    })
    
    # Download Plot
    output$downloadPlots <- shiny::downloadHandler(
      filename = function() paste0(shiny::req(input$filename), ".pdf"),
      content = function(file) {
        grDevices::pdf(file, width = 9, height = main_par$height)
        shiny::req(plotObject())
        grDevices::dev.off()
      })
    
    # Download DataTable
    output$downloadTables <- shiny::downloadHandler(
      filename = function() paste0(shiny::req(input$filename), ".csv"),
      content = function(file) {
        utils::write.csv(shiny::req(tableObject()), file, row.names = FALSE)
      })
  })
}
