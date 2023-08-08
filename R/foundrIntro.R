foundrIntro <- function(helppath = NULL) {
  if(!is.null(helppath) && helppath != "" && file.exists(helppath)) {
    datainfo <- shiny::includeMarkdown(helppath)
  } else {
    datainfo <- shiny::includeMarkdown(
      system.file(file.path("shinyApp", "help.md"), package='foundr'))
  }
  
  renderUI({
    tagList(
      shiny::includeMarkdown(
        system.file(file.path("shinyApp", "intro.md"), package='foundr')),
      "See GitHub:",
      shiny::a(paste("byandell/foundr",
                     paste0("(version ", utils::packageVersion("foundr"), ")")),
               href = "https://github.com/byandell/foundr"),
      shiny::br(),
      shiny::br(),
      datainfo)
    
    # Maybe eventually add this, but too confusing for now.
    #   shiny::tags$div(
    #        id = "popup",
    #        helpPopup(
    #          "Foundr Help",
    #          shiny::includeMarkdown(system.file(file.path("shinyApp", "morehelp.md"), package='foundr')),
    #          placement = "right", trigger = "click")))
  })
}
