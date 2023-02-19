progress <- function(traitData, functionname, messagename = "Stats") {
  shiny::withProgress(
    message = paste(messagename, 'calculations in progress'),
    detail = 'This may take a while...',
    value = 0.5,
    { 
      newtraitstats <- functionname(traitData)
      shiny::setProgress(
        message = "Done",
        value = 1)
    })
  newtraitstats
}
