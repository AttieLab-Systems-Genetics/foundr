#' GGplot of Strains over Time
#'
#' @param traitData data frame
#' @param traitSignal data frame of summary signals
#' @param datatraits `dataset: trait` name(s) (typically one)
#' @param timecol column to use for time
#' @param response type of response
#' @param ... additional parameters
#'
#' @return ggplot object
#' @export
#' @importFrom dplyr filter mutate rename select
#' @importFrom rlang .data
#'
ggplot_strain_time <- function(traitData, traitSignal, datatraits,
                               timecol = "week",
                               response = c("value","cellmean","signal"),
                               ...) {
  response <- match.arg(response)
  
  if(is.null(traitData) | is.null(traitSignal))
    return(plot_null("no data for time plot"))
  
  
  
  object <- switch(
    response,
    value = {
      traitData
    },
    cellmean = {
      dplyr::select(
        dplyr::rename(
          traitSignal,
          value = "cellmean"),
        -signal)
    },
    signal = {
      dplyr::select(
        dplyr::rename(
          traitSignal,
          value = "signal"),
        -cellmean)
    })
  
  object <- 
    dplyr::rename(
      unite_datatraits(
        object,
        datatraits,
        TRUE),
      time = timecol)
  
  if(!nrow(object))
    return(plot_null("no data over time"))
  
  if(timecol == "minute") {
    facet_time <- "week"
    object <-
      dplyr::mutate(
        object,
        week = paste0(week, "wk"))
  } else
    facet_time <- NULL
  
  ggplot_time(object, xlab = timecol, facet_time = facet_time, ...)
}
