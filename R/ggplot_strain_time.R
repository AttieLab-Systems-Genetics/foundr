#' GGplot of Strains over Time
#'
#' @param traitData data frame
#' @param traitSignal data frame of summary signals
#' @param traits trait name(s) (typically one)
#' @param response type of response
#' @param timecol column to use for time
#' @param ... additional parameters
#'
#' @return ggplot object
#' @export
#' @importFrom dplyr filter rename select
#' @importFrom rlang .data
#'
#' @examples
ggplot_strain_time <- function(traitData, traitSignal, traits,
                               response = c("value","cellmean","signal"),
                               timecol = "week",
                               ...) {
  response <- match.arg(response)
  
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
      dplyr::filter(
        object,
        .data$trait %in% traits),
      time = timecol)
  
  ggplot_time(object, ...)
}
