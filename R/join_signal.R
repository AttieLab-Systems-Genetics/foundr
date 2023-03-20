#' Join trait data and signal
#'
#' @param traitData data frame with harmonized data
#' @param traitSignal data frame with `signal` and `cellmean`
#' @param response name of response to create through joining
#'
#' @return data frame with `value` as individual signal, plus `signal` column
#' @export
#' @importFrom dplyr left_join mutate select
#'
join_signal <- function(traitData, traitSignal, response = c("ind_signal", "rest", "noise")) {
  response <- match.arg(response)
  
  if("condition" %in% names(traitData)) {
    bys <- c("strain","sex","condition","trait")
  } else {
    bys <- c("strain","sex","trait")
  }
  if("dataset" %in% names(traitData))
    bys <- c("dataset", bys)
  
  out <-
    dplyr::left_join(
      traitData,
      traitSignal,
      by = bys)
  
  switch(response,
         ind_signal = {
           dplyr::select(
             dplyr::mutate(
               out,
               value = value - cellmean + signal),
             -cellmean)
         },
         rest = {
           dplyr::select(
             dplyr::mutate(
               out,
               value = value - signal),
             -cellmean, -signal)
         },
         noise = {
           dplyr::select(
             dplyr::mutate(
               out,
               value = value - cellmean),
             -cellmean, -signal)
         })
}
