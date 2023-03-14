#' Calculate individual signal
#'
#' @param traitData data frame with harmonized data
#' @param traitSignal data frame with `signal` and `cellmean`
#'
#' @return data frame with `value` as individual signal, plus `signal` column
#' @export
#' @importFrom dplyr left_join mutate select
#'
calc_ind_signal <- function(traitData, traitSignal) {
  if("condition" %in% names(traitData)) {
    bys <- c("strain","sex","condition","trait")
  } else {
    bys <- c("strain","sex","trait")
  }
  if("dataset" %in% names(traitData))
    bys <- c("dataset", bys)
  
  dplyr::select(
    dplyr::mutate(
      dplyr::left_join(
        traitData,
        traitSignal,
        by = bys),
      value = value - cellmean + signal),
    -cellmean)
}
