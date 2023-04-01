#' Separate Time Traits 
#'
#' @param object data frame
#' @param datatraits data frame with dataset and trait names
#' @param timeunit name of time unit
#'
#' @return data frame
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom rlang .data
#' @importFrom tidyr separate_wider_delim
#' @importFrom stringr str_replace
#'
separate_time <- function(object, datatraits, timeunit = c("week", "minute")) {
  timeunit <- match.arg(timeunit)
  
  if(!nrow(datatraits))
    return(NULL)
  
  switch(
    timeunit,
    minute = separate_minute(object, datatraits),
    week   = separate_week(object, datatraits))
}
separate_minute <- function(object, datatraits) {
  
  dplyr::mutate(
    tidyr::separate_wider_delim(
      dplyr::mutate(
        foundr::unite_datatraits(
          object,
          foundr::unite_datatraits(
            datatraits,
            filters = list(timetrait = "minute")),
          undo = TRUE),
        trait = stringr::str_replace(
          .data$trait,
          "_([0-9]+)_([0-9]+)wk$",
          ":\\1:\\2")),
      trait,
      delim = ":",
      names = c("trait", "minute", "week")),
    minute = as.numeric(.data$minute),
    week = as.numeric(.data$week))
}
separate_week <- function(object, datatraits) {
  dplyr::mutate(
    tidyr::separate_wider_delim(
      dplyr::mutate(
        foundr::unite_datatraits(
          object,
          foundr::unite_datatraits(
            datatraits,
            filters = list(timetrait = "week")),
          undo = TRUE),
        trait = str_replace(.data$trait, "_([0-9]+)wk$", ":\\1")),
      trait,
      delim = ":",
      names = c("trait", "week")),
    week = as.numeric(.data$week))
}

