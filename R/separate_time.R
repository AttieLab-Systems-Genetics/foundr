#' Separate Time Traits 
#'
#' @param object data frame
#' @param traitnames character vector with `dataset: trait` names
#' @param timeunit name of time unit
#'
#' @return data frame
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom rlang .data
#' @importFrom tidyr separate_wider_delim
#' @importFrom stringr str_detect str_replace
#'
separate_time <- function(object, traitnames, timeunit = c("week", "minute","minsum")) {
  timeunit <- match.arg(timeunit)
  
  if(is.null(object) | !length(traitnames))
    return(NULL)
  
  # str_time:  detect _MM_NNwk
  # str_colon: replace "_" by ":"
  switch(
    timeunit,
    minute = {
      str_time  <- "_([0-9]+)_([0-9]+wk)$"
      str_colon <- ":\\1:\\2"
      str_names <- c("trait", "minute", "week")
    },
    week   = {
      str_time  <- "_([0-9]+)wk$"
      str_colon <- ":\\1"
      str_names <- c("trait", "week")
    },
    minsum = {
      str_time  <- "_(AUC|Emax|Tmax|HalfLife)_([0-9]+wk)$"
      str_colon <- ":\\1:\\2"
      str_names <- c("trait", "minsum", "week")
    })
  
  object <-
    # Filter on traitnames
    unite_datatraits(
      # Separate `trait`, `minute`, `week` into columns.
      tidyr::separate_wider_delim(
        # Change delimiters before `minute` and `week` to colons.
        # Important as trait names may have "_"
        dplyr::mutate(
          dplyr::filter(
            object,
            stringr::str_detect(
              .data$trait,
              str_time)),
          trait = stringr::str_replace(
            .data$trait,
            str_time,
            str_colon)),
        trait,
        delim = ":",
        names = str_names),
      traitnames,
      undo = TRUE)
    
  switch(
    timeunit,
    minute = {
      # Minute becomes numeric. Week stays as character ending in "wk".
      dplyr::mutate(
        object,
        minute = as.numeric(.data$minute))
    },
    week   = {
      # Week becomes numeric.
      dplyr::mutate(
        object,        
        week = as.numeric(.data$week))
    },
    minsum = {
      object
    })
}
