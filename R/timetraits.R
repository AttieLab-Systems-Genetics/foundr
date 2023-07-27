

#' @importFrom dplyr arrange count desc distinct filter mutate select
#' @importFrom rlang .data
#' @export
timetraitsall <- function(traitSignal) {
  datatraits <- 
    # Filter out traits with no time component.
    dplyr::filter(
      # New `timetrait` identifies trait as "no", "week" or "minute"
      # "week": trait name ends with `_NNwk` (`NN` = week)
      # "minute": trait name ends with `_MM_NNwk` (`MM` = minute)
      dplyr::mutate(
        # Get distinct dataset, trait (with trait including time info).
        dplyr::distinct(
          traitSignal,
          .data$dataset, .data$trait),
        timetrait = c("no", "week", "minute")[
          1 + grepl("_[0-9]+_[0-9]+wk$", .data$trait) +
            grepl("_[0-9]+wk$", .data$trait)]),
      .data$timetrait != "no")
  
  if(!nrow(datatraits))
    return(NULL)
  else
    datatraits
}
#' @importFrom dplyr arrange count desc distinct filter mutate select
#' @importFrom rlang .data
#' @export
timetraits <- function(object,
                       timecol = c("week", "minute",
                                   "week_summary", "minute_summary")) {
  
  # If `object` not run through timetraitsall to find distinct timetraits, do now.
  if(!("timetrait" %in% names(object))) {
    object <- timetraitsall(object)
  }
  
  if(is.null(object))
    return(NULL)

  timecol <- match.arg(timecol)
  if(timecol == "minute_summary")
    timecol <- "minute"
  if(timecol == "week_summary")
    timecol <- "week"
  
  # Get traitnames without time information.
  traitnames <- 
    unite_datatraits(
      dplyr::mutate(
        object,
        # Replace _MM_NNwk by _NNwk:MM
        trait =
          stringr::str_replace(
            .data$trait,
            "_([0-9]+)_([0-9]+wk)$",
            "_\\2:\\1"),
        # Remove _NNwk or :MM only at end of name.
        trait = 
          # Remove :MM$
          stringr::str_remove(
            # Remove _NNwk$
            stringr::str_remove(
              .data$trait,
              "_[0-9]+wk$"),
            ":[0-9]+$")))
  
  # Unite dataset, trait into `traitnames` = `dataset: trait` vector.
  unite_datatraits(
    # Arrange in descending order of number of time points.
    dplyr::arrange(
      # Filter to traits with at least 2 time points.
      dplyr::filter(
        # Count number of time units per dataset, trait.
        dplyr::count(
          # Get distinct dataset, trait, 
          dplyr::distinct(
            # Separate timecol as column(s) for `object`.
            # minute and week for timecol = "minute"
            # week for timecol = "week"
            separate_time(
              object,
              traitnames,
              timecol),
            .data$dataset, .data$trait, .data[[timecol]]),
          .data$dataset, .data$trait),
        n > 1),
      dplyr::desc(.data$n)))
}
timetraits_filter <- function(object,
                              timeunit = c("week", "minute",
                                           "week_summary", "minute_summary"),
                              traitnames) {
  
  if(is.null(object) || is.null(traitnames) || !length(traitnames))
    return(NULL)
  
  timeunit <- match.arg(timeunit)
  
  # If `object` not run through timetraitsall to find distinct timetraits, do now.
  if(!("timetrait" %in% names(object))) {
    object <- timetraitsall(object)
  }

  # Filter to time unit.
  object <- dplyr::filter(
    object,
    .data$timetrait == timeunit)
  
  # Remove segment of trait name corresponding to time unit.
  switch(
    timeunit,
    minute = {
      object <- dplyr::mutate(object,
        traitroot = stringr::str_replace(.data$trait, "_[0-9]*_", "_"))
    },
    week   = {
      object <- dplyr::mutate(object,
        traitroot = stringr::str_remove(.data$trait, "_[0-9]*wk$"))
    })
  
  object <- dplyr::filter(
    dplyr::mutate(
      object,
      traitroot = paste(dataset, traitroot, sep = ": ")),
    .data$traitroot %in% traitnames)
  
  paste(object$dataset, object$trait, sep = ": ")
}
