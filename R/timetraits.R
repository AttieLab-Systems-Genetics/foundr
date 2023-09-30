

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
        timetrait = c("no", "week", "minute",
                      "week_summary", "minute_summary")[
          1 + grepl(time_codes("minute"), .data$trait) +
            grepl(time_codes("week"), .data$trait) +
            3 * grepl(time_codes("week_summary"), .data$trait) +
            3 * grepl(time_codes("minute_summary"), .data$trait)]),
      .data$timetrait != "no")
  
  if(!nrow(datatraits))
    return(NULL)
  else
    datatraits
}
#' Get time trait roots
#' 
#' @param object data frame
#' @param timecol time column
#' 
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

  # Get traitnames without time information.
  traitnames <- untime_traitnames(object)
  
  # Unite dataset, trait into `traitnames` = `dataset: trait` vector.
  unite_datatraits(
    # Deselect constructed `datatrait`.
    dplyr::select(
      # Filter to traits with at least 2 time points.
      dplyr::filter(
        # Count number of time units per dataset, trait.
        dplyr::count(
          # Make trait names as factor to keep order.
          dplyr::mutate(
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
            datatrait = paste(.data$dataset, .data$trait, sep = ": "),
            datatrait = factor(.data$datatrait, unique(.data$datatrait))),
          .data$datatrait, .data$dataset, .data$trait),
        n > 1),
      -datatrait))
}
untime_traitnames <- function(object) {
  # Get traitnames without time information.
  unite_datatraits(
    dplyr::mutate(
      object,
      # Replace _MM_NNwk by _NNwk:MM (minute)
      trait =
        stringr::str_replace(
          .data$trait,
          time_codes("minute"),
          "_\\2::"),
      # Replace _AA_NNwk by _NNwk:AA (minute_summary)
      trait =
        stringr::str_replace(
          .data$trait,
          time_codes("minute_summary"),
          "_\\2::"),
      # Remove week ending (_NNwk)
      trait = 
        stringr::str_remove(
          .data$trait,
          time_codes("week")),
      # Remove week summary ending (_AA)
      trait =
        stringr::str_remove(
          .data$trait,
          time_codes("week_summary")),
      # Remove :: (minute or minute_summary)
      trait =         
        stringr::str_remove(
          .data$trait,
          "::$")))
}
timetraits_filter <- function(object, traitTimesData) {
  
  if(is.null(object) || is.null(traitTimesData$traits))
    return(NULL)
                                
  timeunit <- attr(traitTimesData$traits, "time")
  traitnames <- names(traitTimesData$traits)
  
  # If `object` not run have `timetrait` columnt, get it now.
  if(!("timetrait" %in% names(object))) {
    object <- timetraitsall(object)
  }

  # Filter to time unit.
  object <- dplyr::filter(
    object,
    .data$timetrait == timeunit)
  
  object <- 
    dplyr::filter(
      dplyr::mutate(
        object,
        traitroot = untime_traitnames(object)),
      .data$traitroot %in% traitnames)
  
  paste(object$dataset, object$trait, sep = ": ")
}
time_codes <- function(timeunit = c("week", "minute",
                                    "week_summary", "minute_summary",
                                    "userAUC")) {
  timeunit <- match.arg(timeunit)
  
  switch(
    timeunit,
    "week" = {"_[0-9]+wk$"},
    "minute" = {"_([0-9]+)_([0-9]+wk)$"},
    "week_summary" = {"_(Ave|Vmax|Tmax|Slope)$"},
    "minute_summary" = {"_(AUC|Emax|Tmax|HalfLife)_([0-9]+wk)$"},
    "userAUC" = {"_([0-9]+|tAUC|iAUC)_[0-9]+wk$"})
}

#' @importFrom dplyr count distinct filter mutate select
#' @importFrom tidyr separate_wider_delim unite
#' @importFrom rlang .data
#' 
timetraits_summary <- function(out, timeunit = c("minute", "week")) {
  
  switch(
    timeunit,
    minute = {
      # Add area under curve traits for measurements over minutes.
      out <-
        dplyr::mutate(
          tidyr::separate_wider_delim(
            dplyr::filter(
              out,
              # Find traits with minute information.
              grepl(time_codes("minute"), .data$trait)),
            # Separate out minutes and week.
            # Kludge to catch cpep ratio trait.
            trait,
            delim = "_",
            names = c("cpep1", "cpep2", "gtt","trait","minute","week"),
            too_few = "align_end"),
          trait = ifelse(
            .data$trait == "ratio",
            paste(.data$cpep1, .data$cpep2, .data$gtt, .data$trait, sep = "_"),
            paste(.data$gtt, .data$trait, sep = "_")))
      
      # Filter to traits with >1 minute measurement.
      outct <- 
        dplyr::filter(
          dplyr::count(
            dplyr::distinct(
              out,
              .data$trait, .data$minute, .data$week),
            .data$trait, .data$week),
          n > 1)
      
        # Harmonize names.
        dplyr::select(
          # Unite summary name with week.
          tidyr::unite(
            # Calculate AUC and other summaries.
            area_under_curve(
              dplyr::filter(
                out,
                trait %in% outct$trait & week %in% outct$week),
              "minute"),
            trait, trait, week),
          strain, sex, animal, condition, trait, value)
    },
    week = {
      # Add area under curve traits for measurements over weeks.
      out <-
        dplyr::mutate(
          # Kludge to use AUC routine for now by calling weeks as minutes.
          tidyr::separate_wider_delim(
            dplyr::filter(
              out,
              grepl(time_codes("week"), trait) &
                !grepl(time_codes("minute"), trait) &
                !grepl(time_codes("userAUC"), trait)),
            trait,
            delim = "_",
            names = c("trait1","trait","week"),
            too_few = "align_end"),
          trait = ifelse(is.na(.data$trait1),
                         .data$trait,
                         paste(.data$trait1, .data$trait, sep = "_")),
          week = as.numeric(str_remove(.data$week, "wk$")))
      
      # Filter to traits with >1 week measurement.
      outct <- 
        dplyr::filter(
          dplyr::count(
            dplyr::distinct(
              out,
              .data$trait, .data$week),
            .data$trait),
          n > 1)
      
      # Harmonize names.
      dplyr::select(
        # Calculate AUC and other summaries.
        area_under_curve(
          dplyr::filter(
            out,
            .data$trait %in% outct$trait),
          "week"),
        strain, sex, animal, condition, trait, value)
    })
}
