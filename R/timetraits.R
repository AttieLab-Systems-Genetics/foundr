

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
            time_codes("minute"),
            "_\\2:\\1"),
        # Remove _NNwk or :MM only at end of name.
        trait = 
          # Remove :MM$
          stringr::str_remove(
            # Remove _NNwk$
            stringr::str_remove(
              .data$trait,
              time_codes("week")),
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
time_codes <- function(timeunit = c("week", "minute",
                                    "week_summary", "minute_summary",
                                    "userAUC")) {
  timeunit <- match.arg(timeunit)
  
  switch(
    timeunit,
    "week" = {"_[0-9]+wk$"},
    "minute" = {"_([0-9]+)_([0-9]+wk)$"},
    "week_summary" = {"_(Ave|Vmax|Tmax|Slope)$"},
    "minute_summary" = {"_(AUC|Emax|Tmax|HalfLife)_[0-9]+wk"},
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
