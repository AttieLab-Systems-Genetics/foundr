#' Strains over Time
#'
#' @param traitData data frame with trait data
#' @param traitSignal data from with trait signals
#' @param traitnames names of `dataset: trait`
#' @param response character string for type of response
#' @param timecol column to use for time
#' @param ... additional parameters ignored
#'
#' @return object of class `strain_time`
#' @export
#' @importFrom dplyr rename select
#' @importFrom rlang .data
#'
strain_time <- function(traitData,
                        traitSignal,
                        traitnames = timetraits(traitSignal, timecol)[1],
                        response = c("value","cellmean","signal"),
                        timecol = c("week", "minute"),
                        ...) {
  response <- match.arg(response)
  
  if(is.null(traitData) | is.null(traitSignal))
    return(NULL)

  # Select object based on `response`.
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
        -.data$cellmean)
    })
  
  # Rename timecol to `time`. Add "wk" to `week column if it is "minute".
  timecol <- match.arg(timecol)
  
  # Filter object based on traitnames.
  object <- separate_time(object, traitnames, timecol)
  
  if(!nrow(object))
    return(NULL)
  
  object <- 
    dplyr::mutate(
      tidyr::unite(
        object,
        datatraits,
        dataset, trait,
        sep = ": "),
      datatraits = factor(datatraits, traitnames))
  
  object <-
    split(
      object,
      object$datatraits)
  object <- 
    purrr::map(
      purrr::set_names(names(object)),
      function(x, object) {
        object <- object[[x]]
        names(object)[match("value", names(object))] <- x
        
        attr(object, "pair") <- c("time", x)
        if(length(unique(object[[timecol]])) < 3)
          smooth_method <- "lm"
        else
          smooth_method <- "loess"
        
        attr(object, "smooth_method") <- smooth_method
        
        object
      }, object)

  class(object) <- c("strain_time", class(object))
  attr(object, "traitnames") <- traitnames
  attr(object, "response") <- response
  attr(object, "time") <- timecol
  
  object
}
#' GGplot of Strains over Time
#'
#' @param object object of class `strain_time`
#' @param ... additional parameters
#'
#' @return ggplot object
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
ggplot_strain_time <- function(object,
                               ...) {
  if(is.null(object) || !nrow(object[[1]]))
    return(plot_null("no data for strain_time plot"))
  
  timecol <- attr(object, "time")
  
  # Rename timecol to `time`.
  object <- 
    purrr::map(
      object,
      function(x) dplyr::rename(x, time = timecol))
  
  if(timecol == "minute") {
    facet_time <- "week"
  } else
    facet_time <- NULL
  
  ggplot_time(object, xlab = timecol, facet_time = facet_time, ...)
}

#' @export
#' @importFrom dplyr arrange count desc distinct filter mutate select
#' @importFrom rlang .data
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
#' @export
#' @importFrom dplyr arrange count desc distinct filter mutate select
#' @importFrom rlang .data
timetraits <- function(traitSignal, timecol = c("week","minute")) {
  timecol <- match.arg(timecol)
  
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

  # Get traitnames without time information.
  traitnames <- 
    unite_datatraits(
      dplyr::mutate(
        datatraits,
        trait = 
          stringr::str_remove(
            stringr::str_remove(
              .data$trait,
              "_[0-9]+wk$"),
            "_[0-9]+$")))

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
            # Separate timecol as column(s) for datatraits.
            # minute and week for timecol = "minute"
            # week for timecol = "week"
            separate_time(
              datatraits,
              traitnames,
              timecol),
            .data$dataset, .data$trait, .data[[timecol]]),
          .data$dataset, .data$trait),
        n > 1),
      dplyr::desc(.data$n)))
}

