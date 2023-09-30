#' Traits over Time
#'
#' @param object data frame with trait data or trait stats from `strainstats`
#' @param traitSignal data from with trait signals
#' @param traitnames names of `dataset: trait`
#' @param response character string for type of response
#' @param timecol column to use for time
#' @param ... additional parameters ignored
#'
#' @return object of class `traitTimes`
#' @export
#' @importFrom dplyr rename select
#' @importFrom rlang .data
#'
traitTimes <- function(traitData, traitSignal, traitStats, ...) {
  
  if(is.null(traitData) || is.null(traitSignal) || is.null(traitStats))
    return(NULL)
  
  list(
    traits = strain_time(traitData, traitSignal, ...),
    stats  = stats_time(traitStats, response = "p.value", models = "terms",
                        ...))
}
#' Strains over Time
#' 
#' @param traitData data frame with trait data
#' @param traitSignal data from with trait signals
#' @param traitnames names of `dataset: trait` without `timecol` information
#' @param timecol column to use for time
#' @param response character string for type of response
#' @param strains names of strains to subset
#' @param ... additional parameters ignored
#'
#' @importFrom dplyr rename select
#' @importFrom rlang .data
#'

strain_time <- function(traitData,
                        traitSignal,
                        traitnames = timetraits(traitSignal, timecol)[1],
                        timecol = c("week", "minute","minute_summary","week_summary"),
                        response = c("value","cellmean","signal"),
                        strains = names(foundr::CCcolors),
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
  
  # Rename timecol to `time`. 
  timecol <- match.arg(timecol)
  
  # Filter object based on traitnames and strains.
  object <- 
    dplyr::filter(
      separate_time(object, traitnames, timecol),
      .data$strain %in% strains)
  
  if(!nrow(object))
    return(NULL)
  
  # Object has column labeled `timecol`.
  
  # Unite `dataset: trait` as datatraits ordered by `traitnames`
  object <- 
    dplyr::mutate(
      tidyr::unite(
        object,
        datatraits,
        dataset, trait,
        sep = ": "),
      datatraits = factor(datatraits, traitnames))
  
  # Split object based on `traitnames`.
  object <-
    split(
      object,
      object$datatraits)
  
  out <- template_time(object, traitnames, timecol, response)
  attr(out, "timetype") <- "strain"
  out
}
template_time <- function(object,
                          traitnames = names(object),
                          timecol = c("week", "minute","week_summary","minute_summary"),
                          response = "value",
                          ...) {
  
  # Rename `value` by names of object. Add attributes.
  object <- 
    purrr::map(
      purrr::set_names(names(object)),
      function(x, object) {
        object <- object[[x]]
        names(object)[match("value", names(object))] <- x
        
        attr(object, "pair") <- c("time", x)
        if(length(unique(object[[timecol]])) < 3)
          smooth_method <- "lm"
        else {
          if(timecol %in% c("week_summary","minute_summary"))
            smooth_method <- "line"
          else
            smooth_method <- "loess"
        }
        
        attr(object, "smooth_method") <- smooth_method
        
        object
      }, object)
  
  class(object) <- c("traitTimes", class(object))
  attr(object, "traitnames") <- traitnames
  attr(object, "response") <- response
  attr(object, "time") <- timecol
  
  object
}
#' GGplot of Strains over Time
#'
#' @param object,objectSum object of class `strain_time`
#' @param ... additional parameters
#' @param drop_xlab drop xlab for all but last plot if `TRUE`
#' @param legend_position position of legend ("none" for none)
#'
#' @return ggplot object
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @rdname traitTimes
#'
ggplot_traitTimes <- function(object,
                              objectSum = NULL,
                              ...,
                              drop_xlab = TRUE,
                              facet_strain = (timetype != "strain"),
                              legend_position = "bottom") {
  
  if(is.null(object) || !nrow(object[[1]]))
    return(plot_null("no data for strain_time plot"))
  
  if(!is.null(objectSum))
    return(ggplot_traitTimes_twoplot(
      object,
      objectSum,
      ...,
      drop_xlab = drop_xlab,
      facet_strain = facet_strain,
      legend_position = legend_position))
  
  response <- attr(object, "response")
  
  timetype <- attr(object, "timetype")
  if(is.null(timetype))
    timetype <- "strain"
  # Force facet_strain to be TRUE if stats. Kludge.
  if(timetype == "stats")
    facet_strain <- TRUE
  
  timecol <- attr(object, "time")
  
  # Rename timecol to `time`.
  object <- 
    purrr::map(
      object,
      function(x) {
        out <- dplyr::rename(x, time = timecol)
        attr(out, "response") <- response
        attr(out, "timetype") <- timetype
        out
      })
  
  if(timecol == "minute") {
    facet_time <- "week"
  } else
    facet_time <- NULL
  
  ggplot_template(
    object,
    line_strain = TRUE,
    parallel_lines = FALSE &
      (timecol %in% c("week_summary","minute_summary")),
    xlab = timecol,
    facet_time = facet_time,
    facet_strain = facet_strain,
    drop_xlab = drop_xlab,
    legend_position = legend_position,
    ...)
}
#' @export
#' @rdname traitTimes
#' @method autoplot traitTimes
autoplot.traitTimes <- function(object, ...) {
  ggplot_traitTimes(object, ...)
}
#' @export
#' @rdname traitTimes
#' @method plot traitTimes
plot.traitTimes <- function(x, ...) {
  autoplot.traitTimes(x, ...)
}

ggplot_traitTimes_twoplot <- function(
  object,
  objectSum,
  ...,
  drop_xlab = TRUE,
  facet_strain = (timetype != "strain"),
  legend_position = "bottom") {
  
  if(!inherits(objectSum, "traitTimes"))
    return(plot_null("Second argument should be traitTimes object"))

  p1 <- ggplot_traitTimes(
    object,
    ...,
    drop_xlab = drop_xlab,
    facet_strain = facet_strain,
    legend_position = legend_position,
    legend_nrow = 1)
  
  p2 <- ggplot_traitTimes(
    objectSum,
    ...,
    drop_xlab = drop_xlab,
    facet_strain = TRUE,
    legend_position = legend_position)
  
  cowplot::plot_grid(p1,p2, nrow = 2)
}
