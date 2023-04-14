#' Calculate area under curve over time
#'
#' @param object data frame with `value` and other columns
#' @param timecol column containing time values
#'
#' @return object collapsed over time with summaries
#' @export
#' @importFrom dplyr arrange group_by mutate n summarize ungroup
#' @importFrom rlang .data
#' @importFrom zoo rollmean
#'
area_under_curve <- function(object, timecol = c("minutes","minute","week")) {
  timecol <- match.arg(timecol)
  
  cols <- c("strain", "sex", "condition", "animal", "week", "trait")
  cols <- cols[(cols %in% names(object)) & (cols != timecol)]
  
  object <-
    # Arrange time in ascending order.
    dplyr::arrange(
      # Group by data frame columns except value and time.
      dplyr::group_by(
        # Make sure `timecol` is numeric and assign to `time`. 
        dplyr::mutate(
          object,
          time = as.numeric(.data[[timecol]])),
        dplyr::across(dplyr::all_of(cols))),
      .data$time)
  
  # Measurements depend on time span.
  switch(
    timecol,
    # Measurements by weeks over long period.
    week = {
      object <-
        tidyr::pivot_longer(
          dplyr::ungroup(
            # Summarize over time.
            dplyr::summarize(
              object,
              # Summaries.
              Ave = mean(.data$value),
              Vmax = max(.data$value),
              Tmax = .data$time[which.max(.data$value)],
              Slope = slope(.data$time, .data$value),
              .groups = "drop")),
          Ave:Slope, names_to = "sums", values_to = "value")
    },
    # Measurements by minutes over short period.
    minutes =,
    minute = {
      object <-
        tidyr::pivot_longer(
          dplyr::ungroup(
            # Summarize over time.
            dplyr::summarize(
              object,
              # Summaries.
              AUC = sum(
                diff(.data$time) * 
                  zoo::rollmean(.data$value, 2)),
              Emax = max(.data$value),
              Tmax = .data$time[which.max(.data$value)],
              HalfLife = half(Tmax, .data$time, .data$value),
              .groups = "drop")),
          AUC:HalfLife, names_to = "sums", values_to = "value")
    })
  
  # Unite `trait` as `trait_sums` for the summaries.
  tidyr::unite(
    # Pivot longer to put summary traits in new rows.
    object,
    trait,
    trait, sums)
  
}
slope <- function(time, value) {
  fit <- tryCatch(lm(value ~ time), error = function(e) NULL)
  if(is.null(fit))
    NA
  else
    coef(fit)[2]
}
half <- function(Tmax, time, value) {
  value <- log2(value[time >= Tmax]) - Tmax
  time <- time[time >= Tmax]
  fit <- tryCatch(lm(value ~ time), error = function(e) NULL)
  if(is.null(fit))
    NA
  else
    -1 / coef(fit)[2]
}
