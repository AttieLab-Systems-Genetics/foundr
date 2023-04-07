#' Calculate area under curve over minutes
#'
#' @param object data frame with `value` and other columns
#'
#' @return object collapsed over minutes with AUC and other summaries
#' @export
#' @importFrom dplyr arrange group_by mutate n summarize ungroup
#' @importFrom rlang .data
#' @importFrom zoo rollmean
#'
area_under_curve <- function(object, sum_function = auc) {
  cols <- c("strain", "sex", "condition", "animal", "week", "trait")
  cols <- cols[cols %in% names(object)]
  
  tidyr::unite(
    tidyr::pivot_longer(
      dplyr::ungroup(
        dplyr::summarize(
          dplyr::arrange(
            dplyr::group_by(
              dplyr::mutate(
                object,
                minutes = as.numeric(.data$minutes)),
              dplyr::across(dplyr::all_of(cols))),
            .data$minutes),
          AUC = sum(
            diff(.data$minutes) * 
              zoo::rollmean(.data$value, 2)),
          Emax = max(.data$value),
          Tmax = .data$minutes[which.max(.data$value)],
          HalfLife = half(Tmax, .data$minutes, .data$value),
          .groups = "drop")),
      AUC:HalfLife, names_to = "sums", values_to = "value"),
    trait,
    trait, sums)
}
half <- function(Tmax, minutes, value) {
  value <- log2(value[minutes >= Tmax]) - Tmax
  minutes <- minutes[minutes >= Tmax]
  fit <- tryCatch(lm(value ~ minutes), error = function(e) NULL)
  if(is.null(fit))
    NA
  else
    -1 / coef(fit)[2]
}
