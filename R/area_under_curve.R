#' Calculate area under curve over minutes
#'
#' @param object data frame with `value` and other columns
#'
#' @return object collapsed over minutes with AUC
#' @export
#' @importFrom dplyr arrange group_by mutate n summarize ungroup
#' @importFrom rlang .data
#' @importFrom zoo rollmean
#'
area_under_curve <- function(object) {
  cols <- c("strain", "sex", "condition", "animal", "week", "trait")
  cols <- cols[cols %in% names(object)]
  
  dplyr::mutate(
    dplyr::ungroup(
      dplyr::summarize(
        dplyr::arrange(
          dplyr::group_by(
            dplyr::mutate(
              object,
              minutes = as.numeric(.data$minutes)),
            dplyr::across(dplyr::all_of(cols))),
          .data$minutes),
        value = sum(
          diff(.data$minutes) * 
            zoo::rollmean(.data$value, 2)),
        .groups = "drop")),
    trait = paste0(.data$trait, "_AUC"))
}
