#' Separate Week from Trait Name
#'
#' @param object data frame
#' @param traits trait names
#'
#' @return data frame
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom rlang .data
#' @importFrom tidyr separate_wider_delim
#'
separate_week <- function(object, traits) {
  dplyr::mutate(
    tidyr::separate_wider_delim(
      dplyr::mutate(
        dplyr::filter(
          object,
          .data$trait %in% wktraits),
        trait = str_replace(.data$trait, "_([0-9]+)wk$", ":\\1")),
      trait,
      delim = ":",
      names = c("trait", "week")),
    week = as.numeric(.data$week))
}
