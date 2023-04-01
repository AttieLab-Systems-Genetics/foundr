#' Separate GTT Traits with Minute and Week
#'
#' @param object data frame
#' @param traits trait names
#'
#' @return data frame
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom rlang .data
#' @importFrom tidyr separate_wider_delim
#' @importFrom stringr str_replace
#'
separate_GTT <- function(object, traits) {
  dplyr::mutate(
    tidyr::separate_wider_delim(
      dplyr::mutate(
        dplyr::filter(
          object,
          .data$trait %in% mintraits),
        trait = stringr::str_replace(
          .data$trait,
          "_([0-9]+)_([0-9]+)wk$",
          ":\\1:\\2")),
      trait,
      delim = ":",
      names = c("trait", "minute", "week")),
    minute = as.numeric(.data$minute),
    week = as.numeric(.data$week))
}
