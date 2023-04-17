#' Title
#'
#' @param dataset name of dataset desired
#' @param links data frame with `shortname`, `address`, `longname`
#'
#' @return data frame with `shortname`, `address`, `longname`
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @export
#'
linkpath <- function(dataset, links) {
  # Data for this repository are identified by `data/source.csv`,
  # which is not saved with the repo.
  
  (dplyr::filter(links, .data$shortname == dataset))$address
}
