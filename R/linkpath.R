#' Title
#'
#' @param dataset name of dataset desired
#' @param links data frame with `shortname`, `address`, `longname`
#' @param datadir data directory (default is "data")
#'
#' @return data frame with
#' @importFrom dplyr filter
#' @export
#'
#' @examples
linkpath <- function(dataset, links, datadir = "data") {
  # Data for this repository are identified by `data/source.csv`,
  # which is not saved with the repo.
  
  filename <- (dplyr::filter(links, shortname == dataset))$address
  if(datadir != "")
    filename <- file.path(datadir, filename)
  filename
}
