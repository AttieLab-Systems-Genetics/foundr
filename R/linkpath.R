#' Title
#'
#' @param dataset name of dataset desired
#' @param links data frame with links to files
#' @param datadir data directory (default is "data")
#'
#' @return
#' @importFrom dplyr filter
#' @export
#'
#' @examples
linkpath <- function(dataset, links, datadir = "data") {
  # Data for this repository are identified by `data/source.csv`,
  # which is not saved with the repo.
  
  filename <- (dplyr::filter(links, object == dataset))$address
  if(datadir != "")
    filename <- file.path(datadir, filename)
  filename
}
