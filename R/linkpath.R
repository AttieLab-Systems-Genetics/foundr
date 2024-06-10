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
#' Title
#'
#' @param object data frame
#' @param linkfile CSV file with shortname, URL, longname
#' @param deployDir directory with `help.Rmd` for `help.md`
#'
#' @return invisible
#' @export
#' @importFrom rmarkdown md_document render
#'
link_datasets <- function(object, linkfile, deployDir = "deploy") {
  # Pull dataset names from `source.csv`
  dataset <- read.csv(linkfile)
  dataset <- dataset[dataset$longname != "", c(1,3)]
  rownames(dataset) <- NULL
  datasets <- dataset$longname
  names(datasets) <- dataset$shortname
  
  # Restrict to datasets used in this app.
  datasets <- datasets[names(datasets) %in% unique(object$dataset)]
  
  # Save `datasets` as RDS in `deployDir`.
  if(!dir.exists(deployDir)) dir.create(deployDir)
  saveRDS(datasets, file.path(deployDir, "datasets.rds"))
  
  # Render `help.md` in `deployDir`.
  rmarkdown::render(file.path(deployDir, "help.Rmd"), rmarkdown::md_document())
  invisible()
}
