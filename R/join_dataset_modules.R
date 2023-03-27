#' Join Dataset Modules
#'
#' @param dmods list of objects of class `listof_wgcnaModules`
#' @param response names of response to extract
#'
#' @return object of class `listof_wgcnaModules`
#' @export
#' @importFrom purrr transpose
#' @importFrom dplyr filter inner_join
#' @importFrom tidyr unite
#' @importFrom rlang .data
#'
join_dataset_modules <- function(dmods, response) {
  if(is.null(dmods))
    return(NULL)
  
  eclass <- class(dmods[[1]])
  lclass <- class(dmods)
  dmods <- purrr::transpose(dmods)[[response]]
  if(is.null(dmods))
    return(NULL)
  
  dmods <- purrr::transpose(dmods)
  
  # Cannot combine trees, so drop them.
  dmods$geneTree <- NULL
  
  # Reduce to common IDs
  if(is_animal <- ("animal" %in% names(dmods$ID[[1]])))
    bys <- c("ID", "animal")
  else
    bys <- "ID"
  dmodsID <- dplyr::inner_join(
    dmods$ID[[1]],
    dmods$ID[[2]],
    by = bys)
  if(length(dmods$ID) > 2) {
    for(i in seq(3, length(dmods$ID)))
      dmodsID <- dplyr::inner_join(
        dmodsID,
        dmods$ID[[i]],
        by = bys)
  }
  for(i in names(dmods$ID))
    dmods$ID[[i]] <- dmodsID
  
  # Reduce eigen data frames to common IDs.
  if(is_animal)
    dmodsID <- tidyr::unite(
      dmodsID, 
      ID,
      .data$ID, .data$animal,
      na.rm = TRUE)
  dmodsID <- dmodsID$ID
  dmods$eigen <- lapply(dmods$eigen,
                        function(x, ID) x[rownames(x) %in% ID, ],
                        dmodsID)
  
  # Reduce modules to common traits
  dmodstrait <- dplyr::inner_join(
    dmods$modules[[1]]["trait"],
    dmods$modules[[2]]["trait"],
    by = "trait")
  if(length(dmods$modules) > 2) {
    for(i in seq(3, length(dmods$modules)))
      dmodstrait <- dplyr::inner_join(
        dmodstrait,
        dmods$modules[[i]]["trait"],
        by = "trait")
  }
  dmodstrait <- dmodstrait$trait
  
  dmods$modules <- lapply(
    dmods$modules,
    function(x, dmodstrait) dplyr::filter(x, .data$trait %in% dmodstrait),
    dmodstrait)
  dmods <- lapply(
    purrr::transpose(dmods),
    function(x, eclass) {
      class(x) <- eclass
      x},
    eclass)
  class(dmods) <- lclass
  dmods
}
