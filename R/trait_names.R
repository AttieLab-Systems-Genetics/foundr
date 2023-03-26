#' Title
#'
#' Trait names are distinct within a dataset, but may be duplicated in
#' different datasets (for instance same type of trait in two tissues).
#' The `object` will have a column `dataset` if it comprises multiple
#' datasets, but it is also possible that the `dataset` column is absent
#' (in case there is only one dataset).
#' 
#' The argument `traitnames` can be one of the following:
#' 1. names found in `trait` column that are distinct across any possible datasets.
#' 2. name of the form `dataset: trait`, that is, separated by the `sep` string.
#' 3. data frame with columns for `dataset` and `trait`, with one row entry per traitname.
#' 4. NULL, in which case all `datatraits` in `object` are returned
#' 
#' @param object 
#' @param traitnames names of traits, possibly including dataset name with separator 
#' @param sep 
#'
#' @return vector of trait names prepended by dataset if appropriate
#' @export
#' @importFrom tidyr unite
#' @importFrom dplyr distinct mutate
#'
#' @examples
#' trait_names(sampleData, c("A","C"))
trait_names <- function(object, traitnames = NULL, sep = ": ") {
  if(is.null(object))
    return(NULL)
  
  # Create `datatraits` column in object.
  if(is_dataset <- ("dataset" %in% names(object))) {
    # Add column for `dataset: trait`
    object <- 
      dplyr::distinct(
        tidyr::unite(
          object,
          datatraits,
          dataset, trait,
          sep = sep, remove = FALSE),
        datatraits, dataset, trait)
  } else {
    object <- 
      dplyr::distinct(
        dplyr:mutate(
          object,
          datatraits = trait),
        datatraits)
  }
  
  
  if(is.null(traitnames)) {
    return(object$datatraits)
  }
  
  # Traitnames is a data frame with columns `dataset` and `trait`
  if(is.data.frame(traitnames)) {
    if(is_dataset) {
      traitnames <- unique(
        tidyr::unite(
          traitnames,
          datatraits,
          dataset, trait, sep = sep)$datatraits)
    } else {
      traitnames <- unique(traitnames$trait)
    }
    traitnames <- traitnames[traitnames %in% object$datatraits]
  }
  
  # `traitnames` is a vector of trait names
  if(is_dataset) {
    # The `object` has `dataset` column.
    
    # Verify that traitnames are in `datatraits` = `dataset: trait`.
    trynames <- traitnames %in% object$datatraits
    if(any(!trynames)) {
      # Supplied traitnames do not include `dataset`?
      trynames2 <- !trynames & (traitnames %in% object$trait)
      if(!all(trynames | trynames2))
        return(NULL) # Could not find all names
      
      # Expand trait names to include dataset
      object2 <- dplyr::filter(
        object,
        trait %in% traitnames[trynames2])
      traitnames <- c(traitnames[trynames], object2$datatraits) 
    }
  } else {
    # Dataset not part of object or traitnames.
    traitnames <- traitnames[traitnames %in% object$datatraits]
  }
  traitnames
}