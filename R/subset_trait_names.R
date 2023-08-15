#' Subset Trait Data on Names
#'
#' Trait names of form `dataset: trait` will be selected.
#' 
#' @param object data frame
#' @param traitnames character string
#' @param remove_columns remove columns `dataset` and `trait` if `TRUE`
#' @param drop_united drop `datatraits` if `TRUE`
#' @param sep separator string
#'
#' @return data frame
#' @importFrom dplyr filter select
#' @importFrom tidyr unite
#' @importFrom rlang .data
#' @export
#'
subset_trait_names <- function(object, traitnames = NULL,
                               remove_columns = FALSE,
                               drop_united = TRUE,
                               sep = ": ") {
  
  if(is.null(object) || is.null(traitnames))
    return(NULL)
  
  # If `object` is a function, it is assumed to be subsetting from a database
  # using `subset_trait_names.sql()`.
  if(inherits(object, "tbl_SQLiteConnection")) {
    return(subset_trait_names.sql(object, traitnames))
  }
  
  object <- tidyr::unite(object,
    datatraits,
    .data$dataset, .data$trait,
    sep = sep, remove = remove_columns)
  
  if(!all(traitnames %in% object$datatraits))
    return(NULL)
  
  object <- dplyr::filter(object, .data$datatraits %in% traitnames)
  
  if(drop_united) {
    object <- dplyr::select(object, -datatraits)
  }
  
  object
}
subset_trait_names.sql <- function(object, traitnames = NULL) {
  if(is.null(traitnames))
    return(NULL)
  
  # Select only those that match both
  dplyr::collect(
    dplyr::select(
      dplyr::filter(
        dplyr::mutate(
          object,
          datatraits = paste(.data$dataset, .data$trait, sep = ": ")),
        .data$datatraits %in% traitnames),
      -datatraits))
}
