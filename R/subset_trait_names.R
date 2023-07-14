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
  object <- tidyr::unite(object,
    datatraits,
    .data$dataset, .data$trait,
    sep = sep, remove = remove_columns)
  
  if(!is.null(traitnames) && all(traitnames %in% object$datatraits)) {
    object <- dplyr::filter(object,
      .data$datatraits %in% traitnames)
  }
  
  if(drop_united) {
    object <- dplyr::select(object, -datatraits)
  }
  
  object
}
