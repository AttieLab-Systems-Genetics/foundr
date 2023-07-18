#' Select data including Key Trait and Related Datasets
#'
#' @param object data frame
#' @param key_trait name of key trait
#' @param rel_dataset name of related datasets
#'
#' @importFrom dplyr filter select
#' @importFrom tidyr unite
#' @importFrom rlang .data
#' 
#' @return data frame
select_data_pairs <- function(object, key_trait, rel_dataset = NULL) {
  dplyr::select(
    dplyr::filter(
      tidyr::unite(
        object,
        datatraits,
        .data$dataset, .data$trait,
        sep = ": ", remove = FALSE),
      (.data$datatraits %in% key_trait) |
        (.data$dataset %in% rel_dataset)),
    -datatraits)
}