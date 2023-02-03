#' Pivot pair of traits
#'
#' @param object data frame with specific columns
#' @param pair 2-vector of trait names
#'
#' @return data frame
#' @export
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_wider
#'
#' @examples
pivot_pair <- function(object, pair) {
  if(!all(pair %in% object$trait)) {
    return(NULL)
  }
  
  dplyr::filter(
    tidyr::pivot_wider(
      dplyr::filter(
        object,
        trait %in% c(pair[1],pair[2])),
      names_from = "trait", values_from = "value"),
    # Make sure x and y columns have no missing data.
    !(is.na(.data[[pair[1]]]) | is.na(.data[[pair[2]]])))
  
}
