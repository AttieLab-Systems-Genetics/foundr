#' Order Trait Statistics
#'
#' @param orders name of order criterion 
#' @param traitStats data frame with statistics
#'
#' @return data frame
#' @export
#' @importFrom dplyr arrange filter
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#'
orderTraitStats <- function(orders, traitStats) {
  out <- traitStats
  if(is.null(out))
    return(NULL)
  
  if(orders == "alphabetical") {
    out <- dplyr::arrange(out, .data$trait)
  } else {
    if(orders != "original") {
      # Order by p.value for termname
      termname <- stringr::str_remove(orders, "p_")
      out <- 
        dplyr::arrange(
          dplyr::filter(
            out,
            .data$term == termname),
          .data$p.value)
    }
  }
  out
}