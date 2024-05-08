#' Order Trait Statistics
#'
#' @param orders name of order criterion 
#' @param traitStats data frame with statistics
#'
#' @return data frame
#' @export
#' @importFrom dplyr arrange filter left_join select
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
orderChoices <- function(traitStats) {
  p_types <- paste0("p_", unique(traitStats$term))
  p_types <- p_types[!(p_types %in% c("p_cellmean", "p_signal", "p_rest", "p_noise", "p_rawSD"))]
  p_types <- stringr::str_remove(p_types, "^p_")
  if("strain:diet" %in% p_types)
    p_types <- unique(c("strain:diet", p_types))
  c(p_types, "alphabetical", "original")
}