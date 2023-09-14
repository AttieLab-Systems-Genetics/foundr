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

# Replace `SD` with `rawSD` in `termstats`
replace_rawSD <- function(termstats, rawstats = NULL) {
  # `termstats` is filtered to `term` of interest.
  # `rawstats` is all stats (for traits in `termstats`).
  if(!is.null(rawstats) && "rawSD" %in% unique(rawstats$term)) {
    # Extract rawSD from `rawstats`
    rawSD <- dplyr::select(
      dplyr::filter(
        rawstats,
        .data$term == "rawSD"),
      dataset, trait, SD)

    # Replace SD by rawSD    
    dplyr::left_join(
      dplyr::select(termstats, -SD),
      rawSD,
      by = c("dataset", "trait"))
    
  } else {
    dplyr::mutate(termstats, SD = 1)
  }
}