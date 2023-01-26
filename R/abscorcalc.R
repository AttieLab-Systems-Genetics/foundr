#' Calculate Absolute Correlations
#'
#' @param object data frame
#' @param method method for correlation
#' @param abs absolute correlation if `TRUE`
#'
#' @return data frame with design columns, `trait` names and `value` of absolute correlations
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' 
#' @export
#'
#' @examples
abscorcalc <- function(object,
                       method = "spearman",
                       abs = TRUE) {
  out <- 
    cor(
      tidyr::pivot_wider(
        object,
        names_from = "trait",
        values_from = "value"),
      use = "pairwise",
      method = "spearman")
  if(abs)
    out <- abs(out)
  out
}
