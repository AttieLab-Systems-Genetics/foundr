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
#' abscorcalc(sampleData)
abscorcalc <- function(object,
                       method = c("spearman","pearson"),
                       abs = TRUE) {
  method <- match.arg(method)
  
  colnames <- names(object)
  colnames <- colnames[!(colnames %in% c("trait","value"))]
  
  out <- 
    cor(
      dplyr::select(
        tidyr::pivot_wider(
          object,
          names_from = "trait",
          values_from = "value"),
        -colnames),
      use = "pairwise",
      method = "spearman")
  if(abs)
    out <- abs(out)
  out
}
