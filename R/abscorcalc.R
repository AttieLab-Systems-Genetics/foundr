#' Calculate Absolute Correlations
#'
#' @param object data frame
#' @param design names of design columns
#' @param method method for correlation
#'
#' @return data frame with design columns, `trait` names and `value` of absolute correlations
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' 
#' @export
#'
#' @examples
abscorcalc <- function(object,
                       design = c("strain", "number", "sex", "diet"),
                       method = "spearman") {
  abs(
    cor(
      dplyr::select(
        tidyr::pivot_wider(
          object,
          names_from = "trait",
          values_from = "value"),
        -design),
      use = "pairwise",
      method = "spearman"))
}
