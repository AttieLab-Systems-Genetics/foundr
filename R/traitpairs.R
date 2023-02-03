#' Create trait pairs from trait names
#'
#' @param traitnames 
#' @param sep string to separate trait names in pairs
#'
#' @return
#' @export
#' @importFrom utils combn
#' @importFrom dplyr across everything mutate
#'
#' @examples
traitpairs <- function(traitnames, sep = " ON ") {
  as.vector(
    unlist(
      dplyr::mutate(
        as.data.frame(utils::combn(traitnames, 2)),
        dplyr::across(
          dplyr::everything(), 
          function(x) {
            c(paste(x, collapse = sep),
              paste(rev(x), collapse = sep))
          }))))
}