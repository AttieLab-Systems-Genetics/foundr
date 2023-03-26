#' Module Band Object
#'
#' @param traitModule object of class `listof_wgcnaModules`
#' @param response name of response for top row
#' @param solo do one response if `TRUE`
#'
#' @return data frame of class `module_band`
#' @export
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map set_names
#'
module_band <- function(traitModule, response = responses[1], solo = FALSE) {
  if(is.null(traitModule))
    return(NULL)
  
  # Responses in module; put `response` first.
  responses <- names(traitModule)
  responses <- unique(c(response, responses))
  
  out <- 
    dplyr::mutate(
      # Bind rows together across reponses
      dplyr::bind_rows(
        # Determine module bands for each response.
        purrr::set_names(
          purrr::map(
            responses,
            module_band1,
            traitModule),
          responses),
        .id = "response"),
      response = factor(response, responses))

  class(out) <- c("module_band", class(out))
  out
}

module_band1 <- function(response, traitModule) {
  tModule <- traitModule[[response]]
  
  # Order of traits on traitTree
  order <- tModule$geneTree$order
  
  # Modules in order of traits on traitTree
  module <- tModule$modules$module[order]
  
  # Count contingent module traits.
  mle <- rle(as.character(module))
  dplyr::tibble(
    lengths = mle$lengths,
    color = mle$values,
    group = seq_along(mle$values))
}

#' GGplot of module bands
#'
#' @param object object of class `module_band`
#' @param ... additional parameters
#'
#' @return ggplot object
#' 
#' @importFrom ggplot2 aes coord_flip element_text geom_bar ggplot
#'             scale_fill_identity scale_y_reverse theme theme_void
#' @export
#' @rdname module_band
#'
ggplot_module_band <- function(object, ...) {
  ggplot2::ggplot(object) +
    ggplot2::aes(response, lengths,
                 group = group, label = color, fill = color) + 
    ggplot2::geom_bar(stat="identity")+
    ggplot2::scale_fill_identity() + 
    ggplot2::theme_void() + 
    ggplot2::coord_flip() + 
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_discrete(limits = rev) +
    ggplot2::theme(axis.text.y = ggplot2::element_text())
  
}
#' @export
#' @rdname module_band
#' @method autoplot module_band
#'
autoplot.module_band <- function(object, ...)
  ggplot_module_band(object, ...)
  