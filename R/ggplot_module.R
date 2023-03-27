#' GGplot of Module P-values
#'
#' @param object data frame with appropriate columns
#' @param terms names of two columns with p-values
#' 
#' @return ggplot2 object
#' @export
#' @importFrom ggplot2 aes facet_wrap geom_hline geom_point geom_vline ggplot ggtitle
#'             scale_color_manual scale_x_log10 scale_y_log10 theme xlab ylab
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
ggplot_module <- function(object, terms = c("p_strain_sex", "p_strain")) {
  # Set up module colors
  module_colors <- unique(object$module)
  names(module_colors) <- module_colors
  
  object <- dplyr::mutate(object,
                         x = -log10(.data[[terms[1]]]),
                         y = -log10(.data[[terms[2]]]))
  
  ggplot2::ggplot(object) +
    ggplot2::aes(.data$x, .data$y, col = .data$module) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 5, col = "darkgrey") +
    ggplot2::geom_vline(xintercept = 5, col = "darkgrey") +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::facet_wrap(~ facets) +
    ggplot2::scale_color_manual(values = module_colors) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab(terms[1]) +
    ggplot2::ylab(terms[2]) +
    ggplot2::ggtitle(
      paste("-log p-value for",
            terms[1], "and", terms[2],
            "by WGCNA module"))
}
