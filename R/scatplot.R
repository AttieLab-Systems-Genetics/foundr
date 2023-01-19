#' Scatter plot by trait values
#'
#' @param data data frame
#' @param x first trait
#' @param y second trait
#'
#' @return
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 aes facet_grid geom_point geom_smooth ggplot
#'                     ggtitle scale_x_log10 scale_y_log10
#' @export
#'
#' @examples
scatplot <- function(data, x, y) {
  data <- 
    tidyr::pivot_wider(data, names_from = "trait", values_from = "value")
  
  ggplot2::ggplot(data) +
    ggplot2::aes(.data[[x]], .data[[y]], col = Condition) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
    ggplot2::facet_grid(Sex ~ Strain) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::ggtitle(paste(x, "vs", y, "by Condition"))
}
