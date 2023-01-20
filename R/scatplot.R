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
  
  p <- ggplot2::ggplot(data) +
    ggplot2::aes(.data[[x]], .data[[y]]) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
    ggplot2::facet_grid(sex ~ strain) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10()
  
  if(condition %in% names(data)) {
    p <- p + ggplot::aes(col = condition) +
      ggplot2::ggtitle(paste(x, "vs", y, "by condition"))
  } else {
    p <- p + ggplot2::ggtitle(paste(x, "vs", y))
  }
  p
}
